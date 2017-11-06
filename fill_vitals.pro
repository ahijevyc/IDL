function greatest_vital_range, vitals
  gr = 0.
  for ivital=0,n_tags(vitals)-1 do if max(vitals.(ivital).range) gt gr then gr = max(vitals.(ivital).range)
  return, gr
end


pro fill_vitals, mpas, iCells, init_date, valid_time, vitals, vital_itimes, model_file=model_file, $
  model_basedirs, ignore_parent_id=ignore_parent_id
  ; INPUT
  ; mpas - Output from mpas_mesh('mpas_xx') function. Information about model grid.
  ;   A structure with tags like .latCell, .areaCell, etc.
  ;
  ; iCells - cell indices.  output from mpas_nearest_cell(lons,lats,mpas)
  ;
  ; init_date - string of date/hour ("2015040300") of initialization of model run
  ;
  ; valid_time - valid time of forecast. Double precision output of julday(month,day,year,hour,minute,second)
  ;
  ; vitals - structure of vitals field names and data. from vitals_structure(ntimes) or custom structure like
  ;   vitals = {max_spd10m:{field:'speed10',range:[0,0],op:'max',data:replicate(!VALUES.D_NAN, nbox)}}
  ;
  ; vital_itimes - data array elements to which iCells refer to. [optional]. Needed when processing multiple tracks with common times.
  ; This routine fills in data[vital_itimes]--one by one, in the case of range>0 or all at once for nearest neighbor.
  ;
  ; model_basedirs  - ordered list of directories to search for model forecasts. Use the first one that exists.
  ;
  ; OUTPUT KEYWORD
  ; model_file - path and filename of the model forecast
  ;
  ; KEYWORDS
  ; ignore_parent_id - The default is to stop if the parent_id of the diagnostics file doesn't contain
  ; the parent_id of the init.nc from which the lats and lons were taken. If this is set, don't stop.



  if n_elements(ignore_parent_id) eq 0 then ignore_parent_id=0

  ; My vitals units are km and m/s, while ATCF uses nm and kt. This is crucial when writing ATCF format.
  nfields = n_tags(vitals)
  if n_elements(vital_itimes) eq 0 then vital_itimes = lindgen(n_elements(vitals.(0).data))
  if strlen(init_date) ne 10 then begin
    print, 'init_date '+init_date+' not in yyyymmddhh format'
    stop
  endif

  init_hh = strmid(init_date,8,2)
  diag_datestr = 'diag*.' + string(valid_time, format = '( C(CYI4.4,"-",CMOI2.2,"-",CDI2.2,"_",CHI2.2,".",CMI2.2,".00"))') + '.nc'

  ; GFS has different filename convention.
  if strmatch(mpas.name,'GFS*') then begin
    fh = round(24*(valid_time - julday(strmid(init_date,4,2),strmid(init_date,6,2),strmid(init_date,0,4),init_hh,0,0)))
    diag_datestr = [init_date + "_i"+init_hh+"_f" + string(fh, format='(I3.3)') + '_' + mpas.name +'.nc',$
      'gfs_4_' + strmid(init_date,0,8) + '_' + init_hh+ '00_' + string(fh, format='(I3.3)') + '.grb2.nc',$
      'gfs.t'+init_hh+'z.pgrb2.0p*.f'+string(fh,format='(I3.3)') + '.nc',$
      'gfs.0p25.' + init_date +'.f'+ string(fh, format='(I3.3)') + '.grib2.nc']
  endif

  ; model_basedirs is a string array of possible directories.
  ; diag_datestr may also be a string array (in the case of GFS, it is).
  model_files = file_search(model_basedirs + diag_datestr, count=nfiles)

  if nfiles gt 1 then begin
    print, 'fill_vitals: WARNING. found ',strtrim(nfiles,2),' matching diag* files:'
    print, model_files
    print, 'using first one'
  endif
  ; take 1st array element, even if it is just an empty string.
  ; I think this is designed to get the "rt/" version instead of the "./" version
  ; because "rt/" is listed first above (when model_basedirs is defined)
  model_file = model_files[0]

  if nfiles eq 0 then begin
    print, "fill_vitals: no " +model_basedirs + diag_datestr
    model_file = ''
    return
  endif



  ; First find the greatest range.
  greatest_range = greatest_vital_range(vitals)
  ; Make a list of neighbor arrays for the greatest range.
  if n_elements(mpas.LONCELL) gt 1100000 then print, "Finding neighbors ",strtrim(greatest_range,2)," km from cell", iCells
  iCell_neighbors = list()
  if greatest_range gt 0 then for iCell = 0, n_elements(iCells)-1 do iCell_neighbors.add, $
    mpas_neighbors(iCells[iCell],mpas.lonCell,mpas.latCell,mpas.nEdgesOnCell,mpas.cellsOnCell, range=greatest_range)
  ncid = NCDF_OPEN(model_file)
  print, "fill_vitals: opened "+model_file


  tmp = ncdf_attinq(ncid, "parent_id", /global)
  if tmp.DATATYPE ne "UNKNOWN" then begin
    ; Sanity check
    ; make sure we have the right mpas mesh. with the right latCells, lonCells, etc.
    ; We have two meshes named 'wp'. One for 2016 and before one for 2017 onward.
    ; We should differentiate them by their parent_id attribute.
    ; However, after topo and gwd were updated, around Oct 1, the parent id changed
    ; without changing the mesh.
    ncdf_attget, ncid, "parent_id", model_file_parent_id, /GLOBAL
    model_file_parent_id = string(model_file_parent_id) ; convert from char to string
    matchinit = 0
    ; are any init.nc parent ids in the model file parent id?
    foreach parent_id, mpas.parent_id do begin
      if strpos(model_file_parent_id, parent_id) ne -1 then matchinit = 1
    endforeach
    if not matchinit and not ignore_parent_id then begin
      print, "looking for init.nc mesh parent_id "+mpas.parent_id+" in model file parent_id"
      print, 'init.nc mesh parent_id not in '+model_file_parent_id
      stop
    endif
  endif

  got_fields = hash()
  for ifield = 0, nfields-1 do begin
    name = vitals.(ifield).field
    range = vitals.(ifield).range
    op = vitals.(ifield).op
    ; print, name, range, op, ncid, ncdf_inquire(ncid)
    ; If field has been gotten already, use hash value instead of reading again.
    if got_fields.HasKey(name) then data = got_fields[name] else begin
      ; If field has not been read, read it from file.
      if name eq 'speed10' || name eq 'mse2' || strmatch(mpas.name, 'GFS*') then data = mpas_read(model_file, field=name, ncid=ncid) $
      else ncdf_varget, ncid , ncdf_varid(ncid, name), data
      ; Add data to hash of got_fields.
      got_fields[name] = data
    endelse

    if max(range) gt 0 then begin
      for iCell = 0, n_elements(iCells)-1 do begin
        ineighbors = iCell_neighbors[iCell]
        ; filter for range
        ineighbors = ineighbors[where(ineighbors.range gt range[0] and ineighbors.range lt range[1], /null)]
        case 1 OF
          (op eq 'max')  : vitals.(ifield).data[vital_itimes[iCell]] = max(data[ineighbors.iCell])
          (op eq 'min')  : vitals.(ifield).data[vital_itimes[iCell]] = min(data[ineighbors.iCell])
          ; could mean be weighted by inverse distance? -nyeh.
          (op eq 'mean') : vitals.(ifield).data[vital_itimes[iCell]] = $
            total(data[ineighbors.iCell] * mpas.areaCell[ineighbors.iCell]) / total(mpas.areaCell[ineighbors.iCell])
          (op eq 'maxr') :  begin
            junk = max(data[ineighbors.iCell],imax)
            vitals.(ifield).data[vital_itimes[iCell]] = ineighbors[imax].range
          end
          (strmid(op,2,2) eq '34' or strmid(op,2,2) eq '50' or strmid(op,2,2) eq '64') :  begin
            mps = float(strmid(op,2,2)) * !ATMOS.kts2mps
            ifast = where(data[ineighbors.iCell] ge mps, /null)
            if n_elements(ifast) gt 0 then begin
              ineighbors = ineighbors[ifast]
              case strmid(op,0,2) of
                'NE' : ineighbors = ineighbors[where(ineighbors.az ge    0. and ineighbors.az lt  90., /null)]
                'SE' : ineighbors = ineighbors[where(ineighbors.az ge   90. and ineighbors.az lt 180., /null)]
                'SW' : ineighbors = ineighbors[where(ineighbors.az ge -180. and ineighbors.az lt -90., /null)]
                'NW' : ineighbors = ineighbors[where(ineighbors.az ge  -90. and ineighbors.az lt   0., /null)]
                else: stop
              endcase
              if n_elements(ineighbors) gt 0 then vitals.(ifield).data[vital_itimes[iCell]] = max(ineighbors.range)
            endif
          end
          else : stop
        endcase
      endfor ; iCell
    endif else vitals.(ifield).data[vital_itimes] = data[iCells] ; else for range=0


  endfor ; fields
  ncdf_close, ncid
end

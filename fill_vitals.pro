function greatest_vital_range, vitals
  gr = 0.
  for ivital=0,n_tags(vitals)-1 do if max(vitals.(ivital).range) gt gr then gr = max(vitals.(ivital).range)
  return, gr
end


pro fill_vitals, mpas, iCells, init_date, valid_time, vitals, imatch, model_file=model_file
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
  ; vitals - structure of vitals field names and data. from vitals_structure(n[,nt]) or custom structure like
  ;   vitals = {max_spd10m:{field:'speed10',range:[0,0],op:'max',data:replicate(!VALUES.D_NAN, nbox)}}
  ;
  ; imatch - data array elements to which iCells refer to. [obsolete, optional] I think this was useful when processing multiple forecasts and initializations at once.
  ; This routine fills in data[imatch]--one by one, in the case of range>0 or all at once for nearest neighbor.
  ;
  ; OUTPUT
  ; model_file - path and filename of the model forecast
  ;


  ; My vitals units are km and m/s, while ATCF uses nm and kt. This is crucial when writing ATCF format.
  nfields = n_tags(vitals)
  if n_elements(imatch) eq 0 then imatch = lindgen(n_elements(vitals.(0).data))
  if strlen(init_date) ne 10 then begin
    print, 'init_date '+init_date+' not in yyyymmddhh format'
    stop
  endif

  basedir = ['/glade/p/nmmm0024/','/glade/scratch/mpasrt/'] ; The location of the diag* files is always changing. .. unfortunately
  model_basedir = basedir + mpas.name + "/" + init_date + '/'
  ; kludge to look in /rt subdirectory first for Joaquin 2015
  model_basedir = [model_basedir+'rt/', model_basedir]
  init_hh = strmid(init_date,8,2)
  diag_datestr = string(valid_time, format = '( C(CYI4.4,"-",CMOI2.2,"-",CDI2.2,"_",CHI2.2,".",CMI2.2,".00"))')
  model_files = file_search(model_basedir + 'diag*.' + diag_datestr + '.nc*', count=nfiles)
  ; added wildcard * to end of search string to allow for _hide files.
  ; There were some that needed to be hidden in mpas_ep 20140902-20140909 because relhum was set to zero.
  ; This messed up fcst-init and mpas_ep-mpas statistics but not atcf vitals.

  if nfiles gt 1 then print, 'found ',nfiles,' matching diag* files:',model_files
  ; take 1st array element, even if it is just an empty string.
  ; I think this is designed to get the "rt/" version instead of the "./" version
  ; because "rt/" is listed first above (when model_basedir is defined)
  model_file = model_files[0]

  if nfiles eq 0 then begin
    print, "fill_vitals: no " +model_basedir + 'diag*.' + diag_datestr + '.nc*'
    if strmatch(mpas.name,'GFS*') then begin
      fh = round(24*(valid_time - julday(strmid(init_date,4,2),strmid(init_date,6,2),strmid(init_date,0,4),init_hh,0,0)))
      model_file = '/glade/scratch/ahijevyc/'+mpas.name+'/' + init_date + "_i"+init_hh+"_f" + string(fh, format='(I3.3)') + '_' + mpas.name +'.nc'
      if file_test(model_file) ne 1 then begin
        print, "fill_vitals: no "+model_file
        model_file = '/glade/scratch/ahijevyc/'+mpas.name+'/'+init_date+'/gfs_4_' + strmid(init_date,0,8) + '_' + init_hh+ '00_' + string(fh, format='(I3.3)') + '.grb2.nc'
        print, "trying " + model_file
        if file_test(model_file) ne 1 then begin
          print, "fill_vitals: no "+model_file
          model_file = '/glade/scratch/ahijevyc/'+mpas.name+'/'+init_date+'/gfs.0p25.' + init_date +'.f'+ string(fh, format='(I3.3)') + '.grib2.nc'
        endif
      endif
    endif
  endif

  if file_test(model_file) ne 1 then begin
    print, "fill_vitals: file "+model_file+" don't exist"
    ; if it is not in /glade/scratch/ahijevyc/ check in /glade/scratch/mpasrt/.
    ia = strpos(model_file,'ahijevyc')
    model_file = strmid(model_file,0,ia)+'mpasrt'+strmid(model_file,8+ia)
    if file_test(model_file) ne 1 then begin
      print, "fill_vitals: no " +model_file
      model_basedir = '/glade/p/nmmm0024/'+mpas.name+'/'+init_date+'/'
      model_files = file_search(model_basedir + 'diag*.'+diag_datestr+'.nc',count=nfiles)
      if nfiles eq 0 then begin
        print, "fill_vitals: no "+model_basedir+"diag*"+diag_datestr+".nc"
        model_file = ''
        return
      endif
      if nfiles ne 1 then stop ; sanity check
      model_file = model_files[0]
    endif
  endif
  ncid = NCDF_OPEN(model_file)
  ;print, "fill_vitals: opened "+model_file
  ; First find the greatest range.
  greatest_range = greatest_vital_range(vitals)
  ; Make a list of neighbor arrays for the greatest range.
  iCell_neighbors = list()
  if greatest_range gt 0 then for iCell = 0, n_elements(iCells)-1 do iCell_neighbors.add, $
    mpas_neighbors(iCells[iCell],mpas.lonCell,mpas.latCell,mpas.nEdgesOnCell,mpas.cellsOnCell, range=greatest_range)
  for ifield = 0, nfields-1 do begin
    name = vitals.(ifield).field
    range = vitals.(ifield).range
    op = vitals.(ifield).op
    ;  print, name, range, op, ncid, ncdf_inquire(ncid)
    if name eq 'speed10' || name eq 'mse2' || strmatch(mpas.name, 'GFS*') then data = mpas_read(model_file, field=name, ncid=ncid) $
    else ncdf_varget, ncid , ncdf_varid(ncid, name), data

    if max(range) gt 0 then begin
      for iCell = 0, n_elements(iCells)-1 do begin
        ineighbors = iCell_neighbors[iCell]
        ; filter for range
        ineighbors = ineighbors[where(ineighbors.range gt range[0] and ineighbors.range lt range[1], /null)]
        case 1 OF
          (op eq 'max')  : vitals.(ifield).data[imatch[iCell]] = max(data[ineighbors.iCell])
          (op eq 'min')  : vitals.(ifield).data[imatch[iCell]] = min(data[ineighbors.iCell])
          ; could mean be weighted by inverse distance? -nyeh.
          (op eq 'mean') : vitals.(ifield).data[imatch[iCell]] = $
            total(data[ineighbors.iCell] * mpas.areaCell[ineighbors.iCell]) / total(mpas.areaCell[ineighbors.iCell])
          (op eq 'maxr') :  begin
            junk = max(data[ineighbors.iCell],imax)
            vitals.(ifield).data[imatch[iCell]] = ineighbors[imax].range
          end
          (strmid(op,2,2) eq '17') :  begin ; 34 knots
            ifast = where(data[ineighbors.iCell] ge 17.4911, /null)
            if n_elements(ifast) gt 0 then begin
              ineighbors = ineighbors[ifast]
              case strmid(op,0,2) of
                'NE' : ineighbors = ineighbors[where(ineighbors.az ge    0. and ineighbors.az lt  90., /null)]
                'SE' : ineighbors = ineighbors[where(ineighbors.az ge   90. and ineighbors.az lt 180., /null)]
                'SW' : ineighbors = ineighbors[where(ineighbors.az ge -180. and ineighbors.az lt -90., /null)]
                'NW' : ineighbors = ineighbors[where(ineighbors.az ge  -90. and ineighbors.az lt   0., /null)]
                else: stop
              endcase
              if n_elements(ineighbors) gt 0 then vitals.(ifield).data[imatch[iCell]] = max(ineighbors.range)
            endif
          end
          else : stop
        endcase
      endfor ; iCell
    endif else vitals.(ifield).data[imatch] = data[iCells]


  endfor ; fields
  ncdf_close, ncid
end

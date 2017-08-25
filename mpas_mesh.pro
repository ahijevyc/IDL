function mpas_mesh, mpas_name, nomesh=nomesh, parent_id=parent_id
  if ~keyword_set(nomesh) then nomesh=0

  if nomesh eq 1 then return, {name:mpas_name}
  
  parent_id_string = ''
  if keyword_set(parent_id) then parent_id_string = '.' + parent_id
  
  savfile = '/glade/p/work/ahijevyc/mpas_plots/'+mpas_name+'/'+mpas_name+parent_id_string+'_mesh.sav'
  if file_test(savfile) ne 1 then begin
    print, "could not find "+savfile
    print, "run make_quickie, found in this file"
    stop
  endif

  restore, savfile ; retrieve mpas structure, which has lonCell, cellsOnCells, etc.

  mpas.name = mpas_name  
  ; fixed the mpas3 save file after mar 25, 2014. no need to subract 155 deg from mpas2 longitudes.
  return, mpas
end



pro make_quickie

  mpass = ['wp']
;  mpass = ['mpas_al','mpas_wp']
  ;mpass=['ep','al']
  basedir = '/glade/scratch/ahijevyc/'
  for impas = 0, n_elements(mpass)-1 do begin
    mpas_name = mpass[impas]
    
    if STRMATCH(mpas_name, 'GFS*') || mpas_name eq 'mpas_ep-mpas' then begin
      if mpas_name eq 'GFS_0p50' then begin
        ncid = ncdf_open(basedir+mpas_name+'/2013081000/20130810_i00_f000_GFS004.nc')
        ncdf_varget, ncid, ncdf_varid(ncid, 'lon_0'), lon
        ncdf_varget, ncid, ncdf_varid(ncid, 'lat_0'), lat
        result = ncdf_varinq(ncid, ncdf_varid(ncid,'VGRD_P0_L103_GLL0')); float VGRD_P0_L103_GLL0(lv_HTGL9, lat_0, lon_0)
        if not array_equal(result.dim, [ncdf_dimid(ncid,'lon_0'),ncdf_dimid(ncid,'lat_0'),ncdf_dimid(ncid,'lv_HTGL9')]) then stop
      endif
      if mpas_name eq 'GFS' then begin
        ncid = ncdf_open(basedir+mpas_name+'/2017082000/gfs.t00z.pgrb2.0p25.f000.nc')
        ncdf_varget, ncid, ncdf_varid(ncid, 'lon_0'), lon
        ncdf_varget, ncid, ncdf_varid(ncid, 'lat_0'), lat
        result = ncdf_varinq(ncid, ncdf_varid(ncid,'VGRD_P0_L103_GLL0')); float VGRD_P0_L103_GLL0(lv_HTGL9, lat_0, lon_0)
        if not array_equal(result.dim, [ncdf_dimid(ncid,'lon_0'),ncdf_dimid(ncid,'lat_0'),ncdf_dimid(ncid,'lv_HTGL7')]) then stop
        parent_id = '0p25'
        landmask = replicate(0, [lon.length, lat.length])
      endif
      if mpas_name eq 'mpas_ep-mpas' then begin
        ncid = ncdf_open(basedir+'mpas_ep/2014081000/latlon_0.500deg_025km/diagnostics.2014-08-16_18.00.00_0.500deg_025km.nc')
        ncdf_varget, ncid, ncdf_varid(ncid, 'lon'), lon
        ncdf_varget, ncid, ncdf_varid(ncid, 'lat'), lat
        result = ncdf_varinq(ncid, ncdf_varid(ncid,'uzonal_500hPa')); double uzonal_500hPa(time, lat, lon) 
      endif
      ncdf_close, ncid
      nlon = n_elements(lon)
      nlat = n_elements(lat)
      lonCell = replicate(1d,nlat)##lon
      latCell = lat##replicate(1d,nlon)
      areaCell = (111320d*0.5)^2 * cos(latCell*!DTOR) ; 1 deg = 111320 m
      nEdgesOnCell = replicate(4L,n_elements(latCell))
      i = lindgen(nlon,nlat)
      ; get top neighbor, bottom neighbor, left and right neighbor...
      top = shift(i, 0, -1)
      bot = shift(i, 0,  1)
      left = shift(i,  1, 0)
      right= shift(i, -1, 0)

      half_lon = round(nlon/2.) ; halfway around world
      quar_lon = round(nlon/4.)
      ; Redefine neighbors of top and bottom rows, which are all the same location (the pole).  
      ; Technically, the whole row equatorward of the pole are neighbors to the pole.
      ; Instead just use 4 points from the row next to the pole, spaced equally around the globe.  
      row_next_to_pole = i[*,nlat-2]
      top[-nlon:-1] = shift(row_next_to_pole, half_lon)
      right[-nlon:-1] = shift(row_next_to_pole, quar_lon)
      left[-nlon:-1] = shift(row_next_to_pole, -quar_lon)
      
      row_next_to_pole = i[*,1]
      right[0:nlon-1] = shift(row_next_to_pole, quar_lon)
      bot[0:nlon-1] = shift(row_next_to_pole, half_lon)
      left[0:nlon-1] = shift(row_next_to_pole, -quar_lon)
      
      lonCell = lonCell[*]
      latCell = latCell[*] ; convert to 1-D
      areaCell = areaCell[*]
      
      cellsOnCell = transpose([[top[*]],[right[*]],[bot[*]],[left[*]]]) + 1 ; cellsOnCell is Fortran 1-based?
    endif else begin
      initnc_files = file_search(basedir+mpas_name+'/2016092300/init.nc',count=nfiles)
      if nfiles eq 0 then begin
        print, "did not find init.nc file in", basedir+mpas_name
        stop
      endif
      init_info = file_info(initnc_files[0])
      init_size = init_info.size
      for ifile=1,nfiles-1 do begin 
        init_info = file_info(initnc_files[ifile])
        if init_info.size ne init_size then stop
      endfor
      i = mpas_read(initnc_files[0])
      latCell = i["latCell","value"]
      lonCell = i["lonCell","value"]
      nEdgesOnCell = i["nEdgesOnCell","value"]
      areaCell = i["areaCell","value"]
      cellsOnCell = i["cellsOnCell","value"]
      landmask = i["landmask","value"] ; landmask needed by mpas_water_budget.pro
      parent_id = i["parent_id"]
    endelse
    
    savfile = '/glade/p/work/ahijevyc/mpas_plots/'+mpas_name+'/'+mpas_name+'.'+parent_id+'_mesh.sav'
    mpas = {name:mpas_name, lonCell:lonCell, latCell:latCell, $
      nEdgesOnCell:nEdgesOnCell, cellsOnCell:cellsOnCell, areaCell:areaCell, $
      landmask:landmask, parent_id:parent_id, savfile:savfile}
    
    save, mpas, filename = savfile
  endfor
  
end

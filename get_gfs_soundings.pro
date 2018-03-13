function get_gfs_soundings,pgi,date,hour,inlevel,best_track,mtm00,block_radius=block_radius,$
    debug=debug,absolute_wind=absolute_wind, units=inunits
  ; called by get_mean_gfs
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(block_radius) then block_radius = 0
  ; use absolute_wind=1 if you don't want the system motion subtracted.  You might need
  ; absolute wind for surface wind and flux calculations.
  if ~keyword_set(absolute_wind) then absolute_wind = 0
  if ~keyword_set(units) then units = 'Pa'
  level = inlevel
  units = inunits
  if units eq 'mb' || units eq 'hPa' then begin
    level = inlevel*100.
    units='Pa'
  endif
  
  
  nmissions=1
  imission=0
  stringhour = string(hour, format='(I2.2)')
  
  gfs_file = '/Volumes/pecan2/ahijevyc/PREDICT/GFS/'+date+stringhour+'.nc'
  ncid = ncdf_open(gfs_file)
  ncdf_varget,ncid,'RH',rhs
  ncdf_varget,ncid,'SOUTH_NORTH',lats
  ncdf_varget,ncid,'WEST_EAST',lons
  ncdf_varget,ncid,'plevs',plevs
  ncdf_varget,ncid,'lv_ISBL7',lv_ISBL7 ; used for RH
  ncdf_varget,ncid,'U',us
  ncdf_varget,ncid,'V',vs
  ncdf_varget,ncid,'T',ts ; Temperature in K
  ncdf_varget,ncid,'Z',Zs
  
  ; when I created the GFS netCDF files with NCL, there were sometimes 1e+20 (_fillValues) on the edge.
  ; This happened when the domain extended east of prime meridian 0E. I don't want to treat them as real values.
  ncdf_replace_fillvalue, ncid, 'U', us
  ncdf_replace_fillvalue, ncid, 'V', vs
  ncdf_replace_fillvalue, ncid, 'T', ts
  ncdf_replace_fillvalue, ncid, 'Z', zs
  ncdf_replace_fillvalue, ncid, 'RH', rhs
  
  ncdf_diminq, ncid, 0, dim0, size0
  ncdf_diminq, ncid, 1, dim1, size1
  ncdf_diminq, ncid, 2, dim2, size2
  ncdf_diminq, ncid, 3, dim3, size3
  ncdf_diminq, ncid, 4, dim4, size4
  if dim3 ne 'WEST_EAST' then stop
  if dim2 ne 'SOUTH_NORTH' then stop
  if dim4 ne 'lv_ISBL7' then stop
  if dim0 ne 'Time' then stop
  if size0 ne 1 then stop
  
  varinq = ncdf_varinq(ncid, 'RH')
  if array_equal(varinq.dim, [3,2,4,0]) ne 1 then stop
  
  ncdf_attget, ncid, ncdf_varid(ncid, 'plevs'), 'units', plevs_units
  if string(plevs_units) ne 'hPa' then stop
  
  ncdf_close, ncid ; forgot this for the longest time and had 1000's of open files.
  ; a good way to cound the number of open files on linux is to find the process number of idl
  ; and issue this command # lsof -p 28290
  
  
  
  
  nlon=n_elements(lons)
  nlat=n_elements(lats)
  ndrops = nlat*nlon
  
  xdrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  ydrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  radiusdrops = replicate(!VALUES.F_NAN,ndrops, nmissions)
  stiddrops   = replicate('',           ndrops, nmissions)
  udrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  vdrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  tdrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  zdrops    = replicate(!VALUES.F_NAN, ndrops, nmissions)
  tddrops   = replicate(!VALUES.F_NAN, ndrops, nmissions)
  rhdrops   = replicate(!VALUES.F_NAN, ndrops, nmissions)
  pdrops_mb = replicate(!VALUES.F_NAN, ndrops, nmissions)
  msedrops  = replicate(!VALUES.F_NAN, ndrops, nmissions)
  
  mtm_center, pgi, date, stringhour+'0000', clat, clon, best_track=best_track, mtm00=mtm00, silent=debug?0:1, u=usys, v=vsys
  if NOT FINITE(clat) then begin
    print, "no center found for "+pgi+" "+date+" "+stringhour+". stopping"
    if best_track eq 1 then print, "are you sure best track exists for this time?"
    stop
  endif
  if ~finite(usys) or ~finite(vsys) then stop
  
  for ilon = 0, nlon-1 do begin
    for ilat = 7, nlat-16 do begin
      idrop = ilon*nlat+ilat
      lon = lons[ilon]
      lat = lats[ilat]
      result = map_2points(clon, clat, lon, lat)
      ; az is degrees east of north (meteorological)
      A = 6371.
      r = result[0] * !DTOR * A
      if r lt block_radius then continue
      az = result[1]
      x = sin(az*!DTOR) * r
      y = cos(az*!DTOR) * r
      xdrops[idrop,imission] = x
      ydrops[idrop,imission] = y
      radiusdrops[idrop,imission] = r
      stiddrops[idrop,imission] = string(lat,lon,format='(F5.2,",",F6.2)')
      if units eq 'Pa' then begin
        ilevel = where(plevs*100. eq level, n)
        if n ne 1 then begin
          ;print, 'get_gfs_soundings: not gfs level:',level
          u = !VALUES.F_NAN
          v = !VALUES.F_NAN
          t = !VALUES.F_NAN
          z = !VALUES.F_NAN
          rh = !VALUES.F_NAN
        endif else begin
          u = us[ilon,ilat,ilevel]
          v = vs[ilon,ilat,ilevel]
          t = ts[ilon,ilat,ilevel]
          z = zs[ilon,ilat,ilevel]
          irh = where(lv_ISBL7*100. eq level, n)
          rh=rhs[ilon,ilat,irh]
        endelse
        p = level / 100.  ; PRESS is output in mb
      endif else if units eq 'm' then begin
        u = !VALUES.F_NAN
        v = !VALUES.F_NAN
        t = !VALUES.F_NAN
        z = level
        rh = !VALUES.F_NAN
        p  = !VALUES.F_NAN
        ; assumes zs is descending heights
        ; make sure zs is descending heights
        if zs[ilon,ilat,0] lt zs[ilon,ilat,1] then stop
        iabv = max(where(zs[ilon,ilat,*] ge level))
        ibel = min(where(zs[ilon,ilat,*] le level))
        if iabv ge 0 and iabv lt n_elements(plevs) and ibel ge 0 and ibel lt n_elements(plevs) then begin
          ; added more complexity to deal with rare occasions when requested height in meters exactly
          ; corresponds to an available height.  If this happens, iabv eq ibel and dz=0.
          ; This causes an arithmetic error when you divide by dz below.
          dz = zs[ilon,ilat,iabv]-zs[ilon,ilat,ibel]
          if iabv eq ibel then begin
            ilevel = iabv
            u = us[ilon,ilat,ilevel]
            v = vs[ilon,ilat,ilevel]
            t = ts[ilon,ilat,ilevel]
            p = plevs[ilevel]
          endif else begin
            du = us[ilon,ilat,iabv]-us[ilon,ilat,ibel]
            u = us[ilon,ilat,ibel] + (level-zs[ilon,ilat,ibel])*du/dz
            dv = vs[ilon,ilat,iabv]-vs[ilon,ilat,ibel]
            v = vs[ilon,ilat,ibel] + (level-zs[ilon,ilat,ibel])*dv/dz
            dt = ts[ilon,ilat,iabv]-ts[ilon,ilat,ibel]
            t = ts[ilon,ilat,ibel] + (level-zs[ilon,ilat,ibel])*dt/dz
            dp = plevs[iabv]-plevs[ibel]
            p = plevs[ibel] + (level-zs[ilon,ilat,ibel])*dp/dz
          endelse
          
          iabv_rh = (where(lv_isbl7 eq plevs[iabv]))[0]
          ibel_rh = (where(lv_isbl7 eq plevs[ibel]))[0]
          if iabv_rh ge 0 and iabv_rh lt n_elements(lv_ISBL7) and ibel_rh ge 0 and ibel_rh lt n_elements(lv_ISBL7) then begin
            ; do i need to redefine dz here? or is it the same with drh. I think it is the same. 20120229.
            if iabv_rh eq ibel_rh then begin
              rh = rhs[ilon,ilat,ibel_rh]
            endif else begin
              drh = rhs[ilon,ilat,iabv_rh]-rhs[ilon,ilat,ibel_rh]
              rh = rhs[ilon,ilat,ibel_rh] + (level-zs[ilon,ilat,ibel])*drh/dz
            endelse
          endif
          
        endif
      endif else message, 'unexpected units '+units
      
      ; GFS U and V are not adjusted for system motion until now
      udrops[idrop,imission] = absolute_wind eq 1 ? u : u-usys
      vdrops[idrop,imission] = absolute_wind eq 1 ? v : v-vsys
      tdrops[idrop,imission] = t-!CONST.T0; Kelvin to Celcius
      zdrops[idrop,imission] = z
      rhdrops[idrop,imission] = rh
      pdrops_mb[idrop,imission]  = p
    endfor
  endfor
  irh0 = where(rhdrops eq 0, nrh0)
  if nrh0 gt 0 then tddrops[irh0] = -!VALUES.F_INFINITY
  igoodrh = where(rhdrops gt 0, ngoodrh)
  if ngoodrh gt 0 then tddrops[igoodrh] = rh2tdew(tdrops[igoodrh], rhdrops[igoodrh])
  
  msedrops = moist_static_energy(tdrops+!CONST.T0, tddrops+!CONST.T0, pdrops_mb, zdrops)
  
  ; PRESS is output in mb
  return, {ndrops:ndrops, xdrops:xdrops, ydrops:ydrops, stiddrops:stiddrops, tdrops:tdrops, $
    msedrops:msedrops, zdrops:zdrops, $
    tddrops:tddrops, rhdrops:rhdrops, udrops:udrops, vdrops:vdrops, radiusdrops:radiusdrops, PRESS:pdrops_mb}
    
    
end

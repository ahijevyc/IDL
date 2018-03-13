pro vort_rings
  debug=1
   
  files = file_search('/Volumes/pecan2/ahijevyc/2005PHILIPPE/12KM/2005-09-20_12:00:00/[0-9][0-9][0-9].nc', count=nfiles)
  if !D.NAME eq 'PS' then device, /close, /color, ysize=5, yoffset=5, /inches
;  if !D.NAME eq 'X' and !D.WINDOW ne -1 then wdelete
  loadct, 40

  for ifile = 0,30 do begin
    file = files[ifile]
    if ~File_test(file) then message, 'file '+file+' not found'
    ncid = NCDF_OPEN(file)            ; Open The NetCDF file
    NCDF_VARGET, ncid,  "Time", Time
    vortid = NCDF_VARID(ncid, "rel_vort_latlon") 
    NCDF_VARGET, ncid, vortid, vort
    NCDF_ATTGET, ncid, vortid, "_FillValue", vort_FillValue
    NCDF_ATTGET, ncid, "U", "_FillValue", u_FillValue
    NCDF_ATTGET, ncid, "V", "_FillValue", v_FillValue
    NCDF_VARGET, ncid, "U", u  
    NCDF_VARGET, ncid, "V", v
    NCDF_VARGET, ncid, "Rain", Rain  
    NCDF_VARGET, ncid, "WEST_EAST", lon
    NCDF_VARGET, ncid, "SOUTH_NORTH", lat
    
    NCDF_CLOSE, ncid      ; Close the NetCDF file
    
    vortbad = where(vort eq vort_FillValue, n)
    if n gt 0 then vort[vortbad] = !VALUES.F_NAN
    
    ubad = where(u eq u_FillValue, n)
    if n gt 0 then u[ubad] = !VALUES.F_NAN
    
    vbad = where(v eq v_FillValue, n)
    if n gt 0 then v[vbad] = !VALUES.F_NAN
    
    zeta = vort[*,*,3]
    zeta = 40+zeta*1.1
    u = u[*,*,3]
    v = v[*,*,3]
    


    nlon = (size(zeta))[1]
    clon = nlon/2
    nlat = (size(zeta))[2]
    clat = nlat/2
    print, "clon=",clon,"(",lon[clon],") clat=",clat, "(",lat[clat],")"
    print, "nlon=",nlon,"nlat=",nlat
    print, lat[clat], lon[clon]
    r = fltarr(nlon,nlat)
    az = fltarr(nlon,nlat)
    tang_wind = fltarr(nlon,nlat)
          
    for i=0,nlon-1 do begin
      for j=0,nlat-1 do begin
        result = map_2points(lon[i], lat[j], lon[clon], lat[clat])
        ; az is degrees east of north (meteorological)
        A = 6378.1
        r[i,j] = result[0] * !DTOR * A 
        az[i,j] = result[1]
        ; tang_wind = V dot T , where V is the velocity vector and T is the tangential unit vector
        ; T = -k x r, where k is the vertical unit vector and r is the vector from the point to the center of the vortex.
        ; r = sin(90-az)I - cos(90-az)J, where I and J are unit vectors
        ; r = cos(az)I - sin(az)J
        ; tang_wind = u * cos(az)  - v * sin(az)
        tang_wind[i,j] = u[i,j] * cos(az[i,j]*!DTOR) - v[i,j] * sin(az[i,j]*!DTOR) 
      endfor
    endfor

    if debug then begin
      i = 50
      j = 100
      print, i,j,lat[j],lon[i],r[i,j], format='("i=",i0,2x,"j=",i0,3x,"lat=",F,3x,"lon=",F,4x,"r=",F)'
      print, u[i,j],v[i,j], tang_wind[i,j], format='("u=",F0,2x,"v=",F0,3x,"tang_wind=",F0)'
      print, az[i,j], format='("az=",F)'
      i = 150
      j = 100
      print, i,j,lat[j],lon[i],r[i,j], format='("i=",i0,2x,"j=",i0,3x,"lat=",F,3x,"lon=",F,4x,"r=",F)'
      print, u[i,j],v[i,j], tang_wind[i,j], format='("u=",F0,2x,"v=",F0,3x,"tang_wind=",F0)'
      print, az[i,j], format='("az=",F)'
    endif


    nrings = 25
    maxr = 1000
    dr = maxr/nrings
    az_avg_tang_wind   = fltarr(nrings)
    az_avg_tang_wind_r = fltarr(nrings)
    C                  = replicate(!VALUES.F_NAN, nrings)
    dCdr               = replicate(!VALUES.F_NAN, nrings)
        
    for r1 = 0, nrings-1 do begin
      iring = where(r ge r1*dr and r lt (r1+1)*dr, n)
      az_avg_tang_wind[r1]   = mean(tang_wind[iring])
      az_avg_tang_wind_r[r1] = mean(r[iring]*1000. * tang_wind[iring])
      circum = 2 * !DPI * (0.5+r1)*dr * 1000. 
      C[r1] = circum * az_avg_tang_wind[r1] 
      if r1 ge 1 then dCdr[r1] = (C[r1] - C[r1-1]) / (dr*1000.)
    endfor
    for r1 = 0, maxr, 100 do begin
      iring = where(r gt r1-4 and r lt r1+4, n)
      if n gt 0 then zeta[iring] = 255B
      if n gt 0 then tang_wind[iring] = max(tang_wind)
    endfor 
    

;    tvscl,   r, nlon*1, 0
;    tvscl,  u, nlon*0, nlat*1
;    tvscl,  v, nlon*1, nlat*1
;    tvscl, tang_wind, nlon*2, nlat*2
;    tvscl, tang_wind * r, nlon*3, nlat*2
;    tvscl, Rain, nlon*0, nlat*2

    km = dr * (indgen(nrings)+0.5)
;    plot, km, az_avg_tang_wind, yrange = [0,35], /noerase

    
    plot, km, az_avg_tang_wind_r, yrange=[0,2e7], xtitle='radius [km]', ytitle='radius times average tangential speed [m^2/s]', title=ifile
    tv, zeta, nlon*2, nlat*2-130
    outfile = STRING(ifile, format='("RV",i03,".png")')
    write_png, outfile, TVRD()

    plot, km, dCdr, yrange=[-100,400], xtitle='radius [km]', ytitle='derivative of circulation w respect to range [m/s?]', title=ifile
    tv, zeta, nlon*2, nlat*2-130
    outfile = STRING(ifile, format='("dCdr",i03,".png")')
    write_png, outfile, TVRD()
    
  endfor
q  if !D.NAME eq 'PS' then device, /close
end
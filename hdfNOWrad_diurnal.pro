function raw2rr, a

  a = a mod 16 ; subtract multiples of 16
  ; Convert from 0-15 scale to 0-75 dBZ scale
  dBZ = (a)*5
  ; Convert from logrithmic dBZ to linear reflectivity factor Z
  z = 10.^(dBZ/10.)
  ; Set Z = 0 where dBZ = zero.
  izero = where(a eq 0, nzero)
  if nzero gt 0 then Z[izero] = 0.
  ; Convert from Z to rain rate
  ;    Z = 300.*rr^1.5
  return, (Z/300.)^(2./3.)
  
end

pro hdfNOWrad_diurnal, debug=debug

  IF ~KEYWORD_SET(debug) THEN debug = 0
  if !D.NAME eq 'PS' then device, /close, /color, bits=8, xoffset=1, yoffset=1, xsize=6.5, ysize=9, /inches
  loadct, 41, file='/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl'
  close, /all
  
  ; threshold in mm/h
  thresh = 0.25
  
  
  latmin=20. & lonmin=-130. & latmax=53. & lonmax=-60.
  
  dlon = (lonmax-lonmin)/3660
  dlat = (latmax-latmin)/1836
  
  pos = [0.1, 0.15, 0.9, 0.9]
  zlimit = [38, -110, 42, -80]
  mlimit = [30, -100, 49, -95]
  
  ; array index bounds of zonal Hovmoller
  zx0 = (zlimit[1] - lonmin) / dlon
  zx1 = (zlimit[3] - lonmin) / dlon
  zy0 = (zlimit[0] - latmin) / dlat
  zy1 = (zlimit[2] - latmin) / dlat
  ; array index bounds of meridional Hovm
  mx0 = (mlimit[1] - lonmin) / dlon
  mx1 = (mlimit[3] - lonmin) / dlon
  my0 = (mlimit[0] - latmin) / dlat
  my1 = (mlimit[2] - latmin) / dlat
  
  ; Get list of hdf NOWrad files
  files = file_search('/data1a/pd/ahijevyc/200107/U????Z01.???_refl_2km.hdf', count = nfiles)
  
  ntimes = 24*4
  n = lonarr(ntimes)
  
  ; Save results in this file.
  restore_file = '/data1a/pd/ahijevyc/200107/diurnalhdfnowrad.sav'
  ; If restore file exists, restore the results.
  if file_test(restore_file) then restore, restore_file, verbose=debug
  for ifile=0, nfiles-1 do begin
    ; If the restore file exists skip the ingest loop.
    if file_test(restore_file) then break
    file=files[ifile]
    if debug then print, 'file: ', file
    ; Print a little progress bar.
    if ifile mod 20 eq 0 then print, '.', format='(A,$)'
    
    HDF_DFR8_RESTART
    HDF_DFR8_GETIMAGE, file, raw
    raw = rotate(raw, 7)
    zraw = raw[zx0:zx1, zy0:zy1]
    mraw = raw[mx0:mx1, my0:my1]
    
    zrr = raw2rr(zraw)
    mrr = raw2rr(mraw)
    
    
    zsz = size(zrr)      ;Size of image
    msz = size(mrr)      ;Size of image
    
    ; Get the hour, minute, and year from the filename.
    hh = strmid(file,STRPOS(file,'U')+1,2)
    mm = strmid(file,STRPOS(file,'U')+3,2)
    yy = strmid(file,STRPOS(file,'U')+6,2)
    ; index of the time-dimension.
    hour = hh * 4 + mm/15
    
    ; Set to true (1) to Plot 2-D radar field for debugging purposes.
    if 0 then begin
      title = file
      MAP_SET, 36.5, (lonmin+lonmax)/2., LIMIT=zlimit, TITLE=title, /NOBORDER, /isotropic, position = pos
      
      ; higher scale means more resolution 0.02 is default
      ; COMPRESS=1 is most accurate
      ; MISSING=0 sets the values outside of the domain to 0.
      ; If the current device is 'PS' it gets set to 255 by default
      ; MAP_PATCH is much slower than MAP_IMAGE - 20070703
      result = MAP_IMAGE(16 * (zraw mod 16), Startx, Starty, Xsize, Ysize, COMPRESS=1, SCALE=0.1, LATMIN=zlimit[0], LONMIN=zlimit[1], LATMAX=zlimit[2], LONMAX=zlimit[3])
      
      
      IF !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0
      TV, result, Startx, Starty, XSIZE=Xsize, YSIZE=ysize
      IF !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
      
      map_continents, /usa
      contourbar, 5*(1+indgen(15)), 16*(1+indgen(15)), position=[0.045,0.1,0.055,0.9], title='dBZ', format='(i2)', /vertical
      
      
      MAP_SET, 36.5, (lonmin+lonmax)/2., LIMIT=mlimit, TITLE=title, /NOBORDER, /isotropic, position = pos
      result = MAP_IMAGE(16 * (mraw mod 16), Startx, Starty, Xsize, Ysize, COMPRESS=1, SCALE=0.1, LATMIN=mlimit[0], LONMIN=mlimit[1], LATMAX=mlimit[2], LONMAX=mlimit[3])
      
      
      IF !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0
      TV, result, Startx, Starty, XSIZE=Xsize, YSIZE=ysize
      IF !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
      
      map_continents, /usa
      contourbar, 5*(1+indgen(15)), 16*(1+indgen(15)), position=[0.045,0.1,0.055,0.9], title='dBZ', format='(i2)', /vertical
      
      
    endif
    
    ; Get meridional average.
    zrr = rebin(zrr, zsz[1], 1)
    ; Get zonal average.
    mrr = reform(rebin(mrr, 1, msz[2]))
    
    
    zhits = float(zrr ge thresh) ; 1 if true, 0 if false
    mhits = float(mrr ge thresh) ; 1 if true, 0 if false
    
    ; Manufacture 2-D distance x time array that holds the Hovmoller information.
    if n_elements(ztot) eq 0 then ztot = rebin(zhits, n_elements(zhits), ntimes) else ztot[*,hour] = ztot[*,hour] + zhits
    if n_elements(mtot) eq 0 then mtot = rebin(mhits, n_elements(mhits), ntimes) else mtot[*,hour] = mtot[*,hour] + mhits
    n[hour] = n[hour] + 1
  endfor
  
  ; Open output files.
  openw,  lun, '/data1a/pd/ahijevyc/200107/zonal_hovm.txt', /get_lun
  openw, mlun, '/data1a/pd/ahijevyc/200107/merid_hovm.txt', /get_lun
  
  
  lon = zlimit[1] + dlon * findgen(n_elements(zhits))
  lat = mlimit[0] + dlat * findgen(n_elements(mhits))
  zavg = float(ztot)
  mavg = float(mtot)
  for t = 0, ntimes-1 do begin
    ; Divide the number of cases that passed the threshold test by the total number of cases (for each time).
    if n[t] gt 0 then begin
      zavg[*,t] = 100*float(ztot[*,t])/n[t]
      mavg[*,t] = 100*float(mtot[*,t])/n[t]
    endif
    ; Output 3 columns (lon, time, avg) to text file for plotting later with Stan's script.
    for ix = 0, n_elements(zhits)-1 do printf,  lun, lon[ix], t/4., zavg[ix,t]
    ; Output 3 columns (lat, time, avg) to text file for plotting later with Stan's script.
    for ix = 0, n_elements(mhits)-1 do printf, mlun, lat[ix], t/4., mavg[ix,t]
  endfor
  ; repeat 24-h period for clarity, adding 24h to the values in the time column
  for t = 0, ntimes-1 do for ix=0, zsz[1]-1 do printf, lun, lon[ix], t/4.+24, zavg[ix,t]
  for t = 0, ntimes-1 do for ix=0, msz[2]-1 do printf, mlun, lat[ix], t/4.+24, mavg[ix,t]
  
  
  levels = [5,10,15,20,25,30,35,40,45,50]
  colors = mycolors(levels)
  ytickname = ['0', '18', '12', '6', '0', '18', '12', '6']
  
  ; Plot zonal Hovmoller diagram
  contour, [[zavg],[zavg]], lon, indgen(ntimes*2), /cell_fill, levels=levels, xstyle=1, ystyle=1, ytickname=ytickname, c_colors=colors, position=pos, yrange=[191, 0], yticks=8, xrange=[-110, -90]
  contourbar, levels, colors, format='(I2)', pos=[0.2,0.04,0.8,0.05], title='%'
  ; meridional Hovmoller plot
  contour, [[mavg],[mavg]], lat, indgen(ntimes*2), /cell_fill, levels=levels, xstyle=1, ystyle=1, ytickname=ytickname, c_colors=colors, position=pos, yrange=[191, 0], yticks=8, xrange=[33, 48]
  contourbar, levels, colors, format='(I2)', pos=[0.2,0.04,0.8,0.05], title='%'
  
  
  if !D.NAME eq 'PS' then device, /close

  ; Free logical units reserved for output text files.  If you don't, you may run out of available input/output units. 
  free_lun, lun, mlun

  ; If the restore file does not exist, create it.
  if ~file_test(restore_file) then save, /var, filename = restore_file
end

pro cmorph_IR_figure, pgi, hour, mtm00=mtm00, date=date
  date='2010'+date
  hour=string(hour, format='(I2.2)')
  ; copied from vort_rings.pro
  debug=0
  
  mintemp = -90
  maxtemp =  30
  
  force_new = 0
  ;  type = 'Vis'
  type = 'Cloud*Temp-TEMP*' ; if you add an * after 'TEMP' then you will get two of everything
  units = 'deg C'
  loadct, 39, /silent
  nowindows=0
  
  ; Look for .area files
  ; Look for single-digit indices first, then double-digit because IDV did not pad the indices with zeroes.
  ; Be aware the numbering is random, as far as I can tell, not chronological, i.e. 0 does not correspond to the first time.
  ; Therefore find the actual time and use it in the output file name.
  files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/GOES/'+date+'*/' + [type+'[0-9].area',type+'[0-9][0-9].area'])
  
  ;pgi must be a string with format 'PGI38L'
  if not strmatch(pgi,'PGI[2345][0-9]L') then stop
  
  outdir = '/Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats/'
  best_track = 0
  ; if mtm=1, use mtm track files at 00 UTC consensus model analysis time and interpolate between them. Don't use the 1-XX h forecasts.
  
  
  maxr = 720
  dx = 32. ; km
  dy = 32. ; km
  nx = round(2*maxr/dx)
  ny = round(2*maxr/dy)
  
  
  for ifile = 0,n_elements(files)-1 do begin
    file = files[ifile]
    if ~File_test(file) then message, 'file '+file+' not found'
    
    mcidas_read,file=file,image=image,area=area,nav=nav,LINhdr=LINhdr,cmd=cmd
    nlon = area.num_elem
    nlat = area.num_line
    imageDate = strtrim(string(area.img_date + 1900000l),2)
    imageTime = strtrim(string(area.img_time,FORMAT='(I6.6)'),2)
    imageHour = strmid(imageTime,0,2)
    imageMinute = strmid(imageTime, 2,2)
    ; The year part of imageDate appears to be screwed up (2010 = 110 for some reason) but the day-of-the-year seems fine.
    julian = julday(1,1,2010,0,0,0) + long(strmid(imageDate,2,3,/reverse)) - 1 + imageHour/24. + imageMinute/24./60.
    caldat, julian, month, day
    ; make sure you skip this time before opening up a text file - otherwise it will overwrite and make empty if it exists.
    datestring = '2010'+string(month,day,format='(I2.2,I2.2)')
    
    print, datestring+imageTime+ " "+files[ifile]
    
    
    cmorphjulian = julday(month,day,2010,hour,0,0)
    
    if abs(julian-cmorphjulian) gt 0.3/24. then continue
    
    zeta = image
    
    ; moved messy temperature calibration conversion to another program
    if strmatch(type, 'Cloud*') eq 1 then zeta = goes(image)
    
    snapshot = zeta
    snapshot = (snapshot-40.)*(-255./130.)
    
    nlon = (size(zeta))[1]
    nlat = (size(zeta))[2]
    
    ; get storm center
    ; use the MTM track files
    
    if type ne 'debug' then mtm_center, pgi, datestring, imageTime, clat, clon, best_track=best_track, mtm00=mtm00
    if NOT FINITE(clat) then begin
      print, "no mtm center found, skipping time"
      continue
    endif
    
    ; I think the iclon and iclat are reversed from what mcidas_nav documentation specifies.
    mcidas_nav,nav,area,clat,clon,iclon,iclat,/ll2ij
    
    print, iclon,clon,iclat,clat,format='("iclon=",I6," (",F9.3,") iclat=",I6, " (",F8.3,")")'
    ;    print, "nlon=",nlon," nlat=",nlat
    
    r     = replicate(!VALUES.F_NAN,nlon,nlat)
    az    = replicate(!VALUES.F_NAN,nlon,nlat)
    lat2d = replicate(!VALUES.F_NAN,nlon,nlat)
    lon2d = replicate(!VALUES.F_NAN,nlon,nlat)
    thisthreshold = replicate(0., nx, ny)
    thisn    = replicate(0., nx, ny)
    
    increment = 1L
    progress = 0.
    
    ; save some time by finding i,j coordinates of a box that encompasses the storm.  it is approximately 20x20 deg.
    ; Restrict the complicated calculations of lat/lon, dist/az to that box.  This cuts down computation time.
    
    mcidas_nav,nav,area,0,0,ullat,ullon,/ij2ll
    mcidas_nav,nav,area,nlon-1,0,urlat,urlon,/ij2ll
    mcidas_nav,nav,area,nlon-1,nlat-1,lrlat,lrlon,/ij2ll
    mcidas_nav,nav,area,0,nlat-1,lllat,lllon,/ij2ll
    
    dlon = (lrlon - lllon) / (nlon - 1)
    startlon = max([(clon - lllon) / dlon - 8./dlon, 0])
    endlon   = min([(clon - lllon) / dlon + 9./dlon, nlon-1])
    dlat = (lllat - ullat) / (nlat - 1)
    startlat = max([(clat - ullat) / dlat + 8.4/dlat, 0])
    endlat   = min([(clat - ullat) / dlat - 6.5/dlat, nlat-1])
    
    ringcolor = min(zeta, /nan)
    savfile = '/Volumes/pecan2/ahijevyc/PREDICT/tmp/'+date+hour+'.sav'
    if file_test(savfile) eq 1 then restore, savfile else begin
      for i=startlon[0],endlon[0],increment do begin
        ; this negation of the increment may be a kludge for SST.
        for j=startlat[0],endlat[0],type eq 'SST' ? -increment : increment do begin
          mcidas_nav,nav,area,i,j,lat1,lon1,/ij2ll
          lat2d[i,j] = lat1
          lon2d[i,j] = lon1
          if sqrt((lat1-clat)^2.+(lon1-clon)^2.) gt 9 then continue
          ;result = map_2points(lon[i,j], lat[i,j], lon[iclon,iclat], lat[iclon,iclat])
          ; map_2points returns distance and azimuth of great circle connecting the two points, P0 to P1,
          result = map_2points(clon, clat, lon1, lat1)
          ; az is degrees east of north (meteorological)
          A = 6371.
          r[i,j] = result[0] * !DTOR * A
          az[i,j] = result[1]
          x = sin(az[i,j]*!DTOR) * r[i,j]
          y = cos(az[i,j]*!DTOR) * r[i,j]
          ix = floor((x + maxr) / dx)
          iy = floor((y + maxr) / dy)
          if ix ge 0 and ix lt nx and iy ge 0 and iy lt ny then begin
            thisthreshold[ix,iy] = thisthreshold[ix,iy] + zeta[i,j]
            thisn[ix,iy] = thisn[ix,iy] + 1
          endif
          ; Draw box around Cartesian grid domain
          if ix eq -1 or iy eq -1 or ix eq nx or iy eq ny then snapshot[i,j] = ringcolor
          if long(i)*j gt progress*nlon*nlat then begin
            print, progress*100, format='(I2,"%",$)'
            progress = progress + 0.1
          endif
        endfor
      endfor
      
      
      
      save, lat2d, lon2d, r, az, thisthreshold, thisn, snapshot, filename=savfile, /verbose
    endelse
    
    files2 = file_search('/Volumes/pecan2/ahijevyc/PREDICT/CMORPH/2010'+date+'_3hr*comb')
    if n_elements(files2) ne 1 then stop
    expanded_files = replicate(files2[0], 8)
    for j = 1, n_elements(files2)-1 do expanded_files = [expanded_files, replicate(files2[j],8)]
    files2 = expanded_files[hour/3]
    junk = cmorph(date, hour + '0000')
    cmorph = junk.data
    cmlon = junk.lon
    cmlat = junk.lat
    cmnlon = n_elements(cmlon)
    cmnlat = n_elements(cmlat)
    
    cr     = replicate(!VALUES.F_NAN,cmnlon,cmnlat)
    caz    = replicate(!VALUES.F_NAN,cmnlon,cmnlat)
    clat2d = replicate(!VALUES.F_NAN,cmnlon,cmnlat)
    clon2d = replicate(!VALUES.F_NAN,cmnlon,cmnlat)
    cthisthreshold = replicate(0., nx, ny)
    cthisn    = replicate(0., nx, ny)
    
    
    lonerr = min(abs(cmlon - clon), cmiclon)
    laterr = min(abs(cmlat - clat), cmiclat)
    
    ullat = cmlat[0]        & ullon = cmlon[0]
    urlat = cmlat[0]        & urlon = cmlon[cmnlon-1]
    lrlat = cmlat[cmnlat-1] & lrlon = cmlon[cmnlon-1]
    lllat = cmlat[cmnlat-1] & lllon = cmlon[0]
    
    dlon = (lrlon - lllon) / (cmnlon - 1)
    startlon = 900
    endlon   = cmnlon-1
    dlat = (lllat - ullat) / (cmnlat - 1)
    startlat = 10
    endlat   = 240
    
    ringcolor = max(zeta, /nan)
    for i=startlon[0],endlon[0],increment do begin
      ; this negation of the increment may be a kludge for SST.
      for j=startlat[0],endlat[0], increment do begin
        lat1 = cmlat[j]
        lon1 = cmlon[i]
        clat2d[i,j] = lat1
        clon2d[i,j] = lon1
        ;result = map_2points(lon[i,j], lat[i,j], lon[iclon,iclat], lat[iclon,iclat])
        ; map_2points returns distance and azimuth of great circle connecting the two points, P0 to P1,
        result = map_2points(clon, clat, 360+lon1, lat1)
        ; az is degrees east of north (meteorological)
        A = 6371.
        cr[i,j] = result[0] * !DTOR * A
        caz[i,j] = result[1]
        x = sin(caz[i,j]*!DTOR) * cr[i,j]
        y = cos(caz[i,j]*!DTOR) * cr[i,j]
        ix = floor((x + maxr) / dx)
        iy = floor((y + maxr) / dy)
        if ix ge 0 and ix lt nx and iy ge 0 and iy lt ny then begin
          cthisthreshold[ix,iy] = cthisthreshold[ix,iy] + cmorph[i,j]
          cthisn[ix,iy] = cthisn[ix,iy] + 1
        endif
      endfor
    endfor
    
    
    ; get centered image of storm
    centered_image_dx = 200
    centered_image_dy = 200
    centered_left   = max([0, round(centered_image_dx-iclon)])
    left            = max([0, round(iclon-centered_image_dx)])
    right           = min([nlon-1, round(iclon+centered_image_dx)])
    centered_bottom = max([0, round(centered_image_dy-iclat)])
    bottom          = max([0, round(iclat-centered_image_dy)])
    top             = min([nlat-1, round(iclat+centered_image_dy)])
    centered_image  = fltarr(1+2*centered_image_dx, 1+2*centered_image_dy)
    if left le right && bottom le top then centered_image[centered_left:centered_left+right-left, centered_bottom:centered_bottom+top-bottom] = snapshot[left:right, bottom:top]
    freq = thisthreshold
    igood = where(thisn ne 0, ngood)
    if ngood gt 0 then freq[igood] = thisthreshold[igood]/thisn[igood]
    cfreq = cthisthreshold
    igood = where(cthisn ne 0, ngood)
    if ngood gt 0 then cfreq[igood] = cthisthreshold[igood]/cthisn[igood]
    
    if nowindows ne 1 then begin
      set_plot, 'X'
      window, 0, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
      tv, centered_image
      xyouts, centered_image_dx, 12, STRING(month,day,imageTime,clat,clon,format='(I2.2,"/",I2.2," ",I6.6," UTC!C",F7.2,F8.2)'), /device, align=0.5
      
      window, 2, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
      tv, congrid((freq-40.)*(-255./130.),centered_image_dx*2+1,centered_image_dy*2+1)
      
      centered_left   = max([0, round(centered_image_dx-cmiclon)])
      left            = max([0, round(cmiclon-centered_image_dx)])
      right           = min([cmnlon-1, round(cmiclon+centered_image_dx)])
      centered_bottom = max([0, round(centered_image_dy-cmiclat)])
      bottom          = max([0, round(cmiclat-centered_image_dy)])
      top             = min([cmnlat-1, round(cmiclat+centered_image_dy)])
      centered_image  = fltarr(1+2*centered_image_dx, 1+2*centered_image_dy)
      if left le right && bottom le top then centered_image[centered_left:centered_left+right-left, centered_bottom:centered_bottom+top-bottom] = cmorph[left:right, bottom:top]
      window, 1, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
      tv, centered_image*15
      xyouts, centered_image_dx, 12, STRING(month,day,imageTime,clat,clon,format='(I2.2,"/",I2.2," ",I6.6," UTC!C",F7.2,F8.2)'), /device, align=0.5
      
      window, 3, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
      tv, congrid(cfreq*15,centered_image_dx*2+1,centered_image_dy*2+1)
    endif
    
    
    ; Create final Postscript plots of % below temp threshold
    old_device = !D.NAME
    set_plot, 'PS'
    device, /close, /color, ysize=6.5, yoffset=3.5, /inches, bits=8
    
    outfile = outdir + 'images/' + pgi + date + hour + (mtm00?"mtm00":"")+(best_track?"best_track":"")+ "IR_CMORPH.ps"
    if type eq 'SST' or type eq 'CMORPH' then outfile = file_dirname(outfile, /mark_directory) + type + file_basename(outfile)
    device, filename=outfile
    pos = [0.1, 0.1, 0.9, 0.9]
    plot, [0], [0], xrange=[-maxr,maxr], yrange=[-maxr,maxr],xstyle=1,ystyle=1, /nodata, position=pos , /isotropic, $
      xtitle='km' + string(dx,format='(2x,"dx=",I0,"km")'), ytitle='km' + string(dy,format='(2X,"dy=",I0,"km")'), title = type+" "+datestring+" "+imageTime+"!CCMORPH"+date+" "+hour, /norm
      
    if !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0    
    tv, bytscl(freq, min=mintemp, max=maxtemp, /nan), pos[0], pos[1], xsize=!X.window[1]-!X.window[0], ysize=!Y.window[1]-!Y.window[0], /norm
    if !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
    
    xkm = (findgen(nx)+0.5)*dx-maxr
    ykm = (findgen(ny)+0.5)*dy-maxr
    contour, cfreq, xkm, ykm, levels=[1,4,16,64], thick=4, /overplot, /follow
    for range = 50, maxr, 50 do begin
      x = range*cos(!PI*xkm/maxr)
      y = range*sin(!PI*xkm/maxr)
      oplot, x, y
      size = 0.6
      xbox = 38*size
      ybox = 18*size
      if range mod 100 eq 0 then begin
        polyfill, [x[0]-xbox, x[0]+xbox, x[0]+xbox, x[0]-xbox], 15*size+[y[0]-ybox, y[0]-ybox, y[0]+ybox, y[0]+ybox], color=255
        xyouts, x[0], y[0], string(range, format='(I0)'), align=0.5, charsize=size
      endif
    endfor
    
    mycolorbar,range=[mintemp,maxtemp], /vertical,position=[pos[2],pos[1],pos[2]+0.05,pos[3]],title=units,/right,format=format
    
    if debug then print, 'saved '+outfile
    device, /close
    
    set_plot, old_device
  endfor
  
  
  if !D.NAME eq 'PS' then device, /close
end

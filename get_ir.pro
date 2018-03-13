pro get_ir, out, indate, intime, lat, lon, closest_imageJulian, next_closest=next_closest
  basedir = '/Volumes/pecan2/ahijevyc/'
  date = indate
  time = intime
  out = replicate(!VALUES.F_NAN, n_elements(lat))
  
  next_closest = {IR_temp:replicate(!VALUES.F_NAN, n_elements(lat)), imageJulian:!VALUES.D_NAN, IRfile:''}
  
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  julday = julday(month, day, year, hour, minute, second)
  dtime  = 9999.
  dtime2 = 9999.
  closest_IRfile = ''
  closest_imageJulian = -1d
  ; Loop thru all the IR files for this date - may not be the best way to do it for each microwave temperature profile
  if year eq 2010 then SatFiles =  file_search(basedir+'PREDICT/GOES/'+date +'*/Cloud*Temp-TEMP[0-9]*' + ['.area','[0-9].area'])
  if year eq 2013 then SatFiles =  file_search(basedir+   'mpex/GOES/'+date +'/GOES??_WV*.nc')
  
  SatFiles = SatFiles[uniq(SatFiles,sort(SatFiles))] ; the order can change, and if you need to restart with a higher file index, then you want constancy
  
  nfiles = n_elements(SatFiles)
  for iSatFile = 0,nfiles-1 do begin
    SatFile = SatFiles[iSatFile]
    mcidas_read,file=SatFile,noread=1, times=times
    imagejulians = times
    
    
    for itime=0,n_elements(imagejulians)-1 do begin
      imagejulian = imagejulians[itime]
      this_dtime = abs(imagejulian - julday)
      
      if this_dtime lt dtime then begin
        ; this is the best time yet so
        ; push best to next-best
        dtime2 = dtime
        next_closest.IRfile = closest_IRfile
        next_closest.imageJulian = closest_imageJulian
        ; remember this as best now.
        dtime = this_dtime
        closest_IRfile = SatFile
        closest_imageJulian = imagejulian
      endif else if this_dtime lt dtime2 then begin
        ; remember this as next-best now.
        dtime2 = this_dtime
        next_closest.IRfile = Satfile
        next_closest.imageJulian = imageJulian
      endif
    endfor
    
    
  endfor
  
  ; I tried a save file, but it wasn't faster. Ahijevych 1 Mar 2012
  mcidas_read,file=closest_IRfile,image=image,area=area,nav=nav, times=times
  
  if strmid(closest_IRfile, 4, /reverse) eq '.area' then zeta = goes(image) else begin
    k = where(abs(times - closest_imageJulian) lt 0.001, nmatch)
    if nmatch ne 1 then stop
    zeta = image[*,*,k]-!CONST.T0
  endelse
  
  mcidas_nav,nav,area,lat,lon,i,j,/ll2ij
  
  out = zeta[i,j]
  
  ;  print, indate, ' ', intime
  ;  print, closest_IRfile, closest_imageJulian, format='("closest:",A,2X,C())'
  ;  print, next_closest.IRfile, next_closest.imageJulian, format='("next-closest:",A,2X,c())'
  
  mcidas_read,file=next_closest.IRfile,image=image,area=area,nav=nav, times=times
  
  if strmid(next_closest.IRfile, 4, /reverse) eq '.area' then zeta = goes(image) else begin
    k = where(abs(times - next_closest.imageJulian) lt 0.001, nmatch)
    if nmatch ne 1 then stop
    zeta = image[*,*,k]-!CONST.T0
  endelse
  
  mcidas_nav,nav,area,lat,lon,i,j,/ll2ij
  next_closest.IR_temp = zeta[i,j]
  
  return
end

pro compare_all_ir, binsize
  basedir = '/Volumes/pecan2/ahijevyc/'
  SatFiles =  file_search(basedir+'PREDICT/GOES/20100902*/Cloud*Temp-TEMP[0-9]*' + ['.area','[0-9].area'], count=nfiles)
  for ifile=0,nfiles-1 do begin
    file = SatFiles[ifile]
    mcidas_read, file=file, image=image
    image = goes(image)
    y = ifile eq 0 ? histogram(image, locations=xlocs, binsize=binsize, omax=omax, omin=omin) : histogram(image, input=y, binsize=binsize, max=omax, min=omin)
  endfor
  
  SatFiles =  file_search(basedir+'PREDICT/GOES/20100902*/Cloud*Temp-TEMP*.nc', count=nfiles)
  for ifile=0,nfiles-1 do begin
    file = SatFiles[ifile]
    
    mcidas_read, file=file, image=image
    image = image-!CONST.T0
    nc = ifile eq 0 ? histogram(image, binsize=binsize, max=omax, min=omin) : histogram(image, input=nc, binsize=binsize, max=omax, min=omin)
  endfor
  
  plot, xlocs, y/total(y), psym=10
  oplot, xlocs, nc/total(nc), psym=10, color=190
  
end



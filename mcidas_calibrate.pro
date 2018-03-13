pro mcidas_calibrate
  basedir = '/Volumes/pecan2/ahijevyc/mpex/GOES/'
  xs=!NULL
  ys=!NULL
  dates=file_search(basedir+'201?????', count=ndate)
  for idate=0,ndate-1 do begin
    date = dates[idate]
    ncfiles = file_search(date+'/GOES1?_WV.nc', count=nf)
    for incfile = 0, nf-1 do begin
      ncfile = ncfiles[incfile]
      g = strmid(file_basename(ncfile),4,2)
      ncid = ncdf_open(ncfile) ; exported from IDV (contains all times).
      ;IDV nc file may have all times but the dimensions are assumed to be constant, when they are not.
      varids = ncdf_varidsinq(ncid)
      for i=0,n_elements(varids)-1 do if strmatch((ncdf_varinq(ncid, varids[i])).name, 'Band3_TEMP*') then varid=varids[i]
      ncdf_varget, ncid, varid, ncimages
      ncdf_varget, ncid, ncdf_varid(ncid, 'time'), times
      sznc = size(ncimages)
      areafiles =  file_search(date +'/GOES'+g+'_WV*.area', count=nfiles)
      for ifile=0,nfiles-1 do begin
        areafile = areafiles[ifile]
        mcidas_read,file=areafile,image=image,area=area,nav=nav,LINhdr=LINhdr,cmd=cmd
        imageDate = strtrim(string(area.img_date + 1900000L),2)
        imageTime = strtrim(string(area.img_time,FORMAT='(I6.6)'),2)
        imageHour = strmid(imageTime,0,2)
        imageMinute = strmid(imageTime, 2,2)
        ; The year part of imageDate appears to be screwed up (2010 = 110 for some reason) but the day-of-the-year seems fine.
        imageYear = strmid(imageDate, 0, 4)
        imagejulian = julday(1,1,imageYear,0,0,0) + long(strmid(imageDate,2,3,/reverse)) - 1 + imageHour/24. + imageMinute/24./60.
        secs = 24*3600L* (imagejulian - julday(1,1,1970,0,0,0))
        itime = where(round(secs) eq times, nmatch)
        if nmatch eq 0 then continue
        
        sz1 = size(image)
        if array_equal(sz1[1:2], sznc[1:2]) ne 1 then begin
          ;print, areafile, ' different image size'
        endif
        x = image[*]
        y = (ncimages[*,*,itime])[*]
        n = min([n_elements(x),n_elements(y)])
        x = x[0:n-1]
        y = y[0:n-1]
        xs = [xs, x]
        ys = [ys, y]
        result = linfit(x,y)
        print, areafile, result, format='(A,F,F)'
      endfor
      result = linfit(xs,ys)
      plot, xs, ys, psym=3, nsum=n_elements(xs)/100
      oplot, !X.CRANGE, result[0]+!X.CRANGE*result[1]
      print, result, format='(F,F)'
    endfor
  endfor
  i=where(xs ne 0 and ys ne 0,nz)
  if nz eq 0 then stop
  plot, xs[i], ys[i], psym=3, nsum=nz/100
  result = linfit(xs[i],ys[i])
  oplot, !X.CRANGE, result[0]+!X.CRANGE*result[1]
  print, result, format='(F,F)'
  
end


pro try_nc
  file = '/Volumes/pecan2/ahijevyc/mpex/GOES/20130516/GOES13_WV.nc'
  result = readAREAnc(file, area=area, nav=nav, images=images, lat=lat, lon=lon)
  
  ncid = ncdf_open(file, /write)
  
  ncdf_control, ncid, /redef
  ie = ncdf_dimdef(ncid, 'ImageElement', n_elements(ImageElement))
  if ie eq -1L then ie = ncdf_dimid(ncid, 'ImageElement')
  il = ncdf_dimdef(ncid, 'ImageLine', n_elements(ImageLine))
  if il eq -1L then il = ncdf_dimid(ncid, 'ImageLine')
  latid = ncdf_vardef(ncid, 'lat', [ie, il], /double)
  if latid eq -1L then latid = ncdf_varid(ncid, 'lat')
  ncdf_attput, ncid, latid, 'long_name', 'latitude'
  ncdf_attput, ncid, latid, 'units', 'degrees_north'
  ncdf_attput, ncid, latid, '_FillValue', min(lat)
  lonid = ncdf_vardef(ncid, 'lon', [ie, il], /double)
  if lonid eq -1L then lonid = ncdf_varid(ncid, 'lon')
  ncdf_attput, ncid, lonid, 'long_name', 'longitude'
  NCDF_ATTPUT, ncid, lonID, 'units', 'degrees_east'
  ncdf_attput, ncid, lonid, '_FillValue', min(lon)
  ncdf_control, ncid, /endef
  
  ncdf_varput, ncid, latid, lat
  ncdf_varput, ncid, lonid, lon
  ncdf_close, ncid
  
  if ( 1) then begin
    ; check nav header and area header are the same as what you would get if you read the area file.
    file = strmid(file,0,strlen(file)-3)+'0.area'
    if file_test(file) eq 0 then stop
    mcidas_read, file=file,    images=images2,    AREAstr=AREA2,  NAVstr=NAV2
    help, compare_struct(area,area2), compare_struct(nav, nav2), /str
  endif
  
  if (0) then begin
    res = 0.03d
    width = 22d
    n = width/res
    lat0 = 32.5d
    lon0 = -102d
    lat = dindgen(n)*res + lat0 - width/2.
    lon = dindgen(n)*res + lon0 - width/2.
    lon = replicate(1,n)#lon
    lat = lat#replicate(1,n)
    mcidas_nav,nav,area,lat,lon,i,j,/ll2ij
    x = images[*,*,0]
    x[i,j] = 0
    tvscl, x
    for k=0,24 do tv, 5.9*(images[*,*,k])[i,j]+23
  endif
  
  stop
end
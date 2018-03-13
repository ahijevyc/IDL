pro color_stuff
  tvlct, red, g, b, /get
  tr = congrid([20.,-80.], 255,/interp, /minus)
  ;    yellow
  
  iyellow = where(tr le -80)
  red[iyellow] = 255
  g[iyellow] = 255
  b[iyellow] = 0
  ; for red, blue and green do some gradations.The primary color is still the same, but there is some gradient now.
  ired = where(tr gt -80 and tr le -70,nred)
  red[ired] = 255-reverse(bindgen(nred))
  g[ired] = 2*bindgen(nred)
  b[ired] = 2*bindgen(nred)
  igreen = where(tr gt -70 and tr le -60,ngreen)
  red[igreen] = 2.*bindgen(ngreen)
  g[igreen] = 255-reverse(2*bindgen(ngreen))
  b[igreen] = 2.*bindgen(ngreen)
  iblue = where(tr gt -60 and tr le -50,nblue)
  red[iblue] = 2.*(bindgen(nblue)-nblue/2.)+127
  g[iblue] = 2.*(bindgen(nblue)-nblue/2.)+127
  b[iblue] = 255
  
  tg = min(iblue) ; top gray index
  red[0:tg-1] = findgen(tg)*236./(tg-1)
  g[0:tg-1] = findgen(tg)*236./(tg-1)
  b[0:tg-1] = findgen(tg)*236./(tg-1)
  tvlct, red, g, b
end

pro add_arrow, x, y, arrow_count, label=label, color=color
  s = sqrt(x^2.+y^2.)
  tvlct, oldct, /get
  if keyword_set(color) then tvlct, transpose(color), 0
  scale = 50./s + 23. ; multiply original vector length by 23 and add 50km to length
  arrow, 0., 0., x*scale, y*scale, /data, thick=10, hthick=8, /solid
  scale = -5./s + 23.; multiply original vector length by 23 and add subtract 5km to length
  if keyword_set(label) then begin
    xyouts, x*scale, y*scale, string(s,format='(F3.0)')+label, color=255, align=0.5, charsize=0.7
    legendx = total(!X.WINDOW)*0.57+arrow_count*0.13
    legendy = !Y.WINDOW[0]*0.3
    two_liner = strpos(label,"!C") ne -1
    h = two_liner ? 0.026 : 0.013
    xyouts, legendx, legendy, string(s,format='(F3.0)')+label, align=0.5, charsize=1.1, /norm, width=w
    legendpos = [legendx-w/2.,legendy-h,legendx+w/2.,legendy+h]
    polyfill, [legendpos[0], replicate(legendpos[2],2), legendpos[0]],[replicate(legendpos[1],2),replicate(legendpos[3],2)], /norm
    xyouts, mean(legendpos[[0,2]]), legendpos[3]-0.022, string(s,format='(F3.0)')+label, color=255, align=0.5, charsize=1.1, charthick=!P.CHARTHICK*1.8, /norm
  endif
  tvlct, oldct
  arrow_count = arrow_count + 1
end

pro add_drops, drops_structure, imission, sat=sat, rh=plot_rh, temp=plot_t, td=plot_td, stid=plot_stid, overflow=overflow
  overflow=0
  if imission eq -1 then return
  if ~keyword_set(sat) then sat = 0
  if ~keyword_set(plot_rh) then plot_rh = 0
  if ~keyword_set(plot_t) then plot_t = 0
  if ~keyword_set(plot_td) then plot_td = 0
  if ~keyword_set(plot_stid) then plot_stid = 0
  tags = tag_names(drops_structure)
  gfs = max(strmatch(tags, 'xoffset',/fold)) eq 0
  ndrops  = drops_structure.ndrops[imission]
  xdrops  = drops_structure.xdrops
  ydrops  = drops_structure.ydrops
  radiusdrops = drops_structure.radiusdrops
  stiddrops   = drops_structure.stiddrops
  tdrops  = drops_structure.tdrops
  tddrops = drops_structure.tddrops
  udrops  = drops_structure.udrops
  vdrops  = drops_structure.vdrops
  press   = drops_structure.PRESS
  rhdrops = drops_structure.rhdrops
  
  ; calculate Tv virtual temperature.
  ; water vapor pressure
  edrops = e_h2o(tddrops+!CONST.T0,minval=56) ; output in mb
  ; virtual temperature
  epsilon = 287./461. ; Rd/Rv
  tvdrops = tdrops/(1. - (edrops*100./press) * (1.-epsilon) ) ; from wallace and hobbs p 52.
  
  
  
  ; make GFS soundings smaller
  scale = gfs ? 0.5 : 1
  if sat then scale = 0.7
  
  ; use star for satellite cloud drift winds.
  ang = (360. / 10 * findgen(11) + 90) / !RADEG  ;star angles every 36 deg
  r = ang*0
  r[2*indgen(6)] = 1.
  cp5 = cos(!pi/5.)
  r1 = 2. * cp5 - 1. / cp5
  r[2*indgen(5)+1] = r1
  r = r * scale / sqrt(!pi/4.) * 2. / (1.+r1)
  xarr = r * cos(ang)   &   yarr = r * sin(ang)
  
  usersym, xarr, yarr, FILL = 1,thick=scale*1.1
  
  barbcolor=gfs?0:2
  if sat then barbcolor=226
  
  
  ; use the last 4 characters in the STID .
  ;  if ~gfs then stiddrops = strmid(stiddrops,3,4,/reverse)
  
  tvlct, oldct, /get
  loadct, 39, silent=1
  dk_orange = transpose([200,10,20])
  dk_red = transpose([195,0,10])
  tvlct, dk_orange, 1
  tvlct, dk_red, 2
  
  for idrop = 0, ndrops-1 do begin
    ; plot a dot in case winds are missing
    plots, xdrops[idrop,imission], ydrops[idrop,imission], psym=sat?8:3, color=barbcolor, noclip=0
    t  = tvdrops[idrop,imission] ; or tdrops (virtual temperature or temperature)
    rh = rhdrops[idrop,imission]
    td = tddrops[idrop,imission]
    stid = stiddrops[idrop,imission]
    
    ; using the I0 format truncates the decimal later. Round up, if necessary, first.
    if plot_t    eq 1 and finite(t)  and ~gfs then xyouts, xdrops[idrop,imission]-scale*60, ydrops[idrop,imission]+scale*10., string( round(t), format='(I3)'), charsize=scale*1.24, color=1, charthick=scale*4.02, align=0.5, noclip=0
    if plot_rh   eq 1 and finite(rh) and ~gfs then xyouts, xdrops[idrop,imission]-scale*70, ydrops[idrop,imission]-scale*38., string( round(rh),format='(I0)'), charsize=scale*1.33, color=gfs?47:55, charthick=scale*4, noclip=0
    if plot_td   eq 1 and finite(td)          then xyouts, xdrops[idrop,imission]-scale*70, ydrops[idrop,imission]-scale*30., string( round(td),format='(I3)'), charsize=scale*1.33, color=gfs?47:55, charthick=scale*4, noclip=0
    if plot_stid eq 1 and stid ne ''          then xyouts, xdrops[idrop,imission],          ydrops[idrop,imission]-scale*25., stid, align=0.3, charsize=scale*0.56, color=10, charthick=scale*0.73, noclip=gfs?0:1
    u = udrops[idrop,imission]
    v = vdrops[idrop,imission]
    if u*u+v*v gt 1e10 then overflow=1
    wspeed = sqrt(u*u+v*v)
    direction = -90. - atan(v,u)*!RADEG
    if finite(wspeed) and finite(direction) then begin
      if ~sat then wind_barb, wspeed, direction, xdrops[idrop,imission], ydrops[idrop,imission], size=scale*9, thick=scale*1.05, color=barbcolor, noclip=0
      wind_barb, wspeed, direction, xdrops[idrop,imission], ydrops[idrop,imission], size=scale*100, thick=scale*3.7, color=barbcolor, /nocircle, noclip=0
    endif
  endfor
  tvlct, oldct
  
end

pro add_rh_contour, rhdrops, xdrops, ydrops, pos
  tvlct, oldct, /get
  green = transpose([49,219,128])
  tvlct, green, 13
  dkgreen = transpose([9,179,88])
  tvlct, dkgreen, 14
  ltbrown = transpose([249,219,128])
  tvlct, ltbrown, 3
  brown = transpose([219,189,98])
  tvlct, brown, 2
  dkbrown = transpose([179,149,58])
  tvlct, dkbrown, 1
  levels=[0,10,20,30,80,90]
  colors=[1,2,3,255,13,14]
  igood = where(finite(rhdrops),ngood)
  if ngood gt 0 then begin
    contour, rhdrops[igood], xdrops[igood], ydrops[igood], /irr, /overplot, noclip=0, levels=levels, c_colors=colors, /fill
  ; repair the axes that were overwritten by the filled contour
  ;    axis, xaxis=0, xrange=!X.CRANGE, xstyle=1, xtickformat='(A1)'
  ;    axis, xaxis=1, xrange=!X.CRANGE, xstyle=1, xtickformat='(A1)'
  ;    axis, yaxis=0, yrange=!Y.CRANGE, ystyle=1, ytickformat='(A1)'
  ;    axis, yaxis=1, yrange=!Y.CRANGE, ystyle=1, ytickformat='(A1)'
  endif
  contourbar, levels, colors, title = 'relative humidity %', position=[0.6*(pos[0]+pos[2]),0.028,pos[2],pos[1]/3.5], charsize=0.85, format= '(I0,"%")'
  tvlct, oldct
end



pro calc_rings, pgi, mtm00=mtm00, best_track=best_track, date=date, start_hr=start_hr, debug=debug, type=type, use_trop_tzero=use_trop_tzero, $
    time_window_days=time_window_days, threshold_total=threshold_total, threshold_n=threshold_n, pixel_total=pixel_total, title=title
    
  ; copied from vort_rings.pro
  if ~keyword_set(debug) then debug=0
  if ~keyword_set(use_trop_tzero) then use_trop_tzero = 0 ; could be 1 for tropical depression and 2 for tropical storm
  if ~keyword_set(time_window_days) then time_window_days = 1d
  force_new = 0 ; before setting force_new to 1, consider deleting the old save files.
  nowindows=1; if nowindows eq 1 then the "zoom" png won't get remade
  incomplete = 0 ; set to non-zero if you don't want to make a save file.
  if ~keyword_set(type) then begin
    ;  type = 'Vis'
    type = 'Cloud*Temp-TEMP*' ; if you add an * after 'TEMP' then you will get two of everything
  ;  type = 'debug';  type = 'WV'
  ;  type = 'SST'
  ;type = 'CMORPH'
  endif
  !P.THICK = 1.0
  !P.CHARTHICK = 1.0
  if ~keyword_set(start_hr) then start_hr = 2.99999
  trop_string = '' ; string to tag directories or files
  if keyword_set(date) then begin
    if strmatch(date, '[12][0-9][0-9][0-9][012][0-9][0123][0-9]') eq 0 then message, "date must be yyyymmdd format. got "+ date
    year  = strmid(date, 0, 4)
    month = strmid(date, 4, 2)
    day   = strmid(date, 6, 2)
    if use_trop_tzero eq 1 or use_trop_tzero eq 2 then begin
      first_trop_time = tropical_reference_time(pgi, use_trop_tzero)
      if not finite(first_trop_time) then message, 'requested tropical time for '+pgi+' but it was not found
      caldat, first_trop_time, month3, day3, year3, start_hr
    endif
    if use_trop_tzero eq 3 then begin
      t = get_PREDICT_missions()
    endif
    start_julian = julday(month, day, year, start_hr, 0, 0)
    if use_trop_tzero eq 1 or use_trop_tzero eq 2 then begin
      tropical_day = start_julian - first_trop_time
      if use_trop_tzero eq 1 then trop_string = 'depress'
      if use_trop_tzero eq 2 then trop_string = 'storm'
      tropical_day_string = "Trop " + trop_string + " at " + string(month3,day3,year3,start_hr,tropical_day, format='(I0,"/",I0,"/",I0," ",I0,"UTC"," ",I+0)') + " day"
    endif
    julday2 = start_julian+1d
    caldat, julday2, month2, day2, year2
    date2 = string(year2,month2,day2,format='(I4.4,I2.2,I2.2)')
  endif
  
  ; uncomment to produce 6-hrly lat/lons for Tom G.
  ;  for hr6=0,18,6 do begin
  ;  mtm_center, pgi, date, string(hr6,format='(I2.2,"0000")'), clat, clon, mtm00=mtm00, best_track=best_track, /silent
  ;  print, pgi,year,month,day,hr6,clat,clon,format='(A8,",",I4,",",I2.2,",",I2.2,",",I2.2,",",F8.2,",",F8.2)'
  ;  endfor
  ;  return
  if debug ge 1 then print, start_hr, date, date2, format='("start_hr=",F6.3," date=",A," date2=",A)'
  
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  if type eq 'Vis' or type eq 'Cloud*Temp-TEMP*' or type eq 'WV' then begin
    ; Look for .area files
    ; Look for single-digit indices first, then double-digit because IDV did not pad the indices with zeroes.
    ; Be aware the numbering is random, as far as I can tell, not chronological, i.e. 0 does not correspond to the first time.
    ; Therefore find the actual time and use it in the output file name.
    files = file_search(basedir+'PREDICT/GOES/20100[89][0123][0-9]*/' + type + '*' + ['','[0-9]'] + '[0-9].area')
    if keyword_set(date) then begin
      files =         file_search(basedir+'PREDICT/GOES/'+date +'*/' + type + '*' + ['','[0-9]'] + '[0-9].area')
      files = [files, file_search(basedir+'PREDICT/GOES/'+date2+'*/' + type + '*' + ['','[0-9]'] + '[0-9].area')]
    endif
    intensity_time_window_hrs = 0.5
    outdir = basedir+'PREDICT/GOES/pouch_stats/'
  endif
  if type eq 'SST' then begin
    files = file_search(basedir+'PREDICT/SST/*-NCDC-L4LRblend-GLOB*OI.nc')
    if keyword_set(date) then files = file_search(basedir+'PREDICT/SST/'+[date,date2] +'*-NCDC-L4LRblend-GLOB*OI.nc')
    intensity_time_window_hrs = 24.0
    outdir = basedir+'PREDICT/SST/pouch_stats/'
  endif
  if type eq 'CMORPH' then begin
    files = file_search(basedir+'PREDICT/CMORPH/*_3hr-025deg_cpc+comb')
    if keyword_set(date) then files = file_search(basedir+'PREDICT/CMORPH/'+[date,date2] +'_3hr-025deg_cpc+comb')
    intensity_time_window_hrs = 3.0
    outdir = basedir+'PREDICT/CMORPH/pouch_stats/'
  endif
  files = files[uniq(files,sort(files))] ; the order can change, and if you need to restart with a higher file index, then you want constancy
  if n_elements(files) eq 1 and files[0] eq '' then return
  if type eq 'CMORPH' then begin
    ; Clunky way to deal with CMORPH files, which are actually 8 times rolled into one file.
    expanded_files = replicate(files[0], 8)
    for j = 1, n_elements(files)-1 do expanded_files = [expanded_files, replicate(files[j],8)]
    files = expanded_files
  endif
  
  if !D.NAME eq 'PS' then device, /close, /color, ysize=5, yoffset=5, /inches
  if !D.NAME eq 'X' and !D.WINDOW ne -1 then wset, 0
  if nowindows eq 0 then begin
    set_plot, 'X'
    device, decomposed=0
    window, 0, xsize=500,ysize=400
  endif
  loadct, 0, silent=debug?0:1
  if strmatch(type, 'Cloud*') eq 1 then color_stuff
  
  if ~keyword_set(best_track) then best_track = 0
  ; if mtm=1, use mtm track files at 00 UTC consensus model analysis time and interpolate between them. Don't use the 1-XX h forecasts.
  if ~keyword_set(mtm00) then mtm00=0
  
  savfile = outdir + 'savfiles/' +pgi+(mtm00?"mtm00":"")+(best_track?"best_track":"")+(keyword_set(date)?date:'')+$
    (keyword_set(start_hr)?string(start_hr,format='("_",I2.2)'):'')+(keyword_set(time_window_days)?string(time_window_days*24,format='("_",I2.2,"h")'):'')$
    (use_trop_tzero eq 0?'':trop_string+string(tropical_day,format='(I+0)'))+".sav"
    
    
  maxr = 870 ; used to be 720, but for 0912 and 0921, the drops are far east of center and stretched the domain beyond the 720 boundaries
  if debug eq 1 then maxr = 200
  if type eq 'Vis' then maxr=500
  
  dx = 10. ; km
  dy = 10. ; km
  if type eq 'SST' or type eq 'CMORPH' then dx = 30.
  if type eq 'SST' or type eq 'CMORPH' then dy = 30.
  nx = round(2*maxr/dx)
  ny = round(2*maxr/dy)
  thresholds  = [-80,-70,-60,-50,-40,-30,-20]
  hist_range = [-90.,30.]
  hist_binsize = 1. ; 10 for figures - Must have done that a long long time ago. 
  units = 'C'
  abovebelow = 'below'
  if type eq 'SST' then begin
    thresholds = [28] ; C
    abovebelow = 'above'
  endif
  if type eq 'CMORPH' then begin
    thresholds = [1] ; mm/hr
    units = 'mm_per_h'
    abovebelow = 'above'
  endif
  pixel_total_units = 'h*'+units ; weird, I know, but technically right. I multiply the pixels by intensity_time_window_hrs down below.
  ; 'mm/h' becomes 'h*mm/h', or 'mm'
  nthresholds = n_elements(thresholds)
  ; initalize arrays if not provided in keywords
  if ~keyword_set(threshold_total) then threshold_total = replicate(0., nx, ny, nthresholds)
  if ~keyword_set(threshold_n)     then threshold_n     = replicate(0., nx, ny, nthresholds)
  if ~keyword_set(pixel_total)     then pixel_total     = replicate(0., nx, ny)
  nvalid          = replicate(0., nx, ny)
  mintime = 9999999d
  maxtime = 0d
  old_threshold_total = threshold_total
  old_threshold_n = threshold_n
  old_pixel_total = pixel_total
  
  
  for ifile = 0,n_elements(files)-1 do begin
    if file_test(savfile) and force_new eq 0 then begin
      restore, savfile
      threshold_total = threshold_total + old_threshold_total
      pixel_total = pixel_total + old_pixel_total
      threshold_n = threshold_n + old_threshold_n
      continue
    endif
    
    file = files[ifile]
    if file eq '' then continue; file_search returns '' if it does not find any files
    if n_elements(julian) ne 0 then julian_prev = julian
    if ~File_test(file) then message, 'file '+file+' not found'
    
    if strmatch(file,'*area*.nc') eq 1 then begin
      ncid = NCDF_OPEN(file)            ; Open The NetCDF file
      NCDF_VARGET, ncid, "imageDate", imageDate
      NCDF_VARGET, ncid, "imageTime", imageTime
      NCDF_VARGET, ncid, "image", image
      NCDF_VARGET, ncid, "longitude", lon
      NCDF_VARGET, ncid, "latitude", lat
      NCDF_CLOSE, ncid      ; Close the NetCDF file
    endif else if type eq 'CMORPH' then begin
      imageDate = strmid(file_basename(file),0,8)
      month = strmid(imageDate,4,2)
      day   = strmid(imageDate,6,2)
      imageTime = string((ifile mod 8)*30000L, format='(I6.6)')
      junk = cmorph(imageDate, imageTime, '3hr')
      image = junk.data
      lon = junk.lon
      lat = junk.lat
      nlon = n_elements(lon)
      nlat = n_elements(lat)
      julian = julday(month,day,2010,(ifile mod 8)*3L,0,0)
    endif else if type eq 'SST' then begin
      ncid = NCDF_OPEN(file)
      NCDF_VARGET, ncid, "lat", lat
      NCDF_VARGET, ncid, "lon", lon
      NCDF_VARGET, ncid, "time", time
      NCDF_VARGET, ncid, "analysed_sst", image
      NCDF_ATTGET, ncid, "analysed_sst", "add_offset", add_offset
      NCDF_ATTGET, ncid, "analysed_sst", "scale_factor", scale_factor
      NCDF_ATTGET, ncid, "analysed_sst", "_FillValue", FillValue
      ibad = where(image eq FillValue, nbad)
      image = image * scale_factor + add_offset ; Kelvin
      if nbad gt 0 then image[ibad] = !VALUES.F_NAN
      NCDF_CLOSE, ncid
      nlon = n_elements(lon)
      nlat = n_elements(lat)
      julian = julday(1,1,1981,0,0,0) + time/24d/3600d
      caldat, julian, month, day, year, hour, minute, second
      imageTime = string(hour,minute,second, format='(I2.2,I2.2,I2.2)')
    endif else begin
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
      mcidas_latlon_savfile = file_dirname(file)+'/'+pgi+file_basename(file) + '_latlon.idlsav'
    endelse
    
    
    
    ; make sure you skip this time before opening up a text file - otherwise it will overwrite and make empty if it exists.
    datestring = string(year,month,day,format='(I4.4,I2.2,I2.2)')
    
    
    
    ; time windows are enforced here
    time_separation = julian - start_julian
    ; If this is a daily composite, then julian (data time) must be >= 0 and < 24 hours older.
    if keyword_set(date) and (time_separation lt 0d or time_separation ge time_window_days) then continue ; or continue if you only want these times done
    
    
    
    ;---------------------
    ; get storm center
    ;---------------------
    ; use the MTM track files
    if type ne 'debug' then mtm_center, pgi, datestring, imageTime, clat, clon, best_track=best_track, mtm00=mtm00, silent=debug?0:1
    if NOT FINITE(clat) then begin
      print, "no mtm center found for "+pgi+" "+datestring+" "+imageTime+". skipping time"
      continue
    endif
    
    
    
    
    print, datestring+imageTime
    if debug then print,  "data file= "+files[ifile]
    
    ; Bunch of debugging shortcuts.  We don't want to create a save file if we use a shortcut, so set incomplete = 1.
    ;    if strmatch(file, '/Volumes/pecan2/ahijevyc/PREDICT/GOES/20100821*/Cloud-topTemp-TEMP19.area') eq 0 then begin
    ;      incomplete = 1
    ;      continue
    ;    endif
    ;    if ifile gt 18 then begin
    ;      incomplete = 1
    ;      continue
    ;    endif
    ;    if imageTime ne '134500' then begin
    ;      incomplete = 1
    ;      continue
    ;    endif
    
    if julian lt mintime then mintime = julian
    if julian gt maxtime then maxtime = julian
    if n_elements(julian_prev) gt 0 then intensity_time_window_hrs = (julian - julian_prev)*24.0
    
    outtxt = outdir + pgi+datestring+STRING(imageTime, format='("_",i6.6)')+(mtm00?"mtm00":"")+(best_track ? "best_track" : "")+ ".txt"
    if type eq 'SST' or type eq 'CMORPH' then outtxt = file_dirname(outtxt, /mark_directory) + type + file_basename(outtxt)
    outtxt_info = file_info(outtxt)
    if debug gt 0 and outtxt_info.size gt 0 then print, 'overwriting ',outtxt_info.size,' byte ' + outtxt
    
    if debug gt 0 then print, ifile, format='("ifile=",I0)'
    
    zeta = image
    if type eq 'SST' then zeta = zeta - !CONST.T0
    ; moved messy temperature calibration conversion to another program
    if strmatch(type, 'Cloud*') eq 1 then zeta = goes(image)
    
    scale = 4
    if type eq 'debug' then scale = 1
    
    small_image_nx = nlon/scale
    small_image_ny = nlat/scale
    tvcoordx = !D.X_SIZE-small_image_nx & tvcoordy = !D.Y_SIZE-small_image_ny
    snapshot = zeta
    if strmatch(type, 'Cloud*') eq 1 then snapshot = bytscl(-snapshot, min=-20, max=80,top=254, /nan)
    if type eq 'CMORPH'              then snapshot = SQRT(snapshot) * 100
    if type eq 'SST'                 then snapshot = snapshot*(255./50.)
    
    if nowindows eq 0 then begin
      snapshot0 = snapshot; need a copy for possible flipping
      iNAN = where(~finite(snapshot0),nNaN)
      if nNaN gt 0 then snapshot0[iNaN] = min(snapshot0,/nan)
      ; I used to flip snapshot, but this messes up i,j references later on when I try to draw a box and rings on snapshot
      if strmatch(type, 'Cloud*') eq 1 then snapshot0 = rotate(snapshot0,7)
      tv, congrid(snapshot0,small_image_nx,small_image_ny), tvcoordx, tvcoordy, /device
      plot, xvals, histogram(zeta, locations=xvals, /nan),psym=10, /noerase
    endif
    
    nlon = (size(zeta))[1]
    nlat = (size(zeta))[2]
    
    if strmatch(type, 'Cloud*') eq 1 then begin
      ; I think the iclon and iclat are reversed from what mcidas_nav documentation specifies.
      mcidas_nav,nav,area,clat,clon,iclon,iclat,/ll2ij
    endif else begin
      lonerr = min(abs(lon - clon), iclon)
      laterr = min(abs(lat - clat), iclat)
    endelse
    if debug gt 0 then print, iclon,clon,iclat,clat,format='("iclon=",I6," (",F9.3,") iclat=",I6, " (",F8.3,")")'
    ;    print, "nlon=",nlon," nlat=",nlat
    
    r     = replicate(!VALUES.F_NAN,nlon,nlat)
    az    = replicate(!VALUES.F_NAN,nlon,nlat)
    lat2d = replicate(!VALUES.F_NAN,nlon,nlat)
    lon2d = replicate(!VALUES.F_NAN,nlon,nlat)
    thisthreshold         = replicate(0., nx, ny, nthresholds)
    this_threshold_nvalid = replicate(0., nx, ny, nthresholds)
    this_nvalid           = replicate(0., nx, ny)
    this_pixel_total      = replicate(0., nx, ny)
    
    increment = 1L
    progress = 0.
    
    ; save some time by finding i,j coordinates of a box that encompasses the storm.  it is approximately 20x20 deg.
    ; Restrict the complicated calculations of lat/lon, dist/az to that box.  This cuts down computation time.
    if type eq 'SST' then begin
      ; latitude increases in SST array. It decreases in AREA files
      ullat = min(lat) & ullon = min(lon)
      urlat = min(lat) & urlon = max(lon)
      lrlat = max(lat) & lrlon = max(lon)
      lllat = max(lat) & lllon = min(lon)
      dlon = lon[1]-lon[0]
      dlat = lat[1]-lat[0]
    endif else if type eq 'CMORPH' then begin
      ullat = lat[0] & ullon = lon[0]
      urlat = lat[0] & urlon = lon[nlon-1]
      lrlat = lat[nlat-1] & lrlon = lon[nlon-1]
      lllat = lat[nlat-1] & lllon = lon[0]
      dlon = lon[1]-lon[0]
      dlat = lat[1]-lat[0]
    endif else begin
      mcidas_nav,nav,area,0,0,ullat,ullon,/ij2ll
      mcidas_nav,nav,area,nlon-1,0,urlat,urlon,/ij2ll
      mcidas_nav,nav,area,nlon-1,nlat-1,lrlat,lrlon,/ij2ll
      mcidas_nav,nav,area,0,nlat-1,lllat,lllon,/ij2ll
      ;      if iclon+1 gt nlon then stop; the center lon is on the right edge. can't determine dlon.
      ;      if iclat+1 gt nlat then stop; the center lat is on the edge. Can't determine dlat.
      mcidas_nav,nav,area,iclon+1,iclat,clat1,clon1,/ij2ll
      dlon =  clon1 - clon
      mcidas_nav,nav,area,iclon,iclat+1,clat1,clon1,/ij2ll
      dlat = clat1-clat
      
    endelse
    startlon = max([iclon - 7.8/dlon, 0]) ; try 7.8
    endlon   = min([iclon + 7.8/dlon, nlon-1]) ; try 7.8
    startlat = max([iclat + 6.7/dlat, 0]) ; try 6.7
    endlat   = min([iclat - 7.1/dlat, nlat-1]); try 7.1
    
    if debug then begin
      startlon = max([iclon - 2.1/dlon, 0])
      endlon   = min([iclon + 2.1/dlon, nlon-1])
      startlat = max([iclat + 2.0/dlat, 0])
      endlat   = min([iclat - 2.0/dlat, nlat-1])
    endif
    ringcolor = min(zeta, /nan)
    ringcolor = 255
    if type eq 'CMORPH' or type eq 'SST' then ringcolor = max(zeta, /nan)
    
    
    NO_mcidas_latlon_savfile = 1
    if n_elements(mcidas_latlon_savfile) gt 0 && file_test(mcidas_latlon_savfile) eq 1 then begin
      area_file_info = file_info(file)
      latlon_info = file_info(mcidas_latlon_savfile)
      ; make sure the lat/lon save file is more recently modified than original mcidas area file.
      if latlon_info.mtime gt area_file_info.mtime then begin
        ;if the mcidas lat/lon save file exists, then lat2d is already defined
        ; - not necessarily 20120504. the area may only be defined over a small region on another system.
        ; fixed problem 20120524 by prepending pgi to the front of the savfile name.
        NO_mcidas_latlon_savfile = 0
        restore, mcidas_latlon_savfile
      endif else print, 'original mcidas area file '+mcidas_latlon_savfile+' was modified recently'
    endif
    for i=startlon[0],endlon[0],increment do begin
      if long(i) gt progress*nlon then begin
        print, progress*100, format='(I0,"%",$)'
        progress = progress + 0.1
      endif
      ; this negation of the increment may be a kludge for SST.
      for j=startlat[0],endlat[0],type eq 'SST' ? -increment : increment do begin
        if finite(zeta[i,j], /nan) eq 1 then continue ; skip NaN pixels
        if type eq 'SST' or type eq 'CMORPH' then begin
          lat1 = lat[j]
          lon1 = lon[i]
        endif else if NO_mcidas_latlon_savfile eq 1 then begin ; if the mcidas lat/lon save file exists, then lat2d is already defined - not necessarily 20120504. the area may only be defined over a small region on another system.
          mcidas_nav,nav,area,i,j,lat1,lon1,/ij2ll
          lat2d[i,j] = lat1
          lon2d[i,j] = lon1
        endif else begin
          lat1 = lat2d[i,j]
          lon1 = lon2d[i,j]
        endelse
        
        if finite(lat1) eq 0 || finite(lon1) eq 0 || sqrt((lat1-clat)^2.+(lon1-clon)^2.) gt 9.0 then continue ; 9.0 is a universal one
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
        ; Draw box around Cartesian grid domain
        if ix eq -1 or iy eq -1 or ix eq nx or iy eq ny then snapshot[i,j] = ringcolor
        if ix ge 0 and ix lt nx and iy ge 0 and iy lt ny then begin
          for ithreshold = 0, nthresholds-1 do begin
            if abovebelow eq 'below' then pass_threshold = zeta[i,j] lt thresholds[ithreshold]
            if abovebelow eq 'above' then pass_threshold = zeta[i,j] gt thresholds[ithreshold]
            if pass_threshold then thisthreshold[ix,iy,ithreshold] = 1
          endfor
          this_threshold_nvalid[ix,iy,*] = 1
          this_pixel_total[ix,iy] = this_pixel_total[ix,iy] + intensity_time_window_hrs*zeta[i,j]
          this_nvalid[ix,iy]      = this_nvalid[ix,iy] + 1
        endif
      endfor
    endfor
    if type ne 'SST' and type ne 'CMORPH' then save, lat2d, lon2d, filename=mcidas_latlon_savfile
    threshold_total = threshold_total + thisthreshold
    threshold_n     = threshold_n + this_threshold_nvalid
    ivalid = where(this_nvalid gt 0, nvalid)
    if nvalid gt 0 then pixel_total[ivalid] = pixel_total[ivalid] + this_pixel_total[ivalid] / this_nvalid[ivalid]
    
    
    if debug then begin
    ;      i = 50
    ;      j = 100
    ;      print, i,j,lat[j],lon[i],r[i,j], format='("i=",i0,2x,"j=",i0,3x,"lat=",F,3x,"lon=",F,4x,"r=",F)'
    ;      print, zeta[i,j],az[i,j], format='("image=",F," az=",F)'
    endif
    
    dr = 10.
    nrings = maxr/dr
    
    pmin  = replicate(!VALUES.F_NAN, nrings)
    p05   = replicate(!VALUES.F_NAN, nrings)
    p10   = replicate(!VALUES.F_NAN, nrings)
    p25   = replicate(!VALUES.F_NAN, nrings)
    p50   = replicate(!VALUES.F_NAN, nrings)
    p75   = replicate(!VALUES.F_NAN, nrings)
    p90   = replicate(!VALUES.F_NAN, nrings)
    p95   = replicate(!VALUES.F_NAN, nrings)
    pmax  = replicate(!VALUES.F_NAN, nrings)
    avg   = replicate(!VALUES.F_NAN, nrings)
    std   = replicate(!VALUES.F_NAN, nrings)
    nringvalid= replicate(0L, nrings); make sure you use LONG 0L not short integers 0
    pctcov= replicate(!VALUES.F_NAN, nrings, nthresholds)
    
    
    openw, utxt, outtxt, /get_lun
    
    for iringdisk = 0, 1 do begin
      for r1 = 0, nrings-1 do begin
        rmin = r1*dr
        if iringdisk eq 1 then rmin = 0. ; for disk statistics rmin=0 (as opposed to ring)
        rmax = (r1+1)*dr
        iring = where(r ge rmin and r lt rmax, n)
        nringvalid[r1] = n
        hist_nbins = round(1+(hist_range[1]-hist_range[0])/hist_binsize)
        histIR = replicate(0L, hist_nbins)
        xval = (findgen(hist_nbins)*hist_binsize)+hist_range[0]
        if n gt 0 then begin
          psort    = sort(zeta[iring])
          pmin[r1] = min(zeta[iring], /nan)
          p05[r1]  = ((zeta[iring])[psort])[n*0.05]
          p10[r1]  = ((zeta[iring])[psort])[n*0.10]
          p25[r1]  = ((zeta[iring])[psort])[n*0.25]
          p50[r1]  = ((zeta[iring])[psort])[n*0.50]
          p75[r1]  = ((zeta[iring])[psort])[n*0.75]
          p90[r1]  = ((zeta[iring])[psort])[n*0.90]
          p95[r1]  = ((zeta[iring])[psort])[n*0.95]
          pmax[r1] = max(zeta[iring], /nan)
          avg[r1]  = mean(zeta[iring], /nan)
          if n ge 2 then std[r1]  = stddev(zeta[iring], /nan)
          for ithreshold = 0, nthresholds-1 do begin
            pass_threshold = where(zeta[iring] lt thresholds[ithreshold], npass_threshold)
            if type eq 'CMORPH' then pass_threshold = where(zeta[iring] gt thresholds[ithreshold], npass_threshold)
            pctcov[r1, ithreshold] = float(npass_threshold)/float(n)
          endfor
          histIR[*] = histogram(zeta[iring], binsize=hist_binsize, min=hist_range[0], max=hist_range[1], locations=xval)
        endif
        thresholds_string = strjoin(string(thresholds, format='(I8)'))
        ; column labels for histogram counts is the range for bin. This includes min value and everything up to max, but not max.
        bins_string = strjoin(string(xval,format='(I4)') + '-'+string(shift(xval,-1),format='(I3)'))
        if r1 eq 0 and iringdisk eq 0 then $
          printf, utxt, 'pgi_id yyyymmdd hhmmss     clon    clat rmin rmax  nvalid    pmin     p05     p10     p25     p50     p75     p90     p95    pmax     std     avg'+thresholds_string+bins_string
        printf, utxt, pgi, datestring, imageTime, clon, clat, rmin, rmax, nringvalid[r1], pmin[r1], p05[r1], p10[r1], $
          p25[r1], p50[r1], p75[r1], p90[r1], p95[r1], pmax[r1], std[r1], avg[r1], pctcov[r1,*], histIR, $
          format='(A6, 1x, A8, 1x, I6.6, 1x, F8.2, 1x, F7.2, 2I5, I8, '+strtrim(nthresholds+11,2)+'F8.2, '+strtrim(hist_nbins,2)+'I8)'
      endfor
    endfor
    
    ; draw rings all over snapshot
    ringspacing = 100.
    if type eq 'CMORPH' or type eq 'SST' then ringspacing = 200.
    for r1 = 0, maxr, ringspacing do begin
      thick = 1.
      if type eq 'CMORPH' or type eq 'SST' then thick=5.
      iring = where(r gt r1-thick and r lt r1+thick, n)
      ; Impose concentric circles on the image at a specified interval (e.g. 100km).
      if n gt 0 then snapshot[iring] = ringcolor
    endfor
    
    
    km = dr * (indgen(nrings)+0.5)
    ;    plot, km, avg, yrange = [0,35], /noerase
    
    ; sanity check.  best_track and mtm00 can't be true at the same time.  Technically best_track overrules mtm00=0 and mtm00=1.
    if best_track eq 1 && mtm00 eq 1 then stop
    
    
    ; get zoomed in and centered image of storm
    zoomscale = 1.2
    if nowindows eq 0 then begin
      out_centered_image = outdir + 'images/zoom/' + pgi+datestring+STRING(imageTime, format='("_",i6.6)')+(mtm00?"mtm00":"")+$
        (best_track ? "best_track" : "") + "zoom.png"
      centered_image_dx = 180 ; did 92 for Karl
      centered_image_dy = 180 ; did 92 for Karl (400 km max)
      
      if type eq 'SST' or type eq 'CMORPH' then begin
        out_centered_image = file_dirname(out_centered_image, /mark_dir)+type+file_basename(out_centered_image)
        centered_image_dx = 100
        centered_image_dy = 100
      endif
      ; never figured out why it looks better centered if I subtract 1 and add 1 here
      centered_left   = max([0, round(centered_image_dx-iclon)])
      left            = max([0, round(iclon-centered_image_dx-1)])
      right           = min([nlon-1, round(iclon+centered_image_dx-1)])
      centered_bottom = max([0, round(centered_image_dy-iclat)])
      bottom          = max([0, round(iclat-centered_image_dy-1)])
      top             = min([nlat-1, round(iclat+centered_image_dy-1)])
      centered_image  = fltarr(1+2*centered_image_dx, 1+2*centered_image_dy)
      if left le right && bottom le top then centered_image[centered_left:centered_left+right-left, centered_bottom:centered_bottom+top-bottom] $
        = snapshot[left:right, bottom:top]
      window, 1, xsize=zoomscale*(centered_image_dx*2+1), ysize=zoomscale*(centered_image_dy*2+1)
      ; tried to do this flipping earlier, but it messes things up.
      if strmatch(type, 'Cloud*') eq 1 then centered_image = rotate(centered_image,7)
      iNAN = where(~finite(centered_image),nNaN)
      if nNaN gt 0 then centered_image[iNaN] = min(centered_image,/nan)
      tv, zoomscale ne 1 ? congrid(centered_image,!D.X_SIZE,!D.Y_SIZE) : centered_image
      xyouts, zoomscale*1, zoomscale*(centered_image_dy*2)-8, $
        STRING(pgi,month,day,imageTime,clat,clon,format='(A,"  ",I2.2,"/",I2.2," ",I6.6," UTC!C",F5.2,"N,",F6.2,"E")')+$
        '!Cring spacing '+string(ringspacing, format='(I0,"km")'), /device, align=0
      write_png, out_centered_image, tvrd(true=1), red, g, b, /verbose
      wset, 0
    endif
    
    
    caldat, mintime, minmonth, minday, minyear, minhour, minminute
    caldat, maxtime, maxmonth, maxday, maxyear, maxhour, maxminute
    freq = threshold_total
    igood = where(threshold_n ne 0, ngood)
    if ngood gt 0 then freq[igood] = threshold_total[igood]/threshold_n[igood]
    if nowindows eq 0 then begin
      ; temporary png plots of % below temp threshold (for debugging)
      for ithreshold=0,nthresholds-1 do begin
        window, 2, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
        tv, congrid(255.*freq[*,*,ithreshold],centered_image_dx*2+1,centered_image_dy*2+1)
        xyouts, centered_image_dx, 11, STRING(abovebelow,thresholds[ithreshold],units,minmonth,minday,minhour,minminute,maxmonth,maxday,maxhour,maxminute,$
          format='("% images ",A," ",I0,A,"!C",I2.2,"/",I2.2," ",I2.2,I2.2," - ",I2.2,"/",I2.2," ",I2.2,I2.2," UTC")'), /device, align=0.5
        ; not sure why -80 is *** when format = I3.3.  Use I3. - Ahijevych
        outfile = outdir + 'images/' + pgi+(mtm00?"mtm00":"")+(best_track?"best_track":"")+ string(thresholds[ithreshold],format='(I3)') + units + ".png"
        if type eq 'SST' or type eq 'CMORPH' then outfile = file_dirname(outfile, /mark_dir)+type+file_basename(outfile)
        write_png, outfile, TVRD()
        if debug then print, 'saved '+outfile
        wdelete, 2
      endfor
      if strmatch(type, 'Cloud*') ne 1 then begin ; it doesn't make sense to accumulate temperature.  So skip it.
        window, 2, xsize=centered_image_dx*2+1, ysize=centered_image_dy*2+1
        tv, congrid(2.*pixel_total,centered_image_dx*2+1,centered_image_dy*2+1)
        xyouts, centered_image_dx, 11, STRING(type,pixel_total_units,minmonth,minday,minhour,minminute,maxmonth,maxday,maxhour,maxminute,$
          format='(A, " pixel total ",A,"!C",I2.2,"/",I2.2," ",I2.2,I2.2," - ",I2.2,"/",I2.2," ",I2.2,I2.2," UTC")'), /device, align=0.5
        outfile = outdir + 'images/zoom/' + pgi+(mtm00?"mtm00":"")+(best_track?"best_track":"")+ "pixel_total" + ".png"
        if type eq 'SST' or type eq 'CMORPH' then outfile = file_dirname(outfile, /mark_dir)+type+file_basename(outfile)
        write_png, outfile, TVRD()
        if debug then print, 'saved '+outfile
        wdelete, 2
      endif
      
      ; cheap attempt at box-and-whiskers plot.
      ; draw interquartile box with single, thick line
      wset, 0
      yrange = [-80,60]
      if type eq 'SST' then yrange = [10,50]
      if type eq 'CMORPH' then yrange = [0, 80]
      plot, km, avg, yrange=yrange, xtitle='radius [km] dr='+string(dr,format='(I2," km")'), ytitle='', title='', psym=-1
      errplot, km, p25,p75, thick=8, width=0
      ; overlay min/max whiskers
      errplot, km, pmin, pmax
      snapshot0 = snapshot; need a copy for possible flipping
      iNAN = where(~finite(snapshot0),nNaN)
      if nNaN gt 0 then snapshot0[iNaN] = min(snapshot0,/nan)
      ; I used to flip snapshot, but this messes up i,j references later on when I try to draw a box and rings on snapshot
      if strmatch(type, 'Cloud*') eq 1 then snapshot0 = rotate(snapshot0,7)
      tv, congrid(snapshot0,small_image_nx,small_image_ny), tvcoordx, tvcoordy, /device
      xyouts, tvcoordx, tvcoordy+12, STRING(month,day,imageTime,clat,clon,format='(I2.2,"/",I2.2," ",I6.6," UTC!C",F7.2,F8.2)'), /device
      outfile = outdir + 'images/boxwhisker/'  + pgi+datestring+STRING(imageTime, format='("_",i6.6)')+(mtm00?"mtm00":"")+(best_track?"best_track":"")+".png"
      if type eq 'SST' or type eq 'CMORPH' then outfile = file_dirname(outfile, /mark_dir)+type+file_basename(outfile)
      write_png, outfile, TVRD()
    endif
    
    free_lun, utxt ; txt output named after PGI ID and time, date.
  endfor
  
  ; if you are not in debug mode and you are not incomplete then if force_new or the file doesn't exist then create a save file.
  if debug eq 0 and ~incomplete then if file_test(savfile) eq 0 or force_new eq 1 and maxtime gt 0 and ~keyword_set(pixel_total) and ~keyword_set(threshold_total) and ~keyword_set(threshold_n) then $
    save, filename = savfile, pixel_total, threshold_total, threshold_n, mintime, maxtime, verbose=debug


  if mintime eq 9999999 && maxtime eq 0. then return
  caldat, mintime, minmonth, minday, minyear, minhour, minminute
  caldat, maxtime, maxmonth, maxday, maxyear, maxhour, maxminute
  time_string = (use_trop_tzero gt 0 ?tropical_day_string+"!C":'')+pgi+STRING(minmonth,minday,minhour,minminute,maxmonth,maxday,maxhour,maxminute,$
    format='(3x,I2.2,"/",I2.2," ",I2.2,I2.2,"-",I2.2,"/",I2.2," ",I2.2,I2.2," UTC!C")')
  if keyword_set(title) then title = title+time_string
  ; get motion vector from start to end time.
  mtm_center, pgi, string(minyear,minmonth,minday,format='(I4.4,I2.2,I2.2)'), string(minhour,minminute,0,format='(I2.2,I2.2,I2.2)'), lat0, lon0, best_track=best_track, mtm00=mtm00, silent=debug?0:1
  mtm_center, pgi, string(maxyear,maxmonth,maxday,format='(I4.4,I2.2,I2.2)'), string(maxhour,maxminute,0,format='(I2.2,I2.2,I2.2)'), lat1, lon1, best_track=best_track, mtm00=mtm00, silent=debug?0:1
  result = map_2points(lon0,lat0,lon1,lat1)
  motion_dist = result[0] * !DTOR * 6371.
  motion = motion_dist * 1000. / ((maxtime-mintime)*24d*3600d)
  motion_az   = result[1]
  xmotion = motion * sin(motion_az*!DTOR)
  ymotion = motion * cos(motion_az*!DTOR)
  
  
  ; Read drop locations and time.
  ; Note: don't use datestring. use date.  datestring may have changed to the next day if the time window overlaps the next day.
  
  levs = [70000] ; in the past, if you put a non-standard level first, it broke get_mission_soundings. (like 900 mb)
  nlevs = n_elements(levs)
  
  
  ; Create final Postscript plots of % below temp threshold
  freq = threshold_total
  igood = where(threshold_n ne 0, ngood)
  if ngood gt 0 then freq[igood] = threshold_total[igood]/threshold_n[igood]
  xkm = (findgen(nx)+0.5)*dx-maxr
  ykm = (findgen(ny)+0.5)*dy-maxr
  pos = [0.14, 0.13, 0.94, 0.93]
  ; after doing the % abv/blw threshold plots, do the pixel_total plots. That why the end of the loop is nthresholds, not nthresholds-1.
  for ithreshold=0,nthresholds do begin
    if ithreshold eq nthresholds and type ne 'CMORPH' then continue
    if n_elements(minyear) eq 0 or mintime eq 9999999 then begin
      print, "no "+type+" data for "+pgi+" "+date+" maybe no mtm_center found"
      print, "mintime/maxtime=",mintime,maxtime
      if type ne 'SST' then stop
      print, "not surprising for SST"
      continue
    endif
    if minyear ne 2010 then stop ; sanity check
    old_device = !D.NAME
    set_plot, 'PS'
    !P.THICK = 2
    !P.CHARTHICK = 2
    for ilev=0,nlevs-1 do begin
      ; need to do one level at a time for cloud drift winds. THat is why this is within the ilev loop and not outside it.
      drops_structure = get_mission_soundings(pgi,date,levs[ilev],best_track,mtm00,debug=debug)
      xoffsets = drops_structure.xoffset
      yoffsets = drops_structure.yoffset
      nmissions = n_elements(xoffsets)
      
      ; need to link the cloud drift wind files to the exact hour of the dropfiles.  (12, vs 15 UTC, etc. )
      hours_missions = strmid(drops_structure.dattims,4,2,/reverse)+'0000'
      cloud_drift_structure = get_mission_soundings(pgi,date,levs[ilev],best_track,mtm00,debug=debug,/cloud_drift,time=hours_missions)
      
      for imission=-1,-1 do begin ; used to be nmissions-1
        xoffset = imission eq -1 ? 0 : xoffsets[imission]
        yoffset = imission eq -1 ? 0 : yoffsets[imission]
        if ithreshold eq nthresholds then if strmatch(type, 'Cloud*') eq 1 then continue ; it doesn't make sense to accumulate temperature.  So skip it.
        if imission ge 0 && (abs(xoffset) gt 1000 || abs(yoffset) gt 1000 ) then continue
        gfshour = 18
        if imission ge 0 and n_elements(drops_structure.dattims) gt 0 then gfshour = 6*round(strmid(drops_structure.dattims[imission], 9,2)/6.) mod 24
        if !D.NAME eq 'PS' then device, /close, /color, ysize=7, yoffset=3, xsize=6.5, xoffset=1, /inches, bits=8
        ; if you get an error about minyear undefined it is probably because you have no files that qualified .
        thresh_bit = ithreshold lt nthresholds ? string(thresholds[ithreshold],format='(I0)') + units +"_" : "pixel_total_"
        outfile = outdir + 'images/' + (trop_string ne ''?trop_string:'plain_daily') + '/'+ pgi + $
          STRING(minyear,minmonth,minday,minhour,minminute,maxmonth,maxday,maxhour,maxminute,$
          format='(I4.4,I2.2,I2.2,"_",I2.2,I2.2,"-",I2.2,I2.2,"_",I2.2,I2.2)')+$
          (use_trop_tzero eq 0?'':trop_string+string(tropical_day,format='(I+0)'))+(mtm00?"mtm00":"")+(best_track?"best_track":"")+ thresh_bit+$
          string(levs[ilev]/100,format='(I0)')+"mb_mission"+string(imission+1,format='(I1)')+".ps"
        if keyword_set(threshold_total) then outfile = '/Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats/komaromi/'+thresh_bit+".ps" 
        if type eq 'SST' or type eq 'CMORPH' then outfile = file_dirname(outfile, /mark_directory) + type + file_basename(outfile)
        if !D.NAME eq 'PS' then device, filename=outfile
        ; tv command only works if domain is not clipped or shifted
        ; tvimg = ithreshold lt nthresholds ? 255.-freq[*,*,ithreshold]*255. : 255-10.*sqrt(pixel_total)
        ; tv, tvimg, pos[0], pos[1], xsize=!X.window[1]-!X.window[0], ysize=!Y.window[1]-!Y.window[0], /norm
        title_bit = ithreshold lt nthresholds ? string(abovebelow,thresholds[ithreshold], units, format='("% images ",A,I3,A)') $
          : string(pixel_total_units, format='(A)')
        levels = [20, 40, 60, 80]
        colors = [240, 200, 160, 100]
        
        if ~keyword_set(title) then title = time_string + $
          (imission ne -1 ? 'ref. time '+drops_structure.dattims[imission]:'')+string(levs[ilev]/100,format='(2x,I4)')+"mb "+$
          string(gfshour,format='(I2.2,"Z GFS")')
          
        ; page 1 is CMORPH; page 2 is RH; and if imission = -1, page 3 is no RH, no sondes, just GFS winds.
        for Page = 1, imission eq -1 ? 3 : 2 do begin
          ; Plot axes and labels, but no data.
          plot, [0], [0], xrange=[-600,600]+xoffset, yrange=[-600,600]+yoffset,xstyle=1,ystyle=1, /nodata, position=pos , /isotropic, $
            xtitle='km' + (debug?string(dx,format='(2x,"dx=",I0,"km")'):''), ytitle='km' + (debug?string(dy,format='(2X,"dy=",I0,"km")'):''), $
            title=keyword_set(title)?'':title, /norm, charsize=1.32, charthick=!P.CHARTHICK*1.63
          if keyword_set(title) then xyouts, (pos[0]+pos[2])*0.5, 0.981, title, align=0.5, charsize=(strlen(title)/40.)^(-0.25), /norm
          ; Define the contour field for Page 1 (based on a threshold exceedance frequency or pixel_total).
          ; The thresholds loop has 1 more iteration than the # of thresholds.  It goes from zero to nthresholds, not zero to nthresholds minus one.
          ; What about that extra iteration? When ithreshold eq nthresholds, plot 'pixel_total'.
          if ithreshold lt nthresholds then contour_field = freq[*,*,ithreshold]*100.
          if ithreshold eq nthresholds then begin
            contour_field = pixel_total
            levels = [2, 8, 32, 128]
          endif
          
          case Page of
            1: begin
              ; do CMORPH or satellite IR frequency contour.
              contour, contour_field, xkm, ykm, levels=levels, c_colors=colors, /overplot, /fill
              contour, contour_field, xkm, ykm, levels=levels, /overplot, c_labels=[1,1,1,1], /follow, /closed
              ; negating the contour field is a "clever" way to hatch small values and not cover up large ones. But maybe you don't need to worry if you use cell_fill.
              contour, -threshold_n[*,*,ithreshold], xkm, ykm, levels=[-1], /overplot, c_spacing=.3, c_orientation=45, /noerase, noclip=0, /cell_fill
              ; xyouts, (xkm#replicate(1,n_elements(ykm)))[0:*:15], (ykm##replicate(1,n_elements(xkm)))[0:*:15], string((threshold_n[*,*,ithreshold])[0:*:15],format='(I0)'), align=0.5, charsize=0.4
              if ~keyword_set(threshold_total) then begin ; If this is a cumulative plot then don't show the barbs or shear vectors from this single mission
                get_tomjr_shear, pgi, date, gfshour, xshear, yshear
                if ~finite(xshear) then get_gfs_shear, pgi, date, gfshour, xshear, yshear, best_track, mtm00
                mean_gfs = get_mean_gfs( pgi, date, gfshour, 1000., best_track, mtm00, 500., units='mb')
                gfs1000u = mean_gfs.u
                gfs1000v = mean_gfs.v
                arrow_count = 0
                add_arrow,   xshear,   yshear, arrow_count, color=[0,   5, 90], label="ms!E-1!N!Cshear"
                add_arrow,  xmotion,  ymotion, arrow_count, color=[1, 190, 60], label="ms!E-1!N!Cmotion"
                add_arrow, gfs1000u, gfs1000v, arrow_count, color=[100,10, 50], label="ms!E-1!N!Cgfs1000mb"
                add_drops, cloud_drift_structure, imission, sat=1, rh=0, temp=0
                if imission ge 0 then add_drops, drops_structure, imission, temp=0, rh=Page eq 1
              endif
              contourbar, levels, colors, title = title_bit, position=[0.5*pos[0],0.028,pos[0]+0.4*(pos[2]-pos[0]),pos[1]/3.5], charsize=0.85, format=ithreshold lt nthresholds ? '(I0,"%")': '(I0)'
            end
            2: begin
              gfs_soundings_structure = get_gfs_soundings(pgi,date,gfshour,levs[ilev],best_track,mtm00,block_radius=imission eq -1?0:300.,debug=debug,units='Pa')
              if imission ge 0 then $
                add_rh_contour, [drops_structure.rhdrops[*,imission], gfs_soundings_structure.rhdrops[*,0]], $
                [drops_structure.xdrops[*,imission], gfs_soundings_structure.xdrops], $
                [drops_structure.ydrops[*,imission], gfs_soundings_structure.ydrops], pos $
              else add_rh_contour, gfs_soundings_structure.rhdrops[*,0], gfs_soundings_structure.xdrops, gfs_soundings_structure.ydrops, pos
              add_drops, gfs_soundings_structure, 0, rh=0, temp=0, overflow=overflow
              if overflow eq 1 then stop
              add_drops, cloud_drift_structure, 0, sat=1, rh=0, temp=0, overflow=overflow
              if overflow eq 1 then stop
              if imission ge 0 then add_drops, drops_structure, imission, temp=0, rh=Page eq 1, overflow=overflow
              if overflow eq 1 then stop
            end
            3: begin
              gfs_soundings_structure = get_gfs_soundings(pgi,date,gfshour,levs[ilev],best_track,mtm00,block_radius=0,debug=debug,units='Pa')
              add_drops, gfs_soundings_structure, 0, rh=0, temp=0, overflow=overflow
              if overflow eq 1 then stop
            end
          endcase
          plots, [0,0], !Y.CRANGE, thick=0.4
          plots, !X.CRANGE, [0,0], thick=0.4
          ptimestamp, /right
        endfor ; CMORPH and RH and GFS winds plots
        
        if debug then print, 'saved '+outfile
        if !D.NAME eq 'PS' then device, /close
      endfor
    endfor
    !P.THICK=1
    !P.CHARTHICK=1
    set_plot, old_device
  endfor
  
  if !D.NAME eq 'PS' then device, /close
end
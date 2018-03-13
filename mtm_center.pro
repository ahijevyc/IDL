pro mtm_center, pgi, datestring_input, hhmmss, clat, clon, best_track=best_track, mtm00=mtm00, silent=silent, u=u, v=v

  ; datestring gets augmented by 1. That's why I used datestring_input, not datestring. You don't want to return a changed value.
  datestring = datestring_input
  if not keyword_set(best_track) then best_track = 0
  if not keyword_set(silent) then silent = 0
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  
  imageYear = STRMID(datestring,0,4)
  imageMonth = STRMID(datestring,4,2)
  imageDay = STRMID(datestring,6,2)
  hh = STRMID(hhmmss,0,2)
  mm = STRMID(hhmmss,2,2)
  ss = STRMID(hhmmss,4,2)
  imageJday = julday(imageMonth,imageDay,imageYear,hh,mm,ss)
  
  clat = !VALUES.F_NAN
  clon = !VALUES.F_NAN
  
  
  mtm_track_file = basedir+'PREDICT/mtm_track/hourlyconsensus/'+datestring+'/data.MTM_track.'+datestring+'0000.000_hourlyconsensus.txt2'
  if not silent then print, mtm_track_file
  
  if best_track eq 0 then begin
    if not file_test(mtm_track_file) then begin
      print, "did not find "+mtm_track_file
      return
    endif
    mtm_tmpl = { VERSION: 1., $
      DATASTART: 0L, DELIMITER: 0B, $
      MISSINGVALUE: !VALUES.F_NAN, $
      COMMENTSYMBOL:  '', FIELDCOUNT:  7L, $
      FIELDTYPES: [2,7,7,7,7,7,7], $
      FIELDNAMES: ['idnum','pgi', 'dates','times','LAT','LON','junk'], $
      FIELDLOCATIONS: [0,3,10,17,22,28,35], $
      FIELDGROUPS: indgen(7) }
      
    data = read_ascii(mtm_track_file, template=mtm_tmpl)
    
    id = data.pgi
    
    ; find all lines with the requested PGI id.
    iid = where(id eq pgi, n)
    if mtm00 then iid = where(id eq pgi and data.times eq '0000', n)
    if n eq 0 then return
    
    
    dates = data.dates[iid]
    times = data.times[iid]
    ; lats and lons are strings with 'N' and 'W' tacked on to them.  Must convert to float.
    lats = float(data.LAT[iid])
    lons = data.LON[iid]
    hemis = strmid(lons,0,1, /rev)
    iwest = where(hemis eq 'W', nwest)
    lons = float(lons)
    if nwest gt 0 then lons[iwest] = -lons[iwest]
    
    if dates[0] ne strmid(datestring,2,6) then stop ; sanity check - see if the first entry has the correct date
    
    tc_track_months = replicate(imageMonth,n)
    tc_track_days   = replicate(imageDay,  n)
    tc_track_years  = replicate(imageYear, n)
    tc_track_hh     = strmid(times,0,2)
    tc_track_mm     = strmid(times,2,2)
    tc_track_ss     = replicate(0,n)
    tc_track_jdays = julday(tc_track_months, tc_track_days, tc_track_years, tc_track_hh, tc_track_mm, tc_track_ss)
    
    ; get the subscript (it) of the first tc_track_jdays element to be >= imageJday.
    for it = 0, n_elements(lats)-1 do if tc_track_jdays[it] ge imageJday then break
    
    if it eq n then begin
      caldat, tc_track_jdays[0] + 1d, month, day, year
      datestring = string(year, month, day, format='(I4.4,I2.2,I2.2)')
      mtm_track_file = basedir+'PREDICT/mtm_track/hourlyconsensus/'+datestring+'/data.MTM_track.'+datestring+'0000.000_hourlyconsensus.txt2'
      if not file_test(mtm_track_file) then begin
        print, "no mtm track file "+mtm_track_file
        return
      endif
      data = read_ascii(mtm_track_file, template=mtm_tmpl)
      id = data.pgi
      ; find all lines with the requested PGI id.
      iid = where(id eq pgi, n)
      if n eq 0 then begin
        if not silent then print, pgi + " not found in "+mtm_track_file
        return
      endif
      lats      = [lats,        (data.LAT[iid])[0]]
      lon = (data.LON[iid])[0]
      hemis = strmid(lon,0,1, /rev)
      if hemis eq 'W' then lon = -lon
      lons      = [lons, lon]
      tc_track_jdays = [tc_track_jdays, julday(month,day,year,0,0,0)]
    endif
    
  endif else begin
  
    ; if you requested best track, undefine clat and clon.  You should not have a best track center and a center that you defined with mtm tracks at the same time. It's confusing.
    clat = !VALUES.F_NAN
    clon = !VALUES.F_NAN
    
    
    unisys_template = {$
      VERSION   : 1.00000,   $
      DATASTART : 3L,   $
      DELIMITER : 0B,       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 7L,  $
      FIELDTYPES:    [7,4,4,3,3,3,7] ,  $
      FIELDNAMES:   ['ADV', 'LAT', 'LON', 'month', 'day', 'hour', 'junk'],$
      FIELDLOCATIONS: [0,4,11,19,22,25,27],$
      FIELDGROUPS:  indgen(7) }
      

    if pgi eq 'ALEX' or pgi eq 'PGI07L' then st = read_ascii(basedir+'PREDICT/best_track/ALEX_track.dat', template=unisys_template)

    if pgi eq 'BONNIE' or pgi eq 'PGI17L' then st = read_ascii(basedir+'PREDICT/best_track/BONNIE_track.dat', template=unisys_template)
      
    ; EARL http://weather.unisys.com/hurricane/atlantic/2010/
    if pgi eq 'EARL' or pgi eq 'PGI34L' then st = read_ascii(basedir+'PREDICT/best_track/hurricane_atlantic_2010_EARL_track.dat', template=unisys_template)
    ;    Hurricane EARL
    ;ADV  LAT   LON       TIME     WIND  PR  STAT
    ;  1  12.40  -19.30 08/24/00Z   25  1009 TROPICAL DEPRESSION
    ; 53  55.70  -53.50 09/06/00Z   35   994 EXTRATROPICAL STORM
    
    ; FIONA http://weather.unisys.com/hurricane/atlantic/2010/
    if pgi eq 'FIONA' or pgi eq 'PGI36L' then st = read_ascii(basedir+'PREDICT/best_track/FIONA_track.dat', template=unisys_template)
    ;    Tropical Storm Fiona
    ;ADV  LAT   LON       TIME     WIND  PR  STAT
    ;  1  14.00  -41.80 08/30/00Z   25  1007 TROPICAL DEPRESSION
    ; 24  34.10  -62.00 09/04/18Z   25  1011 TROPICAL DEPRESSION

    if pgi eq 'PGI38L' then st = read_ascii(basedir+'PREDICT/best_track/GASTON_track.dat', template=unisys_template)
    ;Tropical Storm GASTON
    ;ADV  LAT    LON      TIME     WIND  PR  STAT
    ;  1  12.40  -35.80 09/01/15Z   30  1006 TROPICAL DEPRESSION
    ;  6  13.50  -39.50 09/02/21Z   25  1009 TROPICAL DEPRESSION
    
    if pgi eq 'PGI44L' or pgi eq 'KARL' then st = read_ascii(basedir+'PREDICT/best_track/KARL_track.dat', template=unisys_template)
    ;Hurricane-3 KARL
    ;ADV  LAT    LON      TIME     WIND  PR  STAT
    ;  1  18.30  -84.20 09/14/21Z   35  1001 TROPICAL STORM
    ; 16  18.50  -97.60 09/18/09Z   20  1006 TROPICAL DEPRESSION
    
    if pgi eq 'PGI46L' then st = read_ascii(basedir+'PREDICT/best_track/MATTHEW_track.dat', template=unisys_template)
    ;Tropical Storm MATTHEW
    ;ADV  LAT    LON      TIME     WIND  PR  STAT
    ;  1  13.90  -76.20 09/23/18Z   30  1007 TROPICAL DEPRESSION
    ; 13  17.40  -92.90 09/26/15Z   20  1003 TROPICAL DEPRESSION
    
    
    
    lats = st.LAT
    lons = st.LON
    tc_track_months = st.month
    tc_track_days = st.day
    tc_track_hours = st.hour
    tc_track_years = replicate('2010', n_elements(tc_track_hours)) ; the unisys file has no year.
    tc_track_jdays = julday(tc_track_months,tc_track_days,tc_track_years,tc_track_hours,0,0)
    
    ; if best_track was specified and no best_track exists, then return NaNs so calc_rings.pro can skip this time.
    if imageJday lt min(tc_track_jdays) || imageJday gt max(tc_track_jdays) then return
    
    
    ; get the subscript (it) of the first tc_track_jdays element to be >= imageJday.
    for it = 0, n_elements(lats)-1 do if tc_track_jdays[it] ge imageJday then break
    
    
  endelse
  
  
  ; first if clause is for the 1st element best_track array.  Don't want to subscript with it-1, if this is the case.
  if it eq 0 then begin
    clat = lats[it]
    clon = lons[it]
    ; since you can't calculate U and V on the cusp of a lat/lon time, try the next second or the previous second
    if not silent then print, "can't get u and v for "+pgi+" at "+datestring_input+" "+hhmmss
    next_second = imageJday + 1d/3600d/24d
    prev_second = imageJday - 1d/3600d/24d
    caldat, next_second, nsmm, nsdd, nsyy, nshh, nsmn, nsss
    mtm_center, pgi, string(nsyy,nsmm,nsdd,format='(I4.4,2I2.2)'), string(nshh,nsmn,nsss,format='(3I2.2)'), clat_junk, clon_junk, best_track=best_track, mtm00=mtm00, silent=silent, u=u, v=v
    if n_elements(u) eq 0 then begin
      caldat, prev_second, psmm, psdd, psyy, pshh, psmn, psss
      mtm_center, pgi, string(psyy,psmm,psdd,format='(I4.4,2I2.2)'), string(pshh,psmn,psss,format='(3I2.2)'), clat_junk, clon_junk, best_track=best_track, mtm00=mtm00, silent=silent, u=u, v=v
      if n_elements(u) eq 0 then stop
    endif
  endif else begin
    dt = imageJday - tc_track_jdays[it-1]
    npath = 24*60.; one-minute resolution, like satellite images.
    ; get npath evenly spaced lon/lats between 2 points on great circle
    path = map_2points(lons[it-1], lats[it-1], lons[it], lats[it], npath = npath)
    ; find index of array that will provide the correct location.
    ipath = (npath-1) * dt / (tc_track_jdays[it]-tc_track_jdays[it-1])
    clon = path[0,ipath]
    clat = path[1,ipath]
    
    ; First get speed between 2 points
    result = map_2points(lons[it-1], lats[it-1], lons[it], lats[it], /meters)
    speed = result[0] / (tc_track_jdays[it]-tc_track_jdays[it-1]) / 24. / 3600.
    
    ; Then, since azimuth changes over the great circle, find azimuth at the interpolated point in question.
    result = map_2points(clon, clat, lons[it], lats[it])
    az = result[1]
    u = speed * sin(!DTOR*az) ; for the life of me, I can't figure out if this is right. 2011-02-07 It seems to contradict eol's web page: http://www.eol.ucar.edu/instrumentation/sounding/isfs/isff-support-center/how-tos/wind-direction-quick-reference
    v = speed * cos(!DTOR*az)  ; I think it is right. I was just forgetting to convert from degrees to radians for the sine and cosine calculations.
    if not silent then print, string(lons[it-1], lats[it-1], lons[it], lats[it], clon, clat, az, format='("(",F7.2, ",",F6.2,") -> (",F7.2, ",",F6.2,"), clon/clat (",F7.2,",",F6.2,") az=",F6.1,"deg")')
    if not silent then print, datestring_input+" "+hhmmss+" speed = "+string(speed, format='(F6.3,"m/s")')+ ", u = " + string(u, format='(F6.2,"m/s")') + ", v=" + string(v, format='(F6.2,"m/s")')
    
    dlatdt = (lats[it]-lats[it-1])/(tc_track_jdays[it]-tc_track_jdays[it-1])
    dlondt = (lons[it]-lons[it-1])/(tc_track_jdays[it]-tc_track_jdays[it-1])
    oldclat = lats[it-1] + dt*dlatdt
    oldclon = lons[it-1] + dt*dlondt
    
    if abs(oldclat-clat) gt 0.055 and clon gt 10 then stop
    if abs(oldclon-clon) gt 0.072 then stop
  endelse
  
  
  
end

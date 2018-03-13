function find_sondes_in_time_and_space_window, indate, intime, inlat, inlon, GV_only=GV_only, debug=debug, $
    just_filename=just_filename, project = project, upsonde_only=upsonde_only, $
    time_window_minutes = time_window_minutes, distance_window_km = distance_window_km


  ; Return an array of filenames of sondes that are within the time and space window provided. 

  ; Copied from get_closest_sonde_in_time
    
  if ~keyword_set(just_filename) then just_filename = 0
  if ~keyword_set(project) then project = 'PREDICT'
  if ~keyword_set(time_window_minutes) then time_window_minutes = 20.
  ; not sure why I had inlat and inlon as arguments. removed on 20111116 - Ahijevych
  ; probably so DC8 could be added.
  
  if ~keyword_set(GV_only) then GV_only = 0
  if ~keyword_set(upsonde_only) then upsonde_only = 0
  
  ; go thru all the D*_PQC.eol dropsonde files, using the date/time in the filename.
  ; find the one closest to the requested indate/intime.
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  date = indate
  time = intime
  lat0  = inlat
  lon0  = inlon
  if ~keyword_set(debug) then debug = 0
  ; GV_only is probably true if you are finding the closest sounding to the MTP. MTP is only on the GV. So you only want drops from the GV
  ; to be matched to the MTP time series. The matching is done by time, so that is necessary.  If we also looked for closest lat/lon, then
  ; perhaps allowing DC8 drops wouldn't be so bad an idea.  But we know they are done at the same time as the GV, so it is likely
  ; to be close to the requested time, but not likely close in lat/lon.
  
  ; WARNING - 20120428.  are we combining files that have had system motion removed and those that haven't?
  ; HRD/fil/*.fil have had system motion removed, but the *.eol files have not.
  ;  files =  file_search(basedir+'PREDICT/dropsondes/'+(GV_only?'':['','GRIP.DC8/','HRD/fil/','HRD/AVAPS/'])+'D20*_*'+(GV_only?'':['_PQC.eol','_PQC.eol','.fil','QC.eol']), count=nDfiles)
  files = get_dropsonde_files(basedir, GV_only=GV_only, project=project, upsonde_only=upsonde_only)
  
  if n_elements(files) eq 0 then stop
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  if debug gt 0 then print, year, month, day, hour, minute, second, format='(I4.4,"/",I0,"/",I0," ",I2.2,":",I2.2,":",I2.2)'
  if debug gt 0 then print, 'requested alts:', alt_reqs, units
  get_julday = julday(month, day, year, hour, minute, second)
  dtime  = 9999. ; considered making this 1./24. (the max allowed time difference), but this is taken care of below.
  
  close_files = ['']
  for ifile = 0,n_elements(files)-1 do begin
  
    file = files[ifile]
    if ~File_test(file) then begin
      if debug gt 0 then print, 'file '+file+' not found'
      continue
    endif

    dropyear   = strmid(file_basename(file), 1, 4)
    dropmonth  = strmid(file_basename(file), 5, 2)
    dropday    = strmid(file_basename(file), 7, 2)
    dropHour   = strmid(file_basename(file), 10, 2)
    dropMinute = strmid(file_basename(file), 12, 2)
    dropSecond = strmid(file_basename(file), 14, 2)
    dropJulian = julday(dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond)
    if abs(dropJulian - get_julday) gt time_window_minutes* 1d/24./60. then continue

    t = get_sonde(file,findgen(10)*1000,units='press_alt_m',/nocape)
    if total(strcmp(tag_names(t),'slon',/fold_case)) eq 1 and finite(t.slon) then lon1 = t.slon else lon1 = mean(t.lon, /nan)
    if total(strcmp(tag_names(t),'slat',/fold_case)) eq 1 and finite(t.slat) then lat1 = t.slat else lat1 = mean(t.lat, /nan)
    if not finite(lon1) then stop
    if not finite(lat1) then stop
    if map_2points(lon0, lat0, lon1, lat1, /meters)/1000. gt distance_window_km then continue
    close_files = close_files[0] eq '' ? [file] : [close_files,file]
  endfor ; next file
  return, close_files
  
end

function get_closest_sonde_in_time, indate, intime, levels, GV_only=GV_only, debug=debug, units=units, $
    just_filename=just_filename, entrainment_rate=entrainment_rate, nocape=nocape, $
    subtract_avgTs = subtract_avgTs, project = project, upsonde_only=upsonde_only, $
    time_window_minutes = time_window_minutes
    
  ; originally written with alt_reqs an array of heights in meters
  ; added units keyword to make it an array of pressures in Pa or mb
  if ~keyword_set(units) then units = 'm'
  if ~keyword_set(just_filename) then just_filename = 0
  if ~keyword_set(nocape) then nocape = 0
  if ~keyword_set(entrainment_rate) then entrainment_rate = 0
  if ~keyword_set(project) then project = 'PREDICT'
  if ~keyword_set(time_window_minutes) then time_window_minutes = 20.
;  ; subtract_avgTs, if set, is the array with the temperature data to subtract
;  if ~keyword_set(subtract_avgTs) then subtract_avgTs = 0
;  if keyword_set(subtract_avgTs) && units ne 'm' && n_elements(subtract_avgTs) ne 22001 then begin
;    print, 'you wish to subtract a temperature profile, but units must be meters and '
;    print, 'the T profile is a 22001 element array from 0-22000 meters'
;    stop
;  endif
  alt_reqs = levels ; DO NOT CHNAGE LEVELS in function
  nalt_reqs = n_elements(alt_reqs)
  
 
; Used to define empty drop structure here but now done in get_sonde.pro, drop = get_sonde('',levels).  
   
  
;  if units eq 'Pa' then alt_reqs = alt_reqs/100. ; convert to mb - all the dropsonde files are in mb. output is in mb.
;  
  ; not sure why I had inlat and inlon as arguments. removed on 20111116 - Ahijevych
  ; probably so DC8 could be added.
  
  if ~keyword_set(GV_only) then GV_only = 0
  if ~keyword_set(upsonde_only) then upsonde_only = 0
  if GV_only eq 1 and upsonde_only eq 1 then stop ; can't do both!!!!
  
  if not finite(indate) then return, get_sonde('',levels) ; return a blank drop structure if indate is not finite
  
  ; go thru all the D*_PQC.eol dropsonde files, using the date/time in the filename.
  ; find the one closest to the requested indate/intime.
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  date = indate
  time = intime
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
  closest_file = ''
  
  for ifile = 0,n_elements(files)-1 do begin
  
    file = files[ifile]
    dropyear = strmid(file_basename(file), 1, 4)
    dropmonth = strmid(file_basename(file), 5, 2)
    dropday   = strmid(file_basename(file), 7, 2)
    dropHour  = strmid(file_basename(file), 10, 2)
    dropMinute = strmid(file_basename(file), 12, 2)
    dropSecond = strmid(file_basename(file), 14, 2)
    dropJulian = julday(dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond)
    if abs(dropJulian - get_julday) gt time_window_minutes* 1d/24./60. then continue
    if ~File_test(file) then begin
      if debug gt 0 then print, 'file '+file+' not found'
      continue
    endif
    if abs(dropJulian - get_julday) lt dtime then begin
      closest_file = file
      closest_dropJulian = dropJulian
      dtime = abs(dropJulian - get_julday)
    endif
    
  endfor ; next file
  if ~File_test(closest_file) then begin
    if debug gt 0 then print, 'closest file '+closest_file+' not found'
    return, get_sonde(closest_file,levels) ; return a blank drop structure if closest file is not found
  endif
  
  if just_filename then return,   {filename       : closest_file}
  
  return, get_sonde(closest_file, levels, units=units, $
    entrainment_rate=entrainment_rate, nocape=nocape, $
    subtract_avgTs = subtract_avgTs)

end

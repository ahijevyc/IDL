function get_dropsonde_files, basedir, GV_only=GV_only, project=project, upsonde_only=upsonde_only, count=count, times=times

  ; return juldays of files in times array

  if ~keyword_set(project) then project = 'PREDICT'
  if ~keyword_set(GV_only) then GV_only = 0
  if ~keyword_set(upsonde_only) then upsonde_only = 0
  
  ; kludge for reading HRD drops . Yes they are interpolated to every 5 mb by hrd2fil, but oh well.
  ; They were converted to gempak .fil and put in subdirectory HRD/fil/.
  ; don't fall for trap of looking for .fil files in dropsondes/ directory. They exist, but you want _PQC.eol files only.
  ; also tacked on HRD/AVAPS with slightly different suffix - Ahijevych 20111212.
  ; Ahijevych 20120429 - WARNING HRD/fil files have had system motion removed already! I let get_scatter know this.
  ;  All the *.eol files have not! 


  ; GV_only is probably true if you are finding the closest sounding to the MTP. MTP is only on the GV. So you only want drops from the GV
  ; to be matched to the MTP time series. The matching is done by time, so that is necessary.  If we also looked for closest lat/lon, then
  ; perhaps allowing DC8 drops wouldn't be so bad an idea.  But we know they are done at the same time as the GV, so it is likely
  ; to be close to the requested time, but not likely close in lat/lon.
  
  ; WARNING - 20120428.  are we combining files that have had system motion removed and those that haven't?
  ; HRD/fil/*.fil have had system motion removed, but the *.eol files have not.
  if upsonde_only eq 1 then return, file_search(basedir+project+'/soundings/u20*.*.fil')
  
  ; on nov 14, 2013 I started to separate fast falls (dropsondes where the parachute did not open) into their own subdirectory called fast-falls.
  allDfiles = file_search(basedir+project+'/dropsondes/'+(GV_only?'':['','GRIP.DC8/','HRD/fil/','HRD/AVAPS/'])+'D20*_*'+(GV_only?'*_P*QC.eol':['_P*QC.eol','_PQC.eol','.fil','QC.eol']), count=count)




  dropyear = strmid(file_basename(allDfiles), 1, 4)
  dropmonth = strmid(file_basename(allDfiles), 5, 2)
  dropday   = strmid(file_basename(allDfiles), 7, 2)
  dropHour  = strmid(file_basename(allDfiles), 10, 2)
  dropMinute = strmid(file_basename(allDfiles), 12, 2)
  dropSecond = strmid(file_basename(allDfiles), 14, 2)

  times = julday(dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond)
  return, allDfiles
end
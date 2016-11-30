function read_unisys_best_track, file, _extra=_extra
  ; used firefox and download 'em all to get all *.dat files in weather.unisys.com/hurricane/*/2013 - on Nov 13.
  
  unisys_template = {$
    VERSION   : 1.00000,   $
    DATASTART : 3L,   $
    DELIMITER : ' ',       $
    MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '',   $
    FIELDCOUNT: 7L,  $
    FIELDTYPES:    [7,5,5,7,3,7,7] ,  $
    FIELDNAMES:   ['ADV', 'LAT', 'LON', 'TIME', 'WIND', 'PR', 'STAT'],$
    FIELDLOCATIONS: [0,4,11,19,22,25,27],$
    FIELDGROUPS:  indgen(7) }
    
  t = read_ascii(file, template=unisys_template, header=header, count=n, _extra=_extra)
  tc_track_years= replicate(strmid(header[0],3,/reverse), n)
  month = strmid(t.time, 0,2)
  day = strmid(t.time,3,2)
  hour = strmid(t.time,6,2)
  jday = julday(month,day,tc_track_years,hour,0,0)
  stormname = file_basename(file, '_track.dat')
  stormname = replicate(stormname, n_elements(n))
  t = create_struct('yyyy', tc_track_years, 'mm', month, 'dd', day, 'hh', hour, 'julday', jday, 'wind_kt', t.wind, 'stormname', stormname, t)
  return, t
  
end

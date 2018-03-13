function get_mpex_missions, count=nmissions
  ; used to call duh(). renamed Aug 6 2013 - Ahijevych
  cd, '/Volumes/pecan2/ahijevyc/mpex/'
  mtm_tmpl = { VERSION: 1., $
    DATASTART: 0L, DELIMITER: ' ', $
    MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  9L, $
    FIELDTYPES: [7,7,7,7,7,7,7,7,7], $
    FIELDNAMES: ['yyyymmdd','hhmm','u','v','lat1','lon1','lat2','lon2', 'RF'], $
    FIELDLOCATIONS: [0,3,10,17,22,28,35, 39, 40], $
    FIELDGROUPS: indgen(9) }
  return, read_ascii('mission_list.txt', template=mtm_tmpl, count=nmissions) 
end

pro get_mpex_missions_locations
  hourly_interval = 3.
  t = get_mpex_missions()
  for i = 0, n_elements(t.yyyymmdd)-1 do begin
    datestring = t.yyyymmdd[i]
    time = t.hhmm[i]
  endfor
  RFs = t.RF[uniq(t.RF,sort(t.RF))]
  for i = 5, n_elements(RFs)-1 do begin
    RF = RFs[i]
    print, RF
    iRF = where(t.RF eq RF)
    
    first_date = min(t.yyyymmdd[iRF],ii)
    first_hhmm = (t.hhmm[iRF])[ii]
    clat = !VALUES.F_NAN
    repeat begin
      mtm_center, RF, first_date, first_hhmm+'00', clat, clon, mtm00=1, /silent
      jday = julday(strmid(first_date,4,2),strmid(first_date,6,2),strmid(first_date,0,4),strmid(first_hhmm,0,2),strmid(first_hhmm,2,2),0)
      jday = jday - hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      first_hhmm = string(hh, mm, format='(I2.2,I2.2)')
      first_date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
    endrep until finite(clat) eq 0 
    
    last_date  = max(t.yyyymmdd[iRF],ii)
    last_hhmm = (t.hhmm[iRF])[ii]
    clat = !VALUES.F_NAN
    repeat begin
      mtm_center, RF, last_date, last_hhmm+'00', clat, clon, mtm00=1, /silent
      jday = julday(strmid(last_date,4,2),strmid(last_date,6,2),strmid(last_date,0,4),strmid(last_hhmm,0,2),strmid(last_hhmm,2,2),0)
      jday = jday + hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      last_hhmm = string(hh, mm, format='(I2.2,I2.2)')
      last_date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
    endrep until finite(clat) eq 0 

    date = first_date
    hhmm = first_hhmm
    while date+hhmm lt last_date+last_hhmm do begin
      mtm_center, RF, date, hhmm+'00', clat, clon, mtm00=1, /silent
      print, date, " "+hhmm+'00', string(clat, clon, format='(F12.3)')
      jday = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),strmid(hhmm,0,2),strmid(hhmm,2,2),0)
      jday = jday + hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
      hhmm = string(hh, mm, format='(I2.2,I2.2)')
    endwhile
    
  endfor
  
end

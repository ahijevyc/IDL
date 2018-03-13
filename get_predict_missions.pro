function get_PREDICT_missions, count=nmissions
  ; used to call duh(). renamed Aug 6 2013 - Ahijevych
  cd, '/Volumes/pecan2/ahijevyc/PREDICT/'
  fi = file_info('t')
  age_sec = systime(/seconds)-fi.mtime
  if age_sec gt 5*60 then spawn, 'source make_tracks_table'
  mtm_tmpl = { VERSION: 1., $
    DATASTART: 0L, DELIMITER: ' ', $
    MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  9L, $
    FIELDTYPES: [7,7,7,7,7,7,7,7,7], $
    FIELDNAMES: ['yyyymmdd','hhmm','u','v','lat1','lon1','lat2','lon2', 'PGI'], $
    FIELDLOCATIONS: [0,3,10,17,22,28,35, 39, 40], $
    FIELDGROUPS: indgen(9) }
  return, read_ascii('t', template=mtm_tmpl, count=nmissions) 
end

pro get_PREDICT_missions_locations
  hourly_interval = 3.
  t = get_PREDICT_missions()
  for i = 0, n_elements(t.yyyymmdd)-1 do begin
    datestring = t.yyyymmdd[i]
    time = t.hhmm[i]
  ;    mtm_center, t.PGI[i], datestring, time+'00', clat, clon, mtm00=1
  ;    print, datestring, time, clat, clon
  endfor
  pgis = t.PGI[uniq(t.PGI,sort(t.PGI))]
  for i = 5, n_elements(pgis)-1 do begin
    pgi = pgis[i]
    print, pgi
    ipgi = where(t.PGI eq pgi)
    
    first_date = min(t.yyyymmdd[ipgi],ii)
    first_hhmm = (t.hhmm[ipgi])[ii]
    clat = !VALUES.F_NAN
    repeat begin
      mtm_center, pgi, first_date, first_hhmm+'00', clat, clon, mtm00=1, /silent
      jday = julday(strmid(first_date,4,2),strmid(first_date,6,2),strmid(first_date,0,4),strmid(first_hhmm,0,2),strmid(first_hhmm,2,2),0)
      jday = jday - hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      first_hhmm = string(hh, mm, format='(I2.2,I2.2)')
      first_date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
    endrep until finite(clat) eq 0 
    
    last_date  = max(t.yyyymmdd[ipgi],ii)
    last_hhmm = (t.hhmm[ipgi])[ii]
    clat = !VALUES.F_NAN
    repeat begin
      mtm_center, pgi, last_date, last_hhmm+'00', clat, clon, mtm00=1, /silent
      jday = julday(strmid(last_date,4,2),strmid(last_date,6,2),strmid(last_date,0,4),strmid(last_hhmm,0,2),strmid(last_hhmm,2,2),0)
      jday = jday + hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      last_hhmm = string(hh, mm, format='(I2.2,I2.2)')
      last_date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
    endrep until finite(clat) eq 0 

    date = first_date
    hhmm = first_hhmm
    while date+hhmm lt last_date+last_hhmm do begin
      mtm_center, pgi, date, hhmm+'00', clat, clon, mtm00=1, /silent
      print, date, " "+hhmm+'00', string(clat, clon, format='(F12.3)')
      jday = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),strmid(hhmm,0,2),strmid(hhmm,2,2),0)
      jday = jday + hourly_interval/24d
      caldat, jday, month, day, year, hh, mm
      date = string(year, month, day, format='(I4.4,I2.2,I2.2)')
      hhmm = string(hh, mm, format='(I2.2,I2.2)')
    endwhile
    
  endfor
  
end

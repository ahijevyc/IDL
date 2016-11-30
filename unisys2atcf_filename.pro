function unisys2atcf_filename, unisys_file, year=year
  if ~keyword_set(year) then year = 2013
  
  
  u = read_unisys_best_track(unisys_file)
  case (1) OF
    (strpos(unisys_file, 'atlantic') ne -1) :  a = file_search('/glade/p/work/ahijevyc/atcf/quick/aal*'+strtrim(year,2)+'.dat', count=na)
    (strpos(unisys_file, 'w_pacif') ne -1)  :  a = file_search('/glade/p/work/ahijevyc/atcf/quick/a[wc]p*'+strtrim(year,2)+'.dat', count=na)
    (strpos(unisys_file, 'e_pacif') ne -1)  :  a = file_search('/glade/p/work/ahijevyc/atcf/quick/a[ce]p*'+strtrim(year,2)+'.dat', count=na)
    (strpos(unisys_file, '_indian') ne -1)  :  return, ''
    (strpos(unisys_file, 's_pacif') ne -1)  :  return, ''
    else: a = file_search('/glade/p/work/ahijevyc/atcf/quick/*'+strtrim(year,2)+'.dat', count=na)
  endcase
  
  tech = 'CARQ'
  bc = 0
  best_match_count=0
  best_atcf_file_m = ''
  for ia = 0, na-1 do begin
    atcf_file = a[ia]
    num = strmid(file_basename(atcf_file), 3, 2)
    if num ge 70 then continue
    if strpos(unisys_file, 'w_pac') ne -1 && strpos(atcf_file, 'awp') ne -1 then tech = 'WRNG'
    t = read_atcf(atcf_file)
    ; if the stormname is in the atcf_file that's a match.
    i = where(t.stormname eq file_basename(unisys_file, "_track.dat"), /null)
    if i ne !NULL then return, atcf_file
    
    i = where(t.tech eq tech and  t.tau eq 0 and (t.rad eq 34 or t.rad eq 0), ni, /null)
    
    ; if lat, lon, and julday arrays are equal then that's a match - duh.
    if array_equal(t.julday[i], u.julday) && array_equal(t.lon[i], u.lon) && array_equal(t.lat[i], t.lat) then return, atcf_file
    
    ; count and find lat/lon matches
    match, complex(t.lon[i],t.lat[i],/double), complex(u.lon, u.lat,/double), suba, subb, count=match_count, epsilon=0.1
    ; cound lat/lon matches that also match julday.
    if match_count gt 0 then match, (t.julday[i])[suba], u.julday[subb], suba, subb, count=match_count
    
    if match_count gt best_match_count then begin
      best_match_count = match_count
      best_atcf_file_m = atcf_file
      best_match1b = string(t.julday[i],format='(C(CMoI2.2,CDI2.2,CHI2.2))') + string(t.lon[i],format='(F8.1)') + string(t.lat[i], format='(F7.1)')
      best_match2b = string(u.julday, format='(C(CMoI2.2,CDI2.2,CHI2.2))') + string(u.lon, format='(F8.1)') + string(u.lat, format='(F7.1)')
    endif
  endfor
  if 0 then begin
    print, unisys_file, ' ', best_atcf_file_m, best_match_count
    print, best_match1b
    print, best_match2b
  endif
  return,  best_atcf_file_m
end

pro make_static
  u = file_search('/glade/p/work/ahijevyc/hurricane/*/2013/*_track.dat', count=nu)
  for iu = 0, nu-1 do print, u[iu], ' ', file_basename(unisys2atcf_filename(u[iu]))
end
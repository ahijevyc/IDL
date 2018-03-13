function stid2speed, stid
  system_IDs = bamexIOP(stid)
  n = n_elements(stid)
  speeds = fltarr(n)
  for i = 0, n-1 do begin
    system_ID = system_IDs[i]
    if STRPOS(system_ID, 'IOP01') ne -1 then speed = 22.7
    if STRPOS(system_ID, 'IOP02') ne -1 then speed = 20
    if STRPOS(system_ID, 'IOP04b') ne -1 then speed = 17.4
    if STRPOS(system_ID, 'IOP06') ne -1 then speed = 22.2
    if STRPOS(system_ID, 'IOP07A') ne -1 then speed = 19
    if STRPOS(system_ID, 'IOP07B') ne -1 then speed = 17.6
    if STRPOS(system_ID, 'IOP09') ne -1 then speed = 18.8
    if STRPOS(system_ID, 'IOP10') ne -1 then speed = 19.3
    if STRPOS(system_ID, 'IOP12') ne -1 then speed = 22
    if STRPOS(system_ID, 'IOP13') ne -1 then speed = 21.1
    if STRPOS(system_ID, 'IOP17') ne -1 then speed = 17.8
    if STRPOS(system_ID, 'IOP18') ne -1 then speed = 19.0
    speeds[i] = speed
  endfor
  
  RETURN, speeds
END
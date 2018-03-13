pro montage, pgi
  cd, '/Volumes/pecan2/ahijevyc/PREDICT/mtm_track/images'
  for imission = 0, 1 do begin
    command = 'montage -geometry 90% '+pgi+'*_0315-*-60C*n'+strtrim(imission,2)+'.ps '+pgi+'_-60C_700mb_mission'+strtrim(imission,2)+'_montage.gif'
    command = 'montage -geometry 90% CMORPH'+pgi+'*_*-*pixel_total*n'+strtrim(imission,2)+'.ps CMORPH'+pgi+'_pixel_total_mission'+strtrim(imission,2)+'_montage.gif'
    ; need /noshell to avoid stty error and need string array to use /noshell
    command = strsplit(command,' ',/extract)
    print, command
    spawn, command, result, /noshell
  endfor
  return
end

pro run_calc_rings

  type = '';'CMORPH' ; use type='' for regular satellite IR stuff
  use_trop_tzero = 0 ; 1 to use time of tropical depression as reference point or 2 for tropical storm
  
    dates = ['0626','0627','0628','0629','0630','0701'] ; PGI07, but no mtm track. best track 6/25 22 UTC - 7/02 03 UTC
    pgi = 'ALEX'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0720','0721'] ; mtm track available for 0720-0722
    pgi = 'PGI16L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0720','0721','0722','0723'] ; BONNIE
    pgi = 'PGI17L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0720'] ; mtm track available for 0720-0721
    pgi = 'PGI18L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0725'] ; mtm track available for part of 0723, but not 0724. nor 0727 nor 0728, nor 0731
    pgi = 'PGI19L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0725','0726','0727','0728','0729'] ; mtm track available for 0725-0730
    pgi = 'PGI20L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0728','0729'] ;
    pgi = 'PGI21L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0729','0802','0803','0804','0805','0806'] ; Colin - no mtm track data for 0731 0801
    pgi = 'PGI22L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0802','0803','0804'] ; goes from 0802-0807, but no mtm track on 0806
    pgi = 'PGI23L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0802','0803','0804','0805','0806','0807','0808','0809','0810']
    pgi = 'PGI24L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0805','0806','0807','0808','0809','0810','0811','0812']
    pgi = 'PGI25L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0808','0809'] ;
    pgi = 'PGI26L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0808','0809','0810','0811','0812','0813','0814','0815','0816','0817','0818','0819']
    pgi = 'PGI27L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0810','0811','0812','0813','0814','0815']
    pgi = 'PGI28L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0811','0812','0813','0814','0815']
    pgi = 'PGI29L'
    if use_trop_tzero lt 2 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0814','0815','0816','0817','0818','0819','0820','0821','0822']; PGI30's last 00 UTC mtm track is 20100823, so you can't do 03-03UTC 20100822 with mtm00=1 - added 0824 00UTC position by hand
    ;as chris said:"[08/23] was more of an instrument test flight.  The disturbance was not a viable genesis candidate, even among non-developers.  I think we should exclude it - I am not completely confident of the sounding data in this case anyway."
    pgi = 'PGI30L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0817','0818','0819','0820','0821','0822','0823','0824','0825','0826'] ; Danielle
    pgi = 'PGI31L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0818','0819']
    pgi = 'PGI33L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0821','0822','0823','0824','0825','0826','0827','0828','0829','0830'] ; PGI34L becomes EARL; no mtm_track available after 0831.; no GV missions
    ; CMORPH available 0821-0824 but too far east for GOES IR
    pgi = 'PGI34L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0824','0825','0826','0827','0828','0829','0830','0831','0901','0902','0903','0904'] ; EARL was PGI34L
    pgi = 'EARL'
  ;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0824']
    pgi = 'PGI35L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0825','0826','0827','0828','0829','0830','0831','0901'] ; PGI36L becomes FIONA but loses mtm_track at 0903
    pgi = 'PGI36L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0831','0901','0902','0903'] ; FIONA - best track 08/30/21Z - 09/04/03Z
    pgi = 'FIONA'
  ;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0825','0826']
    pgi = 'PGI37L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0828','0829','0830','0831','0901','0902','0903','0904','0905','0906','0907','0908','0909','0910'] ; GASTON - 0829 and 0830 have no satellite coverage. they are almost over africa and have no hope of being covered by GOES.
    pgi = 'PGI38L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0830','0831','0901','0902','0903','0904','0905','0906','0907','0908','0909','0910']
    pgi = 'PGI39L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0831','0901','0902','0903']
    pgi = 'PGI40L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0904','0905','0906','0907','0908','0909','0910','0911','0912','0913','0914','0915','0916'] ; Igor
    pgi = 'PGI41L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0905','0906','0907']
    pgi = 'PGI42L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0908','0909','0910','0911','0912','0913','0914','0915','0916']  ; Julia mssing mtm track for 0918-0925
    pgi = 'PGI43L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0909','0910','0911','0912','0913','0914','0915','0916','0917'] ; PGI44L becomes KARL
    ; Karl had no mtm track for 20109018 00UTC, so you can't do 20100917 with mtm00=1. But I added it by hand.
    ; ahijevyc 20120511
    pgi = 'PGI44L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0915','0916','0917'] ; KARL - best track starts 0914 2100UTC (too late for 18UTC soundings) and ends 0918 0900UTC
    ; should I keep 0914 and 0918 for CMORPH?  they would not be complete 24h periods. . . .
    pgi = 'KARL'
  ;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0912','0913','0914','0915','0916','0917','0918','0919','0920','0921','0922','0923','0924','0925']  ; Lisa
    pgi = 'PGI45L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0916','0917','0918','0919','0920','0921','0922','0923','0924','0925']  ; MATTHEW
    pgi = 'PGI46L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0918','0919','0920']
    pgi = 'PGI47L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0926','0927','0930'] ; could be pre-Otto (along with PGI49L). Supposedly goes to 20100930, but NaN mtm track for 20100929 - added mtm_track for 20101001 by hand
    pgi = 'PGI48L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0926','0927','0928','0929','0930'] ; pre-Otto according to MTM web page, but pre-Otto could be PGI48L  - missing Oct 1-12 mtm track files , but did Oct 1 by hand
    pgi = 'PGI49L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0927','0928']  ; NICOLE mtm track starts 00 UTC 20100927.
    pgi = 'PGI50L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['0928','0929','0930'] ; added mtm_track by hand for 20101001.
    pgi = 'PGI51L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1013','1014'] ; PAULA - starts 20101012, but no 1012 mtm track file.
    pgi = 'PGI55L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1013','1014']; no 1012 mtm track file
    pgi = 'PGI58L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1014','1015']
    pgi = 'PGI59L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1014','1015','1018','1019','1020','1021','1022','1023','1024','1025','1026','1027']; RICHARD - no 20101017 mtm track file
    pgi = 'PGI60L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1015','1018','1019','1020','1021','1022']; no 20101017 mtm track file
    pgi = 'PGI61L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1020','1021','1022','1023','1024','1025','1026','1027','1028','1029'];
    pgi = 'PGI63L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1024','1025','1026','1027','1028','1029','1030'];TOMAS
    pgi = 'PGI65L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1025','1026','1027','1028','1029'];SHARY
    pgi = 'PGI66L'
    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1029','1030']
    pgi = 'PGI67L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
    dates = ['1029','1030']
    pgi = 'PGI68L'
    if use_trop_tzero eq 0 then for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date='2010'+dates[idate], type=type, use_trop_tzero=use_trop_tzero
  
;  t = get_PREDICT_missions(count=nmissions)
;  time_window_days = 1d/24*6 ; 6 hour time window
;  hour = strmid(t.hhmm,0,2)
;  minute = strmid(t.hhmm,2,2)
;  month = strmid(t.yyyymmdd,4,2)
;  year  = strmid(t.yyyymmdd,0,4)
;  day   = strmid(t.yyyymmdd,6,2)
;  start_Julian = julday(month, day, year, hour, minute, 0) - time_window_days/2. ; subtract half of time window to get start time
;  caldat, start_julian, month, day, year, start_hr, minute, second
;  start_date = string(year, format='(I4.4)')+string(month,format='(I2.2)')+string(day, format='(I2.2)')
;  stages = ['Non-genesis_composite', 'Genesis_composite', 'TC_Stage_composite', '72+h_pre-genesis_composite', '48-72h_pre-genesis_composite', '24-48h_pre-genesis_composite', '0-24h_pre-genesis_composite']
;  stages = stages + '_Komaromi'
;  nstages=n_elements(stages)
;  for istage = 0, nstages-1 do begin
;    stage = stages[istage]
;    ; reset counters with new stage
;    last_mission = ''
;    title = stage+'!C'
;    if n_elements(pixel_total) ne 0 then begin
;      pixel_total[*] = 0L
;      threshold_total[*] = 0.
;      threshold_n[*] = 0L
;    endif
;    ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
;    for imission=0,nmissions-2 do begin
;      if is_stage('/Volumes/pecan2/ahijevyc/PREDICT/analysis/', t.yyyymmdd[imission], start_hr[imission], stage) eq 1 then begin
;        calc_rings, t.pgi[imission], /mtm00, date=start_date[imission], start_hr=start_hr[imission], $
;          type=type, time_window_days=time_window_days, pixel_total=pixel_total, threshold_total=threshold_total, threshold_n=threshold_n, title=title
;        last_mission = t.pgi[imission] + start_date[imission]
;      endif
;    endfor
;    dir = '/Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats/komaromi/'
;    files = file_search(dir+'-?0C_.ps', count=nps)
;    for ifile = 0, nps-1 do file_move, files[ifile], dir+stage + file_basename(files[ifile]), /verbose, /overwrite
;  endfor
end

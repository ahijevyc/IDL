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
;
  dates = ['0815','0816','0817','0818','0819'] ;
  pgi = 'PGI27L'
  for idate=0,n_elements(dates)-1 do begin
;        calc_rings, pgi, mtm00=1, date=dates[idate]
    if dates[idate] eq '0817' or dates[idate] eq '0818' then microwave_temp_profile, pgi, date=dates[idate] ; pgi27L not flown by GV until 0817.
  endfor
  ;  montage, pgi
  
  dates = ['0818','0819','0820','0821']; PGI30's last 00 UTC mtm track is 20100823, so you can't do 20100823 with mtm00=1
  pgi = 'PGI30L'
  for idate=0,n_elements(dates)-1 do begin
;        calc_rings, pgi, mtm00=1, date=dates[idate]
    if dates[idate] ge '0821' then microwave_temp_profile, pgi, date=dates[idate]; GV flew only one mission into PGI30L
  endfor
  ;  montage, pgi
  
  dates = ['0821','0822','0823','0824','0825','0826','0827','0828','0829','0830'] ; PGI34L becomes EARL; no mtm_track available after 0831.; no GV missions
  ; CMORPH available 0821-0824 but too far east for GOES IR
  pgi = 'PGI34L'
;    for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date=dates[idate]
  ;  montage, pgi
  
  dates = ['0824','0825','0826','0827','0828','0829','0830','0831','0901','0902','0903','0904'] ; EARL
  pgi = 'EARL'
;    for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date=dates[idate]
  ;  montage, pgi
  
  dates = ['0829','0830','0831','0901'] ; PGI36L becomes FIONA but loses mtm_track at 0903
  pgi = 'PGI36L'
  for idate=0,n_elements(dates)-1 do begin
;    calc_rings, pgi, mtm00=1, date=dates[idate]
      if dates[idate] ge '0830' then microwave_temp_profile, pgi, date=dates[idate]
  endfor
  ;  montage, pgi
  
  dates = ['0830','0831','0901','0902','0903','0904'] ; FIONA - final best track entry 09/04/18Z  
  pgi = 'FIONA'
;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date=dates[idate]
  ;  montage, pgi
  
  dates = ['0829','0830','0831','0901','0902','0903','0904','0905','0906','0907','0908','0909','0910'] ; GASTON - 0829 and 0830 have no satellite coverage. they are almost over africa and have no hope of being covered by GOES.
  pgi = 'PGI38L'
  for idate=0,n_elements(dates)-1 do begin
;    calc_rings, pgi, mtm00=1, date=dates[idate]
      if dates[idate] gt '0901' and dates[idate] lt '0910' then microwave_temp_profile, pgi, date=dates[idate]
  endfor
  ;  montage, pgi
  
  dates = ['0909','0910','0911','0912','0913','0914','0915','0916'] ; PGI44L becomes KARL
  pgi = 'PGI44L'
;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, mtm00=1, date=dates[idate]
  ;  montage, pgi
  dates = ['0909','0910a','0910b','0911','0912','0913','0914','0915','0916'] ; splits 0910 into 2 missions
    for idate=0,n_elements(dates)-1 do microwave_temp_profile, pgi, date=dates[idate]
  
  
  dates = ['0915','0916','0917'] ; KARL - best track starts 0914 2100UTC (too late for 18UTC soundings) and ends 0918 0900UTC
  ; should I keep 0914 and 0918 for CMORPH?  they would not be complete 24h periods. . . .
  pgi = 'KARL'
;  for idate=0,n_elements(dates)-1 do calc_rings, pgi, best_track=1, date=dates[idate]
  ;  montage, pgi
  
  
  dates = ['0916','0917','0918','0919','0920','0921','0922','0923','0924','0925']  ; MATTHEW
  pgi = 'PGI46L'
  for idate=0,n_elements(dates)-1 do begin
;    calc_rings, pgi, mtm00=1, date=dates[idate]
      microwave_temp_profile, pgi, date=dates[idate]
  endfor
  ;  montage, pgi
  
  dates = ['0927','0928']  ; NICOLE mtm track starts 00 UTC 20100927.  
  pgi = 'PGI50L'
  for idate=0,n_elements(dates)-1 do begin
;    calc_rings, pgi, mtm00=1, date=dates[idate]
      microwave_temp_profile, pgi, date=dates[idate]
  endfor
;  montage, pgi
  

end

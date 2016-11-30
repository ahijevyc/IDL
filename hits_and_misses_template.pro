function hits_and_misses_template
  return,  {$
    VERSION   : 1.00000,   $
    DATASTART : 0,   $
    DELIMITER : ' ',       $
    MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '',   $
    FIELDCOUNT: 10L,  $
    FIELDTYPES:   [7, 7, 7, 7, 7, 7, 7, 7, 4, 4] ,  $
    FIELDNAMES:     ['basin','stormname','TECH','init_yyyymmddhh','valid_yyyymmddhh','TAU','VMAX','hm','lon','lat'], $
    FIELDLOCATIONS: indgen(10),$
    FIELDGROUPS:  indgen(10) }
    
end


pro add_init
  old_template = {$
    VERSION   : 1.00000,   $
    DATASTART : 0,   $
    DELIMITER : ' ',       $
    MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '',   $
    FIELDCOUNT: 9L,  $
    FIELDTYPES:   [7, 7, 7, 7, 7, 7, 7, 4, 4] ,  $
    FIELDNAMES:     ['basin','stormname','valid_yyyymmddhh','TAU','VMAX','TECH','hm','lon','lat'], $
    FIELDLOCATIONS: indgen(9),$
    FIELDGROUPS:  indgen(9) }
  in = '~/hm.00Z'
  t = read_ascii(in, template=old_template,count=n)
  v = t.valid_yyyymmddhh
  month = strmid(v,4,2)
  day = strmid(v,6,2)
  year = strmid(v,0,4)
  hour = strmid(v,8,2)
  fh = long(strmid(t.TAU,1))
  init = julday(month,day,year,hour) - fh/24d
  openw,lun,in+'.new',/get_lun
  for i=0,n-1 do printf, lun, string(t.basin[i],t.stormname[i],init[i],t.valid_yyyymmddhh[i],format = '(A2,X,A-11,c(CYI,CMOI2.2,CDI2.2,CHI2.2),x,A)') $
    + ' f'+string(fh[i], t.VMAX[i], t.model[i], t.hm[i], t.lon[i], t.lat[i], $
    format='(I3.3,A6,x,A-8,A4,F8.2,F7.2)')
  free_lun,lun
end
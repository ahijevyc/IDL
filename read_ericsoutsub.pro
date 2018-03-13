
FUNCTION SetIntersection, a, b
minab = min(a, MAX=maxa) > min(b, MAX=maxb) ;Only need intersection of ranges
maxab = maxa < maxb
 
  ;If either set is empty, or their ranges don't intersect: result =NULL.
if maxab lt minab or maxab lt 0 then return, -1
r = where((histogram(a, MIN=minab, MAX=maxab) ne 0) and  $
          (histogram(b, MIN=minab, MAX=maxab) ne 0), count)
if count eq 0 then return, -1 else return, r + minab
end
pro read_ericsoutsub


  if !D.NAME eq 'PS' then device, /color
  loadct, 39
  t_feb08 = { VERSION   : 1.00000,   $
      DATASTART : 0L,   $
      DELIMITER : 32B,       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 3L,  $
      FIELDTYPES:    [7,7,7] ,  $
      FIELDNAMES:   ['f0', 'f1', 'f2'],$
      FIELDLOCATIONS: lindgen(3),$
      FIELDGROUPS:  indgen(3) }
      
  t_apr08 = { VERSION   : 1.00000,   $
      DATASTART : 1L,   $
      DELIMITER : 32B,       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 10L,  $
      FIELDTYPES:    [3,3,3,4,4,4,4,4,4,4] ,  $
      FIELDNAMES:   ['trial', 'model', 'casenum', 'mean', 'lower95', 'upper95', 'lower99', 'upper99', 'lower999', 'upper999'],$
      FIELDLOCATIONS: lindgen(10),$
      FIELDGROUPS:  indgen(10) }
      
  
      
  templ = {$
   VERSION: 1., $
   DATASTART: 0, $
   DELIMITER: 44B, $
   MISSINGVALUE: !VALUES.F_NaN, $
   COMMENTSYMBOL:  ' ', $
   FIELDCOUNT:  9, $
   FIELDTYPES: replicate(4,9), $
   FIELDNAMES: 'FIELD'+strtrim(sindgen(9),2), $
   FIELDLOCATIONS: 2*indgen(9), $
   FIELDGROUPS: replicate(0,9) }
   
  t = read_ascii('/pecan/ahijevyc/Vx/subj_eval/outsubEricg.dat', template=t_feb08)
  dates = t.f0[0:134:15] + " " + t.f1[0:134:15]
;  cstart = 2 ; 2 = 95%, 3 = 99%, 4 = 99.9%
;  wrf2caps_bcl = t.f1[cstart:134:15]
;  wrf4ncar_bcl = t.f1[cstart+5:134:15]
;  wrf4ncep_bcl = t.f1[cstart+10:134:15]
;  wrf2caps_bcu = t.f2[cstart:134:15]
;  wrf4ncar_bcu = t.f2[cstart+5:134:15]
;  wrf4ncep_bcu = t.f2[cstart+10:134:15]
  
  t = read_ascii('/pecan/ahijevyc/Vx/subj_eval/bycaseEricG.dat', template=t_apr08)
  trial = 2

  color2caps = 19
  color4ncar = 90
  color4ncep = 230
  iwrf2caps = where(t.model eq 1)
  iwrf4ncar = where(t.model eq 2)  
  iwrf4ncep = where(t.model eq 3)
  itrial1 = where(t.trial eq 1)
  itrial2 = where(t.trial eq 2)
  itrial0 = where(t.trial eq 0)

  plot, indgen(9), indgen(9), /nodata, yrange=[1.5,4], xrange=[0,8], xstyle=2, xtickname=dates, xminor=1, xtickinterval=1, ytitle='score', yticklen=1
  s = read_ascii('/pecan/ahijevyc/Vx/subj_eval/scores.csv', template = templ, count=count, num_records=3*24)
  wrf2caps = s.field0[*,0:count/3-1]
  wrf4ncar = s.field0[*,count/3:2*count/3-1]
  wrf4ncep = s.field0[*,2*count/3:*]
  i = setIntersection(itrial1, iwrf4ncar)
  x = indgen(9) - 0.15
  errplot, x, t.lower95[i], t.upper95[i], color=color2caps
  oplot, x, total(wrf4ncar,2, /nan)/count*3, psym=2, color=color2caps
;  oplot, x, t.mean[i], psym=2, color=180 
  
  
  s = read_ascii('/pecan/ahijevyc/Vx/subj_eval/scores.csv', template = templ, count=count, data_start=3*24)
  wrf2caps = s.field0[*,0:count/3-1]
  wrf4ncar = s.field0[*,count/3:2*count/3-1]
  wrf4ncep = s.field0[*,2*count/3:*]
  i = setIntersection(itrial2, iwrf4ncar)
  x = x + 0.15
  errplot, x, t.lower95[i], t.upper95[i], color=color4ncar
  oplot, x, total(wrf4ncar,2, /nan)/count*3, psym=2, color=color4ncar
;  oplot, x, t.mean[i], psym=2, color=180 
  
  
  s = read_ascii('/pecan/ahijevyc/Vx/subj_eval/meanscores.csv', template = templ, count=count)
  wrf2caps = s.field0[*,0:count/3-1]
  wrf4ncar = s.field0[*,count/3:2*count/3-1]
  wrf4ncep = s.field0[*,2*count/3:*]
  i = setIntersection(itrial0, iwrf4ncar)
  x = x + 0.15
  errplot, x, t.lower95[i], t.upper95[i], color = color4ncep
  oplot, x, total(wrf4ncar,2, /nan)/count*3, psym=2, color = color4ncep
;  oplot, x, t.mean[i], psym=2, color=180 
  
  
  
  if !D.nAME eq 'PS' then device, /close
  
end

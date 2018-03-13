pro subj_evalscores
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
t = read_ascii('/pecan/ahijevyc/Vx/subj_eval/meanscores.csv', template = templ)
binsize=1.
h = histogram(t.field0, locations=xvals, /nan, binsize=.5, min=0, max=6)
;h = histogram([4,2,3,4,3,3,3,2,3,1.9,5.], locations=xvals, /nan, binsize=binsize, min=0, max=6)
plot, xvals, h, psym=10, xrange=[0.1, 5.9], xstyle=1, xminor=1, yminor=1, xtitle='2-trial mean score', ytitle='count'
if !D.NAME eq 'PS' then device, /close
help, t.field0
print, 'mean', mean(t.field0, /nan)
print, 'medn', median(t.field0)
print, 'std', stddev(t.field0, /nan)
print, 'skewness', skewness(t.field0, /nan)

end

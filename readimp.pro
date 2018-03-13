pro readimp
  if !D.NAME eq 'PS' then device, /close, /color
  loadct, 39
  wp = '/pecan/ahijevyc/CoSPA/RandomForest/'
  cd, wp
  
  sTemplate = {$
    VERSION:         1.00000,$
    DATASTART:       0,$
    DELIMITER:       32B,$
    MISSINGVALUE:    '', $
    COMMENTSYMBOL:   '',$
    FIELDCOUNT:      6,$
    FIELDTYPES:      [7, 4, 4, 4, 4, 7],  $
    FIELDNAMES:      'field'+strtrim(sindgen(6),2),$
    FIELDLOCATIONS:  indgen(6),$
    FIELDGROUPS:     INDGEN(6)}
    f = 'mitll_etops.20km.Avg'
    f = 'WxType'
    SPAWN, ['import.csh', f], /noshell, /nottyreset
    input = '/mmmtmp/ahijevyc/t'
;    input = 'import.inv.wxtype.20081027'
    t = read_ascii(input, template = sTemplate, count=n)
    i = strpos(t.field0[0], '2007')
    year = strmid(t.field0, i, 4)
    month = strmid(t.field0, i+4, 2)
    day = strmid(t.field0, i+6, 2)
    hour = strmid(t.field0, i+9, 2)
    minute = strmid(t.field0, i+11, 2)
    
    j = julday(month, day, year, hour, minute, 0)
    rank = t.field1
    imp = t.field2
    msg = where(rank eq 0)
    rank[msg] = max(rank)
    iday = where(hour gt 12 and hour le 23, nday, complement=inight, ncomplement=nnight)
    if nday gt 0 then plot, j[iday], rank[iday], psym=1, /ylog, xtickunits='Time', yrange=[max(rank), min(rank)], ystyle=1, title=f, ytitle='rank', xtitle='2007'
    if nnight gt 0 then oplot, j[inight], rank[inight], color=220, psym=2
    if nday gt 0 then plot, j[iday], imp[iday], psym=1, xtickunits='Time', ystyle=1, title=f, ytitle='imp', xtitle='2007'
    if nnight gt 0 then oplot, j[inight], imp[inight], color=220, psym=2

  if !D.NAME eq 'PS' then device, /close    
end
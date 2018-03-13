pro plot_inv_simple
  dir = '/pecan/ahijevyc/CoSPA/RandomForest/'
  tpl = {version:1,datastart:0,delimiter:32B,missingvalue:!VALUES.F_NAN,commentsymbol:'',fieldcount:1,fieldnames:'field1',fieldtypes:[7],fieldlocations:[0],fieldgroups:[0]}
  t = read_ascii(dir+ '2007lt5.txt', count=n, template=tpl)
  year = strmid(t.field1, 0,4)
  month = strmid(t.field1, 4,2)
  day = strmid(t.field1, 6,2)
  hour = strmid(t.field1, 9,2)
  min = strmid(t.field1, 11,2)
  j07 = reform(julday(month,day,year,hour,min,0))

  t = read_ascii(dir+ '2008lt5.txt', count=n, template=tpl)
  year = strmid(t.field1, 0,4)
  month = strmid(t.field1, 4,2)
  day = strmid(t.field1, 6,2)
  hour = strmid(t.field1, 9,2)
  min = strmid(t.field1, 11,2)
  j08 = reform(julday(month,day,year-1,hour,min,0))
  
  times = timegen(start = min([j07,j08]), final=max([j07,j08]), units="Hours")
  fields = ['2008', '2007']
  nfields = n_elements(fields)
  xtickinterval = 1
  xtickunits = ['Hours', 'Days', 'Time']
  dummy = label_date(DATE_FORMAT=['%H', '%D', '%M %Y'])
  ytickname = nfields le 60 ? fields : 0
  yticks = nfields le 60 ? nfields-1 : 0
  if n_elements(times) ge 1000 then begin 
    xtickinterval = 2
    xtickunits = ['Days', 'Time']
    dummy = label_date(DATE_FORMAT=['%D', '%M'])
  endif

  plot, times, indgen(nfields), xstyle=1, xtickunits=xtickunits, xtickinterval=xtickinterval, xtickformat='LABEL_DATE', ytickname=ytickname, yticks=yticks, $
  yminor=1, position=pos, /nodata, yticklen=0.0001,xticklen=0.0001,yrange=[0,nfields-1], ystyle=21,xminor=1,yticklayout=1,xticklayout=2,xcharsize=0.4,ycharsize=0.55
  axis, xaxis=1, xstyle=1, xtickunits=xtickunits, xtickformat='LABEL_DATE', xtickinterval=xtickinterval, xticklayout=2, xcharsize=0.4, xminor=1,xticklen=0.0001
  ; Plot line storm regimes (from initial times in regime1 array +6 hours)
  xyouts, replicate(!X.CRANGE[0], nfields), !Y.CRANGE[0] + findgen(nfields)*(!Y.CRANGE[1]-!Y.CRANGE[0])/(nfields-1), fields, /DATA, charsize=1, align=1.0
  regime1 = j07
  ym = (!Y.CRANGE[1] + !Y.CRANGE[0] )/ 2
  for i=0, n_elements(regime1)-1 do polyfill, [regime1[i], regime1[i]+0.25, regime1[i]+0.25, regime1[i]], [!Y.CRANGE[1], !Y.CRANGE[1], ym, ym], /data, color=100
  regime1 = j08
  for i=0, n_elements(regime1)-1 do polyfill, [regime1[i], regime1[i]+0.25, regime1[i]+0.25, regime1[i]], [ym, ym, !Y.CRANGE[0], !Y.CRANGE[0]], /data, color=200
 

end

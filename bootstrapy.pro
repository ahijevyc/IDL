pro bootstrapy
  pwin = GetWindows(/current)
  if pwin ne !NULL then pwin.erase
  n = 26
  seed = 15
  c = replicate(0L,n)
  b = list()
  for i=0,n-1 do begin
    ix = randomu(seed)*n
    c[ix]=c[ix]+1
    b.add, string(byte(97+ix))
  endfor
  b = b.ToArray()
  b = b[sort(b)]
  p = barplot(lindgen(n),c,histogram=0,xminor=0,yminor=0,/current,xrange=[0,25],xtitle='training cases',$
    ytitle='times selected',margin=[0.15,0.4,0.15,0.4],axis_style=1)
  ax=p.axes
  print, ax[1]
  ax[1].tickinterval=1
  ax[0].tickinterval=1
  ax[0].tickname=(strsplit(byte(ax[0].tickvalues+97),/ext)).ToArray()
  ax[0].tickdir=1
  bs = text(13,(p.yrange)[1]+1.,'example of bootstrapped training set $\itT \rm$ for a tree!C!C$\itT\rm$ = {'+strjoin(b,' ')+'}',alignment=0.5,/data,clip=0)
  cs = where(c gt 0,nc)
  ;for ic=0,nc-1 do aa = polyline([cs[ic],13],[c[cs[ic]],(p.yrange)[1]+1], /data,clip=0,color=p.fill_color,antialias=1)
  oob = where(c eq 0,noob)
  soob = (strsplit(byte(oob+97),/ext)).ToArray()
  ;for ic=0,noob-1 do aa = polyline([oob[ic],12],[0,-1.8], /data,clip=0,color='black')
  oob = text(12.5,-2,'"out-of-bag" cases: {'+strjoin(soob,' ')+'}',/data,clip=0,alignment=0.5,vertical_alignment=1)
  p.window.save, '/pecan/ahijevyc/faa/MCS-I/bootstrapy.pdf'
end
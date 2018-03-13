mydevice = !D.NAME
mypmulti = !P.MULTI


; 2 plots to a page
; 1 column; 2 rows
;!P.MULTI=[0,3,2,0,0]

mytime = findgen(24*4*2)/4



levels=[  5, 15, 25, 30, 35, 45,55,70]
colors=[250,225,200,175,150,125,80,40]


@zonal30-48N115-75W1996-20010601-0731freq_double.idl
x = r/7./61 ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[18,2],/edge_truncate); boxcar average
CONTOUR, x, lon, -1* mytime, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '30-48N 115-75W 1996-2001 0601-0731',$
	subtitle = '18x2 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	xticks=8,xTICKFORMAT='TICKS', xtitle='longitude',$
	yticks=16,ytickformat='mod24', ytitle = "hour UTC", /fill
CONTOUR, x, lon, -1*mytime, levels=levels, /OVERPLOT, /follow,/closed



@zonal30-48N115-75W20020601-0731freq_double.idl
x = r/61. ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[18,2],/edge_truncate); boxcar average
CONTOUR, x, lon, -1*mytime, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '30-48N 115-75W 2002 0601-0731',$
	subtitle = '18x2 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	xticks=8,xTICKFORMAT='TICKS', xtitle='longitude',$
	yticks=16,ytickformat='mod24', ytitle = "hour UTC", /fill
CONTOUR, x, lon, -1*mytime, levels=levels, /OVERPLOT,/follow,/closed




@merid30-48N115-78W1996-20010601-0731freq.idl
x = transpose(r)
x = [x,x]; append the same array along the time dimension
x = x/7./61 ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[2,20],/edge_truncate); boxcar average
CONTOUR, x, mytime, lat, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '30-48N 115-78W 1996-2001 0601-0731',$
	subtitle = '2x20 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	yticks=9,YTICKFORMAT='TICKS', ytitle='latitude',$
	xticks=16,xtickformat='mod24', xtitle = "hour UTC", /fill
CONTOUR, x, mytime, lat, levels=levels, /OVERPLOT,/follow, /closed



@merid30-48N115-78W20020601-0731freq.idl
x = transpose(r)
x = [x,x]; append the same array along the time dimension
x = x/61. ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[2,20],/edge_truncate); boxcar average
CONTOUR, x, mytime, lat, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '30-48N 115-78W 2002 0601-0731',$
	subtitle = '2x20 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	yticks=9,yTICKFORMAT='TICKS', ytitle='latitude',$
	xticks=16,xtickformat='mod24', xtitle = "hour UTC", /fill
CONTOUR, x, mytime, lat, levels=levels, /OVERPLOT,/follow,/closed


@zonal30-42N42-48Nskew115-75W20020601-0731freq.idl
x = [[r],[r]]; append the same array along the time dimension
x = x/61. ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[18,2],/edge_truncate); boxcar average
CONTOUR, x, lon, -1*mytime, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '30-48N (skewed N of 42N) 115-75W 2002 0601-0731',$
	subtitle = '18x2 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	xticks=8,xTICKFORMAT='TICKS', xtitle='longitude',$
	yticks=16,ytickformat='mod24', ytitle = "hour UTC", /fill
CONTOUR, x, lon, -1*mytime, levels=levels, /OVERPLOT,/follow,/closed


@zonal40-42N42-50Nskew115-75W20020601-0731freq.idl
x = [[r],[r]]; append the same array along the time dimension
x = x/61. ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[18,2],/edge_truncate); boxcar average
CONTOUR, x, lon, -1*mytime, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '40-50N (skewed N of 42N) 115-75W 2002 0601-0731',$
	subtitle = '18x2 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	xticks=8,xTICKFORMAT='TICKS', xtitle='longitude',$
	yticks=16,ytickformat='mod24', ytitle = "hour UTC", /fill
CONTOUR, x, lon, -1*mytime, levels=levels, /OVERPLOT,/follow,/closed

@zonal40-50N115-75W20020601-0731freq.idl
x = [[r],[r]]; append the same array along the time dimension
x = x/61. ; normalize the array by the # of days in the sampling period
x = x*100; convert to %
x = smooth(x,[18,2],/edge_truncate); boxcar average
CONTOUR, x, lon, -1*mytime, xstyle=1, ystyle=1, $
	c_colors=colors,levels=levels,  $
	title = '40-50N 115-75W 2002 0601-0731',$
	subtitle = '18x2 boxcar smoothing,'+string(string(levels,format='(i2)'),/print)+"%",$
	xticks=8,xTICKFORMAT='TICKS', xtitle='longitude',$
	yticks=16,ytickformat='mod24', ytitle = "hour UTC", /fill
CONTOUR, x, lon, -1*mytime, levels=levels, /OVERPLOT,/follow,/closed



;!P.MULTI = mypmulti
set_plot, mydevice

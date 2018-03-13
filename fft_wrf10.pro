pro mymap, a,pos,limits
	;Set /noborder keyword to not draw a border around the map. The map will fill the extent of the plotting region. If NOBORDER is not specified, a margin equalling 1% of the plotting region will be placed between the map and the border.

	MAP_SET, 35., a.xlonc, standard_parallels=[a.truelat1,a.truelat2],/usa,/continents,/noerase,position=pos,/noborder,/conic,limit=limits
	MAP_GRID,latlab=limits[1]+1,lonlab=limits[0],/box_axes
end ; pro mymap

; I copied this function into its own file, mycolors.pro 20040106 - ahijevyc
;function mycolors, levels
;	return, !D.N_COLORS/n_elements(levels)*indgen(n_elements(levels))
;end; function mycolors


pro meanraw

wrf10_nx = 570
wrf10_ny = 360

; reads the raw unformatted ouput of David Ahijevych's program.
;
; as of 20031007,the write statement was in read_wrf10.f90
;
;   write(ounit)format_version_number,thresh(t),nx,ny,projection_flag,&
;       lat1,lon1,dxm,truelat1,truelat2,average(:,:,t)
;   
;
; 
;
;Where
;      nx, nx are the dimensions of average
;      projection_flag     is the projection flag
;      lat1      is the latitude of gridpoint(1,1)
;      lon1      is the longitude of gridpoint(1,1)
;      dxm       is the grid spacing in m
;      truelat1  is the first true latitude
;      truelat2  is the second true latitude
;      average   is the array

verifyvals_2002 = ['1','','','',string(wrf10_nx),string(wrf10_ny),'3','19.66222','-124.8529','-98','10000','25','55','']
verifyvals_2003 = ['1','','','',string(wrf10_nx),string(wrf10_ny),'3','19.75696','-124.1108','-98','10000','30','60','']

templ = { $
   VERSION:       1.00000,$
   TEMPLATENAME:  '', $
   ENDIAN:        'big',$
   FIELDCOUNT:    14L,$
   TYPECODES:     [3,3,3,4,3,3,3,4,4,4,4,4,4,4], $
   NAMES:         ['version','begin_offset_seconds','end_offset_seconds','thresh','nx','ny','projection_flag','lat1','lon1','xlonc','dxm','truelat1','truelat2','data'],$
   OFFSETS:       ['>4','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0'],$
   NUMDIMS:       [0,0,0,0,0,0,0,0,0,0,0,0,0,2],$
   DIMENSIONS:    STRArr(14, 8),$
   REVERSEFLAGS:  BYTArr(14, 8),$
   ABSOLUTEFLAGS: BYTArr(14),$
   RETURNFLAGS:      [1,1,1,1,1,1,1,1,1,1,1,1,1,1],$
   VERIFYFLAGS:      [1,0,0,0,1,1,1,0,0,1,1,1,1,0],$
   DIMALLOWFORMULAS: [0,0,0,0,0,0,0,0,0,0,0,0,0,1],$
   OFFSETALLOWFORMULAS: INTArr(14),$
   VERIFYVALS:    verifyvals_2003}


templ.dimensions[13,0:1] = ['nx','ny']


temp2 = { $
   VERSION:       1.00000,$
   DATASTART: 0L,$
   DELIMITER: 32B,$
   MISSINGVALUE: !VALUES.F_NAN,$
   COMMENTSYMBOL: '',$
   FIELDCOUNT:    1L,$
   FIELDTYPES:     [5], $
   FIELDNAMES:   ['pts'],$
   FIELDLOCATIONS:  0L,$
   FIELDGROUPS:  [0] }
d = read_ascii('/users/ahijevyc/share/wrf10lats2003.txt',template=temp2)
lats = reform(float(d.pts),wrf10_nx,wrf10_ny)
d = read_ascii('/users/ahijevyc/share/wrf10lons2003.txt',template=temp2)
lons = reform(float(d.pts),wrf10_nx,wrf10_ny)


nx = long(wrf10_nx/2)*2 ; cgm needs even number
ny = long(wrf10_ny/2)*2
dt = 1L
nt = long(24/dt)
thresh = '00.10'
thresh_units = 'mm/h'
field_desc = "% time >= "+thresh+thresh_units
time_offset = 3 ; Analyze the 24-h period beginning with the 3-h fcst.
dataset_desc = "WRF10 " + strtrim(time_offset,2) + "-" + strtrim(time_offset+24-dt,2) + "h fcst "
date_range = 'date_range'

diurnal=fltarr(nx,ny,nt)
for t = 0,nt-1 do begin
	; note the 00h forecast has zero accumulated precip. So start with a later time
	filename='/mmmtmp/ahijevyc/read_wrf10/'+string(t*dt+time_offset,format='(I2.2)')+'-'+string((t+1)*dt+time_offset,format='(I2.2)')+'h_'+thresh+'mm'
	print, 'reading binary file '+filename+'...'
	a=read_binary(filename,template=templ)
	; convert to %
	a.data = a.data*100
	;print, a.lat1, a.lon1
	diurnal[*,*,t]=congrid(a.data,nx,ny) 
        lons = congrid(lons,nx,ny)
        lats = congrid(lats,nx,ny)
endfor


;diurnal[*,*,0]=30
;diurnal[*,*,1]=27 & diurnal[*,*,7]=27
;diurnal[*,*,2]=14 & diurnal[*,*,6]=14
;diurnal[*,*,3]=3 & diurnal[*,*,5]=3
;diurnal[*,*,4]=1


pos=[0.05,0.2,0.95,0.95]
barpos = [0.25,0.00,0.75,0.02]
;loadct,20; nice, light rainbow
LOADCT,42,file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; better rainbow



fftu = COMPLEXARR(nx,ny,nt,/nozero)
r2 = FLTARR(nx,ny,nt,/nozero)
pwr = FLTARR(nx,ny,nt,/nozero)
s2 = FLTARR(nx,ny,nt,/nozero); must retain time dimension so zeros can be filtered out later using the WHERE function.
index_of_max_diurnal = LONARR(nx,ny,/nozero)
for x = 0,nx-1 do begin
	for y = 0, ny-1 do fftu[x,y,*] = fft(diurnal[x,y,*])
	for y = 0, ny-1 do s2[x,y,*] = variance(diurnal[x,y,*])
	for y = 0, ny-1 do begin
		tmp = max(diurnal[x,y,*],i)
		index_of_max_diurnal[x,y] = i
	endfor
	print, '.', format='(a,$)'
endfor


limits = [lats[0,ny-1],lons[0,ny-1],lats[nx-1,ny-1],lons[nx-1,ny-1],lats[nx-1,0],lons[nx-1,0],lats[0,0],lons[0,0]]


; Convert from UTC to Local Solar Time

LSTimeShift = lons/15.

LSTarray = diurnal
for x = 0, nx-1 do LSTarray[x,*,*] = LSTimeShift[x]


; compute power at each harmonic
pwr = ABS(fftu)
; compute % variance explained by each harmonic (R squared)
for t = 0, nt-1 do r2[*,*,t] = 100*(nt/2)*(2*pwr[*,*,t])^2/(nt-1)/s2[*,*,t] 

nodata = where(s2 eq 0,nodata_count)
if nodata_count ne 0 then r2[nodata] = 0
r2[*,*,0]=0
r2[*,*,nt/2]=r2[*,*,nt/2]/2

; The sum of R2 statistics for all nt/2 possible harmonics should equal 100%
; r2[*,*,1] + r2[*,*,2]+ . . . + r2[*,*,nt/2] = 100

;******************************
; ZERO HARMONIC
;******************************

levels = findgen(9)*35/9+1
colors = mycolors(levels)
contour, pwr[*,*,0],xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill, title='Amp. of 0th Harmonic (mean)',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),position=pos
pos = [!X.WINDOW[0],!Y.WINDOW[0],!X.WINDOW[1],!Y.WINDOW[1]]
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,title="% time >= "+thresh+thresh_units, format='(F4.1)'


;******************************
; FIRST HARMONIC
;******************************

levels = 2*findgen(9)+1
colors = mycolors(levels)
contour,2*pwr[*,*,1],xstyle=5,ystyle=5,c_colors=colors,levels=levels, /fill,title='Amp. of 1st Harmonic (diurnal)',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),position=pos
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,format='(F4.1)',title="% time >= "+thresh+thresh_units


levels = findgen(9)*1.4/9+0.1
colors = mycolors(levels)
norm_amp = 2*pwr[*,*,1]/pwr[*,*,0]
weak = where(pwr[*,*,0] eq 0.0)
norm_amp[weak] = 0.0
contour,norm_amp,xstyle=5,ystyle=5,c_colors=colors,levels=levels, /fill,title='Normalized amplitude derived from 1st harmonic of '+field_desc,subtitle=subtitle,position=pos
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,format='(F4.2)'


levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,1],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 1st Harmonic (diurnal)',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),c_colors=colors,levels=levels,position=pos
;contour, s2,/fill,/follow,/overplot
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,title="%", format='(I2)'


u2 = atan(imaginary(fftu[*,*,1]),float(fftu[*,*,1]))
u2 = (-12./!PI * u2) + 48 + LSTimeShift
u2 = u2 + time_offset ; remember we started with 03 UTC
u2 = u2 mod 24
if nodata_count ne 0 then u2[where( pwr[*,*,0] eq 0)] = !VALUES.F_NAN
contour, u2, xstyle=5,ystyle=5,/nodata,title='Time for Peak of 1st Harmonic',subtitle=dataset_desc+date_range+" >="+thresh+thresh_units+" nt="+strtrim(nt,2),position=pos
mymap,a,pos,limits
tvscl, map_patch(u2,lons,lats, XSTART=x0, YSTART=y0, xsize=x1, ysize=y1), x0, y0, xsize=x1, ysize=y1
contour, u2,xstyle=5,ystyle=5,levels=[12],/noerase,position=pos
mymap,a,pos,limits; 2nd calling is necessary to overlay the map on the tvscl stuff, which overlaid the first map.
mycolorbar,position=barpos,range=[0,24],format='(i2)',title='local solar time'



contour, 2*pwr[*,*,1], xstyle=5,ystyle=5,/nodata,title='Time of Maximum Value',subtitle=dataset_desc+date_range+" >="+thresh+thresh_units+" nt="+strtrim(nt,2),position = pos
mymap,a,pos,limits
u2 = 24 * index_of_max_diurnal/nt
u2 = u2 + 48 + LSTarray
u2 = u2 + time_offset ; remember we started with 03 UTC
u2 = u2 mod 24
if nodata_count ne 0 then u2[where(pwr[*,*,0] eq 0)] = !VALUES.F_NAN
tvscl, map_patch(u2,lons,lats, XSTART=x0, YSTART=y0, xsize=x1, ysize=y1), x0, y0, xsize=x1, ysize=y1
contour, u2,xstyle=5,ystyle=5,levels=[12],/noerase,position=pos
mymap,a,pos,limits
mycolorbar,position=barpos,range=[0,24],format='(i2)',title='local solar time'





; SECOND HARMONIC

levels = findgen(8)+1.5
colors = mycolors(levels)
contour,2*pwr[*,*,2],xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill,title='Amp. of 2nd Harmonic (semi-diurnal)',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),position=pos
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,format='(F4.1)',title="% time >= "+thresh+thresh_units

levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,2],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 2nd Harmonic (semi-diurnal)',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),c_colors=colors,levels=levels,position=pos
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,title="%", format='(I2)'


levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,1]+r2[*,*,2],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 1st & 2nd Harmonic',subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),c_colors=colors,levels=levels,position=pos
mymap,a,pos,limits
contourbar,levels,colors,position=barpos,title="%", format='(I2)'



levels = findgen(9)*35/9+1
colors = mycolors(levels)
for t = 0,nt-1,dt do begin
	contour, diurnal[*,*,t],xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill, title='F'+string(t*dt+time_offset,format='(i2.2)'),subtitle=dataset_desc+date_range+" nt="+strtrim(nt,2),position=pos
	mymap,a,pos,limits
	contourbar,levels,colors,position=barpos,title="% time >= "+thresh+thresh_units, format='(F4.1)'
endfor 


;***************************
; Example hourly time series 
;***************************

;0.72 0.42 central NC
;0.77 0.23 central FL
;0.43 0.55 western NE
;0.47 0.56 central NE
;0.38 0.49 central CO
;0.54 0.14 TX Gulf waters

xx = long( nx * [0.77,0.77,0.43,0.47,0.38,0.54])
yy = long( ny * [0.42,0.23,0.55,0.56,0.49,0.14])

for i=0,n_elements(xx)-1 do begin
	x=xx[i] & y=yy[i]
	; Take two copies of the dt-hourly time series and glue them together.
	u2 = reform(diurnal[x,y,*]) & u2 = [u2,u2]
	daily_mean = string(mean(u2),format='(F5.2)')
	print, 'local solar time  = UTC + '+strtrim(LSTimeShift[x],2) + ' h' 
	plot, u2,title="diurnal cycle of "+field_desc+" mean ="+daily_mean+'%',ytitle='%',xstyle=1,subtitle=strtrim(lons[x,y],2)+" E "+strtrim(lats[x,y],2)+" N grid coordinate ("+strtrim(x,2)+","+strtrim(y,2)+")",xtitle='local solar time (h)',xtickname=strtrim(indgen(nt+1)*dt,2),xticks=nt,xrange=[-LSTimeShift[x],-LSTimeShift[x]+24]
	; set everything but the zero and first harmonic power to zero
	tmp = fftu[x,y,*] & tmp[2:nt-2] = 0
	oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=1
	; set everything but the zero and second harmonic power to zero
	tmp = fftu[x,y,*] & tmp[3:nt-3] = 0 & tmp[1] = 0 & tmp[nt-1]=0
	oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=2
	mymap,a,[0.16,0.63,0.51,0.90],limits
	plots, lons[x,y],lats[x,y],psym=2
	print, lons[x,y],lats[x,y], x,y 
endfor



stop ; allow the user to query the current state of the variables before ending.
end


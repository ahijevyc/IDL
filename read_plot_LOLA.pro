; I copied this from read_meanraw.pro on 20030226
; ahijevyc
pro mymap, limits, pos
	MAP_SET, limit=limits,/usa,/continents,/noerase,position=pos,/noborder
	MAP_GRID,latlab=limits[1]+1,lonlab=limits[0],label=2
end ; pro mymap


pro plot_hov


; reads the raw unformatted output of ~tuttle/uswrp/hovm/linux/plot_hov
; These are diurnally averaged lat/lon LOLA arrays with the percentage time exceeding the threshold reflectivity.  An array for each interval of the diurnal cycle.


nt = 24
filename = 'fort.10'
filename = '2001-20020701-0831FREQ25NTHR5.raw'

junk = 0L & grid = 'grid' & nx=0L & ny = 0L

openr, filename,filename,/get_lun

for record = 0,nt-1,1 do begin
	readu,filename,junk
	readu,filename,grid
	if grid ne 'LOLA' then stop
	readu,filename,nx
	readu,filename,ny
	readu,filename,junk & readu,filename,junk
	x_c = findgen(nx)
	y_c = findgen(ny)
	readu,filename,x_c
	readu,filename,junk & readu,filename,junk
	readu,filename,y_c
	readu,filename,junk & readu,filename,junk
	if record eq 0 then data3d = findgen(nx,ny,nt)
	data = findgen(nx,ny)
	readu,filename,data
	data3d[*,*,record]=data
	readu,filename,junk
endfor
print,grid,nx,ny
close,filename


filename = '20020601-0630FREQ25NTHR5.raw'

openr, filename,filename,/get_lun

for record = 0,nt-1,1 do begin
	readu,filename,junk
	readu,filename,grid
	if grid ne 'LOLA' then stop
	readu,filename,nx
	readu,filename,ny
	readu,filename,junk & readu,filename,junk
	x_c = findgen(nx)
	y_c = findgen(ny)
	readu,filename,x_c
	readu,filename,junk & readu,filename,junk
	readu,filename,y_c
	readu,filename,junk & readu,filename,junk
	if record eq 0 then data3d2 = findgen(nx,ny,nt)
	data = findgen(nx,ny)
	readu,filename,data
	data3d2[*,*,record]=data
	readu,filename,junk
endfor
print,grid,nx,ny
close,filename

data3d = ( 2*(31+31)*data3d + 30*data3d2 )/154


; remap to a smaller array
degrade = 1.
nx = LONG(nx/degrade/2)*2; ensure an even number (important for CGM graphics)
ny = LONG(ny/degrade/2)*2
diurnal=congrid(data3d,nx,ny,nt)


;loadct,20; nice, light rainbow
LOADCT,42,file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; better rainbow

thresh = '25'
thresh_units = 'dBZ'
field_desc = "% time with >= 5 native pts within 0.2deg grid box >= " + thresh + thresh_units
date_range = '2001 Jul Aug & 2002 Jun Jul Aug '
dataset = "WSI NOWrad (2km,15min,5dBZ) remapped to .2x.2deg grid "
subtitle = dataset + date_range + "nt="+strtrim(nt,2)

pos=[0.05,0.2,0.95,0.95]
barpos = [0.25,0.00,0.75,0.02]



fftu = COMPLEXARR(nx,ny,nt,/nozero)
r2 = FLTARR(nx,ny,nt,/nozero)
s2 = FLTARR(nx,ny,nt,/nozero)
for x = 0,nx-1 do begin
	for y = 0, ny-1 do fftu[x,y,*] = fft(diurnal[x,y,*])
	for y = 0, ny-1 do s2[x,y,*] = variance(diurnal[x,y,*])
	print, '.', format='(a,$)'
endfor

latmin = y_c[0] & lonmin = x_c[0] & latmax = y_c[ny-1] & lonmax = x_c[nx-1]
print, latmin,lonmin,latmax,lonmax
limits = [latmin,lonmin,latmax,lonmax]



; Convert from UTC to Local Solar Time
dx = (lonmax-lonmin)/nx
lons = lonmin + dx/2 + findgen(nx)*dx 

LSTimeShift = lons/15.

LSTarray = diurnal
for x = 0, nx-1 do LSTarray[x,*,*] = LSTimeShift[x]


; compute power at each harmonic
pwr = ABS(fftu)
; compute % variance explained by each harmonic (R squared)
for t = 0, nt-1 do r2[*,*,t] = 100*(nt/2)*(2*pwr[*,*,t])^2/(nt-1)/s2[*,*,t] 


nodata = where(s2 eq 0,count)
if count ne 0 then r2[nodata] = 0
r2[*,*,0]=0
r2[*,*,nt/2]=r2[*,*,nt/2]/2

; The sum of R2 statistics for all nt/2 possible harmonics should equal 100%
; r2[*,*,1] + r2[*,*,2]+ . . . + r2[*,*,nt/2] = 100






; ZERO HARMONIC

levels = findgen(9)*12/9+2
colors = mycolors(levels)
contour, pwr[*,*,0],xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill, title='Amp. of 0th Harmonic (mean)',subtitle=subtitle,position=pos
print, pos
mymap,limits,pos
contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc



; FIRST HARMONIC

levels = findgen(9)*12/9+2
colors = mycolors(levels)
contour,2*pwr[*,*,1],xstyle=5,ystyle=5,c_colors=colors,levels=levels, /fill,title='Amp. of 1st Harmonic (diurnal)',subtitle=subtitle,position=pos
mymap,limits,pos
contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc



levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,1],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 1st Harmonic (diurnal)',subtitle=subtitle,c_colors=colors,levels=levels,position=pos
;contour, s2,/fill,/follow,/overplot
mymap,limits,pos
contourbar,levels,colors,position=barpos,title="%", format='(I2)'



u2 = atan(imaginary(fftu[*,*,1]),float(fftu[*,*,1]))
u2 = (-12./!PI * u2) + 48 + LSTarray
u2 = u2 mod 24
u2[where( pwr[*,*,0] eq 0)] = !VALUES.F_NAN
contour, u2, xstyle=5,ystyle=5,/nodata,title='Time for Peak of 1st Harmonic',subtitle=subtitle,position=pos
MAP_SET, limit=limits,position=pos,/noerase,/noborder
tvscl, map_patch(u2,lat0=latmin,lat1=latmax,lon0=lonmin,lon1=lonmax, XSTART=x0, YSTART=y0, xsize=x1, ysize=y1), x0, y0, xsize=x1, ysize=y1
contour, u2,xstyle=5,ystyle=5,levels=[12],/noerase,position=pos
mymap,limits,pos
mycolorbar,position=barpos,range=[0,24],format='(i2)',title='local solar time'




; SECOND HARMONIC

levels = 1+findgen(9)*0.5
colors = mycolors(levels)
contour,2*pwr[*,*,2],xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill,title='Amp. of 2nd Harmonic (semi-diurnal)',subtitle=subtitle,position=pos
mymap,limits,pos
contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc

levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,2],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 2nd Harmonic (semi-diurnal)',subtitle=subtitle,c_colors=colors,levels=levels,position=pos
;contour, s2,/fill,/follow,/overplot
mymap,limits,pos
contourbar,levels,colors,position=barpos,title="%", format='(I2)'


levels = [25,50,60,70,80,90,95,97,99]
colors = mycolors(levels)
contour, r2[*,*,1]+r2[*,*,2],xstyle=5,ystyle=5,/fill,title='% Variance Explained by 1st & 2nd Harmonic',subtitle=subtitle,c_colors=colors,levels=levels,position=pos
;contour, s2,/fill,/follow,/overplot
mymap,limits,pos
contourbar,levels,colors,position=barpos,title="%", format='(I2)'








levels = findgen(9)*27/9+1.5
colors = mycolors(levels)
for record = 0, nt-1,3 do begin
	contour,diurnal[*,*,record],xstyle=5,ystyle=5,c_colors=colors, levels=levels,/fill,title=strtrim(record,2)+'-'+strtrim(record+1,2)+' UTC',subtitle=subtitle,position=pos
	mymap,limits,pos
	contourbar,levels,colors,position=barpos,title=field_desc,format='(F5.1)'
endfor



x = long(nx*0.3) & y = long(ny*0.4)
plot, [reform(diurnal[x,y,*]),diurnal[x,y,0]],title="diurnal cycle of "+field_desc,xstyle=1,subtitle="grid coordinate ("+strtrim(x,2)+","+strtrim(y,2)+")",xtitle='index of time dimension with 1st element repeated at end'
; set everything but the zero and first harmonic power to zero
tmp = fftu[x,y,*] & tmp[2:nt-2] = 0
oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=1
; set everything but the zero and second harmonic power to zero
tmp = fftu[x,y,*] & tmp[3:nt-3] = 0 & tmp[1] = 0 & tmp[nt-1]=0
oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=2





stop
end


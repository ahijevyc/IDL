p1 = 700 & p0 = 1000


@sgpsynruc20isobX1.c1.20040717.150000.idl
u = transpose(windup)
v = transpose(windvp)
longitude = transpose(longitude)
latitude  = transpose(latitude)
altitude  = transpose(altitude)


;u[*]=0.
;v[*]=6.
; Create true wind fields
; utrue and vtrue
; from http://ruc.fsl.noaa.gov/RUC.faq.html as of 20040824
;  ROTCON_P    WIND ROTATION CONSTANT, = 1 FOR POLAR STEREO
;                 AND SIN(LAT_TAN_P) FOR LAMBERT CONFORMAL
;  LON_XX_P    MERIDIAN ALIGNED WITH CARTESIAN X-AXIS(DEG)
;  LAT_TAN_P   LATITUDE AT LAMBERT CONFORMAL PROJECTION
;                 IS TRUE (DEG)


rotcon_p = 0.422618
lon_xx_p = -95.0
LAT_TAN_P  =  25.0 

sz = size(u)
longitude = rebin(longitude,sz[1],sz[2],sz[3])

angle2 = rotcon_p * (longitude-lon_xx_p) * 0.017453
sinx2  = sin(angle2)
cosx2  = cos(angle2)
utrue = cosx2*u + sinx2*v
vtrue = -sinx2*u + cosx2*v


; calculate vertical wind shear

k1 = (1000-p1)/25 & k0 = long((1000-p0)/25)
ush = u[*,*,k1] - u[*,*,k0]
vsh = v[*,*,k1] - v[*,*,k0]


speed  = (ush^2+vsh^2)^0.5
image  = reform(speed) ; eliminate dimensions of size 1

sz     = size(image)
left   = [0,sz[2]-1]
top    = [sz[1]-1,sz[2]-1]
right  = [sz[1]-1,0]
bottom = [0,0]
; I think these are defined differently than on the whiteboard in my office
top    = [0,sz[2]-1]
right  = [sz[1]-1,sz[2]-1]
bottom = [sz[1]-1,0]
left   = [0,0]

contour,[[0,0],[1,1]],/nodata, xstyle=5, ystyle = 5
px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px[1]-px[0]		;Size in x in device units
swy = py[1]-py[0]		;Size in Y
six = sz[1]		;Image sizes
siy = sz[2]
aspi = FLOAT(six) / siy		;Image aspect ratio
aspw = FLOAT(swx) / swy		;Window aspect ratio
f = aspi / aspw /111.*85.			;Ratio of aspect ratios
if f ge 1.0 then swy = swy / f else swx = swx * f


loadct,20
;tv,poly_2d(BYTscl(image),$	;Have to resample image
;	[[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
;	keyword_set(interp),swx,swy), $
;	px[0],py[0],xsize=swx,ysize=swy

pos =[px[0]/!d.x_vsize,py[0]/!d.y_vsize,(px[0]+swx)/!d.x_vsize,(py[0]+swy)/!d.y_vsize] 

map_set, 25.0,-95.,0,/CONIC, standard_parallels=25.0, /noborder, /usa, central_azimuth=-95.0,limit=[latitude[left[0],left[1]],longitude[left[0],left[1]],latitude[top[0],top[1]],longitude[top[0],top[1]],latitude[right[0],right[1]],longitude[right[0],right[1]],latitude[bottom[0],bottom[1]],longitude[bottom[0],bottom[1]]],/noerase,position=pos,/cont
map_grid,/box
contour, image, xstyle=5, ystyle=5,/noerase,position=pos,/follow



u = congrid(reform(ush),30,25)
v = congrid(reform(vsh),30,25)
velovect, u, v, xstyle=5, ystyle=5, /noerase, position=pos
;contour, altitude,/xstyle, /ystyle,/noerase,position=pos,/follow 
;contour, latitude,/xstyle, /ystyle,/noerase,position=pos,/follow
;contour, longitude,/xstyle, /ystyle,/noerase,position=pos,/follow
;contour, transpose(u[0,*,*]),/xstyle, /ystyle,/noerase,position=pos

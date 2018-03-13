PRO africa
file_pattern = { $
  VERSION:  1.,$
  DATASTART:  0L, $
  DELIMITER:  32B, $
  MISSINGVALUE: !VALUES.F_NAN, $
  COMMENTSYMBOL:  '', $
  FIELDCOUNT: 221L, $
  FIELDTYPES: replicate(4L,221), $
  FIELDNAMES: 'FIELD'+strtrim(sindgen(221),2), $
  FIELDLOCATIONS: lindgen(221)*8+2, $
  FIELDGROUPS:  replicate(0L,221) $
}

set_plot,"PS"; create PostScript file
device, filename='/mmmtmp/ahijevyc/africa.ps'

model_data =  read_ascii("/mmmtmp/trier/dave/terrain.dat",template=file_pattern)

x=indgen(221)*4
y=indgen(221)*4
z=model_data.FIELD0
z = smooth(z,[20,20])
levels = [600,1200,1800,2400]
colors = [220,190,160,130]
contour,z,x,y, xstyle=1, ystyle=1, levels=leves,c_colors=colors,/ISOTROPIC,/follow,xtitle="x-distance (km)",ytitle="y-distance (km)",title="Terrain",/fill,xrange = [50,830],yrange = [50,830]

;----------------------

; Define number of vectors to sample and plot in x and y dimensions. 0 = default (use all vectors)

nu = 12; # of vectors in x-dim
nv = 12; # of vectors in y-dim

external_max_speed = 15.0

; Normalized plot window coordinates./ISOTROPIC
pos = [0.2,0.2,0.8,0.8]


wind_pattern = { $
  VERSION:  1.,$
  DATASTART:  0L, $
  DELIMITER:  32B, $
  MISSINGVALUE: !VALUES.F_NAN, $
  COMMENTSYMBOL:  '', $
  FIELDCOUNT: 221L, $
  FIELDTYPES: replicate(4L,221), $
  FIELDNAMES: 'FIELD'+string(sindgen(221),format='(i3.3)'), $
  FIELDLOCATIONS: lindgen(221)*8, $
  FIELDGROUPS:  replicate(0L,221) $
}


; Read cape and wind data.

u_data =  read_ascii("/mmmtmp/trier/dave/ushr_init_short_downst.dat",template=wind_pattern)
v_data =  read_ascii("/mmmtmp/trier/dave/vshr_init_short_downst.dat",template=wind_pattern)


km_per_gridpt = 4.
; Assign and plot the cape field.
;mcap=mcap_data.FIELD0
x = km_per_gridpt*findgen(221)+0.5 & y = km_per_gridpt*findgen(221)+0.5
;levels = [500,1000,1500,2000]
; Assign 0 or 1 to each contour level.  0 means no label, and 1 means label it.
;clabels = [1,1,1,1]
;colors = [100,140,180,220]
;contour,mcap,x,y,xstyle=1,ystyle=1,levels=levels,c_colors=colors,c_labels=clabels, /ISOTROPIC, title = 'Mature CAPE, 675-925 Thickness and Shear',position = pos,/fill,xtitle='km',ytitle='km'



; Assign and plot the wind vector field.
u0 = u_data.FIELD000 & v0 = v_data.FIELD000
s = size(u0) & t = size(v0)
if ~array_equal(s,t) then message, 'u0 and v0 not same size'
x0 = km_per_gridpt*findgen(221) & y0 = km_per_gridpt*findgen(221)
resample = nu ne 0 || nv ne 0
if resample then u = congrid(u0,nu,nv,/center,/interp)
if resample then v = congrid(v0,nu,nv,/center,/interp)
if resample then x = congrid(x0,nu,/center,/interp)
if resample then y = congrid(y0,nv,/center,/interp)
thick=3

x_step=(max(x)-min(x))/(nu-1.0)   ; Convert to float. Integer math
y_step=(max(y)-min(y))/(nv-1.0)   ; could result in divide by 0
umax=max(abs(u))  
vmax=max(abs(v))  
xscale = x_step/umax
yscale = y_step/vmax
print, 'umax,vmax=',umax,vmax
print, 'x_step,y_step=',x_step,y_step
print, 'xscale,yscale=',xscale,yscale
if xscale lt yscale then length = umax/external_max_speed else length = vmax/external_max_speed
if xscale lt yscale then arrow_length = x_step else arrow_length = y_step

print, 'external_max_speed, length=',external_max_speed, length
print, 'arrow length=',arrow_length
; remove the semicolon if you already plotted scalar field.
; manually erase vectors along edges
u[*,[0,11]] = !VALUES.F_NAN
v[*,[0,11]] = !VALUES.F_NAN 
u[[0,11],*] = !VALUES.F_NAN
v[[0,11],*] = !VALUES.F_NAN
velovect, u, v, x, y, thick=thick,length=length,xtitle='km',ytitle='km' , xrange=[50,100], /overplot ; noclip = 0
; Plot a scale vector.
x_pos = 0. ; x-position of the tail of the scale vector
y_pos = -1.*max(y0)/9 ; y-position of the scale vector
arrow, x_pos,y_pos,x_pos+arrow_length,y_pos,/DATA,thick=thick
xyouts, x_pos+arrow_length+10,y_pos,strtrim(external_max_speed,2)+'m/s'
xyouts,0.42,0.96,'Short-Lived Mature Stage 0.5-3.5 km AGL Shear',align=0.5,/norm,charsize=1.3
;/ISOTROPIC, title = 'Short-Lived Mature Stage 0.5-3.5 km AGL Shear',position = pos,/fil;l,xtitle='km',ytitle='km'



file_pattern = { $
  VERSION:  1.,$
  DATASTART:  0L, $
  DELIMITER:  32B, $
  MISSINGVALUE: !VALUES.F_NAN, $
  COMMENTSYMBOL:  '', $
  FIELDCOUNT: 221L, $
  FIELDTYPES: replicate(4L,221), $
  FIELDNAMES: 'FIELD'+strtrim(sindgen(221),2), $
  FIELDLOCATIONS: lindgen(221)*8+2, $
  FIELDGROUPS:  replicate(0L,221) $
}

model_data =  read_ascii("/mmmtmp/trier/dave/mcap_init_short_dstrm.dat",template=file_pattern)

x=indgen(221)*4
y=indgen(221)*4
z=model_data.FIELD0
z = smooth(z,[20,20])
contour,z,x,y, levels=[ 250, 500, 750, 1000, 1250, 1500, 1750],/follow,xtitle="x-distance (km)",ytitle="y-distance (km)",title="downstream Short-Lived Initiation Mean CAPE (J/kg)",/overplot, thick=1.01



device,/close ; finish Postscript file.




END 
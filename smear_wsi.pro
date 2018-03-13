pro smearhdf, hdffile, pngfile, smoothx, smoothy, a, b
;+
; NAME:
;	SMEARHDF
;
; PURPOSE:
;	Quick view of WSI NOWrad MASTER 15 in HDF
;       with optional smoothing
;
; CATEGORY:
;	General graphics.
;
; CALLING SEQUENCE:
;	smearhdf, hdffile, pngfile, smoothx, smoothy, a, b
;
; INPUTS:
;	hdffile:	The HDF NOWrad MASTER15 file.
;       pngfile:        output PNG filename
;       smoothx:        width of boxcar smoothing operator in x-dimension
;       smoothy:        width of boxcar smoothing operator in y-dimension
;           a,b:        typical Z-R relationship coefficients (Z = aR^b)
;
; OUTPUT:
;       PNG file
;
;-



debug = 0



HDF_DFR8_RESTART
HDF_DFR8_GETIMAGE, hdffile, image



sz = size(image)			;Size of image
if sz[0] lt 2 then message, 'Parameter not 2D'

; Flip upside down so that the image is right side up.
;image = ROTATE(image,7)
; let tv procedure do this with the order keyword.


LOADCT, 41, FILE='/users/ahijevyc/IDL/resource/colors/colors1.tbl'
;set window used by contour
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4

px = !x.window * !d.x_vsize	;Get size of window in device units
py = !y.window * !d.y_vsize
swx = px[1]-px[0]		;Size in x in device units
swy = py[1]-py[0]		;Size in Y
six = sz[1]		;Image sizes
siy = sz[2]
aspi = FLOAT(six) / siy		;Image aspect ratio
aspw = FLOAT(swx) / swy		;Window aspect ratio
f = aspi / aspw /111.*85.			;Ratio of aspect ratios
;f = aspi / aspw			;Ratio of aspect ratios

if f ge 1.0 then swy = swy / f else swx = swx * f


if (debug) then begin
z = 8
image[0:2,*]=z & image[1829:1831,*]=z & image[3658:3660,*]=z & image[*,0:2]=z & image[*,917:919]=z & image[*,1834:1836]=z
endif

;tv,poly_2d(BYTSCL(image),$	;Have to resample image
;	[[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
;	keyword_set(interp),swx,swy), $
;	px[0],py[0]


notbad = where(image ne 0)
image = (image mod 16)*16.

if (a gt 0.) then begin
	t0 = systime(1)
	image[notbad] = (10.^(image[notbad]/32.)/a)^(1./b)
	print, systime(1) - t0, 's to do image[notbad] = (10.^(image[notbad]/32.)/a)^(1./b)'
endif

if (smoothx gt 1 or smoothy gt 1) then image = bytscl(smooth(image,[smoothx,smoothy],/edge_truncate))

tv,poly_2d(image,$	;Have to resample image
	[[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
	keyword_set(interp),swx,swy), $
	px[0],py[0],xsize=swx,ysize=swy,/order


pos =[px[0]/!d.x_vsize,py[0]/!d.y_vsize,(px[0]+swx)/!d.x_vsize,(py[0]+swy)/!d.y_vsize] 


print, "!x.window=",!x.window
print, "!d.x_size,!d.x_vsize=",!d.x_size,!d.x_vsize
print, "px=",px
print, "!y.window=",!y.window
print, "!d.y_size,!d.y_vsize=",!d.y_size,!d.y_vsize
print, "py=",py
print, "six,swx,six/swx=",six,swx,six/swx
print, "siy,swy,siy/swy=",siy,swy,siy/swy
print, "f=",f
print, "pos=",pos


map_set, /cylindrical, limit=[20.,230.,53.,300.],/usa, position = pos,/noerase,color=7*255/15

xyouts, !x.window[0],!y.region[0],hdffile,/normal

tvlct, r,g,b,/get
write_png,pngfile,tvrd(),r,g,b

if (debug) then stop
end

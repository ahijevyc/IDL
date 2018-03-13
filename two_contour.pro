
set_plot, 'PS'
;basedir = '/pecan/ahijevyc/trier/bmin_capeFeb2013/'
basedir = '.'

loadct, 39

; This particular IDL code block defines 7 colors. The colors are:
; dark red,  red,  light red,  dark blue
; blue,  light blue, and grey.

  dkred = transpose([190,20,20])
  tvlct, dkred, 8
  red = transpose([219,139,139])
  tvlct, red, 13
  ltred = transpose([255,195,195])
  tvlct, ltred, 7
  dkblue = transpose([1,1,245])
  tvlct, dkblue, 14
  blue = transpose([110,110,255])
  tvlct, blue, 3
  ltblue = transpose([205,205,255])
  tvlct, ltblue, 2
  grey = transpose([75,75,75])
  tvlct, grey, 23

; Now you can say
; contour, z, x, y, /follow, color=23
; and the contours will be grey.
; Feel free to change the [red,green,blue] components to make your own colors.
;  The choice of  8, 13, 7, 14, 3, 2, and 23 as color table indices was arbitrary.
; Hopefully the contour routine won't choose one of these custom colors as a fill color.
; If it does, use a different index. For example set dkblue to 15 instead of 14.
; You shouldn't have to worry about it. 


if !D.NAME eq 'PS' then device, /close, /color, bits_per_pixel=8

mydata = read_ascii(basedir+'bmin3_avg_xscn_mature_squall2.dat', delimiter=' ', data_start=0)


x  = (mydata.field1)[0,*]
y  = (mydata.field1)[1,*]
f0 = (mydata.field1)[2,*]
levels = [-11.5, -10, -8.5, -7, -5.5, -4, -2.5, -1, 0.5]
contour, f0,x,y, xstyle=1, ystyle=1, levels=levels,/fill,$
  subtitle="CAPE for Parcels at Different Levels for Mature Squall (J/kg)", $
  xtitle="Distance from Inflow Leading Edge (km)", ytitle="Height (km AGL)", $
  title="Bmin for Parcels at Different Levels for Mature Squall (deg C)", $
  position=[0.1,0.1,0.9,0.9],/irregular,yrange=[0,4.5],xrange=[0,250]
contour, f0,x,y, xstyle=1, ystyle=1, levels=levels,/follow, thick=5, color=23, $
  c_charsize=1, /overplot,/irregular
  
  
  
mydata = read_ascii(basedir+'cap3_avg_xscn_mature_squall2.dat', delimiter=' ', data_start=0)


x  = (mydata.field1)[0,*]
y  = (mydata.field1)[1,*]
f0 = (mydata.field1)[2,*]
levels = [200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600, 2800]

contour, f0,x,y, xstyle=1, ystyle=1, levels = levels, /follow, /overplot, $
  c_charsize=1, /irregular
  
  
  
  
if !D.NAME eq 'PS' then device,/close;finish Postscript files

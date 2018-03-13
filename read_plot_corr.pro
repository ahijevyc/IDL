; I copied this from read_plot_LOTI.pro on 20040102
; ahijevyc

; I copied this from read_plot_hov.pro on 20031117
; ahijevyc
; I copied this from read_meanraw.pro on 20030226
; ahijevyc


; requires /users/ahijevyc/IDL/mycolors.pro


pro plot_hov

old_font=!P.FONT & !P.FONT=0 & print, 'old,new font',old_font,!P.FONT
old_charsize=!P.CHARSIZE & !P.CHARSIZE=0.75 & print, 'old,new charsize',old_charsize,!P.CHARSIZE

;
; reads the ascii file fort.90 produced by ~tuttle/uswrp/hovm/linux/plot_hov_test
; 

filename = 'Fig6a20040106.ascii'

; original write statement copied from ~tuttle/uswrp/hovm/linux/plot_hov_test.f
; on 20040102 by David Ahijevych
;               write(90,201)idate,TMN(NFITS),x00,PX1,PX2,xd,DTMIN,
;     +            wave,rlen,cormax,tmp(i,j),angcor,uspd
temp2 = { $
   VERSION:       1.00000,$
   DATASTART:     0L,$
   DELIMITER:     32B,$
   MISSINGVALUE:  !VALUES.F_NAN,$
   COMMENTSYMBOL: '',$
   FIELDCOUNT:    13L,$
   FIELDTYPES:    replicate(4,13), $
   FIELDNAMES:    ['idate','tmn','x00','px1','px2','xd','dtmin','wave','rlen','cormax','tmp','angcor','uspd'],$
   FIELDLOCATIONS:  [0,14,23,33,44,56,65,75,85,94,106,114,126],$
   FIELDGROUPS:  indgen(13) }
d = read_ascii(filename,template=temp2)

;loadct,20; nice, light rainbow
;LOADCT,42,file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; better rainbow
LOADCT,0 ; black and white

field_desc = "maximum linear correlation * 100"
date_range = '1996-2001, 2003  0620-0831 '
;date_range = '2002 0620-0831 '
xorigin = '105 W'
;xorigin = 'Continental Divide'
lat1 = 34.
lat2 = 45.
lon2km = 111. * cos(!PI/180 * (lat1+lat2)/2.)

dataset = "WSI NOWrad, Z=300R^1.5, mapped to .2x.2deg grid "
lon2km_string = '1 deg =' + STRING(lon2km,FORMAT='(F6.2)') + 'km'
subtitle = 'wavelngth = '+STRING(d.wave[0],FORMAT='(I2)')+', rlen= '+STRING(d.rlen[0],FORMAT='(I2)') 
title = date_range + 'latitude limits: '+STRING(lat1,FORMAT='(I2)')+'-'+STRING(lat2,FORMAT='(I2)')+' N'


pos=[0.2,0.2,0.8,0.95]
barpos = [0.25,0.00,0.75,0.02]

utc = d.tmn/60.
lon = d.x00
corr=d.cormax


levels = findgen(8)*10. + 10.
colors = mycolors(levels)
; If greyscale, reverse the color indices.  We want 0->255 = light->dark
colors = 255-colors
; Trick IDL into reversing the time axis (and the data) by using negative utc and the 'mod24' procedure.
contour,corr,lon,-utc,/irr,xstyle=1,xrange=[-120,-75],yrange=[-48,0],c_colors=colors, levels=levels,/fill,title=title,xtitle='deg longitude ('+lon2km_string+')',xtickformat='ticks',ytitle='UTC [h]',position=pos,ticklen=1,ytickinterval=2,ytickformat='mod24',ygridstyle=1,xgridstyle=1,xtickinterval=5
contour,corr,lon,-utc,/irr,levels=levels,/overplot,/follow,c_charsize=0.5
axis,0,-54,0, xaxis=0, xrange = (!X.CRANGE+105.)*lon2km, xstyle=1, xtitle='km east of '+xorigin,xtickinterval=500
contourbar,levels,colors,position=barpos,format='(I2)'


!P.FONT= old_font & print, 'back to !P.FONT=',!P.FONT
!P.CHARSIZE  = old_charsize & print, 'back to !P.CHARSIZE=',!P.CHARSIZE

stop
end


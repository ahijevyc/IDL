; Get digital grid database from 
; http://www.ngdc.noaa.gov/mgg/geodas/geodas.html
; under Create Custom Grid (as of Nov 3, 2004)
; Get ETOPO2 2-minute Global Relief
; Set your custom Lat/Lon area Bounds
; Try a Grid Cell Size of 4 minutes (10 minutes resulted in bad-looking data over Africa).
; Output Grid Format - ASCII Raster Format - ASCII (Arc) Header



crosslat = -5
yhalfwindow = 7.5 ; in grid points 
xsmooth = 15 ; in grid points

filename = '/data1/pd/ahijevyc/africa_elev.asc'
cols = 1576L
rows = 901
latmin = -20
lonmin = -45
; defines plot area
pos = [0.1,0.15,0.9,0.9]

t = { $
   VERSION:       1.00000,$
   DATASTART:     6L,$
   DELIMITER:     32B,$
   MISSINGVALUE:  !VALUES.F_NAN,$
   COMMENTSYMBOL: '',$
   FIELDCOUNT:    cols,$
   FIELDTYPES:    replicate(3,cols), $
   FIELDNAMES:    'F'+strtrim(sindgen(cols),2),$
   FIELDLOCATIONS:2+7*lindgen(cols),$
   FIELDGROUPS:   replicate(0,cols) }



d = read_ascii(filename,template=t)


e = d.f0
;e[where(e lt 0)] = 0 ; uncomment to set negative elevations to zero.

if (!D.NAME eq 'X') then window, xsize=cols, ysize=rows
tvscl, e, pos[0],pos[1],xsize=pos[2]-pos[0],ysize=pos[3]-pos[1],/order,/normal,/nan
map_set, position=pos, limit=[latmin,lonmin,latmin+rows/15,lonmin+cols/15],/continents,/noerase,/noborder
map_grid,/box

; Draw Cross Section line/bar on map.
plots, [lonmin,lonmin+cols/15], [crosslat-yhalfwindow/15.,crosslat-yhalfwindow/15.]
plots, [lonmin,lonmin+cols/15], [crosslat+yhalfwindow/15.,crosslat+yhalfwindow/15.]

; Calculate which row correponds to the cross section latitude.
yind = (rows-1)-(crosslat-latmin)*15

longitude = lonmin + findgen(cols)/15
plot, longitude, smooth(rebin(e[*,yind-yhalfwindow:yind+yhalfwindow],cols,1),xsmooth),xstyle=1,position=pos, title = 'Elevation Cross Section through ' + strtrim(crosslat,2) + ' latitude +/-'+strtrim(yhalfwindow/15.,2)+'deg', xtitle='longitude',ytitle='m',subtitle=strtrim(xsmooth/15.,2)+'deg smoothing window in x-dimension'


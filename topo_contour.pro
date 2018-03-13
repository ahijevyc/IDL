;old_font = !P.FONT & !P.FONT = 0

;grid_cell_registration   = center
;map_projection           = Lat/Lon
left_map_x               = -115.00000000000
right_map_x              = -100.00000000000
upper_map_y              = 30.00000000000
lower_map_y              = 20.00000000000
number_of_rows           = 1200
number_of_columns        = 1800
grid_size                = 0.00833333333
;elev_m_unit              = meters
;min__value               = 1
;max__value               = 3686
;elev_m_min               = 1
;elev_m_max               = 3686
elev_m_missing_flag      = -500
;number_of_display_colors = 256
;data_value_unit          = elev_m
;data_byte_order          = big_endian
template = {$
	VERSION         : 1.00000,$
	TEMPLATENAME    : '',$
	ENDIAN          : 'big',$
	FIELDCOUNT      : 1L,$
	TYPECODES	: 2,$
	NAMES		: 'topo',$
	OFFSETS		: '>0',$
	NUMDIMS		: 2L,$
	DIMENSIONS	: sindgen(1,8),$
	REVERSEFLAGS	: bindgen(1,8),$
	ABSOLUTEFLAGS	: 0B,$
	RETURNFLAGS	: [1],$
	VERIFYFLAGS	: [0],$
	DIMALLOWFORMULAS: [0],$
	OFFSETALLOWFORMULAS:[0],$
	VERIFYVALS	: ''$
}

template.dimensions[0,0] = '1800'
template.dimensions[0,1] = '1200'
template.reverseflags[0,0] = 0B
template.reverseflags[0,1] = 1B

cd, '/sharp/ahijevyc/NAME'

result = read_binary('20-30N115-100Wtopo.bin',template=template)
topo = result.topo
spol = [-106.9521,23.92890]
x0 = (spol[0]-left_map_x) / grid_size
y0 = (spol[1]-lower_map_y) / grid_size
print, x0,y0
e = rot(topo,35,1.,x0,y0,missing=elev_m_missing_flag,/INTERP)
;e = smooth(e,[5,5])
s = size(e)


position=[0.1,0.2,0.9,0.9]

;levels = [500,1000,2000,3000] ; used for Radar Conf ext. abs.
levels = 100*indgen(13)
;colors = [230,170,5,255]; used for Radar Conf ext. abs.
;colors = [210,100,5,255]; used for JAS note

colors = !D.N_COLORS/n_elements(levels)*indgen(n_elements(levels))
;colors = colors + !D.N_COLORS-1-colors[n_elements(colors)-1] 

west_crop = 6. & east_crop = 6. & north_crop = 3.5 & south_crop = 3.5
e2 = e[west_crop*120:s[1]-1-east_crop*120,south_crop*120:s[2]-1-north_crop*120]
contour, e2, xstyle=1,ystyle=1, levels=levels,position=position, /fill, c_colors=colors,/closed,xtitle='',ytitle='',/ISO
contour, e2, xstyle=5,ystyle=5, levels=[0],position=position, /noerase, /closed, /follow,/ISO
print, !x.crange,!y.crange
s = size(e2)
plots, (s[1]-1)/2.,(s[2]-1)/2.,PSYM=2

;map_set, limit = [20+south_crop,-115+west_crop,30-north_crop,-100-east_crop], position=position, /noerase, /noborder
;map_continents
; Draw box
;plots,[-120,-75,-75,-120,-120],[33,33,48,48,33],thick=2

lons=indgen(35)*2-124
lonnames = STRTRIM(-lons,2)+'W'
lats=indgen(20)*2+20 ; used for JAS note
latnames = STRTRIM(lats,2)+'N'
;map_grid, lons=lons, lonnames=lonnames,lats=lats, latnames=latnames,/box_axes

contourbar, levels, colors, position = [0.3,0.05,0.7,0.07], format = '(I4)', title="surface elevation above mean sea level [m]"


;!P.FONT = old_font



PRO GET_MONTH_RANGE, filename,first_month,last_month
	
	; Reads m1 and m2 from the filename string.
	; filename string can be in several formats:
	; (with any extension)
	;
	; date range		: m1dd-m2dd.txt
	; specific year		: yyyy_m1dd-m2dd.txt
	; file path included	: /path/yyyy_m1dd-m2dd.txt

	last_slash = STRPOS(filename, '/',/REVERSE_SEARCH)
	last_underscore = STRPOS(filename, '_',last_slash)
	start = 1+MAX([last_slash,last_underscore])


	string_first_month = STRMID(filename,start,2)
	start = start + 5
	string_last_month = STRMID(filename,start,2)


	first_month = FIX(string_first_month)
	last_month = FIX(string_last_month)

END ; PRO GET_MONTH_RANGE



FUNCTION GPCC

	;http://daac.gsfc.nasa.gov/CAMPAIGN_DOCS/FTP_SITE/INT_DIS/readmes/gpcc.html

	;Precipitation data are main input to global hydrological cycles and climate
	;models. The conventional rain-gauge measurements are the only direct
	;measure of rain-fall. This dataset is comprised of monthly gridded
	;area-mean rainfall totals for the period January 1986 to March 1999
	;on a 1 by 1 degree global grid.

	;The original precipitation data set has been produced at Global Precipitation
	;Climatology Centre (GPCC), in an effort to provide global data sets of
	;area-averaged and time-integrated precipitation fields based on surface rain
	;gauge measurements. The GPCC collects monthly precipitation totals
	;received from CLIMAT and SYNOP reports via the World Weather Watch
	;GTS (Global Telecommunication System) of the World Meteorological
	;Organization (WMO). The GPCC also acquires monthly precipitation data
	;from international/national meteorological and hydrological
	;services/institutions. An interim database of about 6700 meteorological
	;stations is defined. Surface rain-gauge based monthly precipitation data from
	;these stations are analyzed over land areas and a gridded dataset is
	;created (Rudolf, 1996; Rudolf et. al. 1994 ; and Rudolf, 1993), using
	;a spatial objective analysis method.


	; Meridionally averaged, area-weighed average rainfall rate 
	; from 1996-1998 0601-0831, based on the GPCC 1 degree
	; global climatology of monthly rainfall (see above). 

	array = [ 0.5115265, 0.6552233, 0.8361250, 0.9461784,  0.9993045,  1.0470097,  1.0582789,  1.1114146,  1.2371154,  1.3969785,  1.5949842,  1.7204108, 1.9543395,  2.2625911,  2.3558648,  2.4078722,  2.4606457,  2.4969933,  2.5702507,  2.7063057,  2.9936841,  3.1672752,  3.1154878,  2.9807217, 3.1813164,  3.3218296,  3.3094180,  3.0959442,  3.0138693,  3.0167332,  3.0142622,  2.8299801,  3.0263996,  3.1660004,  3.1196167,  3.0013633, 2.9049916]

	RETURN, array
END ; GPCC





PRO Hovm_rain2, file1, file2, file3, file4, DEBUG=debug, EPSI=epsi, $
	PS=ps, NAMEPS=nameps, RAW=raw, $
	COLORS=colors

; This parses a text file consisting of average rain rates
; as a function of distance and UTC time. 


; Remember current plotting device

mydevice = !D.NAME
mypmulti = !P.MULTI



; 4 plots to a page  (2 x 2)
!P.MULTI = [0,2,2,0,0]


; Assume dx = 0.2 and a longitude range of -115 to -78.

dx = 0.2
nx = (-78.-(-115.))/dx
lons = findgen(-78.-(-115.))*dx - 115.


; Load color table
colortable = KEYWORD_SET(colors) ? colors : 0
LOADCT,colortable

; Switch to PostScript device if PS keyword was set.

IF KEYWORD_SET(epsi) THEN ps = 1
IF KEYWORD_SET(ps) THEN BEGIN
	SET_PLOT, 'PS'
	TVLCT,0,0,0,0
	DEVICE, /LANDSCAPE
	nameps = KEYWORD_SET(nameps) ? nameps $
				     : KEYWORD_SET(epsi) ? 'idl.epsi' $
							 : 'idl.ps'
	DEVICE, /COLOR
	DEVICE, PREVIEW=0
	IF KEYWORD_SET(epsi) THEN DEVICE, PREVIEW=1
	IF KEYWORD_SET(epsi) THEN DEVICE, /ENCAPSULATED
	DEVICE, FILENAME=nameps
ENDIF




debug  = KEYWORD_SET(debug)  ? debug   :   0


;  Open and read the data files.


CD, '/elder1/ahijevyc/MSFC_NIDS', CURRENT=old_dir
RESOLVE_ROUTINE, 'precip_contour'


data = READ_ASCII(file1)
data = data.field001
s = size(data)
d1 = FLTARR(s[1],2*s[2])
d1[*,0:s[2]-1]=data
d1[*,s[2]:2*s[2]-1]=data
GET_MONTH_RANGE, file1,first_month,last_month
GET_R,first_month,last_month,r1


data = READ_ASCII(file2)
data = data.field001
s = size(data)
d2 = FLTARR(s[1],2*s[2])
d2[*,0:s[2]-1]=data
d2[*,s[2]:2*s[2]-1]=data
GET_MONTH_RANGE,file2,first_month,last_month
GET_R,first_month,last_month,r2


data = READ_ASCII(file3)
data = data.field001
s = size(data)
d3 = FLTARR(s[1],2*s[2])
d3[*,0:s[2]-1]=data
d3[*,s[2]:2*s[2]-1]=data
rawd3=d3
GET_MONTH_RANGE,file3, first_month,last_month
GET_R,first_month,last_month,r3



data = READ_ASCII(file4)
data = data.field001
s = size(data)
d4 = FLTARR(s[1],2*s[2])
d4[*,0:s[2]-1]=data
d4[*,s[2]:2*s[2]-1]=data
GET_MONTH_RANGE,file4,first_month,last_month
GET_R,first_month,last_month,r4


; Compensate for artificially depressed areal averages
; caused by including regions outside the range of the
; WSR-88D radar network.
;
; Compensate for these data-void regions by default.
;
; Divide the areal average by a constant coeffiecnt less than or equal to 1.
; This coefficient is equal to the "effectively-covered area"
; divided by the total area of the Hovmoller strip.  These coefficients
; were calcuated in an earlier exercise and are hard-wired into a function 
; listed below.
;
; Override the default by setting the keyword, "raw" (i.e. use the
; "raw" data).
;



IF NOT KEYWORD_SET(raw) THEN BEGIN
	FOR i=0,nx-1 DO BEGIN
		lon = FLOOR(-115. + i * dx)
		coefficient = RADAR_COVERAGE200(lon)
		command = 'radar_coverage -r 200 30 '+STRING(lon)+$
			' 48 ' + STRING(lon+1)

		;PRINT, command
		;SPAWN, command, coefficient
		;PRINT, 'coef= ', coefficient
		
		d1[i,*] = d1[i,*] / coefficient
		d2[i,*] = d2[i,*] / coefficient
		d3[i,*] = d3[i,*] / coefficient
		d4[i,*] = d4[i,*] / coefficient

	ENDFOR

ENDIF






;***************************
;
; Create graphical output.
;
;***************************




IMAGE_CONT,d1,/WINDOW_SCALE, TITLE=title(file1), XSTYLE=5, YSTYLE=5
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1
AXIS, YAXIS=0, YTICKFORMAT='YTICKS', YSTYLE=1, YTICKS=4, $
YTICKV=[0,48,96,144,191]
COLORBAR, /VERTICAL, RANGE=[0,MAX(d1)], $
	FORMAT='(F4.2)', POSITION=NEWPOSITION(), $
	/RIGHT



IMAGE_CONT,d2,/WINDOW_SCALE, TITLE=title(file2), XSTYLE=5, YSTYLE=5
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1 
AXIS, YAXIS=0, YTICKFORMAT='YTICKS', YSTYLE=1, YTICKS=4, $
YTICKV=[0,48,96,144,191]
COLORBAR, /VERTICAL, RANGE=[0,MAX(d2)], $
	FORMAT='(F4.2)', POSITION=NEWPOSITION(), $
	/RIGHT


IMAGE_CONT,d3,/WINDOW_SCALE, TITLE=title(file3), XSTYLE=5, YSTYLE=5
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1, XTITLE='longitude'
AXIS, YAXIS=0, YTICKFORMAT='YTICKS', YSTYLE=1, YTICKS=4, $
YTICKV=[0,48,96,144,191], YTITLE='UTC time (hr)'
COLORBAR, /VERTICAL, RANGE=[0,MAX(d3)], $
	FORMAT='(F4.2)', POSITION=NEWPOSITION(), $
	/RIGHT, TITLE="mm/hr"



IMAGE_CONT,d4,/WINDOW_SCALE, TITLE=title(file4), XSTYLE=5, YSTYLE=5
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1
AXIS, YAXIS=0, YTICKFORMAT='YTICKS', YSTYLE=1, YTICKS=4, $
YTICKV=[0,48,96,144,191]
COLORBAR, /VERTICAL, RANGE=[0,MAX(d4)], $
	FORMAT='(F4.2)', POSITION=NEWPOSITION(), $
	/RIGHT








; The page is full now, so the next plot will begin on a new page.
IF !D.NAME EQ 'X' THEN BEGIN 
	PRINT, 'Type .CONT to continue'
	STOP
ENDIF






; Integrate the rain rate arrays over 0-24 hr UTC (dimension 2)
; and plot the resultant daily cumulative rainfall
; as a function of distance.
;
; Note that the value is HALVED since the entire range of
; dimension 2 covers two days.
; The second half is a duplicate of the first half, so dividing
; the total by 2 is equivalent to integrating over just one day.
;

; Get another set of average rain rates from an independent data set,
; such as rain gauges, and plot them versus distance.

; WARNING: Assumes that the other set has the same distance range.


; Index	Linestyle	
; 0	Solid	
; 1	Dotted	
; 2	Dashed	
; 3	Dash Dot	
; 4	Dash Dot Dot Dot	
; 5	Long Dashes	



dt = 0.25

PLOT, dt*TOTAL(d1,2)/2., TITLE=title(file1), XSTYLE=5 
OPLOT, CONGRID(r1,nx), LINESTYLE=2
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1


PLOT, dt*TOTAL(d2,2)/2., TITLE=title(file2), XSTYLE=5
OPLOT, CONGRID(r2,nx), LINESTYLE=2
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1


PLOT, dt*TOTAL(d3,2)/2., TITLE=title(file3), XSTYLE=5, YTITLE="mm/day"
OPLOT, CONGRID(r3,nx), LINESTYLE=2
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1, XTITLE='longitude'
OPLOT, CONGRID(GPCC(), nx), LINESTYLE=1
;POLYFILL, [INDGEN(185),REVERSE(INDGEN(185))], [dt*TOTAL(d3,2)/2.,REVERSE(dt*TOTAL(rawd3,2)/2.)], col=100




PLOT, dt*TOTAL(d4,2)/2., TITLE=title(file4), XSTYLE=5
OPLOT, CONGRID(r4,nx), LINESTYLE=2
AXIS, XAXIS=0, XTICKFORMAT='XTICKS', XSTYLE=1








; Reset !P.MULTI (probably back to the normal one plot per page)
!P.MULTI = mypmulti



IF KEYWORD_SET(ps) THEN DEVICE, /CLOSE


; Return to the original output device.

SET_PLOT, mydevice

; Return to the old working directory.
CD, old_dir

END





FUNCTION NEWPOSITION

	; This function was created to account for a non-zero !P.MULTI
	; array (i.e. more than one plot per page).
	; By default, the colorbar is positioned with no regard to 
	; the !P.MULTI value.
	; It positions and scales the colorbar as if there was 
	; only one plot.
	; This is not what I want for my multi-plot page.
	; This function returns the position array of the colorbar that
	; is scaled and placed correctly relative to an individual
	; plot on the page.
	 
	default_position = [0.96,0.15,1.,0.95]

	RETURN, default_position * [!X.REGION[1]-!X.REGION[0],!Y.REGION[1]-!Y.REGION[0],!X.REGION[1]-!X.REGION[0],!Y.REGION[1]-!Y.REGION[0]] + [!X.REGION[0],!Y.REGION[0],!X.REGION[0],!Y.REGION[0]]

END ; FUNCTION NEWPOSITION




FUNCTION TITLE, filename
	last_slash = STRPOS(filename, '/',/REVERSE_SEARCH)
	last_period = STRPOS(filename, '.', /REVERSE_SEARCH)
	start = last_slash+1
	title = STRMID(filename,start,last_period-start)
	;title = filename + ' average rain rate estimated from radar reflectivity!C'
	;title = title + 'over 30 to 48 deg N as function of distance and UTC time'

	RETURN, title
END ; END FUNCTION TITLE  









FUNCTION XTICKS, axis, index, value
	value = value/5 - 115.
	label = (value GT 0) ? 'E' : 'W'
	RETURN, STRTRIM(ROUND(ABS(value)),2) + ' ' + label
END ; FUNCTION XTICKS




FUNCTION YTICKS, axis, index, value
	time = ROUND(value*0.25) mod 24 
	RETURN, STRTRIM(time,2)
END ; FUNCTION YTICKS

pro readv

; Read observations and forecasts of precipitation and radar reflectivity over the U.S. and plot on 4-panel page.

;-----------------------------------------------------------------------
; Ascii files produced by Mike Baldwin. Excerpt from his email follows: 

; Precip data from 2005 NSSL/SPC Spring Program. Files are ASCII text data over same subset of US as for reflectivity text data.  Observed data is from NCEP Stage II radar/gage 4km hourly precip product. WRF forecasts have been remapped to same 4km grid as observed data (what I call g240, used to be NCEP grid number for this grid). g240 is NCEP name of full grid, the 4km Stage IV grid.  Lat/lons of grid are in latnorth.txt and loneast.txt.

; forecasts: wrf2caps, wrf4ncar, and wrf4ncep_YYYYMMDDHH.g240.fXX.txt
; Observed precipitation files: ST2ml_YYYYMMDDHH.g240.txt

; YYYYMMDDHH is start time of forecast or (in the case of observations) valid time. XX is forecast hour.

; Files written row by row, 501 rows of 601 integers followed by linefeed, starting with bottom row. Units are hundreths of an inch.  (multiply by 0.254 to convert to mm). 

;-----------------------------------------------------------------------

; Ascii files copied from score@pigpen.rap:/pd4/score/personal/eric/ObjectProject/raw/ Nov 2006.
;
; IDL code initially written Nov 2006 by David Ahijevych ahijevyc@ucar.edu
; 

	if !d.name eq 'X' then device, decomposed=0
	if !D.NAME eq 'PS' then begin
		device, /close
		device, /color
	endif

	basepath = '/data/data1a/pd/ahijevyc/verif/ObjectProject/raw/'
	pos0 = [0.05,0.54,0.45,0.93]
	pos1 = [0.50,0.54,0.90,0.93]
	pos2 = [0.05,0.05,0.45,0.44]
	pos3 = [0.50,0.05,0.90,0.44]
	!P.CHARSIZE=0.5
	loadct, 41, FILE='/users/ahijevyc/IDL/resource/colors/colors1.tbl'
	lons = (read_ascii(basepath+'loneast.txt')).field001
	lons = lons - 360
	lats = (read_ascii(basepath+'latnorth.txt')).field001
	limit = [lats[0,0],lons[0,0],lats[0,500],lons[0,500],lats[600,500],lons[600,500],lats[600,0],lons[600,0]]
	; full grid description below from http://wwwt.emc.ncep.noaa.gov/mmb/ylin/pcpanl/QandA/#GRIDINFO
	;limit = [23.117,-119.023,53.509,-134.039,45.619,-59.959,19.805,-80.750]


	files = file_search(basepath+'pcp/st2/ST2ml_2005050*.txt',count=count)
	if count eq 0 then stop
	levels = 5+10*indgen(10) ; 1/100's of an inch
	for ifile=0,n_elements(files)-1 do begin
		st2file = files[ifile]
		filename = file_basename(st2file)
		ichr = strpos(filename,'_')
		if ichr lt 0 then stop
		type  = strmid(filename,0,ichr)
		year  = strmid(filename,ichr+1,4)
		month = strmid(filename,ichr+5,2)
		day   = strmid(filename,ichr+7,2)
		hour  = strmid(filename,ichr+9,2)
		;ichr = strpos(filename,'f',/reverse_search)
		;if ichr ge 0 then fhour = strmid(filename,ichr+1,2)

		jday = julday(month,day,year,hour,0,0)
		if hour le 6 then begin
			fhour = hour + 24
			jday = jday - 1
		endif else fhour = hour
		caldat, jday, fmonth, fday, fyear
		format = '(I4.4,I2.2,I2.2,"00.g240.f",I2.2)'
		wrf2file = basepath + 'pcp/wrf2/wrf2caps_' + STRING(fyear,fmonth,fday,fhour, FORMAT = format  ) + '.txt'
		wrf4ncarfile = basepath + 'pcp/wrf4ncar/wrf4ncar_' + STRING(fyear,fmonth,fday,fhour, FORMAT = format ) + '.txt'
		wrf4ncepfile = basepath + 'pcp/wrf4ncep/wrf4ncep_' + STRING(fyear,fmonth,fday,fhour, FORMAT = format ) + '.txt'


		for itype = 0,3 do begin
			case itype of
				0: fullpath = st2file
				1: fullpath = wrf2file
				2: fullpath = wrf4ncarfile
				3: fullpath = wrf4ncepfile
			endcase
			case itype of
				0: pos = pos0
				1: pos = pos1
				2: pos = pos2
				3: pos = pos3
			endcase

			if file_test(fullpath) ne 1 then begin
				xyouts, 0.5*(pos[0]+pos[2]), 0.5*(pos[1]+pos[3]), 'NOT AVAILABLE', /norm, alignment=0.5
				continue
			endif
			var = (read_ascii(fullpath)).field001
			; polar stereographic.  Polar implies 90 degrees N.
			map_set, 90, 360-105, 0, limit=limit, position = pos, /stereo, noerase=itype
			title=file_basename(fullpath)
			xyouts, 0.5*(pos[0]+pos[2]), pos[3]+0.04, title, /norm, alignment=0.5
			contour, var, lons, lats, /cell_fill, /overplot, xstyle=4, ystyle=4, levels=levels, c_colors=mycolors(levels)
			map_continents, /usa
			map_grid, /box
		endfor


		contourbar, levels, mycolors(levels), position=[pos[2]+0.05,pos[1]+0.05,pos[2]+0.055,pos[3]-0.05],/vertical, title='hundredths of an inch',format='(I2)',charsize=!P.CHARSIZE*0.8


		xyouts, 0.05, pos[1]-0.1, 'created '+systime(0,/UTC),/norm, charsize=0.4

	endfor
	if !D.NAME eq 'PS' then device, /close
end

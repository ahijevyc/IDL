FUNCTION DIGIT2MONTH, digit

   CASE digit OF
      ' 5': string_month='May'
      ' 6': string_month='Jun'
      ' 7': string_month='Jul'
      ' 8': string_month='Aug'
      ' 9': string_month='Sep'
   ELSE: string_month='???'
   ENDCASE


   RETURN, string_month

END





FUNCTION INDEX2MONTH, index

   CASE 1 OF
      (index GE 0)   AND (index LE 30) : month = ' 5'
      (index GE 31)  AND (index LE 60) : month = ' 6'
      (index GE 61)  AND (index LE 91) : month = ' 7'
      (index GE 92)  AND (index LE 122): month = ' 8'
      (index GE 123) AND (index LE 152): month = ' 9'
   ELSE: PRINT, 'cannot translate index to month'
   ENDCASE


   RETURN, month

END






FUNCTION INDEX2DAY, index
   CASE 1 OF
      (index GE 0)   AND (index LE 30) : month = ' 5'
      (index GE 31)  AND (index LE 60) : month = ' 6'
      (index GE 61)  AND (index LE 91) : month = ' 7'
      (index GE 92)  AND (index LE 122): month = ' 8'
      (index GE 123) AND (index LE 152): month = ' 9'
   ELSE: PRINT, 'cannot translate index to month'
   ENDCASE

   CASE month OF
      ' 5': day = index + 1
      ' 6': day = index - 30
      ' 7': day = index - 60
      ' 8': day = index - 91
      ' 9': day = index - 122
   ELSE: PRINT, 'date not a 2-digit month of type string'
   ENDCASE



   RETURN, STRTRIM(STRING(day),1)

END







FUNCTION DATE2INDEX, date

   string_month = STRMID(date,3,2, /REVERSE_OFFSET)
   day = LONG(STRMID(date,1,2,/REVERSE_OFFSET))


   CASE string_month OF
      ' 5': i = -1
      ' 6': i = 30
      ' 7': i = 60
      ' 8': i = 91
      ' 9': i = 122
   ELSE: PRINT, 'date not a 2-digit month of type string'
   ENDCASE


   RETURN, i + day

END





FUNCTION XTICKS, axis, index, value
   label = (value GT 0) ? 'E' : 'W'
   RETURN, STRTRIM(STRING(ROUND(ABS(value))),2) + ' ' + label
END





PRO PREP, day, string_month, string_day, name_month
   IF (STRTRIM(day,1) GT '366') THEN BEGIN
      string_month = STRMID(day,3,2, /REVERSE_OFFSET)
      string_day = STRMID(day,1,2, /REVERSE_OFFSET)
      name_month=DIGIT2MONTH(string_month)
      day=DATE2INDEX(day)
   ENDIF ELSE BEGIN
      string_month=INDEX2MONTH(day)
      string_day = INDEX2DAY(day)
      name_month=DIGIT2MONTH(string_month)
   ENDELSE
END




PRO HV0, filename, $
	d1, d2, COLOR=color, DEBUG=debug, EPSI=epsi, FIRST_HR=first_hr, $
	INCHES=inches, TOTAL_HRS=total_hrs, CUMULATIVE=cumulative, $ 
	MAP=map, PS=ps, NAMEPS=nameps, ORIENT_LABEL=orient_label, $
	RAW=raw, X_LABEL=x_label, YMAX=ymax

	; This parses a text file consisting of average rain rates
	; for a particular longitude range and time range and 
	; date.
	; It averages across all years for each day of the year.
	; And plots everything



	; Remember current plotting device

	mydevice = !D.NAME

	; filename : this is all the yearly data files, concatenated


	; Load color table
	;TEK_COLOR
	IF KEYWORD_SET(color) THEN LOADCT,39 ELSE LOADCT,0




	; Switch to PostScript device if PS keyword was set.

	IF KEYWORD_SET(epsi) THEN ps = 1
	IF KEYWORD_SET(ps) THEN BEGIN
		SET_PLOT, 'PS'
		nameps = KEYWORD_SET(nameps) ? nameps : KEYWORD_SET(epsi) ? 'idl.epsi' : 'idl.ps'
		IF KEYWORD_SET(color) THEN DEVICE, /COLOR
		IF KEYWORD_SET(epsi) THEN DEVICE, PREVIEW=1
		IF KEYWORD_SET(epsi) THEN DEVICE, /ENCAPSULATED
		DEVICE, FILENAME=nameps
	ENDIF





	inch2mm=25.4

	; Rain rates will be converted to inches per month if INCHES is set.
	; mm/hr * 24hr/day * 30day/month / 25.4mm/inch = inch/month 

	; Otherwise rain rates will be converted to mm per day.
	; mm/hr * 24hr/day = mm/day

	conversion_factor = KEYWORD_SET(inches) ? 24.*30./inch2mm : 24.

	; Use the user-defined ymax argument if available,
	; Otherwise, use 0.25 mm/hr
	ymax = KEYWORD_SET(ymax) ? ymax $
	                         : conversion_factor * 0.25



	debug        = KEYWORD_SET(debug)        ? debug        :   0

	day1         = KEYWORD_SET(d1)           ? d1           : 0501
	day2         = KEYWORD_SET(d2)           ? d2           : 0831

	total_hrs    = KEYWORD_SET(total_hrs)    ? total_hrs    :  24
	first_hr     = KEYWORD_SET(first_hr)     ? first_hr     :  18 

	orient_label = KEYWORD_SET(orient_label) ? orient_label :   0 





	PREP, day1, string_month1, string_day1, name_month1
	PREP, day2, string_month2, string_day2, name_month2




	IF debug THEN PRINT, 'day range = ' + name_month1 + ' ' + $
		string_day1 + ' to ' + name_month2 + ' ' + string_day2





	;  Open the data file.

	OPENR, alltxt, filename, /GET_LUN


	;  Declare the correct variable type to be read in.
	record = FLTARR(153)	; 153 is the total number of days from 1 May-30 Sep.
				; May	 31
				; Jun	 30
				; Jul	 31
				; Aug	 31
				; Sep	 30
				; total	153
	date1 = 0L & date2 = 0L  ;  Both date variables are long integers.
	h1	= 0
	h2	= 0
	hr_index	= 0
	lon_index	= 0
	year_index	= 0


	first_record = 1 
	width	= 0
	hr_interval	= 0
	first_lon = -115.
	n_lon	= 0
	t	= 0

	n_years	= 10 
	bad	= -999.


	;  Loop through each line of the file,
	;  assigning each areally-averaged rain rate value
	;  to the appropriate array element.


	;IF debug THEN PRINT, 'lon2=',lon2, ' hr1=',hr1,' hr2=',hr2
	WHILE (NOT EOF(alltxt)) DO BEGIN
		READF, alltxt, date1, date2, l1, l2, h1, h2, record, $
			FORMAT='(I8,1X,I8,F0,3X,F0,20X,I3,I3,153(/F0))'
		IF h2 LT 0 THEN h2 = -h2

		IF date1 MOD 10000 NE 501 THEN BEGIN
			PRINT, 'Unexpected first date:',date1
			STOP
		ENDIF

		IF date2 MOD 10000 NE 930 THEN BEGIN
			PRINT, 'Unexpected last date:',date2
			STOP
		ENDIF


		IF NOT first_record AND l2-l1 NE width THEN BEGIN
			PRINT, 'Unexpected width:', l2-l1
			STOP
		ENDIF

		width = l2-l1

		IF first_record THEN n_lon = (-78 - first_lon)/width + 1

		IF NOT first_record AND h2-h1 NE hr_interval THEN BEGIN
			PRINT, 'Unexpected hr_interval:', h2-h1
			STOP
		ENDIF

		hr_interval = h2-h1

		;IF debug THEN PRINT, 'h1=',h1,' h2=',h2
		;IF debug THEN PRINT, 'l1=',l1,' lon1=',lon1
		hr_index = h1/hr_interval
		lon_index = (l1+l2)/2 - first_lon
		year_index = date1/10000 - 1996



		;  Prepare an array of the proper dimensions.
		;  Note that the years dimension must be last if 
		;  n_years is greater than the actual number of 
		;  years available.  The last dimension varies 
		;  slowest in IDL, so when the array is flattened, 
		;  the elements corresponding to non-existent
		;  years reside at the end of the list.

		IF first_record THEN $
			t = MAKE_ARRAY(total_hrs/hr_interval,n_lon,153,n_years, $
			/FLOAT, VALUE=bad)




		t[hr_index,lon_index,*,year_index] = record
		;IF debug THEN PRINT, 'year_index=',year_index,' lon_index=',lon_index

		first_record = 0; Not on the first record anymore.
	ENDWHILE

	FREE_LUN, alltxt





	center_lons = first_lon + INDGEN(n_lon)*width
	xrange = [MIN(center_lons) - width/2., MAX(center_lons) + width/2.]
	IF debug THEN PRINT, "center_lons =",center_lons


	final_array = fltarr(n_lon)

	; Create an array whose elements are the start UTC times 
	; of all the time intervals

	time_array = INDGEN(24/hr_interval)*hr_interval

	; Make sure one of the start UTC times is equal to the
	; requested first time.

	match = WHERE(time_array EQ first_hr,count) 

	IF count EQ 0 THEN BEGIN
		PRINT, 'No start UTC times equal the requested first time'
		STOP
	ENDIF


	; Rotate the time array elements so that the first one
	; corresponds to requested first time.

	WHILE time_array[0] NE first_hr DO time_array = SHIFT(time_array,1)


	time_array = REVERSE(time_array)

	IF debug THEN PRINT, 'time array = ', time_array






	FOR j = FIX(total_hrs/hr_interval)-1, 0, -1 DO BEGIN
		hr1 = time_array[j]


		old_final_array = final_array
		FOR i=0, n_lon-1 DO BEGIN
			lon1 = first_lon - width/2. + i*width
			lon2 = lon1 + width
			hr2  = hr1 + hr_interval
			;SPAWN, 'grep -A 153 "\' + STRTRIM(STRING(lon1,FORMAT='(F8.1)'),1) + ' to \' + $
			;	STRTRIM(STRING(lon2,FORMAT='(F8.1)'),1) + $
			;	' deg longitude hours ' + STRTRIM(STRING(hr1),1) + '-' + $
			;	STRTRIM(STRING(hr2),1) + '" ' + datapath + '/' + datafile, t

			format='(A,$)'
			;IF debug THEN PRINTF, -1, '.' , FORMAT=format

			warnings=0
			IF debug THEN HELP, t


			; Make sure you access the correct time.
			hr_index = hr1/hr_interval


			; Flatten the array to one dimensional list for HV1.

			new_t = REFORM(t[hr_index,i,*,*],n_years*153)
			IF debug THEN HELP, new_t
			IF debug THEN PRINT, 'hour index=',hr_index
			;IF debug THEN PRINT, new_t

			;******************************** 
			;  new_t      : input to hv1
			;  my_array   : output from hv1
			;  most_recent_year  : output from hv1
			;  warnings   : output   "   "
			;
			;********************************
        
			HV1, new_t, my_array, most_recent_year, warnings

			IF (warnings GT 0) THEN BEGIN
				PRINT, 'problem area: ' + $
				STRTRIM(STRING(lon1,FORMAT='(F8.1)'),1) + ' to ' + $
				STRTRIM(STRING(lon2,FORMAT='(F8.1)'),1) + $
				' degrees longitude hours ' + STRTRIM(hr1,1) + '-' + $
				STRTRIM(hr2,1)
			ENDIF

			number_of_time_intervals = 24./hr_interval
		
			; Compensate for artificially depressed areal averages
			; caused by including regions outside the range of the
			; WSR-88D radar network.
			;
			; Compensate for these data void regions by default.
			;
			; Divide the areal average by a constant coefficient
			; less than or equal to 1.
			; This coefficient is equal to the "effectively-covered area"
			; divided by the total area of the Hovmoller strip.
			; These coefficients were calcuated in an earlier exercise 
			; and are hard-wired into a function listed below.
			;
			; Override the default by setting the keyword, "raw" (i.e. use the
			; "raw" data).
			;


			coefficient = KEYWORD_SET(raw) 	? 1. $
							: RADAR_COVERAGE(center_lons[i])
		

			new_amount = MEAN(my_array[day1:day2])
			new_amount = new_amount/number_of_time_intervals
			new_amount = new_amount/coefficient
			new_amount = new_amount*conversion_factor

			final_array[i] = KEYWORD_SET(cumulative) $
				? final_array[i] + new_amount $
				: new_amount

      
			; go to next longitude interval

		ENDFOR ; Done with all longitude intervals

		IF debug THEN PRINT, final_array





		;***************************
		;
		; Create graphical output.
		;
		;***************************





		subtitle = (name_month1+string_day1 EQ name_month2+string_day2) $
			? name_month1 + ' ' + string_day1 $
		   	: name_month1 + ' ' + string_day1 + ' - ' + $
		   	  name_month2 + ' ' + string_day2

		timespan = KEYWORD_SET(inches) ? 'monthly' : 'daily'

		title = '1996-' + STRTRIM(most_recent_year,1) + ' average ' $
			+ timespan + ' rainfall estimated from radar reflectivity!C'
		title = title + 'over 30 to 48 deg N broken down by UTC time'

		IF debug THEN PRINT, 'hr1=',hr1
		IF debug THEN PRINT, 'first_hr=',first_hr


		; If this is the first hour interval, do all the
		; titles and subtitles and labelling.

		IF (hr1 EQ first_hr) THEN BEGIN
			ytitle = KEYWORD_SET(inches) ? 'inches' : 'mm'
			PLOT, center_lons, final_array, SUBTITLE= subtitle, $
				TITLE=title, XRANGE=xrange, $
				XSTYLE=1, XTICKFORMAT='XTICKS', $
				XTITLE='degrees longitude', $
				YRANGE=[0,ymax] , YSTYLE=1, $
				YMARGIN=[!Y.MARGIN[0],2*!Y.MARGIN[1]], $
				YTITLE=ytitle,  /NODATA
			old_final_array[*] = !Y.CRANGE[0]
		ENDIF 



		; Construct a one-dimensional vector of x-values for the polygon.
		; We will traverse its vertices from left to right
		; and back again.

		pxval = [center_lons, REVERSE(center_lons)]


		IF NOT KEYWORD_SET(cumulative) THEN BEGIN
			displacement = 1.01 * MAX(old_final_array)
			displacement = FLOAT(ymax) * j / FIX(total_hrs/hr_interval)
			IF debug THEN print, 'displacement=',displacement
			old_final_array[*] = displacement
			final_array[*] = final_array + displacement
		ENDIF

		; Make a polygon

		col =  ROUND((hr1+1)/(24.+1.) * !D.N_COLORS)
		POLYFILL, PXVAL, [old_final_array, REVERSE(final_array)], $
			COL = col


		; Figure out where to put the hour labels.

		diff = final_array-old_final_array
		max = MAX(diff,max_ind)

		IF (KEYWORD_SET(x_label)) THEN BEGIN
			label_xpos = x_label
			xpos_index = WHERE(center_lons EQ x_label,count)

			IF (count EQ 1) THEN $
				label_ypos = old_final_array[xpos_index]+(diff[xpos_index]*0.3) $
			ELSE $
			        PRINT, 'Bad label place'
		
		ENDIF ELSE BEGIN
			label_xpos = center_lons[max_ind]
			label_ypos = old_final_array[max_ind]+(max*0.3)
		ENDELSE

		label = STRTRIM(STRING(hr1),1) + '-' + STRTRIM(STRING(hr1+hr_interval),1)
		XYOUTS, label_xpos, label_ypos, label, $
		ALIGNMENT=0.5, ORIENTATION=orient_label


		; Go to next hour interval.

	ENDFOR ; Done with all hour intervals.


 

	; Draw an overlay map of the U.S. if keyword "map" is set.
 
	IF KEYWORD_SET(map) THEN BEGIN
	   position=[!X.WINDOW[0],!Y.WINDOW[0],!X.WINDOW[1],!Y.WINDOW[1]]

	   IF debug THEN BEGIN
	      PRINT
	      PRINT, !X.CRANGE
	      PRINT, !X.WINDOW
	      PRINT, !Y.WINDOW
	   ENDIF

	   MAP_SET,/CYLINDRICAL,0,0,LIMIT=[30,xrange[0],48,xrange[1]], $
	      POSITION=position, /NOERASE
	   MAP_CONTINENTS, /USA
	ENDIF



	; Write current time and comments on picture.

	comment = KEYWORD_SET(raw) ? '' : 'normalized'
	current_time = SYSTIME(0)
	comment = comment + '!Ccreated!C' + current_time


	IF debug THEN BEGIN
		PRINT
	ENDIF

	XYOUTS, 0.96*(!X.WINDOW[1]-!X.WINDOW[0])+!X.WINDOW[0], $
        0.07*(!Y.WINDOW[1]-!Y.WINDOW[0])+!Y.WINDOW[0], $
        comment, ALIGNMENT = 1, /NORMAL, CHARSIZE=0.75


	PRINT

	IF KEYWORD_SET(ps) THEN DEVICE, /CLOSE


	; Return to the original output device.

	SET_PLOT, mydevice
 
END













  



















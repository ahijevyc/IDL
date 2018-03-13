pro mymap, limits, pos, title=title, subtitle=subtitle, noerase=noerase

	;Set NOBORDER keyword to not draw a border around map.  Map will fill extent of plotting region. If NOBORDER is not specified, a margin equalling 1% of plotting region will be placed between map and border.
	MAP_SET, limit=limits,/usa,/continents,position=pos,/noborder, noerase=keyword_set(noerase)
	if keyword_set(title) then xyouts, (pos[0]+pos[2])/2., pos[3]+0.05, title, /normal, alignment=0.5
	if keyword_set(subtitle) then xyouts, (pos[0]+pos[2])/2., pos[1]-0.08, subtitle, /normal, alignment=0.5, charsize=0.9
	if ~keyword_set(noerase) then MAP_GRID, latlab=limits[1]+1, lonlab=limits[0], /box_axes, charsize=0.8

end ; pro mymap


pro read_meanraw

	if !D.NAME eq 'PS' then device, /close, /color
	LOADCT, 41, file="/users/ahijevyc/IDL/resource/colors/colors1.tbl", /SILENT
	
	;----templ0 template reads raw files prior to Dec 2003---
	
	templ0 = {$
		VERSION:         1.00000,$
		TEMPLATENAME:    'templ0 (prior to Dec 2003)', $
		ENDIAN:          'native',$
		FIELDCOUNT:      11L,$
		TYPECODES:       [3,4,4,4,4,4,4,3,3,4,3], $
		NAMES:           ['record_len0', 'x1', 'x2', 'y1', 'y2', 'xd', 'yd', 'nx', 'ny', 'data', 'record_len1'],$
		OFFSETS:         replicate('>0', 11),$
		NUMDIMS:         [0,0,0,0,0,0,0,0,0,3,0],$
		DIMENSIONS:      STRArr(11, 8),$
		REVERSEFLAGS:    BYTArr(11, 8),$
		ABSOLUTEFLAGS:   BYTArr(11),$
		RETURNFLAGS:     replicate(1,11),$
		VERIFYFLAGS:     [0,0,0,0,0,0,0,0,0,0,1],$
		DIMALLOWFORMULAS:[0,0,0,0,0,0,0,0,0,1,0],$
		OFFSETALLOWFORMULAS: INTArr(11),$
		VERIFYVALS:       ['', '', '', '', '', '', '', '', '', '', 'record_len0']}
	
	templ0.dimensions[9,0:2] = [['nx'],['ny'],['1']]


	;---------------------------------------------------
	; templ1 reads raw unformatted output of precip_freq.f
	; used from Dec 2003 - Mar 2007
	;
	;      OPEN(UNIT=10,file=fname1,status='UNKNOWN',form='UNFORMATTED')
	;      write(10,iostat=iostatus)version,X1,X2,Y1,Y2,XD,YD,nx,ny,nthresh,
	;     +  dBZ_thresholds,method,
	;     +  (((output_array(i,j,ithresh),i=1,nx),j=1,ny),ithresh=1,nthresh)
		
	templ1 = { $
		VERSION:          1.00000,$
		TEMPLATENAME:     'templ1 (used Dec 2003 - Mar 2007)', $
		ENDIAN:           'native',$
		FIELDCOUNT:       13L,$
		TYPECODES:        [4,4,4,4,4,4,4,3,3,3,4,1,4], $
		NAMES:       ['version','x1','x2','y1','y2','xd','yd','nx','ny','nthresh','dBZ_thresholds','method','data'],$
		OFFSETS:          ['>4','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0','>0'],$
		NUMDIMS:          [0,0,0,0,0,0,0,0,0,0,1,1,3],$
		DIMENSIONS:       STRArr(13, 8),$
		REVERSEFLAGS:     BYTArr(13, 8),$
		ABSOLUTEFLAGS:    BYTArr(13),$
		RETURNFLAGS:      [1,1,1,1,1,1,1,1,1,1,1,1,1],$
		VERIFYFLAGS:      [1,0,0,0,0,0,0,0,0,0,0,0,0],$
		DIMALLOWFORMULAS: [0,0,0,0,0,0,0,0,0,0,1,0,1],$
		OFFSETALLOWFORMULAS: INTArr(13),$
		VERIFYVALS:       ['1','','','','','','','','','','','','']}
	
	templ1.dimensions[10:11,0,0] = ['nthresh','80']
	templ1.dimensions[12,0:2,0] = [['nx'],['ny'],['nthresh']]
	

	;-----------------------------------------------------------------------
	; templ2 reads raw unformatted output from precip_freq.f
	; used after April 2007
	;
	;   OPEN(UNIT=10,file=fname1,status='UNKNOWN',form='UNFORMATTED')
	;   write(10,iostat=iostatus)version,X1,X2,Y1,Y2,XD,YD,nx,ny,nthresh,
	;  +  dBZ_thresholds,xscale,yscale,tscale,xavg,yavg,tavg,
	;  +  xmethod, ymethod, tmethod, 
	;  +  (((output_array(i,j,ithresh),i=1,nx),j=1,ny),ithresh=1,nthresh)
	
	; Remember, unformatted FORTRAN output precedes each record with the record length (written as a 4-byte integer). It also puts the same integer at the end of the record.
	templ2 = { $
		VERSION:          1.,$
		TEMPLATENAME:     'templ2 (used after Apr 2007)', $
		ENDIAN:           'native',$
		FIELDCOUNT:       23L,$
		TYPECODES:        [3,4,4,4,4,4,4,4,3,3,3,4,3,3,3,6,6,6,1,1,1,4,3], $
		NAMES:       ['record_len0', 'version', 'x1','x2','y1','y2', 'xd','yd', 'nx', 'ny', 'nthresh','dBZ_thresholds','xscale','yscale', 'tscale','xavg', 'yavg', 'tavg', 'xmethod', 'ymethod','tmethod', 'data', 'record_len1'],$
		OFFSETS:          replicate('>0', 23),$
		NUMDIMS:          [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,1,1,3,0],$
		DIMENSIONS:       STRArr(23, 8),$
		REVERSEFLAGS:     BYTArr(23, 8),$
		ABSOLUTEFLAGS:    BYTArr(23),$
		RETURNFLAGS:      replicate(1,23),$
		VERIFYFLAGS:      [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],$
		DIMALLOWFORMULAS: [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0],$
		OFFSETALLOWFORMULAS: INTArr(23),$
		VERIFYVALS:       ['', '2', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', 'record_len0']}
	
	templ2.dimensions[11,0,0] = ['nthresh']
	templ2.dimensions[18:20,0,0] = ['80','80','80']
	templ2.dimensions[21,0:2,0] = [['nx'],['ny'],['nthresh']]

	; As of 2007-04-16, Who knows why, but precip_freq adds 8 bytes that I can't account for after dBZ_thresholds array and before xscale, yscale, tscale set. So skip 8 bytes before reading xscale.
	; I fixed the problem. The error was in precip_freq where the dBZ_thresholds array still had undefined elements at the end of it.  precip_freq wrote an mthresh-element array, but defined and mentioned only nthresh elements.
	;templ2.offsets[12] = '>8'

	
	nt = 24
	ithresh = 6 ; 1=15dBZ, 3=25dBZ, 6=40dBZ


	savfile = 'diurnal.sav'
	if file_test(savfile) then restore, savfile, /verbose else begin
		for t = 0,nt-1,1 do begin
			;filename = string(t,format='("/data1a/pd/ahijevyc/NIDSdiurnalcycle/NOWrad20040601-0831_",i2.2,".raw")')
			filename = string(t,format='("/data1a/pd/ahijevyc/NIDSdiurnalcycle/scripts/NOWrad1996-20060601-0831_",i2.2,".raw")')
			filename = string(t,format='("/mmmtmp/ahijevyc/1996-20030601-0831_",i2.2,"freq40.raw")')
			print, filename
			openu, tmp_lun, filename, /get_lun
			readu, tmp_lun, record_len0, check
			free_lun, tmp_lun
			case floor(check) of
				-130	: template=templ0
				1		: template=templ1
				2		: template=templ2
			else		: message, 'raw data format unrecognized'
			endcase
			a=read_binary(filename,template=template)
			if floor(check) eq -130 then begin
				a.data = reform(a.data,a.nx,a.ny,1)
				nthresh = 1 & ithresh = 0
				indfreq = strpos(filename, 'freq', strpos(filename, '/')) + 4
				
				a = create_struct('dBZ_thresholds', [float(strmid(filename, indfreq, 2))], 'xscale',1, 'yscale',1, 'tscale',1, 'xavg',complex(0,0), 'yavg',complex(0,0), 'tavg',complex(0,0), 'xmethod','', 'ymethod','', 'tmethod','', a)
			endif
	
			; The bit below was used earlier to fix some bad output from precip_freq
			;openw, new_lun, filename, /get_lun
			;writeu, new_lun, a.record_len0, a.version, a.x1, a.x2, a.y1, a.y2, a.xd, a.yd, a.nx, a.ny, a.nthresh, a.dBZ_thresholds[0:9], a.xscale, a.yscale, a.tscale, a.xavg, a.yavg, a.tavg, a.xmethod, a.ymethod, a.tmethod, a.data, a.record_len1
			;free_lun, new_lun
	
			if (t eq 0) then begin
				; There's not enough memory to work on the original 3661x1837 grid.
				degrade_factor = 10 ; 3 is the minimum
				nx = MIN([3661/degrade_factor,a.nx])
				ny = MIN([1837/degrade_factor,a.ny])
				diurnal=fltarr(nx,ny,nt)
			endif
			if (MAX(a.data) eq 0.) then stop
			diurnal[*,*,t]=congrid(a.data[*,*,ithresh],nx,ny,1)
		endfor
	endelse
	
	
	dataset_desc = "WSI NOWrad (2km,15min,5dBZ) "
	iNR = strpos(filename, 'NOWrad') + 6
	date_range = strmid(filename, iNR, strpos(filename, '_', iNR)-iNR)
	print, a.xscale, a.yscale, a.tscale, a.xavg, a.yavg, a.tavg, STRING(a.xmethod), STRING(a.ymethod), STRING(a.tmethod)
	thresh = STRING(a.dBZ_thresholds[ithresh],format='(I2)')
	field_desc = '% time >= '+thresh+'dBZ'
	subtitle = dataset_desc + date_range + " xscale,yscale,tscale=" + strtrim(a.xscale,2) + ',' + strtrim(a.yscale,2) + "," + strtrim(a.tscale,2) + "!Cnx,ny,nt=" + strtrim(nx,2) + ',' + strtrim(ny,2) + ',' + strtrim(nt,2) + "  " + STRtrim(a.xmethod) + ", " + STRtrim(a.ymethod) + ", " + STRtrim(a.tmethod)
	

	latmin=20. & lonmin=-130. & latmax=53. & lonmax=-60.
	;latmin=a.y1-a.yd/2. & lonmin=a.x1-a.xd/2. & latmax=a.y2+a.yd/2. & lonmax=a.x2+a.xd/2.
	print, a.y1-a.yd/2.,a.x1-a.xd/2.,a.y2+a.yd/2.,a.x2+a.xd/2.
	limits = [latmin,lonmin,latmax,lonmax]
	
	r2 = FLTARR(nx,ny,nt,/nozero)
	if not file_test(savfile) then begin
		diurnal = diurnal*100. ; convert to %
		fftu = COMPLEXARR(nx,ny,nt,/nozero)
		pwr = FLTARR(nx,ny,nt,/nozero)
		s2 = FLTARR(nx,ny,nt,/nozero)
		index_of_max_diurnal = LONARR(nx,ny,/nozero)
		print, strjoin(replicate('_',80)) & pr = nx/80.
		for x = 0, nx-1 do begin
			for y = 0, ny-1 do fftu[x,y,*] = fft(diurnal[x,y,*])
			for y = 0, ny-1 do s2[x,y,*] = variance(diurnal[x,y,*])
			for y = 0, ny-1 do begin
				tmp = max(diurnal[x,y,*],i)
				index_of_max_diurnal[x,y] = i
			endfor
			if x mod pr lt 1 then print, '.', format='(a,$)'
		endfor
		print
	endif
	
	pos=[0.05,0.20,0.95,0.92]
	barpos = [0.25,0.04,0.75,0.05]
	barcharsize = 0.8
	
	
	; Convert from UTC to Local Solar Time
	dx = (lonmax-lonmin)/nx
	dy = (latmax-latmin)/ny
	lons = lonmin + dx/2 + findgen(nx)*dx 
	lats = latmin + dy/2 + findgen(ny)*dy 
	
	LSTimeShift = lons/15.
	
	LSTarray = diurnal
	for x = 0, nx-1 do LSTarray[x,*,*] = LSTimeShift[x]
	
	
	; compute power at each harmonic
	pwr = ABS(fftu)
	; compute % variance explained by each harmonic (R squared)
	for t = 0, nt-1 do r2[*,*,t] = 100*(nt/2)*(2*pwr[*,*,t])^2/(nt-1)/s2[*,*,t]
	nodata = where(s2 eq 0,nodata_count)
	if nodata_count ne 0 then r2[nodata] = 0
	r2[*,*,0]=0
	r2[*,*,nt/2]=r2[*,*,nt/2]/2
	weak = 0.04
	weak_indices = where(pwr[*,*,0] le weak)
	
	;old_font = !P.FONT & !P.FONT = 0 & print, 'old_font,!P.FONT = ',old_font,!P.FONT
	
	
	;*****************************************
	; x-y plots at different times of day
	;*****************************************
	
	levels = findgen(9)*16/9+1; used for 15dBZ Threshold
	if thresh eq '40' then levels = findgen(10)*4./9+0.1; used for 40dBZ Threshold
	colors = mycolors(levels)
	for record = 0, nt-1,3 do begin
		mymap,limits,pos, title=string((record-1+24) mod 24, format='(I2)')+':45-' + string(record,format='(I2)') + ':30 UTC', subtitle=subtitle
		contour, diurnal[*,*,record], lons, lats, c_colors=colors, levels=levels, /cell_fill, /overplot
		mymap, limits, pos, /noerase
		contourbar,levels,colors,position=barpos,title=field_desc,format='(F5.2)', charsize=barcharsize
	endfor
	
	
	
	
	;******************
	; ZEROTH HARMONIC
	;******************
	
	levels = findgen(9)*8/9+1 ; used for 15dBZ threshold
	if thresh eq '40' then levels = findgen(10)*1.2/9+0.04 ; used for 40dBZ threshold
	colors = mycolors(levels)
	mymap, limits, pos, title='Amp of 0th harmonic derived from hourly time series of ' + field_desc, subtitle=subtitle
	contour, pwr[*,*,0], lons, lats, c_colors=colors, levels=levels, /cell_fill,  /overplot
	mymap, limits, pos, /noerase
	;pos = [!X.WINDOW[0],!Y.WINDOW[0],!X.WINDOW[1],!Y.WINDOW[1]]
	print, pos
	contourbar,levels,colors,position=barpos,format='(F4.2)',title='%', charsize=barcharsize
	
	
	;******************
	; FIRST HARMONIC
	;******************
	
	levels = findgen(9)*8/9+1; used for 15dBZ threshold
	if thresh eq '40' then levels = findgen(9)*1.6/9+weak; used for 40dBZ threshold
	colors = mycolors(levels)
	mymap,limits,pos, title='Amp derived from 1st harmonic of '+field_desc, subtitle=subtitle
	contour,2*pwr[*,*,1], lons, lats, c_colors=colors, levels=levels, /cell_fill, /overplot
	mymap, limits, pos, /noerase
	contourbar,levels,colors,position=barpos,format='(F4.2)',title='%', charsize=barcharsize
	
	
	
	levels = findgen(10)*1.4/9+0.1
	colors = mycolors(levels)
	norm_amp = 2*pwr[*,*,1]/pwr[*,*,0]
	; filter points with low daily means
	norm_amp[weak_indices] = 0.
	mymap,limits,pos, title='Normalized amp derived from 1st harmonic of '+field_desc, subtitle=subtitle
	contour, norm_amp, lons, lats, c_colors=colors, levels=levels, /cell_fill,  /overplot
	mymap, limits, pos, /noerase
	contourbar,levels,colors,position=barpos,format='(F4.2)',title='set to zero where mean <= '+string(weak,format='(F4.2)')+'%', charsize=barcharsize
	
	
	levels = [40,50,60,70,80,90,95,97,99] ; used for 15dBZ threshold
	if thresh eq '40' then levels = [30,40,50,60,70,75,80,85,90] ; used for 40dBZ threshold
	colors = mycolors(levels)
	mymap,limits,pos, title='% variance explained by 1st harmonic (diurnal)', subtitle=subtitle
	contour, r2[*,*,1], lons, lats, c_colors=colors, /cell_fill, levels=levels, /overplot
	;contour, s2,/fill,/follow,/overplot
	contourbar,levels,colors,position=barpos,title="%", format='(I2)', charsize=barcharsize
	

	levels = indgen(12)*2
	colors = mycolors(levels)
	mymap,limits,pos, title='Time for peak of 1st harmonic of ' + field_desc, subtitle=subtitle
	imag = imaginary(fftu[*,*,1])
	real = float(fftu[*,*,1])
	u2 = -atan(imag,real)
	u2 = (24./2./!PI * u2) + 48 + LSTarray
	u2 = u2 mod 24
	if nodata_count ne 0 then u2[where(pwr[*,*,0] eq 0)] = !VALUES.F_NAN
	u2[weak_indices] = !VALUES.F_NAN
	contour, u2, lons, lats, c_colors=colors, /cell_fill, levels=levels, /overplot
	mymap, limits, pos, /noerase
 	contour, u2, lons, lats, levels=[12], /overplot, thick=1
	contourbar, levels, colors, position=barpos, format='(i2)', title='local solar time where mean <= '+string(weak,format='(F4.2)')+'%', charsize=barcharsize
	
	
	mymap,limits,pos, title='Time of maximum ' + field_desc, subtitle=subtitle
	u2 = 24 * index_of_max_diurnal/nt
	u2 = u2 + 48 + LSTarray
	u2 = u2 mod 24
	if nodata_count ne 0 then u2[where(pwr[*,*,0] eq 0)] = !VALUES.F_NAN
	u2[weak_indices] = !VALUES.F_NAN
	contour, u2, lons, lats, c_colors=colors, /cell_fill, levels=levels, /overplot
	contour, u2, lons, lats, levels=[12], /overplot, thick=1
	mymap, limits, pos, /noerase
	contourbar, levels, colors, position=barpos, format='(i2)', title='local solar time where mean <= '+string(weak,format='(F4.2)')+'%', charsize=barcharsize
	
	
	
	
	;******************
	; SECOND HARMONIC
	;******************

	mymap,limits,pos, title='Amp of 2nd harmonic (semi-diurnal) of ' + field_desc, subtitle=subtitle
	levels = [0.5,1,1.5,2,2.5,3,3.5,4,4.5]; used for 15dBZ threshold
	if thresh eq '40' then levels = findgen(9)*0.5/9+0.1 ; used for 40dBZ threshold
	colors = mycolors(levels)
	LOADCT, 41, file="/users/ahijevyc/IDL/resource/colors/colors1.tbl", /SILENT
	contour, 2*pwr[*,*,2], lons, lats, c_colors=colors, levels=levels, /cell_fill, /overplot
	contourbar,levels,colors,position=barpos,format='(F4.2)',title=field_desc, charsize=barcharsize

	
	mymap,limits,pos, title='Normalized amp derived from 2nd harmonic of ' + field_desc, subtitle=subtitle
	levels = findgen(9)*1.4/9+0.1
	colors = mycolors(levels)
	norm_amp = 2*pwr[*,*,2]/pwr[*,*,0]
	norm_amp[weak_indices] = 0.0 ; weak_indices should have been defined earlier.
	contour, norm_amp, lons, lats, c_colors=colors, levels=levels, /cell_fill, /overplot
	mymap, limits, pos, /noerase
	contourbar,levels,colors,position=barpos,format='(F4.2)',title='set to zero where mean <= '+strtrim(weak,2)+'%', charsize=barcharsize
	
	
	mymap,limits,pos, title='% variance explained by 2nd harmonic (semi-diurnal) of ' + field_desc, subtitle=subtitle
	levels = [40,50,60,70,80,90,95,97,99]; used for 15dBZ threshold
	if thresh eq '40' then levels = [30,40,50,60,70,75,80,85,90]; used for 40dBZ threshold
	colors = mycolors(levels)
	contour, r2[*,*,2], lons, lats, /cell_fill, c_colors=colors, levels=levels, /overplot
	;contour, s2,/fill,/follow,/overplot
	contourbar,levels,colors,position=barpos,title="%", format='(I2)', charsize=barcharsize
	
	levels = indgen(6)*2
	colors = mycolors(levels)
	mymap,limits,pos, title='Times for peaks of 2nd harmonic derived from time series of '+field_desc, subtitle=subtitle
	imag = imaginary(fftu[*,*,2])
	real = float(fftu[*,*,2])
	u2 = -atan(imag,real)
	u2 = (12./2./!PI * u2) + 24 + LSTarray
	u2 = u2 mod 12
	if nodata_count ne 0 then u2[where(pwr[*,*,0] eq 0)] = !VALUES.F_NAN
	contour, u2, lons, lats, c_colors=colors, levels=levels, /cell_fill, /overplot
	mymap, limits, pos, /noerase
	contourbar, levels, colors, position=barpos, format='(i2)', title='local solar time [plus 0 and 12 h]', charsize=barcharsize
	
	
	mymap,limits,pos, title='% Variance Explained by 1st & 2nd Harmonic', subtitle=subtitle
	levels = [40,50,60,70,80,90,95,97,99]; used for 15dBZ threshold
	if thresh eq '40' then levels = [30,40,50,60,70,75,80,85,90]; used for 40dBZ threshold
	colors = mycolors(levels)
	contour, r2[*,*,1]+r2[*,*,2], lons, lats, /cell_fill, c_colors=colors, levels=levels, /overplot
	mymap, limits, pos, /noerase
	;contour, s2,/fill,/follow,/overplot
	contourbar,levels,colors,position=barpos,title="%", format='(I2)', charsize=barcharsize
	
	
	
	
	;***************************
	; Example hourly time series 
	;***************************
	
	;0.69 0.28 central FL
	;0.35 0.59 central CO
	;0.41 0.60 western NE
	;0.43 0.65 central NE
	;0.50 0.22 TX Gulf waters
	;0.18 0.54 low mean in southwest Nevada
	
	xx = long(nx*[0.69,0.35,0.41,0.43,0.50,0.18])
	yy = long(ny*[0.28,0.59,0.60,0.65,0.22,0.54])
	
	for i=0,n_elements(xx)-1 do begin
		x=xx[i] & y=yy[i]
		u2 = reform(diurnal[x,y,*]) & u2 = [u2,u2]
		daily_mean = string(mean(u2),format='(F5.2)')
		MAP_SET, limit=limits, /usa, /continents, position=[0.07,0.65,0.39,0.90], /noborder
		plots, lons[x], lats[y], psym=2, color=18, thick=2
		plot, u2, ytitle='%', title="diurnal cycle of "+field_desc+" mean ="+daily_mean+'%', subtitle=strtrim(lons[x],2)+" E "+strtrim(lats[y],2)+" N grid coordinate ("+strtrim(x,2)+","+strtrim(y,2)+")", xstyle=1, xtitle='local solar time (h)', xtickname=strtrim(indgen(9)*3,2), xticks=8, xrange=[-LSTimeShift[x],-LSTimeShift[x]+24], /noerase, position=pos
		; set everything but the zero and first harmonic power to zero
		tmp = fftu[x,y,*] & tmp[2:nt-2] = 0
		oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=1
		; set everything but the zero and second harmonic power to zero
		tmp = fftu[x,y,*] & tmp[3:nt-3] = 0 & tmp[1] = 0 & tmp[nt-1]=0
		oplot, [reform(fft(tmp,/inverse)),reform(fft(tmp,/inverse))], linestyle=2
	endfor
	
	
	save, diurnal, filename, a, nx, ny, fftu, pwr, s2, index_of_max_diurnal, ithresh,  filename='diurnal.sav'
	;!P.FONT = old_font & print, '!P.FONT = ', !P.FONT
	
	if !D.NAME eq 'PS' then device, /close

end

function k2km, k, n
	; in the gifs, there are 8 - 10 km per pixel.
	return, 9.*n / k 
end

function f2km, f
	; in the gifs, there are 8 - 10 km per pixel.
	return, 2*9./ f 
end

function max3rd, p
	d = p[0] & c = p[1] & b = p[2] & a = p[3]
	; we divide by a later, so it can't be zero.
	if a eq 0 then return, 0


	; generic cubic polynomial: Ax^3 + Bx^2 + Cx + D
	; 1st derivative :          3Ax^2 + 2Bx + C
	; 2nd derivative :           6ax + 2B
	; Using quadratic formula
	;
	;       -B +- sqrt(B^2 - 4AC)
	;   x = ---------------------
	;                2A
	;
	; find where the 1st derivative is zero.
	; Since A = 3a, B = 2b, and C = c,
	;
	;       -2b +- sqrt(b^2 - 3ac)
	;   x = ----------------------
	;                3a
	;
	; Sanity check.  There may be no local maximum in this cubic curve
	if b^2. - 3.*a*c le 0. then return, 0
	q = sqrt(b^2. - 3.*a*c) ; root thing
	; minus root is one with b + 3*a*x < 0, the condition for a local maximum
	return, (-b - q)/(3.*a)
end


pro tracking, nc, jpeg=jpeg, track=track, png=png, debug=debug
	device, decomposed=0
	device, retain=2
	loadct, 41, file="/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl" ; radar reflectivity
	
	if N_ELEMENTS(nc) eq 0 then nc = 2
	if ~keyword_set(jpeg) then jpeg = 0
	if ~keyword_set(png) then png = 0
	if ~keyword_set(track) then track = 0
	if ~keyword_set(debug) then debug = 0

	fft = 0 & dwt = ~fft
	
	;xinteranimate,/close
	
	;files = [file_search('/sharp/ahijevyc/LDM_NOWrad/2004/national_20040708*.gif') ]
	files = [file_search('/pecan/ahijevyc/LDM_NOWrad/2005/national_20050531*.gif'), file_search('/pecan/ahijevyc/LDM_NOWrad/2005/national_20050601*.gif')]
	;files = [file_search('/sharp/ahijevyc/LDM_NOWrad/2005/national_2005051[23]*.gif')]
	nt = 600
	if n_elements(files) gt nt then begin
		print, 'cutting off # of files at ', nt
		files = files[0:nt-1]
	endif
	nt = n_elements(files)
	if nt eq 0 then message, 'no images found'
	
	
	fullnx=768
	fullny=576
	x1 = 270
	x2 = 525
	nx = x2-x1+1
	y1 = 187
	y2 = 442
	ny = y2-y1+1
	t1 = 0
	t2 = nt-1
	
	
	dlon = 70./768
	y0 = 128 ; blank buffer width in pixels put on bottom part of gif from GEMPAK
	dlat = 33./(576-y0-86) ; 86 is top buffer
	limit_deg = [20+dlat*(y1-y0),-130+dlon*x1,20+dlat*(y2-y0),-130+dlon*x2]
	
	
	
	
	umax = 8 & vmax = 8
	cc = fltarr(2*umax+1,2*vmax+1)
	
	; Create an distance mask.
	; When shifted by half-a-side length and width it looks like an oval.
	sr = float(min([nx,ny])) ; must be float for division much later in the program.
	k2d = dist(sr,sr)
	s  = congrid(k2d,nx,ny,/cubic)
	s  = s/max(s) ; scale (normalize) s so (0 <= s <= 1)
	coeffs = 4 ; used in discrete wavelet transform WTN() (4, 12, or 20)
	
	tskip = 1
	nframes = nt/tskip
	fullimage3D = intarr(fullnx,fullny,nframes)
	x = findgen(nx)#replicate(1,ny) ; needed for x[p] below
	y = transpose(findgen(ny)#replicate(1,nx)) ; needed for y[p] below
	
	
	
	
	xinteranimate, set=[3*nx,3*ny,nframes],/showload
	; first, convert WSI NOWrad composite to gif with GEMPAK (MASTER152gif)
	
	for frame = 0, nframes-1 do begin
;		read_gif, files[t1+(frame*tskip)], newimage
;		fullimage3D[*,*,frame] = newimage

		scale = (frame+5.)/1.5
		fullimage3D[*,*,frame] =  congrid(70.*exp(-(((x-128.)/scale)^2.+((y-160.)/scale)^2.+((frame-64.)/scale/2.5)^2.)),768,576)
	endfor
	
	
	; After ingesting all nframe images smooth them in time and space.
	; Scale to dBZ range and take a spatial subset.
	dBZ = bytscl(fullimage3D[x1:x2,y1:y2,*],top=75)
	pad = 16
	dBZ[0:pad-1,*,*] = 0
	dBZ[nx-pad:nx-1,*,*] = 0
	dBZ[*,0:pad-1,*] = 0
	dBZ[*,ny-pad:ny-1,*] = 0
	; ; dBZ[*,*,0:pad-1] = 0
	; dBZ[*,*,nt-pad:nt-1] = 0
	Z = 10.^(dBZ/10.)
	
	; Spatial smoothing
	;dBZ = smooth(dBZ,[3,3,3])
	;dBZ[where(dBZ lt 15)] = 0
	
	; Average in time.
	; rebin crushes time dimension to a length of one.
	;img = rebin(padImg,nx,ny)
	
	
	
	
	; Morphological smoothing in space in time. (similar to MORPH_CLOSE)
	; str : 3-D structuring element (as refered to in IDL help on DILATE function)
	; str_d : spatial width of one side of the structuring element box in grid points
	; 	set str_d=1 for no spatial smoothing
	; time_smooth : temporal width of structuring element in time steps
	; 
	; set to all zeros and except for a round area in the center, which is ones.
	; element will be spherical in time-space.
	; First dilate, then erode, then dilate again.
	
	; As of 2007-03-30 I like a time_smooth dilation of 3 units, a erosion of 5, and a final dilation of 2 units.
	
	str_d = 1. & str_d2 = str_d/2 & time_smooth = 1
	padImg = replicate(0B, nx, ny, nframes+time_smooth)
	padImg [0,0,time_smooth/2] = dBZ
	str = replicate(0,[str_d,str_d,time_smooth]) ; set to all zeroes
	for it = 0, time_smooth-1 do str[*,*,it] = str_d gt 1 ? shift(dist(str_d),str_d2,str_d2) le str_d2/(1.+abs(it-(time_smooth-1)/2.)) : 1
	padImg = dilate(padImg,str,/gray) & print, 'dilation kernel=',str
	
	str_d = 1. & str_d2 = str_d/2 & time_smooth = 1
	str = replicate(0,[str_d,str_d,time_smooth])
	for it = 0, time_smooth-1 do str[*,*,it] = str_d gt 1 ? shift(dist(str_d),str_d2,str_d2) le str_d2/(1.+abs(it-(time_smooth-1)/2.)) : 1
	padImg = erode(padImg,str,/gray) & print, 'erosion kernel =',str
	
	str_d = 1. & str_d2 = str_d/2 & time_smooth = 1
	str = replicate(0,[str_d,str_d,time_smooth])
	for it = 0, time_smooth-1 do str[*,*,it] = str_d gt 1 ? shift(dist(str_d),str_d2,str_d2) le str_d2/(1.+abs(it-(time_smooth-1)/2.)) : 1
	padImg = dilate(padImg,str,/gray) & print, 'dilation kernel=',str
	
	
	img3D = padImg[*,*,time_smooth/2:time_smooth/2+nframes-1]


	
	padImg = replicate(0., 256,256,256)
	padImg[*,*,63:63+191] = img3D


	; Do transformation on 3-D array. WTN seems to break with 256x256x128 array.
	if fft then fft_img3D = fft(img3D) else begin
		coiflet = wv_fn_coiflet(3, scaling, wavelet, ioff, joff)
		fft_img3D = wv_dwt(padImg, scaling, wavelet, ioff, joff)
	endelse

	nt_tmp = (size(fft_img3D))[3]
	dist3D = fltarr(nx,ny,nt_tmp)
	for it=0, nt_tmp-1 do begin
		dt = nt/2 - abs(nt/2 - it)
		dist3D[*,*,it] = sqrt(dist(nx,ny)^2. + 1.*dt^2.)
	endfor
	fft_img3D[where(dist3D gt 64 or dist3D lt 0) ] = 0.
	if fft then img3D = fft(fft_img3D, /inverse) else begin
		img3D = wv_dwt(fft_img3D, scaling, wavelet, ioff, joff, /inverse)
	endelse
	if ~fft then img3D = img3D[*,*,63:63+191]
	
	
	
	
	
	
	
	
	for frame = 0, nframes-1 do begin
	
		erase
	
	
		img = img3D[*,*,frame]
	
	
		raw = dBZ[*,*,frame]
		vv = 2
		sinn = cos(vv * findgen(nx)/nx*!PI*2) # replicate(1.,ny) 
	;	sinn = sinn + replicate(1,nx) # sin(vv * findgen(ny)/ny*!PI*2)
	;	sinn = shift(sinn,22,8)
		as = sinn
		deg_step = 14
	;	for xc = deg_step,359,deg_step do as = as + rot(sinn,xc,1,3,7)
	; 	raw = as
	; 	Uncomment to ignore time and space-morphing (erode and dilate) done earlier.
	; 	img = raw
		tv, 3*raw, 0
		map_set, 0, -105, limit=limit_deg, pos=[0./3,2./3,1./3,1], /usa, /cylind, /noerase
	
		if total(img) eq 0 then continue
		tv, 3*img, 1
		map_set, 0, -105, limit=limit_deg, pos=[1./3,2./3,2./3,1], /usa, /cylind, /noerase
	
	
	
		fft_img = fft ? fft(img) : wtn(img,coeffs)
	
	
		; Apply Hanning filter to freqency spectrum. It suppresses high wavenumbers in a smooth way..
; 	  	fft_img = fft_img*shift(hanning(nx,ny,alpha=0.54),nx/2,ny/2) ; 0.54 = Hamming filter
	; 	tvscl, fft(fft_img,/inverse), 1
	; 	map_set, 0, -105, limit=limit_deg, pos=[1./3,2./3,2./3,3./3], /usa, /cylind, /noerase
	; 
	; 
	; 	; low wavenumber cut-off (normalized between zero and one)
		fkeep = 0.00
	; 	; high wavenumber cut-off (normalized)
		fk2 = 1.
		fk3 = 2 ; only keep this wavenumber and below
	; 	fft_img[*,fk3:ny-fk3-1] = 0
	; 	fft_img[fk3:nx-fk3-1,*] = 0
	; 	fft_img[fk3:*,fk3:*] = 0
		; Keep power in spatial scales between fkeep and fk2.
	; 	fft_img = fft_img * (s ge fkeep and s le fk2)
		; Set 0th order component to 0. Its power can overwhelm other components
		; and its absence doesn't affect shape of reconstructed image.
	; 	fft_img[0] = 0
		; Remove components of discrete wavelet transform below a threshold.
	;  	if dwt then fft_img = fulstr(sprsin(fft_img,thres=0.0))
	
		; Have fun with ideal pictures in frequency space.
	;	fft_img[1:*,1:*]=0
	;	fft_img[0,3:ny-3] = 0
	;	fft_img[3:nx-3,0] = 0
	;	fft_img[0,0:1] = 0
	;	fft_img[0:1,0] = 0
	;	fft_img = float(s ge frame/200. and s lt (frame+0.8)/200.) 
	; 
	; 
	; 	; Flip back from wavenumber space to distance space
		img = fft ? fft(fft_img, /inverse) : wtn(fft_img, coeffs, /inverse)
		img = float(img) ; necessary or the thresholding doesn't always work below
	
		tvscl, img, 2
		map_set, 0, -105, limit=limit_deg, pos=[2./3,2./3,3./3,3./3], /usa, /cylind, /noerase
		echo_pixels = where(Z[*,*,frame] gt 1, n)
		echo_percentage = float(n)/nx/ny
		; make sure the filtered field has the same amount of coverage as the original echo field.
		cutoff = (img[sort(img)])[(n_elements(img)-1)*(1.-echo_percentage)]
		; use le, not lt to pick out wave_noise. When there are no echoes, using lt will result in one pixel being retained in the filtered image, not zero, as it should be.
		wave_noise = where(img le cutoff, n)
		if n gt 0 then begin
			img = img-min(img) ; minimum will be shifted to zero
			img[wave_noise] = 0. ; wave noise is zero (needed for finding blobs below)
		endif
		tvscl, img, 3
		map_set, 0, -105, limit=limit_deg, pos=[0./3,1./3,1./3,2./3], /usa, /cylind, /noerase
		xyouts, 0.5, 0.99, files[t1+(frame*tskip)],align = 0.5, /norm
	
	
		; plot filtered power spectrum (shifted by half-a-side length)
		; initialize logpower array to negative infinity.
		logpower = fltarr(nx,ny) & logpower[*] = -!VALUES.F_Infinity
		igood = where(abs(fft_img) gt 0,ngood)
		if ngood eq 0 then continue
		logpower[igood] = alog10(abs(fft_img[igood]))
		fin_logpower = logpower[igood]
		if n_elements(minpw) eq 0 then minpw = (fin_logpower[sort(fin_logpower)])[(n_elements(fin_logpower)-1)*0.10]
		if n_elements(maxpw) eq 0 then maxpw = max(fin_logpower)
		if minpw eq maxpw then minpw = -!VALUES.F_Infinity
		logpower = bytscl(logpower,min=minpw,max=maxpw)
		shft_power = logpower
		if fft then shft_power = shift(logpower,nx/2-1,ny/2-1)
		tv, shft_power, 6, /nan
		bzoom = 0.1
		zoomed = fft ? shft_power[(0.5-bzoom)*nx:1+(0.5+bzoom)*nx,(0.5-bzoom)*ny:1+(0.5+bzoom)*ny] : shft_power[0:bzoom*nx,0:bzoom*ny]
		tv, congrid(zoomed,nx,ny), 7, /nan
	
		; Plot power vs. wavelength
		sr2 = sr/2
		pw = findgen(sr2)
		k = findgen(sr2)
		for isr=0,sr2-1 do pw[isr] = total(abs(fft_img[where(s gt isr/sr2 and s le (isr+1)/sr2)])^2.)
		for isr=0,sr2-1 do k[isr] = mean(k2d[where(s gt isr/sr2 and s le (isr+1)/sr2)])
		if n_elements(xrange) eq 0 then xrange=[f2km(max([1./sr2,fkeep])),0.1*f2km(max([1./sr2,fkeep]))]
	; 	if n_elements(yrange) eq 0 then yrange=[10.^minpw,500.*max(pw)]
		if n_elements(yrange) eq 0 then yrange=[0.002*max(pw),50.*max(pw)]*10000.
		pos_power = [1./3+0.07,1./3+0.04,2./3-0.01,2./3-0.01]
		plot, k2km(k,sr), pw, pos=pos_power, /noerase, xrange=xrange, xtitle='km', xstyle=1, /ylog, yrange=yrange, ytitle='power',ystyle=1
		plot, k2km(k,sr), pw, pos=pos_power, /noerase, xrange=xrange/10, xstyle=1, xtickformat='("!C",I3)', /ylog, yrange=yrange, ystyle=5, color=240
	
	
		if 0 then begin
			; Skip to next frame now.
			xinteranimate, frame = frame, image = tvrd(true=2)
			continue
		endif
	
		; Get matrix of cross-correlation coefficients between past and present images at different spatial shifts.
	
		us = indgen(2*umax+1)-umax
		vs = indgen(2*vmax+1)-vmax

		ir_array = [1]
		for ir=0,n_elements(ir_array)-1 do begin
			r = ir_array[ir]
			;Create a binary disc of given radius (taken from ir_array).
			disk = SHIFT(DIST(2*r+1), r, r) LE r
			mcls = ir_array[ir] gt 1 ? morph_close(img,disk) : img
	; 		tvscl, mcls, ir+6
		
			blobs = label_region(mcls)
			h = histogram(blobs,reverse_indices=r)
			xcent=fltarr(n_elements(h))
			ycent=fltarr(n_elements(h))
			labls=strarr(n_elements(h))
			xw   =fltarr(n_elements(h))
			yw   =fltarr(n_elements(h))
	
			; Variable used to keep track of blobs that are actually plotted (i.e. not filtered out because they are too small or too weak).
			plotted_blob = 0
			; Picking a yrange is tricky. It is desireable to have it remain the same for each frame, but the lowest plotted value changes with the "cutoff" value, and the highest plotted value changes too.
			; img3D is changed down below, so don't expect it to remain static. So don't allow blobyrange to be redefined based on the maximum or minimum element in img3D. After defining it once, don't do it again.
			if n_elements(blobyrange) eq 0 then blobyrange=[15, max(img3D)-min(img3D)]
			; A region index of zero indicates that original pixel was zero and belongs to no region. So start at blob one (1).
			for i = 1, n_elements(h) -1 do begin
				p = r[r[i]:r[i+1]-1]
				; Erase small blobs.
				; Upon further review, why do this? After thresholding the FFT-filtered field by an objective amount (making the total area of the blobs equal the total area of original echo) just keep all the small blobs in there. You don't want to break up continuous streaks if they almost, but not quite completely, dissipate.
	
				; If blob is less than 50 points then erase it.
	; 			if n_elements(p) lt 50 then begin
	; 				blobs[p] = 0
	; 				continue
	; 			endif
				; If the sum of reflectivities is low, erase it.
	; 			if total((Z[*,*,frame])[p]) lt 2E5 then begin
	; 				blobs[p] = 0
	; 				continue
	; 			endif
	;			; If convective echo fraction (> 40 dBZ) is small, erase it.
	;			ihgh = where(img[p] gt 40, nhgh)
	;			if nhgh gt 0 then print, 'blob',i,' nhgh=', nhgh, 100.*nhgh/n_elements(p), '%'
	;			if float(nhgh)/n_elements(p) lt 0.005 then begin
	;				blobs[p] = 0
	;				continue
	;			endif
				labls[i] = strtrim(i,2)
				xcent[i] = total(10.^(img[p]/10.)*x[p])/total(10.^(img[p]/10.))
				ycent[i] = total(10.^(img[p]/10.)*y[p])/total(10.^(img[p]/10.))
	; 			print, 'blob', i, xcent[i], ycent[i]
				blob_color = (i+1.)/n_elements(h)*255
				blobs[p] = blob_color
	
	
				; Make a little dot (square) of zeroes at centroid of blob.
	; 			blobs[max([0,xcent[i]-1]):min([xcent[i]+1,nx-1]), max([0,ycent[i]-1]):min([ycent[i]+1,ny-1])] = 0
	
				dist = sqrt((x[p]-xcent[i])^2 + (y[p]-ycent[i])^2)
	
				; Plot distance from centroid vs image pixel value
				if plotted_blob eq 0 then plot, dist, img[p], psym=3, yrange=blobyrange, ystyle=1, ytitle='intensity', xrange=[0,60], xtitle='dist', position=[2./3+0.05,0.05,1-0.01,1./3-0.01], /nodata, /noerase
				oplot, dist, img[p], psym=3, color=blob_color
				xyouts, 35, !Y.CRANGE[1]-(!Y.CRANGE[1]-!Y.CRANGE[0])*(1+plotted_blob)*0.045, string(i,xcent[i],ycent[i], format='(I3,2I4)'), color=blob_color
				plotted_blob = plotted_blob + 1
				hist=histogram(dist,locations=xval,min=0,max=100,binsize=1)
	;			oplot, xval, hist*0.5, psym=10, color=blob_color
				pd = 5
				if n_elements(dist) ge pd then begin
					pf = poly_fit(dist, img[p], pd, chisq=chisq, sigma=sigma, status=status)
					if status ne 0 then print, 'poly_fit '+ string(status)
					if status eq 0 then begin
						d = findgen(100)
						fp = pf[0]
						for deg = 1,pd do fp = fp + pf[deg]*d^deg
						fp[where(hist eq 0)] = !VALUES.F_NAN
						oplot, d, fp, color=blob_color, thick=2
					endif
				endif
	
				if track && frame gt 0 then begin
					; used to multiply by 1.0 to ensure float, but got wierd behavior when applied to array subscripts below.
					; define the search window half-width, srch_wdw
					srch_wdw = long(max([dist,5]))
					if xcent[i]+x1 lt srch_wdw+umax or xcent[i]+x1 gt fullnx-1-srch_wdw-umax then begin
						print, STRING(i, xcent[i], ycent[i], FORMAT='("blob",I4," too close to left or right to track (",I3,",",I3,")")')
						continue
					endif
					if ycent[i]+y1 lt srch_wdw+vmax or ycent[i]+y1 gt fullny-1-srch_wdw-vmax then begin
						print, STRING(i, xcent[i], ycent[i], FORMAT='("blob",I4," too close to top or bottom to track (",I3,",",I3,")")')
						continue
					endif
					tmpimg = fullimage3D[xcent[i]-srch_wdw+x1:xcent[i]+srch_wdw+x1, ycent[i]-srch_wdw+y1:ycent[i]+srch_wdw+y1,frame]
					cc[*]=0.0
					print, indgen(2*umax+1)-umax, format = '(6X,99I6)'
					for v = vmax,-vmax,-1 do begin
						for u = -umax,umax do begin
							old_frame = max([0,frame-tskip]) ; old_frame must be zero or greater
							oldimage = fullimage3D[xcent[i]-srch_wdw-u+x1:xcent[i]+srch_wdw-u+x1, ycent[i]-srch_wdw-v+y1:ycent[i]+srch_wdw-v+y1,old_frame]
							if n_elements(oldimage) lt 2 then message, "oldimage is too small"
							if n_elements(oldimage) ne n_elements(tmpimg) then message, "oldimage, tmpimg differ"
							; If one of arrays is all zeroes, you can't track this blob.
							if total(tmpimg^2) eq 0 then continue
							if total(oldimage^2) eq 0 then continue
							cc[u+umax,v+vmax] = c_correlate(oldimage[*],tmpimg[*],0)
							if finite(cc[u+umax,v+vmax]) eq 0 then stop
						endfor
						print, v, cc[*,v+vmax], format='(99F6.3)'
					endfor
					;cc = smooth(cc, 3, /edge_truncate)
					print, max(cc,icc),format='("max cc",F6.3," for ",$)'
					iuv = array_indices(cc,icc)
					up = poly_fit(us,cc[*,iuv[1]],3)
					vp = poly_fit(vs,cc[iuv[0],*],3)
					; poly_fit returns vector of coefficients of length Degree+1.
					; For 2nd deg polynomial c + b*x + a^2*x, c is 1st element, b is 2nd element, and a is 3rd element of returned vector.
					; To find maxima of polynomial, set 1st derivative to zero, i.e. x = -b/(2a), and make sure 2nd derivative is negative.
					; 3rd deg gets more complicated. Use a function.
					xw[i] = max3rd(up)
					yw[i] = max3rd(vp)
					xws = STRTRIM(STRING(xw[i], FORMAT='(F8.1)'), 1)
					yws = STRTRIM(STRING(yw[i], FORMAT='(F8.1)'), 1)
					if debug gt 0 then labls[i] = labls[i] + "(" + xws + "," + yws + ")" + STRING(max(cc),FORMAT='("!C",F4.2)')
					print, labls[i]
					print
					; tried gauss2dfit on the cc array, but it is not as robust--often does not converge to a solution.
					; result = gauss2dfit(cc,a)
					; print, STRING(a[4]-umax, a[5]-vmax, format='("or is it",F4.1,",",F4.1,"?")')

				endif ; end blob tracking part
	
	
			endfor
	
			img3D[*,*,frame] = blobs gt 0
			tv, blobs, ir+5
			xyouts, xcent+(ir+2)*nx, ycent+ny, labls, align=0.5, /device, color=241
			alen=8.
			arrow, xcent+(ir+2)*nx, ycent+ny, xcent+(ir+2)*nx+xw*alen, ycent+ny+yw*alen, color=241, HSIZE=!D.X_SIZE/64*max(cc)
		endfor
	
		; http://fermi.jhuapl.edu/s1r/idl/s1rlib/idl_color/ explains nicely all the reasons for decomposed = 0, true=, and saving as jpeg instead of gif. -- 20070120
		xinteranimate, frame = frame, image = tvrd(true=1)
		if jpeg then write_image, '/mmmtmp/ahijevyc/frame'+string(frame,format='(i3.3)')+'.jpg', 'JPEG', tvrd(true=1)
		if png then write_png, '/mmmtmp/ahijevyc/frame'+string(frame,format='(i3.3)')+'.png', tvrd(true=1)
	endfor	
	
	xinteranimate
	
	save, dBZ, img3d, filename='dbz.sav'
	

end

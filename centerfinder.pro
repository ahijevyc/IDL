;Program to automatically find blobs.  Array for centres must be
;bigger than the number of blobs you think you're going to find.
;Also, vary minimum intensity to search for to get different number of
;blobs.  Lower number finds more blobs (picks up the faint ones).
;Output array 'centres' contains x and y co-ordinates of point of
;ABS(maximum) intensity and the intensity, i.e. is a 3 column fltarr.

PRO centerfinder, centrearray, framearray
	
	device, decomposed=0
	data = framearray
	help, data
	
	WINDOW, 0, RETAIN=2, XSIZE=(size(data))[1], YSIZE=(size(data))[2]
	TVSCL, data
	print, ' '
	print, 'Maximum and minimum values of the data (helps decide threshold 	intensities)'
	pmm, data
	
	result = WHERE(data gt 6.0) ;Here specify minimum intensity to search for.
	indexy = result / (size(data))[1]
	indexx = result - (indexy*(size(data))[1])
	s = size(indexx)
	indices = fltarr(3,s(1))
	indices[0,*] = indexx
	indices[1,*] = indexy
	FOR i=0,s(1)-1 DO BEGIN
		indices[2,i] = data[indices[0,i],indices[1,i]]
	ENDFOR;
	sind = size(indices)
	
	;-----------Find where the y breaks are-----------
	breaks = fltarr(200)
	numbreaks = 0
	FOR j=1,sind(2)-1 DO BEGIN
		IF indices[1,j] gt indices[1,j-1]+1 THEN BEGIN
			breaks[numbreaks] = j
			numbreaks = numbreaks+1
		ENDIF;
	ENDFOR;
	
	ww = WHERE(breaks eq 0)
	breaks[ww(0)] = sind(2)
	breaks = breaks[0:ww(0)]  ;removes excess 0's from breaks
	sbr = size(breaks)
	
	newindices = fltarr(3,sind(2)+sbr(1))
	snewind = size(newindices)
	
	;----------Seperate blobs by y coord--------------
	increment = 0
	FOR k=0,snewind(2)-1 DO BEGIN
		IF increment le sbr(1)-1 THEN BEGIN
			IF k ne breaks(increment) THEN BEGIN
					newindices[*,k+increment] = indices[*,k]
			ENDIF ELSE BEGIN
					newindices[*,k+increment] = 'NaN'
					increment = increment+1
					IF k le sind(2)-1 THEN newindices[*,k+increment] = indices[*,k]
			ENDELSE;
		ENDIF;
	ENDFOR;
	
	;----Find where NaNs are so we know where to sort by x------
	wnan0 = WHERE(FINITE(newindices[0,*]) eq 0)   ;To find rows where NaNs are
	snan = size(wnan0)
	wnan = intarr(snan(1)+1)
	wnan[1:snan(1)] = wnan0
	wnansize = size(wnan)
	
	centres = fltarr(3,wnansize(1)*2)  ;create array to put centres in later
	;centres = fltarr(3,200)
	centres = fltarr(3,(size(centrearray))[2])
	centres[*,*] = 'NaN'
	blobnumber = 0
	
	;Data sorted by x co-ordinate section:
	FOR sortloop=0,wnansize(1)-2 DO BEGIN
		;Loop to create filename (section of newindices to be sorted by x)
	
		;Sort section by x co-ord and put in its own array
		filename = 'blob' +string(FORMAT='(I0.5)',sortloop)
		filename = fltarr(3,wnan(sortloop+1)-wnan(sortloop)+1)
		
		sorted = SORT(newindices[0,wnan(sortloop):wnan(sortloop+1)])
		loopsize = wnan(sortloop+1)-wnan(sortloop)
		FOR l=0,loopsize DO BEGIN
			filename[*,l] = newindices[*,sorted(l)+wnan(sortloop)]
		ENDFOR;
	
		sblob = size(filename)
	
		;Now divide up blobs by looking for jumps in x co-ord
		breaksx = fltarr(200) ;Needs to be as large as number of blobs
		numbreaksx = 0
		FOR m=1,sblob(2)-1 DO BEGIN
			IF filename[0,m] gt filename[0,m-1]+1 THEN BEGIN
					breaksx[numbreaksx] = m
					numbreaksx = numbreaksx+1
			ENDIF;
		ENDFOR;
	
		ww1 = WHERE(breaksx eq 0) ;remove excess zeros from breaks
		breaksx[ww1(0)] = sblob(2)
		breaksx = breaksx[0:ww1(0)]
		sbx = size(breaksx)
		
		splitfilename = fltarr(3,1)
		splitfilename[*,*]='NaN'
		;Actually seperate blobs by x co-ords (each into own array)
		FOR splits=0,sbx(1)-1 DO BEGIN
			;Loop to actually seperate filename by x coord and intensity (if needed) into spltfilename2  
			
			splitfilename2 = 'blob' +string(FORMAT='(I0.5)', splits) +'split'
			IF splits eq 0 THEN BEGIN
					splitfilename2 = fltarr(3,breaksx(splits))
					increment=0
			ENDIF ELSE BEGIN
					splitfilename2 = fltarr(3,breaksx(splits)-breaksx(splits-1))
					increment=breaksx(splits-1)
			ENDELSE;
			split2size = size(splitfilename2)
	
			IF (size(splitfilename2))[0] eq 2 THEN BEGIN
									
					FOR n=0,split2size(2)-1 DO BEGIN
						splitfilename2[*,n] = filename[*,n+increment]
					ENDFOR              ;
					
					;Now split each of these (splitfilename2) further, by intensity (+ve,-ve) (if necessary)
					whnan = WHERE(FINITE(splitfilename2[2,*]) eq 0) 
					whpos = WHERE(splitfilename2[2,*] ge 0)
					IF (size(whnan))[0] ne 0 THEN BEGIN
						numelements = (size(splitfilename2))[2] - (size(whnan))[1]
					ENDIF ELSE BEGIN
						numelements = (size(splitfilename2))[2]
					ENDELSE             ;   
					
					splitagain=0        ;Trigger for if we need to split by intensities.
					IF (size(whpos))[0] ne 0 AND (size(whpos))[1] ne numelements THEN splitagain=1
					splitagainnum = 0
					IF splitagain eq 1 THEN BEGIN
						;This is bit where we'll sort by intensities to split +ve and -ve
						
						;First, sort by intensities
						sorted2 = SORT(splitfilename2[2,*])
						splitfilename3 = 'blob' +string(FORMAT='(I0.5)',splits)+'splitagain' +string(FORMAT='(I0.5)',splitagainnum)
						splitfilename3 = fltarr(3,(size(splitfilename2))[2])
						FOR y=0,(size(splitfilename2))[2]-1 DO BEGIN
							splitfilename3[*,y] = splitfilename2[*,sorted2(y)]
						ENDFOR          ;
						
						intsplit = fltarr(2)
						FOR z=1,(size(splitfilename3))[2]-1 DO BEGIN
							IF splitfilename3[2,z] lt 0 AND splitfilename3[2,z-1] ge 0 THEN intsplit=z
							IF splitfilename3[2,z] ge 0 AND splitfilename3[2,z-1] lt 0 THEN intsplit=z
						ENDFOR          ;
						
						increment2= 0 
						finalsplit = fltarr(3,(size(splitfilename3))[2]+2)  
						FOR final=0,(size(finalsplit))[2]-2 DO BEGIN
							IF final ne intsplit THEN BEGIN
									finalsplit[*,final] = splitfilename3[*,final-increment2]
							ENDIF ELSE BEGIN
									finalsplit[*,final] = 'NaN'
									increment2 = 1
							ENDELSE     ;
						ENDFOR          ;
						finalsplit[*,(size(splitfilename3))[2]+1] = 'NaN'
						splitfilename2 = finalsplit          
						splitagainnum = splitagainnum+1
					ENDIF               ;
			
					IF FINITE(splitfilename2[2,(size(splitfilename2))[2]-1]) ne 0 THEN BEGIN 
						rowofnans = fltarr(3,1)
						rowofnans[*,*] = 'NaN'
						splitfilename2 = [[splitfilename2],[rowofnans]]
					ENDIF               ;
					
			splitfilename = [[splitfilename],[splitfilename2]]
					
			ENDIF                   ;
			
			
		ENDFOR                      ;
	
		IF FINITE(splitfilename[0,0]) eq 0 THEN splitfilename = splitfilename[*,1:(size(splitfilename))[2]-1]
	
		;Now find where max intensity of blobs is and put into centres
		wnan2 = WHERE(FINITE(splitfilename[0,*]) eq 0)  ;find where nans are in spitfilename
		trigger = 0
	
		FOR loop=1,(size(wnan2))[1]-1 DO BEGIN
			IF wnan2[loop] gt wnan2[loop-1]+1 THEN trigger=loop ;find where consecutive nans are
		ENDFOR;
		wnan2 = wnan2[0:trigger]    ;removes consecutive NaNs (allows it to ignore consecutive nans at end of splitfilename)
	
		FOR intloop=0,(size(wnan2))[1]-1 DO BEGIN ;0 to number of blobs
			IF intloop ne 0 THEN BEGIN
					maxint = MAX(ABS(splitfilename(2,wnan2(intloop-1):wnan2(intloop))),location, /NAN)
					centres[*,blobnumber] = splitfilename(*,location+wnan2(intloop-1))
					blobnumber = blobnumber+1
			ENDIF ELSE BEGIN
					maxint = MAX(ABS(splitfilename(2,0:wnan2(0))), location, /NAN)
					centres[*,blobnumber] = splitfilename(*,location)
					blobnumber = blobnumber+1
			ENDELSE;
		ENDFOR;
	
	ENDFOR;
			
	trim = WHERE(FINITE(centres[0,*]) eq 0)   ;remove excess rows from centres array
	centres = centres[*,0:trim(0)-1]
	centrearray=centres
	
	;Plot data with centres marked.
	TVSCL, data
	PLOTS, centres[0:1,*], THICK=0, COLOR=0, PSYM=1, /DEVICE
	
	
	RETURN;
	

END;

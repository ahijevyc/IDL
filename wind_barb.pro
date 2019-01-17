;+
; NAME:
;	wind_barb
;
; PURPOSE:
;	plot a wind barb as used in surface analysis maps
;	or in vertical soundings
;	For an explanation of wind barbs see
;       <A HREF="http://ww2010.atmos.uiuc.edu/(Gh)/guides/maps/sfcobs/wnd.rxml">Wind barb</A>
;	There are 4 different types of wind barbs:
;	1) calm (0-5 knots)	; no barb at all
;	2) 5 knots		; a short barb
;	3) 10 knots		; a long barb
;	4) 50 knots		; a "pennant" (filled triangle)
;	1 Knot = 1.152 Miles Per Hour (MPH) 
;	1 Knot = 1.853 Kilometers Per Hour (KM/H)
;	1 Knot = 0.515 m/sec 
;	Each short barb represents 5 knots, each long barb 10 knots.
;	Pennants (triangles) are 50 knots.
;	Example:
;	A long barb and a short barb is 15 knots, simply by adding the value of 
;	each barb together (10 knots + 5 knots = 15 knots). If only a station 
;	circle is plotted, the winds are calm. 
;   
; CATEGORY:
;
; CALLING SEQUENCE:
;	wind_barb,wspeed,wangle,xpos,ypos,size=size,thick=thick,$
;                 normal=normal,nocircle=nocircle,_extra=e
;
; EXAMPLE:
;
; INPUTS:
;	wspeed	: wind speed in m/s
;	wangle	: wind angle in degrees, 0=wind from north, clockwise
;	xpos	: x-origin of wind barb symbol in data coordinates
;                 or (with keyword /normal) in normal coos (0-1)
;	ypos	: y-origin of wind barb symbol in data coordinates
;                 or (with keyword /normal) in normal coos (0-1)
;
; OPTIONAL INPUT PARAMETERS:
;	size	: symbol size in data coordinates or
;                 (with keyword /normal) in terms of character height
;                 Default is 1 without /normal or 2. with /normal.
;	thick	: thickness of symbol lines (default is 1.)
;
; KEYWORD INPUT PARAMETERS:
;	nocircle: Don't plot a filled circle at symbol base
;       normal  : Set this keyword if xpos and ypos are provided in
;                 normal coordinates. In this case, xpos and ypos
;                 are converted to device coordinates and all plotting
;                 is done in device coordinates (default is data coordinates)
;       Note: all additional keywords are directly passed through to
;       the plots routine
;
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;	
; MODIFICATION HISTORY:
;
; Originator:
; 	Dominik Brunner, Feb 2000 (brunner@atmos.umnw.ethz.ch)
;-
PRO wind_barb,wspeed,wangle,xpos,ypos,size=size,thick=thick,nocircle=nocircle,$
              normal=normal,_extra=e

; check for input
IF n_elements(wspeed) NE 1 THEN BEGIN
   print,'Parameter wspeed must have one element'
   return
ENDIF
IF n_elements(wangle) NE 1 THEN BEGIN
   print,'Parameter wangle must have one element'
   return
ENDIF
IF n_elements(xpos) NE 1 THEN BEGIN
   print,'Parameter xpos must have one element'
   return
ENDIF
IF n_elements(ypos) NE 1 THEN BEGIN
   print,'Parameter ypos must have one element'
   return
ENDIF
IF n_elements(thick) EQ 0 THEN thick=1.	; default size is two characters

IF keyword_set(normal) THEN BEGIN
   IF n_elements(size) NE 1 THEN size=2.
   IF (xpos LT 0) OR (xpos GT 1) THEN BEGIN
      print,'xpos out of normal coordinates range 0-1'
      return
   ENDIF
   IF (ypos LT 0) OR (ypos GT 1) THEN BEGIN
      print,'ypos out of normal coordinates range 0-1'
      return
   ENDIF
   device=1 ; all plotting is done in device coordinates
   scale=size*!d.Y_CH_SIZE ; symbol size in device coordinates
   bxpos=xpos*!d.x_size ; x-position of wind barb in device coordinates
   bypos=ypos*!d.y_size ; y-position
ENDIF ELSE BEGIN
   IF n_elements(size) NE 1 THEN size=2.
   device=0 ; plotting is done in data coordinates
   scale=size
   bxpos=xpos & bypos=ypos
ENDELSE

; some constants
kn2mpsec=0.5147965	; multiply knots by this factor to get m/sec
deg2rad=!pi/180.	; multiply degrees by this factor to get radians

wsknots=wspeed/kn2mpsec	; wind speed in knots

; create a circle user symbol for the symbol origin
IF ~keyword_set(nocircle) THEN BEGIN
   a=findgen(49)*(!pi*2/48.)
   IF keyword_set(device) THEN $
      usersym,scale/24.*cos(a),scale/24.*sin(a),/fill $
   ELSE usersym,scale/6.*cos(a),scale/6.*sin(a),/fill
   plots,bxpos,bypos,psym=8,device=device,_extra=e
ENDIF ELSE plots,bxpos,bypos,psym=3,device=device,_extra=e ; plot a dot instead

; the rotation matrix
; Wind angle conventions: 0 deg is wind from the North
MROT=FltArr(2,2)
MROT[0,0]=cos(-deg2rad*wangle)
MROT[0,1]=-sin(-deg2rad*wangle)
MROT[1,0]=sin(-deg2rad*wangle)
MROT[1,1]=cos(-deg2rad*wangle)

; now determine the number of barbs and pennants,
; their origins and end points relative to the origin
; First number of pennants
wsknots = round(wsknots/5.)*5. ; round to the nearest 5
npennant=fix(wsknots)/50			; number of 50 knot pennants
nlbarb=fix(wsknots-npennant*50)/10		; number of long 10 knot barbs
nsbarb=fix(wsknots-npennant*50-nlbarb*10)/5	; number of short 5 knot barbs


IF wsknots LT 5 THEN return	; return if wind is calm


; Spacing between pennants and barbs measured in units of stick length.
; Used to be 0.2, so only 5 could fit on a stick. (not counting origin)
barb_spacing = 1./5 ; 1/6. means 6 fit on a stick


; plot symbol line
orig=[0.,0.]	; line origin
epts=[0.,1.]	; line end point
; rotate end points and plot line
epts=orig+MROT#epts
plots,bxpos+scale*[orig[0,0],epts[0,0]],bypos+scale*[orig[1,0],epts[1,0]],$
   thick=thick,device=device,_extra=e

; draw the pennants
FOR i=0,npennant-1 DO BEGIN
	; used to be 1-i*barb_spacing, but this used up space on the stick for pennants.
	; Pennants should be appended to the end of the stick.
    orig=[0.,1.+i*barb_spacing]	; y-reference position of pennant
    penn=fltarr(2,3)	; 3 triangle points x,y
    penn[*,0]=[0,0.]
    penn[*,1]=[0.4,0.9*barb_spacing]
    penn[*,2]=[0,barb_spacing]
    ; rotate
    orig=MROT#orig
    penn=MROT#penn
    polyfill,bxpos+scale*(orig[0]+penn[0,*]),$
	bypos+scale*(orig[1]+penn[1,*]),thick=thick,/fill,device=device,_extra=e
ENDFOR

; draw the long barbs
FOR i=0,nlbarb-1 DO BEGIN
    orig=[0.,1.-(npennant+i)*barb_spacing]
    endpt=[0.4,0.9*barb_spacing]
    ; rotate
    orig=MROT#orig
    endpt=orig+MROT#endpt
    plots,bxpos+scale*[orig[0,0],endpt[0,0]],bypos+scale*[orig[1,0],endpt[1,0]],$
       thick=thick,device=device,_extra=e
ENDFOR

; draw the short barbs
yorig=-barb_spacing*((npennant+nlbarb) EQ 0) ; special pos if no long barb or pennant
FOR i=0,nsbarb-1 DO BEGIN
    orig=[0.,1.-(npennant+nlbarb+i)*barb_spacing+yorig]
    endpt=[0.4,0.9*barb_spacing]/2.
    ; rotate
    orig=MROT#orig
    endpt=orig+MROT#endpt
    plots,bxpos+scale*[orig[0,0],endpt[0,0]],bypos+scale*[orig[1,0],endpt[1,0]],$
       thick=thick,device=device,_extra=e
ENDFOR

END


;========================================================================
;  SKEWT.PRO  (IDL CODE)
;
;  Draw a Skew-T, Log(P) diagram given a temperature range for your data.
;
;  Originator:  Andrew F. Loughe  (afl@cdc.noaa.gov)
;               CIRES/NOAA
;               Boulder, CO  USA
;               This code carries no warranty or claim
;               as to its usefulness or accuracy!
;
;  A Number of the functions found in this file were converted from
;  FORTRAN code that was received from NCAR in Boulder, CO USA.
;  The original source of the equations is thought to be:
;    "Algorithms for Generating a Skew-T, Log P Diagram
;     and Computing Selected Meteorological Quantities"
;     by G.S. Stipanuk, White Sands Missle Range, Report ECOM-5515.
;
;========================================================================
;  FUNCTION TO COMPUTE SATURATION ADIABATIC TEMP AT 1000 MB GIVEN T & P.
;  OS AND T (KELVIN or CELSIUS), P (MILLIBARS )
FUNCTION  OS, T, P
  kappa = !ATMOS.Rd/!ATMOS.Cpd
  TK = T + !CONST.T0*(T LT 100.)	; convert to Kelvin if T<100
  qsat = mixr_sat(tk,p)/1000.
  ;OS = TK * ((1000./P)^kappa) / (EXP( -2.6518986*MIXR_SAT(TK,P)/TK) )
  OS = TK * (1000./P)^kappa * EXP(!ATMOS.LV0*qsat/!ATMOS.Cpd/TK)
  RETURN, OS
END
;========================================================================
;  FUNCTION TO COMPUTE TEMPERATUE (KELVIN) OF A MOIST ADIABAT GIVEN
;  OS(KELVIN), P(MILLIBARS)
;  SIGN(A,B) REPLACES THE ALGEBRAIC SIGN OF A WITH THE SIGN OF B
;
; Commented out in lieu of tsa.pro - Ahijevych Oct 22 2014. 
; 
;
;FUNCTION TSA, OS, P
;  A  = OS
;  TQ = 253.16
;  D  = 120.
;  FOR  I = 1,12 DO BEGIN	; iterations
;    D = D/2.
;    ;  IF THE TEMPERATURE DIFFERENCE, X, IS SMALL, EXIT THIS LOOP.
;    X = A * EXP (-2.6518986*MIXR_SAT(TQ,P)/TQ)-TQ*((1000./P)^.286)
;    IF ( ABS(X) LT 0.01 ) THEN GOTO, JUMP2
;    D = - (X LT 0)*ABS(D)+(X GT 0)*ABS(D)
;    TQ = TQ + D
;  ENDFOR
;  JUMP2:    TSA=TQ
;  RETURN, TSA
;END
;========================================================================
;  Function to determine position (temp, press) in the unskewed
;  coordinate system (Opposite of Tnew).
;  Used only when placing the labels on various lines.
;  Originator: Andrew F. Loughe
Function Told, T, METHOD
  trange=!X.CRANGE
  prange = !Y.TYPE ? 10^!Y.CRANGE : !Y.CRANGE

  P0 = prange[0]
  P1 = prange[1]
  
  T0 = trange[0]
  T1 = trange[1]
  
  if (method eq 1) then begin
    xy1 = convert_coord( [T,   P0], /data, /to_device )
    xy2 = convert_coord( [T0,  P0], /data, /to_device )
    dx  = xy2[0] - xy1[0]
    
    xy  = convert_coord( [xy2[0], xy2[1]+dx/1.0], /device, /to_data )
    
    xy1 = convert_coord( [xy[0],  xy[1]], /data, /to_device )
    xy2 = convert_coord( [xy[0],     P1], /data, /to_device )
    dy  = xy2[1] - xy1[1]
    
    xy = convert_coord([xy1[0]+1.0*(dy/2.), xy1[1]+(dy/2.)],$
      /device, /to_data)
  endif
  
  if (method eq 2) then begin
    xy1 = convert_coord( [T,   P0], /data, /to_device )
    xy2 = convert_coord( [T1,  P0], /data, /to_device )
    dx  = xy2[0] - xy1[0]
    
    xy  = convert_coord( [xy1[0]+1.0*dx/2. , xy1[1]+dx/2.], $
      /device, /to_data)
  endif
  
  return, xy
end

;========================================================================
;
;  PROCEDURE TO DRAW A SKEW-T, Log(P) DIAGRAM GIVEN A DESIRED
;  TEMPERATURE RANGE FOR THE DATA.
;
;  Originator:  Andrew F. Loughe
;


PRO SKEWT, t_range, everyT=everyT, everyDA=everyDA, prange=prange, $
  everySA=everySA, everyW=everyW, title=title, notitle=notitle
  on_error, 2
  

  trange= t_range
  atmos_const
  
  if (n_elements(everyT)  le 0) then everyT  = 10   ; T  = Temperature
  if (n_elements(everyDA) le 0) then everyDA = 20   ; DA = Dry adiabat
  if (n_elements(everySA) le 0) then everySA = 1    ; SA = Saturated adiabat
  if (n_elements(everyW)  le 0) then everyW  = 1    ; W  = Mixing ratio
  
  if (not keyword_set(title)) then title='Skew-T, Log(P) Diagram'
  if (keyword_set(notitle))   then title=' '
  if (n_elements(prange)) eq 0 then prange = [1040., 60]
  
  if (N_params() eq 0) then $
    message,$
    'EXAMPLE:  skewt, [-20, 20], everyT=10, everyDA=10, everySA=2, everyW=2'
  if (n_elements(t_range)) eq 1 then t_range=[-40., 40.]
  
  ;  Set some defaults
  trange   = t_range
  charsize = 1.8            ; Set default character size
  if !D.NAME eq 'PS' then charsize=0.7
  tcharsize=charsize*1.3 ; axis label charsize
  
  if !D.NAME eq 'X' then device, decomposed=0
  
  ;  Set default color positions
  
  RED   = transpose([255,0,0])
  mixr_color = transpose([205,202,37]) ; like Greg T. has
  BLUE  = transpose([0,0,255])
  BLACK = transpose([0,0,0])
  WHITE = transpose([255,255,255])
  gray  = transpose([127,127,127])
  ORANGE = transpose([255,168,40])
  tvlct, red, 1 & red=1
  tvlct, mixr_color, 2 & mixr_color=2
  tvlct, blue, 3 & blue=3
  tvlct, black,0 &black=0
  tvlct, white,4 &white=4
  tvlct, gray, 5 & gray=5
  tvlct, orange, 6 & orange=6
  Rd  = !ATMOS.Rd ; gas constant dry air = R*/M_dry
  Cpd = !ATMOS.Cpd  ; J/K/kg  heat capacity at constant pressure of dry air
  
  
  ;  Make plot square for arbitrarily chosen trange of 80 degrees.
  ;  Code from Ken Bowman
  
  daspect = FLOAT(!D.Y_SIZE)/FLOAT(!D.X_SIZE) * (trange[1]-trange[0])/80.
  margin  = 0.1
  aspect  = 1. ; A square
  x0 = 0.50 - (0.5 - margin)*(daspect/aspect)
  y0 = margin
  x1 = 0.50 + (0.5 - margin)*(daspect/aspect)
  y1 = 1.0 - margin
  
  ;  x0=!p.position[0]
  ;  y0=!p.position[2]
  ;  x1=!p.position[1]
  ;  y1=!p.position[3]
  
  
  !P.POSITION = [x0, y0, x1, y1]    ; Set value of sytem variable.
  
  ;  Determine character height and width.  Apply charsize.
  char_ht = convert_coord([0, !d.y_ch_size], /device, /to_norm)
  char_ht = char_ht[1] * 1.0
  if (!d.name ne 'X' and charsize gt 1.) then $
    char_ht = char_ht * charsize
  char_wd = convert_coord([0, !d.x_ch_size], /device, /to_norm)
  char_wd = char_wd[1]
  
  ;  Create the plot space. plot_io is a x, log-y. plot_oo and plot_ii exist too.
  plot_io, trange, prange, yrange=prange, /nodata, xstyle=9, /ys, $
    ytickname=replicate(' ',30), charsize=tcharsize, xminor=-1, $
    title=title, xticklen=-0.01
    
  ;  Print PRESSURE title along the y-axis.
  lnt=alog(prange[1])  &  lnb=alog(prange[0])  &  avg=exp(.5*(lnt+lnb))
  xy = convert_coord([trange[0], avg],/data,/to_norm)
  xyouts, xy[0]-(4.*char_wd), xy[1], 'PRESSURE  (hPa)', orient=90, $
    /norm, align=.5, charsize=tcharsize
    
  ;  Print TEMPERATURE title along the x-axis.
  xy = convert_coord([.5*(trange[0]+trange[1]), prange[0]], /data, /to_norm)
  xyouts, xy[0], xy[1]-(3*char_ht), 'TEMPERATURE (!uo!nC)', align=.5, /norm, charsize=tcharsize
  
  ;  Draw Pressure labels next to tick marks along the y-axis.
  pressures = [1000,900,800,700,600,500,400,300,200,100]
  for i = 0, n_elements(pressures)-1 do begin
    ytick = pressures[i]
    if (ytick ge prange[1] && ytick le prange[0]) then begin
      xy = convert_coord( [trange[0], ytick], /data, /to_norm )
      xyouts, xy[0]-(.2*char_wd), xy[1]-(.25*char_ht), $
        strcompress(string(ytick),/remove_all), align=1, $
        charsize=tcharsize, /norm
        
      plots, [trange[0], trange[1]], [ytick, ytick]  ; Horizontal line.
    endif
  endfor
  
  clip=[trange[0],prange[0],trange[1],prange[1]]   ; Define clipping space.
  
  ;========================================================================
  ;  Draw skewed isotherms every "everyT (10C)"  (Lines are straight).
  first_T10 = trange[0] - (trange[0] mod 10) ; ahijevych
  for temp = first_T10-100, trange[1]+5, everyT do begin
    x0 = temp
    y0 = prange[0]
    x1 = temp
    y1 = prange[1]
    
    ;  Draw the line.
    newx0 = tnew(x0, y0)  ; Find rotated temperature position
    newx1 = tnew(x1, y1)  ; Find rotated temperature position
    plots, [newx0, newx1], [y0, y1], clip=clip, noclip=0,color=!D.NAME eq 'X' ? white : black
    
    ;  Draw line labels
    ;  Use method #1 in xy function to determine a place for the label.
    drew_label = x1 mod (everyT*2) eq 0
    xy = Told(temp, 1)
    if ( xy[0] gt trange[0] and xy[0] lt trange[1] and $
      xy[1] gt prange[1] and xy[1] lt prange[0] and $
      drew_label) then begin
      xyouts, xy[0], xy[1], strcompress(string(fix(temp)), /rem), clip=clip, noclip=0, $
        orient=45, align=.5, charsize=charsize,color=!D.NAME eq 'X' ? white : black
    endif
    
  endfor
  
  ;========================================================================
  ;  Draw dry adiabats every "everyDA (10C)"  (Lines are curved).
  for temp = trange[0], trange[0]+200, everyDA do begin
    x1  = float(temp)
    y1  = 1050.
    inc = -2.     ; Lines will be curved, so use a small press. increment.
    drew_label='no'
    if x1 mod (everyDA*5) ne 0 || x1 gt 100 then drew_label='yes'
    icount = 0
    
    ;  Dry adiabats from 1050mb up to prange[1].
    ;  For a given temperature and pressure, compute theta and plot a line.
    for press = y1, prange[1], inc do begin
      icount = icount + 1
      x0 = float(x1)                                       ; Orig Temp
      y0 = float(press + inc)                              ; Orig Press
      y1 = float(y0 + inc)                                 ; New  Press
      x1 = (temp+!CONST.T0) * ( y1 / 1000. ) ^ (Rd/Cpd)   ; New Temp
      x1 = x1 - !CONST.T0
      
      newx0 = tnew(x0, y0)  ; Find rotated temperature position
      newx1 = tnew(x1, y1)  ; Find rotated temperature position
      
      xy = convert_coord([newx0,newx1], [y0,y1], /data, /to_device )
      dy = xy[1,1]-xy[1,0]
      dx = xy[0,1]-xy[0,0]
      orientation = !RADEG*atan(-dy,-dx)
      ;  Draw the labels.
      if (fix(x1) eq fix(trange[0]) and drew_label eq 'no') then begin
        drew_label='yes'
        if ( newx1 gt trange[0] and newx1 lt trange[1] and $
          y1 gt prange[1] and y1 lt prange[0] ) then $
          xyouts,newx1,y1,strcompress(string(fix(temp)),/remove), noclip=0, $
          align=.5, charsize=charsize, orientation=orientation,color=ORANGE
      endif
      
      ;  Draw the line.
      if (icount gt 1) then $
        plots, [newx0, newx1], [y0, y1], clip=clip, noclip=0,color=ORANGE
      if (newx1 lt trange[0]) then goto, jump2
    endfor
    
    jump2: dummy=0
  endfor
  
  ;========================================================================
  ;  Draw saturated adiabats.  Begin at 40C and step backwards by 5C.
  ;  These lines are curved.
  TS = 40.
  FOR TS = 40, -64, -everySA*4 DO BEGIN
    P   = 1060.
    TK  = TS + !CONST.T0
    AOS = OS(TK, 1000.)
    
    ATSA  = TSA(AOS, P) - !CONST.T0
    FOR J = 0, 85 DO BEGIN
      P0 = P
      T0 = ATSA
      
      P = P - 10.
      ATSA = TSA(AOS, P) - !CONST.T0
      if (j gt 0) then begin
        newx0=tnew(T0,P0)  ; Find rotated temperature position
        newx1=tnew(ATSA,P) ; Find rotated temperature position
        
        ;  Leave a space for the labels and draw them.
        if (P gt 730 or P lt 700) then $
          plots, [newx0, newx1], [P0, P], $
          clip=clip, noclip=0,color=gray
          
        if ( P eq 730 ) then begin
          if (newx1 gt trange[0] and newx1 lt trange[1]) then $
            xyouts,newx1,P,strcompress(string(fix(TS)),/remove),align=.5,$
            charsize=charsize,color=gray, noclip=0
        endif
      endif
    ENDFOR
    
  ENDFOR
  
  ;========================================================================
  ;  Draw mixing ratio lines (Lines are straight).
  ;  Find temperature for a given Ws (g/kg) and Press (mb).
  
  Ws=[ 1.,2.,4,7,10,13, $
    16,20,25,30,36  ]
    
  wcharsize=charsize*0.75
  
  for i = 0, N_elements(Ws)-1, everyW do begin
    press1 = prange[0]
    tmr1   = tmr(Ws(i), press1) - !CONST.T0
    
    press2 = 600.
    tmr2   = tmr(Ws(i), press2) - !CONST.T0
    
    newx0=tnew(tmr1,press1) ; Find rotated temperature position
    newx1=tnew(tmr2,press2) ; Find rotated temperature position
    
    ;  Draw the line.
    plots, [newx0, newx1], [press1, press2], linestyle=5, $
      clip=clip, noclip=0, color=mixr_color
      
    ;  Draw the line label.
    drew_label='no'
    if (newx0 gt trange[0] and newx0 lt trange[1]) then begin
      drew_label='yes'
      ;      if (Ws[i] ge 1.0) then $
      ;        xyouts, newx0, press1-2, strcompress(string(fix(Ws[i])),/remove),$
      ;        align=.5,charsize=wcharsize,color=mixr_color
      ;      if (Ws[i] lt 1.0) then $
      ;        xyouts, newx0, press1-2, string(Ws[i],format='(f3.1)'), align=.5,$
      ;        charsize=wcharsize,color=gray
    endif
    if (newx1 gt trange[0] and newx1 lt trange[1]) then begin
      if (Ws[i] ge 1.0) then $
        xyouts, newx1, press2-2, strcompress(string(fix(Ws[i])),/remove),$
        align=.5, charsize=wcharsize,color=mixr_color
      if (Ws[i] lt 1.0) then $
        xyouts, newx1, press2-2, string(Ws[i],format='(f3.1)'), align=.5,$
        charsize=wcharsize, color=mixr_color
    endif
    
  endfor
  
  ;========================================================================
  ; Redraw the plot boundary.
  plots, [trange[0],trange[1],trange[1],trange[0],trange[0]], $
    [prange[0],prange[0],prange[1],prange[1],prange[0]], thick=2
    
    
  !p.position = [.05, .05, .95, .95]      ; Reset position parameter
  
END



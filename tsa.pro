;========================================================================
;  FUNCTION TO COMPUTE TEMPERATUE (KELVIN) OF A MOIST ADIABAT GIVEN
;  OS(KELVIN), P(MILLIBARS)
FUNCTION TSA, OS, P
  thetae  = OS
  TQ = 253.16
  
  ; allow scalars and arrays
  D=P
  D[*] = 120.
  if n_elements(os) eq 1 && n_elements(p) gt 1 then thetae = replicate(thetae,size(p,/dim))
  
  FOR  I = 1,12 DO BEGIN  ; iterations
    D = D/2.
    theta = TQ * (1000./p)^(!ATMOS.Rd/!ATMOS.Cpd) ; thetaential temp
    qsat = MIXR_SAT(TQ, p)/1000. ; saturation mixing ratio
    ;  IF THE TEMPERATURE DIFFERENCE, X, IS SMALL, EXIT THIS LOOP.
    X = thetae * EXP (-!ATMOS.LV0*qsat/!ATMOS.Cpd/TQ) - theta
    ;X = thetae * EXP (-2.6518986*MIXR_SAT(TQ,P)/TQ) - theta
    ; -2.65189... comes from G.S. Stipanuk, White Sands Missle Range, 1973
    ; But -Lv/Cp/1000. is -2.48682.  That's 6.6% smaller!
    if ( max(ABS(X)) LT 0.01 ) THEN break
    D = - (X LT 0)*ABS(D)+(X GT 0)*ABS(D)
    TQ = TQ + D
  ENDFOR
  TSA=TQ
  RETURN, TSA
END



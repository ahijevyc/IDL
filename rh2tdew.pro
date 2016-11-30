;+
; NAME:
;	rh2tdew
;
; PURPOSE:
;	Calculates dew-point temperature for given temperature and
;       relative humidity (nearly) according to WMO standard procedure.
;       The parameters are slightly adapted however to fit the
;       formula of Bolton (1980), see esat.pro for details, which
;       is a very close fit to Goff and Gratch.
;
; CATEGORY:
;	atmospheric physics
;
; CALLING SEQUENCE:
;	result=rh2tdew(T,RH)
;
; EXAMPLE:
;       T = 25.                ; T in deg C or K
;       RH = (indgen(5)+1)*20
;       print,rh2tdew(T,RH)    ; dew point in same units as input T
;          prints    0.494527      10.4776      16.7054      21.3125      25.0000
;
;       Reference: http://www.ofcm.gov/fmh3/text/appendd.htm
;
; INPUTS:
;	T : Scalar or array of temperatures in C or K
;	RH: Scalar or array of relative humidities in %
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;       Calculated dew-point temperature in deg C or in K, depending
;       of units of input parameter T.
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
; Dominik Brunner, March 2000 (brunner@atmos.umnw.ethz.ch)
;       following Reference: http://www.ofcm.gov/fmh3/text/appendd.htm
; DB, August 2001: parameters adapted to formula of Bolton,
;                  Mon. Wea. Rev., 108, 1047-1053, 1980.
;-
FUNCTION RH2TDEW,T,RH

  ON_Error,2
  
  ; first, define some constants.
  ;b=17.502 ;(WMO)
  ;c=240.97 ;(WMO)
  b=17.67   ; Bolton
  c=243.5   ; Bolton
  
  IF mean(T,/nan) GT 105. THEN T0=-!CONST.T0 ELSE T0=0. ; degC or Kelvin?

  ; now calculate the dew-point temperature
  fact=alog(RH/100.)+b*(T+T0)/(c+T+T0)
  
  td=c*fact/(b-fact)-T0
  
  return,td
  
END

;+
; NAME:
;	MIXR_SAT
;
; PURPOSE:
;	Compute water mixing ratio W at saturation given the temperature
;       (=dew point temperature) and pressure (g H2O/kg dry air)
;   
; CATEGORY:
;	atmospheric physics
;
; CALLING SEQUENCE:
;	result=MIXR_SAT(T,p)
;
; EXAMPLE:
;	print,MIXR_SAT(15,850)
;
; INPUTS:
;	T	SCALAR OR VECTOR(n) OF TEMPERATURES IN CELSIUS OR K
;       p	SCALAR OR VECTOR(n) OF PRESSURES IN hPa
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;	returns the saturation water vapor mixing ratio [grams water/kilogram dry air]
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
;  Dominik Brunner (brunner@atmos.umnw.ethz.ch), Feb 2000,
;-
FUNCTION MIXR_SAT, T, P
   ON_ERROR,2
   Mw=18.0160 ; molec mass of water
   Md=28.9660 ; molec mass of dry air
   
   X = ESAT(T)	; Note: ESAT accepts temperatures in Celsius or K
   
   ; From http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/gempak/nawips1/gempak/source/prmcnvlib/pr/prmixr.f
   ; University of Wisconsin green sheet
   ;     CORR is a correction to the vapor pressure
   ;     since the atmosphere is not an ideal gas.
   corr = (1.001 + (( p - 100.) / 900.) * .0034)
   x = x * corr


   MIXR_SAT = Mw/Md* X*1000. / (P - X)
   RETURN, MIXR_SAT
END
 

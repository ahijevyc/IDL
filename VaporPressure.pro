Function VaporPressure, Temp, Dew = Dew, Ice = Ice, liquid = liquid,  $
    MartiMauersberger = MartiMauersberger, $
    MagnusTetens = MagnusTetens, $
    GoffGratch = GoffGratch , $
    Buck_original = Buck_original, $
    Buck_manual = Buck_manual, $
    CIMO = CIMO, $
    Vaisala = Vaisala, $
    WMO_Goff = WMO_Goff, $
    WMO2000 = WMO2000, $
    Wexler = Wexler, $
    Sonntag = Sonntag, $
    Bolton = Bolton, $
    Fukuta = Fukuta, $
    HylandWexler = HylandWexler, $
    IAPWS = IAPWS, $
    Preining = Preining, $
    MurphyKoop = MurphyKoop, $
    Formula = Formula

; Input: Temperature [deg C]
;
; Output: Saturation vapor pressure [hPa]
;
; Author: Holger V�mel
;
;
; Example: Calculate the vapor pressure over liquid water using the WMO formula
; Temp = FINDGEN(100) - 100.
; P = VaporPressure(Temp, /dew, /WMO)
;
; For temperatures above 0 deg C the vapor pressure over liquid water
; is calculated.
;
; The optional parameter /Dew changes the calculation to vapor pressure
; over liquid water over the entire temperature range.
; The formula to be used can be selected with the appropriate keyword
;
; The current default fomulas are Hyland and Wexler for liquid and Goff Gratch for ice. (hv20040521)

   IF NOT KEYWORD_SET(Formula) THEN Formula = ''

   IF KEYWORD_SET(MagnusTetens)      THEN Formula = 'MagnusTetens'
   IF KEYWORD_SET(Buck_original)     THEN Formula = 'Buck'
   IF KEYWORD_SET(Buck_manual)       THEN Formula = 'Buck2'
   IF KEYWORD_SET(CIMO)              THEN Formula = 'CIMO'
   IF KEYWORD_SET(GoffGratch)        THEN Formula = 'GoffGratch'
   IF KEYWORD_SET(HylandWexler)      THEN Formula = 'HylandWexler'
   IF KEYWORD_SET(WMO_Goff)          THEN Formula = 'WMO'
   IF KEYWORD_SET(WMO2000)           THEN Formula = 'WMO2000'
   IF KEYWORD_SET(Wexler)            THEN Formula = 'Wexler'
   IF KEYWORD_SET(Vaisala)           THEN Formula = 'Vaisala'
   IF KEYWORD_SET(Sonntag)           THEN Formula = 'Sonntag'
   IF KEYWORD_SET(Bolton)            THEN Formula = 'Bolton'
   IF KEYWORD_SET(Fukuta)            THEN Formula = 'Fukuta'
   IF KEYWORD_SET(MartiMauersberger) THEN Formula = 'MartiMauersberger'
   IF KEYWORD_SET(MurphyKoop)        THEN Formula = 'MurphyKoop'
   IF KEYWORD_SET(IAPWS)             THEN Formula = 'IAPWS'
   IF KEYWORD_SET(Preining)          THEN Formula = 'Preining'

   Phase = 'Ice'
   IF Keyword_SET(Liquid) THEN Phase = 'Dew'
   IF Keyword_SET(Dew)    THEN Phase = 'Dew'
   IF Keyword_SET(Ice)    THEN Phase = 'Ice'

   T = Temp + !CONST.T0   ; Most formulas use T in [K]
                       ; Formulas using [C] use the variable Temp

; =============================================================================
;  Calculate saturation pressure over liquid water ----------------------------

   IF Phase EQ 'Dew' THEN BEGIN

      ; Default uses Hyland and Wexler over liquid. While this may not be the best formula, it is consistent with what Vaisala uses in their system
      IF Formula EQ '' THEN Formula = 'HylandWexler'

      CASE Formula OF
      'MartiMauersberger' : BEGIN
         Show_Message, "Marti and Mauersberger don't have a vapor pressure curve over liquid. Using Goff Gratch instead"
         ; Goff Gratch formulation
         ; Source : Smithsonian Meteorological Tables, 5th edition, p. 350, 1984
         ; From original source: Goff and Gratch (1946), p. 107.

         Ts    = 373.16       ; steam point temperature in K
         ews   = 1013.246     ; saturation pressure at steam point temperature, normal atmosphere

         Psat = 10.^(-7.90298*(Ts/T-1.) + 5.02808 * ALOG10(Ts/T) $
                     - 1.3816E-7 * (10.^(11.344*(1.-T/Ts))-1) $
                     + 8.1328E-3*(10.^(-3.49149*(Ts/T-1)) -1) + ALOG10(ews))
      END
      'HylandWexler' : BEGIN
         ; Source Hyland, R. W. and A. Wexler, Formulations for the Thermodynamic Properties of the saturated Phases of H2O from 173.15K to 473.15K, ASHRAE Trans, 89(2A), 500-519, 1983.
         Psat = EXP(  -0.58002206E4 / T $
	             + 0.13914993E1 $
		     - 0.48640239E-1 * T $
		     + 0.41764768E-4 * T^2. $
		     - 0.14452093E-7 * T^3. $
		     + 0.65459673E1 * ALOG(T)) / 100.
      END
      'Preining' : BEGIN
         ; Source : Vehkamaeki, H., M. Kulmala, I. Napari, K. E. J. Lehtinen, C.Timmreck, M. Noppel, and A. Laaksonen (2002), J. Geophys. Res., 107, doi:10.1029/2002JD002184.
	      Psat = EXP(-7235.424651/T  $
                    +77.34491296 $
                    +5.7113E-3 * T $
                    -8.2 * ALOG(T)) / 100.
      END
      'Wexler' : BEGIN
         ; Wexler, A., Vapor pressure formulation for ice, Journal of Research of the National Bureau of Standards-A. 81A, 5-20, 1977.
         Psat = EXP( -2.9912729E3 * T^(-2.) $
                     - 6.0170128E3 * T^(-1.) $
                     + 1.887643854E1 * T^0. $
                     - 2.8354721E-2 * T^1. $
                     + 1.7838301E-5 * T^2. $
                     - 8.4150417E-10 * T^3. $
                     - 4.4412543E-13 * T^4. $
                     + 2.858487 * ALOG(T)) / 100.
      END
      'GoffGratch': BEGIN
         ; Goff Gratch formulation
         ; Source : Smithsonian Meteorological Tables, 5th edition, p. 350, 1984
         ; From original source: Goff and Gratch (1946), p. 107.
         Ts    = 373.16       ; steam point temperature in K
         ews   = 1013.246     ; saturation pressure at steam point temperature, normal atmosphere

         Psat = 10.^(- 7.90298*(Ts/T-1.) + 5.02808 * ALOG10(Ts/T) $
                     - 1.3816E-7 * (10.^(11.344*(1.-T/Ts))-1.) $
                     + 8.1328E-3*(10.^(-3.49149*(Ts/T-1)) -1.) + ALOG10(ews))
      END
      'CIMO' : BEGIN
         ; Source: Annex 4B, Guide to Meteorological Instruments and Methods of Observation, WMO Publication No 8, 7th edition, Geneva, 2008. (CIMO Guide)
         Psat = 6.112 * EXP(17.62 * Temp/(243.12 + Temp))
      END
      'MagnusTetens' : BEGIN
         ; Source: Murray, F. W., On the computation of saturation vapor pressure, J. Appl. Meteorol., 6, 203-204, 1967.
         ; Psat = 10.^(7.5*(Temp)/(Temp+237.5) + 0.7858)         ; Murray quotes this as the original formula and
         Psat = 6.1078 * EXP(17.269388 * (T-273.16) / (T-35.86)) ; this as the mathematical aquivalent in the form of base e. 
      END
      'Buck' : BEGIN
         ; Bucks vapor pressure formulation based on Tetens formula
	      ; Source: Buck, A. L., New equations for computing vapor pressure and enhancement factor, J. Appl. Meteorol., 20, 1527-1532, 1981.
         Psat = 6.1121 * EXP(17.502 * Temp / (240.97+Temp))
      END
      'Buck2' : BEGIN
         ; Bucks vapor pressure formulation based on Tetens formula
      	; Source: Buck Research, Model CR-1A Hygrometer Operating Manual, Sep 2001
         Psat = 6.1121 * EXP((18.678 - (Temp) / 234.5) * (Temp) / (257.14+Temp))
      END
      'WMO' : BEGIN
         ; Intended WMO formulation, originally published by Goff (1957)
         ; incorrectly referenced by WMO technical regulations, WMO-NO 49, Vol I, General Meteorological Standards and Recommended Practices, App. A, Corrigendum Aug 2000.
         ; and incorrectly referenced by WMO technical regulations, WMO-NO 49, Vol I, General Meteorological Standards and Recommended Practices, App. A, 1988.

         Ts    = 273.16       ; triple point temperature in K

         Psat = 10.^(10.79574*(1.-Ts/T) $
                     - 5.02800 * ALOG10(T/Ts) $
                     + 1.50475E-4 * (1.-10.^(-8.2969*(T/Ts-1.))) $
                     + 0.42873E-3 * (10.^(+4.76955*(1.-Ts/T))-1.) $
                     + 0.78614)
      END
      'WMO2000' : BEGIN
         ; WMO formulation, which is very similar to Goff Gratch
         ; Source : WMO technical regulations, WMO-NO 49, Vol I, General Meteorological Standards and Recommended Practices, App. A, Corrigendum Aug 2000.

         Ts    = 273.16       ; triple point temperature in K

         Psat = 10.^(10.79574*(1.-Ts/T) $
                     - 5.02800 * ALOG10(T/Ts) $
                     + 1.50475E-4 * (1.-10.^(-8.2969*(T/Ts-1.))) $
                     + 0.42873E-3 * (10.^(-4.76955*(1.-Ts/T))-1.) $
                     + 0.78614)
      END
      'Sonntag' : BEGIN
         ; Source: Sonntag, D., Advancements in the field of hygrometry, Meteorol. Z., N. F., 3, 51-66, 1994.
         Psat = EXP( -6096.9385 * T^(-1.) $
                     + 16.635794 $
                     - 2.711193E-2 * T^1. $
                     + 1.673952E-5 * T^2. $
                     + 2.433502 * ALOG(T))
      END
      'Bolton' : BEGIN
         ; Source: Bolton, D., The computation of equivalent potential temperature, Monthly Weather Report, 108, 1046-1053, 1980. equation (10)
         Psat = 6.112 * EXP(17.67 * Temp / (Temp+243.5))
      END
      'Fukuta' : BEGIN
         ; Source: Fukuta, N. and C. M. Gramada, Vapor pressure measurement of supercooled water, J. Atmos. Sci., 60, 1871-1875, 2003.
         ; This paper does not give a vapor pressure formulation, but rather a correction over the Smithsonian Tables.
         ; Thus calculate the table value first, then use the correciton to get to the measured value.
         Ts    = 373.16       ; steam point temperature in K
         ews   = 1013.246     ; saturation pressure at steam point temperature, normal atmosphere

         Psat = 10.^(-7.90298*(Ts/T-1.) + 5.02808 * ALOG10(Ts/T) $
                     - 1.3816E-7 * (10.^(11.344*(1.-T/Ts))-1.) $
                     + 8.1328E-3*(10.^(-3.49149*(Ts/T-1)) -1.) + ALOG10(ews))

         x = Temp + 19
         Psat = Psat * (0.9992 + 7.113E-4*x - 1.847E-4*x^2. + 1.189E-5*x^3. + 1.130E-7*x^4. - 1.743E-8*x^5.)
         s = WHERE(Temp LT -39)
         Psat(s) = 9.99E99
      END
      'IAPWS' : BEGIN
         ; Source: Wagner W. and A. Pru? (2002), The IAPWS formulation 1995 for the thermodynamic properties of ordinary water substance for general and scientific use, J. Phys. Chem. Ref. Data, 31(2), 387-535.
         ; This is the 'official' formulation from the International Association for the Properties of Water and Steam
         ; The valid range of this formulation is 273.16 <= T <= 647.096 K and is based on the ITS90 temperature scale.
         Tc = 647.096       ; K   : Temperature at the critical point
         Pc = 22.064 * 10^4 ; hPa : Vapor pressure at the critical point
         nu = (1-T/Tc)
         a1 = -7.85951783
         a2 = 1.84408259
         a3 = -11.7866497
         a4 = 22.6807411
         a5 = -15.9618719
         a6 = 1.80122502
         Psat = Pc * EXP(Tc/T * (a1*nu + a2*nu^1.5 + a3*nu^3. + a4*nu^3.5 + a5*nu^4. + a6*nu^7.5))
      END
      'MurphyKoop' : BEGIN
         ; Source : Murphy and Koop, Review of the vapour pressure of ice and supercooled water for atmospheric applications, Q. J. R. Meteorol. Soc (2005), 131, pp. 1539-1565.
         Psat = exp(54.842763 - 6763.22 / T - 4.210 * ALOG(T) + 0.000367 * T + TANH(0.0415 * (T - 218.8)) * (53.878 - 1331.22 / T - 9.44523 * ALOG(T) + 0.014025 * T)) / 100.
      END
      'Other' : BEGIN
      END
      ENDCASE

; =============================================================================
;  Calculate saturation pressure over ice -------------------------------------
   ENDIF ELSE BEGIN

      ; Default uses Goff Gratch over ice. There is little ambiguity in the ice saturation curve. Goff Gratch is widely used.
      IF Formula EQ '' THEN Formula = 'GoffGratch'
      IF Formula EQ 'WMO2000' THEN Formula = 'WMO'  ;  There is no typo issue in the WMO formulations for ice
      IF Formula EQ 'IAPWS' THEN BEGIN
         PRINT, 'IAPWS does not provide a vapor pressure formulation over ice; use Goff Gratch instead'
         Formula = 'GoffGratch'  ;  IAPWS does not provide a vapor pressure formulation over ice; use Goff Gratch instead
      ENDIF

      CASE Formula OF
      'MartiMauersberger' : BEGIN
         ; Source : Marti, J. and K Mauersberger, A survey and new measurements of ice vapor pressure at temperatures between 170 and 250 K, GRL 20, 363-366, 1993.
         Psat = 10. ^(-2663.5 / T + 12.537) / 100.
      END
      'HylandWexler' : BEGIN
         ; Source Hyland, R. W. and A. Wexler, Formulations for the Thermodynamic Properties of the saturated Phases of H2O from 173.15K to 473.15K, ASHRAE Trans, 89(2A), 500-519, 1983.
         Psat = EXP( -0.56745359E4 / T $
                     + 0.63925247E1 $
                     - 0.96778430E-2 * T $
                     + 0.62215701E-6 * T^2. $
                     + 0.20747825E-8 * T^3. $
                     - 0.94840240E-12 * T^4. $
                     + 0.41635019E1 * ALOG(T)) / 100.
      END
      'GoffGratch' : BEGIN
         ; Source : Smithsonian Meteorological Tables, 5th edition, p. 350, 1984

         ei0    = 6.1071       ; mbar
         T0     = 273.16       ; triple point in K

         Psat = 10.^(-9.09718 * (T0 / T - 1.) - 3.56654 * ALOG10(T0 / T) $
                     + 0.876793 * (1. - T / T0) + ALOG10(ei0))
      END
      'MagnusTetens' : BEGIN
         ; Source: Murray, F. W., On the computation of saturation vapor pressure, J. Appl. Meteorol., 6, 203-204, 1967.
         ; Psat = 10.^(9.5 * Temp/(265.5+Temp) + 0.7858)         ; Murray quotes this as the original formula and
         Psat = 6.1078 * EXP(21.8745584 * (T-273.16) / (T-7.66)) ; this as the mathematical aquivalent in the form of base e. 
      END
      'Buck' : BEGIN
         ; Bucks vapor pressure formulation based on Tetens formula
         ; Source: Buck, A. L., New equations for computing vapor pressure and enhancement factor, J. Appl. Meteorol., 20, 1527-1532, 1981.
         Psat = 6.1115 * EXP(22.452 * Temp / (272.55+Temp))
      END
      'Buck2' : BEGIN
         ; Bucks vapor pressure formulation based on Tetens formula
         ; Source: Buck Research, Model CR-1A Hygrometer Operating Manual, Sep 2001
         Psat = 6.1115 * EXP((23.036 - Temp / 333.7) * Temp / (279.82+Temp))
      END
      'CIMO' : BEGIN
         ; Source: Annex 4B, Guide to Meteorological Instruments and Methods of Observation, WMO Publication No 8, 7th edition, Geneva, 2008. (CIMO Guide)
         Psat = 6.112 * EXP(22.46 * Temp/(272.62 + Temp))
      END
      'WMO' : BEGIN
         ; WMO formulation, which is very similar to Goff Gratch
         ; Source : WMO technical regulations, WMO-NO 49, Vol I, General Meteorological Standards and Recommended Practices, Aug 2000, App. A.

         T0    = 273.16       ; triple point temperature in K

         Psat = 10.^(-9.09685 * (T0 / T - 1.) - 3.56654 * ALOG10(T0 / T) $
                     + 0.87682 * (1. - T / T0) + 0.78614)
      END
      'Sonntag' : BEGIN
         ; Source: Sonntag, D., Advancements in the field of hygrometry, Meteorol. Z., N. F., 3, 51-66, 1994.
         Psat = EXP( -6024.5282 * T^(-1.) $
	             + 24.721994 $
		     + 1.0613868E-2 * T^1. $
		     - 1.3198825E-5 * T^2. $
		     - 0.49382577 * ALOG(T))
      END
      'MurphyKoop' : BEGIN
         ; Source : Murphy and Koop, Review of the vapour pressure of ice and supercooled water for atmospheric applications, Q. J. R. Meteorol. Soc (2005), 131, pp. 1539-1565.
         Psat = exp(9.550426 - 5723.265/T + 3.53068 * ALOG(T) - 0.00728332 * T) / 100.
      END
      ELSE : BEGIN
      END
      ENDCASE

      s = WHERE(Temp GT 0)
      IF s(0) GE 0 THEN BEGIN
         ; Independent of the formula used for ise, use GoffGratch (water) for temperatures above freezing.
         Ts    = 373.16       ; steam point temperature in K
         ews   = 1013.246     ; saturation pressure at steam point temperature, normal atmosphere

         T = T(s)
         Psat_w = 10.^( -7.90298*(Ts/T-1.) + 5.02808 * ALOG10(Ts/T) $
                        - 1.3816E-7 * (10.^(11.344*(1.-T/Ts))-1) $
                        + 8.1328E-3*(10.^(-3.49149*(Ts/T-1)) -1) + ALOG10(ews))
         Psat(s) = Psat_w
      ENDIF
   ENDELSE

   RETURN, Psat
END

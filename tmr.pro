;========================================================================
;  FUNCTION TO COMPUTE THE TEMPERATURE (KELVIN) OF AIR AT A GIVEN
;  PRESSURE AND WITH A GIVEN MIXING RATIO.
;  TMR(KELVIN), W(GRAMS WATER VAPOR/KILOGRAM DRY AIR), P(MILLIBAR)
FUNCTION  TMR, W, P
  X   =  ALOG10 ( W * P / (622.+ W) )
  TMR = 10. ^ ( .0498646455 * X + 2.4082965 ) - 7.07475 + $
    38.9114 * ( (10.^( .0915 * X ) - 1.2035 )^2 )
  RETURN, TMR
END


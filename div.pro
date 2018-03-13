;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; div.pro     - This file calculates divergence from u and v winds on a
;               lat/lon grid        
;
;  This file contains the following functions and procedures:
;     functions:
;        delta1 - value one grid point towards increasing indices
;                 minus value one grid point towards decreasing indices
;                 (E-W) dimension
;        delta2 - value one grid point towards increasing indices
;                 minus value one grid point towards decreasing indices
;                 (N-S) dimension
;          
;     procedures:
;        div  - calculates divergence
;
;  History:
;  Date       Name          Action
;  ---------  ------------  ----------------------------------------------
;  Apr 2000   D. Ahijevych  Created.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


function delta1, values
 ; Example Call: du = delta1(uwnd)
 
 print, "in func delta1"
 ndims = SIZE(values, /N_DIMENSIONS)
 if (ndims NE 4) then stop
 diff = SHIFT(values,-1,0,0,0) - shift(values,1,0,0,0)

 ; Return the difference array
 return, diff


end ; End function delta1

function delta2, values
 ; Example Call: dv = delta2(vwnd)
 
 print, "in func delta2"
 ndims = SIZE(values, /N_DIMENSIONS)
 if (ndims NE 4) then stop
 diff = SHIFT(values,0,-1,0,0) - shift(values,0,1,0,0)

 ; Return the difference array
 return, diff

end ; End function delta2


pro div, u, v, lon, lat, diverg, atts
 ; pro_description
 ;    Divergence
 ; Inputs:           u       - u wind
 ;                   v       - v wind
 ;                   lon     - longitude of grid points
 ;                   lat     - latitude of grid points
 ; Outputs:          diverg  - divergence
 ; Example:  div, u, v, lon, lat, diverg, atts

 a = 6.37E+06
 t = [10,6,1,1]

 print, "test point: ", t

 print, "u input"
 print, u [(t[0]-1):(t[0]+1),t[1],t[2],t[3]]

 du = delta1(u)

 print, "du output"
 print, du[(t[0]-1):(t[0]+1),t[1],t[2],t[3]]


 print, "v input"
 print, v [t[0],(t[1]-1):(t[1]+1),t[2],t[3]]

 dv = delta2(v)

 print, "dv output"
 print, dv[t[0],(t[1]-1):(t[1]+1),t[2],t[3]]

 ; Convert degrees to radians
 lon_in_rad = lon/!RADEG
 lat_in_rad = lat/!RADEG

 dlamb = lon_in_rad[1] - lon_in_rad[0]
 dphi  = lat_in_rad[1] - lat_in_rad[0]


 diverg = u

 FOR y = 0, N_ELEMENTS(lat)-1 DO BEGIN
   print, FORMAT = '(A,$)', "."
   cosphi  = COS(lat_in_rad[y])
   tanphi  = TAN(lat_in_rad[y])
   diverg[*,y,*,*] = du[*,y,*,*]/(2*dlamb)/cosphi/a + $
                     dv[*,y,*,*]/(2*dphi)/a - v[*,y,*,*]*tanphi/a

   IF (y EQ t[1]) THEN BEGIN
           print, "diverg = ", du[t[0],y,1,1]/(2*dlamb)/a/cosphi, $
                            " + ", dv[t[0],y,1,1]/a/(2*dphi), $
                            " - ", v [t[0],y,1,1]*tanphi/a
           PRINT, " cosphi = ", cosphi, " tanphi = ", tanphi  
           PRINT, " a =      ", a
           PRINT, " dlamb =  ", dlamb,  " dphi  =  ", dphi
           PRINT, " lat =    ", lat
   ENDIF  

 ENDFOR

 IF N_ELEMENTS(atts) EQ 0 THEN $
    atts   = {div_scale_factor: 1E-08} $
 ELSE $
    atts = CREATE_STRUCT(atts, 'div_scale_factor', 1E-08)
 
 diverg = diverg/atts.div_scale_factor


 ; Return to the caller.
 return

 ; End procedure.
end


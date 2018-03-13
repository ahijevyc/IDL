
pro vort, u, v, lon, lat, vorticity, atts
 ; pro_description
 ;    Vertical vorticity
 ; Inputs:           u       - u wind
 ;                   v       - v wind
 ;                   lon     - longitude of grid points
 ;                   lat     - latitude of grid points
 ; Outputs:          vorticity 
 ; 			atts - attributes (units, scale_factor)
 ; Example:  vort, u, v, lon, lat, vorticity, atts



 ; 
 a = 6.37E+06
 t = [10,6,1,1]

 print, "test point: ", t

 print, "u input"
 print, u [t[0],(t[1]-1):(t[1]+1),t[2],t[3]]

 du = delta2(u)

 print, "du output"
 print, du[t[0],(t[1]-1):(t[1]+1),t[2],t[3]]


 print, "v input"
 print, v [(t[0]-1):(t[0]+1),t[1],t[2],t[3]]

 dv = delta1(v)

 print, "dv output"
 print, dv[(t[0]-1):(t[0]+1),t[1],t[2],t[3]]

 ; Convert degrees to radians
 lon_in_rad = lon/!RADEG
 lat_in_rad = lat/!RADEG

 dlamb = lon_in_rad[1] - lon_in_rad[0]
 dphi  = lat_in_rad[1] - lat_in_rad[0]


 vorticity = u

 FOR y = 0, N_ELEMENTS(lat)-1 DO BEGIN
   PRINT, FORMAT = '(A,$)', "."
   cosphi  = COS(lat_in_rad[y])
   tanphi  = TAN(lat_in_rad[y])
   vorticity[*,y,*,*] = dv[*,y,*,*]/(2*dlamb)/cosphi/a $
                      - du[*,y,*,*]/(2*dphi)/a $
                      +  u[*,y,*,*]*tanphi/a

     IF (y EQ t[1]) THEN BEGIN
          PRINT, "vorticity = ", dv[t[0],y,1,1]/(2*dlamb)/a/cosphi, $
                            " - ", du[t[0],y,1,1]/a/(2*dphi), $
                            " + ", u [t[0],y,1,1]*tanphi/a
          PRINT, " cosphi = ", cosphi, " tanphi = ", tanphi  
          PRINT, " a =      ", a
          PRINT, " dlamb =  ", dlamb,  " dphi  =  ", dphi
          PRINT, " lat =    ", lat
     ENDIF  

 ENDFOR

 IF N_ELEMENTS(atts) EQ 0 THEN $
    atts = {vort_scale_factor: 1E-08} $
 ELSE $
    atts = CREATE_STRUCT(atts, 'vort_scale_factor', 1E-08)
 
 atts = CREATE_STRUCT(atts,'units','1/s')
 vorticity = vorticity/atts.vort_scale_factor


 ; Return to the caller.
 return

 ; End procedure.
end

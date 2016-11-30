;========================================================================
;  Function to determine position (temp, press) when the isotherms
;  in the diagram are rotated (skewed) 45 degrees to the right.
;  Used for finding the points needed to connect the dots when
;  drawing ALL of the lines (except pressure).
;  Originator: Andrew F. Loughe
Function Tnew, T, P
  trange=!X.CRANGE
  prange = !Y.TYPE ? 10^!Y.CRANGE : !Y.CRANGE
  
  P0   = prange(0)
  xy1  = convert_coord( [T, P0], /data, /to_device)
  xy2  = convert_coord( [T,  P], /data, /to_device)
  dy   = xy2[1] - xy1[1]
  ; customary for 45 degree skew.
  ; if dry and moist adiabat lines look too slanted or too upright, it's the
  ; pressure range, not the temperature skew that is the issue.
  dx   = 1.0*dy     ; dx = dy for this 45-45-90 triangle
  xy   = convert_coord( [xy2[0]+dx, xy2[1]], /device, /to_data)
  Tnew = xy[0]
  return, Tnew
end


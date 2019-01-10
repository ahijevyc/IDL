function interpol_NAN, v_in, x_in, u_in, max_gap=max_gap, circle=circle

  ; given x and v=f(x), estimate v at arbitrary x values (given by u), using linear interpolation.
  
  ; if circle keyword is set, assume values are angles in degrees and average appropriately

  v = v_in
  x = x_in
  u = u_in
  m = N_elements(v)               ;# of input pnts
  if n_elements(x) ne m then $
    message, 'V and X arrays must have same # of elements'
    
  igood = where(finite(x) and finite(v), m)
  
  if m eq 1 && x eq u then return, v
  if m lt 2 then return, replicate(!VALUES.F_NAN, n_elements(u_in))
  
  ; Only use finite points at finite levels
  x = x[igood]
  v = v[igood]
    
  ; do the magic
  
  s = VALUE_LOCATE(x, u) > 0L < (m-2) ;Subscript intervals.
  p = (u-x[s])*(v[s+1]-v[s])/(x[s+1] - x[s]) + v[s]
  
  if keyword_set(circle) then begin
    sinv = sin(v*!DTOR)
    cosv = cos(v*!DTOR)
    sinp = (u-x[s])*(sinv[s+1]-sinv[s])/(x[s+1] - x[s]) + sinv[s]
    cosp = (u-x[s])*(cosv[s+1]-cosv[s])/(x[s+1] - x[s]) + cosv[s]
    p = atan(sinp, cosp) * !RADEG
  endif

  p = float(p)  
  
  if keyword_set(max_gap) then begin
    ; This could be a major change - Oct 7 2013
    ; Instead of looking for large gaps between observed levels, look for gaps
    ; between requested level and observed levels. If the req level is close to an
    ; observed level, allow it to be interpolated.  Don't worry about the gap between obs levels.
    ; In other words, there are two observed levels above and below the requested level. 
    ; These are x[s+1] and x[s].  See if the gap b/t the the requested level and either of the
    ; observed levels is larger than the max allowed.
    ;gap = min([[u-x[s+1]],[u-x[s]]], /absolute, dimension=2)
  
    gap = abs(x[s+1] - x[s])
    igap = where(gap gt max_gap, ngap)
    if ngap gt 0 then p[igap] = !VALUES.F_NAN
  endif
  
  
  ; Are any of the new, arbitrary, requested levels (u) outside the range of given levels x?
  iout_of_range = where(u lt min(x) or u gt max(x), nout_of_range, ncomplement=n_in_range, complement=in_range)
  ; If so set them to NaN.
  if nout_of_range gt 0 then p[iout_of_range] = !VALUES.F_NAN
  
  ; what if the requested location is already in x?
  ; loop thru requested levels (inside the range of given levels x)
  foreach iu, in_range do begin
      iexact = where(x eq u[iu], nexact)
      ; sometimes a given level x is repeated.  Make sure its value (v) is repeated too. 
      if nexact eq 1 then p[iu] = v[iexact]
      if nexact gt 1 then begin
        ; If any of the v values in v[iexact] are not the same as the first value, then stop.
        if total(v[iexact] ne v[iexact[0]]) gt 0 then begin
          print, 'multiple v values assigned to same x value:', x[iexact], v[iexact]
          stop
        endif
        p[iu] = v[iexact[0]]
      endif
  endforeach
  
  
  ;  for ij = 0, n_elements(js)-1 do begin
  ;    j = js[ij]
  ;    req_level = req_levels[ij]
  ;    iexact_level = where(levels eq req_level, nexact_level)
  ;    if nexact_level gt 0 then begin
  ;      y[ij] = x[iexact_level]
  ;      continue
  ;    endif
  ;    if j eq -1 or j eq n-1 then continue
  ;    ;    if j eq -1  && req_level eq levels[0] then y[ij] = x[0]
  ;    ;    if j eq n-1 && req_level eq levels[n-1] then y[ij] = x[n-1]
  ;    dxdz = (x[j] - x[j+1]) / (levels[j] - levels[j+1])
  ;    dz   = req_level - levels[j]
  ;    if keyword_set(max_gap) && dz gt max_gap then continue
  ;    y[ij] = x[j] + dz * dxdz
  ;  endfor
  return, p
end
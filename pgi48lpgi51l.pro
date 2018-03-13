function PGI48LPGI51L,pgi,dropJulian
  err = 'not PGI48L or PGI51L'
  ; first 5 drops of 9/30 mission were for PGI51L.  But then mission was changed to PGI48 and shifted 2 deg further east. 
  ; I say the split is around 1330 UTC, but it is not exact. This at least prevents a drop from being counted for both PGI48L and PGI51L.
  if pgi ne 'PGI48L' and pgi ne 'PGI51L' then return, err
  PGI51_last = julday(9,30,2010,13,30,0)
  
  ; return value for scalar
  if n_elements(dropJulian) eq 1 then begin
    if not finite(dropJulian) then return, err
    if dropJulian lt PGI51_last then return, 'PGI51L' else return, 'PGI48L' 
  endif

  ; return value for array
  a = replicate(err, n_elements(dropJulian))
  i51 = where(dropJulian lt PGI51_last, n51)
  i48 = where(dropJulian ge PGI51_last, n48)
  if n51 gt 0 then a[i51] = 'PGI51L'
  if n48 gt 0 then a[i48] = 'PGI48L'
  return, a
end
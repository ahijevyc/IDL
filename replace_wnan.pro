function replace_wnan, x, bad_val
  ibad = where(x eq bad_val, nbad)
  if nbad gt 0 then x[ibad] = !VALUES.F_NAN
  return, x
end
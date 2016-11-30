function replace_nan, x, bad_val
  ibad = where(not finite(x), nbad)
  if nbad gt 0 then x[ibad] = bad_val
  return, x
end

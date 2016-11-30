function format_width, format_codes
  width = replicate(0, n_elements(format_codes))
  for i=0,n_elements(format_codes)-1 do begin
    format_code = format_codes[i]
    result = stregex(format_code, '([0-9]*)([ABDEFGIOXZ])\+?-?([0-9]+)(\.([0-9]+))?', /extract, /subexpr, /fold_case)
    repeat_count = result[1]
    FC = result[2]
    width[i] = result[3]
  endfor
  return, strtrim(width,2)
end
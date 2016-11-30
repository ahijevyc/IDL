function contrasting_color, color
  if typename(color) eq 'STRING' then begin
    ic = where(tag_names(!COLOR) eq strupcase(color),nc)
    if nc eq 0 then print, "bad color name:" + color
    colorRGB = !COLOR.(ic) 
  endif else begin
    if n_elements(color) ne 3 or min(color) lt 0 or max(color) gt 255 then begin
      print, "bad RGB triplet: ", color
      stop
    endif
    colorRGB = color
  endelse
  contrasting_color = 'white'
  if mean(colorRGB) ge 160 then contrasting_color = 'black'
  return, contrasting_color
end
;========================================================================
;  Routine to plot the temperature and dew point temperature sounding
;  on top of a skew-T, Log(P) diagram.
PRO plot_skewt, t, td, p, col_t=col_t,col_dewpt=col_dewpt, _Extra=_Extra

  trange=!X.CRANGE
  prange = !Y.TYPE ? 10^!Y.CRANGE : !Y.CRANGE
  
  if n_elements(col_t) eq 0 then col_t=[254,0,17]
  if ~keyword_set(col_dewpt) then col_dewpt=[19,138,19]
  ;  Find number of data levels
  nlevels = n_elements(t)
  ngood = total(finite(t))
  if ngood eq 0 then begin
    print, 'zero good temperatures'
    return
  endif
  
  ;  Define clipping space.
  clip=[trange[0],prange[0],trange[1],prange[1]]
  
  ; Ensure that temperatures are in Celsius.
  if total(t,/nan)/ngood gt 100. then stop
  
  plot_Td = n_elements(td) gt 0 &&  finite(td[0]) ? 1 : 0
  if plot_Td && total(td,/nan)/ngood gt 100. then stop
  
  ;  Overplot the data onto the digram.
  for i = 0, nlevels-2 do begin
    ;  Plot temperature sounding data.
    x0 = tnew( t[i],   p[i]   )
    y0 = p[i]
    x1 = tnew( t[i+1], p[i+1] )
    y1 = p[i+1]
    if n_elements(col_t) eq 3 then begin
      tvlct, old, /get
      tvlct, transpose(col_t), 0
      plots, [x0, x1], [y0, y1], clip=clip, noclip=0, thick=4 ,color=0, _Extra=_Extra
      tvlct, old
    endif else plots, [x0, x1], [y0, y1], clip=clip, noclip=0, thick=4 ,color=col_t, _Extra=_Extra
    
    ;  Plot dew point temperature sounding data.
    if plot_Td then begin ; ahijevych
      x0 = tnew( td[i],   p[i]   )
      y0 = p[i]
      x1 = tnew( td[i+1], p[i+1] )
      y1 = p[i+1]
      if n_elements(col_dewpt) eq 3 then begin
        tvlct, old, /get
        tvlct, transpose(col_dewpt), 0
        plots, [x0, x1], [y0, y1], clip=clip, noclip=0, thick=3.9, color=0, _Extra=_Extra
        tvlct, old
      endif else plots, [x0, x1], [y0, y1], clip=clip, noclip=0, thick=3.9, color=col_dewpt, _Extra=_Extra
    endif
  endfor
  
  ;print,n_elements(col_dewpt)
  
end




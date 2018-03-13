function get_beg, x
  xbeg = (x + shift(x,1))/2.
  xbeg[0] = x[0] - (x[1]-x[0])/2.
  return, xbeg
end
function get_end, x
  nx = n_elements(x)
  xend = (x + shift(x,-1))/2.
  xend[nx-1] = x[nx-1] + (x[nx-1]-x[nx-2])/2.
  return, xend
end

function get_spacing, x

  ; Get spacing between elements.
  dx = x[1]-x[0]
  
  ; Make sure elements are equally spaced.
  dxs = (x-shift(x,1))[1:n_elements(x)-1]
  iodd_man_out = where(dxs ne dx, nodd)
  if nodd gt 0 then message, 'unequal spacing in '+ string(x)
  
  return, dx
  
end

pro contour2blocks, z_in, x_in, y_in, range=range
  z = z_in
  x = x_in
  y = y_in
  debug = 0
  ncolors=9
  if ~keyword_set(range) then range=[min(z,/nan),max(z,/nan)]
  if n_elements(range) ne 2 then stop
  colors = bytscl(z, min=range[0], max=range[1], /nan)
  bar_colors = (bindgen(ncolors)+1)*254/ncolors
  within_range = where(z ge range[0] and z lt range[1], nwithin_range)
  if nwithin_range gt 0 then colors[within_range] = bar_colors[floor((z[within_range]-range[0])/(range[1]-range[0])*ncolors)]
  over = where(z ge range[1],nover)
  if nover gt 0 then colors[over] = max(bar_colors)
  under = where(z lt range[0], nunder)
  if nunder gt 0 then colors[under] = 0B
  levels = range[0] + bindgen(ncolors)*(range[1]-range[0])/ncolors
  contourbar, levels, bar_colors, /vert, charsize = !P.CHARSIZE/2.
  
  xbeg = get_beg(x)
  xend = get_end(x)
  ybeg = get_beg(y)
  yend = get_end(y)
  for j = 0, n_elements(y)-1 do begin
  
    for i = 0, n_elements(x)-1 do begin
      if finite(z_in[i,j]) then begin
        polyfill, [xbeg[i], xend[i], xend[i], xbeg[i]], [yend[j], yend[j], ybeg[j], ybeg[j]], $
          color=colors[i,j], noclip=0
        if debug then xyouts, (xbeg[i]+xend[i])/2., (ybeg[j]+yend[j])/2., string(z[i,j], format='(F5.2)'), align=0.5, charsize=!P.CHARSIZE*0.4
      endif
    endfor
  endfor
  
  if (0) then begin
    z = rotate(z, 7); flip upside-down
    dx = get_spacing(x)
    dy = get_spacing(y)
    
    xrange = [x[0] - dx/2.,x[n_elements(x)-1] + dx/2.]
    yrange = [y[0] - dy/2.,y[n_elements(y)-1] + dy/2.]
    npix = convert_coord(xrange, yrange, /DATA, /to_device)
    xpix = floor(npix[0,1] - npix[0,0])
    ypix = floor(npix[1,1] - npix[1,0])
    ; deal with descending values of x or y
    if xpix lt 0 then xrange = reverse(xrange)
    if ypix lt 0 then yrange = reverse(yrange)
    
    ; scalable pixels (e.g. Postscript)
    if !D.FLAGS AND 1 then tv, z, xrange[0], yrange[0], xsize=xrange[1]-xrange[0], ysize=yrange[1]-yrange[0], /data else begin
      ; not scalable pixels (e.g. X window)
      tv, congrid(z, abs(xpix), abs(ypix)), xrange[0], yrange[0], /data
    endelse
  endif
  
  ; fix axes, which was covered by tv
  !P.MULTI[0] = !P.MULTI[0] + 1; tv advanced the plot window.
  plot, !X.CRANGE, !Y.CRANGE, xstyle=1, ystyle=1, xtickformat='(A1)', ytickformat='(A1)', /nodata, /noerase
  !P.MULTI[0] = !P.MULTI[0] - 1; some kind of bug with tv- it advances the plot window, but not really.
  
  
end
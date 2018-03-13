PRO tri_sphere , n, seed
  lon=randomu(seed,n)*360
  lat=randomu(seed,n)*180-90
  
  print, lon,lat
  
  ;QHULL, lon, lat, tr, CONNECTIVITY = list, sphere=sphere
  map_set,/cont,/satellite
  ; Plot our input points.
  PLOTS, lon, lat, PSYM=4
  xyouts,lon,lat,strtrim(sindgen(n),2),charsize=2
  for i = 1, n_elements(lon)-1 do begin
    plots, map_2points(lon[i-1],lat[i-1],lon[i],lat[i],dpath=1)
  endfor
  plots, map_2points(lon[n_elements(lon)-1],lat[n_elements(lat)-1],lon[0],lat[0],dpath=1)
  
  
  for x0 = 0.,2.*!PI,2.*!PI/40. do begin
    for y0 = -1.5,1.5,0.1 do begin
      ;min_d = min(SQRT((x0-x)^2+(y0-y)^2),winner)
      ;d=2.*asin(sqrt((sin((y0-lat)/2.))^2 + cos(y0)*cos(lat)*(sin((x0-lon)/2.))^2))
      d=acos(sin(y0)*sin(lat/180.*!PI)+cos(y0)*cos(lat/180.*!PI)*cos(x0-lon/180.*!PI))
      min_d = min(d,winner)
      xyouts,x0*180./!PI,y0*180./!PI,strtrim(winner,2),color=255*winner/n
    endfor
  endfor
  
END

PRO tri_sphere2 , n, seed
  x=randomu(seed,n)
  y=randomu(seed,n)
  Z=EXP(-3*((X-0.5)^2+(Y-0.5)^2))
  ; Construct the Delaunay triangulation
  ; and the Voronoi diagram.
  QHULL, x, y, triangle, /DELAUNAY, VDIAGRAM=vdiagram, VVERTICES=vvert, VNORM=vnorm
  
  ; Plot our input points.
  pos = [00.1,0.1,0.9,0.9]
  z = indgen(n)
  levels = findgen(n)-0.5
  contour, z, x, y, /irr,levels=levels,c_colors=mycolors(levels), /fill, position=pos
;  contour, z, x, y, /irr,levels=levels, /follow, /overplot
  OPLOT, x, y, PSYM=3
  xyouts,x,y-0.011,strtrim(z,2),charsize=2, align=0.5, charthick=3
  
  for x0 = -0.2,1.2,0.1 do begin
    for y0 = -0.2,1.2,0.1 do begin
;      min_d = min(SQRT((x0-x)^2+(y0-y)^2),winner)
;      xyouts,x0,y0,strtrim(winner,2), noclip=0
    endfor
  endfor
  ; Plot the Voronoi diagram.
  for i=0,N_ELEMENTS(vdiagram[2,*])-1 do begin
    print, 'i=',i
    vdiag = vdiagram[*,i]
    print, 'vdiagram[*,',i,']=',vdiag
    j = vdiag[2] + 1
    ; Bounded or unbounded?
    if (j gt 0) then begin ; Bounded.
      PLOTS, vvert[*, vdiag[2:3]],psym=-5, linestyle=2, noclip=0
      continue
    endif
    
    ; Unbounded, retrieve starting vertex.
    xystart = vvert[*,vdiag[3]]
    ; determine line equation.
    ; Vnorm[0]*x + Vnorm[1]*y + Vnorm[2] = 0
    slope = -vnorm[0,-j]/vnorm[1,-j]
    intercept = -vnorm[2,-j]/vnorm[1,-j]
    ; need to determine the line direction.
    ; pick a point on one side along the line.
    xunbound = xystart[0] + 5
    yunbound = slope*xunbound + intercept
    
    ; Find the closest original vertex.
    void = MIN( (x-xunbound)^2 + (y-yunbound)^2, idx)
    ; by defn of Voronoi diagram, line should be closest to
    ; one of the bisected points. If not, our line went in the wrong direction.
    if (idx ne vdiag[0] && idx ne vdiag[1]) then begin
      xunbound = xystart[0] - 5
      yunbound = slope*xunbound + intercept
    endif
    
    
    PLOTS, [[xystart], [xunbound, yunbound]],linestyle=2, noclip=0
    print, 'xystart=',xystart, ' slope=',slope, ' intercept=',intercept
  endfor
  
  
  
  TRIANGULATE,X,Y,TR
  
  FOR i=0,N_ELEMENTS(TR)/3 - 1 DO BEGIN & T=[TR[*, i],TR[0, i]] & PLOTS,X[T],Y[T],thick=1 & ENDFOR
  
END

pro tri_sphere3
  theta = [70,60,50,40]/180.*!PI
  r = 1+findgen(4)
  x = r # cos(theta)
  y = r # sin(theta)
  if !D.NAME eq 'X' then device, decomposed=0
  loadct, 39
  
  plot, x, y, xstyle=1, ystyle=1, psym=4
  TRIANGULATE,X,Y,TR,CONNECTIVITY=C
  
  ; Plot the Voronoi diagram.
  for i=0,N_ELEMENTS(x)-1 do begin
    ;  Get the ith polygon:
    VORONOI, x, y, i, c, Xp, Yp
    ; Draw it:
    POLYFILL, Xp, Yp, COLOR = i*254/n_elements(x)+1, noclip=0
  ENDFOR
  FOR i=0,N_ELEMENTS(TR)/3 - 1 DO BEGIN & T=[TR[*, i],TR[0, i]] & PLOTS,X[T],Y[T],linestyle=1 & ENDFOR
  ; Plot our input points.
  oPLOT, /POLAR, rebin(r,n_elements(r),n_elements(theta)), rebin(THETA,n_elements(theta),n_elements(r)),PSYM=4
  oPLOT, x, y, PSYM=4
end;t

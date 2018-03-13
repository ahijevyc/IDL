pro rhum_box, x, y, _extra=_extra
  n = n_elements(x)
  for i=0,n-2 do begin
    plots, map_2points(x[i],y[i],x[i+1],y[i+1],npath=15,/rhumb), _extra=_extra
    box = plot(  map_2points(x[i],y[i],x[i+1],y[i+1],npath=15,/rhumb), _extra=_extra)
  endfor
  plots, map_2points(x[n-1],y[n-1],x[0],y[0],npath=15,/rhumb), /continue, _extra=_extra
  box = plot(  map_2points(x[n-1],y[n-1],x[0],y[0],npath=15,/rhumb), _extra=_extra)
  
end


pro simple_map
  loadct,39,/sil
  device, decomposed=0
  
  limit=[25.4,-112,51,-66]
  map_set, 10, -110, limit=limit, /satellite
  map = map('Geographic', limit=limit)
  tvlct, oldct, /get
  tvlct, transpose([53,69,32]), 0; land
  tvlct, transpose([34,10,119]), 255 ; ocean
  tvlct, transpose([20,10,20]), 1; space
  tvlct, transpose([133,149,112]), 2 ; borders
  polyfill, [!X.WINDOW,reverse(!X.WINDOW)], [replicate(!Y.WINDOW[0],2),replicate(!Y.WINDOW[1],2)], color=1, /norm
  map_grid, /fill_horizon, /no_grid
  grid = map.MAPGRID
  grid.linestyle="dotted"
  grid.label_position=0
  map_continents, /contin, fill=1, color=0
  m2 = mapcontinents(/hires)
  m3 = mapcontinents(/usa)
  m4 = mapcontinents(/canada)
  m5 = mapcontinents(/lakes)
  ;map_continents, /rivers, color=255
  map_continents, /usa, /cont, color=2
  tvlct, oldct
  
  COSPA_NX   = 1135
  COSPA_NY   = 610
  COSPA_DX   = .038239
  COSPA_DY   = .035933
  COSPA_MINX =-110.
  COSPA_MINY = 27.
  ;rhum_box, [-110, -110, cospa_minx+cospa_nx*cospa_dx,cospa_minx+cospa_nx*cospa_dx],[cospa_miny,cospa_miny+cospa_dy*cospa_ny, cospa_miny+cospa_dy*cospa_ny, cospa_miny], thick=2, color=100, overplot=map
  ;rhum_box, [-103,-103,-68,-73], [30,48,48,30], thick=4, color=140, overplot=map
  ;rhum_box, [-102.,-102.,-83,-81], [29.0,35,35,29], thick=8, color='red', overplot=map
  ;rhum_box, [-110,-110,-67,-67,-75,-110],[25,48,48,37,25,25], thick=4, color='brown', overplot=map
  
  
  ;t = read_ascii('~/share/RF_latlon.txt')
  t = read_ascii('~/v', count=n)
  lon = t.field1[2,n*0.38:*]
  lat = t.field1[1,n*0.38:*]
  count = t.field1[0,*]
  out = plot(lon,lat,symbol='square', sym_filled=1, sym_size=0.1038, sym_fill_color='green', sym_transparency=60, overplot=map, linestyle=6)
  out.order, /send_to_back
  map.save, 'map.png',width=880, resolution=1000

end


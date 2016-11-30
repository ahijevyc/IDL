function rotate_segment, lon0, lat0, lon1, lat1, alpha_deg
  ; input vector with start and end points in lat, lon
  result = map_2points(lon0, lat0, lon1, lat1)
  d = result[0]
  az = result[1]
;  result = map_2points(lon0, lat0, lon1, lat1, npath=10)
;  m = map('Mollweide', fill_color='light blue', margin=[0.1,0.1,0.2,0.1],/current)
;  m1 = mapcontinents(fill_color='beige', /continents)
;  p = plot(result,overplot=m,'green',/data,thick=4)
  lonlat1 = ll_arc_distance([lon0,lat0], d*!DTOR, az + alpha_deg, /degrees)
  result = map_2points(lon0, lat0, lonlat1[0], lonlat1[1], npath=10)
;  p1 = plot(result,overplot=m,'red',/data)
  return, lonlat1

end
function get_uv, lon0, lat0, lon1, lat1, time0, time1, debug=debug
  if ~keyword_set(debug) then debug=0
  junk = map_2points(lon0,lat0,lon1,lat1)
  dir = junk[1]
  dist = junk[0] * !DTOR * !CONST.R_EARTH 
  dsec = (time1-time0)*24*3600
  u = dist * cos(!DTOR*(90.-dir)) / dsec
  v = dist * sin(!DTOR*(90.-dir)) / dsec
  if debug gt 0 then begin
    print, 'get_uv:',lon0,lat0,lon1,lat1,format='(A,"(",F0.2,"E,",F0.2,"N) -> (",F0.2,"E,",F0.2,"N)")'
    print, 'get_uv:',time0, time1, format='(A,C(CMoA,X,CDI0,X,CHI2.2,":",CMI2.2,X,CYI4)," -> ",C(CMoA,X,CDI0,X,CHI2.2,":",CMI2.2,X,CYI4))'
    print, 'get_uv:',dist, dir, dsec, format='(A,I0,"m, ",I0,"deg, ",I0,"sec")'
    print, 'get_uv:',u, v, format='(A,"u = ",F0.2," m/s  v = ",F0.2," m/s")'
  endif
  return, [u,v]
end
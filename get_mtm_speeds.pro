pro get_mtm_speeds
  t = get_PREDICT_missions(count=nmissions)
  for imission=0,nmissions-1 do begin
    mtm_center, t.pgi[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', clat, clon, mtm00=1, u=u,v=v, /silent
    print, t.pgi[imission], ' ', t.yyyymmdd[imission], ' ',t.hhmm[imission], clat, clon, u, v, format='(a,a,a,a,a,4f12.3)'
  endfor
end
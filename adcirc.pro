function read14, basedir
  openr, noslun, basedir+"fort.14", /get_lun
  line = ''
  readf, noslun, line
  readf, noslun, line
  words = strsplit(line,/extract)
  nnode = long(words[1])
  latlonelev = dblarr(4,nnode)
  readf, noslun, latlonelev
  free_lun, noslun
  data = dictionary() ; dictionary is case-insensitive, unlike hash.
  data.lon = latlonelev[1,*]
  data.lat = latlonelev[2,*]
  data.elev = latlonelev[3,*]
  return, data

end


pro adcirc

  basedir = '/glade2/scratch2/ahijevyc/hwt2017/2017090700/ens_5/NWS_16/'

  close, /all
  loadct, 39, /silent
  device, decomposed=0
  fort14 = read14(basedir)
  plot, fort14.lon, fort14.lat, psym=3

  if (0)then begin
    ffile = basedir+"far.txt"
    ; assumes fort.16 has been cleaned
    ;
    ; grep "too far" PE*/fort.16 | sed -e 's/.*get_gfdl: too far=  //' > far.txt

    n = file_lines(ffile)
    inll = dblarr(4,n)
    openr, noslun, ffile, /get_lun
    readf, noslun, inll
    free_lun, noslun
    lon = inll[2,*]
    lat = inll[3,*]
    oplot, lon, lat, psym=3, color=180
  endif

  ; wind file
  wfile = basedir + 'fort.74.nc'
  ncdf_get, wfile, ['x','y','windx','windy'], result
  itime = 0
  x = result['x','value']
  y = result['y','value']
  windx = result['windx','value']
  windy = result['windy','value']
  i = where(windx[*,itime] eq 0.01 and windy[*,itime] eq 0, ni)
  if ni eq 0 then stop
  oplot, x[i], y[i], psym=3, color=250
  i = where(windx[*,itime] eq 0.02, ni)
  if ni eq 0 then stop
  oplot, x[i], y[i], psym=3, color=155


  i = where(windx[*,itime] eq 0, ni)
  if ni eq 0 then stop
  oplot, x[i], y[i], psym=3, color=86
  
  if ni lt 100 then print, x[i], y[i]


end
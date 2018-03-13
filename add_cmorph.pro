pro add_cmorph
  imageDate = '20100912'
  files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/CMORPH/'+imageDate+'_3hr*comb')
  
  files = files[uniq(files,sort(files))] ; the order can change, and if you need to restart with a higher file index, then you want constancy
  ; Clunky way to deal with CMORPH files, which are actually 8 times rolled into one file.
  
  expanded_files = replicate(files[0], 8)
  for j = 1, n_elements(files)-1 do expanded_files = [expanded_files, replicate(files[j],8)]
  files = expanded_files
  
  total_image = fltarr(1440,480)
  for ifile = 0,n_elements(files)-1 do begin
    imageTime = string((ifile mod 8)*30000L, format='(I6.6)')
    junk = cmorph(imageDate, imageTime, '3hr')
    image = junk.data
    lon = junk.lon
    lat = junk.lat
    total_image = total_image+image*3
  endfor
  window, 0
  pos = [0.1,0.2,0.9,0.8]
  map_set, limit=[-45,-80,-20,-30], position = pos, /cont, /iso
  contour, total_image, lon, lat, min=0, levels=[1,10,20,75,100,150],position=pos, xstyle=1, ystyle=1, /overplot, /fill
  contour, total_image, lon, lat, min=0, levels=[1,10,20,75,100,150],/follow, position=pos, xstyle=1, ystyle=1, /overplot
  
  
  junk = cmorph(imageDate, '000000', 'dly')
  window, 1
  
  image = junk.data
  lon = junk.lon
  lat = junk.lat
  map_set, limit=[-45,-80,-20,-30], position = pos, /cont, /iso
  contour, image*24, lon, lat, min=0, levels=[1,10,20,75,100,150],position=pos, xstyle=1, ystyle=1, /overplot, /fill
  contour, image*24, lon, lat, min=0, levels=[1,10,20,75,100,150],/follow, position=pos, xstyle=1, ystyle=1, /overplot
  
end
pro read_landuse
  path = '/pecan/ahijevyc/geog/landuse_10m/'
  di = 120
  dj = 120
  for i = 0, 17 do begin
    for j = 0, 8 do begin
      file = string(i*di+1,(i+1)*di,j*dj+1,dj*(j+1), format='(I5.5,"-",I5.5,".",I5.5,"-",I5.5)')
      print, file
      t = read_binary(path+file, data_dims=[126,126,24])
      tv, t[*,*,15], i*di, j*dj
    endfor
  endfor
end
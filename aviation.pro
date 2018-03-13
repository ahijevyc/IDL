pro aviation
  d = read_ascii('/users/ahijevyc/Desktop/EWCR_Performance_EM_combined.txt', header=header, data_start=1)
  data = d.field1
  
  date = data[0,*]
  time = data[1,*]
  yind = data[2,*]
  xind = data[3,*]
  lead = data[4,*]
  thresh = data[5,*]
  scale = data[6,*]
  obs = data[7,*]
  model = data[8,*]
  scales = [400, 200, 100, 50]
  for iscale = 0, n_elements(scales)-1 do begin
    s = scales[iscale]
    for lb = 0.0, 1.0, 0.1 do begin
      ub = lb+0.1
      if ub eq 1.0 then ub = 1.1
      i = where(scale eq s and model ge lb and model lt ub, ni)
      if ni eq 0 then begin
        print, s, lb, ub, "    empty", format='(I8, F8.1, F8.1, A)'
        continue
      endif
      z = obs[i]
      z = z[sort(z)]
      nz = n_elements(z)
      print, s, lb, ub, nz, z[0], z[nz*0.25-1],  z[nz*0.5-1], z[nz*0.75-1], z[nz-1], format='(I8, F8.1, F8.1, I8, F8.5, F8.5, F8.5, F8.5, F8.5)'
    endfor
  endfor
end

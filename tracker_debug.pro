pro tracker_debug
  t = read_ascii('~/asc')
  z = t.field1[2,*]
  x = t.field1[0,*]
  y = t.field1[1,*] 
  c = contour(z, x, y,irregular=irr)
stop
end
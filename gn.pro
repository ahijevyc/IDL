function y, x, s, mu
  return, (1./s/sqrt(2.*!PI))*exp(-(x-mu)^2./2./s^2.)
end
pro gn
  g = 0.001
  n = 9*24
  x = findgen(5./g)*g
  s = 0.794184
  se = s/sqrt(n)
  mu = 3.02
  y1 = y(x, se, mu)
  plot, x, y1, xrange=[2.5,3.4]
  s = 0.865583
  se = s/sqrt(n)
  mu = 2.83
  y2 = y(x, se, mu) 
  oplot, x, y2
  print, total(y1)*g, total(y2)*g
  print, (total(y1[where(y1 lt y2)]) + total(y2[where(y2 lt y1)]) )*g
;
;  for i = 1, 500 do begin
;  se = sqrt(1000000./i^3.)
;  mu = 0
;  y = (1./se/sqrt(2.*!PI))*exp(-(x-mu)^2./2./se^2.)
;  plot, x, y, yrange=[0,3]
;  wait, .04
;  endfor
  
end
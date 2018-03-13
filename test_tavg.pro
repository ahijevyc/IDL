function th, p, t
  th = t*(1000./p)^0.286
  return, th
end

function as, p, th
  t = th*(p/1000.)^0.286
  return, t
end

pro test_tavg, p1, p2, t1, t2
  ; Don't use pressure as weight. Use change in pressure, dP.
  dP = p2-p1
  ths = [th(p2,t2), th(p1,t1)]
  th2 = total(ths * dP) / total(dP)

   pavg = (p1+p2)/2.
   print, (t1+t2)/2.-!CONST.T0, as(pavg,th2)- !CONST.T0


end
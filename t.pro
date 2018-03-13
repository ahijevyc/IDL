pro t

t = read_ascii('/mmmtmp/trier/stan_idl.dat')

contour, t.field1[2,*], t.field1[0,*], t.field1[1,*], /irr


stop



end
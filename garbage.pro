pro garbage
  ; fiddle around with smoothing
  ; this is 1/d
  n = 250
  z = replicate(0d,n,n)
  z[n/2,n/2] = 1
  p = z
  pn = z
  p0 = z
  j = replicate(1,n)#findgen(n)
  i = replicate(1,n)##findgen(n)
  for x=0.5,n-0.5 do for y=0.5,n-0.5 do begin
    ;    d = ((i-x)^2.+(j-y)^2.)^0.1
    ;   p[x,y] = total(z/d)/total(1./d)
  endfor
  tvscl, congrid(p,450,450),0
  nfilt = n/2
  for ifilt=0,nfilt-1 do begin
    for x=0,n-2 do for y=0,n-2 do p0[x,y]=0.5*pn[x,y]+0.125*(pn[x-1,y]+pn[x,y-1]+pn[x+1,y]+pn[x,y+1])
    pn = p0
  endfor
  tvscl, congrid(pn,450,450),1
  stop
end
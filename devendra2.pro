pro devendra2
noha = 3

ns = 200
nl = 100
eqint = [[2,2,3,3],[3,1,4,3],[3,3,3,2]]
num_tiles=1
for q = 2, noha do begin
  bruce = intarr(eqint[q,1])

  for i=0, num_tiles-1 do begin
    steve = 0
    data2 = fltarr(ns,nl)
    data = fltarr(ns,nl)
    
    iq = where(data eq q, nq)
    bruce = data2[iq]
    for yy=0, nl-1 do begin
      for xx=0, ns-1 do begin
        if (data[xx,yy] eq q) then begin
          steve = steve + 1
          bruce[steve-1] = data2[xx,yy]
        endif
      endfor
    endfor
    janick = long(total(bruce))
    eqint[q,i+2] = janick/eqint[q,1]
  endfor
endfor

end
pro devendra
; Create an image with different valued regions:
img = FLTARR(512, 512)
img[3:503, 9:488] = 0.7
img[37:455, 18:438] = 0.5
img[144:388, 90:400] = 0.7
img[200:301, 1:255] = 1.0
img[155:193, 333:387] = 0.3
TVSCL, img
  
  th = .1
  west = abs(img - shift(img, -1,  0)) lt th
  east = abs(img - shift(img,  1,  0)) lt th
  north = abs(img - shift(img, 0,  1)) lt th
  south = abs(img - shift(img, 0, -1)) lt th
  
  tvscl, (west+east+north+south) eq 4
;
;  for xx = 1, ns-2 do begin
;
;    for zz = 0, nb-1 do begin
;      rnn[zz] = dati2[zz,xx] - dati1[zz,xx]
;      if (rnn[zz] lt 0) then rnn[zz] = -(rnn[zz])
;      roo[zz] = dati2[zz,xx] - dati2[zz,xx+1]
;      if (roo[zz] lt 0) then roo[zz] = -(roo[zz])
;      rzz[zz] = dati2[zz,xx] - dati3[zz,xx]
;      if (rzz[zz] lt 0) then rzz[zz] = -(rzz[zz])
;      rww[zz] = dati2[zz,xx] - dati2[zz,xx-1]
;      if (rww[zz] lt 0) then rww[zz] = -(rww[zz])
;    endfor
;      
;    valnn = total(rnn)
;    valoo = total(roo)
;    valzz = total(rzz)
;    valww = total(rww)
;    
;    if (valnn lt th) and (valoo lt th) and (valzz lt th) and (valww lt th) then begin
;      org[xx,yy] = 1   
;      org[xx,yy-1] = 1
;      org[xx+1,yy] = 1
;      org[xx,yy+1] = 1
;      org[xx-1,yy] = 1
;    endif
;    endfor
;  endfor


end
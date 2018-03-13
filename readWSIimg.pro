pro r,giffile

LOADCT, 41, FILE='/users/ahijevyc/IDL/resource/colors/colors1.tbl'
read_gif,giffile,img,dBZ
pos = [0.1,0.1,0.9,0.9]
contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4,position=pos
tvimage,dBZ/75*255,/half_half,position=pos
map_set, /cylindrical, limit=[20.,230.,53.,300.],/usa, position = pos,/noerase,color=7*255/15


end ; pro r


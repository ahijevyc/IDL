pro metars
d = read_ascii('/pecan/ahijevyc/BaMEx/sfc/t', count=n)
map_set, 0, 0, 0, limit = [35, -101, 48, -87], /usa, /cylind,  position=pos, /iso, title=d.field1[0,0]
map_grid, /box
; remove duplicates
grid_input, d.field1[2,*], d.field1[1,*], fltarr(n), x1, y1, f1 

plots, x1, y1, psym=1, noclip=0
device, /close

end
; maybe you are looking for read_ihop_sfc.pro
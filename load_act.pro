;+
; NAME:
;	load_act
;
; PURPOSE:
;	load an Adobe colortable from the
;	directory share/colorbars
;
;-
pro load_act,c_table
  ncolors = strmid(c_table, 1, 2, /reverse)
  ct = read_binary('~ahijevyc/share/colorbars/'+c_table+'.act', data_dims=[3,ncolors], data_type=1)
  ;ct = congrid(ct, 3, 254, /center)
  ;ct = [[0,0,0],[ct],[255,255,255]]
  ct = congrid(ct, 3, 256, /center)
  red = ct[0,*]
  grn = ct[1,*]
  blu = ct[2,*]
  
  if (0) then begin
    ; added Feb 11, 2013 to make 0-index black
    red[0]=0
    grn[0]=0
    blu[0]=0
    red[255]=255
    grn[255]=255
    blu[255]=255
  endif
  tvlct,red,grn,blu
  
end

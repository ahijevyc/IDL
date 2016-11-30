function windbarb, wsknots, dir
    r = 0.125
    n = 12
    t = (1+findgen(n))/n * 2*!PI ; theta 0-2PI
    po = obj_new('IDLgrPolygon', r*cos(t), r*sin(t))
    po->SetProperty, ANTIALIAS=1
    symModel = OBJ_NEW('IDLgrModel')
    ; uncomment for filled circle
    ;symModel->Add, po
    ; Spacing between pennants and barbs measured in units of stick length.
    ; Used to be 0.2, so only 5 could fit on a stick. (not counting origin)
    barb_spacing = 1./5 ; 1/6. means 6 fit on a stick
    
    ; now determine the number of barbs and pennants,
    ; their origins and end points relative to the origin
    ; First number of pennants
    wsknots = round(wsknots/5.)*5. ; round to the nearest 5
    npennant=fix(wsknots)/50      ; number of 50 knot pennants
    nlbarb=fix(wsknots-npennant*50)/10    ; number of long 10 knot barbs
    nsbarb=fix(wsknots-npennant*50-nlbarb*10)/5 ; number of short 5 knot barbs

    ; staff
    oStaff = OBJ_NEW('IDLgrPolyLine', [0,0], [0,1.+npennant*barb_spacing])
    oStaff->SetProperty, ANTIALIAS=1
    symModel->Add, oStaff
    
        FOR i=0,npennant-1 DO BEGIN
      ; used to be 1-i*barb_spacing, but this used up space on the stick for pennants.
      ; Pennants should be appended to the end of the stick.
      y0 = 1.+i*barb_spacing ; y-reference position of pennant
      pennx=[0,0.5,0]
      penny=[1.0*barb_spacing, 1.4*barb_spacing, 0.2*barb_spacing]
      oPenn = OBJ_NEW('IDLgrPolygon', pennx, y0+penny)
      oPenn->SetProperty, ANTIALIAS=1
      symModel->Add, oPenn
    ENDFOR
    ; draw the long barbs
    FOR i=0,nlbarb-1 DO BEGIN
      y0 = 1.-i*barb_spacing
      oBarb = OBJ_NEW('IDLgrPolyline', [0,0.5], y0+[0,0.5*barb_spacing])
      oBarb->SetProperty, ANTIALIAS=1
      symModel->Add, oBarb
    ENDFOR
    ; draw the short barbs
    yorig=-barb_spacing*((npennant+nlbarb) EQ 0) ; special pos if no long barb or pennant
    FOR i=0,nsbarb-1 DO BEGIN
      y0=1.-(nlbarb+i)*barb_spacing+yorig
      oBarb = OBJ_NEW('IDLgrPolyline',[0,0.25],y0+[0,0.25*barb_spacing])
      oBarb->SetProperty, ANTIALIAS=1
      symModel->Add, oBarb
    ENDFOR

  symModel->scale, 8, 8, 1   
  symModel->rotate, [0,0,1], -dir
  return, symModel
  
end
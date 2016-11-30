function TC_symbol, vmax_knots

  ; returns a symbol for vmax_knots.
  ; used to return a list() and tried to accomodate an array of vmax_knots, but you return a list, and plot sym_object doesn't want a list; just an object.

  ; Tropical depression (circle)
  if vmax_knots lt 34 then begin
    n = 12
    t = (1+findgen(n))/n * 2*!PI ; theta 0-2PI
    po = obj_new('IDLgrPolygon', cos(t), sin(t))
    po->SetProperty, ANTIALIAS=1
    symModel = OBJ_NEW('IDLgrModel')
    symModel->Add, po
    return, symModel
  endif else begin
    ; HURRICANE
    ; 2 half-spirals put together
    n = 10. ; number of points in half-spiral
    dcircle = 0.65 ; how far to wrap around (0.75 is 3/4 of the way)
    r0 = 2 ; starting radius
    t = dcircle * (1+findgen(n))/n * 2*!PI ; theta 0-2PI
    ; radius r describes half-spiral shape
    r  = r0 - r0*.55*(t^0.55-t[0]^0.55)
    ; maintain radius for last half of half spiral.
    r[n*0.45:n-1] = r[n*0.45]
    x = r*cos(t)+r0*0.001 ; create x,y points of half spiral and smush closer to origin
    y = r*sin(t)-r0*0.001

    ; radius of transition point between tip and armpit of symbol
    ri = r[-1] + 0.5*(r[0]-r[-1])
    ; angle of point between tip and armpit of symbol
    ti = atan(y[0],x[0]) + 0.45*(atan(-y[-1],-x[-1]) - atan(y[0], x[0]))
    xi = ri * cos(ti)
    yi = ri * sin(ti)
    ; merge half-spirals and transition points
    x = [x, -xi, -x, xi]
    y = [y, -yi, -y, yi]
    oTess = OBJ_NEW('IDLgrTessellator')
    oTess->AddPolygon, x, y
    ; TROPICAL STORM
    ; add inner circle
    if vmax_knots lt 64 then begin
      ro = r[-1]*0.4
      t = findgen(n)/n * 2 * !DPI
      xo = 1.0*ro*cos(t)
      yo = ro*sin(t)
      rot = 0.6
      oTess->AddPolygon, sin(rot)*xo + cos(rot)*yo, cos(rot)*xo - sin(rot)*yo
    endif
    result = oTess->Tessellate(v, c)

    po = obj_new('IDLgrPolygon', v, POLYGONS=c)
    po->SetProperty, ANTIALIAS=1 ; make smooth-looking . helpful when small.
    symModel = OBJ_NEW('IDLgrModel')
    symModel->Add, po
    symModel->rotate, [0,0,1], 35
    return, symModel
  endelse

end

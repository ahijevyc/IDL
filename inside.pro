FUNCTION Inside, x, y, px, py, Index=index

  roi = Obj_New('IDLanROI', px, py)

  result = roi->ContainsPoints(x,y)
  exterior = where(result eq 0, /null)
  interior = where(result eq 1, /null)
  edge = where(result eq 2, /null)
  vertex = where(result eq 3, /null)

  RETURN, interior
  
END

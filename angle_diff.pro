function angle_diff, a, b
  d = a-b
  d = (d + 180.) - floor((d+180.)/360.) * 360. - 180 
  return, d
end
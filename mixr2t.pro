function mixr2t, q0, p0
  ; input q0 in kg/kg
  ; input p0 in mb
  ; same constants used in fortran routine tlift.f
  bsat = 243.5
  asat = 17.67
  es00 = 6.11
  esl = p0*q0/(0.622+q0)
  c1 = alog(esl/es00)
  return, bsat*c1/(asat - c1)
end
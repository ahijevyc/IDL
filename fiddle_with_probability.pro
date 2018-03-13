pro fiddle_with_probability
  ; probability of 2 10 member samples taken from the same population
  ; having means differ by more than the standard deviation.
  n = 10.
  t = 10000000L
  c = 0L
  for i=0,t do begin
    x = randomn(seed, n)
    y = randomn(seed, n)
    if (mean(x) - mean(y)) gt 0.3413*2 then c=c+1
  endfor
  print, float(c)/t
end
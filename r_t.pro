pro r_t
  openr, lun, '/mmmtmp/ahijevyc/v', /get_lun
  n = 0L
  sumx=0d
  sumy=0d
  sumx2=0d
  sumy2=0d
  sumxy=0d
  while not eof(lun) do begin
    readf, lun, x, a, y, b
    n = n+1
    sumx  = sumx  + x
    sumy  = sumy  + y
    sumx2 = sumx2 + x^2.
    sumy2 = sumy2 + y^2.
    sumxy = sumxy + x*y
    if n mod 2000000 eq 0 then print, n, (sumxy - sumx*sumy/n)/sqrt(sumx2 - sumx^2./n)/sqrt(sumy2 - sumy^2./n)
  endwhile
  free_lun, lun
  print, (sumxy - sumx*sumy/n)/sqrt(sumx2 - sumx^2./n)/sqrt(sumy2 - sumy^2./n)
  stop
   
end
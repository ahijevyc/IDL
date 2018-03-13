function votes2prob, votes
  n = n_elements(votes)
  prob = fltarr(n)
  for i =0, n-1 do begin
  if      votes[i] eq 0   then prob[i]=0 $
  else if votes[i] le 40  then prob[i]=0.0005+(votes[i]-  1)*(0.02 - 0.0005)/39 $
  else if votes[i] le 180 then prob[i]=0.02  +(votes[i]- 40)*(0.7  - 0.02  )/140 $
  else if votes[i] le 200 then prob[i]=0.7   +(votes[i]-180)*(0.81 - 0.7   )/20 $
  else stop
  endfor
  
  
  return, prob


end
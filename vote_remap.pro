pro vote_remap

  ; used for RunForest
  ;
  ;
  ;
  
  nTree = 200.
  nForest = 1
  votes = findgen(nTree+1)

  steepness = 4 ; 10
  shift_right = 1.1 ; 0
  prob = 1.5*(atan(-shift_right -steepness/2+steepness*votes/nTree))/!DPI+.6

  iz = where(prob lt 0, /null)
  if iz ne !NULL then prob[iz] = 0
  iz = where(prob gt 1, /null)
  if iz ne !NULL then prob[iz] = 1

  plot, votes, prob, ticklen=0.5, ytickinterval=0.1
  
  prob = trim_more(string(prob, format='(F5.3)'), /leading_zero)+','
  
  for iForest = 0, nForest-1 do begin
    print, strjoin(prob)
  endfor
  
  print, strjoin(replicate('-999',nForest),',')
  
  
  
end
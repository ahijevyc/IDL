function mycurvefit, xin, yin, locations=locations, std=std, clustern=clustern

  ; Given a scatterplot of data that is sorted in the x dimension
  ; return an average of the points in n-element clusters.
  ; used to blend MTP profiles with different vertical arrays. 

  ; INPUT
  ; xin - sorted x values
  ; yin - matching y values
  ; OUTPUT
  ; locations - mean locations of clusters
  ; yout - corresponding y values
  ; std - standard deviation of y at each location

  x=xin ; input should be sorted
  if array_equal(x, xin) ne 1 then stop
  y=yin
  
  ; blend 10 points at a time by default
  if ~keyword_set(clustern) then clustern = 10
  nout = ceil(n_elements(x)/clustern)
  
  locations = replicate(!VALUES.D_NAN, nout)
  yout = replicate(!VALUES.D_NAN, nout)
  std = replicate(!VALUES.D_NAN, nout)

  ; get mean of 10 point and standard deviation
  for i=0,nout-1 do begin
    imax = min( [ (i+1)*clustern-1, n_elements(x)-1] )
    locations[i] = mean(x[i*clustern:imax])
    yout[i] = mean(y[i*clustern:imax])
    std[i] = stddev(y[i*clustern:imax])
  endfor

  return, yout
end
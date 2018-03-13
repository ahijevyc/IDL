pro prob_matching, array, pdf

  ; Probability matching algorithm
  ; Can be used on an ensemble mean to give it the same distribution of intensities as its member components.
  ; In this case, the pdf array is the histogram of intensities for all the ensemble members. 
  ; One could also use it on a probabilistic forecast from the random forest to give it similar appearance to a deterministic forecast. 
  ; At least the distribution of intensities would be identical to the deterministic forecast.
  ; In this case, pdf is the deterministic forecast array. 
  
  ;
  ; array - array to change. It will have the same spatial structure, but an intensity distribution that matches pdf. 
  ; pdf  - array with the desired intensity distribution
;     array[sort(array)] = pdf[sort(pdf)]
     sarr = sort(array)
     iarr = uniq(array[sarr])

  ; uncomment to "randomize the position of the elements that are equal to each other
;     for i = 0, n_elements(iarr)-1 do begin
;      mini = i eq 0 ? 0 : iarr[i-1]
;      maxi = iarr[i]
;      x = lindgen(maxi-mini+1)
;      y = randomu(dseed, maxi-mini+1)
;      z = x[sort(y)]
;      sarr[mini:maxi] = (sarr[mini:maxi])[z] 
; 
;     endfor 
     array[sarr] = pdf[sort(pdf)]
;     vars = fltarr(3,n_elements(array))
;     vars[0,*] = lindgen(n_elements(array)) mod 601
;     vars[1,*] = lindgen(n_elements(array)) mod 501
;     vars[2,*] = 30*array[*]
;     weights= clust_wts(vars, /double, n_clusters=5, n_iterations=10)
;     array[*] = 10*cluster(vars, weights, /double)
;     s = shift(dist(2), 2, 2) 
;     array = morph_open(array, s)*array
end
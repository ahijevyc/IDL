function get_histogram_cutoff, x_in, h_in
  h = h_in
  x = x_in
  mx_h = max(h,imax_h)
  izero = where(h eq 0, nzero)
  if nzero eq 0 then stop
  izero_gt_imax_h = izero[where(izero ge imax_h)]
  ibin = min(izero_gt_imax_h)
  
  
  cutoff = x[ibin]
  ;  plots, replicate(cutoff,2), !Y.CRANGE, thick=8, linestyle=2
  return, cutoff
  
  
end
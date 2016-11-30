
; functionality moved to read_maxima_info

;function get_mcv_specs, feature_info_file_search_string



  ; return specs from track info file
  ;
  ;
  
  specs = {min_intensity:!VALUES.D_NAN, min_size:!VALUES.D_NAN, max_size:!VALUES.D_NAN, nbins:0L, bin_spacing:!VALUES.D_NAN, $
    max_std:!VALUES.D_NAN, intensity_dropoff_thresh:!VALUES.D_NAN}
    
  files2check = file_search(feature_info_file_search_string, count=nfiles)
  for ifile=0,nfiles-1 do begin
    file = files2check[ifile]
    line = ''
    openr, info, file, /get_lun
    header = 1
    while header eq 1 do begin
      readf, info, line
      case (1) of
        strpos(line, 'minimum vorticity'): specs.min_intensity = strmid(line, strpos(line,'=')+1)
        strpos(line, 'number of bins, bin '): begin ; used to search for "bin spacing" but now "bin width" is output from find_maxima
          words = strsplit(strmid(line, strpos(line,'=')+1), /extract)
          specs.nbins = words[0]
          specs.bin_spacing = words[1]
        end
        strpos(line, 'min,max radii'): begin
          words = strsplit(strmid(line, strpos(line,'=')+1), /extract)
          specs.min_size = words[0]
          specs.max_size = words[1]
        end
        strpos(line, 'maximum standard deviation of values in bin as percentage of central value'): begin
          readf, info, line
          specs.max_std = line
        end
        strpos(line, 'mean value of points in bin must drop to this fraction of central value'): begin
          readf, info, line
          specs.intensity_dropoff_thresh = line
        end
        strmid(line, 0, 5) eq ' MCV(' : header=0
        else:
      endcase
      if ifile gt 0 then begin
        if specs.min_intensity ne old_min_intensity then stop
        if specs.min_size ne old_min_size then stop
        if specs.max_size ne old_max_size then stop
        ;if specs.nbins ne old_nbins then stop ; not crucial
        if specs.bin_spacing ne old_bin_spacing then stop
        if specs.max_std ne old_max_std then stop
        if specs.intensity_dropoff_thresh ne old_intensity_dropoff_thresh then stop
      endif
      old_min_intensity = specs.min_intensity
      old_min_size = specs.min_size
      old_max_size = specs.max_size
      old_nbins = specs.nbins
      old_bin_spacing = specs.bin_spacing
      old_max_std = specs.max_std
      old_intensity_dropoff_thresh = specs.intensity_dropoff_thresh
    endwhile
    free_lun, info
  endfor
  
  return, specs
end
function read_maxima_info_txt, feature_info_file_search_string, specs=specs

  ; kind of built off of old feature_specs routine, which took a search string
  ; pointing to *_maxima_info.txt files produced by find_maxima.f90.
  ; It not only verifies the search criteria are the same for all files; it also
  ; returns all the feature (MCV) data.  This used to be done with a spawn, grep, cut command and read_ascii.
  
  ; initialize everything. 
  x = !NULL
  y = !NULL
  intensity = !NULL
  size = !NULL
  lat = !NULL
  lon = !NULL
  time = !NULL
  nbin = !NULL
  num_pts = list()
  avg_dist = LIST()
  mean = LIST()
  stddev = liST()
  
  specs = {min_intensity:!VALUES.D_NAN, min_size:!VALUES.D_NAN, max_size:!VALUES.D_NAN, nbins:0L, bin_spacing:!VALUES.D_NAN, $
    max_std:!VALUES.D_NAN, intensity_dropoff_thresh:!VALUES.D_NAN}
    
  maxima_info_files = file_search(feature_info_file_search_string, count=nfiles)
  
  if nfiles eq 0 then begin
    print, "no files found. search_string ", feature_info_file_search_string
    stop
  endif
  for ifile=0,nfiles-1 do begin
    file = maxima_info_files[ifile]
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
        ; important to have zeroes in there for hours (or else 12 is used)
        strpos(line, 'days since 1970'): jday = julday(1,1,1970,0,0,0)+double(strmid(line, strpos(line,'=')+1))
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
    
    
    ; Now that we are past the header.......
    while strpos(line, 'MCV(') ne -1 do begin
      ; we already have line with first 'MCV' in it from reading the header
      p1 = strpos(line,'(')
      c1 = strpos(line,',')
      p2 = strpos(line,')')
      x = [x, long(strmid(line, p1+1, c1-p1))]
      y = [y, long(strmid(line, c1+1, p2-c1))]
      rest = strsplit(strmid(line, p2+1), /extract)
      intensity = [intensity, double(rest[0])]
      size = [size, double(rest[1])]
      lat = [lat, double(rest[2])]
      lon = [lon, double(rest[3])]
      readf, info, line
      if strpos(line, 'bin =') eq -1 then stop
      words = strsplit(line, /extract)
      nbin = [nbin, long(words[-1])]
      readf, info, line
      if strpos(line, 'num of pts =') eq -1 then stop
      num_pts.add, long(strsplit(strmid(line, strpos(line,'=')+1), /extract))
      readf, info, line
      if strpos(line, 'avg dist =') eq -1 then stop
      avg_dist.add, double(strsplit(strmid(line, strpos(line,'=')+1), /extract))
      readf, info, line
      if strpos(line, 'mean =') eq -1 then stop
      mean.add, double(strsplit(strmid(line, strpos(line,'=')+1), /extract))
      readf, info, line
      if strpos(line, 'std deviat =') eq -1 then stop
      stddev.add, double(strsplit(strmid(line, strpos(line,'=')+1), /extract))
      readf, info, line ; blank line
      readf, info, line ; next MCV line
      if jday eq !NULL then begin
        print, 'did you run_find_maxima on this set yet? the latest find_maxima outputs days since 1970'
        stop
      endif
      time = [time, jday]
    endwhile
    
    free_lun, info
  endfor
  
  ; Return extra information that ain't used by tracking.pro, like x, y, size, nbin, num_pts, avg_dist, mean, and stddev.
  data = {x:x, y:y, intensity:intensity, size:size, lat:lat, lon:lon, time:time, nbin:nbin,num_pts:num_pts, $
    avg_dist:avg_dist, mean:mean, stddev:stddev, search_string:feature_info_file_search_string, files:maxima_info_files }
  return, data
end
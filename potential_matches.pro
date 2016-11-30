function potential_matches, inmodel, start_date, end_date, storm_name=storm_name, fh=fh, spd=spd, basin=basin, verbose=verbose, $
  tracker=tracker, smoothing_filter=smoothing_filter, min_duration_days=min_duration_days,lun=lun
  
  ; counts potential matches between model forecasts and best tracks using fort.15 
  ; (in the case of gfdl_tracking).
  
  model = inmodel ; this is a string name
  if ~keyword_set(storm_name) then storm_name = ''
  if n_elements(fh) eq 0 then fh=0
  ; skip if the observed best track is below the wind threshold, spd
  if ~keyword_set(spd) then spd=0 ; in knots
  if ~keyword_set(basin) then basin=''
  if ~keyword_set(verbose) then verbose=0
  if ~keyword_set(tracker) then tracker='gfdl'
  if ~keyword_set(smoothing_filter) then smoothing_filter='025'
  if ~keyword_set(min_duration_days) then min_duration_days = 0d
  model_max_vmax_thresh_kt = 0 ; defined in find_matching_model_track.pro (as vmax_thresh_kt)
  if n_elements(lun) eq 0 then lun = -1


  dxdetails = string(0.5, smoothing_filter,format='("_",F5.3,"deg_",I3.3,"km")')


  valid_times = !NULL
  lead_times_h = !NULL
  init_times = !NULL
  last_times = !NULL

  if strmatch(model,'GFS*') then subpath='' else subpath='/latlon'+dxdetails
  if tracker eq 'gfdl' then subpath=subpath+'/gfdl_tracker'

  start_jday = julday(strmid(start_date, 4, 2), strmid(start_date, 6, 2), strmid(start_date,0,4), strmid(start_date,8,2), 0, 0)
  finish_jday = julday(strmid(end_date, 4, 2), strmid(end_date, 6, 2), strmid(end_date,0,4), strmid(end_date,8,2), 0, 0)

  ; find all the forecasts for this model
  ; look through 'scratch/ahijevyc/'+model+'/2*/'
  dday = strmatch(model, 'GFS*') ? 0.25d : 1d
  for init_jday = start_jday, finish_jday, dday do begin
    datedir = string(init_jday, format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
    if verbose ge 3 then print, "looking for date dir "+datedir
    dir = (strmatch(model,'GFS*') ? '/glade/scratch/ahijevyc/' : '/glade/p/nmmm0024/' ) +model+'/'+datedir+subpath
    if file_test(dir) ne 1 then begin
      print, 'no date directory  '+dir
      continue
    endif

    if tracker eq 'gfdl' then begin
      if file_test(dir+'/fort.66') ne 1 then begin
        print, 'no fort.66 in '+dir
        stop
      endif
      ; read GFDL tracker lead times from fort.15
      t = read_ascii(dir+'/fort.15', count=ntimes)
      ; use reform -= if u allow it to be 2-D the concatenated array is 2-D.  Then array indicies don't
      ; correspond well to init_times and last_times arrays.
      leadtimefort15 = reform(t.field1[1,*]/60)
      lead_times_h = [lead_times_h, leadtimefort15]
      valid_times = [valid_times, init_jday+leadtimefort15/24d] ; double precision for juldays
      last_time = init_jday + leadtimefort15[-1]/24d
      if verbose ge 2 then print, 'found ',strtrim(ntimes,2),' lead times in ',dir,' fort15:', leadtimefort15
    endif else begin
      ; File names can't be added willy-nilly without regard to their date directory.
      ; The date directory may just have a few random times in it; not the full suite needed for tracks.
      ; Before adding a bunch of files to the list, inspect the date directory to make sure
      ; it has a tracking.sav file, 3-hourly stuff, and not just daily, or just a single init time.
      files = file_search(dir+'/*_maxima_info.txt', count=nfiles)
      if nfiles lt 30 then begin
        if verbose then print, dir+' has only ', nfiles, ' *_maxima_info files. Skipping. '
        continue
      endif
      track_savfile = file_search(dir+'/vorticity_*tracks.sav', count=junk)
      if junk eq 0 then stop ; sanity check . make sure there's an IDL tracks save file.
      for ifile = 0, nfiles-1 do begin
        ; catalog the forecasts' lead times and valid times
        file = files[ifile]
        valid_time = mpas_diagnostics_jday(file)
        valid_times = [valid_times, valid_time]
        lead_times_h  = [lead_times_h, 24d*(valid_time - init_jday)]
      endfor
      last_time = valid_times[-1]
    endelse
    init_times = [init_times, replicate(init_jday, ntimes)]
    last_times = [last_times, replicate(last_time, ntimes)]

  endfor



  ; loop through bdecks (observed best tracks)
  ; count model valid times that match bdeck times (filtered by forecast hour, storm, speed, and basin)
  ;
  ntotal=0L
  files = file_search('/glade/p/work/ahijevyc/atcf/b??[0-6][0-9]????.dat', count=nfiles)
  if verbose ge 3 then print, 'found ',strtrim(nfiles,2),' BDECKs:', files
  for ifile = 0, nfiles-1 do begin
    file = files[ifile]
    ;    savfile = file + '.sav'
    ;    if file_test(savfile) then restore, savfile else begin
    st = read_atcf(file)
    ;      save, st, filename=savfile
    ;    endelse
    if st eq !NULL then begin
      print, 'no ',file,'? check http://ftp.nhc.noaa.gov/atcf/NOTICE for notices about Best track problems and updates'
      stop
    endif
    tc_track_jdays = st.julday
    b1 = min(tc_track_jdays)
    b2 = max(tc_track_jdays)

    bspd = st.vmax
    ; Grab storm name from line with greatest wind.
    junk=max(st.vmax,imax) & stormname = (st.stormname)[imax]
    if strmatch(stormname, '*'+storm_name+'*',/fold) eq 0 then begin
      if verbose ge 3 then print, file, ' no match for '+storm_name+' in '+stormname
      continue
    endif
    if strmatch(st.basin[0], '*'+basin+'*',/fold) eq 0 then begin
      if verbose ge 3 then print, file, ' not '+basin+': '+st.basin[0]
      continue
    endif

    ;overlapdays = min(last_times,b2) - max(init_times,b1)
    overlapdays = (last_times lt b2)*last_times + (last_times ge b2)*b2 - ( (init_times gt b1)*init_times + (init_times le b1)*b1 )
    if verbose ge 3 then print, file, ' overlap days:',overlapdays

    ; loop through BDECK track times
    for itime=0,n_elements(tc_track_jdays)-1 do begin
      bdeck_time = tc_track_jdays[itime]
      if bdeck_time eq tc_track_jdays[itime-1] then begin
        if verbose ge 1 then print, 'repeat time: ', string(bdeck_time,format='(c())')
        continue; skip repeated times. repeated times occur when multiple wind thresholds are used for max radius of wind
      endif
      ; skip if the observed best track is below the wind threshold
      if bspd[itime] lt spd then begin
        if verbose ge 1 then print, 'failed speed threshold test at ', string(bdeck_time,format='(c())')
        continue
      endif
      if bdeck_time lt min(valid_times) || bdeck_time gt max(valid_times) then begin
        if verbose ge 1 then print, 'outside valid time window at ', string(bdeck_time,format='(c())')
        continue
      endif
      if verbose ge 3 then print, file, ' made it so far ', string(bdeck_time,format='(c())')
      iforecasts = where(valid_times eq bdeck_time and lead_times_h eq fh and overlapdays ge min_duration_days, nforecasts, /null)
      ntotal = ntotal+min([nforecasts,1]) ; tempting to add nforecasts, but could have 2 or three matches for multiple wind thresholds.
      if iforecasts eq !NULL then begin
        if verbose ge 1 && where(valid_times eq bdeck_time) eq !NULL then print, 'no matching valid times at ', string(bdeck_time,format='(c())')
        if verbose ge 1 && where(valid_times eq bdeck_time and lead_times_h eq fh) eq !NULL then print, 'no matching valid times with requested lead time at ', string(bdeck_time,format='(c())')
        if verbose ge 1 then begin
          print, 'no matching valid times with requested lead time and minimum duration at ', string(bdeck_time,format='(c())')
        endif
      endif else begin
        output = string(stormname,model,init_times[iforecasts[0]],valid_times[iforecasts[0]], $
          format='(A-11,A-8,c(CYI,CMOI2.2,CDI2.2,CHI2.2),x,c(CYI,CMOI2.2,CDI2.2,CHI2.2))') $
          +' f'+string(lead_times_h[iforecasts[0]], bspd[itime], format='(I3.3,I4,"kt")')
        if verbose ge 1 then print, output
      endelse
      if lun ne -1 then begin
        ; classify as hit or miss. Look in adeck files for matching line.
        if nforecasts gt 0 then begin
          basedir = '/glade/p/work/ahijevyc/tracking_'+tracker+'/adeck/'+model + $
            string(model_max_vmax_thresh_kt,format='("/vmax_thresh_kt.ge.",I2.2)')
          adeck = file_search(basedir+'/a'+strlowcase(st.basin[0])+st.cy[0]+st.yyyy[0]+dxdetails+'_'+tracker+'*_minimum')
          if adeck[0] eq '' then begin
            print, 'couldnt find adeck file'
            stop
          endif
          found_it = 'miss'
          if file_test(adeck, /zero) then continue ; skip if zero size
          t = read_atcf(adeck) ; atcf in knots and nautical miles
          ii = where(t.julday eq bdeck_time and t.tau eq fh,nii)
          if nii gt 0 then found_it = 'hit'
          output = string(st.basin[itime],stormname,model,init_times[iforecasts[0]],valid_times[iforecasts[0]], $
            format='(A2,X,A-11,A-8,c(CYI,CMOI2.2,CDI2.2,CHI2.2),x,c(CYI,CMOI2.2,CDI2.2,CHI2.2))') $
            +' f'+string(lead_times_h[iforecasts[0]], bspd[itime], found_it, st.lon[itime], st.lat[itime], $
            format='(I3.3,I4,"kt ",A4,F8.2,F7.2)')
          if verbose ge 1 then print, output
          printf, lun, output
          flush, lun

        endif
      endif

    endfor; loop thru model track times
  endfor; loop thru bdeck files

  return, ntotal

end


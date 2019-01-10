pro run_find_matching_model_track_old, smooth_radius=smooth_radius, debug=debug, force_new=force_new, model=model, basin=basin, $
  trackfield=trackfield, vmax_thresh_kt=vmax_thresh_kt
  ; Create ADECK files for all the BDECK files in a year.  Can be restricted to a particular basin or done globally
  ; by setting basin to '*'.
  ; BDECK files are best tracks for each tropical storm. I keep them in /glade/work/ahijevyc/atcf/b*.dat
  ; An ADECK file contains the model tracks that match a particular BDECK system. They can be lumped together all in the same file
  ; or done separately for each model.  This program does it separately for each model and outputs them to /glade/work/ahijevyc/tracking_<trackfield>/<modelname>/.
  ; This program used to be included in the IDL file get_vitals.pro.
  if ~keyword_set(debug) then debug=0
  ; If force_new=1, then any existing output files will be overwritten. If force_new=0 then the output file will
  ; not be overwritten.
  if ~keyword_set(force_new) then force_new=0
  if ~keyword_set(basin) then basin = '*'
  if ~keyword_set(trackfield) then trackfield = 'gfdl'
  year = '2014'
  ; BDECK identification numbers must be between 0 and 69.  70 and above are not standard tropical systems or are tracked differently.
  files = file_search('/glade/work/ahijevyc/atcf/b'+basin+'[0-6][0-9]'+year+'.dat', count=nfiles, /fold_case)
  if ~keyword_set(model) then begin
    if year eq '2014' then models = ['GFS', 'mpas', 'mpas_ep']
    if year eq '2015' then models = ['ep', 'wp', 'al', 'GFS']
  endif else models=[model]
  if n_elements(vmax_thresh_kt) eq 0 then vmax_thresh_kt = 34.
  ; Loop through each model and loop through each BDECK.
  for m=0,n_elements(models)-1 do begin
    model = mpas_mesh(models[m])
    origmesh=model.name eq 'GFS'?0:1
    openw, miss_lun, '/glade/work/ahijevyc/tracking_gfdl/miss_'+model.name+'_'+idl_validname(strupcase(basin))+$
      '_origmesh' + (origmesh eq 1 ? 'True' : 'False') + $
      string(vmax_thresh_kt,format='("_",I2,"kt")')+'.txt', /get_lun
    for ifile = 0, nfiles-1 do find_matching_model_track_old, files[ifile], model=model, $
      smooth_radius=smooth_radius, debug=debug, force_new=force_new, trackfield=trackfield, $
      miss_lun=miss_lun, vmax_thresh_kt=vmax_thresh_kt, origmesh=origmesh
    if n_elements(miss_lun) ne 0 && (fstat(miss_lun)).open ne 0 then free_lun, miss_lun
    
  endfor
  
end

function expand_old, x
  dim = size(x, /dim)
  ntracks = dim[0]
  ntimes = dim[1]
  ; one extra time on end, set to NaN
  y = replicate(!VALUES.D_NAN, [ntracks,ntimes+1])
  for itrack=0,ntracks-1 do y[itrack,0:ntimes-1] = x[itrack,*]
  return, y
end

function get_time_distance_thresh_old, forecast_hour_in
  ; Values for E(t) are taken from the following plot, using the 95th percentile error of the NHC $
  ; forecasts at various lead times: http://www.nhc.noaa.gov/verification/figs/5yr_ALtkerrdist.jpg
  forecast_hour = round(forecast_hour_in)
  f = [   0,  24,  48,  72,  96, 120,  144,  240]
  d = [ 200, 200, 350, 510, 680, 860, 1000, 1000]
  ; Allow separation to grow beyond 1000 km.
  ;  f = [   0,  24,  48,  72,  96, 120,  144,  168,  192,  216,  240]
  ;  d = [ 200, 200, 350, 510, 680, 860, 1050, 1250, 1460, 1680, 1910]
  if forecast_hour lt f[0] || forecast_hour gt f[-1] then stop
  return, interpol(d, f, forecast_hour)
  
end



pro find_matching_model_track_old, bdeck_file, model=model, trackfield=trackfield, $
  smooth_radius=smooth_radius, force_new=force_new, debug=debug, miss_lun=miss_lun, $
  vmax_thresh_kt=vmax_thresh_kt, origmesh=origmesh
  
  ; for ONE observed best track (BDECK), find all model runs that overlap the time window of the BDECK system.
  ; Find the closest vortex track in each model run and determine if it is close enough to match.
  ; Write matches in ATCF format to an ADECK file that is saved in /glade/work/ahijevyc/tracking_gfdl/<modelname>/.
  atmos_const
  if ~keyword_set(debug) then debug = 0
  min_duration_days = 1.d  ; minimum duration of model model_track to consider  (unless observation is on boundary of model time window)
  must_reach_intensity = 5e-5
  if ~keyword_set(force_new) then force_new = 0 ; force_new=1 forces atcf file and vitals save file to be remade.
  if n_elements(bdeck_file) eq 0 then bdeck_file = '/glade/work/ahijevyc/atcf/bep142014.dat'
  if ~keyword_set(model) then model = mpas_mesh('mpas15_3') else if isa(model,/scalar) then model=mpas_mesh(model)
  total_model_days = strmatch(model.name, 'GFS*') ? 8d : 10d
  if (model.name eq 'mpas15_3') then total_model_days = 3d
  dist_threshold = 1000*1000L ; distance threshold in meters ; in meters (use long integer)
  if ~keyword_set(trackfield) then trackfield = 'gfdl'
  if ~keyword_set(smooth_radius) then smooth_radius='025'
  grid_dx = 0.5
  dxdetails = string(grid_dx,smooth_radius,format='("_",F5.3,"deg_",I3.3,"km")')
  if n_elements(miss_lun) eq 0 then miss_lun = -1
  if n_elements(vmax_thresh_kt) eq 0 then vmax_thresh_kt = 64
  start_new_atcf_output_file=1
  
  ; Read observed best track file, whether it is ATCF or UNISYS
  atcf=strmatch(file_basename(bdeck_file),'[ab]??[0-9][0-9]20??.dat')
  if atcf then begin
  
    if file_test(bdeck_file, /zero) eq 1 then return
    year = strmid(file_basename(bdeck_file),5,4)
    obs = read_atcf(bdeck_file)
    ; if BDECK, use 'BEST', otherwise use 'CARQ', unless it's w. pacific--then use 'WRNG'
    if strmid(file_basename(bdeck_file),0,1) eq 'b' then tech = 'BEST' else if strpos(file_basename(bdeck_file), 'wp') ne -1 then tech = 'WRNG' else tech = 'CARQ'
    iobs = where(obs.TECH eq tech and obs.TAU eq 0, nobs)
    if nobs eq 0 then stop
    junk=max(obs.vmax,imax)
    stormname = (obs.stormname)[imax]
  endif else begin
    bdeck_file = file_search('/glade/work/ahijevyc/hurricane/*/20*/'+bdeck_file, count=count)
    if count ne 1 then stop
    obs = read_unisys_best_track(bdeck_file, header=header)
    iobs = where(finite(obs.valid_time), /null)
    stormname = file_basename(bdeck_file,"_track.dat")
    bdeck_file = unisys2atcf_filename(bdeck_file)
  endelse
  
  ; print_atcf will use characters in columns 2-3 of file_basename for basin name
  my_atcf_file = '/glade/work/ahijevyc/tracking_'+trackfield+'/adeck/'+ model.name + $
    string(vmax_thresh_kt,format='("/vmax_thresh_kt.ge.",I2.2)') + $
    '/a' + strmid(file_basename(bdeck_file),1) + dxdetails +'_'+trackfield+ $
    '_origmesh' + (origmesh?'True':'False') + string(min_duration_days, format='("_",F3.1,"d_minimum")')
    
  if file_test(my_atcf_file) && file_test(my_atcf_file, /zero_length) eq 0 && force_new eq 0 then begin
    print, "found ", my_atcf_file, ". skipping. "
    return
  endif
  
  
  observed_times = obs.valid_time[iobs]
  obs_first = min(observed_times)
  obs_last  = max(observed_times)
  observed_lons = obs.lon[iobs]
  observed_lats = obs.lat[iobs]
  observed_vmax = obs.vmax[iobs]
  
  print, stormname, obs_first, observed_lons[0], observed_lats[0], obs_last, format='(A,1x,C(),F7.1,"E",F6.1,"N ",C())'
  if debug ge 1 then print, string(observed_times, format='(C())') + string(observed_lons, format='(F7.1,"E")') + $
    string(observed_lats, format='(F6.1,"N")')
    
  ; find the first time the storm reaches TS strength (34 kt)
  i_TS_strength = where(obs.vmax[iobs] ge 34, /null) ; obs (atcf format) is in knots
  ; if it never reaches TS strength use time=infinity
  TS_first_time = i_TS_strength ne !NULL ? observed_times[min(i_TS_strength)] : !VALUES.D_INFINITY
  
  ; Find the model tracks files for overlapping model runs for this particular model type, tracker algorithm, and smoothing radius.
  if trackfield eq 'gfdl' then begin
    model_tracks_files = file_search(['/glade/scratch/ahijevyc/','/glade/scratch/mpasrt/','/glade/p/nmmm0024/']+model.name+'/'+year+'[01]?[0-3]?[012]?/'+(strmatch(model.name,'GFS*')?'':'latlon'+dxdetails)+$
      '/gfdl_tracker/fort.66', count=nmodel_tracks_files)
  endif else $ ; branch for custom find_maxima tracker results
    model_tracks_files = file_search('/glade/scratch/ahijevyc/'+model.name+'/'+year+'[01]?[0-3]?[012]?/latlon'+dxetails+'/'+trackfield+'_'+smooth_radius+$
    '_tracks.sav', count=nmodel_tracks_files)
  if nmodel_tracks_files eq 0 then stop
  ; extract the yyyymmddhh substrings from the directory paths of the model tracks files.
  ; These are model initialization times
  i201 = strpos(model_tracks_files,'/201')
  ; if total(i201-shift(i201,1)) ne 0 then stop ; sanity check ; make sure all elements are equal. After adding "reform" don't need them to be equal.
  model_tracks_files_date_dirs = strmid(model_tracks_files, reform(i201+1,1,nmodel_tracks_files), 10)
  ; Convert to JULDAYs
  model_tracks_file_juldays = julday(strmid(model_tracks_files_date_dirs, 4, 2), strmid(model_tracks_files_date_dirs, 6, 2), $
    strmid(model_tracks_files_date_dirs, 0, 4), strmid(model_tracks_files_date_dirs, 8, 2), 0, 0)
  ; Find all the model runs overlapping the time window of the observed best track.
  ; Used to have model_tracks_file_juldays lt obs_last, but that elimates the model run that starts at the same
  ; time as the last observed time. Might as well keep it because we keep the model run if it ends at the same
  ; time as the first observed time. (changed Apr 7, 2015)
  imatch = where(model_tracks_file_juldays ge (obs_first-total_model_days) and model_tracks_file_juldays le obs_last, /null)
  model_tracks_files = model_tracks_files[imatch]
  model_tracks_file_juldays = model_tracks_file_juldays[imatch]
  if n_elements(model_tracks_files) eq 0 then begin
    print, "no potential "+model.name+" model tracks files found"
    return
  endif
  
  ; Loop through the model tracks files that overlap the observed system.
  for iinit = 0, n_elements(model_tracks_files)-1 do begin
    init_time = model_tracks_file_juldays[iinit]
    init_date = string(init_time, format='(C(CYI4.4, CMoI2.2, CDI2.2, CHI2.2))')
    if trackfield eq 'gfdl' then begin
      gfdl_m = read_atcf(model_tracks_files[iinit], lats=mcv_lats, lons=mcv_lons, times=mcv_times, $
        intensity=mcv_intensity, id=mcv_id)
      specs = {tracker:'gfdl'}
    endif else restore, model_tracks_files[iinit] ; branch for custom tracker find_maxima.F
    if not isa(model,'Anonymous') then stop; make sure model is a structure.
    
    matching_model_tracks = LIST()
    ; Loop through each model_track in the model tracks file
    final_track = max(where(finite(mcv_times[*,0]), /null))
    if debug ge 1 then print, model_tracks_files[iinit], ' found ', strtrim(final_track+1,2),' ',model.name,' tracks'
    for itrack = 0, final_track do begin
      early_distance_match_time=0d
      
      ; Track must reach a certain intensity at some point in its lifecycle.
      ; GFDL track intensity is Infinity so this has no effect on GFDL tracks.
      if max(mcv_intensity[itrack,*], /nan) lt must_reach_intensity then begin
        if debug gt 0 then print, 'track '+mcv_id[itrack]+ ' never reaches ', must_reach_intensity, ' intensity. Skipping'
        continue
      endif
      
      ; I moved this outside the itime-observed time loop Thr Feb 20. Don't know why it was in there.
      first_time = min(mcv_times[itrack,*], max=last_time, /nan)
      
      ; Skip this track if a) the previously matching track hasn't ended yet and
      ; b) it starts after the previously matching track's early_distance_match_time.
      ; If previous track hasn't ended yet but this one starts before the previous track's
      ;  early_distance_match time, then don't skip.
      ; See mpas_al 20130827 Gabrielle #29. It's a poor match compared to #35 and #556.
      if matching_model_tracks.count() gt 0 && first_time le matching_model_tracks[-1].valid_time[-1] && $
        first_time gt matching_model_tracks[-1].early_distance_match_time then continue
        
      ; if this model_track segment doesn't last at least min_duration_days then skip it.
      ; Without this you get poor matches that overlap better matches for TORAJI for 2013 Aug 27 GFS.
      ; Allow for smaller minimum duration if track is on a boundary of the model time window.
      ; Would like to allow for smaller minimum duration if storm moves off grid, like AL082014 Gonzalo in
      ; 20141019 run.   It is tracked in 0-6 hours, but goes off grid at f012. Can't be matched here
      ; yet still shows up as potential match because best track goes to 20141019 18Z.
      tmp_min_duration = min([min_duration_days, observed_times[-1]-init_time, init_time+total_model_days -observed_times[0]])
      if last_time - first_time lt tmp_min_duration then begin
        if debug ge 2 then print, 'track '+mcv_id[itrack]+' does not last ', string(tmp_min_duration,format='(F5.2)'),' days. Skipping'
        continue
      endif
      
      for itime=0, n_elements(observed_times)-1 do begin
        observed_time = observed_times[itime]
        ; if this observed time is outside the time range of this model_track, then skip it.
        ; In the future, one might keep track of how many matches are possible with the nmatchmax variable and assign distances
        ; even if the model_track doesn't exist at this time, like the max search distance (1000km).  In that case
        ; "first_time, last_time" will need to be changed to "init_time, init_time+total_model_days".
        if observed_time lt first_time || observed_time gt last_time then begin
          if debug ge 2 then print, stormname+' time '+string(observed_time,format='(c())')+' outside model track '+ $
            mcv_id[itrack]+' time window. Trying next '+stormname+' time'
          continue
        endif
        
        ; Does this model model_track exist at this observed time?
        imatch = where(abs(mcv_times[itrack,*] - observed_time) lt 1/24d, nmatch, /null)
        if nmatch gt 1 then stop
        ; If the observed time is within the model_track time window, but doesn't match
        ; a model_track time exactly, then interpolate the model_track lat/lon/intensity to the oberved time and
        ; insert the interpolated values in the mcv_lats,mcv_lons,mcv_itensity,mcv_times arrays
        ; I could use great circle interpolation but for small distances, it shouldn't matter.
        if imatch eq !NULL && observed_time ge first_time && observed_time le last_time then begin ; interpolate to observed time
          imatch = 1+max(where(mcv_times[itrack,*] le observed_time))
          newlon       = interpol(     mcv_lons[itrack,imatch-1:imatch], mcv_times[itrack,imatch-1:imatch], observed_time)
          newlat       = interpol(     mcv_lats[itrack,imatch-1:imatch], mcv_times[itrack,imatch-1:imatch], observed_time)
          newintensity = interpol(mcv_intensity[itrack,imatch-1:imatch], mcv_times[itrack,imatch-1:imatch], observed_time)
          if finite(mcv_lons[itrack,-1]) then begin
            ; expand time dimension by 1 to allow for new point.
            ; last element should be unfilled
            mcv_lons = expand_old(mcv_lons)
            mcv_lats = expand_old(mcv_lats)
            mcv_times = expand_old(mcv_times)
            mcv_intensity = expand_old(mcv_intensity)
          endif
          mcv_lons[itrack,imatch:*]      = shift(     mcv_lons[itrack,imatch:*],1)
          mcv_lats[itrack,imatch:*]      = shift(     mcv_lats[itrack,imatch:*],1)
          mcv_intensity[itrack,imatch:*] = shift(mcv_intensity[itrack,imatch:*],1)
          mcv_times[itrack,imatch:*]     = shift(    mcv_times[itrack,imatch:*],1)
          mcv_lons[itrack,imatch]      =        newlon
          mcv_lats[itrack,imatch]      =        newlat
          mcv_intensity[itrack,imatch] =  newintensity
          mcv_times[itrack,imatch]     = observed_time
        endif
        
        displacement = map_2points(observed_lons[itime], observed_lats[itime], mcv_lons[itrack, imatch], mcv_lats[itrack, imatch], /meters)
        ; if this is within the first day of the observed or model track, see if track is close enough to match
        ; or if the observed track hasn't reached TS strength yet, see if track is close enough to match
        if mcv_times[itrack,imatch] - TS_first_time le 0d || mcv_times[itrack,imatch] - first_time le 1d then begin
          ; could wait a day after 1st TS strength, but I try 0.
          forecast_hour = 24d *(observed_time - init_time)
          if displacement/1000. lt get_time_distance_thresh_old(forecast_hour) then begin
            ; keep track of the earliest early_distance_match_time
            early_distance_match_time = observed_time
            if debug ge 1 then print, stormname, mcv_id[itrack], forecast_hour, displacement/1000., format='(A," - track ",A," matches at f",i3.3,". Displacement ",F4.0, "km")'
            break ; this track MATCHES! no need to keep looping through observed times looking for times the model track
            ; is close enough.
          endif
        endif else if early_distance_match_time eq 0 then begin
          if debug ge 1 then print, 'track ',mcv_id[itrack]+ ' beyond 1 day && Best Track has been TS strength, but still ',string(displacement/1000,format='(I0)'), ' km away. Skipping'
          break ; if model is beyond 1 day and Best Track has reached TS strength and no match then skip this track
        endif
      endfor ; Loop thru each observed time within the time range of the model run
      
      ; If the distance is less than the distance threshold we have a potential match. . .
      if early_distance_match_time gt 0 then begin
        if debug ge 1 then print, 'found a match to ', stormname, model_tracks_files[iinit], mcv_id[itrack], format='(2A," in ",A,": ", A)'
        last_itime = max(where(finite(mcv_times[itrack,*]),/NULL))
        
        i0 = value_locate(observed_times, mcv_times[itrack,0])
        i1 = value_locate(observed_times, mcv_times[itrack,last_itime])
        if i1 - i0 gt 0 then begin
          obs_uv = get_uv(observed_lons[i0],observed_lats[i0],observed_lons[i1],observed_lats[i1],$
            observed_times[i0],observed_times[i1],debug=debug)
          uv = get_uv(mcv_lons[itrack,0],mcv_lats[itrack,0],mcv_lons[itrack,last_itime],mcv_lats[itrack,last_itime],$
            mcv_times[itrack,0],mcv_times[itrack,last_itime], debug=debug)
          vmd =  sqrt(total((obs_uv-uv)^2.)) ; magnitude of vector motion difference
          if debug ge 1|| vmd gt 10 then print, stormname, mcv_id[itrack], 'vector motion difference:', vmd, format='(A," - ",A,x,A,F6.2," m/s")'
        endif
        
        model_track = {itrack:itrack, tracks_file:model_tracks_files[iinit], init_time:init_time, init_date:init_date, $
          early_distance_match_time:early_distance_match_time, id:mcv_id[itrack], $
          lon:  reform(mcv_lons[itrack,0:last_itime]),   lat:     reform(mcv_lats[itrack,0:last_itime]), $
          times:reform(mcv_times[itrack,0:last_itime]), intensity:reform(mcv_intensity[itrack,0:last_itime]), specs:specs, $
          model_name:model.name, bdeck_file:bdeck_file, stormname:stormname, min_duration_days:min_duration_days, $
          average_separation:!VALUES.D_NAN}
          
        ; if there is no previous match, start the list
        if matching_model_tracks.count() eq 0 then begin
          matching_model_tracks.add, model_track
        endif else begin
          ; if this match starts earlier than the previous one then throw out the previous one
          ; and add this one instead.
          if early_distance_match_time lt matching_model_tracks[-1].early_distance_match_time then begin
            ; new match could match earlier than last segment and previous segments (like 93E20141027)
            while matching_model_tracks.count() gt 0 && early_distance_match_time lt matching_model_tracks[-1].early_distance_match_time do begin
              print, model_track.id+ ' matches earlier than last match. throwing out '+matching_model_tracks[-1].id
              matching_model_tracks.Remove
            endwhile
            matching_model_tracks.add, model_track
          endif
          ; add if this new track starts after the previous track ends.
          if first_time gt matching_model_tracks[-1].valid_time[-1] then matching_model_tracks.add, model_track
          
        endelse
      endif
      
    endfor ; itrack loop. Loop through each model_track in the model tracks file
    
    
    
    ipossible_misses = where(observed_times ge init_time and observed_times le (init_time + total_model_days) and observed_vmax ge vmax_thresh_kt,/null)
    if matching_model_tracks.count() eq 0 then begin
      print, 'MISS1: '+strmid(file_basename(bdeck_file),1,2)+' '+stormname+ ' '+model.name+' '+ model_tracks_files[iinit]
      foreach imiss,ipossible_misses do printf, miss_lun, stormname, init_time, observed_times[imiss], $
        observed_lons[imiss],observed_lats[imiss],observed_vmax[imiss], 24d*(observed_times[imiss]-init_time),$
        format='(A16,x,2C(CYI4.4,CMOI2.2,CDI2.2,CHI2.2,x),F7.1,"E",F6.1,"N",I4," kt fh=",I3)'
    endif else begin
    
      ; join the model_tracks and interpolate to observed times--but only the observed times for which there
      ;  is a potential model track.
      ; Not sure I want to only interpolate to the observed times . That makes sense when there are gaps between
      ; matching model tracks, but can u keep times in the matching model tracks that are not in the observed tracks?
      best_model_track = join_model_tracks(matching_model_tracks, observed_times)
      duration_h = (max(best_model_track.valid_time) - min(best_model_track.valid_time)) * 24
      print, stormname + ' best match is ' + strjoin(best_model_track.id,'/'), model_tracks_files[iinit],$
        max(best_model_track.intensity), duration_h, format='(A, " in ", A," ",E8.1,I4,"h")'
        
        
      if (1) then best_model_track = add_vitals(list(best_model_track),model, origmesh=origmesh)
      best_model_track = best_model_track[0] ; un-"list" best_model_track.
      
      ; Requirement after Sep 21, 2015. Model tracks must reach vmax_thresh_kt.
      ; convert to kts and then round (to replicate print_atcf)
      imodelHUR = where(round(best_model_track.max_spd10m / !ATMOS.kts2mps ) ge vmax_thresh_kt, /null)
      if imodelHUR eq !NULL then begin
        print, best_model_track.id+' never >='+string(vmax_thresh_kt, format='(I3," kts")')
        print, 'MISS2: '+strmid(file_basename(bdeck_file),1,2)+' '+stormname+ ' '+model.name+' '+ model_tracks_files[iinit]
        foreach imiss,ipossible_misses do printf, miss_lun, stormname, init_time, observed_times[imiss], $
          observed_lons[imiss],observed_lats[imiss],observed_vmax[imiss], 24d*(observed_times[imiss]-init_time),$
          format='(A16,x,2C(CYI4.4,CMOI2.2,CDI2.2,CHI2.2,x),F7.1,"E",F6.1,"N",I4," kt fh=",I3)'
        continue ; miss
      endif else begin
        ; Requirement after Sep 21, 2015. If model track reaches vmax_thresh_kt, best track
        ; must not reach vmax_thresh_kt during forecast period.
        iobsHUR = where(observed_times ge init_time and observed_times le (init_time + total_model_days) and observed_vmax ge vmax_thresh_kt,/null)
        if iobsHUR eq !NULL then continue ; no observed vmax_thresh_kts during forecast period, so false alarm
      endelse
      
      
      ; Removed this requirement Jan 12, 2015. It stopped me from counting tracks that were always outside the tropics.
      ; Ensure max wind gets to TS strength somewhere in the tropics
      ;iTS = where(best_model_track.max_spd10m gt 17 and abs(best_model_track.lat) lt 30., /null)
      ;if iTS eq !NULL then begin
      ;  print, best_model_track.id+' never TS strength in tropics'
      ;  continue
      ;endif
      
      if debug eq 0 then begin
        if start_new_atcf_output_file then openw, atcf_lun, my_atcf_file, /get_lun
        start_new_atcf_output_file=0
        print_atcf, atcf_lun, my_atcf_file, best_model_track, debug=debug
      endif
    endelse
    
  endfor; Look through each model tracks file that could intersect the observed system.
  
  if n_elements(atcf_lun) ne 0 && (fstat(atcf_lun)).open ne 0 then free_lun, atcf_lun
  flush, miss_lun
end

pro run_find_matching_model_track, debug=debug, force_new=force_new, model=model, basin=basin, $
  year=year, origmesh=origmesh, trackertype=trackertype, GFDL_warmcore_only=GFDL_warmcore_only
  ; Create ADECK files for all the BDECK files in a year.  Can be restricted to a particular basin or
  ; done globally by setting basin to '*'.
  ; BDECK files are best tracks for each tropical storm. I keep them in /glade/p/work/ahijevyc/atcf/b*.dat
  ; An ADECK file contains the model tracks that match a particular BDECK system. They can be lumped together all in the same file
  ; or done separately for each model.  This program does it separately for each model and outputs them to 
  ; /glade/p/work/ahijevyc/tracking_<tracker>/<modelname>/(tcgen|tracker)/.
  ; This program used to be included in the IDL file get_vitals.pro.
  if ~keyword_set(debug) then debug=0
  ; If force_new=1, then any existing output files will be overwritten. If force_new=0 then the output file will
  ; not be overwritten.
  if ~keyword_set(force_new) then force_new=0
  if ~keyword_set(basin) then basin = '*'
  if ~keyword_set(year) then year = '2017'
  if n_elements(trackertype) eq 0 then trackertype='tcgen'
  if n_elements(GFDL_warmcore_only) eq 0 then GFDL_warmcore_only = 1 ; Made default 1 Nov 21, 2016. It was 0 by default in read_atcf.pro prior to that.

  ; BDECK identification numbers must be between 0 and 69.  70 and above are not standard tropical systems or are tracked differently.
  files = file_search('/glade/p/work/ahijevyc/atcf/b'+basin+'[0-6][0-9]'+year+'.dat', count=nfiles, /fold_case)
  if ~keyword_set(model) then begin
    if year eq '2012' then models = ['4km']
    if year eq '2014' then models = ['mpas', 'mpas_ep', 'GFS']
    if year eq '2015' then models = ['ep', 'wp', 'al', 'GFS']
    if year eq '2016' then models = ['GFS', 'wp']; , 'al', 'ep']
    if year eq '2017' then models = ['wp','GFS', 'uni']
  endif else models=[model]

  ; Loop through each model and loop through each best track BDECK.
  for m=0,n_elements(models)-1 do begin
    model = mpas_mesh(models[m])
    origmesh=strmatch(model.name, 'GFS*')?0:1
    for ifile = 0, nfiles-1 do find_matching_model_track, files[ifile], model=model, debug=debug, $
      force_new=force_new, origmesh=origmesh, trackertype=trackertype, $
      GFDL_warmcore_only=GFDL_warmcore_only

  endfor

end

function expand, x
  dim = size(x, /dim)
  ntracks = dim[0]
  ntimes = dim[1]
  ; one extra time on end, set to NaN
  y = replicate(!VALUES.D_NAN, [ntracks,ntimes+1])
  for itrack=0,ntracks-1 do y[itrack,0:ntimes-1] = x[itrack,*]
  return, y
end

function get_time_distance_thresh, forecast_hour_in
  ; Values for E(t) are taken from the following plot, using the 95th percentile error of the NHC $
  ; forecasts at various lead times: http://www.nhc.noaa.gov/verification/figs/5yr_ALtkerrdist.jpg
  forecast_hour = round(forecast_hour_in)
  f = [   0,  24,  48,  72,  96, 120,  144,  240, 480]
  d = [ 200, 200, 350, 510, 680, 860, 1000, 1000, 1000]
  if forecast_hour lt f[0] || forecast_hour gt f[-1] then stop
  return, interpol(d, f, forecast_hour)

end



pro find_matching_model_track, bdeck_file, model=model, force_new=force_new, debug=debug, $
  origmesh=origmesh, trackertype=trackertype, GFDL_warmcore_only=GFDL_warmcore_only

  ; for ONE observed best track (BDECK), find all model runs that overlap the time window of the BDECK system.
  ; Find the closest vortex track in each model run and determine if it is close enough to match.
  ; Write matches in ATCF format to an ADECK file that is saved in /glade/p/work/ahijevyc/tracking_gfdl/<modelname>/.
  atmos_const
  if ~keyword_set(debug) then debug = 0
  if n_elements(trackertype) eq 0 then trackertype = 'tcgen'
  min_duration_days = 1.d  ; minimum duration of model model_track to consider (unless observation is on boundary of model time window)
  if ~keyword_set(force_new) then force_new = 0 ; force_new=1 forces atcf file and vitals save file to be remade.
  if n_elements(bdeck_file) eq 0 then bdeck_file = '/glade/p/work/ahijevyc/atcf/bal112017.dat'
  parent_id = '9qw58wx10r'
  if ~keyword_set(model) then model = mpas_mesh('uni') else if isa(model,/scalar) then model=mpas_mesh(model)
  total_model_days = strmatch(model.name, 'GFS*') ? 8d : 10d
  if (model.name eq 'mpas15_3') then total_model_days = 3d
  ; If set, GFDL_warmcore_only will make sure at least one time is warm core in the model track
  ; because read_atcf will filter it out if it doesn't have one warm core time.
  ; It wasn't set for the MWR paper, reverting to the default (un-set) in read_atcf.pro.
  ; GFDL_warmcore_only was set back when I was using "tracker.pro", as opposed to this script.
  ; You can find tracks and histograms with GFDL_warmcore_only in "old/" subdirectory under
  ; /glade/p/work/ahijevyc/tracking_gfdl/.
  if n_elements(GFDL_warmcore_only) eq 0 then GFDL_warmcore_only = 1
  
  ; fort.64 or fort.66 file must be in possible_fort66_dirs/yyyymmdd00/latlon_dxdetails/gfdl_tracker/tracker_type/.
  possible_fort66_dirs = ['/glade/scratch/ahijevyc/','/glade/scratch/mpasrt/'] + model.name
  
  tracker = 'gfdl'
  smooth_radius_str = strmatch(model.name, 'GFS*') ? '' : '_025km'
  grid_dx = 0.5
  if strmatch(model.name, 'GFS*') then grid_dx = 0.25
  dxdetails = string(grid_dx,format='("_",F5.3,"deg")')+smooth_radius_str
  ; Tried requiring model tracks to reach a threshold in order to be a hit.
  ; Now we let them through and let tc_stat handle weak tracks. 
  ; That is why we had "vmax_thresh_kt.ge.XX" in the output directory
  ; structure.
  start_new_atcf_output_file=1

  ; Read observed best track file
  if file_test(bdeck_file, /zero) eq 1 then return
  year     = strmid(file_basename(bdeck_file),5,4)
  storm_id = strmid(file_basename(bdeck_file),1,8)
  ; if BDECK, use 'BEST', otherwise use 'CARQ', unless it's w. pacific--then use 'WRNG'
  if strmid(file_basename(bdeck_file),0,1) eq 'b' then tech = 'BEST' else if strpos(file_basename(bdeck_file), 'wp') ne -1 then tech = 'WRNG' else tech = 'CARQ'
  obs = read_atcf(bdeck_file, tech=tech, rad=34) ; only get 0 and 34-kt lines.
  max_vmax=max(obs.vmax) ; storm name taken from the time with greatest vmax.
  ; there can be multiple times with maximum wind. use the latest one. This helps al082014 (HANNA)
  ; it is only INVEST for the first time it reaches max wind speed but becomes HANNA later.
  imax = max(where(obs.vmax eq max_vmax))
  stormname = (obs.stormname)[imax]
  ; Require best track is TS status at some point. ; added Mar 2016.
  ; We don't want Tropical depression sixteen in EP 2014. It never gets named but it is
  ; in the best track database for some reason. We don't want it in our study.
  ; If you do include it, ep15 Major Hurricane ODILE tracks will also match it, which you don't want..
  if max_vmax lt 34 then begin
    print, storm_id + ' ' + stormname + ' is not TS at some point. skipping. '
    return
  endif

  ; print_atcf will use characters in columns 2-3 of file_basename for basin name
  ; if you change "fiorino" path, make sure you also change it where model_tracks_files is
  ; defined below (fort.64 path)
  my_atcf_file = '/glade/p/work/ahijevyc/tracking_'+tracker+'/adeck/'+ model.name + $
    "/" + trackertype + '/a' + storm_id + dxdetails +'_'+tracker+ '_origmesh' + (origmesh?'True':'False') + $
    string(min_duration_days, format='("_",F3.1,"d_minimum")') + $
    (GFDL_warmcore_only ? '_GFDL_warmcore_only' : '')

  if file_test(my_atcf_file) && file_test(my_atcf_file, /zero_length) eq 0 && force_new eq 0 then begin
    print, "found ", my_atcf_file, ". skipping. "
    return
  endif

  obs_first = min(obs.time)
  obs_last  = max(obs.time)

  print, stormname, obs_first, obs.lon[0], obs.lat[0], obs_last, format='(A,1x,C(),F7.1,"E",F6.1,"N ",C())'
  if debug ge 1 then print, string(obs.time, format='(C())') + string(obs.lon, format='(F7.1,"E")') + $
    string(obs.lat, format='(F6.1,"N")')

  ; find the first time the storm reaches TS strength (34 kt)
  i_TS_strength = where(obs.vmax ge 34, /null) ; obs (atcf format) is in knots
  ; if it never reaches TS strength use time=infinity
  TS_first_time = i_TS_strength ne !NULL ? obs.time[min(i_TS_strength)] : !VALUES.D_INFINITY

  ; Find the model tracks files for overlapping model runs for this particular model type and smoothing radius.
  search_str = possible_fort66_dirs +'/'+year+'[01][0-9][0-3][0-9][012][0-9]'+$
     ; '/ecic'+$ ; Temporary kludge - also alter model_basedirs in add_vitals.pro
    (strmatch(model.name,'GFS*')?'':'/latlon'+dxdetails)+$
    '/gfdl_tracker/'+trackertype+'/fort.' + (strmatch(trackertype,'tcgen*')? '66':'64')
  model_tracks_files = file_search(search_str, count=nmodel_tracks_files)
  if nmodel_tracks_files eq 0 then begin
    print, "no model tracks files for "+model.name+" "+year
    print, "searched "+search_str
    stop
  endif
  ; extract the yyyymmddhh substrings from the directory paths of the model tracks files.
  ; These are model initialization times
  i201 = strpos(model_tracks_files,'/201')
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

  ; Don't open this too early.
  openw, misses_lun, file_dirname(my_atcf_file)+'/misses_'+file_basename(my_atcf_file), /get_lun


  ; Loop through the model tracks files that overlap the observed system.
  for iinit = 0, n_elements(model_tracks_files)-1 do begin
    init_time = model_tracks_file_juldays[iinit]
    init_date = string(init_time, format='(C(CYI4.4, CMoI2.2, CDI2.2, CHI2.2))')
    ;if init_date ne '2017080400' then continue

    ; Read entire atcf file (usually fort.64 or fort.66)
    ; gfdl_m dictionary will have a 1-D array for each atcf column
    ; mcv_lats, mcv_lons, mcv_times, and mcv_intensity, and mcv_id are 2-d arrays with
    ; dimensions [storm, time]. They will be used below.
    gfdl_m = read_atcf(model_tracks_files[iinit], lats=mcv_lats, lons=mcv_lons, times=mcv_times, $
      intensity=mcv_intensity, id=mcv_id)
    ; Be aware of bad tc_vitals files that cause zero lat/lon in GFDL tracker output.
    ibad_tc_vitals = where(gfdl_m.lat eq 0 and gfdl_m.lon eq 0, /null)
    if ibad_tc_vitals ne !NULL then begin
      print, "zero lat/lon in ",model_tracks_files[iinit]," ", $
        (gfdl_m.basin+gfdl_m.cy)[ibad_tc_vitals], ". Maybe a messed up TCGP "
      print, "TC vitals file input to GFDL tracker. See Google Doc methodology."
      ;For tracks after Dec 9, 2016, there should be no issue with multiple instances
      ;of the same storm. Now wget_tcvitals.csh fixes it.
      ;However it didn't fix the zeros for al/2015091400/tracker.
      ; It still gets zeros in lat lon for the first hour for 94L Invest.      
      ; stop
    endif


    matching_model_tracks = LIST()
    ; Loop through each model_track in the model tracks file
    ntrack = n_elements(mcv_times[*,0])
    if debug ge 1 then print, model_tracks_files[iinit], ' found ', strtrim(ntrack,2),' ',model.name,' tracks'
    for itrack = 0, ntrack-1 do begin
      early_distance_match_time=0d

      ; I moved this outside the itime-observed time loop Thr Feb 20. Don't know why it was in there.
      first_time = min(mcv_times[itrack,*], max=last_time, /nan)

      ; Skip this track if a) the previously matching track hasn't ended yet and
      ; b) it starts after the previously matching track's early_distance_match_time.
      ; If previous track hasn't ended yet but this one starts before the previous track's
      ;  early_distance_match time, then don't skip.
      ; See mpas_al 20130827 Gabrielle #29. It's a poor match compared to #35 and #556.
      if matching_model_tracks.count() gt 0 && first_time le matching_model_tracks[-1].times[-1] && $
        first_time gt matching_model_tracks[-1].early_distance_match_time then continue

      ; if this model_track segment doesn't last at least min_duration_days then skip it.
      ; Without this you get poor matches that overlap better matches for TORAJI for 2013 Aug 27 GFS.
      ; Allow for smaller minimum duration if track is on a boundary of the model time window.
      ; Would like to allow for smaller minimum duration if storm moves off grid, like AL082014 Gonzalo in
      ; 20141019 run.   It is tracked in 0-6 hours, but goes off grid at f012. Can't be matched here
      ; yet still shows up as potential match because best track goes to 20141019 18Z.
      tmp_min_duration = min([min_duration_days, obs.time[-1]-init_time, init_time+total_model_days -obs.time[0]])
      if last_time - first_time lt tmp_min_duration then begin
        if debug ge 2 then print, 'track '+mcv_id[itrack]+' does not last ', string(tmp_min_duration,format='(F5.2)'),' days. Skipping'
        continue
      endif

      ; Requre gfdl_warmcore_only warm core times for this track to count.
      nwarmcore = total(gfdl_m.twod.warmcore[itrack,*] eq 'Y')
      if debug then print, mcv_id[itrack], nwarmcore, format='(A," found ",I0," warm core times.")'
      if nwarmcore lt gfdl_warmcore_only then begin
        print, mcv_id[itrack], gfdl_warmcore_only, nwarmcore, format='(A,I0," warm core times required. Found only ",I0,".")'
        continue
      endif


      for itime=0, n_elements(obs.time)-1 do begin
        observed_time = obs.time[itime]
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
            mcv_lons = expand(mcv_lons)
            mcv_lats = expand(mcv_lats)
            mcv_times = expand(mcv_times)
            mcv_intensity = expand(mcv_intensity)
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

        displacement_m = map_2points(obs.lon[itime], obs.lat[itime], mcv_lons[itrack, imatch], mcv_lats[itrack, imatch], /meters)
        ; if this is within the first day of the observed or model track, see if track is close enough to match
        ; or if the observed track hasn't reached TS strength yet, see if track is close enough to match
        ; could wait a day after 1st TS strength, but I try 0.
        if mcv_times[itrack,imatch] - TS_first_time le 0d || mcv_times[itrack,imatch] - first_time le 1d then begin
          forecast_hour = 24d *(observed_time - init_time)
          if displacement_m/1000. lt get_time_distance_thresh(forecast_hour) then begin
            ; keep track of the earliest early_distance_match_time
            early_distance_match_time = observed_time
            if debug ge 1 then print, stormname, mcv_id[itrack], forecast_hour, displacement_m/1000., $
              format='(A," - track ",A," matches at f",i3.3,". Displacement ",F4.0, "km")'
            break ; this track MATCHES! no need to keep looping through observed times looking for times the model track
            ; is close enough.
          endif
        endif else if early_distance_match_time eq 0 then begin
          if debug ge 1 then begin
            print, 'at', string(mcv_times[itrack,imatch], format='(c(CMoI,"/",CDI," ",CHI),"Z",$)'), $
              ' track ',mcv_id[itrack]+ ' > 1 day && observed Best Track has been TS strength ', $
              'but still ', string(displacement_m/1000,format='(I0)'), ' km away. Skipping'
      
          endif
          break ; if model is beyond 1 day and Best Track has reached TS strength and no match then skip this track
        endif
      endfor ; Loop thru each observed time within the time range of the model run

      ; If the distance is less than the distance threshold we have a potential match. . .
      if early_distance_match_time gt 0 then begin
        if debug ge 1 then print, 'found a match to ', stormname, model_tracks_files[iinit], mcv_id[itrack], format='(2A," in ",A,": ", A)'
        last_itime = max(where(finite(mcv_times[itrack,*]),/NULL))

        i0 = value_locate(obs.time, mcv_times[itrack,0])
        i1 = value_locate(obs.time, mcv_times[itrack,last_itime])
        if i1 - i0 gt 0 then begin
          obs_uv = get_uv(obs.lon[i0],obs.lat[i0],obs.lon[i1],obs.lat[i1],$
            obs.time[i0],obs.time[i1],debug=debug)
          uv = get_uv(mcv_lons[itrack,0],mcv_lats[itrack,0],mcv_lons[itrack,last_itime],mcv_lats[itrack,last_itime],$
            mcv_times[itrack,0],mcv_times[itrack,last_itime], debug=debug)
          vmd =  sqrt(total((obs_uv-uv)^2.)) ; magnitude of vector motion difference
          if debug ge 1|| vmd gt 10 then print, stormname, mcv_id[itrack], 'vector motion difference:', vmd, format='(A," - ",A,x,A,F6.2," m/s")'
        endif

        ; If you add another tag, make sure you add it in join_model_tracks.pro also.
        ; Added to plot_storm.pro too. 20170818
        model_track = {itrack:itrack, tracks_file:model_tracks_files[iinit], init_time:init_time, init_date:init_date, $
          early_distance_match_time:early_distance_match_time, id:mcv_id[itrack], $
          lon:  reform(mcv_lons[itrack,0:last_itime]),   lat:     reform(mcv_lats[itrack,0:last_itime]), $
          times:reform(mcv_times[itrack,0:last_itime]), intensity:reform(mcv_intensity[itrack,0:last_itime]), $
          model_name:model.name, bdeck_file:bdeck_file, stormname:stormname, $
          GFDL_warmcore_only: GFDL_warmcore_only, min_duration_days:min_duration_days}

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
          if first_time gt matching_model_tracks[-1].times[-1] then matching_model_tracks.add, model_track

        endelse
      endif

    endfor ; itrack loop. Loop through each model_track in the fort.66 model tracks file



    missformat =  '(A8,x,A-16,x,2C(CYI4.4,CMOI2.2,CDI2.2,CHI2.2,x),F7.1,"E",F6.1,"N",I4," kt fh=",I3,A15," genesis_fh=",I4)'
    if matching_model_tracks.count() eq 0 then begin
      imisses = where(obs.time ge init_time and obs.time le (init_time + total_model_days),/null)
      ; NO matching model track for the observed best track in this bdeck file.
      print, 'COMPLETE MISS: '+strmid(file_basename(bdeck_file),1,2)+' '+stormname+ ' '+model.name+' '+ model_tracks_files[iinit]
      foreach imiss,imisses do printf, misses_lun, storm_id, stormname, init_time, obs.time[imiss], $
        obs.lon[imiss],obs.lat[imiss],obs.vmax[imiss], 24d*(obs.time[imiss]-init_time),$
        "completemiss", 24d*(TS_first_time-init_time), format=missformat
    endif else begin

      ; join the model_tracks and interpolate to observed times--but only the observed times for which
      ;  there is a potential model track.
      ; Not sure I want to only interpolate to the observed times . That makes sense when there are gaps between
      ; matching model tracks, but can u keep times in the matching model tracks that are not in the observed tracks?
      matching_model_track = join_model_tracks(matching_model_tracks, obs.time)
      duration_h = (max(matching_model_track.times) - min(matching_model_track.times)) * 24
      print, stormname + ' best match is ' + strjoin(matching_model_track.id,'/'), model_tracks_files[iinit],$
        max(matching_model_track.intensity), duration_h, format='(A, " in ", A," ",E8.1,I4,"h")'

      imisses = where(obs.time ge init_time and obs.time le (init_time + total_model_days)$
        and (obs.time lt min(matching_model_track.times) or obs.time gt max(matching_model_track.times)),/null)
      ; There is a matching model track for this observed best track, but not at these particular times.
      foreach imiss,imisses do printf, misses_lun, storm_id, stormname, init_time, obs.time[imiss], $
        obs.lon[imiss],obs.lat[imiss],obs.vmax[imiss], 24d*(obs.time[imiss]-init_time),$
        "partial_miss", 24d*(TS_first_time-init_time), format=missformat

      matching_model_track = add_vitals(list(matching_model_track), model, origmesh=origmesh)

      if debug eq 0 then begin
        if start_new_atcf_output_file then openw, atcf_lun, my_atcf_file, /get_lun
        start_new_atcf_output_file=0
        print_atcf, atcf_lun, my_atcf_file, matching_model_track, debug=debug
      endif

    endelse
    flush, misses_lun

  endfor; Look through each model run that could intersect the observed system.



  if n_elements(atcf_lun) ne 0 && (fstat(atcf_lun)).open ne 0 then free_lun, atcf_lun
  if n_elements(misses_lun) ne 0 && (fstat(misses_lun)).open ne 0 then free_lun, misses_lun
  plot_storm, my_atcf_file, ofile=my_atcf_file+'.png', title=storm_id+" "+stormname+" "+$
    tracker+" "+trackertype+" tracks!Cfrom "+string(grid_dx,format='(F5.3)')+"° "+$
    model.name+" model. origmesh"+(origmesh?'True':'False')+" GFDL_warmcore_only="+$
    strtrim(GFDL_warmcore_only,2), force_new=force_new, buffer=1, model=model
end

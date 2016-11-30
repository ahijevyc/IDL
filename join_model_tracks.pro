function join_model_tracks, model_tracks, observed_times_in
  observed_times = observed_times_in ; don't want to alter this. 
  
  ; would you believe the best track files can have multiple lines with the same time and storm? 
  ; like when you have radius of 1) 34-knot 2) 50-knot and 3) 64-knot winds
  ;  join_model_tracks() just needs a stack of unique times to interpolate to. Repeated times don't help (or hurt) it.
   
  if not array_equal(observed_times, observed_times[sort(observed_times)]) then stop ; sanity check - observed_times monotonically ascends
  observed_times = observed_times[uniq(observed_times)] 

  ; join the model_tracks from tip to tail and interpolate to observed_times
  
  ; removed "specs" keyword Oct 2015
  model_track0 = model_tracks[0]
  for imodel_track=1,model_tracks.count()-1 do begin
    model_track = model_tracks[imodel_track]
    if model_track.tracks_file ne model_track0.tracks_file then stop
    if model_track.init_time ne model_track0.init_time then stop
    if model_track.init_date ne model_track0.init_date then stop
    if model_track.model_name ne model_track0.model_name then stop
    if model_track.bdeck_file ne model_track0.bdeck_file then stop
    if model_track.stormname ne model_track0.stormname then stop
    if model_track.min_duration_days ne model_track0.min_duration_days then stop
  endfor
  
  itrack  = !NULL
  lons  = !NULL
  lats  = !NULL
  times = !NULL
  intensity = !NULL
  id = !NULL
  for imodel_track=0,model_tracks.count()-1 do begin
    model_track = model_tracks[imodel_track]
    
    itrack = [itrack, model_track.itrack]
    lons = [lons, model_track.lon]
    lats = [lats, model_track.lat]
    times = [times, model_track.times]
    intensity = [intensity, model_track.intensity]
    id = [id, model_track.id]
  endfor

  
  ; interpolate to observed_times that are within the total track time window
  observed_times = observed_times[where(observed_times ge min(times) and observed_times le max(times))]
  ; re-included original times Aug 11 , 2014. Important not to forget times for which the 
  ; wind speed may reach tropical storm strength in the tropics but the time isn't an "OBSERVED" best track
  ; time.  Like HUMBERTO AL09 for GFS004 initialized 20130916.  It does reach TS strength in the tropics
  ; but not on a multiple of 6 hours.  
  all_times = [times, observed_times]
  all_times = all_times[uniq(all_times,sort(all_times))]
  lons       = interpol_nan(     lons, times, all_times)
  lats       = interpol_nan(     lats, times, all_times)
  intensity  = interpol_nan(intensity, times, all_times)

  


  model_track = {itrack:itrack, tracks_file:model_track0.tracks_file, init_time:model_track0.init_time, init_date:model_track0.init_date, $
    lon:lons, lat:lats, times:all_times, intensity:intensity, model_name:model_track0.model_name, bdeck_file:model_track0.bdeck_file, $
    stormname:model_track0.stormname, min_duration_days:model_track0.min_duration_days, $
    id:id}
    
    
    
  return, model_track
  
end
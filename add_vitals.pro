function vitals_structure, ntimes
  data = replicate(!VALUES.D_NAN, ntimes)
  vitals = {$
     max_spd10m:{field:'speed10',            range:[0,500],   op:'max',  data:data} $
    ,min_slp:   {field:'mslp',               range:[0,100],   op:'min',  data:data} $
    ,maxr_s10m: {field:'speed10',            range:[0,500],   op:'maxr', data:data} $
    ,NE_spd10m: {field:'speed10',            range:[0,500],   op:'NE17', data:data} $
    ,SE_spd10m: {field:'speed10',            range:[0,500],   op:'SE17', data:data} $
    ,SW_spd10m: {field:'speed10',            range:[0,500],   op:'SW17', data:data} $
    ,NW_spd10m: {field:'speed10',            range:[0,500],   op:'NW17', data:data} $
;    ,t850_core: {field:'temperature_850hPa', range:[0,100],   op:'mean', data:data} $
;    ,t500_core: {field:'temperature_500hPa', range:[0,100],   op:'mean', data:data} $
;    ,t200_core: {field:'temperature_200hPa', range:[0,100],   op:'mean', data:data} $
;    ,t850_surr: {field:'temperature_850hPa', range:[300,500], op:'mean', data:data} $
;    ,t500_surr: {field:'temperature_500hPa', range:[300,500], op:'mean', data:data} $
;    ,t200_surr: {field:'temperature_200hPa', range:[300,500], op:'mean', data:data} $
;    ,z850_core: {field:'height_850hPa',      range:[0,200],   op:'mean', data:data} $
;    ,z200_core: {field:'height_200hPa',      range:[0,200],   op:'mean', data:data} $
;    ,z850_surr: {field:'height_850hPa',      range:[300,500], op:'mean', data:data} $
;    ,z200_surr: {field:'height_200hPa',      range:[300,500], op:'mean', data:data} $
;    ,rainc:     {field:'rainc',              range:[0,100],   op:'mean', data:data} $
;    ,rainnc:    {field:'rainnc',             range:[0,100],   op:'mean', data:data} $
  }
  return, vitals
end


function add_vitals, model_tracks, mpas, origmesh=origmesh
  ; Takes a list of tracks (model_tracks) 
  ; Lists come from get_all_model_vitals or find_matching_model_track
  if n_elements(origmesh) eq 0 then origmesh = 1
  atmos_const
  ; tear apart rows of data in each track
  ; Put them into equal-sized 1-D vectors of times, lats, and lons
  times = !NULL
  lats  = !NULL
  lons  = !NULL
  init_date = model_tracks[0].init_date
  ntracks = model_tracks.count()
  
  ; model_tracks was written for get_all_model_vitals, which gives a list of model tracks
  ; find_matching_model_track gives a single track, and it's not a list, just a structure.
  ; fixed this by converting output of find_matching_model_track to a list() 20140718
  
  ; First take care of origmesh=False. Don't get original mesh values for this IF block
  if origmesh eq 0 && strmatch(model_tracks[0].tracks_file, '*fort.[5-9]*') then begin
    foreach model_track, model_tracks, itrack do begin
      t = read_atcf(model_track.tracks_file) ; read_atcf returns 'vmax' tag in knots and mslp in hPa
      nm2km = 1.852
      ig = !NULL ; find all matching ids in the fort.66 file so we can grab the vmax and mslp
      ; added clause for fort.64 files to match their ids (t.init_yyyymmddhh+t.basin+t.cy)
      for iid=0,n_elements(model_track.id)-1 do ig = [ig, where(t.cy+t.stormname eq model_track.id[iid] $
                                               or t.init_yyyymmddhh+t.basin+t.cy eq model_track.id[iid], /null)]
      vitals = vitals_structure(n_elements(model_track.times))
      
      ; but there may be times in the track that were interpolated with join_model_tracks.pro.
      ; How do we deal with that?  With interpol_nan
      vitals.max_spd10m.data = interpol_nan(t.vmax[ig] * !ATMOS.kts2mps, t.julday[ig], model_track.times)
      if total(finite(vitals.max_spd10m.data) eq 0) then stop
      vitals.min_slp.data   = interpol_nan(t.mslp[ig]*100.,    t.julday[ig], model_track.times)
      vitals.ne_spd10m.data  = interpol_nan(t.rad1[ig] * nm2km, t.julday[ig], model_track.times)
      vitals.se_spd10m.data  = interpol_nan(t.rad2[ig] * nm2km, t.julday[ig], model_track.times)
      vitals.sw_spd10m.data  = interpol_nan(t.rad3[ig] * nm2km, t.julday[ig], model_track.times)
      vitals.nw_spd10m.data  = interpol_nan(t.rad4[ig] * nm2km, t.julday[ig], model_track.times)
      vitals.maxr_s10m.data  = interpol_nan( t.mrd[ig] * nm2km, t.julday[ig], model_track.times)
      ; return vmax in m/s and radius of max wind in km
      for itag=0,n_tags(vitals)-1 do begin
        field = (tag_names(vitals))[itag]
        model_tracks[itrack] = create_struct(field, vitals.(itag).data, model_tracks[itrack])
      endfor
      model_tracks[itrack] = create_struct('origmesh', origmesh, model_tracks[itrack])
    endforeach 
    return, model_tracks
  endif ; origmesh=False
  
  ; Get vitals from raw mesh (as opposed to fort.66 GFDL tracker output) - MPAS and even GFS!
  for itrack=0,ntracks-1 do begin
    track = model_tracks[itrack]
    times = [times, track.times]
    lats  = [lats, track.lat]
    lons  = [lons, track.lon]
    if track.init_date ne init_date then stop ; sanity check
  endfor
  ; get unique times in all tracks (not really needed anymore)
  ; we used to be efficient in how we ran through each diagnostic file only once
  ; even if multiple storms are in it.
  vitals_times = times[uniq(times,sort(times))]
  vitals = vitals_structure(n_elements(times))
  mcv_nearestCells = mpas_nearest_cell(lons, lats, mpas) ; works with GFS too. 
  
  ; model_files is a list of files from which the vitals were read for this set of model tracks.
  model_files = list()
  ; loop through each vitals time (a unique list of times)
  for itime = 0, n_elements(vitals_times)-1 do begin
    ; times is an ordered array of concatenated times from all the tracks (with repeats)
    ; vitals_times has no repeats
    ; vital_itimes will be used to locate the time indicies that match this vitals time.
    vital_itimes = where(times eq vitals_times[itime], /null)
    fill_vitals, mpas, mcv_nearestCells[vital_itimes], init_date, vitals_times[itime], vitals, vital_itimes, $
      model_file=model_file ;  , model_basedir='/glade/scratch/ahijevyc/al/2016092900/'
    model_files.add, model_file
  endfor
  
  i=0L
  for itrack=0,ntracks-1 do begin
    track = model_tracks[itrack]
    n = total(finite(track.times))
    ; add each vitals field one at a time to the model track structure
    for itag=0,n_tags(vitals)-1 do begin
      field = (tag_names(vitals))[itag]
      model_tracks[itrack] = create_struct(field, vitals.(itag).data[i:i+n-1], model_tracks[itrack])
    endfor
    ; tag origmesh and model_files to model track structure
    model_tracks[itrack] = create_struct('origmesh', origmesh, 'model_files', model_files, model_tracks[itrack])
    i=i+n
  endfor
  
  return, model_tracks
  
end


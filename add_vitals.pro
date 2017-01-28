function vitals_structure, dim1, dim2
  data = n_elements(dim2) ? replicate(!VALUES.D_NAN, dim1, dim2) : replicate(!VALUES.D_NAN, dim1)
  vitals = {$
     max_spd10m:{field:'speed10',            range:[0,500],   op:'max',  data:data, format:'(F7.2,$)'} $
    ,min_mslp:  {field:'mslp',               range:[0,100],   op:'min',  data:data, format:'(F9.2,$)'} $
    ,maxr_s10m: {field:'speed10',            range:[0,500],   op:'maxr', data:data, format:'(F8.2,$)'} $
    ,NE_spd10m: {field:'speed10',            range:[0,500],   op:'NE17', data:data, format:'(F8.2,$)'} $
    ,SE_spd10m: {field:'speed10',            range:[0,500],   op:'SE17', data:data, format:'(F8.2,$)'} $
    ,SW_spd10m: {field:'speed10',            range:[0,500],   op:'SW17', data:data, format:'(F8.2,$)'} $
    ,NW_spd10m: {field:'speed10',            range:[0,500],   op:'NW17', data:data, format:'(F8.2,$)'} $
;    ,t850_core: {field:'temperature_850hPa', range:[0,100],   op:'mean', data:data, format:'(F8.2,$)'} $
;    ,t500_core: {field:'temperature_500hPa', range:[0,100],   op:'mean', data:data, format:'(F8.2,$)'} $
;    ,t200_core: {field:'temperature_200hPa', range:[0,100],   op:'mean', data:data, format:'(F8.2,$)'} $
;    ,t850_surr: {field:'temperature_850hPa', range:[300,500], op:'mean', data:data, format:'(F8.2,$)'} $
;    ,t500_surr: {field:'temperature_500hPa', range:[300,500], op:'mean', data:data, format:'(F8.2,$)'} $
;    ,t200_surr: {field:'temperature_200hPa', range:[300,500], op:'mean', data:data, format:'(F8.2,$)'} $
;    ,z850_core: {field:'height_850hPa',      range:[0,200],   op:'mean', data:data, format:'(F8.2,$)'} $
;    ,z200_core: {field:'height_200hPa',      range:[0,200],   op:'mean', data:data, format:'(F8.2,$)'} $
;    ,z850_surr: {field:'height_850hPa',      range:[300,500], op:'mean', data:data, format:'(F8.2,$)'} $
;    ,z200_surr: {field:'height_200hPa',      range:[300,500], op:'mean', data:data, format:'(F8.2,$)'} $
;    ,rainc:     {field:'rainc',              range:[0,100],   op:'mean', data:data, format:'(F7.3,$)'} $
;    ,rainnc:    {field:'rainnc',             range:[0,100],   op:'mean', data:data, format:'(F7.3,$)'} $
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
    for itrack=0,ntracks-1 do begin
      model_track = model_tracks[itrack]
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
      vitals.min_mslp.data   = interpol_nan(t.mslp[ig]*100.,    t.julday[ig], model_track.times)
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
    endfor
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
  vitals_times = times[uniq(times,sort(times))]
  vitals = vitals_structure(n_elements(times))
  mcv_nearestCells = mpas_nearest_cell(lons, lats, mpas) ; works with GFS too. 
  
  for itime = 0, n_elements(vitals_times)-1 do begin
    vital_itime = where(times eq vitals_times[itime], /null)
    fill_vitals, mpas, mcv_nearestCells[vital_itime], init_date, vitals_times[itime], vitals, vital_itime; , model_basedir='/glade/scratch/mpasrt/wp/2016092200/test/'
  endfor
  
  i=0L
  for itrack=0,ntracks-1 do begin
    track = model_tracks[itrack]
    n = total(finite(track.times))
    for itag=0,n_tags(vitals)-1 do begin
      field = (tag_names(vitals))[itag]
      model_tracks[itrack] = create_struct(field, vitals.(itag).data[i:i+n-1], model_tracks[itrack])
    endfor
    model_tracks[itrack] = create_struct('origmesh', origmesh, model_tracks[itrack])
    i=i+n
  endfor
  
  return, model_tracks
  
end


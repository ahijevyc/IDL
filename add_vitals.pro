function vitals_structure, ntimes
  data = replicate(!VALUES.D_NAN, ntimes)
  nm300 = 300 * 1.852 ; 555.6km
  ; 300 picked by Chris D and matches lib/python2.7/atcf.py as of Nov 13 2018
  vitals = {$
    max_spd10m: {field:'speed10',            range:[0,250],   op:'max',  data:data} $
    ,min_slp:   {field:'mslp',               range:[0,100],   op:'min',  data:data} $
    ,maxr_s10m: {field:'speed10',            range:[0,250],   op:'maxr', data:data} $
    ,NE34:      {field:'speed10',            range:[0,nm300], op:'NE34', data:data} $
    ,SE34:      {field:'speed10',            range:[0,nm300], op:'SE34', data:data} $
    ,SW34:      {field:'speed10',            range:[0,nm300], op:'SW34', data:data} $
    ,NW34:      {field:'speed10',            range:[0,nm300], op:'NW34', data:data} $
    ,NE50:      {field:'speed10',            range:[0,500],   op:'NE50', data:data} $
    ,SE50:      {field:'speed10',            range:[0,500],   op:'SE50', data:data} $
    ,SW50:      {field:'speed10',            range:[0,500],   op:'SW50', data:data} $
    ,NW50:      {field:'speed10',            range:[0,500],   op:'NW50', data:data} $
    ,NE64:      {field:'speed10',            range:[0,500],   op:'NE64', data:data} $
    ,SE64:      {field:'speed10',            range:[0,500],   op:'SE64', data:data} $
    ,SW64:      {field:'speed10',            range:[0,500],   op:'SW64', data:data} $
    ,NW64:      {field:'speed10',            range:[0,500],   op:'NW64', data:data} $
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


function add_vitals, model_tracks, mpas, origmesh=origmesh, model_basedirs=model_basedirs, $
  ignore_parent_id=ignore_parent_id, debug=debug
  ; Takes a list of tracks (model_tracks)
  ; Lists come from get_all_model_vitals or find_matching_model_track
  if n_elements(origmesh) eq 0 then origmesh = 1
  if n_elements(ignore_parent_id) eq 0 then ignore_parent_id = 0
  atmos_const
  if model_tracks.count() eq 0 then return, model_tracks
  init_date = model_tracks[0].init_date
  ntracks = model_tracks.count()

  ; model_tracks was written for get_all_model_vitals, which gives a list of model tracks
  ; find_matching_model_track gives a single track, and it's not a list, just a structure.
  ; fixed this by converting output of find_matching_model_track to a list() 20140718

  ; First take care of origmesh=False. Don't get original mesh values for this IF block
  if ~isa(origmesh, /string) then begin
    if origmesh eq 0 then begin
      if ~strmatch(model_tracks[0].tracks_file, '*fort.[5-9]*') then stop ; was in if-test above. don't know why.
      foreach model_track, model_tracks, itrack do begin
        t = read_atcf(model_track.tracks_file) ; read_atcf returns 'vmax' tag in knots and mslp in hPa
        nm2km = 1.852
        ig = !NULL ; find all matching ids in the fort.66 file so we can grab the vmax and mslp
        ; added clause for fort.64 files to match their ids (t.init_yyyymmddhh+t.basin+t.cy)
        ; read_atcf returns t.mslp and t.vmax as 2d arrays
        for iid=0,n_elements(model_track.id)-1 do ig = [ig, where(t.cy+t.stormname eq model_track.id[iid] $
          or t.init_yyyymmddhh+t.basin+t.cy eq model_track.id[iid], /null)]
        vitals = vitals_structure(n_elements(model_track.valid_time))

        ; but there may be times in the track that were interpolated with join_model_tracks.pro.
        ; How do we deal with that?  With interpol_nan
        ; return vmax in m/s
        vitals.max_spd10m.data = interpol_nan(t.vmax[ig] * !ATMOS.kts2mps, t.valid_time[ig], model_track.valid_time)
        if total(finite(vitals.max_spd10m.data)) eq 0 then stop
        ; return min_slp in Pascals
        vitals.min_slp.data    = interpol_nan(t.mslp[ig] * 100.,  t.valid_time[ig], model_track.valid_time)
        ; return radius of max wind in km
        vitals.maxr_s10m.data  = interpol_nan( t.rmw[ig] * nm2km, t.valid_time[ig], model_track.valid_time)

        ; Process multiple wind threshold (rad) lines individually.
        ; First process rad=34. Assume 34-kt ATCF lines are present.
        ig34 = where(t.rad[ig] eq 34, /null)
        vitals.NE34.data = interpol_nan((t.rad1[ig])[ig34] * nm2km, (t.valid_time[ig])[ig34], model_track.valid_time)
        vitals.SE34.data = interpol_nan((t.rad2[ig])[ig34] * nm2km, (t.valid_time[ig])[ig34], model_track.valid_time)
        vitals.SW34.data = interpol_nan((t.rad3[ig])[ig34] * nm2km, (t.valid_time[ig])[ig34], model_track.valid_time)
        vitals.NW34.data = interpol_nan((t.rad4[ig])[ig34] * nm2km, (t.valid_time[ig])[ig34], model_track.valid_time)

        ; Process rad=50, if needed.
        ig50 = where(t.rad[ig] eq 50, /null)
        if ig50 ne !NULL then begin
          vitals.NE50.data = interpol_nan((t.rad1[ig])[ig50] * nm2km, (t.valid_time[ig])[ig50], model_track.valid_time)
          vitals.SE50.data = interpol_nan((t.rad2[ig])[ig50] * nm2km, (t.valid_time[ig])[ig50], model_track.valid_time)
          vitals.SW50.data = interpol_nan((t.rad3[ig])[ig50] * nm2km, (t.valid_time[ig])[ig50], model_track.valid_time)
          vitals.NW50.data = interpol_nan((t.rad4[ig])[ig50] * nm2km, (t.valid_time[ig])[ig50], model_track.valid_time)
        endif

        ; Process rad=64, if needed.
        ig64 = where(t.rad[ig] eq 64, /null)
        if ig64 ne !NULL then begin
          vitals.NE64.data = interpol_nan((t.rad1[ig])[ig64] * nm2km, (t.valid_time[ig])[ig64], model_track.valid_time)
          vitals.SE64.data = interpol_nan((t.rad2[ig])[ig64] * nm2km, (t.valid_time[ig])[ig64], model_track.valid_time)
          vitals.SW64.data = interpol_nan((t.rad3[ig])[ig64] * nm2km, (t.valid_time[ig])[ig64], model_track.valid_time)
          vitals.NW64.data = interpol_nan((t.rad4[ig])[ig64] * nm2km, (t.valid_time[ig])[ig64], model_track.valid_time)
        endif

        for itag=0,n_tags(vitals)-1 do begin
          field = (tag_names(vitals))[itag]
          model_tracks[itrack] = create_struct(field, vitals.(itag).data, model_tracks[itrack])
        endfor
        model_tracks[itrack] = create_struct('origmesh', origmesh, model_tracks[itrack])
      endforeach
      return, model_tracks
    endif ; origmesh = 0
  endif ; origmesh not a string

  ; Get vitals from raw mesh (as opposed to fort.66 GFDL tracker output) - MPAS and even GFS!

  ;
  ; possible directories of raw model diagnostic files
  if ~keyword_set(model_basedirs) then $
    model_basedirs = '/glade/scratch/' + ['ahijevyc','mpasrt'] + "/" + mpas.name + "/" + init_date + '/'

  ; Look in subdirectory under date directory
  ; "rt" for Joaquin 2015 "ecic" for Harvey / Irma 2017.
  ; Alter search_str in find_matching_model_vitals.pro for fort.66 locations.
  ; Could check model_track[n].tracks_file to make sure model_basedirs is 3 directories up. (init_date/lat*/gfdl*/tcgen|tracker/

  ;model_basedirs = [model_basedirs+'rt/', model_basedirs] ; list /rt first. If multiple directories have diagnostic file, use 1st one.
  ; model_basedirs = "/glade/scratch/mpasrt/"+mpas.name+"/"+init_date+"/ecic/"


  ; tear apart rows of data in each track
  ; Put them into equal-sized 1-D vectors of times, lats, and lons
  times = !NULL
  lats  = !NULL
  lons  = !NULL

  for itrack=0,ntracks-1 do begin
    track = model_tracks[itrack]
    times = [times, track.valid_time]
    lats  = [lats, track.lat]
    lons  = [lons, track.lon]
    if track.init_date ne init_date then stop ; sanity check
  endfor
  ; get unique times in all tracks (not really needed anymore)
  ; we used to be efficient in how we ran through each diagnostic file only once
  ; even if multiple storms are in it.
  vitals_times = times[uniq(times,sort(times))]
  vitals = vitals_structure(n_elements(times))
  tc_nearestCells = mpas_nearest_cell(lons, lats, mpas) ; works with GFS too.

  ; model_files is a list of files from which the vitals were read for this set of model tracks.
  model_files = list()
  ; loop through each vitals time (a unique list of times)
  for itime = 0, n_elements(vitals_times)-1 do begin
    ; times is an ordered array of concatenated times from all the tracks (with repeats)
    ; vitals_times has no repeats
    ; vital_itimes will be used to locate the time indices that match this vitals time.
    vital_itimes = where(times eq vitals_times[itime], /null)
    fill_vitals, mpas, tc_nearestCells[vital_itimes], init_date, vitals_times[itime], vitals, vital_itimes, $
      model_basedirs, model_file=model_file, ignore_parent_id=ignore_parent_id, debug=debug, $
      lon0=lons[vital_itimes], lat0=lats[vital_itimes]
    if debug && model_file eq !NULL then print, vitals_times[itime],' model_file=', model_file
    model_files.add, model_file
  endfor

  i=0L
  for itrack=0,ntracks-1 do begin
    track = model_tracks[itrack]
    n = total(finite(track.valid_time))
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


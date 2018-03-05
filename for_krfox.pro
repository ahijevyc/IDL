; Add Dave's IDL directory to an environmental variable called IDL_PATH before starting IDL:
; setenv IDL_PATH +~ahijevyc/IDLWorkspace84/Default
; You may add this your .tcshrc file and skip this step in the future.

pro for_krfox, track_in=track_in, stormname=stormname, outfile=outfile
  ; ATCF file with MPAS track
  if ~keyword_set(track_in) then track_in = '/glade/scratch/krfox/Data/Good_Storm_Data/storm_1574.dat'
  if ~keyword_set(stormname) then stormname='NILAM'
  ; Output filename should start with an "a"
  ; "a" stands for "a-deck", next 2 letters are the basin, next 2 digits are the storm number.
  ; The rest may be anything. The basin and storm number are used in the atcf output.
  if ~keyword_set(outfile) then outfile = 'aio02.out.txt'
  ; GFDL_warmcore_only=0 means don't require warm core
  GFDL_warmcore_only = 0
  ; Minimum duration in days
  min_duration_days = 0.
  
  ; Read ATCF file with MPAS track(s)
  adeck = read_atcf(track_in, lats=lats, lons=lons, times=times, intensity=intensity, id=mcv_id)
  ; Get lat/lon of MPAS mesh.
  model = mpas_mesh('4km')
  bdeck_file = ''

  ; tracks file only has one track so itrack = 0
  itrack = 0
  init_date = adeck.init_YYYYMMDDHH[0] ; for example, '2012102000'
  init_time = julday(strmid(init_date, 4, 2), strmid(init_date, 6, 2), strmid(init_date, 0, 4), strmid(init_date, 8, 2), 0, 0)
  last_itime = max(where(finite(times[itrack,*]),/NULL))

  matching_model_track = {itrack:itrack, tracks_file:track_in, init_time:init_time, init_date:init_date, $
    early_distance_match_time:0d, id:mcv_id[itrack], $
    lon:  reform(lons[itrack,0:last_itime]),   lat:     reform(lats[itrack,0:last_itime]), $
    times:reform(times[itrack,0:last_itime]), intensity:reform(intensity[itrack,0:last_itime]), $
    model_name:model.name, bdeck_file:bdeck_file, stormname:stormname, $
    GFDL_warmcore_only: GFDL_warmcore_only, min_duration_days:min_duration_days}

  matching_model_track = add_vitals(list(matching_model_track), model, origmesh=origmesh, $
     model_basedirs='/glade/scratch/fjudt/MPAS-optimized-noPIO/4km/run/2012_10_20/ctrl_20d/', /ignore_parent_id)
  matching_model_track = matching_model_track[0] ; un-"list" matching_model_track.

  openw, atcf_lun, outfile, /get_lun
  print_atcf, atcf_lun, outfile, matching_model_track

end
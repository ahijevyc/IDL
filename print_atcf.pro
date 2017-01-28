pro print_atcf, atcf, atcf_file_in, best_model_track, debug=debug, origmesh=origmesh
  if ~keyword_set(debug) then debug=0
  if n_elements(origmesh) eq 0 then origmesh=strmatch(best_model_track.model_name, 'GFS', /fold)?0:1
  atcf_file = file_basename(atcf_file_in)
  model_abbrev = atcf_modelname(best_model_track.model_name)
  ; basin equals characters 2-3 of atcf filename
  basin = strmid(atcf_file,1,2)
  CY = strmid(atcf_file,3,2)
  fh = (best_model_track.times - best_model_track.init_time)*24
  init_date = best_model_track.init_date
  meters_per_second2knots = 1.94384
  km2nm = 0.539957
  mpas = mpas_mesh(best_model_track.model_name)
  
  for itime = 0, n_elements(best_model_track.times)-1 do begin
    if not finite(best_model_track.times[itime]) then continue
    
    lon = best_model_track.lon[itime]
    lat = best_model_track.lat[itime]
    
    NS = lat ge 0 ? 'N' : 'S'
    EW = lon ge 0 ? 'E' : 'W'
    max_spd10m = round(best_model_track.max_spd10m[itime] * meters_per_second2knots)
    min_mslp   = round(best_model_track.min_mslp[itime]/100)
    NE_spd10m  = finite(best_model_track.NE_spd10m[itime]) ? round(best_model_track.NE_spd10m[itime] * km2nm) : 0
    SE_spd10m  = finite(best_model_track.SE_spd10m[itime]) ? round(best_model_track.SE_spd10m[itime] * km2nm) : 0
    SW_spd10m  = finite(best_model_track.SW_spd10m[itime]) ? round(best_model_track.SW_spd10m[itime] * km2nm) : 0
    NW_spd10m  = finite(best_model_track.NW_spd10m[itime]) ? round(best_model_track.NW_spd10m[itime] * km2nm) : 0
    RMW        = round(best_model_track.maxr_s10m[itime] * km2nm)
    if origmesh eq 1 then begin
      ;print, "lat-lon grid:", max_spd10m, min_mslp, NE_spd10m, SE_spd10m, SW_spd10m, NW_spd10m, RMW, format='(A17,8I5)'
      ; Replace with values from original MPAS mesh, not the values GFDL tracker
      ; sees on the smoothed lat-lon grid.      ; 
      ; 
      ; COMMMENTED OUT Jan 27 2017.  THINK IT IS ALL DONE IN ADD_VITALS (CALLED BY FIND_MATCHING_MODEL_TRACK.pro)
      ; AND GET_ALL_MODEL_VITALS.PRO. CAN'T REMEMBER WHY I CALLED FILL_VITALS HERE. IT IS A 2ND TIME AS FAR AS I
      ; CAN TELL
;      data = replicate(!VALUES.D_NAN, 1)
;      vitals = { vmax:{field:'speed10',range:[0, 500],op:'max',data:data} $
;        ,     mslp: {field:'mslp',               range:[0,500],   op:'min',  data:data} $
;        ,maxr_s10m: {field:'speed10',            range:[0,500],   op:'maxr', data:data} $
;        ,NE_spd10m: {field:'speed10',            range:[0,500],   op:'NE17', data:data} $
;        ,SE_spd10m: {field:'speed10',            range:[0,500],   op:'SE17', data:data} $
;        ,SW_spd10m: {field:'speed10',            range:[0,500],   op:'SW17', data:data} $
;        ,NW_spd10m: {field:'speed10',            range:[0,500],   op:'NW17', data:data} $
;      }
;      nearestCells = mpas_nearest_cell(lon, lat, mpas)
;      fill_vitals, mpas, nearestCells, init_date, best_model_track.times[itime], vitals, model_file=model_file
;      if model_file eq '' then begin
;        print, "Can't find model file. Perhaps it aged off scratch. If you are running get_all_model_vitals.pro "
;        print, " you may need to remove the incomplete false alarm file from the tcgen directory. Stopping."
;        stop
;      endif
;      max_spd10m  = round(vitals.vmax.data * meters_per_second2knots)
;      min_mslp    = round(vitals.mslp.data/100)
;      NE_spd10m   = finite(vitals.NE_spd10m.data) ? round(vitals.NE_spd10m.data * km2nm) : 0
;      SE_spd10m   = finite(vitals.SE_spd10m.data) ? round(vitals.SE_spd10m.data * km2nm) : 0
;      SW_spd10m   = finite(vitals.SW_spd10m.data) ? round(vitals.SW_spd10m.data * km2nm) : 0
;      NW_spd10m   = finite(vitals.NW_spd10m.data) ? round(vitals.NW_spd10m.data * km2nm) : 0
;      RMW         = round(vitals.maxr_s10m.data * km2nm)
      ;print, "diagnostics mesh:", max_spd10m, min_mslp, NE_spd10m, SE_spd10m, SW_spd10m, NW_spd10m, RMW, format='(A17,8I5)'
    endif

    userdefined='gfdl_warmcore_only ddZ rain'
    dT850 = 0 & dT500 = 0 & dT200 = 0 & ddZ850200 = 0 & rainc = 0 & rainnc = 0
    depth='X' ; X-unknown
    dir = 0
    speed = 0
    stormname = CY eq 'XX' ? CY : best_model_track.stormname
    mybasin = atcf_basin(lon, lat, subregion=subregion) ; subregion used in atcf line
    
    atcf_line =STRUPCASE(basin) + ', '+ CY +', '+init_date+', 03, '+model_abbrev+', ' + $
      string(fh[itime], round(10*abs([lat, lon])), format='(i3,", ",i3,"'+NS+', ",i4,"'+EW+', ")') + $
      string(max_spd10m, min_mslp,  format='(i3,", ",i4,", XX,  34, NEQ")') + $
      string(NE_spd10m,SE_spd10m,SW_spd10m,NW_spd10m,format='(4(", ", i4))') + ', ' + $
      '   0,    0, ' + string(RMW, format='(i3,", ")') + '  0,   0, ' + $
      string(subregion, format='(A3)') + $
      ',   0, DAA, ' + $
      string(dir, speed, stormname, format='(I3,", ",I3,", ",A10,", ")') + $
      depth + ',   , NEQ,    0,    0,    0,    0, ' + $
      string(userdefined, best_model_track.GFDL_warmcore_only, dT500, dT200, $
      ddZ850200, rainc, rainnc, strcompress(strjoin(best_model_track.id,'/'),/remove_all), $
      format='(A20,", ",F5.2,", ",F5.2,", ",F5.2,", ",F7.2,", ",F7.2,", ",F7.2,", ",A,", ")')
    if n_elements(atcf) ne 0 then begin
      printf, atcf, atcf_line
      flush, atcf
    endif else print, atcf_line
  endfor
  return
end

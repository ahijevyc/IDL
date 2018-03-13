pro run_get_mean_gfs
  ; copied and pasted into microwave_temp_profile.pro to start new routine to get the mean mission profile.

  t = get_PREDICT_missions(count=nmissions)
  ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
  for imission=0,nmissions-2 do begin
    ;meanGFS_PGI44L_20100917_18.txt note for this zero-size file there is no mtm_track data for PGI44 the next day, so position can't be calculated. no GV flights that day anyway.
    pgi =  t.pgi[imission]
    date = t.yyyymmdd[imission]
    hour = strmid(t.hhmm[imission],0,2)
    ; Will Komaromi has 0823_1200_-10.1_+1.7_18_-73_27_-57_PGI30L, made possible by adding mtm_track location at 8/24 00UTC by hand, but Chris says: \
    ;This was more of an instrument test flight.  The disturbance was not a viable genesis candidate, even among non-developers. \
    ; I think we should exclude it - I am not completely confident of the sounding data in this case anyway." \
    if hour eq '15' || hour eq '21' then hour='18' ; kludge
    best_track=0
    mtm00=1
    search_radius=500.
    units = 'm'
    ofile = "/Volumes/pecan2/ahijevyc/PREDICT/MTP/meanGFS_"+pgi+"_"+date+"_"+hour+".txt"
    if file_test(ofile) eq 1 then continue ; skip if output file already exists.
    openw, mgfs, ofile, /get_lun
    for level = 0, 22000, 1 do begin
      m = get_mean_gfs(pgi, date, hour, level, best_track, mtm00, search_radius, units=units)
      printf, mgfs, level, m.t
    endfor
    free_lun, mgfs
  endfor
end

function get_mean_gfs, pgi, date, hour, level, best_track, mtm00, search_radius, units=units
  ; return the mean gfs variables within a search_radius km.
  mean_gfs = {$
    u : !VALUES.F_NAN,$
    v : !VALUES.F_NAN,$
    t : !VALUES.F_NAN,$
    td: !VALUES.F_NAN,$
    tv: !VALUES.F_NAN,$
    rh: !VALUES.F_NAN,$
    z : !VALUES.F_NAN,$
    mse:!VALUES.F_NAN $
    }
    
  t = get_gfs_soundings(pgi,date,hour,level,best_track,mtm00,debug=debug,absolute_wind=1,units=units)
  
  case units of
    'Pa' : level_mb = level/100.
    'mb' : level_mb = level
    'm'  : level_mb = mean(t.press,/nan)
    else: stop
  endcase
  us = t.udrops
  vs = t.vdrops
  zs = t.zdrops
  ts  = t.tdrops ; get_gfs_soundings returns temperature in C
  tds = t.tddrops
  rhs = t.rhdrops
  ws = replicate(0.,n_elements(tds))
  ; not sure next 2 lines are necessary because everything is already zero.
  ;  i_minus_inf = where(finite(tds, /infinity, sign=-1), n)  ; don't use complement snd ncomplement - don't want to include crap like NaNs, complement=igood, ncomplement=ngood)
  ;  if n gt 0 then ws[i_minus_inf] = 0.
  igood = where(finite(tds), ngood)
  if ngood gt 0 then ws[igood] = mixr_sat(tds[igood], replicate(level_mb,ngood)) ; mixing ratio g/kg
  
  if n_elements(search_radius) eq 1 then iradius = where(t.radiusdrops lt search_radius, nradius)
  if n_elements(search_radius) eq 2 then iradius = where(t.radiusdrops ge search_radius[0] and t.radiusdrops lt search_radius[1], nradius)
  if nradius gt 0 then begin
    mean_gfs.u  = mean(us[iradius])
    mean_gfs.v  = mean(vs[iradius])
    mean_gfs.t  = mean(ts[iradius])
    ; first average the mixing ratios
    w           = mean(ws[iradius])
    ; then calculate the relative humidity with the average mixing ratio and average temperature
    rh = w gt 0 ? mixr2rh(w, level_mb, mean_gfs.t) : 0.
    ; then calculate the dew point.
    td = rh gt 0. ? rh2tdew(mean_gfs.t, rh) : -!VALUES.F_INFINITY
    mean_gfs.rh = rh
    mean_gfs.td = td
    
    ; I don't trust t_virtual. I don't use it in cape_sound.pro - ahijevych 20120927
    ; t_virtual breaks when Td is -Inf
    ;    mean_gfs.tv = t_virtual(mean_gfs.t+!CONST.T0,level_mb,e_h2o(td+!CONST.T0))-!CONST.T0
    atmos_const ; define a system variable !atmos with physical constants - Ahijevych
    ; eventually want to use atmos_const everywhere instead of declaring them in each procedure.
    ;    eps = !ATMOS.RD/!ATMOS.RV ; commented out because in cape_sound I use rv=461.4.
    
    RV=461.5d  ; gas constant water vapor = R*/M_H2O
    RD=287.04d ; gas constant dry air = R*/M_dry
    EPS=RD/RV
    mean_gfs.tv = mean_gfs.t*(1.+w/EPS)/(1.+w)
    mean_gfs.z  = mean(zs[iradius])
    mean_gfs.mse = mean(moist_static_energy(ts[iradius]+!CONST.T0, tds[iradius]+!CONST.T0, replicate(level_mb, nradius), zs[iradius]), /nan)
  endif
  ;  if finite(mean_gfs.mse) eq 0 && level lt 17000. then stop
  return, mean_gfs
end
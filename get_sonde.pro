function get_sonde, file,  in_levels, units=units, debug=debug, $
  entrainment_rate=entrainment_rate, nocape=nocape, $
  subtract_avgTs = subtract_avgTs, _extra=_extra
  if ~keyword_set(units) then units = 'm'
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(nocape) then nocape = 0
  if ~keyword_set(entrainment_rate) then entrainment_rate = 0
  ; subtract_avgTs, if set, is the array with the temperature data to subtract
  if ~keyword_set(subtract_avgTs) then subtract_avgTs = 0
  if keyword_set(subtract_avgTs) && units ne 'm' && n_elements(subtract_avgTs) ne 22001 then begin
    print, 'you wish to subtract a temperature profile, but units must be meters and '
    print, 'the T profile is a 22001 element array from 0-22000 meters'
    stop
  endif
  
  if units eq 'Pa' then stop ; all the dropsonde files are in mb. output is in mb.
  
  
  
  if file eq '' then return, {filename:file} ; if file is empty string return an "empty" structure.
  
  
  ; May 2014 - added capability to return all data - don't define levels, and everything will be returned.
  
  
  dropyear = strmid(file_basename(file), 1, 4)
  dropmonth = strmid(file_basename(file), 5, 2)
  dropday   = strmid(file_basename(file), 7, 2)
  dropHour  = strmid(file_basename(file), 10, 2)
  dropMinute = strmid(file_basename(file), 12, 2)
  dropSecond = strmid(file_basename(file), 14, 2)
  dropJulian = julday(dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond)
  
  
  ; started removing initial dropsonde point in read_eol_sounding() on dec 3 2013
  if strmatch(file, '*.eol') then t = read_eol_sounding(file, _extra=_extra)
  if strmatch(file, '*.fil') then t = read_gempak_fil(file)
  ; Note we read the GeoPoAlt_m, not the GPSAlt.  This is true even when running microwave_temp_profile with GPSAlt specified.
  ; It's confusing.  There are two heights in MTP files (pressure altitude and GPS geometric altitude),
  ; and 2 heights in the dropsonde files (geopotential altitude and GPS geometric altitude).  We would just use GPS geometric altitude
  ; from the dropsonde files, but it is not good. GPS geometric altitude has dropouts and wasn't QC'ed according to Brigitte.
  ; Therefore, we read GeoPoAlt_m and convert it to geometric altitude with MJ's "exact" theoretical conversion (function of latitude).
  ; assign Ts, Tds, Presss, lons, and lats. Replace -999 with NaN
  ; if undefined .r replace_wNAN
  Time_secs = t.Time_sec
  Ts        = t.T
  Alts      = t.GeoPoAlt_m
  GeomAlts  = t.GPSAlt
  Tds       = t.Td
  Presss    = t.Press
  Us        = t.U
  Vs        = t.V
  lons      = t.lon
  lats      = t.lat
  
  
  ; before dealing with MTP, 19km was a ridiculous height, so I set Alts to NaN in those cases. 
  ; But as of jun 11 2014, I deal with MTP, so I commented out this section. Ahijevych
;  itoo_high = where(Alts gt 19000,ntoo_high)
;  if ntoo_high gt 0 then Alts[itoo_high] = !VALUES.F_NAN
  ;for D20100914_162309_PQC.eol drop, I found a hght>19000m near the bottom. I set it to a reasonable number by hand.
  ; 27601.24m for D20100914_153709_PQC.eol at time=733.5
  ; 14970.34m at time=840.3s and p=843.05mb for D20100903_180753_PQC.eol
  ; Kate said GPS is not QCed.  I think the crazy heights were for that.
  
  case units of
    'm' :  begin
      ; added this conversion 20130617 - Ahijevych
      ; Didn't know you had to convert from geopotential to geometric. I thought they were close enough.
      ; Don't use the GPSAlt from the dropsonde file. It has missing data and wasn't QC'ed according to Brigitte.
      ; The problem with this function is it needs geopotential and latitude. But geopotential and latitude are usually not
      ; both defined on the same level.  Just use the mean latitude. It will probably only be off by 0.2 deg latitude or 0.1 m
      ; geometric height at the most.
      ;    if geopotential_to_geometric(20000, min(lats)) - geopotential_to_geometric(20000, max(lats)) gt 2 then stop
      Alts = geopotential_to_geometric(Alts, replicate(mean(lats, /nan), n_elements(Alts)))
      raw_levels = Alts
      max_gap = 200
    end
    'press_alt_m' : begin
      ; Convert pressure to pressure altitude.  This is usually reserved for airplanes (static pressure is used), but it
      ; can be used for dropsondes too.
      Alts = press2Alt(Presss)
      raw_levels = Alts
      max_gap = 2000 ; used to be 200, but for upsondes, the soundings may be very low resolution like just significant levels, which have large separations .
    end
    'mb': begin
      raw_levels = Presss
      max_gap = 10
    end
    else: begin
      max_gap = !VALUES.F_INFINITY
    end
  endcase
  
  levels = n_elements(in_levels) gt 0 ? in_levels : raw_levels; DO NOT CHNAGE in_levels in function
  nlevels = n_elements(levels)
  dropdata = {$
    Julian         : replicate(!VALUES.D_NAN,nlevels),$
    Dsec           : replicate(!VALUES.D_NAN,nlevels),$
    u              : replicate(!VALUES.F_NAN,nlevels),$
    v              : replicate(!VALUES.F_NAN,nlevels),$
    t              : replicate(!VALUES.F_NAN,nlevels),$
    dT             : replicate(!VALUES.F_NAN,nlevels),$
    td             : replicate(!VALUES.F_NAN,nlevels),$
    tv             : replicate(!VALUES.F_NAN,nlevels),$
    p              : replicate(!VALUES.F_NAN,nlevels),$
    z              : replicate(!VALUES.F_NAN,nlevels),$
    geomAlt        : replicate(!VALUES.D_NAN,nlevels),$
    rev_parcl_dT   : replicate(!VALUES.F_NAN,nlevels),$
    pseudo_parcl_dT: replicate(!VALUES.F_NAN,nlevels),$
    lat            : replicate(!VALUES.D_NAN,nlevels),$
    lon            : replicate(!VALUES.D_NAN,nlevels),$
    liquidwater    : replicate(!VALUES.F_NAN,nlevels),$
    icewater       : replicate(!VALUES.F_NAN,nlevels),$
    rev_cape       : !VALUES.F_NAN, $
    pseudo_cape    : !VALUES.F_NAN, $
    rev_cin        : !VALUES.F_NAN, $
    pseudo_cin     : !VALUES.F_NAN, $
    rev_bmin       : !VALUES.F_NAN, $
    pseudo_bmin    : !VALUES.F_NAN, $
    slat           : !VALUES.D_NAN, $
    slon           : !VALUES.D_NAN, $
    filename       : file, $
    max_gap        : !VALUES.F_INFINITY, $
    entrain_rate   : entrainment_rate, $
    subtract_avgTs : subtract_avgTs $
  }
  
  dropdata.max_gap = max_gap
  if total(strcmp(tag_names(t),'slon',/fold_case)) eq 1 then dropdata.slon = t.slon
  if total(strcmp(tag_names(t),'slat',/fold_case)) eq 1 then dropdata.slat = t.slat
  
  
  igood = where(finite(raw_levels) and finite(Ts) and finite(Tds),ngood)
  if ngood gt 1 then begin
    mixing_ratio_kgkg = mixr_sat(Tds[igood],Presss[igood])/1000.
    ; on Jul 17 2012, cape_sound was 2x faster than cape_field
    ;  cape1 = cape_field(reform(Presss[igood],1,ngood),reform(Ts[igood]+!CONST.T0,1,ngood),mixing_ratio_kgkg)
    
    dz = min(abs(Alts-100.), /nan, iz)
    ;      if file ne '/Volumes/pecan2/ahijevyc/PREDICT/dropsondes/D20100830_121336_PQC.eol'  then return, dropdata
    ; allow MTP timeseries to skip the parcel calculations by specifying keyword nocape=1
    if nocape ne 1 then begin
      ihi2loP = reverse(sort(Presss[igood]))
      hi2loP = (Presss[igood])[ihi2loP]
      hi2loT = (Ts[igood])[ihi2loP]
      hi2loTd= (Tds[igood])[ihi2loP]
      hi2loR = mixr_sat(hi2loTd,hi2loP)/1000.
      if debug then begin
        set_plot, 'X'
        skewt, [-20.,36.9], title=file, everyT=10, everyDA=10
        plot_skewt, hi2loT, hi2loTd, hi2loP
      endif
      for ice = 1, 1 do begin
        cape_sound,hi2loP,hi2loT+!CONST.T0,hi2loR,parcel_layer_mb=50,tvrdif=rev_parcl_dT,$
          tvpdif=pseudo_parcl_dT, ice=ice, iw=iw, lw=lw, $
          caper=caper,capep=capep,entrainment_rate=entrainment_rate, cinp=cinp, cinr=cinr, bminp=bminp, bminr=bminr
        if debug then plot_skewt, (hi2loT+!CONST.T0)*(1.+hi2loR/0.622)/(1.+hi2loR)-!CONST.T0, hi2loTd, hi2loP,thick=1
        dropdata.rev_cape = max(caper, imax, /nan)
        dropdata.rev_cin  = cinr[imax]
        rev_parcl_dT = rev_parcl_dT[imax,*]
        liquidwater = LW[imax,*]; makes sense for reversible but not pseudoadiabatic
        icewater    = IW[imax,*]
        if debug then plot_skewt, hi2loT + rev_parcl_dT, hi2loTd, hi2loP, col_t=183, thick=(1+entrainment_rate/5)*2*ice
        dropdata.pseudo_cape = max(capep, imax, /nan)
        dropdata.pseudo_cin  = cinp[imax]
        pseudo_parcl_dT = pseudo_parcl_dT[imax,*]
        if debug then plot_skewt, hi2loT + pseudo_parcl_dT, hi2loTd, hi2loP, col_t=208, thick=(1+entrainment_rate/5)*2*ice
        dropdata.pseudo_bmin = bminp[imax]
        dropdata.rev_bmin = bminr[imax]
      endfor
    endif
  endif
  
  
  ; assumes avgTs[0] is for 0m , avgTs[1000] is for 1000m, etc.
  if keyword_set(subtract_avgTs) then begin
    dTs = Ts - subtract_avgTs[round(raw_levels)]
    dropdata.dT       = interpol_nan(dTs,       raw_levels, levels, max_gap=max_gap)
  endif
  
  dropdata.t        = interpol_nan(Ts,        raw_levels, levels, max_gap=max_gap)
  dropdata.Td       = interpol_nan(Tds,       raw_levels, levels, max_gap=max_gap)
  dropdata.P        = interpol_nan(Presss,    raw_levels, levels, max_gap=max_gap); Note,  Pa->mb already done
  dropdata.Z        = interpol_nan(Alts,      raw_levels, levels, max_gap=max_gap)
  dropdata.geomAlt  = interpol_nan(GeomAlts,  raw_levels, levels, max_gap=max_gap)
  if ngood gt 1 && units eq 'mb' && nocape ne 1 then begin
    dropdata.rev_parcl_dT    = interpol_nan(rev_parcl_dT, hi2loP, levels, max_gap=max_gap)
    dropdata.pseudo_parcl_dT = interpol_nan(pseudo_parcl_dT, hi2loP, levels, max_gap=max_gap)
    dropdata.liquidwater     = interpol_nan(liquidwater, hi2loP, levels, max_gap=max_gap)
    dropdata.icewater        = interpol_nan(icewater,    hi2loP, levels, max_gap=max_gap)
  endif
  
  
  dropdata.lon      = interpol_nan(lons,      raw_levels, levels, max_gap=max_gap*5)
  dropdata.lat      = interpol_nan(lats,      raw_levels, levels, max_gap=max_gap*5)
  dropdata.U        = interpol_nan(Us,        raw_levels, levels, max_gap=max_gap)
  dropdata.V        = interpol_nan(Vs,        raw_levels, levels, max_gap=max_gap)
  Time_sec          = interpol_nan(Time_secs, raw_levels, levels, max_gap=max_gap*5)
  dropdata.Dsec[0:nlevels-1]  = Time_sec
  
  
  if ~keyword_set(subtract_avgTs) then dropdata.Tv = T_virtual(dropdata.T+!CONST.T0,dropdata.P,e_h2o(dropdata.Td+!CONST.T0))-!CONST.T0
  dropdata.Julian = dropJulian + dropdata.Dsec/24d/3600d
  
  return, dropdata
  
  
end
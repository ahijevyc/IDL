pro print_atcf, atcf, best_model_track, fullcircle=fullcircle, debug=debug

  ; Mandatory arguments
  ;
  ; atcf             - logical unit number (lun) of atcf file
  ; best_model_track - track structure to write
  ;
  ; Optional keywords
  ;
  ; fullcircle - fullcircle=1 means "calculate the full circle maximum wind radii"
  ;   WINDCODE='AAA' instead of (or in addition to) the quadrants wind radii WINDCODE='NEQ'
  ;   By default, don't do it.
  ;   As of met-8.0, if both NEQ and AAA lines are present, tc_pairs/tc_stat produces
  ;   missing wind radii errors in the .tcst file.
  ;
  ; debug - run in debug mode


  atmos_const
  if ~keyword_set(debug) then debug=0
  if ~keyword_set(fullcircle) then fullcircle=0

  ; un-"list" matching_model_track.
  if isa(best_model_track,'List') then best_model_track = best_model_track[0]

  model_abbrev = atcf_modelname(best_model_track.model_name)

  ; Derive basin and storm number CY from bdeck filename
  ; BDECK_FILE should be a tag of track structure
  ; unless it's a false alarm track
  if total(tag_names(best_model_track) eq "BDECK_FILE") then begin
    bdeck_file = file_basename(best_model_track.bdeck_file)
    ; basin equals characters 2-3 of best track bdeck filename
    basin = strmid(bdeck_file, 1, 2)
    CY = strmid(bdeck_file,3,2)
  endif else begin
    ; In the case of a false alarm track, basin comes from 
    ; get_all_model_vitals.pro and storm number is 'XX'
    basin = best_model_track.basin
    CY = 'XX'
  endelse

  ; round forecast hour fh or else 0.99999 will be printed as 0.
  fh = round((best_model_track.valid_time - best_model_track.init_time)*24)
  init_date = best_model_track.init_date
  meters_per_second2knots = 1/!ATMOS.KTS2MPS
  km2nm = 0.539957
  mpas = mpas_mesh(best_model_track.model_name)

  for itime = 0, n_elements(best_model_track.valid_time)-1 do begin
    if ~finite(best_model_track.valid_time[itime]) then continue

    ; if origmesh=1 but model file doesn't exist at observed time,
    ; vmax, mslp, and RMW may be missing.
    ; Don't print the line in this case.
    ; note use of tilde ~ instead of "not". if "not" is used, it returns 254 (a true value)
    ; instead of 0, a false value. That is because "not" is a bitwise operator not a logical operator (like ~)
    if ~finite(best_model_track.max_spd10m[itime]) && ~finite(best_model_track.min_slp[itime]) then begin
      print, 'print_atcf: ',init_date, ' fh=',fh[itime], ' bad vmax and mslp. skipping line'
      continue
    endif

    lon = best_model_track.lon[itime]
    lat = best_model_track.lat[itime]

    NS = lat ge 0 ? 'N' : 'S'
    EW = lon ge 0 ? 'E' : 'W'
    max_spd10m = round(best_model_track.max_spd10m[itime] * meters_per_second2knots)
    min_slp    = round(best_model_track.min_slp[itime]/100)
    ; Print NaNs as zeros. Convert km to nm, rounding to the nearest nm.

    NE34      = 0
    SE34      = 0
    SW34      = 0
    NW34      = 0
    if total(strmatch(tag_names(best_model_track),'NE34')) then begin
      if finite(best_model_track.NE34[itime]) then NE34 = round(best_model_track.NE34[itime] * km2nm)
      if finite(best_model_track.SE34[itime]) then SE34 = round(best_model_track.SE34[itime] * km2nm)
      if finite(best_model_track.SW34[itime]) then SW34 = round(best_model_track.SW34[itime] * km2nm)
      if finite(best_model_track.NW34[itime]) then NW34 = round(best_model_track.NW34[itime] * km2nm)
    endif
    NE50      = 0
    SE50      = 0
    SW50      = 0
    NW50      = 0
    if total(strmatch(tag_names(best_model_track),'NE50')) then begin
      if finite(best_model_track.NE50[itime]) then NE50 = round(best_model_track.NE50[itime] * km2nm)
      if finite(best_model_track.SE50[itime]) then SE50 = round(best_model_track.SE50[itime] * km2nm)
      if finite(best_model_track.SW50[itime]) then SW50 = round(best_model_track.SW50[itime] * km2nm)
      if finite(best_model_track.NW50[itime]) then NW50 = round(best_model_track.NW50[itime] * km2nm)
    endif
    NE64      = 0
    SE64      = 0
    SW64      = 0
    NW64      = 0
    if total(strmatch(tag_names(best_model_track),'NE64')) then begin
      if finite(best_model_track.NE64[itime]) then NE64 = round(best_model_track.NE64[itime] * km2nm)
      if finite(best_model_track.SE64[itime]) then SE64 = round(best_model_track.SE64[itime] * km2nm)
      if finite(best_model_track.SW64[itime]) then SW64 = round(best_model_track.SW64[itime] * km2nm)
      if finite(best_model_track.NW64[itime]) then NW64 = round(best_model_track.NW64[itime] * km2nm)
    endif
    ; All-quadrant max radiii
    R34 = max([NE34,SE34,SW34,NW34])
    R50 = max([NE50,SE50,SW50,NW50])
    R64 = max([NE64,SE64,SW64,NW64])
    RMW       =  round(best_model_track.maxr_s10m[itime] * km2nm)

    ; Default userdefined string, depth, initials, direction, and speed of movement
    userdefined='min_warmcore_fract ddZ rain'
    dT850 = 0 & dT500 = 0 & dT200 = 0 & ddZ850200 = 0 & rainc = 0 & rainnc = 0
    depth='X' ; X-unknown
    initials = 'DAA'
    dir = 0
    speed = 0

    stormname = CY eq 'XX' ? CY : best_model_track.stormname
    mybasin = atcf_basin(lon, lat, subregion=subregion) ; subregion used in atcf line

    ; Build atcf line in 3 parts, a beginning, part having to do with max radius of wind thresh, and end.
    atcf_beg = STRUPCASE(basin) + ', '+ CY +', '+init_date+', 03, '+model_abbrev+', ' + $
      string(fh[itime], round(10*abs([lat, lon])), format='(i3,", ",i3,"'+NS+', ",i4,"'+EW+', ")') + $
      string(max_spd10m, min_slp, format='(i3,", ",i4,", XX,")')

    ; "fullcircle" WINDCODE or radius code is also available with the python module
    ; ~ahijevyc/lib/python2.7/atcf.py.  It finds the greatest radius in all quadrants.
    rad_windcode = "  34, NEQ"
    if NE34 + SE34 + SW34 + NW34 eq 0 then rad_windcode = "   0,    " ; zero and blank, like official bdeck
    thresh_stuff = rad_windcode + string(NE34, SE34, SW34, NW34,format='(4(", ", i4))')

    atcf_end =  ',    0,    0, ' + string(RMW, format='(i3,", ")') + '  0,   0, ' + $
      string(subregion, format='(A3)') $ ;  even though atcf web page suggests this is 1 column
      ; all the best tracks use 3 columns. 
      + ',   0, ' + $
      string(initials, format='(A3)')  + ', ' + $
      string(dir, speed, stormname, format='(I3,", ",I3,", ",A10,", ")') + $
      depth + ',  0,    ,    0,    0,    0,    0, ' + $
      string(userdefined, best_model_track.min_warmcore_fract, dT500, dT200, $
      ddZ850200, rainc, rainnc, strcompress(strjoin(best_model_track.id,'/'),/remove_all), $
      format='(A20,", ",F5.2,", ",F5.2,", ",F5.2,", ",F7.2,", ",F7.2,", ",F7.2,", ",A,", ")')

    atcf_line = atcf_beg + thresh_stuff + atcf_end

    if n_elements(atcf) ne 0 then begin
      printf, atcf, atcf_line & flush, atcf
    endif else print, atcf_line

    ; print radius of 50-knot winds line, if needed.
    if NE50 + SE50 + SW50 + NW50 gt 0 then begin
      thresh_stuff = "  50, NEQ" + string(NE50, SE50, SW50, NW50,format='(4(", ", i4))')
      atcf_line = atcf_beg + thresh_stuff + atcf_end
      if n_elements(atcf) ne 0 then begin
        printf, atcf, atcf_line & flush, atcf
      endif else print, atcf_line
    endif

    ; print radius of 64-knot winds line, if needed.
    if NE64 + SE64 + SW64 + NW64 gt 0 then begin
      thresh_stuff = "  64, NEQ" + string(NE64, SE64, SW64, NW64,format='(4(", ", i4))')
      atcf_line = atcf_beg + thresh_stuff + atcf_end
      if n_elements(atcf) ne 0 then begin
        printf, atcf, atcf_line & flush, atcf
      endif else print, atcf_line
    endif

  endfor
  return
end

function read_atcf, file, header=header, count=count, storms=uniq_storms, GFDL_warmcore_only = GFDL_warmcore_only, $
  lats=lats, lons=lons, times=times, intensity=intensity, id=id, tech=tech

  ; read ATCF files
  ; b-deck
  ; a-deck
  ; fort.66
  ; fort.64
  ; The last two are ATCF-like. Produced by GDFL vortex tracker but with extra columns.
  
  atmos_const

  if n_elements(GFDL_warmcore_only) eq 0 then GFDL_warmcore_only=1

  adeck_template = {$
    VERSION   : 1.0, DATASTART : 0L,   $
    DELIMITER : ',', MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '', $
    FIELDCOUNT: 44L,  $
    FIELDTYPES:    [7,7,7,4,7,4,7,7,4,4, $
    7,4,7,4,4,4,4,4,4,4, $
    4,4,7,4,7,4,4,7,7,4, $
    7,4,4,4,4,7,7,7,7,7, $
    7,7,7,0] ,  $
    FIELDNAMES:   ['BASIN', 'CY', 'init_YYYYMMDDHH', 'TECHNUM', 'TECH', 'TAU', 'LATNS', 'LONEW', 'VMAX', 'MSLP', $
    'TY', 'RAD', 'WINDCODE', 'RAD1', 'RAD2', 'RAD3', 'RAD4', 'RADP', 'RRP', 'MRD', $
    'GUSTS', 'EYE', 'SUBREGION', 'MAXSEAS','INITIALS','DIR','SPEED','STORMNAME','DEPTH','SEAS',$
    'SEASCODE', 'SEAS1','SEAS2','SEAS3','SEAS4','USERDEFINED'+strtrim(sindgen(8),2),'empty'],$
    FIELDLOCATIONS: indgen(44),$
    FIELDGROUPS: indgen(44)}

  ; slight difference for fort.66 format
  fort66_template = {$
    VERSION   : 1.0, DATASTART : 0L,   $
    DELIMITER : ',', MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '', $
    FIELDCOUNT: 31L,  $
    FIELDTYPES:    [7,7,7,7,4,7,4,7,7,4, $
    4,7,4,7,4,4,4,4,4,4, $
    4,4,4,4,7,4,4,4,4,4, $
    4], $
    FIELDNAMES:   ['BASIN', 'CY', 'stormname', 'init_YYYYMMDDHH', 'TECHNUM', 'TECH', 'TAU', 'LATNS', 'LONEW', 'VMAX', 'MSLP', $
    'TY', 'RAD', 'WINDCODE', 'RAD1', 'RAD2', 'RAD3', 'RAD4', 'RADP', 'RRP', 'MRD', $
    'PARAMB', 'TWLOWER', 'TWUPPER', 'WARMCORE', 'DIR', 'STMSPD', 'MEANZETA850', 'MAXZETA850','MEANZETA700','MAXZETA700'], $
    FIELDLOCATIONS: indgen(31),$
    FIELDGROUPS:  indgen(31)}

  ; slight difference for fort.64 format ('tracker' mode, not 'tcgen')
  fort64_template = {$
    VERSION   : 1.0, DATASTART : 0L,   $
    DELIMITER : ',', MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '', $
    FIELDCOUNT: 43L,  $
    FIELDTYPES:    [7,7,7,4,7,4,7,7,4,4, $
    7,4,7,4,4,4,4,4,4,4, $
    7,7,7,7,7,7,7,7,7,7, $
    7,7,7,7,7,7,4,4,4,7, $
    4,7,7], $
    FIELDNAMES:   ['BASIN', 'CY', 'init_YYYYMMDDHH', 'TECHNUM', 'TECH', 'TAU', 'LATNS', 'LONEW', 'VMAX', 'MSLP', $
    'TY', 'RAD', 'WINDCODE', 'RAD1', 'RAD2', 'RAD3', 'RAD4', 'RADP', 'RRP', 'MRD', $
    'c'+strtrim(21+indgen(16),2), 'PARAMB', 'TWLOWER', 'TWUPPER', 'WARMCORE', 'WARMCOREK', 'c42','c43'], $
    FIELDLOCATIONS: indgen(43),$
    FIELDGROUPS:  indgen(43)}

  b = ''

  ; Return !NULL if file doesn't exist
  if not file_test(file) then begin
    print, 'read_atcf: ',file, ' does not exist!'
    return, !NULL
  endif

  ; Return !NULL if zero-size
  if file_test(file, /zero) then begin
    print, 'read_atcf: ',file, ' is zero size!'
    return, !NULL
  endif

  openr, lun, file, /get_lun
  readf, lun, b
  free_lun, lun
  words = strsplit(b,',',/extract)
  matches_fort66 = strmatch(b, '*, *, ????????00_F???_????_?????_*,*') eq 1 || file_basename(file) eq 'fort.66'
  matches_fort64 = strmatch(b, 'THERMO PARAMS,') eq 1 || file_basename(file) eq 'fort.64' || n_elements(words) eq 23

  template = adeck_template
  if matches_fort66 then template = fort66_template
  if matches_fort64 then template = fort64_template
  if not matches_fort66 and not matches_fort64 then begin
    ;print, 'read_atcf does not recognize ',file,' as fort.64 or fort.66 format'
    ;print, 'assuming adeck_template will work for read_ascii()'
  endif
  if not matches_fort66 and not matches_fort64 and GFDL_warmcore_only then begin
    print, 'GFDL_warmcore_only set but no warm core column in ATCF file', file
    stop
  endif

  t = read_ascii(file, template=template, header=header, count=count)
  t = dictionary(t)

  if matches_fort64 then begin
    storm_spd_ms = replicate(!VALUES.F_NAN, count)
    stormname = replicate('', count)
    t['stormname'] = stormname
  endif else begin
    ; fort.66 storm speed in tenths of meters per second and adeck in knots.
    storm_spd_ms = matches_fort66 ? t.stmspd/10. : t.speed * !ATMOS.kts2mps

    ibad = where(strmatch(t.stormname, '*   NaN') ne 0, nbad)
    if nbad gt 0 then t['stormname', ibad] = strtrim(t.cy[ibad], 2)
  endelse


  ; If tech is defined just get that model. tech is another word for model name.
  if keyword_set(tech) then begin
    i = where(t.tech eq tech, /null)
    foreach x,t,key do t[key] = t[key,i]
  endif


  yyyy = strmid(t.init_YYYYMMDDHH, 0, 4)
  mm   = strmid(t.init_YYYYMMDDHH, 4, 2)
  dd   = strmid(t.init_YYYYMMDDHH, 6, 2)
  hh   = strmid(t.init_YYYYMMDDHH, 8, 2)
  initdate = julday(mm, dd, yyyy, hh, 0,0)
  istars=where(t.latns eq '***S',nstars,/null)
  if nstars gt 0 then print, '***S found for latitude in '+file+'. will cause type conversion error.'
  lat = double(t.latns)/10. ; once I got ***S - caused by interpol when interpolating between joined tracks.
  lon = double(t.lonew)/10. ; it caused a type conversion error here and got set to zero. interpol_nan solved problem.
  i = where(strmid(t.latns, 0, /reverse_offset) eq 'S', n)
  if n gt 0 then lat[i] = -lat[i]
  i = where(strmid(t.lonew, 0, /reverse_offset) eq 'W', n)
  if n gt 0 then lon[i] = -lon[i]
  t['yyyy'] = yyyy
  t['mm'] = mm
  t['dd'] = dd
  t['hh'] = hh
  t['julday'] = initdate + t.tau/24d
  t['lat'] = lat
  t['lon'] = lon
  t['storm_spd_ms'] = storm_spd_ms

  ; remove other maximum radius of winds besides 34 knots.
  ; Maybe I'll need these down the line but they have caused more trouble than they are worth
  i34 = where(t.rad eq 0 or t.rad eq 34, n34, /null) ; had "and warmcore_check" for a while but it stripped all cold-core times! not wanted.
  foreach x,t,key do t[key] = t[key,i34]

  ; sort by basin + storm number (CY)
  ; Added basin Dec 11 2016. to allow two storms with same storm number
  ; in different basins.
  isort = sort(t.basin+t.cy)
  foreach x,t,key do t[key] = t[key, isort]


  ; filter out repeats in bdecks and adecks
  ; used to not filter out repeats in fort.64 and fort.66, but why not? You certainly don't need erroneous repeats
  ; and you don't even need the multiple wind threshold lines.  Fixed Mar 14, 2016
  ; used to not have t.tau. but need it for reading my adeck files. added 20151130
  ; used to not have t.basin either but need it to differentiate storms in different basins with same storm number
  iuniq = uniq(t.basin + t.cy + t.init_YYYYMMDDHH + strtrim(t.tau,2))
  foreach x,t,key do t[key] = t[key, iuniq]



  ; stormnames in fort.66 (tcgen mode) are not unique. especially when they are near 0N,0E! Add the cy string.
  ; As of Mar 2016, I noticed with fort.64 'tracker' mode output, you have to go beyond adding cy string. 
  ; fort.64 doesn't have a 1-letter basin abbreviation attached to cy. In other words,
  ; fort.66 would have something like '08L', but fort.64 has '08'.  You could have
  ; concurrent storms in different basins with same cy. Better add basin. (Mar 18, 2016)
  storms =  matches_fort66 ? t.cy + t.stormname : t.init_YYYYMMDDHH + t.basin + t.cy
  uniq_storms = storms[uniq(storms,sort(storms))]


  nstorms=n_elements(uniq_storms)
  ; sorted by t.cy now , not t.tau. use sort()
  max_time_dim_size=n_elements(t.tau[uniq(t.tau, sort(t.tau))])

  lats              = replicate(!VALUES.D_NAN, nstorms, max_time_dim_size)
  lons              = replicate(!VALUES.D_NAN, nstorms, max_time_dim_size)
  times             = replicate(!VALUES.D_NAN, nstorms, max_time_dim_size) ; not just start time. all times for an MCV.
  intensity         = replicate(!VALUES.F_NAN, nstorms, max_time_dim_size)
  id                = replicate('',nstorms)

  jmcv =0L

  for istorm=0,nstorms-1 do begin
    ; it is nice to save the gfdl storm id. it is derived from the jmcv index.
    stormid = uniq_storms[istorm]
    ; some times have multiple entries because of wind radii stats
    ; some are just repeated by mistake (e.g. bcp012015.dat lines 3-4, lines 5-6)
    ; some are repeated with slightly different lat/lons (e.g. bep072014.dat lines 23-24, 25-26, 52-53)
    ; see if there are any repeating valid times (julday) with the same storm name.
    ; This check is not redundant because there are errors in the bdecks.  Even though you
    ; only selected the radius of >34 wind lines, there still may be repeats.

    ; If you filter out repeats here, you also have to filter out repeats in t. structure above.
    ;  Mar 2016,... ; Used to do a shifted comparison to filter out repeats if nstorms > 1 and hard-coded
    ; icy=0L and nt=1 if nstorms=1.  But this doesn't make sense. Why would you only want first time?
    ; I guess this was never a problem when looking at tcgen output (fort.66) because you always had more
    ; than one unique storm. But when you look at 'tracker' output (fort.64) you can have only one storm in the file.
    ; Why even bother with a filter?  It is does already on the t. structure above. Mar 14 2016.
    ; if nstorms eq 1 then begin  ; shifted comparison won't work with 1 element
    ;   icy = 0L
    ;   nt = 1
    ; endif else icy = where(storms eq stormid and (storms ne shift(storms,1) or t.julday ne shift(t.julday,1)), nt, /null)
    icy = where(storms eq stormid, nt, /null)
    if icy eq !NULL then stop
    ; Only fill in lat/lon/times/intensity/id if there is at least one warmcore = 'Y'
    if GFDL_warmcore_only eq 1 && total(t.warmcore[icy] eq 'Y') eq 0 then continue
    lats[jmcv,0:nt-1] = t.lat[icy]
    lons[jmcv,0:nt-1] = t.lon[icy]
    times[jmcv,0:nt-1] = t.julday[icy]
    intensity[jmcv,0:nt-1] = t.vmax[icy]; use maximum wind speed [kts] as intensity. !VALUES.F_INFINITY
    id[jmcv]=stormid
    jmcv=jmcv+1
  endfor

  return, t

end

function read_atcf_test, file, header=header, count=count, storms=uniq_storms, GFDL_warmcore_only = GFDL_warmcore_only, $
  lats=lats, lons=lons, times=times, intensity=intensity, id=id
  
  
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
    
    
  b = ''
  openr, lun, file, /get_lun
  readf, lun, b
  free_lun, lun
  matches_fort66 = strmatch(b, '*, *, ????????00_F???_????_?????_*,*') || file_basename(file) eq 'fort.66'
  
  template = matches_fort66 ? fort66_template : adeck_template
  
  ; Return !NULL if zero-size
  if file_test(file, /zero) then begin
    print, 'read_atcf: ',file, ' is zero size!'
    return, !NULL
  endif
  t = read_ascii(file, template=template, header=header, count=count)
  
  ; fort.66 storm speed in tenths of meters per second and adeck in knots.
  storm_spd_ms = matches_fort66 ? t.stmspd/10. : t.speed * !ATMOS.kts2mps
  
  
  ibad = where(strmatch(t.stormname, '*   NaN') ne 0, nbad)
  if nbad gt 0 then t.stormname[ibad] = strtrim(t.cy[ibad], 2)
  
  
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
  t = create_struct('yyyy', yyyy, 'mm', mm, 'dd', dd, 'hh', hh, 'julday', initdate + t.tau/24d, $
    'lat', lat, 'lon', lon, 'storm_spd_ms', storm_spd_ms, t)
    
  ;  ; remove other maximum radius of winds besides 34 knots.
  ;  ; Maybe I'll need these down the line but they have caused more trouble than they are worth
  ;  ; Can't truncate an array in a structure (t). That's why I created a new structure t2.
  ;  t2 = {}
  ;  tnames = tag_names(t)
  ;  i34 = where(t.rad eq 0 or t.rad eq 34, n34, /null) ; had "and warmcore_check" for a while but it stripped all cold-core times! not wanted.
  ;  for itag = 0,n_tags(t)-1 do begin
  ;    t2 = create_struct(tnames[itag], (t.(itag))[i34], t2)
  ;  endfor
  ;  t = t2
  ;
  ;  ; sort by storm number (CY)
  ;  isort = sort(t.cy)
  ;  for itag = 0,n_tags(t)-1 do begin
  ;    t.(itag) = (t.(itag))[isort]
  ;  endfor
  
  storms = t.cy + t.stormname
  uniq_storms = storms[uniq(storms,sort(storms))]
  if not matches_fort66 then begin
    ; bdeck stormnames are not unique. especially when they are near 0N,0E! Add the cy string.
    storms = t.init_YYYYMMDDHH + t.cy
    ; some times have multiple entries because of wind radii stats
    ; some are just repeated by mistake (e.g. bcp012015.dat, lines 3-4, lines 5-6)
    ; see if there are any repeating valid times (julday) with the same storm name.
    ; This check is not redundant because there are errors in the bdecks.  Even though you
    ; only selected the raduis of >34 wind lines, there still may be repeats.
    
    ; sort by storm id
    ius   = uniq(storms,sort(storms))
    storms = storms[ius]
    t2 = {}
    tnames = tag_names(t)
    for itag = 0,n_tags(t)-1 do begin
      t2 = create_struct(tnames[itag], (t.(itag))[ius], t2)
    endfor
    t = t2
    uniq_storms = storms[ius]
  endif

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

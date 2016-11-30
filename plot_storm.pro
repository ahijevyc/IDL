pro run_plot_storm, forcenew = force_new
  files = file_search("/glade/p/work/ahijevyc/tracking_gfdl/adeck/*/vmax_thresh_kt.ge.00*/acp022015*[ym]", count=nfiles)
  for ifile=0,nfiles-1 do plot_storm, files[ifile], ofile=files[ifile]+'.png', /buffer, force_new=force_new
end

pro plot_storm, adeck_file, bdeck_file=bdeck_file, model=model, ofile=ofile, $
  title=title, buffer=buffer, force_new=force_new
  ; Plot TC track and matching model tracks for one storm.
  ; Plot TC intensity and matching model intensities too.
  ; Input
  ; a-deck file
  ; Optional
  ; b-deck file (default is derived from a-deck file name)
  ; model (default is the first model in the a-deck file).

  if ~keyword_set(bdeck_file) then bdeck_file = '/glade/p/work/ahijevyc/atcf/b'+strmid(file_basename(adeck_file),1,8)+'.dat'

  ;
  ; Read entire atcf file
  ;  adeck structure will have a 1-D array for each atcf column with lengths equal
  ;  to # of non-duplicated lines. (not used for some reason)
  ;  mcv_lats, mcv_lons, mcv_times, and mcv_intensity, and mcv_id are 2-d arrays that
  ;  will be used below.
  ; choose GFDL_warmcore_only=0 because a-decks don't have a warm core column.
  adeck = read_atcf(adeck_file, lats=lats, lons=lons, times=times, $
    intensity=intensities, id=ids, GFDL_warmcore_only=0) ; adeck atcf in knots and nautical miles

  if ~keyword_set(model) then model = adeck.tech[0]
  if n_elements(buffer) eq 0 then buffer = 0
  if ~keyword_set(ofile) then ofile = 't.png'
  if n_elements(force_new) eq 0 then force_new = 0
  if file_test(ofile) eq 1 and not force_new then begin
    print, "found "+ofile+". skipping"
    return
  endif
  
  if ~keyword_set(title) then title = file_dirname(adeck_file)+"!C"+file_basename(adeck_file)

  ii = where(adeck.tech ne model, /null)
  if ii ne !null then stop

  obs = read_atcf(bdeck_file, GFDL_warmcore_only=0) ; no warm core column in b-deck best track obs.
  ; if BDECK, use 'BEST', otherwise use 'CARQ', unless it's w. pacific--then use 'WRNG'
  if strmid(file_basename(bdeck_file),0,1) eq 'b' then tech = 'BEST' else if strpos(file_basename(bdeck_file), 'wp') ne -1 then tech = 'WRNG' else tech = 'CARQ'
  iobs = where(obs.TECH eq tech and obs.TAU eq 0, nobs)
  if nobs eq 0 then stop
  max_vmax=max(obs.vmax) ; storm name taken from the time with greatest vmax.
  ; there can be multiple times with maximum wind. use the latest one. This helps al082014 (HANNA)
  ; it is only INVEST for the first time it reaches max wind speed but becomes HANNA later.
  imax = max(where(obs.vmax eq max_vmax))
  stormname = (obs.stormname)[imax]
  ; Require best track is TS status at some point. ; added Mar 2016.
  ; We don't want Tropical depression sixteen in EP 2014. It never gets named but it is
  ; in the best track database for some reason. We don't want it in our study.
  ; If you do include it, ep15 Major Hurricane ODILE tracks will also match it, which you don't want..
  if max_vmax lt 34 then begin
    print, stormname + ' is not TS at some point. skipping. '
    return
  endif
  observed_times = obs.julday[iobs]
  obs_first = min(observed_times)
  obs_last  = max(observed_times)
  observed_lons = obs.lon[iobs]
  observed_lats = obs.lat[iobs]
  observed_vmax = obs.vmax[iobs]
  ; Fix dateline straddle for observed track & model tracks
  o = [observed_lons, lons[*]] ; make 1-D array of longitudes
  ineg = where(o lt 0, /null)
  if ineg ne !NULL and (max(o) gt 90 or (min(o) lt -179 and max(o) lt -90)) then o[ineg] = o[ineg] + 360
  limit = [min([observed_lats,min(lats)])-2.5, min(o)-1, max([observed_lats,max(lats)])+1, max(o)+1]
  map = map('Cylindrical', limit=limit, font_size=10, fill_color='light blue', $
    margin=[0.08,0.08,0.2,0.15], title=title, layout=[1,2,1], buffer=buffer)
  grid = map.MAPGRID & grid.thick=0 & grid.linestyle = 'dotted' & grid.LABEL_POSITION = 0
  legend_items = list()
  track_colors = ['red','blue','green', 'gold', 'light sea green', 'cyan', 'salmon', $
    'pale green', 'silver','yellow','medium orchid','purple','orange','brown','indigo',$
    'wheat','lime','crimson']

  m1 = mapcontinents(fill_color='beige'); tried to run this before tracks but it would omit N. Japan & E. Russia!
  ; even tried variants of /continents, /countries, /hires. IDL is a joke now.
  crud = {sym_filled:1, transparency:0.3, linestyle:'solid', symbol:"circle", $
    sym_transparency:0.3}

  lineplot = plot(observed_times, observed_vmax, layout=[1,2,2], ytitle='knots', $
    sym_size=0.69, thick=1.5, name=stormname, _extra=crud, xtitle='Date', $
    title=stormname+" "+tech+" and "+model+" tracks intensity", /current, $
    xrange=[min(times),max(times)])


  map.Refresh, /disable
  lineplot.Refresh, /disable

  foreach id, ids, i do begin
    init_date = strmid(id,0,10)
    color = track_colors[(legend_items.count()-1) mod n_elements(track_colors)]
    thick = 1.1
    sym_size = 0.51
    legend_items.add, plot(lons[i,*], lats[i,*], /data, _extra=crud, thick=thick, color=color, $
      sym_size=sym_size, name=init_date, overplot=map)
    atimes = times[i,*]
    caldat, atimes, m, d, y, h
    day_str = replicate('', n_elements(atimes))
    ii = where(finite(atimes) and h eq 0, /null)
    if ii ne !NULL then day_str[ii] = string(atimes[ii], format='(C(CDI0))')
    junk = symbol(lons[i,*], lats[i,*], /data, label_string=day_str,$
      label_color=contrasting_color(color),label_position='C', label_font_size=3.3)

    junk = plot(times[i,*], intensities[i,*], overplot=lineplot, _extra=crud, $
      sym_size=sym_size, thick=thick, color=color, name=init_date)
  endforeach
  lineplot.Order, /BRING_TO_FRONT
  observed_track = plot(observed_lons, observed_lats, /data, _extra=crud, $
    sym_size=lineplot.sym_size, thick=lineplot.thick, color=lineplot.color, $
    overplot=map, name=stormname)

  legend_items.add, observed_track, 0 ; insert at the beginning of list

  day_str = replicate('', n_elements(observed_times))
  caldat, observed_times, m, d, y, h
  ii = where(h eq 0, /null)
  if ii ne !NULL then day_str[ii] = string(observed_times[ii], format='(C(CDI0))')
  obs_days = symbol(observed_lons, observed_lats, /data, label_string=day_str, $
    label_color='white', label_position='C', label_font_size=3.7, target=map)
  ; tried target=map and /relative but put in lower left corner of window
  symbol_label_explan = text((map.position)[0]+0.003, (map.position)[1]+0.004, $
    'Tracks labeled with day of month at 0 UTC', font_size=6)
  font_size = 4 > (110/legend_items.length) < 9 ; between 4 and 9
  l = legend(target=legend_items,font_size=font_size,vertical_alignment=0.65, shadow=0, $
    horizontal_alignment='right')
  l.position = [0.998,0.75]; doesn't work when called with this as keyword.
  map.Refresh
  pretty_TC_intensity, lineplot

  junk = timestamp_text(target=lineplot)
  l.window.save, ofile, resolution=210
  print, "created "+ofile
  l.window.close

end
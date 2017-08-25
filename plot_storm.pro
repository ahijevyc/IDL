pro run_plot_storm, forcenew = force_new
  files = file_search("/glade/p/work/ahijevyc/tracking_gfdl/adeck/*/*/a????201*[ym]", count=nfiles)
  for ifile=0,nfiles-1 do plot_storm, files[ifile], ofile=files[ifile]+'.png', /buffer, force_new=force_new
end

pro plot_storm, adeck_file, bdeck_file=bdeck_file, model=model, ofile=ofile, $
  title=title, buffer=buffer, force_new=force_new, get_origmesh= get_origmesh
  ; Plot TC track and matching model tracks for one storm.
  ; Plot TC intensity and matching model intensities too.
  ; Input
  ; a-deck file
  ; Optional
  ; b-deck file (default is derived from a-deck file name)
  ; model (default is the first model in the a-deck file).

  if ~keyword_set(bdeck_file) then bdeck_file = '/glade/p/work/ahijevyc/atcf/b'+strmid(file_basename(adeck_file),1,8)+'.dat'
  if strmid(file_basename(bdeck_file),0,1) ne 'b' then begin
    print, "b-deck file "+bdeck_file+" does not begin with 'b'. are you sure about it?"
    stop
  endif
  ;
  ; Read entire atcf file
  ;  adeck structure will have a 1-D array for each atcf column with lengths equal
  ;  to # of non-duplicated lines. (not used for some reason)
  ;  mcv_lats, mcv_lons, mcv_times, and mcv_intensity, and mcv_id are 2-d arrays that
  ;  will be used below.
  ; choose GFDL_warmcore_only=0 because a-decks don't generally have a warm core column.
  ; I added a custom column but only after Nov 2016.
  GFDL_warmcore_only = 0
  adeck = read_atcf(adeck_file, GFDL_warmcore_only=GFDL_warmcore_only) ; adeck atcf in knots and nautical miles
  if adeck.IsEmpty() then return

  if ~keyword_set(model) then model = mpas_mesh(adeck.tech[0])
  if n_elements(buffer) eq 0 then buffer = 1
  if ~keyword_set(ofile) then ofile = file_dirname(adeck_file)+'/'+file_basename(adeck_file,'.dat')+'.png'
  if n_elements(force_new) eq 0 then force_new = 0
  if file_test(ofile) eq 1 and not force_new then begin
    print, "found "+ofile+". skipping"
    return
  endif

  if ~keyword_set(title) then title = file_dirname(adeck_file)+"!C"+file_basename(adeck_file)
  if ~keyword_set(get_origmesh) then get_origmesh = 0

  ii = where(adeck.tech ne model.name, /null)
  if ii ne !null then begin
    print, 'adeck.tech not equal to model.name'
    print, 'adeck tech = ' + adeck.tech[0] +', model.name=' + model.name
  endif

  lons = adeck.lons[*] ; needed to avoid wrapping lines when straddling map limits
  lats = adeck.lats[*]
  ; if BDECK, use 'BEST', otherwise use 'CARQ', unless it's w. pacific--then use 'WRNG'
  if strmid(file_basename(bdeck_file),0,1) eq 'b' then tech = 'BEST' else if strpos(file_basename(bdeck_file), 'wp') ne -1 then tech = 'WRNG' else tech = 'CARQ'
  obs = read_atcf(bdeck_file, GFDL_warmcore_only=0, tech=tech) ; no warm core column in b-deck best track obs.
  if not obs.IsEmpty() then begin
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
      print, storm_id + ' ' + stormname + ' is not TS at some point. skipping. '
      return
    endif
    obs_first = min(obs.julday)
    obs_last  = max(obs.julday)
    ; Fix dateline straddle for observed track & model tracks
    lons = [obs.lon, lons] ; make 1-D array of longitudes
    lats = [obs.lat, lats]
  endif

  ineg = where(lons lt 0, /null)
  if ineg ne !NULL and (max(lons) gt 90 or (min(lons) lt -179 and max(lons) lt -90)) then lons[ineg] = lons[ineg] + 360
  limit = [min(lats)-2.5, min(lons)-1, max(lats)+1, max(lons)+1]
  ; for JOAQUIN cut 40° from east and 20° from north
  ; limit = [limit[0],limit[1],limit[2]-8,limit[3]-40]
  map = map('Cylindrical', limit=limit, font_size=10, fill_color=[235,235,242], $
    margin=[0.08,0.08,0.2,0.15], title=title, layout=[1,2,1], buffer=buffer)
  grid = map.MAPGRID & grid.thick=1 & grid.color='white'& grid.label_color='black'& grid.LABEL_POSITION = 0
  grid.Order, /send_to_back
  legend_items = list()
  track_colors = list('red','blue','green', 'gold', 'light sea green', 'cyan', 'salmon', $
    'pale green', 'silver','yellow','medium orchid','purple','orange','brown','indigo',$
    'wheat','lime','crimson')
  ; got these RGB triplets from from seaborn module in python
  ; import seaborn as sns
  ; import numpy as np
  ; np.array((sns.color_palette("Set1",n_colors=9))) * 255
  track_colors = list([ 228.00000161,   26.00000035,   28.00000023],$
    [  55.08627504,  126.19215697,  183.56863168],$
    [  77.58823833,  174.23922044,   74.69804242],$
    [ 153.21177077,   78.5764735 ,  161.08235836],$
    [ 255.        ,  129.00784317,    0.80000001],$
    [ 253.25490206,  251.68627456,   50.7843145 ],$
    [ 167.90588754,   87.0117673 ,   43.55294265],$
    [ 244.41960847,  129.658831  ,  189.95686662],$
    [ 153.00000608,  153.00000608,  153.00000608])
  m1 = mapcontinents(fill_color='beige', thick=0.5); tried to run this before tracks but it would omit N. Japan & E. Russia!
  ; even tried variants of /continents, /countries, /hires. IDL is a joke now.
  crud = {sym_filled:1, transparency:0.3, linestyle:'solid', symbol:"circle", $
    sym_transparency:0.3}

  lineplot = plot(obs.julday, obs.mslp, layout=[1,2,2], ytitle='hPa', $
    sym_size=0.69, thick=1.5, name=stormname, _extra=crud, xtitle='Date', $
    title=stormname+" "+tech+" and "+model.name+" tracks intensity", /current, $
    xrange=[min(adeck.times),max(adeck.times)])

  lineplot.Refresh, /disable
  map.Refresh, /disable

  foreach id, adeck.id, i do begin
    i201 = strpos(id,'201',3)
    init_date = strmid(id,i201,10)
    init_time = julday(strmid(init_date,4,2), strmid(init_date,6,2), strmid(init_date,0,4), strmid(init_date,8,2), 0 , 0)
    stid = strmid(id,0,i201)
    itrack_color = legend_items.count() mod track_colors.count()
    track_name = init_date
    color = track_colors[itrack_color]
    thick = 1.1
    sym_size = 0.51
    ; Replaced * with ifinite to avoid  arithmetic error: Floating illegal operand
    ifinite = where(finite(adeck.lons[i,*]), ntimes, /null)
    min_duration_days = 0
    if n_elements(adeck.id) gt 40 then begin ; ran this block on Ryder's 4-km tracks to filter out short, non-tropical tracks
      track_name = stid ; good for multiple storms
      min_duration_days = 2
      if ntimes lt min_duration_days * 8 then begin
        print, id + string(format='(i3)',ntimes)+ " times too short, not plotting"
        continue
      endif
      if min(abs(adeck.lat[i,ifinite])) gt 33. then begin
        print, id + " poleward of 33, not plotting"
        continue
      endif
    endif
    if max(adeck.vmax[i,ifinite]) lt 25. then begin
      print, id + " max vmax < 25, not plotting"
      continue
    endif
    model_track = {tracks_file:adeck_file, init_time:init_time, init_date:init_date, $
      lon: adeck.lons[i,ifinite], lat: adeck.lats[i,ifinite], times:adeck.times[i,ifinite]}

    legend_items.add, plot(model_track.lon, model_track.lat, /data, _extra=crud, thick=thick, color=color, $
      sym_size=sym_size, name=track_name, overplot=map)
    atimes = model_track.times
    caldat, atimes, m, d, y, h

    ; label tracks with day of month at 0 UTC.
    day_str = replicate('', n_elements(atimes))
    ii = where(finite(atimes) and h eq 0, /null)
    if ii ne !NULL then day_str[ii] = string(atimes[ii], format='(C(CDI0))')
    junk = symbol(model_track.lon, model_track.lat, /data, label_string=day_str,$
      label_color=contrasting_color(color),label_position='C', label_font_size=2.)

    toplot = adeck.mslp[i,ifinite]
    if get_origmesh then begin
      model_track = add_vitals(list(model_track), model, origmesh=get_origmesh)
      str = " from original mesh"
      if strpos(lineplot.title.string, str) eq -1 then lineplot.title.string = lineplot.title.string + str
      model_track = model_track[0]
      toplot = model_track.max_spd10m*1.94384
      toplot = model_track.min_slp/100
    endif

    junk = plot(atimes, toplot, overplot=lineplot, _extra=crud, $
      sym_size=sym_size, thick=thick, color=color, name=init_date)
  endforeach
  ; bring observed best track time series to front (other time series called 'junk')
  lineplot.Order, /BRING_TO_FRONT
  if not obs.IsEmpty() then begin
    observed_track = plot(obs.lon, obs.lat, /data, _extra=crud, $
      sym_size=lineplot.sym_size, thick=lineplot.thick, color=lineplot.color, $
      overplot=map, name=stormname)

    legend_items.add, observed_track, 0 ; insert at the beginning of list
    day_str = replicate('', n_elements(obs.julday))
    caldat, obs.julday, m, d, y, h
    ii = where(h eq 0, /null)
    if ii ne !NULL then day_str[ii] = string(obs.julday[ii], format='(C(CDI0))')
    obs_days = symbol(obs.lon, obs.lat, /data, label_string=day_str, $
      label_color='white', label_position='C', label_font_size=3.7, target=map)
    ; tried target=map and /relative but put in lower left corner of parent window

  endif

  if 0 then begin
    t = map.limit
    t = t + [3, 0, -5, -40]
    map.limit = t
    t = lineplot.xrange
    t = t + [8, -5]
    lineplot.xrange = t
    obs_days.label_font_size=2.5
  endif

  symbol_label_explan = text((map.limit)[1], (map.limit)[0], /DATA, $
    'Tracks labeled with day of month at 0 UTC', font_size=6)
  ; tried target=map and /relative but put in lower left corner of parent window
  font_size = 4 > (110/legend_items.length) < 9 ; between 4 and 9
  l = legend(target=legend_items,font_size=font_size,vertical_alignment=0.65, shadow=0, $
    horizontal_alignment='right')
  l.position = [0.998,0.75]; doesn't work when called with this as keyword.



  map.Refresh
  pretty_TC_intensity, lineplot

  junk = timestamp_text(target=lineplot)
  l.window.save, ofile, resolution=210
  print, "created "+ofile
  if buffer then l.window.close else stop

end
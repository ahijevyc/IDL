pro run_plot_storm, forcenew = force_new
  files = file_search("/glade/p/work/ahijevyc/tracking_gfdl/adeck/uni/tcgen/a????2017*[ym]", count=nfiles)
  files = file_search("/glade/u/home/ahijevyc/aal112017.ens_*.dat", count=nfiles)
  for ifile=0,nfiles-1 do plot_storm, files[ifile], ofile=files[ifile]+'.png', /buffer, force_new=force_new, $
    output_atcf=file_dirname(files[ifile])+"/"+file_basename(files[ifile],"dat")+"origmeshTrue.dat",$
    /get_origmesh, model=mpas_mesh('hwt2017'); , parent_id='qn8h9pp21e.66hrbdndmz')
end

function day_str, atimes
  caldat, atimes, m, d, y, h

  ; label points with day of month at 0 UTC.
  day_str = replicate('', n_elements(atimes))
  ii = where(finite(atimes) and h eq 0, /null)
  if ii ne !NULL then day_str[ii] = string(atimes[ii], format='(C(CDI0))')
  return, day_str
end

pro plot_storm, adeck_file, bdeck_file=bdeck_file, model=model, ofile=ofile, $
  title=title, buffer=buffer, force_new=force_new, get_origmesh= get_origmesh, $
  tech=tech, toplot=toplot, output_atcf=output_atcf, ytitle=ytitle
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

  atmos_const
  ;
  ; Read entire atcf file
  ;  adeck structure will have a 1-D array for each atcf column with lengths equal
  ;  to # of non-duplicated lines. (not used for some reason)
  ;  mcv_lats, mcv_lons, mcv_times, and mcv_intensity, and mcv_id are 2-d arrays that
  ;  will be used below.
  adeck = read_atcf(adeck_file, tech=tech) ; adeck atcf in knots and nautical miles
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
  if ~keyword_set(toplot) then toplot = 'vmax'
  if ~keyword_set(ytitle) then ytitle=''
  start_new_atcf_output_file = 1
  close, /all

  ii = where(adeck.tech ne model.name, /null)
  if ii ne !null then begin
    print, 'adeck.tech not equal to model.name'
    print, 'adeck tech = ' + adeck.tech[0] +', model.name=' + model.name
  endif

  lons = adeck.lon ; needed to avoid wrapping lines when straddling map limits
  lats = adeck.lat
  ; if BDECK, use 'BEST', otherwise use 'CARQ', unless it's w. pacific--then use 'WRNG'
  if strmid(file_basename(bdeck_file),0,1) eq 'b' then tech = 'BEST' else if strpos(file_basename(bdeck_file), 'wp') ne -1 then tech = 'WRNG' else tech = 'CARQ'
  obs = read_atcf(bdeck_file, tech=tech)
  obstimes = obs.twod.time[*,0] ; Get 1-d array of times from 2-d array of julian day times

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
    obs_first = min(obstimes)
    obs_last  = max(obstimes)
    ; Fix dateline straddle for observed track & model tracks
    lons = [obs.lon, lons] ; make 1-D array of longitudes
    lats = [obs.lat, lats]
  endif

  ineg = where(lons lt 0, /null)
  ; 20170913. I think lons come in between -180 and 180.
  ; if there are longitudes below zero...and we straddle the prime meridian or dateline, add 360 to them.
  if ineg ne !NULL and (max(lons) gt 90 or (min(lons) lt -180 and max(lons) lt -90)) then lons[ineg] = lons[ineg] + 360
  limit = [min(lats)-2.5, min(lons)-1, max(lats)+1, max(lons)+1]
  ; per idl documentation, lonmax-lonmin must be 360 deg or less
  if limit[3]-limit[1] gt 360 then limit[3] = limit[1]+360

  map = map('Cylindrical', limit=limit, font_size=10, fill_color=[235,235,242], $
    margin=[0.08,0.08,0.2,0.15], title=title, layout=[1,2,1], buffer=buffer)
  grid = map.MAPGRID & grid.thick=1 & grid.color='white'& grid.label_color='black'& grid.LABEL_POSITION = 0
  grid.Order, /send_to_back
  legend_items = list()
  day_of_month_sym = list()

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
  m1 = mapcontinents(fill_color='beige', thick=0.5, /hires); tried to run this before tracks but it would omit N. Japan & E. Russia!
  ; even tried variants of /continents, /countries, /hires. IDL is a joke now.
  crud = {sym_filled:1, transparency:0.3, linestyle:'solid', symbol:"circle", $
    sym_transparency:0.3}

  lineplot = plot(obstimes, obs.twod[toplot], layout=[1,2,2], ytitle=ytitle, $ # placeholder for y-axis title (or else is cut off)
    sym_size=0.62, thick=1.5, name=stormname, _extra=crud, xtitle='Date', $
    title=stormname+" "+tech+" and "+model.name+" tracks "+toplot, /current, $
    xrange=[min(adeck.time),max(adeck.time)], uvalue=toplot) ; experimented with uvalue - not used.


  lineplot.Refresh, /disable
  map.Refresh, /disable

  foreach id, adeck.id, i do begin
    i201 = strpos(id,'201',3)
    init_date = strmid(id,i201,10)
    init_time = julday(strmid(init_date,4,2), strmid(init_date,6,2), strmid(init_date,0,4), strmid(init_date,8,2), 0 , 0)
    stid = strmid(id,0,i201)
    ; if stid ne '11L' then continue

    itrack_color = legend_items.count() mod track_colors.count()
    track_name = init_date
    color = track_colors[itrack_color]
    thick = 1.1
    sym_size = 0.51
    ; Replaced * with ifinite to avoid  arithmetic error: Floating illegal operand
    ifinite = where(finite(adeck.twod.lon[i,*]), ntimes, /null)
    min_duration_days = 0
    if n_elements(adeck.id) gt 40 then begin ; ran this block on Ryder's 4-km tracks to filter out short, non-tropical tracks
      track_name = stid ; good for multiple storms
      min_duration_days = 3
      tmp_duration = max(adeck.twod.time[i,ifinite]) - min(adeck.twod.time[i,ifinite])
      if tmp_duration lt min_duration_days then begin
        print, id + string(format='(i3)',tmp_duration)+ " days too short, not plotting"
        continue
      endif
      ; use adeck.twodlat, not adeck.lat. lats is 2d lat is 1d. Is there a better naming convention?
      if min(abs(adeck.twod.lat[i,ifinite])) gt 33. then begin
        print, id + " poleward of 33, not plotting"
        continue
      endif
    endif
    if max(adeck.twod.vmax[i,ifinite]) lt 34. then begin
      print, id + " max vmax < 34, not plotting"
      continue
    endif


    ;if init_date lt '2017090100' or init_date gt '2017091200' then continue

    model_track = {tracks_file:adeck_file, init_time:init_time, init_date:init_date, $
      lon: adeck.twod.lon[i,ifinite], lat: adeck.twod.lat[i,ifinite], times:adeck.twod.time[i,ifinite], $
      model_name: model.name, id:id, stormname:stormname}

    legend_items.add, plot(model_track.lon, model_track.lat, /data, _extra=crud, thick=thick, color=color, $
      sym_size=sym_size, name=track_name, overplot=map)
    atimes = model_track.times
    ; label tracks with day of month at 0 UTC.
    day_of_month_sym.add, symbol(model_track.lon, model_track.lat, /data, label_string=day_str(atimes),$
      label_color=contrasting_color(color),label_position='C', label_font_size=2.75)

    xdata = (adeck.twod[toplot])[i,ifinite]

    if get_origmesh then begin
      iens = strpos(file_basename(adeck_file), "ens_")
      if iens eq -1 then stop
      ens = strmid(file_basename(adeck_file),iens+4,2)
      ens = string(long(ens),format='(I0)')
      model_track = add_vitals(list(model_track), model, origmesh='/glade/scratch/ahijevyc/hwt2017/2017090700/ens_'+ens+'/')
      str = " from original mesh"
      if strpos(lineplot.title.string, str) eq -1 then lineplot.title.string = lineplot.title.string + str
      model_track = model_track[0]
      if toplot eq 'vmax' then xdata = model_track.max_spd10m / !ATMOS.KTS2MPS ; m/s to knots
      if toplot eq 'mslp' then xdata = model_track.min_slp/100
    endif
    if keyword_set(output_atcf) then begin
      if start_new_atcf_output_file then openw, atcf_lun, output_atcf, /get_lun
      start_new_atcf_output_file=0
      print_atcf, atcf_lun, output_atcf, model_track, debug=debug
    endif

    model_timeseries = plot(atimes, xdata, overplot=lineplot, _extra=crud, $
      sym_size=sym_size, thick=thick, color=color, name=init_date)

  endforeach
  if keyword_set(output_atcf) then free_lun, atcf_lun

  ; bring observed best track time series to front (other time series called 'model_timeseries')
  lineplot.Order, /BRING_TO_FRONT
  lineplot_day_label = symbol(obstimes, obs.twod[toplot], /data, label_string=day_str(obstimes), $
    label_color='white', label_position='C', label_font_size=3.8, target=lineplot)


  if not obs.IsEmpty() then begin
    observed_track = plot(obs.lon, obs.lat, /data, _extra=crud, $
      sym_size=lineplot.sym_size, thick=lineplot.thick, color=lineplot.color, $
      overplot=map, name=stormname)

    legend_items.add, observed_track, 0 ; insert at the beginning of list
    ; plot obs.lon, obs.lot, obs.time (1d with multple wind threshold lines), not obstimes
    obs_days = symbol(obs.lon, obs.lat, /data, label_string=day_str(obs.time), $
      label_color='white', label_position='C', $
      label_font_size=lineplot_day_label.label_font_size, target=map)
    ; tried target=map and /relative but put in lower left corner of parent window

  endif

  if 0 then begin
    ; for JOAQUIN cut 40° from east and 20° from north
    ; limit = [limit[0],limit[1],limit[2]-8,limit[3]-40]
    ; Irma
    ;t = [17, 272, 35, 303]

    t = map.limit
    ;t = [12, 267, 43, 323] ; 1st zoom for Rebecca's Irma plot
    t = [17.5, 271, 31, 297] ; 2nd zoom for Rebecca's Irma plot
    font_size = 0.102 * (t[2]-t[0])
    m1.thick = 0.35
    obs_days.label_font_size=font_size
    foreach trk, day_of_month_sym do trk.label_font_size=0.7*font_size
    map.limit = t

    t = lineplot.xrange
    t = t + [3, -3]
    lineplot.xrange = t
  endif

  symbol_label_explan = text((map.limit)[1], (map.limit)[0], /DATA, $
    ' labeled with day of month at 0 UTC!C', font_size=6)
  ; tried target=map and /relative but put in lower left corner of parent window
  font_size = 4 > (110/legend_items.length) < 9 ; between 4 and 9
  l = legend(target=legend_items,font_size=font_size,vertical_alignment=0.65, shadow=0, $
    horizontal_alignment='right')
  l.position = [0.998,0.75]; doesn't work when called with this as keyword.

  map.Refresh
  pretty_TC_intensity, lineplot, toplot=toplot


  junk = timestamp_text(target=lineplot)
  l.window.save, ofile, resolution=240
  print, "created "+ofile
  if buffer then l.window.close else stop

end

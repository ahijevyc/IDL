pro run_plot_storm, force_new = force_new, toplot=toplot, file=file, bdeck_file=bdeck_file, $
    first_time_cutoff=first_time_cutoff, last_time_cutoff=last_time_cutoff, maplimit=maplimit, $
    sym_size=sym_size, debug=debug
  files = file_search("/glade/work/ahijevyc/tracking_gfdl/adeck/uni/tcgen/a????2017*[ym]", count=nfiles)
  files = file_search("/glade/scratch/ahijevyc/hwt2017/2017090700/ens*/latlon_0.500deg*/gfdl_tracker/tracker/fort.64.Irma", count=nfiles)
  nfiles = 0 ; finished making raw-mesh atcf
  if ~keyword_set(bdeck_file) then bdeck_file='/glade/work/ahijevyc/atcf/archive/2016/bal092016.dat'
  if ~keyword_set(maplimit) then maplimit=[22, 360-95, 32, 360-78] ; Hermine 2016, Nate 2017, Michael 2018, Barry 2019
  if ~keyword_set(debug) then debug = 0

  for ifile=0,nfiles-1 do plot_storm, files[ifile], ofile=files[ifile]+'.png', /buffer, force_new=force_new, $
    output_atcf=file_dirname(files[ifile])+"/"+file_basename(files[ifile],".dat")+".origmeshTrue.dat",$
    /get_origmesh, model=mpas_mesh('hwt2017'), bdeck_file=bdeck_file, $;  parent_id='qn8h9pp21e.66hrbdndmz'),$
    first_time_cutoff=first_time_cutoff, last_time_cutoff=last_time_cutoff, maplimit=maplimit, sym_size=sym_size


  if ~keyword_set(file) then begin 
      ; plot MPAS ensemble tracks and time series
      ; on one page - MPAS ensemble dat file created with ~/bin/rename_model.csh, run in /glade/scratch/hwt2017/2017090700/.
      file = "/glade/u/home/ahijevyc/aal112017.ens.dat"
      file = "/glade/work/ahijevyc/ADCIRC/IRMA/rmax_nws19.fort.22"
      file = "/glade/work/ahijevyc/ADCIRC/IRMA/mpas.uni.fort.64"
      file = "/glade/u/home/ahijevyc/t.64"
      file = "/glade/work/ahijevyc/ADCIRC/IRMA/WRF.2017090512.atcf"
      file = "/glade/work/ahijevyc/atcf/Irma2017/0p125/aal112017.2017090812.1hr.dat_origmeshTrue"
      file = "/glade/work/ahijevyc/atcf/Hermine2016/aal092016.2016082912.dat"
  endif

  plot_storm, file, get_origmesh=0, model=mpas_mesh('ECMWF', /nomesh), toplot=toplot, output_atcf='', $
    bdeck_file=bdeck_file, force_new=force_new, $
    first_time_cutoff=first_time_cutoff, last_time_cutoff=last_time_cutoff, maplimit=maplimit, sym_size=sym_size
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
  first_time_cutoff=first_time_cutoff, last_time_cutoff=last_time_cutoff, maplimit=maplimit, $
  tech=tech, toplot=toplot, output_atcf=output_atcf, sym_size=sym_size, debug=debug
  ; Plot TC track and matching model tracks for one storm.
  ; Plot TC intensity and matching model intensities too.
  ; Requires best track file.
  ; Input
  ; a-deck file
  ; Optional
  ; b-deck file (default is derived from a-deck file name)
  ; model (default is the first model in the a-deck file).
  ; ofile - png output filename

  if ~keyword_set(bdeck_file) then bdeck_file = '/glade/work/ahijevyc/atcf/b'+strmid(file_basename(adeck_file),1,8)+'.dat'
  if ~keyword_set(maplimit) then maplimit=[22, 360-95, 32, 360-78] ; Hermine 2016, Nate 2017, Michael 2018, Barry 2019
  if ~keyword_set(sym_size) then sym_size=0.5
  if strmid(file_basename(bdeck_file),0,1) ne 'b' then begin
    print, "b-deck file "+bdeck_file+" does not begin with 'b'. are you sure about it?"
    stop
  endif

  atmos_const
  ;
  ; Read entire atcf file
  ;  adeck structure will have a 1-D array for each atcf column with lengths equal
  ;  to # of non-duplicated lines. (not used for some reason)
  adeck = read_atcf(adeck_file, tech=tech) ; adeck atcf in knots and nautical miles
  if adeck.IsEmpty() then return

  if ~keyword_set(model) then model = mpas_mesh(adeck.tech[0])
  if n_elements(buffer) eq 0 then buffer = 1
  if ~keyword_set(toplot) then toplot = 'vmax'
  if ~keyword_set(ofile) then ofile = file_dirname(adeck_file)+'/'+file_basename(adeck_file,'.dat')+'.'+toplot+'.png'
  if n_elements(force_new) eq 0 then force_new = 0
  if file_test(ofile) eq 1 and ~force_new then begin
    print, "found "+ofile+". skipping"
    return
  endif

  if ~keyword_set(title) then title = file_dirname(adeck_file)+"!C"+file_basename(adeck_file)
  if ~keyword_set(get_origmesh) then get_origmesh = 0
  if ~keyword_set(debug) then debug=0
  min_max_vmax = 0.
  start_new_atcf_output_file = 1
  close, /all

  if ~keyword_set(first_time_cutoff) then first_time_cutoff = julday(8,28,2016,12)
  if ~keyword_set(last_time_cutoff) then last_time_cutoff = julday(9,3,2016,12)

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
  obstimes = obs.twod.valid_time[*,0] ; Get 1-d array of times from 2-d array of julian day times

  if ~ obs.IsEmpty() then begin
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

  mapobj = map('Cylindrical', limit=limit, font_size=9, fill_color=[235,235,242], $
    margin=[0.08,0.08,0.2,0.15], title=title, layout=[1,2,1], buffer=buffer)
  grid = mapobj.MAPGRID & grid.thick=0.75 & grid.color='white'& grid.label_color='black'& grid.LABEL_POSITION = 0
  grid.Order, /send_to_back
  legend_items = list()
  day_of_month_sym = list()
  ofcl_timeseries = list()

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
  m1 = mapcontinents(fill_color='beige', thick=0.35, /hires); tried to run this before tracks but it would omit N. Japan & E. Russia!
  ; even tried variants of /continents, /countries, /hires. IDL is a joke now.
  crud = {sym_filled:1, transparency:0.3, symbol:"circle", sym_transparency:0.3}

  lineplot = plot(obstimes, obs.twod[toplot], layout=[1,2,2], $ # placeholder for y-axis title (or else is cut off)
    sym_size=sym_size, thick=1.5, name=stormname, _extra=crud, xtitle='Date', $
    title=stormname+" "+tech+" and "+model.name+" tracks "+toplot, /current, $
    xrange=[min(adeck.valid_time),max(adeck.valid_time)], uvalue=toplot) ; experimented with uvalue - not used.


  lineplot.Refresh, /disable ; does this speed things up?
  mapobj.Refresh, /disable

  foreach id, adeck.id, i do begin
    i201 = strpos(id,'201',3)
    init_date = strmid(id,i201,10)
    init_time = julday(strmid(init_date,4,2), strmid(init_date,6,2), strmid(init_date,0,4), strmid(init_date,8,2), 0 , 0)
    stid = strmid(id,0,i201)
    ; if init_date lt 2017081600 or init_date gt 2017083000 then continue ; for harvey

    itrack_color = legend_items.count() mod track_colors.count()
    track_name = init_date
    ;    if array_equal(strmid(adeck.id,14,2), replicate('MP',n_elements(adeck.id))) or $
    ;      array_equal(strmid(adeck.id,14,2), replicate('VR',n_elements(adeck.id))) or $
    ;      array_equal(strmid(adeck.id,14,2), replicate('VM',n_elements(adeck.id))) then $
    ;      track_name = strtrim(long(strmid(id,16 ,2)),2); use veer amt for MPAS ensemble
    mytech = strmid(adeck.id[i],14,18) ;differentiate ensemble members and official forecast
    track_name = strmid(adeck.id[i],4,6)+" "+mytech
    color = track_colors[itrack_color]
    if mytech eq "OFCL" then color = [50,30,50]
    thick = 0.6
    time_range = (adeck.twod.valid_time[i,*] ge first_time_cutoff) and (adeck.twod.valid_time[i,*] le last_time_cutoff)
    if total(time_range) eq 0 then begin
        print, "No valid "+adeck.id[i]+" track times in requested time range for plot:"
        print, "     " + string(first_time_cutoff,format="(c())")
        print, "     " + string(last_time_cutoff,format="(c())")
        continue
    endif
    ; Replaced * with ifinite to avoid  arithmetic error: Floating illegal operand
    ifinite = where(finite(adeck.twod.lon[i,*]) and time_range, ntimes, /null)
    min_duration_days = 0
    filter_Ryder = 0
    if filter_Ryder then begin ; ran this block on Ryder's 4-km tracks to filter out short, non-tropical tracks
      track_name = stid ; good for multiple storms
      min_duration_days = 3
      tmp_duration = max(adeck.twod.valid_time[i,ifinite]) - min(adeck.twod.valid_time[i,ifinite])
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
    max_vmax = max(adeck.twod.vmax[i,ifinite])
    if max_vmax lt min_max_vmax then begin
      print, id + " max vmax " + string(max_vmax, format="(F4.1)") + "<" + string(min_max_vmax, format="(F4.0)") +  ", not plotting"
      continue
    endif


    ;if init_date lt '2017090100' or init_date gt '2017091200' then continue

    model_track = {tracks_file:adeck_file, init_time:init_time, init_date:init_date, $
      lon: adeck.twod.lon[i,ifinite], lat: adeck.twod.lat[i,ifinite], valid_time:adeck.twod.valid_time[i,ifinite], $
      model_name: model.name, id:id, stormname:stormname, gfdl_warmcore_only:0}

    case i/9 of ; 9 colors
        0:linestyle='solid'
        1:linestyle="dotted"
        2:linestyle="dashed"
        3:linestyle="dash_dot"
        4:linestyle="dash_dot_dot_dot"
        5:linestyle="long_dash"
        else: linestyle="solid"
    endcase
    model_track_plot = plot(model_track.lon, model_track.lat, /data, _extra=crud, thick=thick, color=color, $
      linestyle=linestyle, sym_size=sym_size, name=track_name, overplot=mapobj)
    legend_items.add, model_track_plot
    if mytech eq "OFCL" then begin
        ofcl_timeseries.add, model_track_plot
        model_track_plot.linestyle='dotted'
        model_track_plot.thick=5*thick
    endif
    atimes = model_track.valid_time
    ; label tracks with day of month at 0 UTC.
    day_of_month_sym.add, symbol(model_track.lon, model_track.lat, /data, label_string=day_str(atimes),$
      label_color=contrasting_color(color),label_position='C', label_font_size=sym_size*2.0)

    xdata = (adeck.twod[toplot])[i,ifinite]

    if get_origmesh then begin
      iens = strpos(adeck_file, "ens_")
      if iens ne -1 then begin
        ens = strmid(adeck_file,iens+4,2)
        ens = string(long(ens),format='(I0)')
        model_track = add_vitals(list(model_track), model, origmesh=get_origmesh, $
          model_basedirs='/glade/scratch/ahijevyc/hwt2017/2017090700/ens_'+ens+'/', debug=debug)

      endif else model_track = add_vitals(list(model_track), model, origmesh=get_origmesh, debug=debug, $
        model_basedirs='/glade/scratch/mpasrt/uni/'+init_date+'/'); +'/ecic/')
      str = " from original mesh"
      if strpos(lineplot.title.string, str) eq -1 then lineplot.title.string = lineplot.title.string + str
      model_track = model_track[0]
      case toplot of
        'vmax': xdata = model_track.max_spd10m / !ATMOS.KTS2MPS ; m/s to knots
        'mslp': xdata = model_track.min_slp/100
        'rad34' : xdata = model_track.twod.rad34
        else: xdata = model_track[toplot]
      endcase

    endif
    if keyword_set(output_atcf) then begin
      if start_new_atcf_output_file then openw, atcf_lun, output_atcf, /get_lun
      start_new_atcf_output_file=0
      print_atcf, atcf_lun, model_track, debug=debug
    endif

    ; if xdata are all NaN the yrange will extend to zero. not good for minp (mslp)
    if total(finite(xdata)) gt 0 then $
        model_timeseries = plot(atimes, xdata, overplot=lineplot, _extra=crud, $
          sym_size=model_track_plot.sym_size, thick=model_track_plot.thick, color=model_track_plot.color,$
          name=model_track_plot.name, linestyle=model_track_plot.linestyle)

  endforeach
  if keyword_set(output_atcf) then free_lun, atcf_lun

  ;if toplot eq 'vmax' then lineplot.yrange = [0, 160] ; before lineplot_day_label, so not vertically stretched
  if toplot eq 'mslp' then begin
    yrange = lineplot.yrange
    if yrange[0] lt 900. then yrange[0] = 900.
    lineplot.yrange = yrange
  endif
  case toplot of
    'vmax'  : units = "knots"
    'mslp'  : units = "hPa"
    'rad34' : units = "nautical miles"
    'rad50' : units = "nautical miles"
    'rad64' : units = "nautical miles"
    else: units = ""
  endcase



  if ~ obs.IsEmpty() then begin
    ; if it errors here, enclose obs.lon and obs.lat in square brackets (to make arrays)
    ; otherwise you get an error about "input must be array or format string".
    ; this needed for TC FOUR in Indian Ocean in 2018 . apparently it only had one time in obs structure
    observed_track = plot([obs.lon], [obs.lat], /data, _extra=crud, $
      linestyle="solid", sym_size=1.2*lineplot.sym_size, thick=lineplot.thick, color=lineplot.color, $
      overplot=mapobj, name=stormname)

    legend_items.add, observed_track, 0 ; insert at the beginning of list
    ; plot obs.lon, obs.lot, obs.valid_time (1d with multple wind threshold lines), not obstimes
    obs_days = symbol(obs.lon, obs.lat, /data, label_string=day_str(obs.valid_time), $
      label_color='white', label_position='C', $
      label_font_size=1.2*day_of_month_sym[0].label_font_size, target=mapobj)
    ; tried target=mapobj and /relative but put in lower left corner of parent window

    best_track_modification_time = 'best track ' + bdeck_file + ' last modified ' + systime(0, file_modtime(bdeck_file))
    mtime_label = text(0,9,/device, best_track_modification_time, font_size=3.5)

  endif

  ; Legend (if fewer than 30 lines)
  font_size = 2 > (180/legend_items.length) < 9 ; between 2 and 9
  if legend_items.length gt 30 then vertical_spacing = 0.0 else vertical_spacing=0.03
  sample_width = legend_items.length gt 30 ? 0.18 : 0.15
  if legend_items.length lt 100 then begin ; use 'lt' not '<'
    l = legend(target=legend_items,font_size=font_size,vertical_alignment=0.65, shadow=0, $
      horizontal_alignment='right', vertical_spacing = vertical_spacing, sample_width=sample_width)
    l.position = [0.998,0.8]; doesn't work when called with this as keyword.
  endif

  print, stormname
  if stormname eq 'HARVEY' then begin
    ; for JOAQUIN cut 40° from east and 20° from north
    ; limit = [limit[0],limit[1],limit[2]-8,limit[3]-40]
    ; Irma
    ;maplimit = [17, 272, 35, 303]

    ;maplimit = [10, 257, 40, 323] ; Harvey plot
    ;maplimit = [13, 360-105, 33, 360-73] ; 1st zoom for Harvey
    maplimit = [18, 360-101, 32, 360-84] ; 2nd zoom for Harvey
    ;maplimit = [12, 267, 43, 323] ; 1st zoom for Rebecca's Irma plot
    ;maplimit = [17.5, 271, 31, 297] ; 2nd zoom for Rebecca's Irma plot
    ;maplimit = [22, 275, 28, 284] ; 3nd zoom for AMS talk plot
  endif

  mapobj.limit = maplimit

  ; Set time range for x axis.
  lineplot.xrange = [ first_time_cutoff, last_time_cutoff ]

  ; bring observed best track time series to front (other time series called 'model_timeseries')
  lineplot.Order, /BRING_TO_FRONT
  ; setting order of ofcl_timeseries members has no effect
  ; foreach i, ofcl_timeseries do i.Order, /BRING_TO_FRONT
  ; lineplot_day_label not showing up
  ;lineplot_day_label = symbol(obstimes, obs.twod[toplot], /data, label_string=day_str(obstimes), $
  ;  label_color='white', label_position='C', label_font_size=3.8, target=lineplot)
  lineplot.ytitle = units

  symbol_label_explan = text((mapobj.limit)[1], (mapobj.limit)[0], /DATA, $
    ' labeled with day of month at 0 UTC!C model tracks cut off at '+string(last_time_cutoff, format='(C(CYI,CMOI2.2,CDI2.2,X,CHI))')+' UTC', font_size=5.5)
  ; tried target=mapobj and /relative but put in lower left corner of parent window

  mapobj.Refresh
  pretty_TC_intensity, lineplot, toplot=toplot


  junk = timestamp_text(target=lineplot)
  mapobj.window.save, ofile, resolution=240
  print, "created "+ofile
  if buffer then mapobj.window.close else stop

end

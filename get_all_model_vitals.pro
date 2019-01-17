function matches_a_best_track, track, best_track=best_track
  best_track = {adeck_file_name:'?',maxw:!VALUES.D_NAN} ; initialize to nothing
  ; if you find this track in the adeck files it is matched to an observed best track.
  ; All files in the adeck list were created by find_matching_model_track.pro.
  ; They are the closest model matches to each bdeck best track.
  ;

  ; Returns 1 if track matches an observed best track.
  ; Returns 0 otherwise (it may be considered a false alarm, depending on the criteria for false alarms).

  tracks_file = track.tracks_file

  suffix='_origmesh' + (track.origmesh?'True':'False') + string(track.min_duration_days,track.min_warmcore_fract, $
    format='("_",F3.1,"d_minimum.min_warmcore_fract",F4.2)')

  if strmatch(tracks_file, '*/gfdl_tracker/*') then begin
    trackername='gfdl'
    suffix = '_gfdl'+suffix
  endif
  ; assumes mpas is under a subdirectory called latlon_0.500deg_025km or some other grid spacing and smooth radius
  ; with the same number of characters.
  if strmatch(tracks_file, '*/latlon_0*/*') then suffix= strmid(tracks_file,strpos(tracks_file,'latlon_')+6,15)+suffix
  if strmatch(tracks_file, '*_tracks.sav') then suffix = strmid(file_basename(tracks_file, '_tracks.sav'),2,/rev)

  basedir = '/glade/work/ahijevyc/tracking_'+trackername+'/adeck/'

  yyyy = strmid(track.init_date,0,4)

  ; If trackertype is 'tracker' there should not be any false alarms. All model tracks should be
  ; matched to a best track.  But I have made code flexible enough to run on 'tracker' mode
  ; output.
  ;
  searchstr = basedir+track.model_name+'/'+track.trackertype+'/a*'+yyyy+'*'+suffix
  adeck_files = file_search(searchstr, count=nfiles)

  if nfiles eq 0 then stop
  print, "looking for track ", track.id, " in ", strtrim(nfiles,2), " adeck files matching ", searchstr
  for ifile=0,nfiles-1 do begin
    adeck_file=adeck_files[ifile]
    adeck_file_info = file_info(adeck_file)
    track_info = file_info(tracks_file)

    if adeck_file_info.mtime lt track_info.mtime then begin
      print, 'adeck file '+adeck_file+' should be modified after tracks file '
      print, 'track file ', tracks_file, systime(0,track_info.mtime)
      print, '           ', systime(0,track_info.mtime)
      print, 'adeck file ', systime(0,adeck_file_info.mtime)
      print, 'run find_matching_model_track?'
      stop
    endif
    if file_test(adeck_file, /zero) then continue ; skip if zero size
    ; choose min_warmcore_fract=0 because a-decks don't have a warm core column.
    adeck = read_atcf(adeck_file) ; adeck atcf in knots and nautical miles
    ii = where(adeck.init_yyyymmddhh eq track.init_date,/null)
    if ii eq !NULL then continue
    trackids = adeck.userdefined7[ii]
    if min(trackids) ne max(trackids) then stop ; make sure they are all the same
    trackids = strsplit(trackids[0],'/', /extr) ; could be a composite track, which is separated by slashes.
    ihits = where(trackids eq track.id, /null)
    if ihits ne !NULL then begin
      bdeck_file = '/glade/work/ahijevyc/atcf/b'+strmid(file_basename(adeck_file),1,8)+'.dat'
      bdeck = read_atcf(bdeck_file)
      best_track = {adeck_file_name:file_basename(adeck_file),maxw:max(bdeck.vmax)*!ATMOS.kts2mps}
      return, 1
    endif
  endfor
  return, 0
end



pro get_all_model_vitals, model_name=model_name, date=date, trackername=trackername, $
  smooth_radius=smooth_radius, fcst_hr_hist=fcst_hr_hist, hist_vital=hist_vital, $
  trk_color=trk_color, basin=basin, ofile=ofile, $
  min_duration_days=min_duration_days, debug=debug, min_warmcore_fract=min_warmcore_fract,$
  vmax_thresh_kt=vmax_thresh_kt, trackertype=trackertype

  ; Get Vitals for all model tracks in a single model run.
  ; Operate on a single model run (the old way) by setting the date keyword,
  ; Operating on all matching model tracks at once was another possibility.
  ; This last way could save I/O due to shared times among tracks, but it is broken.

  if ~keyword_set(model_name) then model_name = 'uni'
  if ~keyword_set(date) then date = '2015080500'
  basedir = '/glade/p/nmmm0024/'+model_name+'/'+date+'/'
  if strmatch(model_name, 'GFS*') then basedir = '/glade/scratch/ahijevyc/'+model_name+'/'+date+'/'
  if date gt 2016050100 and not strmatch(model_name, 'GFS*') then basedir = '/glade/scratch/mpasrt/'+model_name+'/'+date+'/'
  if ~keyword_set(trackername) then trackername = 'gfdl'
  if ~keyword_set(smooth_radius) then smooth_radius='025'; carried from mpas_to_latlon. number of grid points in smoothing radius.
  if ~keyword_set(basin) then basin='EP'
  if n_elements(trk_color) eq 0 then trk_color={minrange:5,maxrange:35}
  if ~keyword_set(debug) then debug=0
  if ~keyword_set(vmax_thresh_kt) then vmax_thresh_kt = 34.
  if ~keyword_set(trackertype) then trackertype = 'tcgen'
  if ~keyword_set(ofile) then ofile = ''
  if n_elements(min_warmcore_fract) eq 0 then min_warmcore_fract = 0.25

  atmos_const
  dxdetails = trackername eq 'gfdl' && model_name eq 'GFS' ? '*' : string(0.5,smooth_radius,format='("_",F5.3,"deg_",I3.3,"km")')
  mpas = mpas_mesh(model_name)
  in_tropics = 30d ; tropical latitudes less than this
  ; use arg_present because min_duration_days could be zero, but keyword_set returns false if the value is zero.
  if ~arg_present(min_duration_days) then min_duration_days = 1d

  subpath = strmatch(mpas.name,'GFS*') ? '' : 'latlon'+dxdetails+'/'
  basedir = basedir+subpath
  model_tracks_file = basedir+(trackername eq 'gfdl' ? 'gfdl_tracker/'+trackertype+'/fort.66' : trackername+'_'+smooth_radius+'_tracks.sav')
  model_tracks_files = file_search(model_tracks_file, count=ntracks_files)
  if ntracks_files ne 1 then begin
    if debug then print, "no tracks files found in ", model_tracks_file
    xyouts, 0.5, 0.5, 'no model tracks file for '+mpas.name+' '+date, /normal, align=0.5, charsize=2, charthick=3
    return
  endif
  model_tracks_file = model_tracks_files[0]
  ; open false alarms file
  false_alarms_file = file_dirname(model_tracks_file)+ "/false_alarms_" + basin + $
    string(vmax_thresh_kt,format='(".vmax_thresh_kt.ge.",I2.2,".dat")')

  ; Skip if false alarm file exists and is modified later than model tracks file.
  if file_test(false_alarms_file) eq 0 || $
    (file_info(false_alarms_file)).mtime le (file_info(model_tracks_files[0])).mtime then begin
    openw, fa_lun, false_alarms_file+'.tmp', /get_lun
  endif else begin
    print, basin+" basin false alarms file exists, newer than "+model_tracks_files[0]+" skipping."
    print, 'false alarms file: '+ false_alarms_file
    return
  endelse

  ; If using GFDL tracker, don't use IDL save file; just read gfdl output ATCF fort.66 file
  if trackername eq 'gfdl' then begin
    gfdl_m = read_atcf(model_tracks_file, lats=mcv_lats, lons=mcv_lons, valid_time=mcv_times, intensity=mcv_intensity, $
      id=mcv_id)
    init_time = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),strmid(date,8,2),0,0)
    specs = {tracker:'gfdl'}
  endif else restore, model_tracks_file ; get mcv_lats, mcv_lons, mcv_times, init_time, etc. from my tracking method.

  if !D.NAME ne 'NULL' then loadct, 13, /silent
  empty ; update graphics (useful for ps)
  if !D.NAME eq 'X' then begin
    device, decomposed=0 & loadct, 39, /silent
  endif



  model_tracks = list()
  ntrack = total(finite(mcv_times[*,0]))

  ; Filter out tracks in extratropics, SH, insufficient duration, over land, or in wrong basin
  for itrack=0,ntrack-1 do begin
    last_itime = max(where(finite(mcv_times[itrack,*]),/null))
    lons      = reform(     mcv_lons[itrack,0:last_itime])
    lats      = reform(     mcv_lats[itrack,0:last_itime])
    intensity = reform(mcv_intensity[itrack,0:last_itime])

    ; is track between +/- 30 latitude at some point?
    itropics = where(abs(lats) lt in_tropics, /null)
    if itropics eq !NULL then begin
      if debug then print, date, ' track ',mcv_id[itrack],' never in tropics'
      continue
    endif
    if max(lats) lt 0 then begin
      if debug then print, date, ' track ',mcv_id[itrack],' never in Northern Hemisphere'
      continue ; ignore southern hemisphere (for now)
    endif
    tduration = mcv_times[itrack,last_itime]-mcv_times[itrack,0]
    if tduration lt min_duration_days then begin
      if debug then print, date, ' track ',mcv_id[itrack],' lasted only',string(tduration,format='(F6.2)'), ' days'
      continue
    endif
    
    ; Requre min_warmcore_fract warm core times for this track to count.
    nwarmcore = total(gfdl_m.twod.warmcore[itrack,*] eq 'Y')
    fract_warmcore = nwarmcore/n_elements(mcv_times[itrack,*])
    if debug then print, mcv_id[itrack], nwarmcore, fract_warmcore, format='(A," found ",I0," warm core times. (",F4.2,")")'
    if nwarmcore lt min_warmcore_fract then begin
      print, mcv_id[itrack], min_warmcore_fract, fract_warmcore, format='(A,I0," warm core time required. Found only ",I0,".")'
      continue
    endif


    dland = distance_to_land_km(lons,lats)
    iocean = where(dland gt 0, /null)
    if iocean eq !NULL then begin
      if debug then print, date, ' entire track ',mcv_id[itrack],' over land'
      continue
    endif

    ; on 23 mar 2014 I checked all the matching track segments and wrote down the maximum intensity of each one
    ; for mpas, mpas_al, mpas_wp, and GFS, the lowest "maximum" was 5.01e-05.  So if I require
    ; the maximum intensity to be greater than 5e-05 I will eliminate a bunch of weak systems objectively, but not throw out any matches.
    if max(intensity) le 5e-05 then begin
      if debug then print, itrack, 'not intense', max(intensity)
      continue
    endif

    ; If found-on-fly (FOF) get basin based on official navy domains
    ;      if strmid(track.id, 2, /reverse) eq 'FOF' then begin
    mean_lon = atan( mean(sin(!DTOR*lons)), mean(cos(!DTOR*lons)) )*!RADEG
    mybasin = atcf_basin(mean_lon, mean(lats), subregion=subregion)
    if strmatch(mybasin, basin, /fold) eq 0 then begin
      ; if it isnt in the requested basin skip this track
      print, 'skipping '+mcv_id[itrack]+'. '+mybasin+' not '+basin
      continue
    endif


    model_tracks.add, {itrack:itrack, tracks_file:model_tracks_file, init_time:init_time, init_date:date, $
      lon:lons, lat:lats, valid_time:reform(mcv_times[itrack,0:last_itime]), intensity:intensity, $
      specs:specs, model_name:mpas.name, min_duration_days:min_duration_days, in_tropics:in_tropics, $
      id:mcv_id[itrack], min_warmcore_fract:min_warmcore_fract, trackertype:trackertype}

    ;    for iseg = 0, n_elements(model_tracks[-1].lat)-2 do $
    ;    oplot, model_tracks[-1].lon[iseg:iseg+1], model_tracks[-1].lat[iseg:iseg+1], $
    ;    color=bytscl(model_tracks[-1].intensity[iseg], min=3e-5,max=20e-05)

  endfor

  ; Fill in the TC vitals for the list of model_track structures (model_tracks).
  ; If 'tracks_file' tag exists in a model_track structure and it is a fort.66 file, add_vitals() will
  ; pull the vitals from fort.66--otherwise, directly from the MPAS diagnostics mesh.
  ; You will get smaller vmax in the fort.66 output because it
  ; comes from the smoothed lat-lon file, read by gfdl_vortex_tracker.
  ; This is not what we want, so on Jul 31, 2015, we added an origmesh option.
  ; This forces vitals to be drawn from the MPAS diagnostics mesh (not the fort.66 file).
  origmesh = 1 ; should be 1 for MPAS (temporarily 0 for comparing numbers from smooth grid)
  if strmatch(mpas.name,'GFS*') then origmesh = 0


  
  model_tracks = add_vitals(model_tracks, mpas, origmesh = origmesh)
  ; vmax will be in m/s.  It is returned as knots by read_atcf() but add_vitals converts it to m/s.

  ; Filter by basin, max wind attained in tropics, and warmcore fraction-of-time
  for itrack=0,model_tracks.count()-1 do begin
    track = model_tracks[itrack]

    ; Does max wind get to TS strength somewhere in the tropics?
    iTS = where(track.max_spd10m ge vmax_thresh_kt * !ATMOS.kts2mps and abs(track.lat) lt 30., /null)
    if iTS eq !NULL then begin
      if debug then print, 'skipping '+track.id+' not '+string(vmax_thresh_kt,format='(">=",I0,"kt")')+' in tropics'
      continue ; if not then skip it
    endif

    ; Here's the moment of truth: is this track a match or false alarm?
    ; row 0 of histogram array is match count; row 1 is false alarm count
    matchrow = 0
    farow = 1
    row = matches_a_best_track(track, best_track=best_track) ? matchrow : farow
    ; Don't use the match count here for hits. This count could be smaller than
    ; the tc_stat method for a couple reasons:
    ;
    ; 1) This match count can be different than tc_stat method if the model track centroid is
    ; in one basin but it matches a best track in another.
    ; For example EP082013 Henriette best track is EP but the matching model
    ; track extends way west of the end of Henriette's best track into the WP.
    ; So much so, that the model track centroid is WP. Another case is Odile EP152014. It
    ; starts in the Gulf of California and tracks to Michigan, so it is counted as AL here
    ; and EP in tc_stat.
    ; This isn't an issue for false alarms, which don't have a corresponding
    ; best track to disagree with.
    ;
    ; 2) Matches aren't counted here if the model track is shorter than
    ; the time threshold min_duration_days, but find_matching_model_track.pro/tc_stat allows hits if
    ; the overlap is on a time boundary.
    ;

    ; Originally write false alarm track to a file for COSMIC radio occultation/GFS study
    ; (Chris's contact). But now, also for TC verification paper.
    if row eq farow then begin
      if debug then print, 'false alarm: '+track.id
      print_atcf, fa_lun, 'a'+mybasin+'XX', track
    endif

    ; draw track
    for iseg = 0, n_elements(track.lat)-2 do begin
      oplot, track.lon[iseg:iseg+1], track.lat[iseg:iseg+1], thick=row?.2:1.5, $
        linestyle=row, color=bytscl(track.max_spd10m[iseg], min=trk_color.minrange,max=trk_color.maxrange)
      if n_elements(old_mdd) eq 0 then xyouts, 0,-10,string(track.min_duration_days, vmax_thresh_kt, track.in_tropics, $
        format='("model tracks >",F3.1,"days and at least one time >=",I0,"kt in tropics (+/-",F4.1,"deg)")'), $
        align=0.5, charsize=0.8 else begin
        if track.in_tropics ne old_it then stop
        if track.min_duration_days ne old_mdd then stop ; sanity checks
      endelse
      old_mdd = track.min_duration_days
      old_it = track.in_tropics
    endfor

    ; Only count forecast hours when the track vmax >= vmax_thresh_kt.
    ; Again this is only good for false alarms. The official hit count is different.
    iTS = where(track.max_spd10m ge vmax_thresh_kt * !ATMOS.kts2mps, /null) ; where is Vmax over the threshold (34kt TS or 64kt hurricane) strength?
    fh = 24d*(track.valid_time[iTS] - track.init_time)

    if n_elements(fcst_hr_hist) gt 0 then begin
      input = fcst_hr_hist.data[row,*]
      junk =  histogram(fh, binsize=fcst_hr_hist.binsize, min=fcst_hr_hist.min, max=fcst_hr_hist.max, input=input)
      fcst_hr_hist.data[row,*] = junk
    endif

    vital_histogram = max(track.max_spd10m,imax)
    ; best_track.maxw is in m/s (!VALUES.D_NAN if false alarm).
    if strmatch(file_basename(ofile), '*observed*') then vital_histogram = best_track.maxw ; Chris would also like the observed maximum (not just model)
    ;vital_histogram = (track.z200_core[imax] - track.z850_core[imax]) - (track.z200_surr[imax] - track.z850_surr[imax])

    print, track.init_date, model_name,trackername, strjoin(track.id,'/'), best_track.adeck_file_name, vital_histogram, $
      vmax_thresh_kt, fh, format='(A,x,A,x,A," track ",A," is ",A," and vital_histogram=",F5.1," m/s. forecast hour(s) >=",i0,"kt: ",99(i0,x))'
    ;print, fcst_hr_hist.data[0,6:*:6], format='(99I4)'

    ; Augment histogram hist_vital
    if n_elements(hist_vital) then begin
      input = hist_vital.data[row,*]
      junk = histogram([vital_histogram], min=hist_vital.min, max=hist_vital.max, binsize=hist_vital.binsize, input=input, locations=xvals)
      hist_vital.data[row,*] = junk
      hist_vital.xvals = xvals+hist_vital.binsize/2. ; kludge for plot histogram procedure (plot function doesn't need it)
    endif

  endfor ; filter and classify each track
  free_lun, fa_lun
  ; Remove .tmp from false alarm file, signifying the program is finished.
  file_move, false_alarms_file+'.tmp', false_alarms_file

end

pro run_get_all_model_vitals, date=date, trackername=trackername, debug=debug, $
  min_duration_days=min_duration_days, min_warmcore_fract=min_warmcore_fract, basins=basins, $
  models=models, year=year, trackertype=trackertype
  ; For a single date (date keyword) or a range, get vitals for all model tracks.
  ; Then plot hits and false alarms and histograms of a vitals variable (e.g. max_wind).
  ; False alarms are fine, but the hit count is often smaller than tc_stat. See explanation below.
  ;
  ; Run this after find_matching_model_track.pro pairs best tracks (bdeck) to a model track.
  ; This is the easy way of determining hits and false alarms.

  if ~keyword_set(trackername) then trackername = 'gfdl'
  if ~arg_present(min_duration_days) then min_duration_days=1d
  if ~keyword_set(year) then year = 2018
  if ~keyword_set(trackertype) then trackertype = 'tcgen'
  if n_elements(debug) eq 0 then debug = 0

  set_plot, 'NULL'
  if !D.NAME ne 'NULL' then loadct, 13, /silent
  if !D.NAME eq 'X' then begin
    device, decomposed=0
    loadct, 39, /silent
    erase
  endif
  if keyword_set(date) then begin
    start_ymd = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),strmid(date,8,2))
    final_ymd = start_ymd
  endif else begin
    start_ymd = julday( 8,1,year,0)
    final_ymd = julday(11,3,year,0)
    if year eq 2015 then begin
      start_ymd = julday(7,1,year,0)
      final_ymd = julday(10,08,year,0)
    endif
    if year eq 2016 then begin
      start_ymd = julday(6,29,year,0)
      final_ymd = julday(10,31,year,0)
    endif
    if year eq 2017 then begin
      start_ymd = julday(8,1,year,0)
      final_ymd = julday(10,31,year,0)
    endif
    if year eq 2018 then begin
      start_ymd = julday(6,1,year,0)
      final_ymd = julday(10,31,year,0)
    endif
  endelse
  if debug then print, "requested date range ", string(start_ymd, final_ymd, format='(c())')

  ; Used to make this variable (64 or 34) but now we just use 34 and filter out the 64s later
  ; This has the effect of filtering out model tracks that don't ever reach TS strength.
  vmax_thresh_kt=34 ; [knots]

  if n_elements(min_warmcore_fract) eq 0 then min_warmcore_fract = 0.25 ; start_ymd gt julday(1,1,2014)

  ; models = ['mpas','mpas_al','mpas_wp','GFS'] ; 'mpas','mpas_al','mpas_wp','GFS'
  if ~keyword_set(models) then models = ['uni','GFS']

  for imodel = 0, n_elements(models)-1 do begin
    model_name = models[imodel]
    smooth_radius='025'
    grid_dx=0.5
    smooth_radius_str = string(smooth_radius,format='("_",I3.3,"km")')
    if strmatch(model_name, 'GFS*') then begin
      if year ge '2015' then grid_dx=0.25
      smooth_radius_str = ''
    endif
    dxdetails = string(grid_dx,format='("_",F5.3,"deg")')+smooth_radius_str
    if debug then print, model_name, dxdetails
    ; If mpas_wp and 2013 start at Sep 1 instead of Aug 10.
    ; use IDL maximum operator "A > B" is equal to larger of A or B.
    if model_name eq 'mpas_wp' then start_ymd = julday(9,1,2013,0) > start_ymd
    if model_name eq 'ep' then start_ymd = julday(7,18,2015,0) > start_ymd
    final_ymd = final_ymd < systime(/julian, /utc) ; no later than current time
    if debug then print, "final date range ", string(start_ymd, final_ymd, format='(c())')
    yyyymmddhh = string(timegen(start=start_ymd, final=final_ymd, step=model_name eq 'GFS' ? 0.25 : 1), format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
    if ~keyword_set(basins) then basins = ['WP','AL','EP']
    nbasin=n_elements(basins)
    ; changed model_name to 'observed' temporarily. June 21 2015.
    model_or_observed = model_name + '_vmax.ge.' + string(round(vmax_thresh_kt),format='(I2)') + 'kt.observed';+model_name;  + model_name ; 'observed' or model_name
    for b = 0,nbasin-1 do begin
      basin = STRUPCASE(basins[b]) ; Force uppercase so keyword entry can be lowercase or uppercase 
      ofile = '/glade/work/ahijevyc/tracking_'+trackername+'/'+model_or_observed+'.'+basin+'_tracks_and_histograms'+$
        string(start_ymd,final_ymd,format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2),"-",C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')+ $
        dxdetails + string(min_duration_days, min_warmcore_fract, format='("_",F3.1,"d_minimum.min_warmcore_fract",F4.2)') +'.ps'


      if debug then print, "ofile = ",ofile
      if !D.NAME eq 'PS' then device, /close, /color, filename = ofile, bits=8
      !P.MULTI=[2,1,2]
      map_set, /cont, limit=[0,-180,50,180], /iso, title=model_name + ' init dates:' + yyyymmddhh[0] + '-'+yyyymmddhh[-1] + $
        string(min_warmcore_fract,format='(".min_warmcore_fract",F4.2)')
      trk_color = {minrange:5, maxrange:35}
      mycolorbar, position=[0.1,0.9,0.9,0.92], minrange=trk_color.minrange, maxrange=trk_color.maxrange, title='m/s', charsize=0.7

      ; Outline and label basin - ATCF_basin.pro uses definitions of basins from
      ; ATCF specifications. which is slightly different than
      ; in_mybasin.pro.  in_mybasin.pro is used for mpas_basin_rmse.pro however.
      ;      tmp = atcf_basin(0,0,basin,px=px,py=py)
      ;      oplot, px, py, thick=4 & xyouts, mean(px), !Y.RANGE[1], basin, charthick=3

      ; there are 2 rows in the histogram data arrays
      ; the first is hits and the 2nd is false alarms
      ; why did i have this outside the basin loop until apr 16 2014?  I don't know.
      fcst_hr_hist = {data:lonarr(2,240+1),min:0.,max:240.,binsize:1.}
      hist_vital = {data:lonarr(2,21), min: 0, max:90, binsize:5.,xvals:fltarr(19),field:'vmax'} ; maxwind
      ;hist_vital = {data:lonarr(2,21), min:-40, max:60, binsize:5, xvals:fltarr(21),field:'850-200mb dZ anom'} ; 850-200mb thickness anomaly in core
      ;hist_vital = {data:lonarr(2,21), min:-40, max:60, binsize:5, xvals:fltarr(21),field:'850-200mb dZ anom'} ; 850-200mb thickness anomaly in core
      if debug then help, yyyymmddhh
      for d=0,n_elements(yyyymmddhh)-1 do get_all_model_vitals, date = yyyymmddhh[d], $
        fcst_hr_hist=fcst_hr_hist, vmax_thresh_kt=vmax_thresh_kt, $
        hist_vital=hist_vital, basin=basin, model_name=model_name, trk_color=trk_color, $
        trackername=trackername, ofile=ofile, trackertype=trackertype, $
        smooth_radius=smooth_radius, min_duration_days=min_duration_days, debug=debug, min_warmcore_fract=min_warmcore_fract

      !P.MULTI = [1,1,2]
      ; plot false alarm histogram in dotted line
      plot, hist_vital.xvals, hist_vital.data[1,*], psym=10, linestyle=1, xtitle=model_or_observed+" "+hist_vital.field+" (m/s)", $
        yrange=[0,max(hist_vital.data)], title=basin, subtitle='solid=hits, dotted=false alarms'
      ; plot hits histogram in solid line
      oplot, hist_vital.xvals, hist_vital.data[0,*], psym=10, linestyle=0
      xyouts, !X.CRANGE[0]+0.6*(!X.CRANGE[1]-!X.CRANGE[0]), !Y.CRANGE[0]+0.8*(!Y.CRANGE[1]-!Y.CRANGE[0]), $
        string(total(hist_vital.data[0,*]),total(hist_vital.data[1,*]),format='(I4," hits!C",I4," false alarms")'), $
        align=0
      if !D.NAME eq 'PS' then begin
        print, 'created '+ofile
        device, /close
      endif
    endfor ; basin
  endfor ; model
end


pro fix_modid
  old_id = 'mpas2'
  new_id = 'mpas_al'
  ; replace the old GFS string 'GFS004' with 'GFS' in the vitals save files.
  basedir = '/glade/scratch/ahijevyc/'
  files = file_search('/glade/scratch/ahijevyc/'+new_id+'/2*/latlon_0*/gfdl_tracker/fort.66_vitals_0.0d_minimum.sav', count=n)
  for ifile=0,n-1 do begin
    restore, files[ifile]
    for it=0,model_tracks.count()-1 do begin
      track = model_tracks[it]
      if strmatch(track.tracks_file, '*/'+old_id+'/*') eq 0 then continue
      ipos = strpos(track.tracks_file, old_id)
      track.tracks_file = strmid(track.tracks_file,0,ipos)+new_id+strmid(track.tracks_file,ipos+strlen(old_id))
      if strmatch(track.vitals_save_file, '*/'+old_id+'/*') eq 0 then stop
      ipos = strpos(track.vitals_save_file, old_id)
      track.vitals_save_file = strmid(track.vitals_save_file,0,ipos)+new_id+strmid(track.vitals_save_file,ipos+strlen(old_id))
      if track.model_name ne old_id then stop
      track.model_name = new_id
      model_tracks[it] = track
    endfor
    save, model_tracks, filename=files[ifile]
  endfor
end



pro fix_mpas
  ; replace the old mpas string 'mpas' or 'mpas' with the mpas structure in the  tracking save files.
  mpass = ['GFS']
  basedir = '/glade/scratch/ahijevyc/'
  for impas = 0, n_elements(mpass)-1 do begin
    model_name = mpass[impas]
    files = file_search('/glade/scratch/ahijevyc/'+model_name+'/2*/vorticity_850hPa_*tracks.sav', count=n)
    for ifile=0,n-1 do begin
      restore, files[ifile]
      mpas = mpas_mesh(model_name)
      save, /variables, filename=files[ifile]
    endfor
  endfor
end



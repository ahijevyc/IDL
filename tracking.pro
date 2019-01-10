; copied from pecan on Oct 28, 2013 . Ahijevych

function jday_to_string, jday
  return, string(jday, format='(C(CMoA, X, CDI0, X, CHI0, ":", CMI2.2))')
end
function title_string, specs
  return, string(specs.min_intensity, round(specs.min_size), round(specs.max_size), round(specs.bin_spacing), round(specs.max_std), $
    format='(" min feature intensity=",E7.1," size range=",I0,"-",I0,"km, bin width=",I0,"km, max std=",I0,"%!C")')
end


function extrapolate, jmcv, mcv_lons, mcv_lats, mcv_times, time
  ; input mcv_lons, mcv_lats, and mcv_times arrays, but only look at jmcv.
  ; estimate the extrapolated location at "time" assuming the same motion vector from 12h prior to the most recent location.
  igood = where(finite(mcv_times[jmcv,*]), ngood)
  lons  =  mcv_lons[jmcv, igood]
  lats  =  mcv_lats[jmcv, igood]
  times = mcv_times[jmcv, igood]
  
  last_lonlat = [lons[-1], lats[-1]]
  
  ; Return last location if there is only 1 point.
  if ngood eq 1 then return, last_lonlat
  
  ; find the time element 0.5 days before the most recent one, if possible
  initial_extrapolation_itime = value_locate(times, times[-1]-0.5d)
  if initial_extrapolation_itime lt 0 then initial_extrapolation_itime = 0 ; can't backtrack before starting time
  
  time_since_last_point = time - times[-1]
  
  ; if there are at least 2 points to extrapolate from, extrapolate.
  ; how much did it move in the last known interval? (prev to last)
  extrap = map_2points(lons[initial_extrapolation_itime], lats[initial_extrapolation_itime], lons[-1], lats[-1])
  distance_radians = extrap[0]*!DTOR
  ; scale distance according to time gaps
  distance_radians = distance_radians * time_since_last_point / (times[-1] - times[initial_extrapolation_itime])
  heading = extrap[1]
  ; Return new expected lon and lat
  return, ll_arc_distance(last_lonlat, distance_radians, heading, /degrees)
  
end

pro run_tracking, debug=debug, date=date, force_new=force_new, smooth_radius=smooth_radius, start=start
  if ~keyword_set(debug) then debug=0
  year = 2014
  if ~keyword_set(start) then start = julday(8,10,2013,0,0,0)
  if year eq 2014 then start = julday(8,1,2014,0,0,0)
  yyyymmdd = keyword_set(date) ? date : string(timegen(start=start, final=julday(11,3,year), step=1), format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
  
  if ~keyword_set(smooth_radius) then smooth_radius = '025'
  if ~keyword_set(force_new) then force_new = 0 ; force_new = 1 means redo the save file for the tracks even if it exists
  gs = [6]/24d ; time gaps in days.
  ng = n_elements(gs)
  
  for i = 0, ng-1 do begin
    max_time_gap = gs[i]+0.01
    for d=0,n_elements(yyyymmdd)-1 do tracking, max_time_gap = max_time_gap, date = yyyymmdd[d], debug=debug, buffer=1, $
      smooth_radius=smooth_radius, force_new=force_new
  endfor
  
end

pro tracking, debug=debug, use_extrap=use_extrap, max_time_gap=max_time_gap, date=date, buffer=buffer, force_new=force_new, smooth_radius=smooth_radius
  loadct, 38, /silent, rgb_table=rgb_table
  rgb_table = rgb_table[256*findgen(11)/10, *]
  if !D.NAME eq 'X' then device, decomposed=0
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(use_extrap) then use_extrap =1 ; extrapolate position of old features when looking for matching current features.
  if ~keyword_set(max_time_gap) then max_time_gap = 6.01d/24 ; time gap allowed between mcv points in days
  if ~keyword_set(date) then date = '2014083100'
  if ~keyword_set(smooth_radius) then smooth_radius = '025'
  dxdetails = string(0.5,smooth_radius,format='("_",F5.3,"deg_",I3.3,"km")')
  if ~keyword_set(buffer) then buffer = 0
  if ~keyword_set(force_new) then force_new = 0 ; force_new = 1 means redo the save file for the tracks even if it exists
  GFDL_warmcore_only = 0; now 2013 has warmcore stats. date ge 20140000L
  
  extrapolation_search_radius = 200000 ; meters
  no_extrap_search_radius = 300000; meters
  max_mcvs = 16000
  delta = 3d/24 ; time spacing in days. 3d/24 = 3 hours.
  init_time = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),strmid(date,8,2),0,0)
  maxtime = init_time+10d
  max_time_dim_size = (maxtime-init_time)/delta+2 ; +2 to fix reference to itime+1 down below. 240h forecast and 3h interval requires >=81. hourly interval would require >=241
  starting_at_hour = 6 ; set to zero if gfdl
  
  xdim = 2000 ; width of plot
  trackfields=['gfdl']
  minimum_duration2plot = n_elements(trackfields) gt 1 ? 2 : 1.
  mean_intensity_to_plot = 10e-05
  max_intensity_to_plot = 5e-05
  
  clean = 0 ; less labeling bigger font
  
  
  ofile = '/glade/work/ahijevyc/tracking_'+strjoin(trackfields,'_')+'/tracking_'+date+'_'+strjoin(trackfields,'_')+dxdetails+'.png'
  
  basedir = '/glade/p/nmmm0024/'
  mpass=['mpas','mpas_al','mpas_wp','GFS']
  mpass=['mpas_ep']
  ;mpass=['gfdl_tracker.onebig']
  
  maps=list()
  for impas = 0, n_elements(mpass)-1 do begin
    mpas = mpas_mesh(mpass[impas],/nomesh)
    
    path = basedir+mpass[impas]+'/'+date+'/latlon'+dxdetails+'/'
    if strmatch(mpas.name, 'GFS*',/fold) then path =  '/glade/scratch/ahijevyc/'+mpass[impas]+'/'+date+'/'
    if file_test(path) eq 0 then continue ; skip if this date directory doesn't exist. needed after I added mpas3, which
    ; only has September 2013.
    
    if ~debug then begin
      latmin=5 & latmax=50
      lonmin=-180 & lonmax=180
      latmin=8 &  latmax=32.5 & lonmin=-122.5 & lonmax=-90.5
      thick = min([6,360*4/(lonmax-lonmin)])
    endif else begin
      latmin=5 & latmax=50
      lonmin=-180 & lonmax=180
    endelse
    font_size = min([16,360.*9/(lonmax-lonmin) + clean])
    ydim = 1.8*(latmax-latmin)/(lonmax-lonmin)*xdim*n_elements(mpass)
    ymax = 1200 ; pixels
    map = map('Cylindrical', limit=[latmin, lonmin, latmax, lonmax], fill_color='light blue', $
      dimensions=[xdim, ydim]*min([ydim, ymax])/ydim, font_size=font_size, $
      current=impas, layout=[1,n_elements(mpass),impas+1], buffer=buffer, margin=0.1)
    maps.add, map
    grid = map.MAPGRID
    if ~debug then map.Refresh, /disable
    grid.thick=0
    grid.linestyle = 'dotted'
    grid.LABEL_POSITION = 0
    m1 = mapcontinents(fill_color='beige', /continents, thick=90/(lonmax-lonmin))
    legend_items = list()
    
    
    
    for ifield = 0, n_elements(trackfields)-1 do begin
      trackfield = trackfields[ifield]
      tracks_file = path+trackfield+'_'+smooth_radius+'_tracks.sav'
      if trackfield eq 'gfdl' then begin
        starting_at_hour = 0 ; Set to 6 for tracking vorticity. Not used for gfdl tracking. Needs to be 0 for time window of best tracks.
        max_intensity_to_plot = 0. ; knots
        mean_intensity_to_plot = 0
      endif
      
      my_tracking_params = 'Ahijevych tracking params: starting at hour ' + string(starting_at_hour, format='(I0)') + $
        ', time step=' +strtrim(round(delta*24),2)+ 'h, max time gap='+$
        string(round(max_time_gap*24.),format='(I0)')+ 'h, extrap search radius='+ $
        strtrim(round(extrapolation_search_radius/1000),2)+ 'km, no extrap search radius=' + $
        strtrim(round(no_extrap_search_radius/1000),2) + 'km, use_extrap='+strtrim(use_extrap,2)+'!C'
      filter_params = 'min duration to plot=' + strtrim(round(24*minimum_duration2plot),2) + 'h' + $
        ', min avg intensity to plot=' + string(mean_intensity_to_plot,format='(E7.1)') + $
        '!Csmallest max intensity to plot=' + string(max_intensity_to_plot,format='(E7.1)')
        
      mcv_lats              = replicate(!VALUES.D_NAN, max_mcvs, max_time_dim_size)
      mcv_lons              = replicate(!VALUES.D_NAN, max_mcvs, max_time_dim_size)
      mcv_times             = replicate(!VALUES.D_NAN, max_mcvs, max_time_dim_size) ; not just start time. all times for an MCV.
      mcv_intensity         = replicate(!VALUES.F_NAN, max_mcvs, max_time_dim_size)
      mcv_id                = replicate('',max_mcvs)
      
      ; special case for trackfield 'gfdl', GFDL vortex tracker. Don't track, just read fort.66
      if trackfield eq 'gfdl' then begin
        gfdl_m = read_atcf(path+'gfdl_tracker/fort.66', storms=storms, GFDL_warmcore_only=GFDL_warmcore_only, $
          lats=mcv_lats, lons=mcv_lons, times=mcv_times, intensity=mcv_intensity, id=mcv_id)
        map.title = path+'!C'+filter_params
        if GFDL_warmcore_only then map.title.string=map.title.string + ', GFDL warm core only'
        if clean then begin
          map.title = mpas.name ; If you want clean titles.
          map.title.font_size=18
        endif
        nstorm=n_elements(storms)
        next_empty_mcv = nstorm
        times = gfdl_m.valid_time
      endif else if file_test(tracks_file) && force_new eq 0 then begin
        restore, tracks_file
        map.title = tracks_file + title_string(specs) + my_tracking_params + filter_params
      endif else begin
      
        cd, path
        feature_info_file_search_string          = '*_'+smooth_radius+'.'+trackfield+'_maxima_info.txt'
        if mpas.name eq 'GFS004' then feature_info_file_search_string = '*.'+trackfield+'_maxima_info.txt'
        
        data = read_maxima_info_txt(feature_info_file_search_string, specs=specs)
        map.title = path + feature_info_file_search_string + title_string(specs) + my_tracking_params + filter_params
        
        ; fix for mpas3. Before JUN 2014  mpas3 was accidentally lat/lon ized with mpas2's fortran save file.
        ; The end result was mpas3 longitude was shifted 155 deg, but otherwise was fine.
        ;if mpas.name eq 'mpas3' then data.lon = data.lon-155d
        
        ; map longitude from -180 to 180 - helpful for wrapping tracks
        ; not only does this next part fix lons in kludged mpas3, but also GFS004.
        imod = where(data.lon lt -180., /null)
        if imod ne !NULL then data.lon[imod] = data.lon[imod] + 360.
        imod = where(data.lon ge 180., /null)
        if imod ne !NULL then data.lon[imod] = data.lon[imod] - 360.
        
        times = data.valid_time
        
        ; TIME LOOP - min time thru max time by delta time.
        for time = init_time+starting_at_hour/24d, max(times), delta do begin
          ; Find all MCVs at this time
          itime = where(abs(times - time) lt 0.01, n_current_mcvs)
          if debug then print, jday_to_string(time)+': there are ', strtrim(n_current_mcvs,2), ' current mcvs'
          if n_current_mcvs gt 0 then begin
            newlons=data.lon[itime]
            newlats=data.lat[itime]
            newintensities=data.intensity[itime]
            ; Number of old, tracked mcvs that are matched to a "new" mcv. reset each time.
            tracked_mcvs_matched_by_current_mcvs = replicate(-1L, n_current_mcvs, 10)
          endif else begin
            print, 'no mcvs at '+jday_to_string(time)
            continue
          endelse
          
          
          
          ; Loop through mcvs that have already been tracked.
          ; See if there is a current feature that matches a feature that has already been tracked.
          ; It can't be too separated in time or space.
          next_empty_mcv = max(where(finite(mcv_times[*,0])))+1
          if debug gt 1 then print, '  next empty tracked mcv id is ', next_empty_mcv
          for jmcv = 0, next_empty_mcv-1 do begin
            mcv_itimes = where(finite(mcv_times[jmcv,*]))
            last_itime = max(mcv_itimes)
            n_matches_for_tracked_mcv = 0
            
            ; If last known location was too long ago then move to next tracked mcv.
            time_since_last_point = time - mcv_times[jmcv,last_itime]
            if time_since_last_point gt max_time_gap then continue
            
            
            ; for this tracked mcv calculate the distance to all current mcvs and find shortest distance, min_d.
            ; and count the number of current mcvs within search radius (n_matches_for_tracked_mcv)
            expected_lon = mcv_lons[jmcv, last_itime]
            expected_lat = mcv_lats[jmcv, last_itime]
            if use_extrap then begin
              ; if extrapolation is used, extrapolate the previous position to a new position based on previous movement.
              expected_lonlat = extrapolate(jmcv, mcv_lons, mcv_lats, mcv_times, time)
              expected_lon = expected_lonlat[0]
              expected_lat = expected_lonlat[1]
            endif
            min_d = !VALUES.D_INFINITY
            i_closest_current_mcv = -1
            i_close_current_mcvs = !NULL
            max_dist_gap = no_extrap_search_radius
            if use_extrap and last_itime ge 1 then max_dist_gap = extrapolation_search_radius ; make search radius small if extrapolation is used and you have 2 pts to extrapolate.
            ; loop through all mcvs found in current time.
            for i = 0, n_current_mcvs-1 do begin
              ; for this tracked mcv, see how far each current mcv is.
              result = map_2points(expected_lon, expected_lat, newlons[i], newlats[i])
              d =  result[0]*!DTOR*!CONST.R_earth
              if debug gt 1 then print, 'distance from mcv track ', strtrim(jmcv,2), ' to new mcv ', strtrim(i,2), ': ', string(d/1000., format='(F6.0,"km")')
              ; Keep track of current mcv that is closest to this tracked mcv.
              if d lt min_d then begin
                min_d = d
                i_closest_current_mcv = i
                ; remembers tracked mcvs that match each current one (tracked_mcvs_matched_by_current_mcvs).
                ; Each row of this array represents one of the current mcvs. If fist element is still -1L after looping thru tracked mcvs,
                ;  the current mcv represented by that element must be new.
                ; more than one tracked tracked_mcv means this is a merger?
                if d lt max_dist_gap then tracked_mcvs_matched_by_current_mcvs[i, total(tracked_mcvs_matched_by_current_mcvs[i,*] ne -1L) ] = jmcv
              endif
              
              ; If current mcv is within than the distance threshold.
              if d lt max_dist_gap then begin
                n_matches_for_tracked_mcv = n_matches_for_tracked_mcv+1 ; if > 1 this is a split? (closest one kept, further ones ignored)
                i_close_current_mcvs = [i_close_current_mcvs, i]
              endif
            endfor ; loop thru all mcvs found in current time. potential mcvs to be matched or start tracking
            
            ; find_maxima eliminates neighbors in square gridpoint box, so you shouldn't have "clusters" any more
            ; near the equator. Near the poles, the gridpoint box doesn't cover as much territory because x (longitude)
            ; spacing goes to zero.
            if n_matches_for_tracked_mcv ge 4 && abs(expected_lat) lt 80 then stop; >1 means some mcvs found in current time were within search radius of tracked mcv, but not closest.
            
            if min_d gt max_dist_gap then begin
              ;  No current mcvs are close to this tracked mcv.
              if debug gt 0 then print, '  closest neighbor to mcv ', strtrim(jmcv,2), ' is ', string(round(min_d/1000.), jday_to_string(time), $
                jday_to_string(mcv_times[jmcv,last_itime]), mcv_lats[jmcv,last_itime], mcv_lons[jmcv,last_itime], $
                format='(I0," km away at ",A,". last track position at ", A,":", F7.2,"N",F8.2,"E")')
            endif else if tracked_mcvs_matched_by_current_mcvs[i_closest_current_mcv,1] eq -1L then begin
              ; if the closest current mcv has not already been assigned to another tracked mcv
              ; this tracked mcv will have its track augmented by closest current mcv.
              if debug gt 0 then print, '  mcv track ', strtrim(jmcv,2), ' matches current mcv ', strtrim(i_closest_current_mcv,2), ', ', strtrim(round(min_d/1000.),2), ' km away'
              
              mcv_times           [jmcv,last_itime+1]   = time
              take_average=0
              ; Take average of all close current features, or just the closest one.
              if (take_average) then begin
                mcv_lats            [jmcv,last_itime+1]   = mean(newlats[i_close_current_mcvs])
                mcv_lons            [jmcv,last_itime+1]   = atan(mean(sin(!DTOR*newlons[i_close_current_mcvs])), mean(cos(!DTOR*newlons[i_close_current_mcvs])))*!RADEG
                mcv_intensity       [jmcv,last_itime+1]   = mean(newintensities[i_close_current_mcvs])
              endif else begin
                mcv_lats            [jmcv,last_itime+1]   = newlats[i_closest_current_mcv]
                mcv_lons            [jmcv,last_itime+1]   = newlons[i_closest_current_mcv]
                mcv_intensity       [jmcv,last_itime+1]   = newintensities[i_closest_current_mcv]
              endelse
            endif else begin
              if debug then print, '  tracked mcv ', strtrim(jmcv,2), ' matches current mcv ', strtrim(i_closest_current_mcv,2),$
                ', but current mcv already added to tracked mcv ', strtrim(tracked_mcvs_matched_by_current_mcvs[i_closest_current_mcv,0],2)
            endelse
            
          endfor ; tracked mcv loop
          if max(total(tracked_mcvs_matched_by_current_mcvs ne -1L,2)) ge 5 then stop ; n tracked mcvs have merged into the same current mcv.
          
          
          ; Start tracking mcvs that haven't been matched to an old tracked mcv.
          if n_current_mcvs eq 0 then continue ; if there are no mcvs at this time, continue
          ; if there are mcvs then activate new ones (ones that aren't tagged on an old tracked one).
          inew = where(tracked_mcvs_matched_by_current_mcvs[*,0] eq -1L, nnew)
          if nnew gt 0 then begin
            if debug then print, '  ', strtrim(nnew,2), ' new mcvs at ' , jday_to_string(time) ;, string(inew, format='(I4)')
            mcv_lats            [next_empty_mcv:next_empty_mcv+nnew-1,0]   = newlats[inew]
            mcv_lons            [next_empty_mcv:next_empty_mcv+nnew-1,0]   = newlons[inew]
            mcv_times           [next_empty_mcv:next_empty_mcv+nnew-1,0]   = time
            mcv_intensity       [next_empty_mcv:next_empty_mcv+nnew-1,0]   = newintensities[inew]
          endif
          print, '.', format='(A,$)'
          endfor ; time loop
        if ~debug then save, mpas, mcv_lons, mcv_lats, mcv_times, mcv_intensity, trackfield, use_extrap, max_time_gap, date, $
          extrapolation_search_radius, no_extrap_search_radius, max_mcvs, delta, times, init_time, starting_at_hour, $
          max_time_dim_size, data, next_empty_mcv, specs, filename=tracks_file
      endelse
      
      durations = replicate(!VALUES.F_NAN, next_empty_mcv)
      colors = floor((mcv_times-init_time))
      
      for jmcv = 0, next_empty_mcv-1 do begin
      
        last_itime = max(where(finite(mcv_times[jmcv,*])))
        ; skip empty rows in GFDL matrix
        if last_itime eq -1L then continue
        ; duration threshold
        durations[jmcv] = mcv_times[jmcv, last_itime] - mcv_times[jmcv,0]
        if debug le 1 && durations[jmcv] lt minimum_duration2plot then continue
        ; mcv intensity threshold (vorticity maximum)
        mean_mcv_intensity = mean(mcv_intensity[jmcv,0:last_itime])
        if mean_mcv_intensity lt mean_intensity_to_plot then continue
        if max(mcv_intensity[jmcv,*], /nan) lt max_intensity_to_plot then begin
          if debug gt 0 then print, mcv_id[jmcv] + ' only gets up to ', max(mcv_intensity[jmcv,*],/nan), ' intensity', format='(A, E12.3, A)'
          continue
        endif
        
        if max(mcv_lons[jmcv,*]) lt lonmin || min(mcv_lons[jmcv,*]) gt lonmax || $
          max(mcv_lats[jmcv,*]) lt latmin || min(mcv_lats[jmcv,*]) gt latmax then continue ; skip if outside the plot window
          
        if ~debug then map.Refresh, /disable
        ; label start and end point
        ;xyouts, mcv_lons[jmcv,[0,last_itime]], mcv_lats[jmcv,[0,last_itime]], [jday_to_string(mcv_times[jmcv,0]), (debug?strtrim(jmcv,2)+'!C':'')+jday_to_string(mcv_times[jmcv,last_itime])], align=0.5, noclip=0, charsize=0.6
        stormid = mcv_id[jmcv] ne '' ? mcv_id[jmcv] : '#'+strtrim(jmcv,2)
        if strlen(stormid) gt 10 then stormid = strmid(stormid,0,strpos(stormid,'_')-10)+strmid(stormid,strpos(stormid,'_',/reverse_search))
        ; if you want this clean comment this out
        if ~clean then text = text(mcv_lons[jmcv,0],   mcv_lats[jmcv,0], stormid, $
          vertical_alignment=1.0, font_size=font_size*.7, align=0.5, target=map, /data, transparency=25)
          
        ; kludge to deal with longitude wrapping. where path wraps, break into 2 paths.
        wraps = where(abs(mcv_lons[jmcv,*] - shift(mcv_lons[jmcv,*],1)) gt 180, nwrap)
        
        wrapstart = 0L
        for iwrap = 0, nwrap-1 do begin
          wrap = wraps[iwrap]
          if wrap eq 0 then continue ; rare case when the track wraps and fills up all the time slots.
          a = plot(mcv_lons[jmcv,wrapstart:wrap-1], mcv_lats[jmcv,wrapstart:wrap-1], vert_colors=transpose(rgb_table(colors[jmcv,wrapstart:wrap-1],*)),$
            /data, thick=thick, transparency=30, linestyle=trackfield eq 'vorticity_850hPa' ? 0 : 2, overplot=map, name=stormid)
          wrapstart = wrap
        endfor ; tracks that wrap
        
        a = plot(mcv_lons[jmcv,wrapstart:last_itime], mcv_lats[jmcv,wrapstart:last_itime], vert_colors=transpose(rgb_table(colors[jmcv,wrapstart:last_itime],*)),$
          /data, thick=thick, transparency=30, linestyle=trackfield eq 'vorticity_850hPa' ? 0 : 2, overplot=map, name=stormid)
        if debug gt 0 then print, 'plotted ', mcv_id[jmcv]
          
      endfor ; feature (jmcv loop)
      
      
      ; plot, x, histogram(durations, min=0, max=9, binsize=delta, locations=x), /ylog, yrange=[.9,next_empty_mcv], psym=10
      if debug then print, trackfield+' max duration =', max(durations, imax), ' days. '
      li = a
      li.name = trackfield
      legend_items.add, li
    endfor ; 850hPa and 700hPa (trackfield)
    best_tracks, init_time, maxtime, init_time+starting_at_hour/24d, max(times), map, rgb_table, bts=bts
    map.refresh
    
  endfor ; impas (mpas  mpas2 mpas3 GFS004)
  ; between 2nd-to-last and last maps
  ypos = n_elements(mpass) gt 1 ? ((maps[-2].position)[1] + (maps[-1].position)[3])*0.5 : 0.05
  xpos = 0.3
  ; tried using a range of julian days, but results were inconsistent. perhaps it converts double precision to integers
  c = colorbar(rgb_table=rgb_table, position=[xpos,ypos,xpos+max([0.6,360/(map.window.dimensions)[0]]),ypos+0.01], $
    range=[0,11], tickinterval=1, ticklen=1, font_size=12, subticklen=0, title=string(init_time,format='(C(CYI4))'), $
    minor=1, border=1, tickname=string(init_time+indgen(maxtime-init_time+1), format='(C(CMoA,CDI))'))
  legend = legend(target=[legend_items.ToArray(),bts.ToArray()], position=[xpos-0.03,ypos], font_size=font_size, $
    vertical_alignment=0.65, vertical_spacing=0.008)
  stamp = text(0.01, 0.01, systime(),/normal, font_size=8)
  
  if debug || n_elements(mpass) lt 4 then begin
    print, 'saving '+ file_dirname(ofile)+'/'+file_basename(ofile)
    map.window.save, file_dirname(ofile)+'/'+file_basename(ofile), resolution=100
  endif else begin
    print, 'saving '+ofile
    map.window.save, ofile, resolution=125
  endelse
  map.window.close
  
end






pro best_tracks, init_time, finish, forecast_start, forecast_end, map, rgb_table, bts=bts
  bts=list()
  ; colors are scaled from init_time to finish, but
  ; tracks are plotted from forecast_start to forecast_end.
  ;loadct, 38, /silent
  ;tvlct, rgb_table, /get
  
  ; used firefox and download 'em all to get all *.dat files in weather.unisys.com/hurricane/*/2013 - on Nov 13.
  caldat, init_time, month, day, startyear, hour, minute ; just here to capture year
  caldat, finish, month, day, endyear, hour, minute ; just use year
  
  for year = startyear, endyear do begin
    ; uncomment for Unisys files
    ;files = file_search('/glade/work/ahijevyc/hurricane/*/'+strtrim(year,2)+'/*_track.dat', count=nfiles)
    files = file_search('/glade/work/ahijevyc/atcf/b??[0-6][0-9]????.dat', count=nfiles)
    ; files = file_search('/glade/scratch/ahijevyc/mpas2/2013091300/latlon'+dxdetails+'/gfdl_tracker/a??[0-9][0-9][0-9][0-9][12][0-9][0-9][0-9].dat', count=nfiles)
    
    for ifile = 0, nfiles-1 do begin
      file = files[ifile]
      unisys_file = strmid(file,9,/rev) eq '_track.dat'
      ;print, "plotting best track "+file
      name = file_basename(file, '_track.dat')
      align = 0.5
      savfile = file + '.sav'
      ;      if file_test(savfile) then restore, savfile else begin
      if file_test(file, /zero) eq 1 then continue
      if unisys_file then st = read_unisys_best_track(file) else st = read_atcf(file)
      ;        save, st, filename=savfile
      ;      endelse
      tc_track_jdays = st.valid_time
      junk=max(st.vmax,imax)
      stormname = (st.stormname)[imax]
      
      scale = 360./((map.limit)[3]-(map.limit)[1])
      font_size = min([18,8.*scale])
      sym_size = min([2, 0.6 * scale])
      
      itimes = where(tc_track_jdays ge forecast_start and tc_track_jdays le forecast_end, ntimes)
      if ntimes gt 0 then begin
        colors = floor(tc_track_jdays-init_time)
        ; Don't worry about longitude wrapping. We are just doing symbols and no continuous lines.
        lon = st.lon[itimes]
        lat = st.lat[itimes]
        
        spds = [0,35,65,!VALUES.F_INFINITY] ; wind categories (0-35, 35-65, and 65+ knots)
        for ispd = 0, n_elements(spds)-2 do begin
          idots = where(st.vmax[itimes] ge spds[ispd] and st.vmax[itimes] lt spds[ispd+1], ndots, /null)
          if ndots gt 0 then begin
            junk = plot(lon[idots], lat[idots], sym_object=TC_symbol(spds[ispd]), linestyle=6, sym_transparency=0.2, $
              vert_colors=(colors[itimes])[idots], rgb_table=rgb_table, /data, overplot=map, name=stormname, sym_size=sym_size)
            ; add to legend plot list if this is the highest wind category the storm achieves.
            ;if max(st.vmax[itimes]) lt spds[ispd+1] && lon[0] ge (map.limit)[1] && lon[0] le (map.limit)[3] $
            ;  && lat[0] ge (map.limit)[0] && lat[0] le (map.limit)[2] then bts.add, junk ; if no winds in next category add plot to legend
          endif
          
        endfor
        
        t = text(st.lon[itimes[0]], st.lat[itimes[0]], stormname, font_size=font_size, /data, target=map, transparency=20)
        print, stormname
      endif
    endfor ; basins/files
  endfor ; year
  
  return
end


pro hits_and_misses

  ; Reads output from potential_matches.pro
    
  atmos_const  
  if !D.NAME eq 'X' then erase
  outdir = '/glade/work/ahijevyc/mpas_plots/'
  basins = ['EP','AL','WP']
  models = ['mpas','mpas_ep','GFS']
  hm_file='/glade/work/ahijevyc/tracking_gfdl/hm.'+strjoin(models,'.')
  if file_test(hm_file) eq 0 then begin
    openw, lun, hm_file, /get_lun
    foreach model, models do begin
      for fh=0,240,6 do print, potential_matches(model, '2014080100', '2014110300', fh=fh, spd=34, lun=lun)
    endforeach
    free_lun, lun
  endif
  if file_test(hm_file, /zero_length) then print, hm_file + ' is zero length'
  t = read_ascii(hm_file, template=hits_and_misses_template())
  units='kt'
  if where(strmid(t.VMAX,1,/rev) ne units,/null) ne !NULL then stop
  spd = LONG(t.VMAX)
  foreach model, models, imodel do begin
    hms = ['hit','miss']
    yrange = model eq 'GFS' ? [0,120] : [0,40] ; 4x more in GFS
    hplots=list()
    foreach hm, hms, ihm do begin
      map = map('Cylindrical', limit=[0, -180, 50, 180], fill_color='light blue', layout=[1,3,ihm+1], buffer=0, current=ihm)
      grid = map.MAPGRID
      grid.thick=0
      grid.linestyle = 'dotted'
      grid.LABEL_POSITION = 0
      m1 = mapcontinents(fill_color='beige', /continents, thick=0.5)
      
      i = where(t.TECH eq model and t.hm eq hm, ni, /null)
      if i eq !NULL then continue
      ; remove duplicates - there are a lot of these. Up to 10 model hits and misses match up to a single best track.
      grid_input, t.lon[i], t.lat[i], spd[i], x, y, color1
      maxspd=100
      dots = scatterplot(x, y, symbol="Circle", sym_filled=1, magnitude=bytscl(color1,max=maxspd), rgb_table=13, overplot=map, sym_size=0.25)
      if ihm eq 0 then c = colorbar(target=dots,title=units,taper=3,range=[min(spd[i]),maxspd],/relative,position=[0.25,-0.55,0.75,-0.49])
      map.title = strupcase(model)+" "+ hm + " best tracks"
      hm_vmax_file = '/glade/scratch/ahijevyc/'+model+'/'+hm+'_vmax'
      if file_test(hm_vmax_file) eq 0 then begin
        print, 'no '+hm_vmax_file+'. run ~/get_first_hit.csh on '+hm_file+' first?'
        stop
      endif
      hm_vmax = read_ascii(hm_vmax_file,template=hits_and_misses_template())
      for ibasin=0,n_elements(basins)-1 do begin
        basin = basins[ibasin]
        i = where(t.TECH eq model and t.hm eq hm and t.basin eq basin, ni, /null)
        if i eq !NULL then continue
        pdf = histogram(spd[i], min=25, max=170, binsize=10, locations=xvals)
        hplots.add, plot(xvals, pdf, /current, layout=[3,3,7+ibasin], /histogram, title=basin, $
          linestyle=hm eq 'miss', margin=[0.2,0.3,0.1,0.15], overplot=hplots.count() lt n_elements(basins) ? 0 : hplots[ibasin], name=string(total(pdf),format='(I0)')+" "+hm, $
          xtitle='vmax '+units)
        
          
        !P.MULTI = [n_elements(basins)*n_elements(models)-ibasin-n_elements(basins)*imodel,n_elements(basins),2]
        ; plot miss histogram in dotted line
        ii = where(hm_vmax.basin eq basin and hm_vmax.hm eq hm, nii, /null)
        y = histogram(long(hm_vmax.vmax[ii])*!ATMOS.kts2mps, binsize=5, min=0, max=100, locations=xloc)
        plot, xloc+2.5, y, psym=10, linestyle=hm eq 'hit'?0:1, xtitle='observed peak wind speed along track (m/s)', $
          title=model+' '+basin, subtitle='solid=hits, dotted=misses', yrange=yrange, charsize=2
        xyouts, !X.CRANGE[0]+0.6*(!X.CRANGE[1]-!X.CRANGE[0]), !Y.CRANGE[0]+(ihm*0.05+0.8)*(!Y.CRANGE[1]-!Y.CRANGE[0]), $
          string(nii, format='(I4)')+' '+hm, align=0, charsize=3/!P.MULTI[1]
        
      endfor ; basin
    endforeach ; hit and miss maps
    
    ; add hit and miss line legend
    for ibasin=0,n_elements(basins)-1 do l = legend(target=hplots[ibasin:*:n_elements(basins)], /relative, position=[1.21,0.99], font_size=7, vertical_spacing=0.01, horizontal_spacing=0.01)

    ofile=outdir+model+'_hit_miss.png'
    map.window.save, ofile, resolution=175
    print, 'wrote ',ofile
    
  endforeach; model
end

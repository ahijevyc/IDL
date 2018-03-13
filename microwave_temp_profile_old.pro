function get_color,ialt,nalt
  if nalt eq 1 then return, !D.NAME eq 'PS'? 0:255
  old_color = 1+ialt*254./(nalt-1)
  return, 255-old_color
end


function avg_LST, in_mission

  ipgi = strpos(in_mission, 'PGI')
  mission = strmid(in_mission,ipgi,22)
  case mission of
    'PGI27L_20100817_120000': time =   8.58391
    'PGI27L_20100818_150000': time =   9.14827
    'PGI30L_20100821_120000': time =   7.91732
    'PGI36L_20100830_120000': time =   10.7421
    'PGI36L_20100831_120000': time =   9.84153
    'PGI36L_20100901_120000': time =   10.1943
    'PGI38L_20100902_180000': time =   13.9799
    'PGI38L_20100903_180000': time =   13.7161
    'PGI38L_20100905_180000': time =   12.8289
    'PGI38L_20100906_150000': time =   10.7449
    'PGI44L_20100910_120000': time =   7.83981
    'PGI44L_20100910_180000': time =   15.1523
    'PGI44L_20100911_180000': time =   13.7035
    'PGI44L_20100912_120000': time =   9.28156
    'PGI44L_20100913_120000': time =   8.12226
    'PGI44L_20100914_180000': time =   12.3107
    'PGI46L_20100920_150000': time =   11.7427
    'PGI46L_20100921_150000': time =   10.8139
    'PGI46L_20100922_180000': time =   11.3375
    'PGI46L_20100924_150000': time =   11.2200
    'PGI50L_20100927_150000': time =   10.8573
    'PGI50L_20100928_150000': time =   11.3420
    'PGI48L_20100930_150000': time =   12.0022
    'PGI51L_20100930_150000': time =   12.0022
  endcase
  return, time
end

pro plot_meanMTP, usedrops=usedrops
  if ~keyword_set(usedrops) then usedrops = 0
  whichAlt = 'press_alt'
  loadct, 39
  !P.MULTI=[0,1,1]
  if !D.NAME eq 'X' then begin
    device, decomposed=0
    !P.CHARSIZE=2.0
    !P.CHARTHICK=1.0
    erase
  endif
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  
  path = basedir+'PREDICT/MTP/'+(usedrops eq 1 ? 'usedrops/' : '')
  files = file_search(path + 'mission_avg_profile_PGI*_2010*_*_' + whichAlt + '.txt', count=nfiles)
  
  ; Do all-mission average profile
  if (0) then begin
    savfile = path + 'plot_meanMTP.sav'
    if file_test(savfile) then restore, savfile else begin
      nfiniteTs = replicate(0., nalts)
      TsTotal = replicate(0., nalts)
      for ifile = 0, nfiles-1 do begin
        t = read_ascii(files[ifile], data_start=1, count=nalt)
        avgHgt = t.field1[0,*]
        if ifile gt 0 && array_equal(avgHgt, oldhgt) eq 0 then stop
        temperature = t.field1[1,*]
        nfiniteTs = nfiniteTs + finite(temperature)
        igood = where(finite(temperature),ngood)
        if ngood gt 0 then TsTotal[igood] = TsTotal[igood] + temperature[igood]
        oldhgt = avgHgt
      endfor
      
      avgTs = TsTotal/nfiniteTs
      save, avgHgt, avgTs, filename=savfile
    endelse
    ; problem with 'divide_by_1000'? compile cfad_mtp.pro
    plot, avgTs, avgHgt, ytickformat='divide_by_1000', ystyle=16
  endif
  
  t = read_ascii(files[0], data_start=1, count=nalts)
  time_beg = 7
  time_end = 16
  hours = time_beg + findgen(time_end-time_beg+1)
  colors = 10+findgen(n_elements(hours)-1)/(n_elements(hours)-2)*244
  
  if !D.NAME eq 'X' then wset, 0
  plot, [0], [0], ytickformat='divide_by_1000', yrange=[2000,22000], xrange=[-80,0], /nodata
  
  for ifile = 0, nfiles-1 do begin
    file = files[ifile]
    hour = avg_LST(file_basename(file))
    color = colors[max(where(hours lt hour))]
    ;print, file, color
    t = read_ascii(file, data_start=1)
    hgt = t.field1[0,*]
    temperature = t.field1[1,*]
    bfile = file_basename(file)
    pgi = strmid(bfile,20,6)
    datestring = strmid(bfile,27,8)
    hhmmss = strmid(bfile, 36, 6)
    
    mission = pgi + datestring + '_' + hhmmss +'_' + whichAlt
    path = basedir+'PREDICT/MTP/'+(usedrops?'usedrops/':'')
    savfile = path + 'savfiles/' + mission + '.sav'
    restore, savfile
    
    ; added a distance threshold Aug 28, 2013. In Chris' paper he assumes <500km 
    distance_threshold = 500.
    dist2center = sqrt(time_series_x_km^2. + time_series_y_km^2.)
    igood_MTP = where(good_MTP and dist2center[*,0] lt distance_threshold)
    mtm_center, pgi, datestring, hhmmss, clat, clon, /mtm00, /silent
    print, bfile, " distance < ", string(distance_threshold, format='(I6,"km")'), $
                  min(temperature, /nan), mean(time_series_cold_pt[igood_MTP], /nan), $
                                        stddev(time_series_cold_pt[igood_MTP], /nan), $
                                      variance(time_series_cold_pt[igood_MTP], /nan), $
                                          mean(time_series_Zcold_pt[igood_MTP], /nan), $
                                        stddev(time_series_Zcold_pt[igood_MTP], /nan), $
                                      variance(time_series_Zcold_pt[igood_MTP], /nan), clat, clon, $
                                      format='(A, A, A, 9F9.4)' 
    oplot, temperature, hgt, color=color
    ;    xyouts, temperature[6000], 6000, string(hour, format='(F4.1)'), color = color, align=0.5, charsize=0.8
    ; colorbar designed to work with version in snesbitt/.
    colorbar, hours, colors, format='(I0)',/col,unit='LST',lowleft=[0,!Y.WINDOW[0]], /right
    
  ;    xyouts, (temperature-avgTs)[6000], 5700, string(hour, format='(F4.1)'), color = color, align=0.5, charsize=2, charthick=2
  endfor
  
end





pro plot_bottom_panel, posa, xrange, nalt, time_series_dropTimes,dist2center,time_series_IR,time_series_MTPTimes,time_series_GV_temp,$
    time_series_IR_blocks,time_block_starts,time_block_minutes, time_series_SatIRtime
    
  ; add x axis on top of top panel
  axis, xaxis=1, xrange=xrange, xtickformat=['LABEL_DATE','LABEL_DATE'], xminor=4, xticklen=1
  distColor = 89
  distThick = !D.NAME eq 'PS' ? 3.78 : 1.1
  GVcolor = 252
  time_block_color = 151
  
  plot, [0],[0], position=posa, /noerase, ystyle=4, xrange=xrange, xstyle=4, /nodata
  
  ; draw boxes around times of different IR satellite images
  ; This may result in an error if there are no data at this level (e.g. PGI51L 20120930 at 13km)
  ; try a middle altitude instead of the first one. (replaced 0 with nalt/2)
  i_good_IRtimes = where(finite(time_series_SatIRtime[*,nalt/2]),ngood_IR)
  if ngood_IR gt 0 then begin
    good_IRtimes = time_series_SatIRtime[i_good_IR_times,nalt/2]
    inew_IRtimes = uniq(good_IRtimes)
    tvlct, oldct, /get
    tvlct, replicate(!D.NAME eq 'PS'?237:40,[1,3]), 1
    for ibox = 1, n_elements(inew_IRtimes)-2,2 do begin
      x = good_IRtimes[inew_IRtimes[ibox:ibox+1],0]
      polyfill, [x, reverse(x),x[0]], [!Y.CRANGE[0],!Y.CRANGE,reverse(!Y.CRANGE)],color=1
    endfor
    tvlct, oldct
  endif
  
  axis, xaxis=0, xrange=xrange, xtickformat=['LABEL_DATE','LABEL_DATE'], xminor=4, xticklen=1
  axis, 0, -0.26, xaxis=0, xrange=xrange, xtickformat='kiloSeconds'
  dist_range = [0,1000]
  for ialt=0,nalt-1 do plot, time_series_dropTimes[*,ialt], dist2center[*,ialt], yrange=dist_range, psym=3, $
    xminor=4, position=posa, /noerase, ystyle=13, xstyle=5, xrange=!X.CRANGE, color=distColor, thick=distThick
  if total(finite(dist2center)) gt 0 then begin
    xyouts, max(time_series_dropTimes), dist2center[max(where(finite(dist2center)))], 'distance', color=distColor, charsize=0.62, noclip=0
    axis, yaxis=1, charsize=0.9, charthick=distThick, ytitle='distance to center!Ckm', yrange=dist_range, ystyle=1, color=distColor, $
      ythick=distThick/4., ytickinterval=100., yminor=1
  endif
  ; plot IR satellite temperature at closest pixel
  IR_range = [-82,29]
  for ialt = 0, nalt-1 do begin
    ; for some reason the psym=10 histogram setting goes off the scale on 0817 when it ends in NaN
    ;    ifinite = where(finite(time_series_dropTimes[*,ialt]),nfinite)
    ;    if nfinite gt 0 then
    plot, time_series_dropTimes[*,ialt], time_series_IR[*,ialt], yrange=IR_range, $
      position=posa, /noerase, ystyle=9, xstyle=5, xrange=!x.crange, $
      charsize=0.9, charthick=1.1, thick=2.5, ytitle='GOES IR temp!C!Z(00B0)C'
  endfor
  ; Plot MTP horizon brightness temperature
  oplot, time_series_MTPTimes, time_series_GV_temp-!CONST.T0, thick=3.87, color=GVcolor
  xyouts, min(time_series_MTPTimes,/nan), time_series_GV_temp[max(where(finite(time_series_GV_temp)))]-!CONST.T0, 'MTP!Chorizon temp', $
    color=GVcolor, charsize=0.62, noclip=0, align=1
  ; Plot the 15-minute blocks of mode of IR temperaure mode.
  ; Like dist2center, the start times of the IR time series vary with dropsonde level,
  ; and it looks kind of messy on the time series.  But IR blocks are aligned on top of each other
  ; because they are forced to be aligned with multiples of 15 minutes.
  for ialt=0,nalt-1 do begin
    if total(finite(time_series_dropTimes[*,ialt])) eq 0 then continue
    ; cut time series into blocks of time starting at the first multiple of time_block_minutes in the time series.
    time_block_minutes = 15d ; double is safer
    do_time_blocks, time_series_IR[*,ialt], time_series_dropTimes[*,ialt], time_block_minutes, time_series_IR_block, time_block_start, mode=40 ; get most common mode-deg interval
    time_series_IR_blocks[0:n_elements(time_series_IR_block)-1,ialt] = time_series_IR_block
    time_block_starts[0:n_elements(time_block_start)-1,ialt] = time_block_start
    oplot, time_block_start+time_block_minutes/24d/60d/2d, time_series_IR_block, psym=10, color=time_block_color, thick=5, linestyle=3
    ilast_good_IRblock = where(finite(time_series_IR_block),nlast_good_value)
    if nlast_good_value gt 0 then xyouts, max(time_block_start), time_series_IR_block[max(ilast_good_IRblock)], string(time_block_minutes,format='("lo/mid/hi clouds!C",I0, "min blocks")'), color=time_block_color, charsize=0.622, noclip=0
  endfor
  xyouts, !X.WINDOW[1], 0, systime(), align=1, charsize=!P.CHARSIZE*0.5, /norm
  
end

pro scatter, xs, ys, ialt, alt_reqs
  nalt=n_elements(alt_reqs)
  for ialt=0,nalt-1 do begin
    color=get_color(ialt,nalt)
    alt_req=alt_reqs[ialt]
    x = (size(xs))[0] eq 1 ? xs : xs[*,ialt]
    y = (size(ys))[0] eq 1 ? ys : ys[*,ialt]
    oplot, x, y, color=color, psym=1, symsize=0.75
    ;    result = poly_fit(x,y,2,covar=covar,chisq=chisq)
    ;  print, alt_req, chisq, covar[0,0]/n_elements(x),covar[1,0]/n_elements(x)
    ;    neatx = min(x,/nan) + findgen(21)/20.*(max(x,/nan)-min(x,/nan))
    ;    oplot, neatx, result[0]+result[1]*neatx+result[2]*neatx^2., color=color
    plots, mean(x,/nan), !Y.CRANGE[0], psym=8, color=color, noclip=0
    luminance = ct_luminance(/read_tables)
    label_color = luminance[color] lt 125 ? 255 : 0
    xyouts, mean(x,/nan), !Y.CRANGE[0], string(alt_req,format='(I0,"m")'), color=label_color, charsize=!P.CHARSIZE*.8, noclip=0, align=0.5
  endfor
  xyouts, !X.WINDOW[1], 0, systime(), align=1, charsize=!P.CHARSIZE*0.5, /norm
  
end


pro do_time_blocks, x, time_series_Times, minute_interval, newx, block_time_starts, mode=mode
  if ~keyword_set(mode) then mode = 0
  
  ; input
  ; x - array to process
  ; time_series_Times - double precision array of julian days
  ; minute_interval
  ; output
  ; newx - processed array
  ; block_time_starts - start time of each time block
  
  ; get julian day of first time
  caldat, min(time_series_Times,/nan), month, day, year, hour, minute, second
  ; find closest multiple of 15 minutes greater than first time in the time series.
  newminute = ceil((minute+second/60.)/minute_interval)*minute_interval
  start = julday(month, day, year, hour, newminute, 0)
  block_time_starts = TIMEGEN(start=start, final=max(time_series_Times,/nan), step_size=minute_interval, units='Minutes')
  if n_elements(block_time_starts) le 1 then message, 'no whole time blocks fit in time series'
  nblocks = n_elements(block_time_starts) - 1
  newx = replicate(!VALUES.D_NAN, nblocks)
  for i = 0, nblocks-1 do begin
    i_minute_interval = where(time_series_Times ge block_time_starts[i] $
      and time_series_Times lt block_time_starts[i+1] $
      and finite(x), n_minute_interval ) ; new addition 20120223
    if n_minute_interval gt 0 then begin
      xs = x[i_minute_interval]
      ; it does not make sense to ignore data below min=-80. so set data below -80 to -80
      ibelow = where(xs lt -80, nbelow)
      if nbelow gt 0 then xs[ibelow] = -80
      if mode gt 0 then begin
        binsize = mode
        h = histogram(xs, binsize=binsize, min=-80., locations = bin_starting_locations)
        most = max(h, imode)
        ; find the bin with most elements in it
        ; set newx[i] to the starting location of the bin
        newx[i] = bin_starting_locations[imode]
      endif else newx[i] = mean(xs)
    endif
  endfor
  ; cut off last block_time start because it is not a whole time block.
  block_time_starts = block_time_starts[0:n_elements(block_time_starts)-2]
  
end

pro run_microwave_temperature_profile, start=start
  if ~keyword_set(start) then start = 0
  ;  first mission on 8/15 is not into a system. it is a stalled frontal boundary/
  t = get_PREDICT_missions(count=nmissions)
  ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
  for imission=start,nmissions-2 do microwave_temp_profile_old, t.pgi[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', usedrops=1
end


;
; In order to use the 1-m resolution average profile for all missions, I had to run this once
; with some modifications (like 1-m spacing from 0-22km & not subtracting the all-mission average profile).
; And then I ran plot_MTPmean in
; microwave_temp_profile_old.pro, which averaged the mission-average profiles.
; Then I could run this normally.  Normally, this subtracts the all-mission average profile from the raw
; observations interpolates to 500m spacing, and saves the mission-average deviation profile.
;

pro microwave_temp_profile_old, pgi, date, time, time_range=time_range, units=units, usedrops=usedrops
  force_new = 0
  debug = 0
  if ~keyword_set(units) then units='m' ; don't try 'mb' - the savfile names do not discriminate between m and mb.
  if ~keyword_set(usedrops) then usedrops = 0 ; or MTP usedrops = 0
  whichAlt = 'press_alt'; 'geomAlt'; 'press_alt' ; Pressure altitude is an aviation thing. It is not geometric (true) altitude.
  ; Geometric altitude is the true, tape-measured distance to the surface of the earth. See http://mtp.mjmahoney.net/www/notes/altitude/altitude.html
  if whichAlt eq 'press_alt' then units = 'press_alt_m'
  basedir = '/Volumes/pecan2/ahijevyc/'
  path = basedir+'PREDICT/MTP/'+(usedrops eq 1 ? 'usedrops/' : '')
  
  
  if n_elements(pgi) eq 0 then pgi = 'PGI44L'
  if n_elements(date) eq 0 then date='20100914'
  if n_elements(time) eq 0 then time='180000'
  if strmid(date,0,4) eq '2013' then begin
    path = basedir+'mpex/MTP/'
    pgi=''
  endif
  mtm00 = 1
  best_track = 0
  mean_gfs_search_radius=500.
  earth_radius_km = 6371.
  
  
  if usedrops eq 0 then alt_reqs = get_levels('CFAD_MTP')
  if usedrops eq 1 then alt_reqs = reverse(get_levels('m'))
  ; a page is done for each temp range.
  ; tempranges doesn't need to have the same # of elements as alt_reqs
  tempranges = [[-81, 10], [-22,-14], [-38,-27], [-56,-43], [-68, -59],[-75,-68], [-83,-75]]
  ;  alt_reqs = alt_reqs[4]
  ;  tempranges = tempranges[*,3]
   mean_profile_file = path + "mission_avg_profile_"+pgi+"_"+date+"_"+time+"_"+whichAlt+".txt"
  if file_test(mean_profile_file) eq 0 then begin
    print, 'there is no mean profile file every 1-m . you might want to create it so you can create .csv files'
    stop
    alt_reqs = findgen(22001) ;  uncomment to create components necessary for 1-m all-mission average profile.
  endif
  nalt = n_elements(alt_reqs)
  ncycles = 1700 ; longest single flight has 1678 cycles.
  time_series_IR_blocks = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_block_starts     = replicate(!VALUES.D_NAN, ncycles, nalt)
  
  if ~keyword_set(time_range) then time_range = [0,23.9999999]
  close, /all
  
  ; Subtract the all-mission mean profile in raw MTP coordinates. This may be added back
  ; after interpolating to the requested altitudes with interpol_NAN. (line 330)
  ; after Jul 2013 I stopped subtracting the all-mission profile to simplify things.
  subtract_all_mission_average_before_interpolating = 0
  
  if !D.NAME eq 'PS' then begin
    device, /color, bits=8
    !P.CHARSIZE=1.0
    !P.CHARTHICK=2.0
  endif
  loadct, 39, silent=debug?0:1 ; load color table - use silent settings if debug=0
  !P.MULTI=0
  if !D.NAME eq 'X' then begin
    device, decomposed=0
    !P.CHARSIZE=2.0
    !P.CHARTHICK=1.0
  endif
  
  ; Retrieve CVCWC time series
  savfile = basedir+'PREDICT/GV/cfad_MTP.sav'
  restore, savfile ; multielement lat and lon arrays restored too. This messes up the single element lat and lon below.
  MTP_lat = lat &  MTP_lon = lon &  lat = !VALUES.F_NAN & lon = lat
  
  badMRIcolor=2
  badAltcolor=!D.NAME eq 'X' ? 9 : 255
  
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  mission_julday = julday(month, day, year, hour, minute, second)
  
  
  ; get points on a lat/lon grid within a radius of the sounding point.
  ; These will be plugged into the IR function to get IR temperatures at each point.
  neighbor_radius_m = 10000. ; radius of circle in meters around lat/lon
  
  time_series_Ts        = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_cold_pt       = replicate(!VALUES.F_NAN, ncycles)
  dist2center           = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_MTPTimes  = replicate(!VALUES.D_NAN, ncycles)
  time_series_dropTimes = replicate(!VALUES.D_NAN, ncycles, nalt)
  time_series_UTSEC     = replicate(!VALUES.D_NAN, ncycles)
  time_series_MRI       = replicate(!VALUES.F_NAN, ncycles)
  time_series_SETemp    = replicate(0., ncycles, nalt)  ; Used to be NaN 20130209 - but want zero for usedrops=1
  time_series_IR        = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_Press_alt = replicate(!VALUES.F_NAN, ncycles)
  time_series_SatIRtime = replicate(!VALUES.D_NAN, ncycles, nalt)
  time_series_GV_temp   = replicate(!VALUES.F_NAN, ncycles)
  time_series_dropsonde = replicate(!VALUES.F_NAN, ncycles, nalt)
  ; Why have separate dropsonde and Ts ? so you can plot them both if usedrops=0.
  time_series_mean_gfs  = replicate({u:!VALUES.F_NAN,v : !VALUES.F_NAN,t : !VALUES.F_NAN,$
    td: !VALUES.F_NAN,tv: !VALUES.F_NAN,rh: !VALUES.F_NAN,z : !VALUES.F_NAN, mse:!VALUES.F_NAN}, ncycles, nalt)
  time_series_U         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_V         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_x_km      = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_y_km      = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_lon       = replicate(!VALUES.F_NAN, ncycles, nalt)
  last_good_cycle = replicate(-1L, nalt)
  old_gfs_hour = replicate(!VALUES.F_NAN, nalt)
  oldlat = replicate(!VALUES.F_NAN, nalt)
  oldlon = replicate(!VALUES.F_NAN, nalt)
  
  mtpdate = date
  ; There were two MTP flights Sep. 10. If 1200, then 'a', otherwise 'b'.
  if date eq '20100910' then mtpdate = time eq '120000' ? date+'a' : date+'b'
  ; There were 3 flights Sep 13, but only one GV (with MTP) mission (120000).
  if date eq '20100913' && time ne '120000' then return
  if is_stage(path, date, hour, 'all') eq 0 then return
  infile = path + 'MP' + mtpdate + '.NGV'
  if file_test(infile) ne 1 then begin
    print, 'could not find '+infile
    return
  endif
  savfile = path + 'savfiles/' + pgi + date + '_' + time + '_' + whichAlt + '.sav'
  if file_test(savfile) and force_new ne 1 then begin
    ; sanity check to see if the vertical levels in the save file are the same as what you are requesting now
    requesting_now = alt_reqs
    restore, savfile
    if array_equal(requesting_now, alt_reqs) ne 1 then begin
      print, 'the save file has different vertical levels than the ones you are requesting now'
      print, 'save file:', alt_reqs, ' requesting now: ', requesting_now
      stop
    endif
  endif else begin
  
    ; === read header lines of *.NGV file ===
    openr, lun, infile, /get_lun
    print, 'opened '+infile
    readf, lun, header_lines, nsomething
    junk = ''
    repeat readf, lun, junk until junk eq 'PREDICT' or junk eq 'mpex'
    readf, lun, testa, testb
    if testa ne 1 then stop
    if testb ne 1 then stop
    readf, lun, FLT_year, FLT_month, FLT_day, REDUCTION_year, REDUCTION_month, REDUCTION_day, flight_number
    readf, lun, remote_sensing_altitude, UT_seconds_from_0
    if remote_sensing_altitude ne 0 then stop
    if UT_seconds_from_0 ne 0 then stop
    readf, lun, junk
    readf, lun, junk
    readf, lun, NV
    if NV ne 4 then stop
    readf, lun, scale1, scale2, scale3, scale4
    if scale1 ne 1.0 then stop
    if scale2 ne 1.0 then stop
    if scale3 ne 1.0 then stop
    if scale4 ne 1e+21 then stop
    readf, lun, miss1, miss2, miss3, miss4
    readf, lun, junk
    readf, lun, junk
    readf, lun, junk
    readf, lun, junk
    readf, lun, n_aux_var
    if n_aux_var ne 15 then stop
    iline = 18
    repeat begin
      readf, lun, junk
      iline = iline+1
    endrep until iline eq header_lines
    
    
    ; === read 1 MTP data cycle at a time ===
    icycle = -1L
    while ~ EOF(lun) do begin
      icycle = icycle+1L
      
      if icycle mod 15 eq 0 then print, '.', format='(A,$)'
      ; === read header of MTP data cycle ===
      readf, lun, UTSEC, nlevs, GV_Press_alt_km, Pitch_deg, Roll_deg, Horizon_brightness_temp, trop_alt, trop_alt2, $
        Trop1theta, Trop2theta, lat, lon, lapse_rate, MRI, Tcp_K, Zcp_km
      julday = julday(FLT_month, FLT_day, FLT_year, 0, 0, 0)+UTSEC/3600d/24d
      oldAlt = 9999999.
      ; Reset these incase nlevs = 0 (empty cycles with no T profiles happen a lot in MTP data).
      ; before 20130423 I didn't reset and the previously good cycle data were used.
      ; this resulted in lines of horizontal values on scatterplots. Lots of repeats.
      Alts = !VALUES.F_NAN
      Ts   = !VALUES.F_NAN
      SEtemps = !VALUES.F_NAN
      ; === Read individual lines of the MTP data cycle. Some cycles have just a header and zero data lines. ===
      if nlevs gt 0 then begin
        iline = 0
        Alts = fltarr(nlevs)
        Ts   = replicate(!VALUES.F_NAN,nlevs); initialize with NaNs not zero - sometimes there's no matching all-mission profile height so T should be missing
        SEtemps = fltarr(nlevs)
        time_series_MTPTimes[icycle] = julday
        time_series_dropTimes[icycle,*] = julday
        time_series_UTSEC[icycle] = UTSEC
        caldat,julday, month, day, year, hour, minute, second
        
        
        if hour gt 10 and minute gt 10 and debug gt 1 then print, string(utsec,hour, minute, second, format='("UTSEC=",I0," ",I2.2,":",I2.2,":",I2.2)')
        ; read cycle data
        repeat begin
          readf, lun, Press_alt_m, T, SEtemp, Alt, num_density
          Ts[iline] = T-!CONST.T0
          which_alt = whichAlt eq 'geomAlt' ? Alt : Press_alt_m;Press_alt_m ; Alt
          Alts[iline] = which_alt; Alt
          iavgts = where(avgHgt eq round(which_alt), nmatch)
          ; there are some negative heights in the MTP data.
          if nmatch eq 1 then begin
            ; Subtract the all-mission mean profile in raw MTP coordinates. This may be added back
            ; after interpolating to the requested altitudes with interpol_NAN. (line 330)
            ; after Jul 2013 I stopped subtracting the all-mission profile to simplify things.
            if subtract_all_mission_average_before_interpolating then Ts[iline] = Ts[iline] - avgTs[iavgts]
          endif
          SEtemps[iline] = SEtemp
          iline = iline+1
          if oldAlt le which_alt then stop ; sanity check to make sure Altitude is descending.
          oldAlt = which_alt
        endrep until iline eq nlevs
        datestring = string(year,month,day,format='(I4.4,I2.2,I2.2)')
        timestring = string(hour,minute,second,format='(I2.2,I2.2,I2.2)')
        
        mtm_center, pgi, datestring, timestring, clat, clon, best_track=best_track, mtm00=1, silent=debug?0:1, u=usys,v=vsys
        if usedrops eq 0 && finite(clon) && finite(clat) then begin
          result = map_2points(clon,clat,lon,lat)
          dist2center[icycle,*] = result[0] * !DTOR * earth_radius_km
          az = result[1]
          time_series_x_km[icycle,*] = sin(az*!DTOR) * dist2center[icycle,0]
          time_series_y_km[icycle,*] = cos(az*!DTOR) * dist2center[icycle,0]
          
          ; if you do this for usedrops=1, do it for usedrops=0 too. not sure where to place it
          ; so you don't have to have it 3 places.
          if there_is_a_closer_PREDICT_mission(julday, mission_julday) then begin
            if debug then print, pgi, datestring, timestring, infile, " was double counted in the past"
            continue
          endif
          ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
          ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
          if ( pgi eq 'PGI48L' or pgi eq 'PGI51L' ) and PGI48LPGI51L(pgi,julday) ne pgi then continue
          
          
          
        endif
        
        if usedrops eq 1 then begin
          ; when calling get_closest_sonde_in_time, use /GV_only.  Or you could get drops from other systems-probably dropped from G4 and DC8.
          junk = get_closest_sonde_in_time(datestring, timestring, alt_reqs, /GV_only, /just_filename, /nocape)
          ; if closest dropsonde has not changed since the last cycle, just use the old dropsonde data instead of re-looking it up.
          if n_elements(old_dropsonde_data) gt 0 && old_dropsonde_data.filename eq junk.filename then begin
            dropsonde_data = old_dropsonde_data
            ; besides using old dropsonde variables (temperature, u, v)
            ; use old distance and position if the nearest dropsonde is the same as before.
            dist2center[icycle,*] = dist2center[icycle-1,*]
            time_series_x_km[icycle,*] = time_series_x_km[icycle-1,*]
            time_series_y_km[icycle,*] = time_series_y_km[icycle-1,*]
            new_sonde = 0
          endif else begin
            dropsonde_data = get_closest_sonde_in_time(datestring, timestring, alt_reqs, /GV_only, units=units, /nocape)
            old_dropsonde_data = dropsonde_data
            new_sonde = 1
          endelse
          ; if you do this for usedrops=1, do it for usedrops=0 too. not sure where to place it
          ; so you don't have to have it 3 places.
          if there_is_a_closer_PREDICT_mission(julday, mission_julday) then begin
            if debug then print, pgi, datestring, timestring, infile, " was double counted in the past"
            continue
          endif
          ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
          ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
          if ( pgi eq 'PGI48L' or pgi eq 'PGI51L' ) and PGI48LPGI51L(pgi,julday) ne pgi then continue
          
          
          time_series_dropTimes[icycle,*] = dropsonde_data.Julian
          time_series_U[icycle,*] = dropsonde_data.U
          time_series_V[icycle,*] = dropsonde_data.V
          time_series_dropsonde[icycle,*] = dropsonde_data.t
          time_series_Ts[icycle,*]= dropsonde_data.t
        endif
        
        for ialt = 0, nalt-1 do begin
          alt_req = alt_reqs(ialt)
          
          if usedrops eq 1 && new_sonde then begin
            ; lat and lon used to come from MTP header, now use the lat and lon in dropsonde_data.
            ; that's what cold_pool_analysis.pro does.
            ; Note these may be NaN because the MTP may be turned on, but the closest dropsonde
            ; is still more than 20 minutes away.
            ; And individual levels may have NaN for lat, lon, Julian, etc. because the closest
            ; dropsonde level is more than 200 meters away (or 10 mb in the case of pressure coordinates).
            lat = dropsonde_data.lat[ialt]
            lon = dropsonde_data.lon[ialt]
            if ~finite(lat) then continue ; If we don't have a lat/lon for the dropsonde then don't bother with
            ; distance, IR, GFS_mean or anything else that comes later in this
            ; ialt height for-loop.
            ; get the  time at the moment it crosses the requested level.
            ; This time will be used to determine the IR temps.
            caldat, dropsonde_data.Julian[ialt], month, day, year, Hour, Minute, Second
            datestring = string(year,month,day,format='(I4.4,I2.2,I2.2)')
            timestring = string(Hour,Minute,Second, format='(I2.2,I2.2,I2.2)')
            if timestring eq '2359**' then stop
            ; if you do this for usedrops=1, do it for usedrops=0 too. not sure where to place it
            ; so you don't have to have it 2 places.
            if there_is_a_closer_PREDICT_mission(dropsonde_data.Julian[ialt], get_julday) then begin
              if debug then print, pgi, datestring, timestring, infile, " was double counted in the past"
              continue
            endif
            ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
            ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
            if ( pgi eq 'PGI48L' or pgi eq 'PGI51L' ) and PGI48LPGI51L(pgi,dropsonde_data.Julian[ialt]) ne pgi then continue
            
            mtm_center, pgi, datestring, timestring, clat, clon, best_track=best_track, mtm00=1, silent=debug?0:1
            if finite(clon) and finite(lon) then begin
              result = map_2points(clon,clat,lon,lat)
              dist2center[icycle,ialt] = result[0] * !DTOR * earth_radius_km
              az = result[1]
              time_series_x_km[icycle,ialt] = sin(az*!DTOR) * dist2center[icycle,ialt]
              time_series_y_km[icycle,ialt] = cos(az*!DTOR) * dist2center[icycle,ialt]
            endif
          endif else if usedrops eq 0 then begin
            ; Interpolate MTP temperature to requested level. Only needed if usedrops=0.
            ihigher = max(where(Alts gt alt_req, nhigher))
            if nhigher eq 0 or ihigher ge nlevs -1 then begin
              if debug gt 1 then print, string(icycle,utsec,hour,minute,ihigher, nhigher,alt_req, format=$
                '("icycle=",I0," UTSEC=",I0," ",I2.2,":",I2.2," ihigher=",I0," nhigher=",I0,"  alt_req=",I0)') + " Alts=", Alts
            ;            continue ; used to continue with next cycle here because there is no MTP data, but need other stuff.
            endif else begin
              dTdZ = (Ts[ihigher+1]-Ts[ihigher])/(Alts[ihigher+1]-Alts[ihigher])
              oldinterpT = Ts[ihigher+1] + dTdZ * (alt_req-Alts[ihigher+1])
              interpT = interpol_NAN(Ts,Alts,alt_req,max_gap=2000) ; used to be 500m - but normal MTP gaps are big
              if abs(oldinterpT - interpT) gt 1e-5 then stop
              time_series_Ts[icycle, ialt] = interpT
              time_series_SEtemp[icycle, ialt] = 0.5*(SEtemps[ihigher+1] + SEtemps[ihigher])
              if debug gt 1 then print, string(icycle,ialt,ihigher, nhigher, dTdZ, alt_req, format=$
                '("icycle=",I0," ialt=",I0," ihigher=",I0," nhigher=",I0," dTdZ=",F0," alt_req=",I0)')
              if debug gt 1 then print, string(Ts[ihigher],Ts[ihigher+1],Alts[ihigher],Alts[ihigher+1],interpT,format=$
                '("Ts = ",F7.3,"=",F7.3," Alts=",F9.3,"-",F8.3," T=",F7.3)')
            endelse
          endif
          
          if nalt lt 100 then begin ; >100 levels - IR and GFS mean not practical to calculate
          
            ; ==== Get IR cloud top temperature ====
            ; get points on a lat/lon grid within a radius of the sounding point.
            ; These will be plugged into the IR function to get IR temperatures at each point.
            ; neighbor_radius_m = 10000. ; radius of circle in meters around lat/lon
            gspacing = 0.04 ; lat/lon grid spacing within the circle (in degrees) should be similar to satellite pixel spacing
            ; only run get_rlat_rlon and get_ir if lat or lon has changed. They won't have changed
            ; if usedrops = 0.  In this case, the MTP is being used, and the lat/lon/time is the same for all
            ; altitudes.
            
            ; Can we move this inside the loop above? don't think so.
            if ~finite(lat) || ~finite(oldlat[ialt]) || ~finite(lon) || ~finite(oldlon[ialt]) $
              || lat ne oldlat[ialt] || lon ne oldlon[ialt] then begin
              if ialt eq 0 || usedrops eq 1 then begin
                ; if MTP you don't need to repeat get_ir to get a new IR_temp for each height.
                ; Enter this loop if usedrops=0 and it's the first iteration of the height loop (ialt=0),
                ;  OR if usedrops=1.
                get_rlat_rlon, neighbor_radius_m, gspacing, lat, lon, rlat, rlon
                ; input rlat and rlon arrays to get_ir and get IR_temp array.
                get_ir, IR_temp, datestring, timestring, rlat, rlon, closest_imageJulian
                oldlat[ialt] = lat
                oldlon[ialt] = lon
                if n_elements(old_IR_temp) eq 0 then old_IR_temp = fltarr(n_elements(rlat),nalt)
                ; sometimes IR_temp and rlat can change size. (slightly different # of gridpoints in a circle)
                ; make sure that IR_temp will fit in old container
                
                if total(~finite(IR_temp)) gt 0 then stop
                if n_elements(IR_temp) ne n_elements(old_IR_temp[*,ialt]) then stop
                if n_elements(IR_temp) eq n_elements(old_IR_temp[*,ialt]) then old_IR_temp[*,ialt] = IR_temp
              endif
            endif else IR_temp = old_IR_temp[*,ialt]
            time_series_IR[icycle,ialt] = mean(IR_temp) ; forgot to take mean for a while - 20120227
            time_series_SatIRtime[icycle,ialt] = closest_imageJulian
            
            ; ==== Get GFS mean for reference ===
            gfs_julday = round(time_series_dropTimes[icycle,ialt]*4d)/4d ; get nearest multiple of 6 hours
            caldat, gfs_julday, gfs_month, gfs_day, gfs_year, gfs_hour
            if finite(old_gfs_hour[ialt]) eq 0 || old_gfs_hour[ialt] ne gfs_hour then begin
              gfs_date = string(gfs_year,gfs_month,gfs_day,format='(I4.4,I2.2,I2.2)')
              time_series_mean_gfs[icycle,ialt] = get_mean_gfs( pgi, gfs_date, gfs_hour, alt_req, best_track, mtm00, mean_gfs_search_radius, units=units)
              old_gfs_hour[ialt] = gfs_hour
              last_good_cycle[ialt] = icycle
            endif else time_series_mean_gfs[icycle,ialt] = time_series_mean_gfs[last_good_cycle[ialt],ialt]
            ; there is the possibility of NaNs if the requested alt is not in the GFS (like 0m).
            if finite(time_series_mean_gfs[icycle,ialt].T) eq 0 && alt_req ne 0 then stop
          endif
          
          
        endfor ; ialt loop
        time_series_Press_alt[icycle] = GV_Press_alt_km
        time_series_cold_pt[icycle] = Tcp_K - !CONST.T0
        time_series_MRI[icycle] = MRI
        time_series_GV_temp[icycle] = Horizon_brightness_temp
        
      endif ; If there were any measured levels in this cycle
    endwhile
    free_lun, lun
    
    
    save, alt_reqs, time_series_dropsonde, time_series_SatIRtime, time_series_IR, dist2center, time_series_GV_temp, $
      time_series_MRI, time_series_cold_pt, time_series_Press_alt, time_series_SEtemp, time_series_Ts, time_series_UTSEC, $
      time_series_dropTimes, time_series_mean_gfs, time_series_MTPTimes, time_series_x_km, time_series_y_km, $
      time_series_U, time_series_V, filename=savfile
      
  endelse
  
  
  ; now that I request a specific mission time hhmmss, I don't want to output stuff if this is not the requested mission.
  ; if the requested time is before the first MTP
  ; I added the minus 1 because on 0901, the flight took off at 1200 or a little after and the requested
  ; time is still 1200--slightly before the flight window.
  if time/10000L lt min(time_series_UTSEC/3600.)-1 or time/10000L gt max(time_series_UTSEC/3600.) then return
  
  
  if !D.NAME eq 'PS' then device, /close, filename=path+pgi+"_"+date+'_'+time+'_'+whichAlt+'.ps', /color, ysize=5, yoffset=4, /inches
  
  
  
  date_label = label_date(date_format=['%H:%I', '%D %M, %Y'])
  caldat, min(time_series_MTPTimes,/nan), month, day, year, hour, minute, second
  first_time = string(hour, minute, format='(I2.2,":",I2.2)')
  xrange = [min(time_series_MTPTimes,/nan),max(time_series_MTPTimes,/nan)]
  ; If time_range was not set, then the 1st element is likely 0., the default. If it is not zero, then define xrange to zoom in on the time axis.
  if time_range[0] ne 0 then xrange = julday(month,day,year,time_range,0, 0) ; zoom in time range
  caldat, max(time_series_MTPTimes,/nan), month, day, year, hour, minute, second
  last_time = string(hour, minute, format='(I2.2,":",I2.2)')
  title = pgi + " " + strmid(date,4,2) + "/" + strmid(date,6,2) + " " + first_time + "-" + last_time
  
  
  
  good_MTP    = time_series_Press_Alt ge 12
  iGood_MTP   = where(good_MTP, ngood_MTP)
  
  
  if usedrops eq 1 then begin
    MRI_cutoff = !VALUES.F_NAN
  endif else if usedrops eq 0 then begin
    ; Histogram of quality measure for MTP (MRI) and find cutoff point. Redefine iGood_MTP based on MRI cutoff.
    h2 = histogram(time_series_MRI[igood_MTP], binsize=0.01, min=0, max=2.01, locations=xvals, /nan)
    plot, xvals, h2, xrange=[0,2.01], title=title, $
      xtitle='MRI', ytitle='count', psym=10, thick=4, charthick=3,charsize=2,xstyle=1
    MRI_cutoff = get_histogram_cutoff(xvals, h2)
    ;  print, title, MRI_cutoff
    good_MTP    = good_MTP and time_series_MRI lt MRI_cutoff
  endif
  iGood_MTP   = where(good_MTP, ngood_MTP)
  
  
  
  ; histograms of SEtemp at different levels.
  ;  for ialt=0,nalt-1 do begin
  ;    plot, xvals, histogram(time_series_SEtemp[[igood_MTP],ialt], binsize=0.1, min=0, max=2.1, locations=xvals, /nan), xrange=[0,2.1], title=title, $
  ;      xtitle='SEtemp'+string(alt_reqs[ialt]), ytitle='count', psym=10, thick=4, charthick=3,charsize=2,xstyle=1
  ;  endfor
  
  ;  ibad_MRI = where(time_series_MRI ge MRI_cutoff, nbad_MRI)
  ;  print, nbad_MRI, 100.*float(nbad_MRI)/total(finite(time_series_MRI)), title, format='(I0, " (",F6.2,"%) bad MRI  ",A)'
  
  
  ; Calculate mission-average MTP at each requested height.
  ; Interpolate to 0-22km every 1 m.
  ; Used to create IDV *.csv files that contain deviation from mission average.
  ; Execute run_MTP2csv.csh to create the *.csv files.
  ; Then a temperature curtain is plotted in IDV.
  mean_profile_file = path + "mission_avg_profile_"+pgi+"_"+date+"_"+time+"_"+whichAlt+".txt"
  ; A rebin command will result in NaNs if any of the mission is missing that level.
  ; This doesn't affect the GFS reference.
  ; This is the case at 20km and 6km because they're on the edge of the MTP range
  ; and the flight level is too far from 20km in the beginning and too far from 6 km in the end.
  mission_avg_Ts = replicate(!VALUES.F_NAN, nalt) ; rebin(time_series_Ts[iGood_MTP,*],1,nalt)
  ; because I added a thing above to set time_series_Ts equal to dropsonde_data.T
  total_Ts = total(time_series_Ts[iGood_MTP,*],1, /nan)
  for ialt = 0, nalt-1 do begin
    finite_Ts = total(finite(time_series_Ts[iGood_MTP,ialt]))
    mission_avg_Ts[ialt] = total_Ts[ialt]/finite_Ts
  endfor
  
  openw, meanm, mean_profile_file, /get_lun
  printf, meanm, 'MRI cutoff = ', MRI_cutoff
  for alt_req = 0, 22000, 1 do begin
    iequal = where(alt_reqs eq alt_req, nequal) ; in the odd chance an available level is exactly what was requested
    if nequal eq 1 then mt = mission_avg_Ts[iequal] else begin
      ihigher = max(where(alt_reqs gt alt_req, nhigher))
      if nhigher eq 0 or ihigher ge nalt-1 then mt = !VALUES.F_NAN else begin
        dTdZ = (mission_avg_Ts[ihigher+1]-mission_avg_Ts[ihigher])/(alt_reqs[ihigher+1]-alt_reqs[ihigher])
        interpT = mission_avg_Ts[ihigher+1] + dTdZ * (alt_req-alt_reqs[ihigher+1])
        mt = interpT
      endelse
    endelse
    printf, meanm, alt_req, mt
  endfor
  free_lun, meanm
  
  
  
  
  pos = [0.15,0.15,0.86,0.84]
  posa= [pos[0],pos[1],pos[2],0.35]
  posb= [posa[0],posa[3]+0.01,posa[2],pos[3]]
  if !D.NAME eq 'X' then wset, 0
  ; Plot symbol is a rectangle. Its width is inversely proportional to the time range
  A = FINDGEN(5) * (!PI*2/4.) + !PI/4.
  USERSYM, cos(a)/(xrange[1]-xrange[0])*0.08, 120*sin(a)/nalt, /fill
  
  ; Here's where the IDL version of a "temperature curtain" is drawn.
  ; as opposed to the 3D, coiled version in IDV.
  refs_to_subtract = ['GFS','mission avg']
  for irefs_to_subtract = 0, n_elements(refs_to_subtract)-1 do begin
    ref_to_subtract = refs_to_subtract[irefs_to_subtract]
    if ref_to_subtract eq 'GFS' then ref2sub = time_series_mean_GFS.T
    if ref_to_subtract eq 'mission avg' then ref2sub = rebin(transpose(mission_avg_Ts),ncycles,nalt)
    for ibound_dTemp_range = 3,3,1 do begin
      dTemp_range = [-ibound_dTemp_range,ibound_dTemp_range]
      ; plot MTP minus reference temperature in top panel.
      plot, time_series_MTPTimes, replicate(0,ncycles), yrange=[2000,22000], xtickformat='(A1)', $
        position=posb, /nodata, ystyle=9, xstyle=0, xminor=4, ytickformat='(I0)', $
        ytitle='height from '+whichAlt +' [m]', title=title+'!C', xrange=xrange
      colorbar_position=[posb[2]+0.1*(1.-posb[2]),posb[1]+0.1*(posb[3]-pos[1]),posb[2]+0.2*(1.-posb[2]),posb[1]+0.9*(posb[3]-posb[1])]
      block_out_single_channel_retrieval_times, date, xrange, posb
      if ngood_MTP gt 0 then begin
        dTemp = time_series_Ts[iGood_MTP,*]-ref2sub[iGood_MTP,*]
        ; do not plot NaN's . in .ps they are black.
        igood_dTemp = where(finite(dTemp),ngood_dTemp)
        if ngood_dTemp gt 0 then begin
          colors = bytscl(dTemp[igood_dTemp],min=dTemp_range[0],max=dTemp_range[1])
          USERSYM, cos(a)/(xrange[1]-xrange[0])*0.025, 1.8*sin(a), /fill
          plots, (rebin(time_series_MTPTimes[iGood_MTP],nGood_MTP,nalt))[igood_dTemp], (rebin(transpose(alt_reqs),nGood_MTP,nalt))[igood_dTemp], color=colors,$
            symsize=!D.NAME eq 'X' ? 1 : 0.4,psym=8, noclip=0
          if usedrops eq 1 then begin
            USERSYM, cos(a)/(xrange[1]-xrange[0])*1.4, 1.8*sin(a), /fill
            plots, (time_series_dropTimes[iGood_MTP,*])[igood_dTemp], (rebin(transpose(alt_reqs),nGood_MTP,nalt))[igood_dTemp], color=colors,$
              symsize=!D.NAME eq 'X' ? 1 : 0.5,psym=8
            USERSYM, cos(a)/(xrange[1]-xrange[0])*0.025, 1.8*sin(a), /fill
          endif
        endif
        mycolorbar, position=colorbar_position,title='MTP - '+ref_to_subtract, range=dTemp_range, /vertical,format='(F+4.1,"!Z(00B0)C")',/right, charsize=!P.CHARSIZE*0.7
      endif
      ; Plot bottom panel
      plot_bottom_panel, posa, xrange,nalt,time_series_dropTimes,dist2center,time_series_IR,time_series_MTPTimes,time_series_GV_temp,$
        time_series_IR_blocks,time_block_starts,time_block_minutes, time_series_SatIRtime
      oplot, GV_juldays, temp, thick=1, color=35
      
      ; Plot MRI time series
      if (1) then begin
        plot, time_series_MTPTimes[iGood_MTP], time_series_MRI[iGood_MTP], ystyle=5, xrange=xrange, position=posb, xstyle=4,$
          yrange=[0,1.], thick=1.5, /noerase
        oplot, !X.CRANGE, replicate(MRI_cutoff,2), thick=3, linestyle=2
        axis, !X.CRANGE[1], yaxis=0, ticklen=-0.02, yrange=!Y.CRANGE, ystyle=1, ytitle='MRI',yminor=1
        ; Plot CVCWC time series
        cvcwccolor=231
        plot, GV_juldays, cvcwc, ystyle=5, /ylog, xrange=xrange, position=posb, xstyle=4,$
          yrange=[0.0001,1e14], thick=1.15, color=cvcwccolor, noclip=0, /noerase
        oplot, !X.CRANGE, replicate(0.07,2), thick=3, color=cvcwccolor, linestyle=2 ; changed from 0.0722 to 0.07 on Dec 10, 2013
        
      endif
    endfor
  endfor
  
  if (0) then begin ; this was just a hack because I ran out of memory with nalt>10000. downstream program like cfad_mtp used save files instead.
    for ialt = 0, n_elements(alt_reqs)-1 do begin
      alt_req = alt_reqs[ialt]
      ; =========
      ; histogram code moved to old_microwave_temp_profile_scatterplot_histograms.pro
      ; =========
      scatter_file = path + date + '_' + string(time,whichAlt,alt_req,units,round(neighbor_radius_m/1000),format='(I6.6,"_",A,"_",I5.5,A,"_IR",I2.2,"km")')+'_scatter.txt'
      ; PGI51L and PGI48L share the same mission so don't overwrite the PGI48L contribution with the PGI51L contribution.
      if pgi eq 'PGI51L' then openu, lun, scatter_file, /get_lun, /append else openw, lun, scatter_file, /get_lun
      caldat, time_series_dropTimes, time_series_month, time_series_day, time_series_year, time_series_hour, time_series_minute, time_series_second
      for ipoint = 0, ncycles-1 do begin
        ; For a while I forgot to filter out bad MTP points where(good_MTP eq 0).  - 20120306
        T_point = usedrops eq 1 ? time_series_dropsonde[ipoint, ialt] : good_MTP[ipoint] eq 1 ? time_series_Ts[ipoint, ialt] : !VALUES.F_NAN
        ; MTP only has temperature - no moisture or pressure.  So no Td or Tv & Z instead of P.
        if finite(T_point) and finite(time_series_IR[ipoint,ialt]) and $
          finite(time_series_dropTimes[ipoint,ialt]) then $
          printf, lun, pgi, T_point, !VALUES.F_NAN, $
          !VALUES.F_NAN, !VALUES.F_NAN, $ ; Tv and MSE not defined
          dist2center[ipoint,ialt], time_series_IR[ipoint,ialt], $
          time_series_x_km[ipoint,ialt], $
          time_series_y_km[ipoint,ialt], $
          !VALUES.F_NAN, alt_req, $ ; p not defined ( but Z is. )
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, $ ; skip CAPE(2), parcel dT(2) (rev and ps)
          !VALUES.F_NAN, !VALUES.F_NAN, $ ; liquid and ice water content
          time_series_U[ipoint,ialt],$ ; There's no u and v in MTP,
          time_series_V[ipoint,ialt],$ ; but there is if usedrops=1.
          !VALUES.F_NAN, !VALUES.F_NAN, $ ;  don't bother with usys and vsys either.
          time_series_mean_gfs[ipoint,ialt].T, time_series_mean_gfs[ipoint,ialt].Td, $
          time_series_mean_gfs[ipoint,ialt].Tv, $
          time_series_mean_gfs[ipoint,ialt].MSE, $
          time_series_mean_gfs[ipoint,ialt].Z, $
          time_series_mean_gfs[ipoint,ialt].U, $
          time_series_mean_gfs[ipoint,ialt].V, $
          !VALUES.F_NAN, $ ;SST Reynolds
          !VALUES.F_NAN, $ ;SST 1km
          mission_avg_Ts[ialt], $
          time_series_year[ipoint,ialt], $
          time_series_month[ipoint,ialt], $
          time_series_day[ipoint,ialt], $
          time_series_hour[ipoint,ialt], $
          time_series_minute[ipoint,ialt], $
          time_series_second[ipoint,ialt], $
          infile, format='(A10, 36F10.2, 1X, A-86)'
      endfor
      free_lun, lun
      
    ; Used to calculate mission-average MTP at each requested height here.
    endfor
    
    for itemprange = 0, n_elements(tempranges)/2.-1 do begin
    
      temprange = tempranges[*,itemprange]
      
      if !D.NAME eq 'X' then wset, 0
      ; plot microwave temperature profile in top panel.
      plot, time_series_MTPTimes, time_series_Ts[*,0], yrange=temprange, xtickformat='(A1)', $
        position=posb, /nodata, ystyle=1, xstyle=0, xminor=4, xrange=xrange, $
        ytitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title+'!C'
      block_out_single_channel_retrieval_times, date, xrange, posb
      
      for ialt=0,nalt-1 do begin
        if ngood_MTP gt 0 then begin
          if usedrops eq 1 then time_series_MTPTimes = time_series_dropTimes[*,ialt]
          errplot, time_series_MTPTimes[iGood_MTP], time_series_Ts[iGood_MTP,ialt]-time_series_SEtemp[iGood_MTP,ialt],time_series_Ts[iGood_MTP,ialt]+time_series_SEtemp[iGood_MTP,ialt], color = get_color(ialt,nalt), width=1./ncycles, thick=9./ncycles^0.6
          oplot, time_series_MTPTimes[iGood_MTP], time_series_Ts[iGood_MTP,ialt], color = get_color(ialt,nalt), psym=7, symsize=2./ncycles^0.4, thick=9./ncycles^0.4
          lag_1_autocorrelation = a_correlate(time_series_Ts[iGood_MTP,ialt],1)
          xyouts, time_series_MTPTimes[min(iGood_MTP)], time_series_Ts[min(iGood_MTP),ialt], string(lag_1_autocorrelation,format='("!4q!X!D1!N=",F4.2)'),align=1.1,charsize=!P.CHARSIZE*0.41,noclip=0,color=get_color(ialt,nalt)
        endif
        if debug gt 0 then xyouts, time_series_MTPTimes[iGood_MTP], time_series_Ts[iGood_MTP,ialt], string(time_series_Ts[iGood_MTP,ialt],format='(F7.3)'),charsize=0.68,align=0.5
        ; for some reason the psym=10 histogram setting goes off the scale on 0817 when it ends in NaN
        ifinite = where(finite(time_series_dropsonde[*,ialt]),nfinite)
        if nfinite gt 0 then oplot, time_series_dropTimes[ifinite,ialt], time_series_dropsonde[ifinite,ialt], psym=10, color=get_color(ialt,nalt), thick=4.7
        ymean = mean(time_series_dropsonde[*,ialt],/nan)
        ; if there are no dropsondes at this height (like really high), ymean=NaN
        ; use MTP temperatures for label location
        if ~finite(ymean) && nGood_MTP gt 0 then ymean = mean(time_series_Ts[iGood_MTP,ialt],/nan)
        if debug gt 1 then print, 'ymean=', ymean
        xyouts, mean(!X.CRANGE), ymean, string(alt_reqs[ialt],format='(I0,"m")'), noclip=0
      endfor
      ; Plot temperature of the coldest point
      if ngood_MTP gt 0 then oplot, time_series_MTPTimes[iGood_MTP], time_series_cold_pt[iGood_MTP], psym=5, symsize=0.46
      
      ; Plot bottom panel
      plot_bottom_panel, posa, xrange,nalt,time_series_dropTimes,dist2center,time_series_IR,time_series_MTPTimes,time_series_GV_temp,$
        time_series_IR_blocks,time_block_starts,time_block_minutes, time_series_SatIRtime
        
        
    endfor ; new temprange
  endif
  
  
  ; Moved IR time_series to old_microwave_temp_profile.pro
  ; used to do MRI histogram and SEtemp histograms here.
  ; Moved plot lapse rates for all levels to old_microwave_temp_profile.pro
  
  if !D.NAME eq 'PS' then device, /close
  !P.THICK=1.
  !P.CHARTHICK=1.0
  
  
end


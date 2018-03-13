; copied from microwave_temp_profile.pro Feb 26, 2013.

pro run_microwave_temperature_profile, start=start
  if ~keyword_set(start) then start = 0
  mpex=1
  PREDICT=0
  if PREDICT then begin
    ;  first mission on 8/15 is not into a system. it is a stalled frontal boundary
    t = get_PREDICT_missions(count=nmissions)
    ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
    for imission=start,nmissions-2 do microwave_temp_profile, t.pgi[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', usedrops=0
  endif
  if mpex then begin
    files = file_search('/Volumes/pecan2/ahijevyc/mpex/MTP/MP2013*.NGV',count=nfiles)
    for ifile = start,nfiles-1 do microwave_temp_profile, 'mpex', strmid(file_basename(files[ifile]),2,8), usedrops=0
  endif
end


function MTPcounts
  return, { VERSION: 1.,  $
    DATASTART: 2L, $
    DELIMITER: ' ',  MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  9, $
    FIELDTYPES: [5,7,5,5,5,5,5,5,7], $
    FIELDNAMES: ['Sec','HHMMSS','PALT','lat','lon','cv1','cv2','cv3','good'], $
    FIELDLOCATIONS: lindgen(9), $
    FIELDGROUPS: lindgen(9) }
    
end

pro just_IR
  basedir = '/Volumes/pecan2/ahijevyc/'
  
  first_time = julday(09, 14, 2010, 12, 00, 00)
  last_time  = julday(09, 14, 2010, 21, 35, 00)
  xrange=[first_time,last_time]
  times_uniform = timegen(final=last_time, start=first_time, step_size=30, units="Seconds")
  nt = n_elements(times_uniform)
  GV_file = basedir+'PREDICT/GV/RF19.20100914.130300_213700.PNI.sav'
  restore, GV_file
  this_lat = interpol_NAN(GVdata.lat,GVdata.juldays,times_uniform)
  this_lon = interpol_NAN(GVdata.lon,GVdata.juldays,times_uniform)
  ir_uniform = replicate(!VALUES.F_NAN, nt)
  for i=0,nt-1 do begin
    indate = string(times_uniform[i], format='(C(CYI4,CMOI2.2,CDI2.2))')
    intime = string(times_uniform[i], format='(C(CHI2.2,CMI2.2,CSI2.2))')
    get_ir, out, indate, intime, this_lat[i], this_lon[i], closest_imageJulian
    ir_uniform[i] = out
  endfor
  plot, times_uniform, ir_uniform, xstyle=5, ystyle=1, yrange = [29,-82]
  axis, xaxis=0, xrange=xrange, xtickformat=['LABEL_DATE','LABEL_DATE'], xminor=4, xticklen=1
  axis, 0, 30, xaxis=0, xrange=xrange, xtickformat='kiloSeconds'
  
end

pro block_out_single_channel_retrieval_times, date, xrange_in, pos
  xrange=xrange_in
  single_freq_intervals = mtp_bad_cases(date, xrange)
  tvlct, oldct, /get
  tvlct, replicate(!D.NAME eq 'X' ? 50 : 190, 1,3), 1 ; use dark gray for X and light gray for PS
  if n_elements(single_freq_intervals) gt 1 then begin
    for ibox = 0, n_elements(single_freq_intervals)/2-1  do begin
      polyfill, [single_freq_intervals[0,ibox], replicate(single_freq_intervals[1,ibox],2), single_freq_intervals[0,ibox]], $
        [replicate(!Y.CRANGE[0],2), replicate(!Y.CRANGE[1],2)], color=1, noclip=0
    endfor
  endif
  ; Make legend outside if-clause.  We want the legend even if there are no single channel retrievals to indicate.
  ; get width of text so polyfill can be correct width. use noclip=0 to avoid printing it.
  xyouts, 0., 1.,'MTP single channel retrieval', charsize=!P.CHARSIZE*0.5,/norm,noclip=0,width=label_width
  legend_pos = [pos[0]+0.8*(pos[2]-pos[0]),pos[3]+0.33*(1.-pos[3]),pos[0]+0.8*(pos[2]-pos[0])+label_width,pos[3]+0.51*(1.-pos[3])]
  polyfill, [legend_pos[0],legend_pos[2],legend_pos[2],legend_pos[0]], [legend_pos[1],legend_pos[1],legend_pos[3],legend_pos[3]], color=1,/norm
  tvlct, oldct
  xyouts, legend_pos[0],legend_pos[1]+0.01,'MTP single channel retrieval', charsize=!P.CHARSIZE*0.5,/norm
  
end


function kiloSeconds, axis, index, value
  jday_fract = value-long(value)
  seconds = ((0.5d + jday_fract) mod 1.0) * 3600d * 24.
  ks = seconds/1000.
  return, string(ks,format='(F5.1,"ks")')
end


pro plot_bottom_panel, posa, xrange, xstyle, nalt, time_series_dropTimes,dist2center,time_series_IR,time_series_MTPTimes,$
  time_series_GV_temp,time_series_SatIRtime
  
  ; add x axis on top of top panel
  axis, xaxis=1, xrange=xrange, xtickformat=['LABEL_DATE','LABEL_DATE'], xminor=4, xticklen=1, xstyle=xstyle
  
  if posa[1] ge posa[3] then return ; if range of y position is not positive, return
  
  distColor = 89
  distThick = !D.NAME eq 'PS' ? 3.78 : 1.1
  GVcolor = 252
  
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
  
  axis, xaxis=0, xrange=xrange, xtickformat=['LABEL_DATE','LABEL_DATE'], xminor=4, xticklen=1, xstyle=xstyle
  axis, 0, -0.26, xaxis=0, xrange=xrange, xtickformat='kiloSeconds', xstyle=xstyle
  dist_range = [0,1000]
  if dist2center ne !NULL then begin
    for ialt=0,nalt-1 do plot, time_series_dropTimes[*,ialt], dist2center[*,ialt], yrange=dist_range, psym=3, $
      xminor=4, position=posa, /noerase, ystyle=13, xstyle=5, xrange=!X.CRANGE, color=distColor, thick=distThick
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
  xyouts, max([!X.CRANGE[0],min(time_series_MTPTimes,/nan)]), time_series_GV_temp[max(where(finite(time_series_GV_temp)))]-!CONST.T0, 'MTP!Chorizon temp', $
    color=GVcolor, charsize=0.62, align=1
  xyouts, !X.WINDOW[1], 0, systime(), align=1, charsize=!P.CHARSIZE*0.5, /norm
  
end


;
; In order to use the 1-m resolution average profile for all missions, I had to run this once
; with some modifications (like 1-m spacing from 0-22km & not subtracting the all-mission average profile).
; And then I ran plot_MTPmean in
; microwave_temp_profile_old.pro, which averaged the mission-average profiles.
; Then I could run this normally.  Normally, this subtracts the all-mission average profile from the raw
; observations interpolates to 500m spacing, and saves the mission-average deviation profile.
;

pro microwave_temp_profile, pgi, date, time, time_range=time_range, units=units, usedrops=usedrops
  debug = 0
  if ~keyword_set(units) then units='m' ; don't try 'mb' - the savfile names do not discriminate between m and mb.
  if ~keyword_set(usedrops) then usedrops = 0 ; or MTP usedrops = 0
  whichAlt = 'press_alt'; 'geomAlt'; 'press_alt' ; Pressure altitude is an aviation thing. It is not geometric (true) altitude.
  ; Geometric altitude is the true, tape-measured distance to the surface of the earth. See http://mtp.mjmahoney.net/www/notes/altitude/altitude.html
  if whichAlt eq 'press_alt' then units = 'press_alt_m'
  basedir = '/Volumes/pecan2/ahijevyc/'
  path = basedir+'PREDICT/MTP/'+(usedrops eq 1 ? 'usedrops/' : '')
  atmos_const
  
  if ISA(time, /number) then time = strmid(string(time*10000L,format='(I2.2)'),0,6)
  
  
  if n_elements(pgi) eq 0 then pgi = 'PGI44L'
  if n_elements(date) eq 0 then date='20130608'
  if n_elements(time) eq 0 then time = '120000'
  if strmid(date,0,4) eq '2013' then begin
    path = basedir+'mpex/MTP/'
    time_range = [10.55,10.75]
    pgi='mpex'
    time = !NULL
  endif
  mtm00 = 1
  best_track = 0
  
  if usedrops eq 0 then alt_reqs = get_levels('CFAD_MTP')
  if usedrops eq 1 then alt_reqs = reverse(get_levels('m'))
  color = date eq '20130519' ? '' : 'light '
  thick = 2
  mean_profile_file = path + "mission_avg_profile_"+pgi+"_"+date+"_"+(time eq !NULL ? '' : time+"_")+whichAlt+".txt"
  if file_test(mean_profile_file) eq 0 then begin
    print, 'there is no mean profile file every 1-m . you might want to create it so you can create .csv files'
    alt_reqs = lindgen(22001) ;  uncomment to create components necessary for 1-m all-mission average profile.
  endif
  
  nalt = n_elements(alt_reqs)
  ncycles = 1700 ; longest single flight has 1678 cycles.
  
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
  
  badMRIcolor=2
  badAltcolor=!D.NAME eq 'X' ? 9 : 255
  
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  if time ne !NULL then begin
    hour   = strmid(time, 0, 2)
    minute = strmid(time, 2, 2)
    second = strmid(time, 4, 2)
    mission_julday = julday(month, day, year, hour, minute, second)
  endif else mission_julday = julday(month,day,year)
  
  
  ; transitioning to all lists() apr 2014  - might save memory
  time_series_MTP_Ts            = list()
  dist2center                   = list()
  time_series_MTPTimes          = replicate(!VALUES.D_NAN, ncycles)
  time_series_dropTimes         = replicate(!VALUES.D_NAN, ncycles, nalt)
  time_series_dTs               = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_MRI               = replicate(!VALUES.F_NAN, ncycles)
  time_series_SETemp            = replicate(0., ncycles, nalt)  ; Used to be NaN 20130209 - but want zero for usedrops=1
  time_series_GV_Press_alt_km   = replicate(!VALUES.F_NAN, ncycles)
  time_series_GV_temp           = replicate(!VALUES.F_NAN, ncycles)
  time_series_cold_pt           = replicate(!VALUES.F_NAN, ncycles)
  time_series_Zcold_pt          = replicate(!VALUES.F_NAN, ncycles) ; cold point height
  time_series_UTSEC             = replicate(!VALUES.D_NAN, ncycles)
  ;time_series_GeomAlts          = replicate(!VALUES.F_NAN, ncycles, nalt)
  ;time_series_PressAlts         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_dropsonde         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_IR                = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_xy_km             = list()
  time_series_lon               = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_SatIRtime         = replicate(!VALUES.D_NAN, ncycles, nalt)
  ; Why have separate dropsonde and Ts ? so you can plot them both if usedrops=0.
  time_series_UV                = list()
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
  
  ; This was created by averaging the mission averages in plot_meanMTP.pro.
  ; It's not important to be precise (i.e. it doesn't matter that we weight equally the missions with lots
  ; of observations and the missions with few.).
  ; This is subtracted from the raw MTP temperature observations.
  ; Then the vertical interpolation to a requested vertical level is done.
  all_mission_T = OBJ_NEW('IDL_Savefile',path+'plot_meanMTP.sav')
  all_mission_T->Restore, 'avgTs'
  all_mission_T->Restore, 'avgHgt'
  obj_destroy, all_mission_T
  all_mission_avgTs = interpol_NAN(avgTs, avgHgt, alt_reqs, max_gap=1)
  MTP_minus_dropsonde=0
  
  
  savfile = path + 'savfiles/' + pgi + date + (time eq !NULL ? '' : '_' + time) + '_' + whichAlt + (MTP_minus_dropsonde?'_MTP-dropsonde':'')+'.sav'
  if file_test(savfile) then begin
    ; sanity check to see if the vertical levels in the save file are the same as what you are requesting now
    requesting_now = alt_reqs
    restore, savfile
    if array_equal(requesting_now, alt_reqs) ne 1 then begin
      print, 'the save file has different vertical levels than the ones you are requesting now'
      print, 'save file:', alt_reqs, ' requesting now: ', requesting_now
      stop
    endif
  endif else begin
  
    MTPtime = read_NGV_header(infile, lun=lun)
    
    ; === read 1 MTP data cycle at a time ===
    icycle = -1L
    while ~ EOF(lun) do begin
      if icycle mod 15 eq 0 then print, '.', format='(A,$)'
        ; === read header of MTP data cycle ===
        readf, lun, UTSEC, nlevs, GV_Press_alt_km, Pitch_deg, Roll_deg, Horizon_brightness_temp, trop_alt, trop_alt2, $
        Trop1theta, Trop2theta, lat, lon, lapse_rate, MRI, Tcp_K, Zcp_km
      julday = MTPtime + UTSEC/3600d/24d
      ; Reset these in case nlevs = 0 (empty cycles with no T profiles happen a lot in MTP data).
      ; before 20130423 I didn't reset and the previously good cycle data were used.
      ; this resulted in lines of horizontal values on scatterplots. Lots of repeats.
      GeomAlts  = !VALUES.F_NAN
      PressAlts = !VALUES.F_NAN
      MTP_Ts    = !VALUES.F_NAN
      SEtemps   = !VALUES.F_NAN
      ; === Read individual lines of the MTP data cycle. Some cycles have just a header and zero data lines. ===
      if nlevs gt 0 then begin
        iline = 0
        GeomAlts  = fltarr(nlevs)
        PressAlts = fltarr(nlevs)
        MTP_Ts   = replicate(!VALUES.F_NAN,nlevs); initialize with NaNs not zero - sometimes there's no matching all-mission profile height so T should be missing
        SEtemps  = fltarr(nlevs)
        ; read cycle data
        repeat begin
          readf, lun, Press_alt_m, T, SEtemp, GeomAlt, num_density
          ; Use geometric altitude. Geometric altitude is the true, tape-measured distance to the surface of the earth-like ellipsoid. See http://mtp.mjmahoney.net/www/notes/altitude/altitude.html
          GeomAlts[iline]  = GeomAlt
          PressAlts[iline] = Press_alt_m
          MTP_Ts[iline] = T
          which_alt = whichAlt eq 'geomAlt' ? GeomAlt : Press_alt_m;Press_alt_m ; Alt
          iavgts = where(avgHgt eq round(which_alt), nmatch)
          ; there are some negative heights in the MTP data.
          if nmatch eq 1 then begin
            ; Subtract the all-mission mean profile in raw MTP coordinates. This may be added back
            ; after interpolating to the requested altitudes with interpol_NAN. (line 330)
            ; after Jul 2013 I stopped subtracting the all-mission profile to simplify things.
            if subtract_all_mission_average_before_interpolating then MTP_Ts[iline] = MTP_Ts[iline] - avgTs[iavgts]
          endif
          SEtemps[iline] = SEtemp
          iline = iline+1
        endrep until iline eq nlevs
      endif ; If there were any measured levels in this cycle
      
      keep_cycle = 1
      if GV_Press_alt_km lt 12 or nlevs eq 0 then keep_cycle = 0
      if there_is_a_closer_PREDICT_mission(julday, mission_julday) then begin
        if debug then print, pgi, datestring, timestring, infile, " was double counted in the past"
        keep_cycle = 0
      endif
      ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
      ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
      if ( pgi eq 'PGI48L' or pgi eq 'PGI51L' ) and PGI48LPGI51L(pgi,julday) ne pgi then keep_cycle = 0
      
      if keep_cycle then begin; keep cycle block: be careful not to double count and account for split between PGI48 and PGI51.
        icycle = icycle+1L ; used to be outside the keep_cycle block, but this messes up list MTP_dTs
        time_series_MTPTimes[icycle] = julday  ; can't be counting by cycle for time and by keep_cycle for MTP_Ts.
        time_series_cold_pt[icycle]  = Tcp_K - !CONST.T0
        time_series_Zcold_pt[icycle] = Zcp_km
        time_series_MRI[icycle] = MRI
        time_series_GV_temp[icycle] = Horizon_brightness_temp
        time_series_UTSEC[icycle] = UTSEC
        which_alts = whichAlt eq 'geomAlt' ? GeomAlts : PressAlts
        ; found this MAJOR bug Apr 17 2014. Not sure what effect it had or if it was always there.
        ; was hard-wired to get PressAlts, didn't allow for GeomAlts. I think this only broke GeomAlts.
        time_series_MTP_Ts.add, interpol_NAN(MTP_Ts,which_alts,alt_reqs,max_gap=3000)
        time_series_SEtemp[icycle,*] = interpol_NAN(SEtemps,which_alts,alt_reqs,max_gap=3000)
        time_series_GV_Press_alt_km[icycle] = GV_Press_alt_km
        time_series_lon[icycle,*] = lon
        ; If you subtracted the all-mission mean profile when you read the MTP temperatures in raw MTP coordinates,
        ; you add all_mission_avgTs back to get temperature again.
        if subtract_all_mission_average_before_interpolating then time_series_MTP_Ts[-1]  = time_series_MTP_Ts[-1] + all_mission_avgTs
        
        
        caldat,julday, month, day, year, hour, minute, second
        datestring = string(year,month,day,format='(I4.4,I2.2,I2.2)')
        timestring = string(hour,minute,second,format='(I2.2,I2.2,I2.2)')
        
        if strmatch(pgi, 'PGI*') then begin
          mtm_center, pgi, datestring, timestring, clat, clon, best_track=best_track, mtm00=1, silent=debug?0:1, u=usys,v=vsys
          if finite(clon) && finite(clat) then begin
            result = map_2points(clon,clat,lon,lat)
            dist2center.add, result[0] * !DTOR * !CONST.R_EARTH/1000
            az = result[1]
            time_series_xy_km.add, [sin(az*!DTOR), cos(az*!DTOR)] * dist2center[icycle,0]
          endif
        endif
        
        if usedrops eq 1 then begin
          ; when calling get_closest_sonde_in_time, use /GV_only.  Or you could get drops from other systems-probably dropped from G4 and DC8.
          junk = get_closest_sonde_in_time(datestring, timestring, alt_reqs, /GV_only, /just_filename, /nocape)
          ; if closest dropsonde has not changed since the last cycle, just use the old dropsonde data instead of re-looking it up.
          if n_elements(old_dropsonde_data) gt 0 && old_dropsonde_data.filename eq junk.filename then begin
            dropsonde_data = old_dropsonde_data
            ; besides using old dropsonde variables (temperature, u, v)
            ; use old distance and position if the nearest dropsonde is the same as before.
            dist2center[-1] = dist2center[-2]
            time_series_xy_km[-1] = time_series_xy_km[-2]
          endif else begin
            ; I added the subtract_avgTs keyword so the all-mission average temperature could be subtracted in the
            ; raw coordinates of the dropsonde sounding. Just like it is done with the MTP data.
            dropsonde_data = get_closest_sonde_in_time(datestring, timestring, alt_reqs, /GV_only, units=units, /nocape)
            old_dropsonde_data = dropsonde_data
            for ialt = 0, nalt-1 do begin
              lat = dropsonde_data.lat[ialt]
              lon = dropsonde_data.lon[ialt]
              result = map_2points(clon,clat,lon,lat)
              dist2center[-1,ialt] = result[0] * !DTOR * !CONST.R_EARTH
              az = result[1]
              time_series_xy_km[-1,ialt] = [sin(az*!DTOR), cos(az*!DTOR)] * dist2center[-1,0]
              time_series_lon[icycle,ialt] = lon
            endfor
          endelse
          time_series_dropTimes[icycle,*] = dropsonde_data.Julian
          time_series_UV.add, [dropsonde_data.U, dropsonde_data.V]
          time_series_dropsonde[icycle,*] = dropsonde_data.t ; note, .t is used. But .dT also available.
          if not MTP_minus_dropsonde then time_series_MTP_Ts[-1]=dropsonde_data.t ; comment out to subtract dropsonde T from MTP T
          
        endif ; usedrops eq 1 block - assign dropsonde stuff
      endif ; keep cycle block: be careful not to double count and account for split between PGI48 and PGI51.
      
    endwhile ; finish reading *.NGV MTP file
    free_lun, lun
    
    caldat, time_series_MTPTimes, UTC_Month, UTC_Day, UTC_Year, UTC_hour, UTC_minute, UTC_second
    time_series_LST = (24. + UTC_hour + UTC_minute/60. + UTC_second/3600. + time_series_lon/15.) mod 24
    
    
    ; Histogram of quality measure for MTP (MRI) and find cutoff point. Redefine iGood_MTP based on MRI cutoff.
    h2 = histogram(time_series_MRI, binsize=0.01, min=0, max=2.01, locations=xvals, /nan)
    plot, xvals, h2, xrange=[0,2.01], title=title, $
      xtitle='MRI', ytitle='count', psym=10, thick=4, charthick=3,charsize=2,xstyle=1
    MRI_cutoff = get_histogram_cutoff(xvals, h2)
    ;  print, title, MRI_cutoff
    good_MTP    = time_series_MRI lt MRI_cutoff
    iGood_MTP   = where(good_MTP, ngood_MTP)
    
    
    
    ; add up the 2-D array horizontally,
    ; only use good_MTP cycles
    ; only use finite temperatures
    ; divide total temperature by number of elements
    mission_avg_Ts = total(time_series_MTP_Ts[igood_MTP].ToArray(),1,/nan)/total(finite(time_series_MTP_Ts[igood_MTP].ToArray()),1,/nan)
    ; Assign deviation from mission average to time_series_dTs.
    time_series_dTs = time_series_MTP_Ts.ToArray() - mission_avg_Ts##replicate(1,time_series_MTP_Ts.count())
    
    if (MTP_minus_dropsonde) then begin
      ; Assign MTP-dropsonde temperature difference to time_series_dTs.
      time_series_dTs = (time_series_MTP_Ts-!CONST.T0) - time_series_dropsonde
    endif
    
    ; used to save time_series_GFS, but ran into memory problems.
    if not array_equal(alt_reqs, findgen(22001)) then $;  don't save if this is the 1-m all-mission average profile.
      save, alt_reqs, time_series_dropsonde, time_series_SatIRtime, time_series_IR, dist2center, time_series_GV_temp, $
      time_series_MRI, time_series_cold_pt, time_series_Zcold_pt, time_series_SEtemp, $
      time_series_MTP_Ts, time_series_dTs, time_series_UTSEC, $
      time_series_GV_Press_alt_km, $
      time_series_dropTimes, time_series_MTPTimes, time_series_xy_km, $
      time_series_UV, good_MTP, igood_MTP, ngood_MTP, MRI_cutoff, mission_avg_Ts, $
      time_series_LST, $
      filename=savfile
  endelse
  
  ; tried to debug with this but it often killed IDL
  ;  erase
  ;  plot, alt_reqs##replicate(1,time_series_MTP_Ts.count()), time_series_dTs, psym=3, nsum=100
  
  ; Calculate mission-average MTP at each requested height.
  ; Interpolate to 0-22km every 1 m.
  ; Used to create IDV *.csv files that contain deviation from mission average.
  ; Execute run_MTP2csv.csh to create the *.csv files.
  ; Then a temperature curtain is plotted in IDV.
  if file_test(mean_profile_file) eq 0 then begin
    openw, meanm, mean_profile_file, /get_lun
    printf, meanm, 'MRI cutoff = ', MRI_cutoff
    for ialt = 0, nalt-1 do printf, meanm, long(alt_reqs[ialt]), mission_avg_Ts[ialt]
    free_lun, meanm
    return
  endif
  
  
  
  date_label = label_date(date_format=['%H:%I', '%D %M, %Y'])
  ; note calendar date format
  first_time = string(min(time_series_MTPTimes,/nan), format='(C(CHI2.2, ":", CMI2.2))')
  xrange = [min(time_series_MTPTimes,/nan),max(time_series_MTPTimes,/nan)]
  ; If time_range was not set, then the 1st element is likely 0., the default. If it is not zero, then define xrange to zoom in on the time axis.
  if time_range[0] ne 0 then xrange = julday(month,day,year,time_range,0, 0) ; zoom in time range
  last_time  = string(max(time_series_MTPTimes,/nan), format='(C(CHI2.2, ":", CMI2.2))')
  title = pgi + " " + strmid(date,4,2) + "/" + strmid(date,6,2) + " " + first_time + "-" + last_time
  
  
  
  pos = [0.15,0.15,0.86,0.84]
  posa= [pos[0],pos[1],pos[2],0.15]
  posb= [posa[0],posa[3]+0.01,posa[2],pos[3]]
  ; Plot symbol is a rectangle. Its width is inversely proportional to the time range
  A = FINDGEN(5) * (!PI*2/4.) + !PI/4.
  
  ; Here's where the IDL version of a "temperature curtain" is drawn.
  ; as opposed to the 3D, coiled version in IDV.
  ref2sub = rebin(transpose(mission_avg_Ts),ncycles,nalt)
  for ibound_dTemp_range = 5,5,1 do begin
    dTemp_range = [-ibound_dTemp_range,ibound_dTemp_range]
    Temp_range  = [-85, 5] ; [-80, 10]
    xstyle = time_range[0] ne 0
    ; plot MTP minus reference temperature in top panel.
    plot, time_series_MTPTimes, replicate(0,ncycles), yrange=[7000,18000], xtickformat='(A1)', $
      position=posb, /nodata, ystyle=9, xminor=4, ytickformat='(I0)', yticklen=1, $
      ytitle='height from '+whichAlt +' [m]', title=title+'!C', xrange=xrange, xstyle=xstyle
    colorbar_position=[posb[2]+0.1*(1.-posb[2]),posb[1]+0.1*(posb[3]-posb[1]),posb[2]+0.2*(1.-posb[2]),posb[1]+0.9*(posb[3]-posb[1])]
    USERSYM, cos(a)/(xrange[1]-xrange[0])*0.06, (!Y.WINDOW[1]-!Y.WINDOW[0])*220*sin(a)/nalt, /fill
    block_out_single_channel_retrieval_times, date, xrange, pos
    
    if ngood_MTP gt 0 then begin
      dTemp = time_series_MTP_Ts[iGood_MTP].ToArray()-ref2sub[iGood_MTP,*]
      TempC = time_series_MTP_Ts[iGood_MTP].ToArray()-!CONST.T0
      ; do not plot NaN's . in .ps they are black.
      igood_dTemp = where(finite(dTemp),ngood_dTemp)
      if ngood_dTemp gt 0 then begin
        dTcolors = bytscl(dTemp[iGood_dTemp], min=dTemp_range[0], max=dTemp_range[1])
        RawColors = bytscl(TempC[iGood_dTemp], min= Temp_range[0], max= Temp_range[1])
        generic_time = usedrops ? time_series_dropTimes[iGood_MTP,*] : rebin(time_series_MTPTimes[iGood_MTP],nGood_MTP,nalt)
        rawT = 0 & anom = 1
        timex = generic_time[igood_dTemp]
        alts = (rebin(transpose(alt_reqs),nGood_MTP,nalt))[igood_dTemp]
        if (rawT) then begin
          plots, timex, alts, color=RawColors, symsize=!D.NAME eq 'X' ? 1 : 0.5, psym=8, noclip=0
          mycolorbar, position=colorbar_position,title='MTP ($\circ$)',           range= Temp_range, /vertical,format='(I0,"!Z(00B0)C")',/right, charsize=!P.CHARSIZE*0.7
          endif
        if (anom) then begin
          tvlct, oldct, /get
          load_act, 'div_blue2darkred_86'
          plots, timex, alts, color=dTcolors, symsize=!D.NAME eq 'X' ? 1 : 0.5, psym=8, noclip=0
          mycolorbar, position=colorbar_position,title='MTP - mission avg', range=dTemp_range, /vertical,format='(F+4.1,"!Z(00B0)C")',/right, charsize=!P.CHARSIZE*0.7,color=!D.NAME eq 'X' ? 128:255
          tvlct, oldct

          GV_file = file_search(basedir + pgi + "/GV/RF*."+date+".nc",count=nfiles)
          if nfiles eq 0 then stop
          savfile = file_dirname(GV_file)+'/'+file_basename(GV_file, '.nc')+'.sav'
          if file_test(savfile) then restore, savfile else begin
            GVdata = get_GV(GV_file)
            save, GVdata, filename=savfile
          endelse
          gvdate = string(mean(gvdata.Juldays), format='(c(CYI4.4,CMOI2.2,CDI2.2))')
          i = fft_stuf(gvdata.atx, gvdata.time/3600d/24, dt=1., color='red', name='ATX (K) '+gvdate, thick=thick)

          p = alt2Press((rebin(transpose(alt_reqs),nGood_MTP,nalt)))
          field = (TempC+!CONST.T0)*(1000./p)^(!ATMOS.Rd/!ATMOS.Cpd)
          times = time_series_MTPTimes[iGood_MTP,0]
          min_period_sec=194.5525
          air_spd_kt = 400.
          sn = 9 ; y-axis separation
          fname= '/mmmtmp/ahijevyc/'+date+'_Angle6counts.txt'
          t=read_ascii(fname, data_start=2,template=MTPcounts())
          igood = where(t.good eq 'True', /null)
          i = fft_stuf(transpose(sn^2.*t.cv3[igood]),reform(t.Sec[igood])/3600d/24, color=color+'gray', name='ch3 (V) '+date, thick=thick)
          i = fft_stuf(transpose(sn*t.cv2[igood]),reform(t.Sec[igood])/3600d/24, color=color+'blue', name='ch2 (V) '+date,thick=thick)
          i = fft_stuf(transpose(t.cv1[igood]),reform(t.Sec[igood])/3600d/24, color=color+'green', name='ch1 (V) '+date,thick=thick)
          filt = fft_stuf(0.01*field, times, 0, name='MTP (K) '+date, thick=thick)
          
          
          xyouts, posb[0], posb[3]+0.38*(1-posb[3]), 'min wave period '+ $
            string(min_period_sec,air_spd_kt*0.51444*min_period_sec/1000,$
            air_spd_kt, format='(I0," sec (",F0.1," km @ ",I0," kt)")'), /norm, charsize=1.01
          contour, filt, times, alt_reqs, /overplot, /follow, levels=310+findgen(31)*5, color=0, thick=2
        endif
        
      endif
      oplot, time_series_MTPTimes, time_series_GV_Press_alt_km*1000, thick=1.24
    endif
    
    ; lag-n autocorrelation for one mission - efolding interval for all missions is calculated in cfad_mtp.pro.
    ;    for ialt = 0, nalt-1, 2  do begin
    ;      lag_1_autocorrelation = a_correlate(time_series_MTP_Ts[iGood_MTP,ialt],indgen(300))
    ;      efold_interval = min(where(lag_1_autocorrelation le 1/exp(1)))
    ;      xyouts, time_series_MTPTimes[min(iGood_MTP)], alt_reqs[ialt], string(lag_1_autocorrelation[1], efold_interval, format='("!4q!X!D1!N=",F4.2," n=",I0)'),align=1.1,charsize=0.4,noclip=0
    ;    endfor
    
    ; Plot bottom panel
    plot_bottom_panel, posa, xrange, xstyle, nalt, time_series_dropTimes, dist2center.ToArray(), time_series_IR, time_series_MTPTimes, $
      time_series_GV_temp,time_series_SatIRtime
  endfor
  
  
  if !D.NAME eq 'PS' then device, /close
  !P.THICK=1.
  !P.CHARTHICK=1.0
  
  
end

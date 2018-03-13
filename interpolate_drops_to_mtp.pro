pro run_interpolate_drops_to_MTP, start=start, debug=debug
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(start) then start = 0
  !P.MULTI=[0,2,3]
  if !D.NAME eq 'PS' then device, /close, /color, bits=8, ysize=9, yoffset=1, /inch
  !P.CHARSIZE=2
  loadct, 39, /silent
  
  project = 'mpex'
  
  if project eq 'PREDICT' then begin
    ;  first PREDICT mission on 8/15 is not into a system. it is a stalled frontal boundary/
    t = get_PREDICT_missions(count=nmissions)
    ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
    for imission=start,nmissions-2 do interpolate_drops_to_MTP, t.pgi[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', $
      cumulative_dXs=cumulative_dXs, $
      cumulative_GeomAlts = cumulative_GeomAlts, $
      cumulative_PressAlts = cumulative_PressAlts, $
      cumulative_good_MTP = cumulative_good_MTP, project = 'PREDICT', debug=debug
  endif
  if project eq 'mpex' then begin
    t = get_mpex_missions(count=nmissions)
    
    for imission=start,nmissions-1 do interpolate_drops_to_MTP, t.rf[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', $
      cumulative_dXs=cumulative_dXs, $
      cumulative_GeomAlts = cumulative_GeomAlts, $
      cumulative_PressAlts = cumulative_PressAlts, $
      cumulative_good_MTP = cumulative_good_MTP, $
      cumulative_MRI = cumulative_MRI, $
      cumulative_dropfiles = cumulative_dropfiles, $
      cumulative_GV_Press_alt_km = cumulative_GV_Press_alt_km, $
      cumulative_trop_alt_km = cumulative_trop_alt_km, $
      cumulative_trop_alt2_km = cumulative_trop_alt2_km, $
      cumulative_solar_angle = cumulative_solar_angle, project = 'mpex', debug=debug
  endif
  
  if !D.NAME eq 'PS' then device, /close
  !P.THICK=1.
  !P.CHARTHICK=1.0
  !P.CHARSIZE=1.0
  
end

pro boxy, x, y, _extra=_extra, debug=debug, project=project, independent_blocksize=independent_blocksize
  if ~keyword_set(independent_blocksize) then independent_blocksize=36  ;for MTP (12min per drop/20s per MTP cycle)
  if ~keyword_set(debug) then debug = 0
  
  binsize = project eq 'mpex' ? 500. : 500.; meters
  ; adjacent vertical levels in a sounding are not independent. If the vertical bin spans adjacent levels, account for this
  independent_blocksize = independent_blocksize * round(binsize/500.)
  bmin = 0
  bmax = 14000
  h = histogram(y, binsize=binsize, min = bmin, max=bmax, reverse_indices=r, locations = bin_bottoms, /nan)
  confidence_level = 0.99
  Tmean   = replicate(!VALUES.F_NAN, n_elements(bin_bottoms))
  Tstd    = replicate(!VALUES.F_NAN, n_elements(bin_bottoms))
  TCI     = replicate(!VALUES.F_NAN, n_elements(bin_bottoms))
  Tn      = replicate(0L, n_elements(bin_bottoms))
  N_indep = replicate(0L, n_elements(bin_bottoms))
  for i = 0, n_elements(bin_bottoms)-1 do begin
    if r[i] ne r[i+1] then begin
      bin_Ts = x[R[R[I] : R[i+1]-1]]
      ngood = total(finite(bin_Ts))
      Tn[i] = ngood
      IF ngood ge 1 THEN Tmean[i] = mean(x[R[R[I] : R[i+1]-1]], /nan)
      IF ngood ge 2 THEN Tstd[i] = stddev(x[R[R[I] : R[i+1]-1]], /nan)
      N_indep[i] = round(float(ngood)/independent_blocksize)
      if N_indep[i]-1 ge 1 then TCI[i] = t_cvf((1-confidence_level)/2., N_indep[i]-1) * Tstd[i]/sqrt(N_indep[i])
    endif
  endfor
  
  ;  i12 = where(y gt 11750 and y lt 12250) ; for chris Davis' question about 12km dTs for each mission on Dec 12, 2013. maybe delete.
  ;  print, mean(x[i12],/nan)
  
  
  oplot, [0,0], !Y.CRANGE
  oplot, Tmean, bin_bottoms+binsize/2., _extra=_extra, psym=-1
  oplot, Tmean-TCI, bin_bottoms+binsize/2., _extra=_extra, linestyle=2, psym=-1, symsize=0.8
  oplot, Tmean+TCI, bin_bottoms+binsize/2., _extra=_extra, linestyle=2, psym=-1, symsize=0.8
  xyouts, !X.CRANGE[0], !Y.CRANGE[0] + 0.95*(!Y.CRANGE[1]-!Y.CRANGE[0]), string(confidence_level*100, binsize,$
    independent_blocksize, format='(I4,"% CI!C",I5,"m vert. bins!C  independent block size:",I0)'), $
    charsize= 0.3, align=0.0
  n_column_x = !X.CRANGE[1]+0.01*(!X.CRANGE[1]-!X.CRANGE[0])
  xyouts, n_column_x, !Y.CRANGE[1], 'n, df', align=0, charsize= 0.25
  ypos = bin_bottoms+binsize/4.
  iy = where(ypos gt !Y.CRANGE[0] and ypos lt !Y.CRANGE[1], niy)
  xyouts, replicate(n_column_x, niy), ypos[iy], strtrim(Tn[iy],2)+','+strtrim(N_indep[iy],2), align=0, charsize= 0.25
  ptimestamp
end


pro plot_GV_alt, alts_km
  alts = alts_km*1000.
  std = stddev(alts,/nan)
  confidence_level=0.99
  n_indep = total(finite(alts))
  CI = t_cvf((1-confidence_level)/2., N_indep-1) * std/sqrt(N_indep)
  
  tvlct, old, /get
  loadct, 2, /silent
  oplot, !X.CRANGE, replicate(mean(alts,/nan),2), color=14
  oplot, !X.CRANGE, replicate(mean(alts,/nan),2)-ci, color=14, linestyle=1
  oplot, !X.CRANGE, replicate(mean(alts,/nan),2)+ci, color=14, linestyle=1
  tvlct, old
end

pro append, x, dx
  x = n_elements(x) gt 0 ? [x, dx] : [dx]
end

pro interpolate_drops_to_MTP, pgi, date, time, cumulative_dXs=cumulative_dXs, $
  cumulative_GeomAlts = cumulative_GeomAlts, $
  cumulative_PressAlts = cumulative_PressAlts, $
  cumulative_good_MTP = cumulative_good_MTP, $
  cumulative_MRI = cumulative_MRI, $
  cumulative_dropfiles = cumulative_dropfiles, $
  cumulative_GV_Press_alt_km = cumulative_GV_Press_alt_km, $
  cumulative_trop_alt_km = cumulative_trop_alt_km, $
  cumulative_trop_alt2_km = cumulative_trop_alt2_km, $
  cumulative_solar_angle = cumulative_solar_angle, project=project, debug=debug
  ; Interpolate dropsonde geometric height (function of geopotential height and latitude) to raw MTP geometric heights.
  ; Similar to microwave_temp_profile, but different enough to start a new program.
  ; Temperatures in this program range freely in height instead of being interpolated to common vertical coordinate basis,
  ; like every 0.5 km. Started 20130622
  nalt = 30 ; max # of levels in raw MTP data
  if ~keyword_set(debug) then debug = 0
  usedrops = 1 ; This is not needed here but consistent with microwave_temp_profile.pro
  
  if n_elements(pgi) eq 0 then pgi = 'RF01'
  if n_elements(date) eq 0 then date='20130515'
  if n_elements(time) eq 0 then time='120000'
  if ~keyword_set(project) then project = 'mpex'
  mtm00 = 1
  best_track = 0
  if debug then print, pgi, date, time
  ncycles = 1700 ; longest single flight has 1678 cycles.
  title = pgi + " " + strmid(date,4,2) + "/" + strmid(date,6,2)
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  path = basedir+project+'/MTP/'
  
  close, /all
  
  
  
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  mission_julday = julday(month, day, year, hour, minute, second)
  
  
  time_series_MTP_Ts            = replicate(!VALUES.F_NAN, ncycles, nalt)
  dist2center                   = list()
  time_series_MTPTimes          = replicate(!VALUES.D_NAN, ncycles)
  time_series_dropfiles         = replicate('', ncycles)
  time_series_dropTimes         = replicate(!VALUES.D_NAN, ncycles, nalt)
  time_series_MRI               = replicate(!VALUES.F_NAN, ncycles)
  time_series_SETemp            = replicate(0., ncycles, nalt)  ; Used to be NaN 20130209 - but want zero for usedrops=1
  time_series_GV_Press_alt_km   = replicate(!VALUES.F_NAN, ncycles)
  time_series_trop_alt          = replicate(!VALUES.F_NAN, ncycles)
  time_series_trop_alt2         = replicate(!VALUES.F_NAN, ncycles)
  time_series_GeomAlts          = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_PressAlts         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_dropsonde         = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_xy_km             = list()
  time_series_lon               = replicate(!VALUES.F_NAN, ncycles, nalt)
  time_series_sonde_solar_angle = replicate(!VALUES.F_NAN, ncycles, nalt)
  last_good_cycle = replicate(-1L, nalt)
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
  
  
  savfile = path + 'savfiles/' + pgi + date + '_' + time + '_interpDrops2MTP.sav'
  upsonde_only = 0 ;
  if upsonde_only then begin
    distance_window_km = 150.
    time_window_min = 90.
    savfile = path + 'savfiles/' + pgi + date + '_' + time + '_interpUps2MTP'+string(time_window_min, distance_window_km,format='("_",I3.3,"min_",I3.3,"km")')+'.sav'
  endif
  if file_test(savfile) then restore, savfile else begin
  
    MTPtime = read_NGV_header(infile, lun=lun)  ; === read header lines of *.NGV file ===
    
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
        time_series_MTPTimes[icycle] = julday
        time_series_MRI[icycle] = MRI
        time_series_MTP_Ts[icycle,0:nlevs-1] = MTP_Ts
        time_series_GeomAlts[icycle,0:nlevs-1] = GeomAlts
        time_series_PressAlts[icycle,0:nlevs-1] = PressAlts
        time_series_SEtemp[icycle,0:nlevs-1] = SEtemps
        time_series_GV_Press_alt_km[icycle] = GV_Press_alt_km
        time_series_trop_alt[icycle] = trop_alt
        time_series_trop_alt2[icycle] = trop_alt2
        time_series_lon[icycle,0:nlevs-1] = lon
        
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
          ;this first part of the if-statement is a kludge to match the nearest upsonde,
          ; distance-wise, and within the time_and_space_window, to the MTP.
          if upsonde_only  then begin
            close_files = find_sondes_in_time_and_space_window(datestring, timestring, lat, lon, project=project, upsonde_only=1, $
              distance_window_km = distance_window_km, time_window_min=time_window_min)
            dropsonde_data = get_closest_sonde_in_time(!VALUES.F_NAN, !VALUES.F_NAN, PressAlts)
            min_d = !VALUES.D_INFINITY
            for ifile = 0, n_elements(close_files)-1 do begin
              upfile = close_files[ifile]
              if upfile eq '' then continue
              up_data = get_sonde(upfile, PressAlts, units='press_alt_m', /nocape)
              result = map_2points(lon, lat, up_data.slon, up_data.slat)
              d = result[0]*!DTOR*!CONST.R_earth/1000
              if d lt min_d then begin
                dropsonde_data = up_data
                min_d = d
              endif
              print, datestring, timestring, up_data.filename, d, ' km', (julday-mean(up_data.Julian, /nan))*24., ' h'
            endfor
          endif else begin
            dropsonde_data = get_closest_sonde_in_time(datestring, timestring, PressAlts, /GV_only, units='press_alt_m', /nocape, project=project, /just_filename)
            if dropsonde_data.filename eq '' then continue
            reuse_old_dropsonde = 0 ; It's incorrect to reuse old dropsonde but that's what I did before Oct 4, 2013. I have this option to reproduce old figures.
            if n_elements(old_dropsonde_data) gt 0 && dropsonde_data.filename eq old_dropsonde_data.filename && $
              reuse_old_dropsonde then dropsonde_data = old_dropsonde_data else $
              dropsonde_data = get_closest_sonde_in_time(datestring, timestring, PressAlts, /GV_only, units='press_alt_m', /nocape, project=project)
            if finite(dropsonde_data.max_gap) then max_gap = dropsonde_data.max_gap
            if abs(min(dropsonde_data.Julian,/nan) - julday) gt 5d/24./60. then begin
              ;print, dropsonde_data.filename, min(dropsonde_data.Julian,/nan), julday, format='(A, C(), c())'
              continue ; get_closest_sonde_in_time uses 20 minutes; i want 5. ahijevch sep 30 2014
            endif
          endelse
          ; when calling get_closest_sonde_in_time, use /GV_only.  Or you could get drops from other systems-probably dropped from G4 and DC8.
          ; Prior to Oct 4 2013, after running get_closest_sonde_in_time with /just_filename, if the filename of the
          ; closest dropsonde had not changed since the last cycle, I simply reused the old dropsonde data instead of rerunning get_closest_sonde_in_time .
          ; But I realized you can't use the same old_dropsonde data because the PressAlts array changes with each cycle.
          ; It only made sense when PressAlts was a static list of altitudes to interpolate to every 500m.
          old_dropsonde_data = dropsonde_data
          
          
          for ialt = 0, nlevs-1 do begin
            lat = dropsonde_data.lat[ialt]
            lon = dropsonde_data.lon[ialt]
            time_series_lon[icycle,ialt] = lon
          endfor
          time_series_dropfiles[icycle] = dropsonde_data.filename
          time_series_dropTimes[icycle,0:nlevs-1] = dropsonde_data.Julian
          time_series_dropsonde[icycle,0:nlevs-1] = dropsonde_data.t ; note, .t is used. But .dT also available.
          day_of_year = dropsonde_data.Julian - Julday(1, 0, year)
          caldat, dropsonde_data.Julian, junk, junk1, junk2, hour, minute, second
          zensun,day_of_year,hour + minute/60. + second/3600.,dropsonde_data.lat,dropsonde_data.lon,zenith,azimuth,solfac;input UTC hrs, not local solar hrs
          time_series_sonde_solar_angle[icycle,0:nlevs-1] = zenith
          
        endif ; usedrops eq 1 block - assign dropsonde stuff
      endif ; keep cycle block: be careful not to double count and account for split between PGI48 and PGI51.
      
    endwhile
    free_lun, lun
    time_series_dTs = (time_series_MTP_Ts -!CONST.T0)- time_series_dropsonde ; time_series_dropsonde could be pressure altitude or
    ; geometric height, depending on what units were when get_closest_sonde_in_time() was called.
    
    ; Histogram of quality measure for MTP (MRI) and find cutoff point. Redefine iGood_MTP based on MRI cutoff.
    h2 = histogram(time_series_MRI, binsize=0.01, min=0, max=2.01, locations=xvals, /nan)
    ; .r microwave_temp_profile for get_histogram_cutoff()
    MRI_cutoff = get_histogram_cutoff(xvals, h2)
    ;  plot, xvals, h2, xrange=[0,2.01], title=title, $
    ;    xtitle='MRI', ytitle='count', psym=10, thick=4, charthick=3,charsize=2,xstyle=1
    ;  oplot, replicate(MRI_cutoff, 2), !Y.CRANGE, thick=6, color=233
    ;  print, title, MRI_cutoff
    
    good_MTP    = time_series_MRI lt MRI_cutoff
    ;******* uncomment for temporary debugging ********
    ;  good_MTP[*] = 1B ; make everything good, no matter the MRI.
    ;*******
    iGood_MTP   = where(good_MTP, ngood_MTP, complement=ibad_MTP, ncomplement=nbad_MTP)
    
    
    save, time_series_dropsonde, dist2center, time_series_GeomAlts, time_series_PressAlts, $
      time_series_MRI, time_series_SEtemp, time_series_MTP_Ts, time_series_dTs, time_series_sonde_solar_angle, $
      time_series_GV_Press_alt_km, time_series_trop_alt, time_series_trop_alt2, $
      time_series_dropfiles, $
      time_series_dropTimes, time_series_MTPTimes, time_series_xy_km, $
      good_MTP, igood_MTP, ngood_MTP, nbad_MTP, MRI_cutoff, icycle, $
      filename=savfile
  endelse
  
  
  ;  h2 = histogram(time_series_sonde_solar_angle, nbins=14, locations=xvals, /nan)
  ;  plot, xvals, h2, psym=10, title='solar zenith angle'
  
  
  
  
  ;  h2 = histogram(time_series_trop_alt, min=5, max=18, binsize=1, locations=xvals, /nan)
  ;  plot, xvals, h2, psym=10, title='tropopause altitude'
  ;  oplot, xvals, histogram(time_series_trop_alt2, min=5, max=18, binsize=1, locations=xvals, /nan), psym=10, color=223
  ;
  xrange = [-2,2]
  if !D.NAME eq 'X' then wset, 0
  if !D.NAME eq 'X' then device, decomposed=0
  plot, time_series_dTs, time_series_PressAlts, yrange=[4000, 13500], ytickformat='divide_by_1000', xrange=xrange, psym=3, xticklen=1, $
    xtickinterval=1, ytitle='pressure altitude (km)', xtitle='temp deviation (K)', xtickformat='(I+0)', xstyle=1, title = project + ' ' + title, /nodata
  if ngood_MTP gt 0 then begin
    oplot, time_series_dTs[igood_MTP,*], time_series_PressAlts[igood_MTP,*], psym=3, color=85
    plot_GV_alt, time_series_GV_Press_alt_km[igood_MTP]
    boxy, time_series_dTs[igood_MTP,*], time_series_PressAlts[igood_MTP,*], color=230, thick=4, debug=debug, project=project
  endif
  if nbad_MTP gt 0 and debug then boxy, time_series_dTs[ibad_MTP,*], time_series_PressAlts[ibad_MTP,*], color=220, thick=1, debug=debug, project=project
  append, cumulative_dXs, time_series_dTs[0:icycle,*]
  append, cumulative_GeomAlts, time_series_GeomAlts[0:icycle,*]
  append, cumulative_PressAlts, time_series_PressAlts[0:icycle,*]
  append, cumulative_good_MTP, good_MTP[0:icycle]
  append, cumulative_MRI, time_series_MRI[0:icycle]
  append, cumulative_GV_Press_alt_km, time_series_GV_Press_alt_km[0:icycle]
  append, cumulative_trop_alt_km, time_series_trop_alt[0:icycle]
  append, cumulative_trop_alt2_km, time_series_trop_alt2[0:icycle]
  append, cumulative_solar_angle, time_series_sonde_solar_angle[0:icycle,*]
  append, cumulative_dropfiles, time_series_dropfiles[0:icycle]
  if !D.NAME eq 'X' then begin
    device, window_state = t
    if t[1] eq 0 then window, 1
    wset, 1
  endif
  final_frame =  title eq 'PGI51L 09/30' or title eq 'RF15 06/14'
  if final_frame then begin
    plot, cumulative_dXs, cumulative_PressAlts, yrange=[4000, 13500], ytickformat='divide_by_1000', xrange=xrange, psym=3, xticklen=1, $
      xtickinterval=1, ytitle='pressure altitude (km)', xtitle='temp deviation (K)', xtickformat='(I+0)', xstyle=1, title=project, /nodata
    igood = where(cumulative_good_MTP eq 1, /null)
    oplot, cumulative_dXs[igood,*], cumulative_PressAlts[igood,*], psym=3, color=90
    plot_GV_alt, cumulative_GV_Press_alt_km[igood]
    boxy, cumulative_dXs[igood,*], cumulative_PressAlts[igood,*], color=230, thick=4, debug= final_frame || debug, project=project
    dMRI = 0.1
    for MRI=0.,1.,dMRI do begin
      MRIstr = string(MRI,MRI+dMRI, format='(2x,F3.1,"<= MRI < ",F3.1)')
      plot, cumulative_dXs, cumulative_PressAlts, yrange=[4000, 13500], ytickformat='divide_by_1000', xrange=xrange, psym=3, xticklen=1, $
        xtickinterval=1, ytitle='pressure altitude (km)', xtitle='temp deviation (K)', xtickformat='(I+0)', xstyle=1, title=project+MRIstr, /nodata
      igood = where(cumulative_MRI ge MRI and cumulative_MRI lt MRI+dMRI, /null)
      if igood eq !NULL then continue
      oplot, cumulative_dXs[igood,*], cumulative_PressAlts[igood,*], psym=3, color=90
      plot_GV_alt, cumulative_GV_Press_alt_km[igood]
      boxy, cumulative_dXs[igood,*], cumulative_PressAlts[igood,*], color=230, thick=4, debug= final_frame || debug, project=project
    endfor
    for icrud=0,29 do plot, abs(cumulative_dXs[*,icrud]), cumulative_MRI, yrange=[0, 1], xrange=[0,8], psym=3, xticklen=1, $
      xtickinterval=1, ytitle='MRI', xtitle='temp deviation (K)', xtickformat='(I+0)', xstyle=1, title=project+string(icrud)

    isun = where(cumulative_solar_angle lt 80, nsun)
    ;    boxy, cumulative_dXs[isun], cumulative_PressAlts[isun], color=190, thick=3, project=project, max_gap=max_gap
    ;    print, "fraction in sun:", total(float(nsun)/total(finite(cumulative_dXs[igood,*])))
    ;    iTrop = where(cumulative_GV_Press_alt_km lt (cumulative_trop_alt_km - 3), nTrop)
    ;    boxy, cumulative_dXs[iTrop,*], cumulative_PressAlts[iTrop,*], color=150, thick=3, project=project
    ;    print, "fraction in troposphere:", total(float(nTrop)/total(finite(cumulative_GV_Press_alt_km)))
  endif
  
end

function get_sonde_type, filename
  fb = file_basename(filename)
  stid = strmid(fb, 8, 5, /reverse)
  ; from June Wang's spreadsheet mpex-RadiosondeTypes.xlsx
  stid = long(stid)
  case stid of
    72249: sonde_type = 'LMS-6 GPS'
    72265: sonde_type = 'MKIIA_GPS'
    72274: sonde_type = 'RS92-NGP'
    72357: sonde_type = 'MKIIA_GPS'
    72363: sonde_type = 'MKIIA_GPS'
    72364: sonde_type = 'RS92-NGP'
    72365: sonde_type = 'MKIIA_GPS'
    72376: sonde_type = 'RS92-NGP'
    72451: sonde_type = 'LMS-6 GPS
    72469: sonde_type = 'MKIIA_GPS'
    72476: sonde_type = 'MKIIA_GPS'
    72558: sonde_type = 'MKIIA_GPS'
    72562: sonde_type = 'MKIIA_GPS'
    72572: sonde_type = 'LMS-6 GPS'
    72649: sonde_type = 'MKIIA_GPS'
    72659: sonde_type = 'MKIIA_GPS'
    72662: sonde_type = 'MKIIA_GPS'
    72672: sonde_type = 'MKIIA_GPS'
    74646: sonde_type = 'RS92-NGP' ; Actually just RS92, but same as RS92-NGP for temperature (June's email)
    else: stop
  endcase
  return, sonde_type
end

pro run_run_drops_minus_ups
  if !D.NAME eq 'PS' then device, /close, /color, bits=8
  
  time_window_minutess = [45, 90, 135]
  distance_window_kms = [50, 100, 150]
  field = 'U'
  nt = n_elements(time_window_minutess)
  nd = n_elements(distance_window_kms)
  !P.MULTI = [0, nd, nt, 1,0]
  for t=0,nt-1 do begin
    for d=0,nd-1 do begin
      run_drops_minus_ups, time_window_minutes=time_window_minutess[t], distance_window_km=distance_window_kms[d], field=field
    endfor
  endfor
  if !D.NAME eq 'PS' then device, /close
  
end


pro run_drops_minus_ups, start=start, debug=debug, time_window_minutes=time_window_minutes, distance_window_km=distance_window_km, $
  field=field
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(start) then start = 0
  if ~keyword_set(time_window_minutes) then time_window_minutes = 90.
  if ~keyword_set(distance_window_km) then distance_window_km = 150.
  if ~keyword_set(field) then field='T' ; could be 'U' too
  ;sonde_type = 'RS92-NGP' ; 'RS92-NGP' 'LMS-6 GPS' 'MKIIA_GPS' ; comment out if you dont' want to filter by sonde type.
  
  if !D.NAME eq 'X' then device, decomposed=0
  loadct, 39, /silent
  
  if field eq 'T' then xrange=[-1.75,1.75]
  if field eq 'U' then xrange=[-3,3] ; [-3,3] works for dU; [0,30] works for U
  plot, [0],[0], xrange=xrange, yrange=[4000,14000], ytickformat='divide_by_1000', yticklen=1, xticklen=1, $
    title='upsonde '+(n_elements(sonde_type)?'('+sonde_type+') ':'')+'- drop '+field+' within ' + $
    string(time_window_minutes,distance_window_km, format='(I0,"minutes, ",I0,"km")'),  $
    xtickinterval=1, charsize=2/(!P.MULTI[1]+!P.MULTI[2]), ytitle='pressure altitude (km)', xtitle=field+' deviation', xtickformat='(I+0)', xstyle=1, ystyle=1
    
  project = 'mpex'
  
  
  t = get_mpex_missions(count=nmissions)
  
  for imission=start,nmissions-1 do drops_minus_ups, t.rf[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', $
    cumulative_dXs=cumulative_dXs, cumulative_Us=cumulative_Us, $
    cumulative_GeomAlts = cumulative_GeomAlts, $
    cumulative_PressAlts = cumulative_PressAlts, $
    cumulative_good_MTP = cumulative_good_MTP, project = 'mpex', $
    time_window_minutes=time_window_minutes, distance_window_km=distance_window_km, $
    sonde_type = sonde_type, field=field
    
    
  boxy, cumulative_dXs, cumulative_PressAlts, color=227, thick=4, debug = debug, project=project, independent_blocksize=1
  
  
end

pro drops_minus_ups, pgi, date, time, cumulative_dXs=cumulative_dXs, $
  cumulative_GeomAlts = cumulative_GeomAlts, cumulative_Us=cumulative_Us,  $
  cumulative_PressAlts = cumulative_PressAlts, $
  cumulative_good_MTP = cumulative_good_MTP, project=project, debug=debug, $
  time_window_minutes=time_window_minutes, distance_window_km=distance_window_km, $
  sonde_type = sonde_type, field=field
  
  alt_reqs = 250+findgen(50)*500.
  nalt = 250 ; max # of vert. levels
  debug = 0
  
  if n_elements(pgi) eq 0 then pgi = 'RF01'
  if n_elements(date) eq 0 then date='20130515'
  if n_elements(time) eq 0 then time='120000'
  if ~keyword_set(project) then project = 'mpex'
  if ~keyword_set(distance_window_km) then begin
    print, "not meant to run drops_minus_ups on its own. Run run_drops_minus_ups."
    stop
  endif
  
  if debug then print, pgi, date, time
  ndrops = 50 ; most drops in an mpex flight was 33.
  title = pgi + " " + strmid(date,4,2) + "/" + strmid(date,6,2)
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  path = basedir+project+'/'
  
  
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  mission_julday = julday(month, day, year, hour, minute, second)
  
  dropfiles = file_search(path+'dropsondes/D'+date+'*.eol', count=ndrops)
  
  dist2center           = replicate(!VALUES.F_NAN, ndrops, nalt)
  time_series_dropTimes = replicate(!VALUES.D_NAN, ndrops, nalt)
  time_series_GeomAlts  = replicate(!VALUES.F_NAN, ndrops, nalt)
  time_series_PressAlts = replicate(!VALUES.F_NAN, ndrops, nalt)
  time_series_dropsonde = replicate(!VALUES.F_NAN, ndrops, nalt)
  time_series_lon       = replicate(!VALUES.F_NAN, ndrops, nalt)
  
  for idrop = 0, ndrops-1 do begin
    dropfile = dropfiles[idrop]
    datestring = strmid(file_basename(dropfile),1,8)
    timestring = strmid(file_basename(dropfile),10,6)
    drop = get_sonde(dropfile, alt_reqs, units='press_alt_m', /nocape)
    close_files = find_sondes_in_time_and_space_window(datestring, timestring, mean(drop.lat,/nan), mean(drop.lon,/nan), upsonde_only=1, $
      project=project, time_window_minutes=time_window_minutes, distance_window_km=distance_window_km)
    for ifile = 0, n_elements(close_files)-1 do begin
      ; We include all upsondes within the time and space window, not just the closest one.
      ; How would you compare distances anyway when you are talking about space AND time separation?
      upfile = close_files[ifile]
      if upfile eq '' then continue
      up_data = get_sonde(upfile, alt_reqs, units='press_alt_m', /nocape)
      result = map_2points(mean(drop.lon,/nan), mean(drop.lat,/nan), up_data.slon, up_data.slat)
      if n_elements(sonde_type) gt 0 then if get_sonde_type(up_data.filename) ne sonde_type then continue
      d = result[0]*!DTOR*!CONST.R_earth/1000.
      az = result[1]
      ;      if not finite(d) then stop
      if d gt 1000 then stop
      X_up = up_data.(where(tag_names(up_data) eq field, /null))
      X_dn = drop.(where(tag_names(drop) eq field, /null))
      dX = X_up - X_dn
      if up_data.max_gap ne drop.max_gap then stop
      max_gap = up_data.max_gap
      print, up_data.filename, '  ', dropfile, d, ' km', az, ' deg', (mean(drop.Julian, /nan)-mean(up_data.Julian, /nan))*24., $
        ' h max_gap_drop=', drop.max_gap, ' max_gap_up=', up_data.max_gap
      ; uncomment to print profiles of temperature and delta-temperature for each match
      ;for ialt= 0,24 do print, alt_reqs[ialt], X_up[ialt], X_dn[ialt], dX[ialt], format='(I5,2F10.4,F9.4)'
      append, cumulative_dXs,  dX
      append, cumulative_PressAlts, alt_reqs
      
      oplot, dX, alt_reqs, psym=3, color=170
    endfor
  endfor
  
end

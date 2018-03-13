function top_transition_zone, d, mean_cycle

  ; INPUT
  ; d - dropsonde structure
  ; mean_cycle - MTP structure
  
  ; OUTPUT
  ; top_tz - top of the transition zone in pressure altitude (meters)
  ; 
  ; METHODOLOGY
  ; The transition zone is where the MTP blends into the dropsonde temperature.
  ; It is topped by the highest pressure altitude for which the MTP temperature
  ; is "close" to the dropsonde temperature. Visual inspection of MTP and dropsonde
  ; profiles overlaid on the same skew-T, suggests a reasonable value "close" is about 3 degrees K. 
  ; 
  ; This gets rid of the "bad" dropsonde profiles that start out way too warm (or cold, in one case), but otherwise
  ; have passed the QC tests. 
  ; 
  ; I make the delta-temperature threshold a multiple of the MTP temperature standard error, which increases from 
  ; around 0.2 near the aircraft to 1.6 further away.  
  ; The weight given to the MTP temperature starts at 1 and transitions to 0 at the bottom of the zone.
  ; The bottom of the zone (not defined in this function) is simply 4km below the aircraft.

  drop_PressAlts = Press2Alt(d.P)
  
  drop_t = !CONST.T0 + interpol_NAN(d.T, drop_PressAlts, mean_cycle.PressAlts)
  
  MTP_nlevs = n_elements(mean_cycle.Ts)
  ; where dropsonde is within 2 standard errors of MTP
  thresh = 6 * mean_cycle.setemps
  dT = mean_cycle.Ts - drop_t
  iclose = where(abs(dT) lt thresh or dT*shift(dT,1) lt 0, /null)
  
  if iclose eq !NULL then begin
    for i=0,MTP_nlevs-1 do print, Alt2Press(mean_cycle.PressAlts[i]), mean_cycle.Ts[i]-!CONST.T0, $
      drop_t[i]-!CONST.T0, mean_cycle.Ts[i]-drop_t[i], mean_cycle.setemps[i], format='(F9.2,"mb",2F8.2,F+6.2,F6.2)'
  endif
  
  ; get highest height where this occurs.
  top_tz = max(mean_cycle.PressAlts[iclose])
  
  return, top_tz
end

function hi2loP, d

  ; Get a nice dropsonde structure for cape calculations. 
  ; There also can't be any missing data temperatures or pressures.
  ; The cape routine expects data to start at the surface and go upward. 
  ; So pressures must go from high to low.


  nlevs = n_elements(d.T)
  Ts        = d.T
  Presss    = d.P
  igood = where(finite(Presss) and finite(Ts),ngood)
  if ngood le 1 then return, d
  i = reverse(sort(Presss[igood]))
  
  newd={}
  fields = tag_names(d)
  for itag=0,n_tags(d)-1 do begin
    name = fields[itag]
    x = d.(itag)
    isaprofile = n_elements(x) eq nlevs
    if isaprofile then vals = (x[igood])[i]
    if n_elements(x) eq 1 then vals = x
    newd = create_struct(newd, name, vals)
    
  endfor
  return, newd
  
end

function mean_cycles, cycles

  ; INPUT
  ; cycles - list of MTP cycles.
  ; OUTPUT
  ; return a single, blended MTP cycle that combines all the input cycles. 

  ; METHOD
  ; calculate the mean of variables in each tag or fit a curve.
  
  ; In level flight the MTP vertical levels associated with each element are almost constant.
  ;   1-D arrays of height, T, and SE can be simply averaged across corresponding elements of the arrays (mean_cycle.curve=0).
  ; But when the aircraft is going up or down, the vertical levels may change.  If any particular element of the array
  ; changes by more than a half-kilometer or the number of vertical levels changes, 
  ; then blend 30 pressure alts at a time working from the bottom up (mean_cycle.curve=1) using mycurvefit(..., clustern=10)
  
  ncycle= cycles.count()
  mean_cycle={curve:0}
  if ncycle eq 0 then return, mean_cycle
  tags = tag_names(cycles[0])
  
  ; normally corresponding elements of pressure altitude array are constant or close to each other.
  ; if they aren't, then blend 10 pressure altitudes at a time, starting from the bottom and working upward and
  ; set curve to 1.
  nlevs0 = n_elements(cycles[0].pressalts)
  pressAlts = !NULL
  for icycle=0,ncycle-1 do begin
    pressAlts = [pressAlts, cycles[icycle].pressalts]
    ; used to not check for changing # of vertical levels, but if they are different sizes, it won't complain.
    if n_elements(cycles[icycle].pressalts) ne nlevs0 || max(abs(cycles[icycle].pressalts - cycles[0].pressalts),imax) gt 500. then begin
      mean_cycle.curve=1
      ;print, imax, cycles[0].pressalts[imax], icycle, cycles[icycle].pressalts[imax], format='("ilev:",I0," cycle 0=",I0,"m, cycle",I0,"=",I0,"m")'
    endif
  endfor
  isort = reverse(sort(pressAlts)); mycurvefit requires monotonic pressure altitude, but everything after assumes top down
  
  for itag = 0, n_tags(cycles[0])-1 do begin
    ys = cycles[0].(itag)
    if ncycle eq 1 then begin
      avg = ys
      std = replicate(!VALUES.F_NAN, n_elements(ys))
    endif else if mean_cycle.curve eq 0 || n_elements(ys) eq 1 then begin
      for i=1,ncycle-1 do ys = [[ys],[cycles[i].(itag)]]
      avg = total(ys,2)/ncycle & if n_elements(avg) eq 1 then avg = avg[0]
      std = stddev(ys,dimension=2,/nan) & if n_elements(avg) eq 1 then std = std[0]
    endif else begin
      for i=1,ncycle-1 do ys = [ys,cycles[i].(itag)]
      avg = mycurvefit( pressAlts[isort], ys[isort], std=std, clustern=30)
    endelse
    mean_cycle = create_struct(mean_cycle, tags[itag], avg, tags[itag]+'_std', std)
  endfor
  
  UTC_hh = string(mean_cycle.time, format='(c(CHI2.2))')
  UTC_mm = string(mean_cycle.time, format='(c(CMI2.2))')
  UTC_ss = string(mean_cycle.time, format='(c(CSI2.2))')
  
  return, create_struct(mean_cycle, 'UTC_hh',UTC_hh, 'UTC_mm',UTC_mm, 'UTC_ss',UTC_ss, 'ncycle', ncycle)
  
end

pro merge_MTP_dropsonde

  ; Merge dropsonde and MTP temperature profiles. 
  ; INPUT
  ; dropsonde data in EOL format
  ; MTP data in *.NGV format
  ;
  ; OUTPUT
  ; hybrid sounding in EOL format
  ; skew-T figure (optional)
  
  
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  project = 'PREDICT'
  allDfiles = get_dropsonde_files(basedir, GV_only=1, count=count, times=times, project=project)
  
  
  merged_drop_file_dir = basedir+project+'/MTP_dropsonde_composite/'
  outps = merged_drop_file_dir + project + '_skewts.ps'
  if !D.NAME eq 'PS' then device, /close, /color, bits=8, filename=outps
  loadct, 39,/silent
  
  show_skewt = 1
  max_MTP_dtime_minutes = 5.0
  MRI_threshold = 0.5
  for idrop = 0, count-1 do begin
  
  
    drop = allDfiles[idrop]
    droptime=times[idrop]
    ;    if file_basename(drop) ne 'D20100817_104008_PQC.eol' then continue ; mismatched MTP alts
    ;       if file_basename(drop) ne 'D20100817_141433_PQC.eol' then continue ; preserve drop resolution?
;    if file_basename(drop) ne 'D20100901_154604_PQC.eol' then continue ; high MRI
    ;
    ;if file_basename(drop) ge 'D20100815_120434_PQC.eol' then continue
    ;if file_basename(drop) ne 'D20100818_141832_PQC.eol' then continue
    ;if file_basename(drop) ne 'D20130608_115723_P.3QC.eol' then continue ; # nlevs change
    
    ;if file_basename(drop) ne 'D20100914_170037_PQC.eol' then continue ; single MTP
;        if file_basename(drop) ne 'D20100902_183004_PQC.eol' then continue ; bad match
    ;    if file_basename(drop) ne 'D20100910_165335_PQC.eol' then continue ; mismatched MTP alts
;        if file_basename(drop) ne 'D20100910_092949_PQC.eol' then continue ; mismatched MTP alts
;        if file_basename(drop) ne 'D20100907_170937_PQC.eol' then continue ; mismatched MTP alts
;        if file_basename(drop) ne 'D20130516_135303_P.2QC.eol' then continue ; mismatched MTP alts
;  if file_basename(drop) ne 'D20130528_104813_P.1QC.eol' then continue ; mismatched MTP alts

    print, drop
    
    s = get_sonde(drop, /nocape)
    d = hi2loP(s)
    Ts        = d.T
    Tds       = d.Td
    Presss    = d.P
    
    igood = where(finite(Presss) and finite(Ts),ngood)
    if ngood le 1 then continue
    MTP_bad_below_GV_km = 4.
    
    
    mtpdate = string(droptime, format='(c(CYI4.4,CMOI2.2,CDI2.2))')
    hour = string(droptime, format='(c(CHI2.2))')
    ; There were two MTP flights Sep. 10. If 1200, then 'a', otherwise 'b'.
    if mtpdate eq '20100910' then mtpdate = hour le '15' ? mtpdate+'a' : mtpdate+'b'
    infile = basedir + project + '/MTP/MP' + mtpdate + '.NGV'
    if file_test(infile) ne 1 then continue ; no MTP data, so continue to next drop
    
    if (show_skewt) then begin
      skewt, [-40.,40], title=file_basename(drop)
      ylab = !Y.WINDOW[1]
      charsize=!D.NAME eq 'PS' ? 0.47 : 1
      xyouts, !X.WINDOW[1]+0.01, ylab, 'MTP ' + string(177b)+string(max_MTP_dtime_minutes,MRI_threshold,$
        format='(F0.1," minutes, MRI<",F0.2,", MRI")'), charsize=charsize, /norm
      plot_skewt, Ts, Tds, Presss, col_t=70, col_dewpt=71, thick=4
    endif

    MTPdate = read_NGV_header(infile, lun=lun, primary=primary,aux=aux)
    cycles=list()
    ; === read 1 MTP data cycle at a time ===
    while ~ EOF(lun) do begin
      ; === read header of MTP data cycle ===
      readf, lun, UTSEC, nlevs, GV_Press_alt_km, Pitch_deg, Roll_deg, Horizon_brightness_temp, trop_alt, trop_alt2, $
        Trop1theta, Trop2theta, lat, lon, lapse_rate, MRI, Tcp_K, Zcp_km
      if trop_alt2 eq aux.missing[6] then trop_alt2 = !VALUES.F_NAN
      if Trop2theta eq aux.missing[8] then Trop2theta = !VALUES.F_NAN
      if nlevs gt 0 then begin
        iline = 0
        GeomAlts  = fltarr(nlevs)
        PressAlts = fltarr(nlevs)
        MTP_Ts   = replicate(!VALUES.F_NAN,nlevs); initialize with NaNs not 0 - sometimes there's no matching all-mission profile height so T should be NaN
        SEtemps  = fltarr(nlevs)
        ; read cycle data
        repeat begin
          readf, lun, Press_alt_m, T, SEtemp, GeomAlt, num_density
          if GV_Press_alt_km - Press_alt_m gt MTP_bad_below_GV_km then T = !VALUES.F_NAN
          ; Use geometric altitude. Geometric altitude is the true, tape-measured distance to the surface of the earth-like ellipsoid. 
          ; See http://mtp.mjmahoney.net/www/notes/altitude/altitude.html
          GeomAlts[iline]  = GeomAlt & PressAlts[iline] = Press_alt_m & MTP_Ts[iline] = T & SEtemps[iline] = SEtemp
          iline = iline+1
        endrep until iline eq nlevs
        MTPtime = MTPdate + UTSEC/3600d/24d
        
        delta_min = 24d*60*(MTPtime - droptime)
        if abs(delta_min) ge max_MTP_dtime_minutes then continue
        if MRI ge MRI_threshold then continue
        if (show_skewt) then begin
          ylab = ylab-0.02
          xyouts, !X.WINDOW[1]+0.01, ylab, string(MTPtime, MRI, format='(C(),x,F4.2)'), charsize=charsize, /norm
          tvlct, old_ct, /get &  loadct, 0, /silent
          plot_skewt, MTP_Ts-!CONST.T0, !VALUES.F_NAN, alt2Press(PressAlts), col_t=193, thick=0.5
          tvlct, old_ct
        endif
        ;print, drop, MTPtime, delta_min, format='(A," ",C(), F5.1)'
        
        cycles.add, {GeomAlts:GeomAlts, PressAlts:PressAlts, Ts:MTP_Ts, SEtemps:SEtemps, nlevs:nlevs, $
          time:MTPtime, GV_Press_alt_km:GV_Press_alt_km, Pitch_deg:Pitch_deg, Roll_deg:Roll_deg, $
          Horizon_brightness_temp:Horizon_brightness_temp, trop_alt:trop_alt, trop_alt2:trop_alt2, Trop1theta:Trop1theta, $
          Trop2theta:Trop2theta, lat:lat, lon:lon, lapse_rate:lapse_rate, MRI:MRI, Tcp_K:Tcp_K, Zcp_km:Zcp_km}
      endif ; If there were any measured levels in this cycle
      
    endwhile ; finish reading *.NGV MTP file
    free_lun, lun
    
    openr, in, drop, /get_lun
    merged_drop_file = merged_drop_file_dir + file_basename(drop)
    openw, out, merged_drop_file, /get_lun
    junk = ''
    repeat begin
      readf, in, junk
      printf, out, junk
    endrep until strpos(junk, 'Post Processing Comments:') eq 0
    
    
    ; Write Post Processing Comments about MTP addition.
    MTP_mean = mean_cycles(cycles)
    printf, out, "Inserted mean of ", cycles.count(), " MTP cycles from "+infile, format='(A, I0,A)'
    printf, out, cycles.count(), " MTP cycles within ", max_MTP_dtime_minutes, " minutes of initial dropsonde time:", format='(I3,A,F4.1,A)'
    printf, out, 'MTP cycle ignored if MRI >= ', MRI_threshold, format='(A,F4.2)'
    for i=0,cycles.count()-1 do printf, out, i+1, cycles[i].time, n_elements(cycles[i].pressalts), format='("(",I3,") ",C(),I4," pressure altitudes")'
    
    
    d = s
    if cycles.count() gt 0 then begin
    
      ; Plot MTP mean with 6*stderr interval
      if (show_skewt) then begin
        tvlct, old_ct, /get
        loadct, 0, /silent
        plot_skewt, MTP_mean.Ts-!CONST.T0, !VALUES.F_NAN, alt2Press(MTP_mean.PressAlts), col_t=60, thick=1.1, psym=1, symsize=0.6
        plot_skewt, MTP_mean.Ts-!CONST.T0-MTP_mean.SEtemps*6, !VALUES.F_NAN, alt2Press(MTP_mean.PressAlts), col_t=60, psym=1, symsize=0.3
        plot_skewt, MTP_mean.Ts-!CONST.T0+MTP_mean.SEtemps*6, !VALUES.F_NAN, alt2Press(MTP_mean.PressAlts), col_t=60, psym=1, symsize=0.3
        tvlct, old_ct
      endif
      
      ; Here's where the transition zone is defined. The upper limit is the first good dropsonde
      ; pressure altitude. Currently it is set to the first one.  The lower limit is 4 km below the
      ; aircraft.
      first_good_PressAlt = top_transition_zone(d, MTP_mean)
      transition_PressAlt = [first_good_PressAlt, 1000. * (MTP_mean.GV_Press_alt_km - MTP_bad_below_GV_km)]
      
      printf, out, "transition zone between MTP and dropsonde: ", transition_PressAlt[1], transition_PressAlt[0], $
        format='(A,F9.2," -",F9.2," (pressure altitude in m)")'
    endif
    
    repeat begin
      readf, in, junk
      printf, out, junk
    endrep until strpos(junk, '-----') eq 0
    
    
    ; insert MTP down through transition zone
    if cycles.count() gt 0 then begin
    
      ; print MTP profile cycle to output file.
      ; This profile starts as pure MTP and blends into dropsonde.
      oldpress = 0.
      for i=0, n_elements(MTP_mean.Ts)-1 do begin ; don't use MTP_mean.nlevs (Ts could be a curve fit)
        PressAlt = MTP_mean.PressAlts[i]
        press = alt2Press(PressAlt)
        format2='(F7.1,x,C(CHI2.2,x,CMI2.2,x,CSI5.2),x,9(F7.2,x), F8.2,x, F11.6,x,F11.6,x,F8.2)'

        if PressAlt ge transition_PressAlt[0] then printf, out, -999, MTP_mean.time, press, $
          MTP_mean.Ts[i]-!CONST.T0, -999, -999, -999, -999, -999, -999, -999, $
          geometric_to_geopotential(MTP_mean.GeomAlts[i],MTP_mean.lat), $
          MTP_mean.lon, MTP_mean.lat, MTP_mean.GeomAlts[i], format=format2 $
        else if PressAlt ge transition_PressAlt[1] then begin
          ; blend in transition zone
          ; blend dropsonde structure d and dropsonde
          interpolate_to_dropsonde_levels = 1 ; if = 0 just interpolate to MTP levels in transition zone.
          if (interpolate_to_dropsonde_levels) then begin
            ; Preserve the high vertical resolution of the dropsonde here
            ; Find dropsonde levels between the previous level above and this one.
            idrop_levs = where(d.P gt oldpress[0] and d.P le press[0], ndrop_levs, /null)
            for idrop_lev = 0, ndrop_levs-1 do begin
              ilev = idrop_levs[idrop_lev]
              PressAlt = press2Alt(d.P[ilev])
              drop_wgt = (PressAlt-transition_PressAlt[0])/(transition_PressAlt[1] - transition_PressAlt[0])
              MTP_wgt = 1.- drop_wgt
              MTP_T = interpol_NaN(MTP_mean.Ts,MTP_mean.PressAlts,PressAlt)-!CONST.T0
              MTP_Z = interpol_NaN(geometric_to_geopotential(MTP_mean.GeomAlts,MTP_mean.lat),MTP_mean.PressAlts,PressAlt)
              MTP_GeomAlt = interpol_NaN(MTP_mean.GeomAlts,MTP_mean.PressAlts,PressAlt)
              printf, out, d.dSec[ilev], d.Julian[ilev], d.p[ilev], $
                d.T[ilev]*drop_wgt       + MTP_T*MTP_wgt, finite(d.Td[ilev])?d.Td[ilev]:-999, -999, $
                finite(d.U[ilev])?d.U[ilev]:-999, $ 
                finite(d.V[ilev])?d.V[ilev]:-999, -999, -999, -999, $
                d.Z[ilev]*drop_wgt       + MTP_Z*MTP_wgt, $
                d.lon[ilev]*drop_wgt     + MTP_mean.lon*MTP_wgt, $
                d.lat[ilev]*drop_wgt     + MTP_mean.lat*MTP_wgt, $
                d.GeomAlt[ilev]*drop_wgt + MTP_GeomAlt*MTP_wgt, format=format2
            endfor
          endif else begin
            drop_wgt = (PressAlt-transition_PressAlt[0])/(transition_PressAlt[1] - transition_PressAlt[0])
            MTP_wgt = 1.- drop_wgt
            drop_T = interpol_NaN(d.T,d.P,press)     &   drop_Td = interpol_NaN(d.Td, d.P, press)
            drop_Z = interpol_NaN(d.Z,d.P,press)     &   drop_lon = interpol_Nan(d.lon, d.P, press)
            drop_lat = interpol_NaN(d.lat,d.P,press)  & drop_GeomAlt = interpol_NaN(d.geomAlt, d.P, press)
            printf, out, -999, MTP_mean.UTC_hh, MTP_mean.UTC_mm, MTP_mean.UTC_ss, press, $
              drop_T*drop_wgt + (MTP_mean.Ts[i]-!CONST.T0)*MTP_wgt, drop_Td, -999, -999, -999, -999, -999, -999, $
              drop_Z*drop_wgt + geometric_to_geopotential(MTP_mean.GeomAlts[i],MTP_mean.lat)*MTP_wgt, $
              drop_lon*drop_wgt     + MTP_mean.lon*MTP_wgt, $
              drop_lat*drop_wgt     + MTP_mean.lat*MTP_wgt, $
              drop_GeomAlt*drop_wgt + MTP_mean.GeomAlts[i]*MTP_wgt, format=format
          endelse
        endif
        oldpress = press; save MTP pressure level.  used to find dropsonde levels between this and the one below.
      endfor
    endif
    
    ; You are in data section of dropsonde file.
    ; Skip dropsonde lines until you drop below the MTP/dropsonde transition zone
    ; Then output dropsonde lines verbatim.
    ; Or start writing dropsonde lines right away if there are no MTP cycles.
    start_writing = 0
    while ~ EOF(in) do begin
      readf, in, junk
      words = strsplit(junk, /extract)
      if cycles.count() eq 0 || start_writing || Press2Alt(words[4]) lt transition_PressAlt[1] then begin; after transition zone, write everything, even bad Ts.
        printf, out, junk
        start_writing = 1
      endif
    endwhile
    
    
    free_lun, in, out
    if (show_skewT) then begin
      d = get_sonde(merged_drop_file, /force_new, /use_first_line)
      d = hi2loP(d)
      bmin = d.pseudo_bmin
      plot_skewt, d.t, d.td, d.p, col_t=210, thick=1, col_dewpt=210
      ptimestamp, 'transition zone:'+strcompress(strjoin(Alt2Press(transition_PressAlt),'-'))+'mb', /right
    endif
  endfor ; drops
  if !D.NAME eq 'PS' then begin
    device, /close
    print, "created "+outps
  endif
end
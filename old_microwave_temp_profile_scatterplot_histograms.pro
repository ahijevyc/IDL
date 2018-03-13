pro old_microwave_temp_profile_scatterplot_histograms


  if (1) then begin ; print scatterplot files, level-by-level histograms of cloud top temp, ...
  
    ; Define the symbol to be a filled ellipse:
    A = FINDGEN(17) * (!PI*2/16.)
    USERSYM, 6*COS(A), 1+2.3*SIN(A), /fill
    
    
    
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
      if ngood_MTP gt 0 then oplot, time_series_MTPTimes[iGood_MTP], time_series_Tcp[iGood_MTP], psym=5, symsize=0.46
      
      ; Plot bottom panel
      plot_bottom_panel, posa, xrange,nalt,time_series_dropTimes,dist2center,time_series_IR,time_series_MTPTimes,time_series_GV_temp,$
        time_series_IR_blocks,time_block_starts,time_block_minutes, time_series_SatIRtime
        
      pos = [0.18,0.15,0.85,0.84]
      
      ; Do some scatter plots
      if nGood_MTP gt 0 then begin
        if !D.NAME eq 'X' then window, 1, xsize=600, ysize=600
        if !D.NAME eq 'X' then wset, 1
        x = time_series_Ts[iGood_MTP,*]
        y = dist2center[iGood_MTP]
        plot, x, y, xrange=temprange, xstyle=1, yrange=[0,800], $
          /nodata, position=pos, ytitle='km from center', xtitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title
        scatter, x, y, ialt, alt_reqs
        
        if !D.NAME eq 'X' then window, 2, xsize=600, ysize=600
        if !D.NAME eq 'X' then wset, 2
        y = time_series_IR[iGood_MTP,*]
        plot, x, y, xrange=temprange, xstyle=1, yrange=[-80,30], ystyle=1, $
          /nodata, position=pos, ytitle='IR temperature !Z(00B0)C', xtitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title
        scatter, x, y, ialt, alt_reqs
        
        y = time_series_MRI[iGood_MTP]
        plot, x, y, xrange=temprange, xstyle=1, yrange=[0,.5], $
          /nodata, position=pos, ytitle='MRI', xtitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title
        scatter, x, y, ialt, alt_reqs
        
        y = time_series_SEtemp[iGood_MTP,*]
        plot, x, y, xrange=temprange, xstyle=1, $
          /nodata, position=pos, ytitle='SEtemp', xtitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title
        scatter, x, y, ialt, alt_reqs
        
        x = time_series_dropsonde[iGood_MTP,*]
        y = time_series_Ts[iGood_MTP,*]
        plot, x, y, xrange=temprange, yrange=temprange, /isotropic, $
          /nodata, position=pos, xtitle='dropsonde temperature !Z(00B0)C', ytitle='microwave temp !Z(00B0)C!Cheight from '+whichAlt, title=title
        oplot, !X.CRANGE,!Y.CRANGE, thick=2, linestyle=2
        scatter, x, y, ialt, alt_reqs
      endif
      
      
    endfor ; new temprange
    
    
    ; purposely put after some plots are drawn - wanted to plot single channel retrieval points but not analyze them now.
    ;good_MTP[*] = 0; uncomment to prepare array for using ONLY single channel retrievals.
    ;  single_freq_intervals = mtp_bad_cases(date, xrange)
    ;  if n_elements(single_freq_intervals) gt 1 then begin
    ;    for ibox = 0, n_elements(single_freq_intervals)/2-1  do begin
    ;      isf = where(time_series_MTPTimes ge single_freq_intervals[0,ibox] and $
    ;        time_series_MTPTimes le single_freq_intervals[1,ibox] and Good_MTP eq 1, nsf)
    ;      ; comment next line to include "single channel retrievals"
    ;      if nsf gt 0 then good_MTP[isf] = 1 ; change to 1 to use single channel
    ;    endfor
    ;    iGood_MTP = where(good_MTP, ngood_MTP) ; have to redefine iGood_MTP because we just set good_MTP=0 when MTP collapsed to single frequency.
    ;  endif
    
    ; plot histograms of temperatures for low, mid, and high cloud segments
    ; I moved this away from the beginning because we redefine some of the "good" data based on the MTP time series as we are plotting them above.
    alt2_reqs = alt_reqs
    if !D.NAME eq 'X' then window, 5, xsize=600, ysize=600
    if !D.NAME eq 'X' then wset, 5
    for i_alt2_req = 0, n_elements(alt2_reqs)-1 do begin
      dtemp_alt = alt2_reqs[i_alt2_req]
      i_alt2 = where(alt_reqs eq dtemp_alt,ndtemp_alt)
      if ndtemp_alt eq 0 then stop
      dtemp_params = get_dtemp_hist_params(dtemp_alt)
      tbin = dtemp_params.tbin
      tloc = dtemp_params.tloc
      low_cloud_color = dtemp_params.low_cloud_color
      mid_cloud_color = dtemp_params.mid_cloud_color
      hgh_cloud_color = dtemp_params.high_cloud_color
      
      hgh_histogram = replicate(0, n_elements(tloc))
      mid_histogram = replicate(0, n_elements(tloc))
      low_histogram = replicate(0, n_elements(tloc))
      high_total = 0L
      mid_total  = 0L
      low_total  = 0L
      ymin = -85.
      ymax = 25.
      which_time_series = usedrops eq 0 ? time_series_Ts[*,i_alt2[0]] : time_series_dropsonde[*,i_alt2[0]]
      xmin = min(which_time_series,/nan)
      xmax = max(which_time_series,/nan)
      plot, tloc, xrange=[xmin,xmax+tbin],yrange=[0,1], /nodata, position=pos, xstyle=1, ystyle=9, $
        xtitle=string(usedrops?'dropsonde':'MTP',dtemp_alt,whichAlt,format='(A,1X,I0,"m temp !Z(00B0)C!Cheight from ",A)'), ytitle = 'fraction of total', title=title
      xyouts, !X.WINDOW[1], 0, systime(), align=1, charsize=!P.CHARSIZE*0.5, /norm
      
      for iblock=0,total(finite(time_block_starts[*,i_alt2[0]]))-1 do begin
        IR_block = time_series_IR_blocks[iblock,i_alt2[0]]
        time_block_start = time_block_starts[iblock,i_alt2[0]]
        caldat, time_block_start, tmonth, tday,tyear, thour, tminute, tsecond
        time_block_start_string = string(tyear,tmonth,tday,thour,tminute,tsecond, format='(I4.4,I2.2,I2.2," ",I2.2,":",I2.2,":",I2.2)')
        mintime = time_block_start
        maxtime = time_block_start + time_block_minutes/24d/60d ; make sure you divide by a double. if time_block_minutes is an integer the result could be zero without you wanting it.
        ;          caldat, mintime, tmonth, tday,tyear, thour, tminute, tsecond
        ;          print, string(tyear,tmonth,tday,thour,tminute,tsecond, format='(I4.4,I2.2,I2.2," ",I2.2,":",I2.2,":",I2.2)')
        ;          caldat, maxtime, tmonth, tday,tyear, thour, tminute, tsecond
        ;          print, string(tyear,tmonth,tday,thour,tminute,tsecond, format='(I4.4,I2.2,I2.2," ",I2.2,":",I2.2,":",I2.2)')
        if usedrops eq 1 then icycle_timeblock = where(time_series_dropTimes[*,i_alt2[0]] ge mintime and time_series_dropTimes[*,i_alt2[0]] lt maxtime and within_dist[*,i_alt2[0]] eq 1, ncycle_timeblock)
        if usedrops eq 0 then icycle_timeblock = where(time_series_dropTimes[*,i_alt2[0]] ge mintime and time_series_dropTimes[*,i_alt2[0]] lt maxtime and within_dist[*,i_alt2[0]] eq 1 and good_MTP, ncycle_timeblock)
        ;    print, min(icycle_timeblock), max(icycle_timeblock)
        if ncycle_timeblock gt 0 then begin
          ; used to subtract avgSST but not anymore - 20111230.
          T_histogram_input = which_time_series[icycle_timeblock]
          if max(T_histogram_input) ge max(tloc+tbin/2.) then begin
            print, pgi, ' ', dtemp_alt,units+ pgi+date+'max histogram bin not high enough. max(input) =',max(T_histogram_input),' but max hist =', max(tloc)+tbin/2.
            stop
          endif
          if min(T_histogram_input) lt min(tloc-tbin/2.) then begin
            print, pgi, ' ', dtemp_alt,units+ pgi+date+'min histogram bin not low enough. min(input) =',min(T_histogram_input),' but hist min =', min(tloc)-tbin/2.
            stop
          endif
          hist_Ts = histogram(T_histogram_input, binsize=tbin, min=min(tloc), max=max(tloc), locations=xloc)
          if ~array_equal(xloc, tloc) then stop
          if IR_block lt -40                   then begin
            hgh_histogram = hgh_histogram + hist_Ts
            high_total = high_total + total(T_histogram_input, /nan)
            plots, T_histogram_input, (time_series_IR[icycle_timeblock,i_alt2[0]]-ymin)/(ymax-ymin), color = hgh_cloud_color, thick=3, psym=4, symsize=0.5
          endif else if IR_block ge -40 and IR_block lt 0 then begin
            mid_histogram = mid_histogram + hist_Ts
            mid_total = mid_total + total(T_histogram_input, /nan)
            plots, T_histogram_input, (time_series_IR[icycle_timeblock,i_alt2[0]]-ymin)/(ymax-ymin), color = mid_cloud_color, thick=3, psym=4, symsize=0.5
          endif else if IR_block ge 0                     then begin
            low_histogram = low_histogram + hist_Ts
            low_total = low_total + total(T_histogram_input, /nan)
            plots, T_histogram_input, (time_series_IR[icycle_timeblock,i_alt2[0]]-ymin)/(ymax-ymin), color = low_cloud_color, thick=3, psym=4, symsize=0.5
          endif
        endif
      endfor
      
      count_label_size = 0.87
      tloc=tloc+tbin/2.
      scale = total(low_histogram)
      if scale gt 0 then begin
        oplot, tloc, low_histogram/scale, psym=10, color=low_cloud_color, thick=4
        xyouts, tloc, low_histogram/scale, strtrim(low_histogram,2), charsize=count_label_size, align=0.5, color=low_cloud_color, noclip=0
        low_mean = low_total/scale
        plots, replicate(low_mean,2), !Y.CRANGE, thick=3.4, color=low_cloud_color, linestyle=1
      endif
      scale = total(mid_histogram)
      if scale gt 0 then begin
        oplot, tloc, mid_histogram/scale, psym=10,color=mid_cloud_color, thick=4.1
        xyouts, tloc, mid_histogram/scale, strtrim(mid_histogram,2), charsize=count_label_size, align=0.5,color=mid_cloud_color, noclip=0
        mid_mean = mid_total/scale
        plots, replicate(mid_mean,2), !Y.CRANGE, thick=3.4, color=mid_cloud_color, linestyle=1
      endif
      scale = total(hgh_histogram)
      if scale gt 0 then begin
        oplot, tloc, hgh_histogram/scale, psym=10, color=hgh_cloud_color, thick=3.2
        xyouts, tloc, hgh_histogram/scale, strtrim(hgh_histogram,2), charsize=count_label_size, align=0.5,color=hgh_cloud_color, noclip=0
        hgh_mean = high_total/scale
        plots, replicate(hgh_mean,2), !Y.CRANGE, thick=3.4, color=hgh_cloud_color, linestyle=1
      endif
      ; we convert IR to a scale from 0.0 to 1. so we plot on the original 0.0-1. axis.
      axis, !X.CRANGE[1], yaxis=1, yrange=[ymin,ymax], ystyle=1, /data, ytitle=string(time_block_minutes,neighbor_radius_m/1000,format='(I0, "min time block!C",I2.2,"km IR temperature !Z(00B0)C")')
      xyouts, pos[2]+0.06, pos[3]-0.2, "high clouds",   color=hgh_cloud_color, /norm, orientation=90, charsize=!P.CHARsize*0.75
      xyouts, pos[2]+0.08, pos[3]-0.2, "mid clouds",    color=mid_cloud_color, /norm, orientation=90, charsize=!P.CHARsize*0.75
      xyouts, pos[2]+0.10, pos[3]-0.2, "no/low clouds", color=low_cloud_color, /norm, orientation=90, charsize=!P.CHARsize*0.75
      
      scatter_file = path + date + '_' + string(time,whichAlt,dtemp_alt,units,round(neighbor_radius_m/1000),format='(I6.6,"_",A,"_",I5.5,A,"_IR",I2.2,"km")')+'_scatter.txt'
      ; PGI51L and PGI48L share the same mission so don't overwrite the PGI48L contribution with the PGI51L contribution.
      if pgi eq 'PGI51L' then openu, lun, scatter_file, /get_lun, /append else openw, lun, scatter_file, /get_lun
      caldat, time_series_dropTimes, time_series_month, time_series_day, time_series_year, time_series_hour, time_series_minute, time_series_second
      for ipoint = 0, ncycles-1 do begin
        ;      if ipoint eq 38 and i_alt2[0] eq 4 then stop
        ; For a while I forgot to filter out bad MTP points where(good_MTP eq 0).  - 20120306
        T_point = usedrops eq 1 ? time_series_dropsonde[ipoint, i_alt2[0]] : good_MTP[ipoint] eq 1 ? time_series_Ts[ipoint, i_alt2[0]] : !VALUES.F_NAN
        if dtemp_alt eq 250 and finite(time_series_IR[ipoint,i_alt2[0]]) and finite(time_series_mean_gfs[ipoint,i_alt2[0]].T) ne 1 then stop
        ; MTP only has temperature - no moisture or pressure.  So no Td or Tv & Z instead of P.
        if finite(T_point) and finite(time_series_IR[ipoint,i_alt2[0]]) and $
          finite(time_series_dropTimes[ipoint,i_alt2[0]]) then $
          printf, lun, pgi, T_point, !VALUES.F_NAN, $
          !VALUES.F_NAN, !VALUES.F_NAN, $ ; Tv and MSE not defined
          dist2center[ipoint,i_alt2[0]], time_series_IR[ipoint,i_alt2[0]], $
          time_series_x_km[ipoint,i_alt2[0]], $
          time_series_y_km[ipoint,i_alt2[0]], $
          !VALUES.F_NAN, dtemp_alt, $ ; p not defined ( but Z is. )
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, $ ; skip CAPE(2), parcel dT(2) (rev and ps)
          !VALUES.F_NAN, !VALUES.F_NAN, $ ; liquid and ice water content
          time_series_U[ipoint,i_alt2[0]],$ ;there's no u and v in MTP! (first 2 NaNs)
          time_series_V[ipoint,i_alt2[0]],$ ; but there are if usedrops=1
          !VALUES.F_NAN, !VALUES.F_NAN, $ ;  don't bother with usys and vsys either.
          time_series_mean_gfs[ipoint,i_alt2[0]].T, time_series_mean_gfs[ipoint,i_alt2[0]].Td, $
          time_series_mean_gfs[ipoint,i_alt2[0]].Tv, $
          time_series_mean_gfs[ipoint,i_alt2[0]].MSE, $
          time_series_mean_gfs[ipoint,i_alt2[0]].Z, $
          time_series_mean_gfs[ipoint,i_alt2[0]].U, $
          time_series_mean_gfs[ipoint,i_alt2[0]].V, $
          !VALUES.F_NAN, $ ;SST Reynolds
          !VALUES.F_NAN, $ ;SST 1km
          mission_avg_Ts[i_alt2[0]], $
          time_series_year[ipoint,i_alt2[0]], $
          time_series_month[ipoint,i_alt2[0]], $
          time_series_day[ipoint,i_alt2[0]], $
          time_series_hour[ipoint,i_alt2[0]], $
          time_series_minute[ipoint,i_alt2[0]], $
          time_series_second[ipoint,i_alt2[0]], $
          infile, format='(A10, 36F10.2, 1X, A-86)'
      endfor
      free_lun, lun
      close, lun
      
    ; Used to calculate mission-average MTP at each requested height here.
    endfor
  endif else print, 'skipped scatterplots, histograms. . . '
  
  
  
  IR_time_series_file = path + "IR_time_series_"+pgi+"_"+date+"_"+time+"_"+whichAlt+".txt"
  openw, lun, IR_time_series_file, /get_lun
  for ipoint = 0, ncycles-1 do begin
    t  = time_series_MTPTimes[ipoint]
    ir = time_series_IR[ipoint,0]
    if ~finite(t) || ~finite(ir) then continue
    caldat, t, month, day, year, hour, minute, second
    printf, lun, year, month, day, hour, minute, second, ir, format='(I4.4,I2.2,I2.2," ",I2.2,":",I2.2,":",F06.3,F8.1)'
  endfor
  free_lun, lun
  
  
  
  pos = [0.2,0.15,0.9,0.84]
  ;  if !D.NAME eq 'X' then window, 3, xsize=600, ysize=600
  ;  if !D.NAME eq 'X' then wset, 3
  ;  plot, time_series_IR, time_series_Tcp[*,0], yrange=[-84,-69], ystyle=1, xrange=[-80,30], xstyle=1, $
  ;    /nodata, position=pos, xtitle='IR temperature !Z(00B0)C', ytitle='microwave coldest temp !Z(00B0)C!Cheight from '+whichAlt, title=title, psym=1, symsize=0.75
  ;  if ngood_MTP gt 0 then oplot, time_series_IR[iGood_MTP], time_series_Tcp[iGood_MTP],color=244, psym=1, symsize=0.75
  
  ; plot lapse rates for all levels (if statement set to true (1) below)
  ; (Moved to old_microwave_temp_profile.pro)
  if 0 then begin
    if !D.NAME eq 'X' then window, 4, xsize=600, ysize=600
    if !D.NAME eq 'X' then wset, 4
    for isep = 1, nalt-1 do begin
      for ialt = 0, nalt-isep-1 do begin
        newlayer = [ialt, ialt+isep]
        layers = n_elements(layers) eq 0 ? newlayer : [layers, newlayer]
      endfor
    endfor
    for ilayer = 0, n_elements(layers)/2-1,2 do begin
      top_layer = layers[ilayer]
      bot_layer = layers[ilayer+1]
      dZ = (alt_reqs[top_layer] - alt_reqs[bot_layer])/1000.
      dTdZ = (time_series_Ts[*,top_layer]-time_series_Ts[*,bot_layer])/dZ
      layer = string(alt_reqs[bot_layer]/1000.,alt_reqs[top_layer]/1000.,format='(I0,"-",I0,"km!C")')
      if total(finite(dtdz)) eq 0 then continue
      plot, time_series_IR, dTdZ, ystyle=16, xrange=[-80,30], xstyle=1, $
        /nodata, position=pos, xtitle='IR temperature !Z(00B0)C', ytitle=layer+'microwave temp lapse rate K/km!Cheight from '+whichAlt, title=title, psym=1, symsize=0.75
      if ngood_MTP gt 0 then oplot, time_series_IR[iGood_MTP], dTdZ[iGood_MTP],color=244, psym=1, symsize=0.75
    endfor
  endif
  
  
  
end
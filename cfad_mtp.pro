function IR_time_series_template
  template = {VERSION: 1.,  $
    DATASTART: 0L, $
    DELIMITER: '',  MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  7, $
    FIELDTYPES: [replicate(4,7)], $
    FIELDNAMES: ['year','month','day','hour', 'minute', 'second', 'IR'], $
    FIELDLOCATIONS: [0,4,6,9,12,15,23], $
    FIELDGROUPS: lindgen(7) }
  return, template
end

pro write_range_height, file, count, z, range, h, whichAlt, debug=debug
  if ~keyword_set(debug) then debug = 0
  nr = n_elements(range)/2.
  nh = n_elements(h)
  openw, lun, file, /get_lun
  for k = 0, nh-1 do for i=0, nr-1 do printf, lun, range[*,i], whichAlt, h[k], alt2press(h[k]), z[i,k], count[i,k], format='("r=",I6,"-",I6," ",a,"=",I6," press=",F8.2, " dT=", F8.4, " n=",I4)'
  free_lun, lun
  if debug then print, 'wrote ' + file
end


pro make_note,  mean_dT_in_time_and_cloud_category

  xyouts, mean(!X.CRANGE), !Y.CRANGE[1], 'removed mean dT in time and cloud category'+$
    string(mean_dT_in_time_and_cloud_category, format='(F+4.1)'), charsize=!P.CHARSIZE*0.3, align=0.5
end

pro cfad_MTP, usedrops=usedrops, debug=debug
  if ~keyword_set(usedrops) then usedrops = 0
  if ~keyword_set(debug) then debug = 0
  do_dots = 0
  do_cfad = 1 ; set to zero to skip cfad in 1st column.
  do_Vt = 0
  do_cfad_difference = 0 ; I think do_cfad has to be 1 also.
  dr = 500.
  nranges = 1 ; just do one big range if you don't want height-radius plots.
  radii = dr*[lindgen(1,nranges),(lindgen(1,nranges)+1)]; [[0,50],[50,100],[100,150],...,[500,550]]
  ;  radii = radii+400. ; temporary to check one range bin.
  radius_string = string(min(radii),max(radii), format='(I0,"-",I0,"km from center of disturbance")')
  scatterplot=0
  ; The MTP - drops scatterplot is in another program: interpolate_drops_to_MTP.
  ; 1-2-1 filter in the vertical for drops. The range-height plot is messy without a vertical filter..
  if usedrops eq 1 then one2one_filter = 1 else one2one_filter = 0
  
  count_threshold=1
  
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  loadct, 39, /silent
  
  if !D.NAME eq 'X' then !P.CHARSIZE=2.3 else !P.CHARSIZE=1.9
  if !D.NAME eq 'PS' then device, /close, /color, /inches, xsize=6.5, ysize=9, yoffset=1, xoffset=1
  if !D.NAME eq 'PS' then !P.CHARTHICK=2
  if !D.NAME eq 'X' then device, decomposed=0
  
  whichAlt='press_alt' ;
  ; get_levels is in cold_pool_analysis.pro
  requesting_now = get_levels('CFAD_MTP')
  if usedrops eq 1 then requesting_now = reverse(get_levels('m'))
  
  
  yrange=[4000,14000]
  yrange = [5000,20000]
  yrange = [0,20000]
  ytitle = whichAlt+'!Cheight (km)'
  
  ; In Ana Ortiz's study:
  ; The in-cloud and out-of-cloud designations were based on CVI condensed water content.
  ; Many GV probe variables were tested to see which one was the best at predicting whether the
  ; ambient temperature was colder or warmer than the IR cloud top temperature.
  ; Several probe thresholds and ambient-minus-IR thresholds were also tested to find the
  ; combination with the highest skill.
  ; Probe variables included particle count in different size bins, relative humidity, ozone,
  ; and vertical velocity, among others.  50 thresholds spanning the dynamic range of each probe variable
  ; and 15 different ambient-IR thresholds were tested ranging from -1 C to +13 C.
  ; Why did we test different IR-ambient thresholds?  If the actual cloud top is
  ; ragged or fuzzy then the IR temperature may suggest a cloud top height that is too low.
  ; A fuzzy ambient-IR threshold allowed for any bias in IR cloud top height.
  ; The highest skill, in terms of Gilbert skill score (more commonly known as equitable threat score),
  ; was found with CVCWC, using a threshold of 0.07 g/m^3 and a IR-ambient threshold of +4 C.
  ; Therefore, CVCWC >= 0.07 g/m^3 is our best predictor of actual in-cloud conditions.
  ; Changed from 0.0722 to 0.07 on Dec 10, 2013 to appease Reviewer A.
  
  ; copied from procedure do_something_with_IR in compare_gv_probes.pro
  savfile = basedir+'PREDICT/GV/cfad_MTP.sav'
  if file_test(savfile) then restore, savfile else begin
    ; one might wonder why are we loading this if we don't need cvcwc?
    ; Because we need lon to get local solar time.
    ; we don't save lon in microwave_temp_profile_old.pro.
    GV_files = file_search(basedir+'PREDICT/GV/RF*.PNI.sav',count=nGV)
    for iGV=0,nGV-1 do begin
      GV_file = GV_files[iGV]
      restore, GV_file
      GV_juldays = n_elements(GV_juldays) eq 0 ? GVdata.juldays : [GV_juldays, GVdata.juldays]
      cvcwc = n_elements(CvCwC) eq 0 ? GVdata.CvCwC : [CvCwC, GVdata.CvCwC]
    endfor
    save, cvcwc, GV_juldays, filename=savfile
  endelse
  
  dT_cfad_binsize=0.5
  dT_binrange = [-30, 30] ; something ridiculously big to ensure nothing falls outside the range.
  dT_tickinterval=.2
  
  
  ;stages = ['non-developing','developing_gt2days_prior','developing_le2days_prior','developed']
  stages = ['all']
  ;stages = ['all_but_developed']
  ;stages = ['developing_gt2days_prior']
  stages = ['non-developing', 'developing_le2days_prior']
  ;stages = ['non-developing']
  ND_PGIs = 'PGI' + ['27', '27', '30','38','38','38','38','48','51']+'L20100'+['817_12','818_15','821_12','902_18','903_18','905_18','906_15','930_15','930_15'] + '0000'
  L2_PGIs = 'PGI' + ['36', '44', '44','46','50']+'L20100'+['830_12','913_12','914_18','922_18','927_15'] + '0000'
  ;stages = L2_PGIs
  inoutclouds = ['in cloud', 'out of cloud'] ; based on CVCWC from GV microphysical probe
  IR_categories = ['IR < -60C', '-60C <= IR < -40C','-40C <= IR < -20C','-20C <= IR < 0C', 'IR >= 0C']
  cloud_categories =  ['all']; inoutclouds; ['lt-20C', 'ge-20C']
  time_categories = [[0,24]]
  ;time_categories = [[6,8],[8,10],[10,12],[12,14]]
  ;time_categories = [[6,7],[7,8],[8,9],[9,10],[10,11],[11,12],[12,14]]
  
  
  !P.MULTI = [0,n_elements(cloud_categories)*(do_cfad+do_dots+(nranges gt 1)+do_Vt),n_elements(stages)*n_elements(time_categories)/2.]
  if !P.MULTI[2] gt 4 then !P.MULTI[2] = 4
  if !D.NAME eq 'PS' then if !P.MULTI[2] lt 3 then !P.MULTI[2] = 3
  if array_equal(cloud_categories, IR_categories) then !P.MULTI = [0,2,5]
  
  if scatterplot then !P.MULTI=[0,2,3]
  
  for istage = 0, n_elements(stages)-1 do begin
    stage = stages[istage]
    
    median_profile_for_all_times = replicate(!VALUES.F_NAN, n_elements(requesting_now), n_elements(time_categories)/2.)
    
    for itime_category = 0, n_elements(time_categories)/2-1 do begin
      time_range = time_categories[*,itime_category]
      time_range_string = string(time_range,'(I0,"-",I0,"LST")')
      
      
      for icloud_category = 0, n_elements(cloud_categories)-1 do begin
        cloud_category = cloud_categories[icloud_category]
        
        count_dTs_range_hgt = replicate(0.,nranges,n_elements(requesting_now))
        count_UVs_range_hgt = replicate(0.,nranges,n_elements(requesting_now))
        total_dT = replicate(0., nranges, n_elements(requesting_now))
        total_Vt = replicate(0., nranges, n_elements(requesting_now))
        total_Vr = replicate(0., nranges, n_elements(requesting_now))
        cfad = lonarr(1+round(dT_binrange[1]-dT_binrange[0])/dT_cfad_binsize, n_elements(requesting_now))
        maxpts = 18000
        dTs  = replicate(!VALUES.F_NAN, maxpts, n_elements(requesting_now)); used to calculate median at each height.
        Rs   = replicate(!VALUES.F_NAN, maxpts, n_elements(requesting_now)); used to calculate median at each height and scatterplots of dT vs R
        cumulative_alts =  replicate(!VALUES.F_NAN, maxpts)
        mtplats = replicate(!VALUES.F_NAN, maxpts) ; used to plot MTP locations
        mtplons = replicate(!VALUES.F_NAN, maxpts) ; used to plot MTP locations
        xdots=!VALUES.F_NAN & ydots=!VALUES.F_NAN & zdots=!VALUES.F_NAN
        
        title = stage+' '+cloud_category + ' ' + time_range_string + ' min ' + strtrim(count_threshold,2) + ' pts'
        ; xtitle made more generic (used to say "...from mission avg" but not when it is MTP-dropsondes)
        ; start CFAD plot . for whole numbers use xtickformat='(I+0)'
        if do_cfad then plot, [0],[0], xrange=[-1.2,1.2], yrange=yrange, ystyle=1, ytitle=ytitle, $
          xtitle='temp deviation (!Z(00B0)C)', xtickformat='(F+4.1)', xstyle=1, title=title, $
          ytickformat='divide_by_1000', subtitle=radius_string, xticklen=1, yticklen=1, $
          xtickinterval=dT_tickinterval, xminor=dT_tickinterval/dT_cfad_binsize
          
        t = get_PREDICT_missions(count=nmissions)
        ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
        for imission= 0, nmissions-2 do begin
        
          pgi = t.pgi[imission]
          date = t.yyyymmdd[imission]
          hour = strmid(t.hhmm[imission],0,2)
          if is_stage(basedir+'PREDICT/MTP/', date, hour, stage, pgi=pgi) eq 0 then continue
          mission = pgi + date + '_' + t.hhmm[imission] + '00_' + whichAlt
          path = basedir+'PREDICT/MTP/'+(usedrops?'usedrops/':'')
          savfile = path + 'savfiles/' + mission + '.sav'
          restore, savfile
          
          if usedrops eq 0 && (MRI_cutoff lt 0 || MRI_cutoff gt 2) then stop ; sanity check
          ;mission_avg_alts = interpol_NAN(mission_avg_Ts, alt_reqs, requesting_now) ; maybe delete
          
          
          most_nqualifying_dTs = 0
          isubset_of_alts=-1L
          for ialt=0,n_elements(alt_reqs)-1 do begin
            alt = alt_reqs[ialt]
            irequested_now = where(alt eq requesting_now, nrequested_now)
            if nrequested_now eq 0 then continue
            isubset_of_alts = isubset_of_alts+1
            ; We used to subtract the mission average here, but now microwave_temp_profile.pro produces a
            ; variable called time_series_dTs.
            ; time_series_dTs may be MTP minus dropsonde (time_series_dTs) or deviation from mission average
            dT = time_series_dTs[*,ialt]
            Vt = (time_series_V[*,ialt]*time_series_x_km[*,ialt] - time_series_U[*,ialt]*time_series_y_km[*,ialt])/dist2center[*,ialt]
            Vr = (time_series_U[*,ialt]*time_series_x_km[*,ialt] + time_series_V[*,ialt]*time_series_y_km[*,ialt])/dist2center[*,ialt]
            if max(abs(dT),/nan) gt max(dT_binrange) then stop
            
            ; Used to have this outside ialt loop, but for usedrops=1,
            ; you need to redefine for each height and its independent time_series of times.
            ; time_series_MTPTimes must be redefined to the appropriate height of time_series_dropTimes
            time_series_generic = time_series_MTPTimes
            if usedrops eq 1 then time_series_generic = time_series_dropTimes[*,ialt]
            GV_savfile_interp = strmid(savfile,0,strlen(savfile)-4) + (usedrops eq 0 ?'':string(alt,format='("_",I5.5)')+'m')+'_interp.sav'
            if file_test(GV_savfile_interp) then restore, GV_savfile_interp else begin
              cvcwc_interp = interpol_NAN(cvcwc,GV_juldays,time_series_generic)
              this_lat = interpol_NAN(lat,GV_juldays,time_series_generic)
              this_lon = interpol_NAN(lon,GV_juldays,time_series_generic)
              GV_temp  = interpol_NAN(temp, GV_juldays, time_series_generic)
              save, cvcwc_interp, this_lat, this_lon, GV_temp, filename=GV_savfile_interp
            endelse
            
            
            if cloud_category eq 'all' then correct_cloud_category = replicate(1, n_elements(time_series_LST)) else begin
              if cloud_category eq "in cloud" || cloud_category eq "out of cloud" then begin
                if cloud_category eq "in cloud" then correct_cloud_category = cvcwc_interp ge 0.07
                ; used to also require GVtemp+4<time_series_IR restriction for "out of cloud" for some reason.
                if cloud_category eq "out of cloud" then correct_cloud_category = cvcwc_interp lt 0.07
              endif else if usedrops then correct_cloud_category = IR_range(time_series_IR[*,ialt], cloud_category)
            endelse
            
            correct_time_category = time_series_LST ge time_range[0] and time_series_LST lt time_range[1]
            if time_range[0] eq 0 && time_range[1] eq 24 then correct_time_category = replicate(1,n_elements(time_series_LST))
            
            
            ; This Karl mission has a strange histogram. There are still questionable points with MRI<0.42 (the original threshold)
            ;            if mission eq 'PGI44L20100914_180000_'+whichAlt then MRI_cutoff = 0.37
            for iradius = 0, n_elements(radii)/2-1 do begin
            
              qualifying = $
                correct_cloud_category $
                and dist2center[*,ialt] ge radii[0,iradius] $
                and dist2center[*,ialt] lt radii[1,iradius] $
                and time_series_MRI lt MRI_cutoff $
                and correct_time_category
                
              ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
              ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
              if (pgi eq 'PGI48L' or pgi eq 'PGI51L') then qualifying = qualifying and PGI48LPGI51L(pgi,time_series_MTPTimes) eq pgi
              
              
              qualifying_dTs = where(qualifying and finite(dT), nqualifying_dTs)
              qualifying_UVs = where(qualifying and finite(Vr) and finite(Vt), nqualifying_UVs)
              
              if nqualifying_dTs gt most_nqualifying_dTs then most_nqualifying_dTs = nqualifying_dTs
              count_dTs_range_hgt[iradius,isubset_of_alts] = count_dTs_range_hgt[iradius,isubset_of_alts]+nqualifying_dTs
              count_UVs_range_hgt[iradius,isubset_of_alts] = count_UVs_range_hgt[iradius,isubset_of_alts]+nqualifying_UVs
              ; print mission for this development stage and cloud category.
              mission_color = (mycolors(indgen(nmissions)))[imission]
              if debug && do_cfad && isubset_of_alts eq n_elements(requesting_now)-1 then xyouts, +7, 19600-imission*380., string(most_nqualifying_dTs,mission,format='("(",I0,") ",A)'),$
                charsize=1, charthick=1, color=mission_color, align=1
              if nqualifying_dTs gt 0 then begin
                if debug && do_cfad then xyouts, dT[qualifying_dTs], replicate(alt,nqualifying_dTs), string(iradius, format='(I0)'), $
                  color = mission_color, align=0.5, charsize=!P.CHARSIZE/mean(!P.MULTI[1:2])
                  
                cfad[*,isubset_of_alts] = cfad[*,isubset_of_alts] + histogram(dT[qualifying_dTs], locations=xval, binsize=dT_cfad_binsize,min=dT_binrange[0], max=dT_binrange[1])
                
                total_dT[iradius,isubset_of_alts] = total_dT[iradius,isubset_of_alts] + total(dT[qualifying_dTs],/NAN)
                
                ; dTs is only used to plot the median dT for the CFAD plots and for scatterplots at constant height
                first_el = min(where(finite(dTs[*,isubset_of_alts]) eq 0)) ; dTs starts as a bunch of NaNs at each level. replace with dTs from left-to-right.
                dTs[first_el:first_el+nqualifying_dTs-1,isubset_of_alts] = dT[qualifying_dTs]
                Rs[first_el:first_el+nqualifying_dTs-1,isubset_of_alts] = dist2center[qualifying_dTs,isubset_of_alts]
                mtplats[first_el:first_el+nqualifying_dTs-1]         = this_lat[qualifying_dTs]
                mtplons[first_el:first_el+nqualifying_dTs-1]         = this_lon[qualifying_dTs]
                cumulative_alts[first_el:first_el+nqualifying_dTs-1] = time_series_GV_Press_Alt_km[qualifying_dTs]
                
                if debug && do_cfad then xyouts, !X.CRANGE[1], requesting_now[isubset_of_alts], string(mean(dT[qualifying_dTs]),format='(F0)'), charsize=1, charthick=1, align=1
              endif
              if nqualifying_UVs gt 0 then begin
                total_Vt[iradius,isubset_of_alts] = total_Vt[iradius,isubset_of_alts] + total(Vt[qualifying_UVs],/NAN)
                total_Vr[iradius,isubset_of_alts] = total_Vr[iradius,isubset_of_alts] + total(Vr[qualifying_UVs],/NAN)
              endif
              if total(count_dTs_range_hgt[*,isubset_of_alts]) ne total(cfad[*,isubset_of_alts]) then stop
              
            endfor ; radius range
            
          endfor; height
          
          if do_dots then begin
            ; Define zdots
            ; dots of 16-km minus 12-km dT. or just 12-km dT
            hikm = 16000
            lokm = 16000
            ihi = (where(requesting_now eq hikm, n))[0]; we don't want this to considered an array, just a scalar. array messes up subtraction below
            if n eq 0 then stop
            ilo = (where(requesting_now eq lokm, n))[0]
            if n eq 0 then stop
            ; subtract the mean MTP mission temperature at each level
            ; z = time_series_dTs[*,ihi] - time_series_dTs[*,ilo]
            z = time_series_dTs[*,ilo] & zdot_string = '16km T anomaly'
            dots_qualifying_dTs = where(correct_cloud_category and finite(z) and time_series_MRI lt MRI_cutoff $
              and correct_time_category, ndots_qual_ele)
            if ndots_qual_ele gt 0 then begin
              ;            print, alt, mean(time_series_LST[dots_qualifying_dTs])
              xdots = [xdots, time_series_x_km[dots_qualifying_dTs]]
              ydots = [ydots, time_series_y_km[dots_qualifying_dTs]]
              zdots = [zdots, z[dots_qualifying_dTs]]
            endif
          endif
        endfor; mission
        
        ; plot time series (not really time, just valid points all taped together) for decorrelation time
        ; assume time dt spacing of 20 seconds
        ; sometimes referred to as e-folding distance
        if (0) then begin
          for ialt=0,n_elements(alt_reqs)-1 do begin
            alt = alt_reqs[ialt]
            lag = indgen(200)
            nlag=n_elements(lag)
            ngood = total(finite(dts[*,ialt]))
            if ngood eq 0 then continue
            t = dts[0:ngood-1,ialt]
            r = a_correlate(t,lag)
            ; autoregressive model order 2 or AR(2) model
            ; see pages 357-359 of Wilks 2nd edition for this.
            phi = fltarr(nlag)
            ar2 = fltarr(nlag)
            phi[1] = r[1]*(1.-r[2])/(1.-r[1]^2.)
            phi[2] = (r[2]-r[1]^2.)/(1.-r[1]^2.)
            ar2[0]   = 1
            ar2[1]   = phi[1]/(1.-phi[2])
            ar2[2]   = phi[2]+phi[1]^2./(1.-phi[2])
            for m=3,nlag-1 do for k=1,2 do ar2[m] = ar2[m] + phi[k]*ar2[m-k]
            decorrelation_distance = lag[min(where(r lt exp(-1)))]
            plot, t, xrange=[0,ngood], xstyle=1, title=string(alt, format='(I0,"m")'), ytitle='dT', xtitle='data point'
            plot, r, title = string(alt,decorrelation_distance, format='(I0, "m decorrelation distance:",I0)'), ytitle='autocorrelation', xtitle='lag'
            ; autoregressive model order 1
            oplot, r[1]^indgen(nlag)
            oplot, ar2, linestyle=2
            oplot, !X.CRANGE, replicate(exp(-1),2), linestyle=2
            oplot, !X.CRANGE, [0,0], thick=2
            oplot, replicate(decorrelation_distance,2), !Y.CRANGE
          endfor
        endif
        
        
        if do_cfad then begin
          levels = ['0.1','0.5','1','2','5','10','15','20','25','40']
          
          cfad = float(cfad)
          dTs_stddev     = replicate(!VALUES.F_NAN, n_elements(requesting_now))
          cfad_mean      = replicate(!VALUES.F_NAN, n_elements(requesting_now))
          cfad_CI        = replicate(!VALUES.F_NAN, n_elements(requesting_now))
          confidence_level=0.95
          for isubset_of_alts=0,n_elements(requesting_now)-1 do begin
            ; Print count at each level for this development stage and cloud category.
            count_hgt = total(count_dTs_range_hgt[*,isubset_of_alts])
            if debug && do_cfad then xyouts, !X.CRANGE[0]+0.1, requesting_now[isubset_of_alts], string(count_hgt,format='(I0)'), charsize=!P.CHARSIZE*0.34, charthick=1, align=0, noclip=debug
            if count_hgt ne total(cfad[*,isubset_of_alts]) then stop
            if count_hgt gt 1 then dTs_stddev[isubset_of_alts] = stddev(dTs[*,isubset_of_alts],/nan)
            ifinite_dTs = where(finite(dTs[*,0]),nfinite)
            if count_hgt gt 0 then begin
              cfad[*,isubset_of_alts] = 100.*cfad[*,isubset_of_alts]/float(count_hgt)
              cfad_mean[isubset_of_alts] = mean(dTs[*,isubset_of_alts], /nan)
              ; get degrees of freedom (N_indep).
              ; about 12 minutes between drops, or 36 20-sec MTP cycles. divide sample count by 36 to get degrees of freedom (N_indep)
              ; even for dropsondes, because it is mapped to MTP sampling frequency
              N_indep = count_hgt/36
              if n_indep-1 gt 0 then begin
                t_cvf = t_cvf((1-confidence_level)/2.,N_indep-1)
                cfad_CI[isubset_of_alts] = t_cvf*dTs_stddev[isubset_of_alts]/sqrt(N_indep)
              endif
            endif
          endfor
          
          
          cfad_median = median(dTs,dimension=1,/even) ; if npts is even (no middle) use average.
          ; subtract 2nd cloud category from 1st (e.g. subtract out-of-cloud from in-cloud profile)
          median_profile_for_all_times[*,itime_category] = icloud_category eq 0 ? cfad_median : median_profile_for_all_times[*,itime_category] - cfad_median
          contour, cfad, xval+dT_cfad_binsize/2., requesting_now, /overplot, levels=levels, /fill, c_colors=mycolors(levels)
          ;          contour, cfad, xval+dT_cfad_binsize/2., requesting_now, /overplot, levels=levels, /follow, thick=!P.THICK*0.4, $
          ;            c_charsize=!P.CHARSIZE*0.3, c_charthick=!P.CHARTHICK*0.3, c_annotation=levels+'%'
          ;          oplot, cfad_median, requesting_now, color=255, thick=5, linestyle=2
          oplot, cfad_mean, requesting_now, color=14, thick=7
          oplot, replicate(0,2), !Y.CRANGE, thick=!P.thick*3
          oplot, cfad_mean-cfad_CI, requesting_now, color=25, thick=4.1, linestyle=1
          oplot, cfad_mean+cfad_CI, requesting_now, color=25, thick=4.1, linestyle=1
          contourbar, levels, mycolors(levels), /vert, title='relative freq. (%), bin size='+string(dT_cfad_binsize,format='(F4.2)')
          xyouts, 0, !Y.CRANGE[1]+30, string(confidence_level*100,format='(I2,"% confidence interval")'), charsize=0.9, align=0.5
          
          print, title, mean(cumulative_alts, /nan)
        endif ; do_cfad
        
        ; plot dots (acutally filled circles) of 16 minus 12-km temperature.
        if do_dots then begin
          start_dot_plot, 0, 0, title + ' ' + zdot_string
          dots, zdots, xdots, ydots, '!Z(00B0)C',levels=[-3,-2,-1,0,1,2,3]
        endif
        
        igood = where(count_dTs_range_hgt ge count_threshold, ngood)
        mean_dT = replicate(!VALUES.F_NAN,nranges,n_elements(requesting_now))
        mean_Vt = replicate(!VALUES.F_NAN,nranges,n_elements(requesting_now))
        mean_Vr = replicate(!VALUES.F_NAN,nranges,n_elements(requesting_now))
        if ngood gt 0 then begin
          mean_dT[igood] = total_dT[igood]/count_dTs_range_hgt[igood]
        endif
        igood = where(count_UVs_range_hgt ge count_threshold, ngood)
        if ngood gt 0 then begin
          mean_Vt[igood] = total_Vt[igood]/count_UVs_range_hgt[igood]
          mean_Vr[igood] = total_Vr[igood]/count_UVs_range_hgt[igood]
        endif
        
        
        
        ; ==== Range-height diagram ====
        
        if nranges gt 1 then begin
          xrange=[min(radii),max(radii)-dr/2.]
          levels = [-0.8,-0.6,-0.4,-0.2,0,.2,.4,.6,.8,1,1.2,1.4]
          !P.TICKLEN=1
          ; 1-2-1 filter in the vertical
          if one2one_filter then begin
            mean_dT = convol(mean_dT, [[1],[2],[1]], /normalize, /nan, /edge_truncate, center=1)
            title = title + ' 1-2-1 filter'
          endif
          contour, mean_dT, total(radii,1)/2., requesting_now, ytickformat='divide_by_1000', xrange=xrange, title = 'dT'+title , $
            xstyle=1, xtitle='km from center', ytitle=ytitle, ystyle=1, levels=levels, /cell_fill, yrange=yrange, $
            /nodata
          ;          contour, mean_dT, total(radii,1)/2., requesting_now, levels=levels, /follow, /overplot, c_charsize=!P.CHARSIZE*1
          tvlct, old_ct, /get
          load_act, 'div_blue2darkred_86'
          contour2blocks, mean_dT, total(radii,1)/2., requesting_now,range=[-0.8,1]
          tvlct, old_ct
          write_range_height, path + 'block_mean_dT_'+strcompress(title,/remove_all)+whichAlt+'.txt', count_dTs_range_hgt, mean_dT, radii, requesting_now, whichAlt, debug=debug
          if debug then xyouts, rebin([total(radii,1)/2.], nranges, n_elements(requesting_now)), rebin(transpose(long(requesting_now)),nranges, n_elements(requesting_now)), $
            string(count_dTs_range_hgt,format='(I0)'), align=0.5, charsize=0.8, noclip=0, color=0
            
          if do_Vt then begin
            levels = [-10,-7.5,-5,-2.5,0,2.5,5,7.5,10]
            contour, mean_Vt, total(radii,1)/2., requesting_now, ytickformat='divide_by_1000', xrange=xrange, title = 'V!DT!N '+title , $
              xstyle=1, xtitle='km from center', ytitle=ytitle, ystyle=1, levels=levels, /cell_fill, yrange=yrange, /nodata
            ;            contour, mean_Vt, total(radii,1)/2., requesting_now, levels=levels, /follow, /overplot, c_charsize=!P.CHARSIZE*1, $
            ;              c_label = replicate(1,n_elements(levels))
            contour2blocks, mean_Vt, total(radii,1)/2., requesting_now, range=[-12,15]
            write_range_height, path + 'block_mean_Vt_'+strcompress(title,/remove_all)+whichAlt+'.txt', mean_Vt, radii, requesting_now, debug=debug
            if debug then xyouts, rebin([total(radii,1)/2.], nranges, n_elements(requesting_now)), rebin(transpose(long(requesting_now)),nranges, n_elements(requesting_now)), $
              string(count_UVs_range_hgt,format='(I0)'), align=0.5, charsize=0.8, noclip=0, color=0
          endif
          
        endif
        ; scatterplots of dTs vs range (Rs) for each altitude
        if scatterplot then begin
          print, stage
          print, '    alt       slope±1σ (x1000)     r^2(x100)'
          for ialt=0,n_elements(alt_reqs)-1 do begin
            alt = alt_reqs[ialt]
            x = Rs[*,ialt]
            y = dTs[*,ialt]
            x = x[where(finite(y))]
            y = y[where(finite(y))]
            block = 1.
            x = congrid(smooth(x,block), round(n_elements(x)/block))
            y = congrid(smooth(y,block), round(n_elements(y)/block))
            slope = regress(x, y, ftest=ftest, sigma=sigma, const=const, correlation=correlation) ; y = intercept + (slope)x
            print, alt, 1000*slope, 1000*sigma, 100.*correlation^2., const, format='(I5, F13.4, "±", F5.3, F14.5 , F12.8)'
            title = string(stage,block, alt/1000,format='(A," ",I0,"pt block-avg. hgt=",F5.2,"km!C")') + string("T'=",const,slope,"*km ",format='(A,F6.3,F+10.7,A)')
            plot, x, y, title = title, subtitle = subtitle, psym=3,xtitle='km',ytitle="T'", xrange=xrange, /xstyle, yrange=[-4,4]
            oplot, !X.CRANGE, const + slope[0]*!X.CRANGE, thick=5
          endfor
        endif
        
        
      endfor ; cloud category
    endfor ; time (day/night) category
    
    if do_cfad && do_cfad_difference then begin
      plot, [0],[0], xrange=[-1.2,1.2], yrange=yrange, ystyle=1, $
        ytitle=ytitle, xtitle='temp deviation from mission avg difference (!Z(00B0)C)', xtickformat='(F+4.1)', xstyle=1, $
        title =stage+' '+cloud_categories[0]+' minus '+cloud_categories[1], ytickformat='divide_by_1000', subtitle=radius_string
      oplot, [0,0], !Y.CRANGE
      colors = mycolors(time_categories[0,*])
      for itime_category = 0, n_elements(time_categories)/2-1 do begin
        time_range = time_categories[*,itime_category]
        time_range_string = string(time_range,'(I0,"-",I0,"LST")')
        color = colors[itime_category]
        oplot, median_profile_for_all_times[*,itime_category], requesting_now, color=color, thick=5
        xyouts, !X.CRANGE[1], !Y.CRANGE[1] - 1600*(1+itime_category), time_range_string, color=color, charsize=0.9,charthick=3
      endfor
    endif
    
    if (0) then begin
      mtp_locales, lon, lat ; plot locations of all GV probe measurements
      mtp_locales, mtplons, mtplats, color=212 ; plot locations of qualifying MTP measurements
    endif
    
    if (0) then begin
      for z=21,23 do begin
        t = dts[*,z]
        t = t[where(finite(t))]
        minh = -5
        maxh = 6
        plot, xlocs, histogram(t, locations=xlocs, min=minh, max=maxh), title=title+string(alt_reqs[z],format='(I6,"m ")')+radius_string, psym=10, xrange=[minh,maxh]
      endfor
    endif
    
  endfor ; development stage
  ptimestamp
  
  if !D.NAME eq 'PS' then device, /close
  !P.CHARTHICK=1
  !P.CHARSIZE=1
  
end

pro see_what_GV_time_lag_T_is
  basedir = '/Volumes/pecan2/ahijevyc/'
  GV_files = file_search(basedir+'PREDICT/GV/RF*.PNI.sav',count=nGV)
  for iGV=0,nGV-1 do begin
    GV_file = GV_files[iGV]
    restore, GV_file
    GV_juldays = n_elements(GV_juldays) eq 0 ? GVdata.juldays : [GV_juldays, GVdata.juldays]
    alt = n_elements(alt) eq 0 ? GVdata.alt : [alt, GVdata.alt]
    atx = n_elements(atx) eq 0 ? GVdata.atx : [atx, GVdata.atx]
  endfor
  
  stop
end


pro for_fuqing
  t = get_PREDICT_missions(count=nmissions)
  nmissions = nmissions-1 ; because last mission is g4 for Tom (hurricane, not even tropical storm)
  
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  
  stage = 'all'
  whichAlt = 'press_alt'
  if whichAlt eq 'press_alt' then whichAlt_units = 'm'
  
  ncfile = basedir+'PREDICT/MTP/for_fuqing_'+stage+'_'+whichAlt+'.nc'
  ncid = ncdf_create(ncfile, /clobber)
  
  mission_dimid = ncdf_dimdef(ncid, 'missions', /unlimited)
  strlen_dimid  = ncdf_dimdef(ncid, 'strlen', 128)
  alt_reqs_dimid = ncdf_dimdef(ncid, 'alt_reqs', 57)
  cycle_dimid = ncdf_dimdef(ncid, 'cycle', 1700)
  
  alt_reqs_id       = ncdf_vardef(ncid, whichAlt, [alt_reqs_dimid], /FLOAT)
  dist2center_id    = ncdf_vardef(ncid, 'dist2center', [cycle_dimid, alt_reqs_dimid, mission_dimid], /DOUBLE)
  good_mtp_id       = ncdf_vardef(ncid, 'good_mtp', [cycle_dimid, mission_dimid], /byte)
  mission_id        = ncdf_vardef(ncid, 'mission', [strlen_dimid, mission_dimid], /CHAR)
  mission_avg_Ts_id = ncdf_vardef(ncid, 'mission_avg_Ts', [alt_reqs_dimid, mission_dimid], /FLOAT)
  mri_cutoff_id     = ncdf_vardef(ncid, 'MRI_cutoff', [mission_dimid], /FLOAT)
  cold_pt_id        = ncdf_vardef(ncid, 'cold_pt', [cycle_dimid, mission_dimid], /FLOAT)
  dTs_id            = ncdf_vardef(ncid, 'dTs', [cycle_dimid, alt_reqs_dimid, mission_dimid], /FLOAT)
  GV_press_alt_km_id= ncdf_vardef(ncid, 'GV_press_alt_km', [cycle_dimid, mission_dimid], /FLOAT)
  GV_temp_id        = ncdf_vardef(ncid, 'GV_temp', [cycle_dimid, mission_dimid], /FLOAT)
  LST_id            = ncdf_vardef(ncid, 'LST', [cycle_dimid, mission_dimid], /DOUBLE)
  MRI_id            = ncdf_vardef(ncid, 'MRI', [cycle_dimid, mission_dimid], /FLOAT)
  MTP_times_id      = ncdf_vardef(ncid, 'MTP_times', [cycle_dimid, mission_dimid], /DOUBLE)
  MTP_Ts_id         = ncdf_vardef(ncid, 'MTP_Ts', [cycle_dimid, alt_reqs_dimid, mission_dimid], /FLOAT)
  SETemp_id         = ncdf_vardef(ncid, 'SETemp', [cycle_dimid, alt_reqs_dimid, mission_dimid], /FLOAT)
  UTsec_id          = ncdf_vardef(ncid, 'UTsec', [cycle_dimid, mission_dimid], /DOUBLE)
  x_km_id           = ncdf_vardef(ncid, 'x_km', [cycle_dimid, alt_reqs_dimid, mission_dimid], /DOUBLE)
  y_km_id           = ncdf_vardef(ncid, 'y_km', [cycle_dimid, alt_reqs_dimid, mission_dimid], /DOUBLE)
  zcold_pt_id       = ncdf_vardef(ncid, 'zcold_pt', [cycle_dimid, mission_dimid], /FLOAT)
  
  ncdf_attput, ncid, alt_reqs_id, 'units', whichAlt_units
  ncdf_attput, ncid, dist2center_id, 'long_name', 'distance to center of disturbance'
  ncdf_attput, ncid, dist2center_id, 'units', 'km'
  ncdf_attput, ncid, good_mtp_id, 'long_name', '1=passes QC test'
  ncdf_attput, ncid, mission_avg_Ts_id, 'long_name', 'mission-average temperature'
  ncdf_attput, ncid, MRI_cutoff_id, 'long_name', 'MTP cycle passes QC test if MRI is less than this'
  ncdf_attput, ncid, cold_pt_id, 'long_name', 'temperature of cold point from MTP'
  ncdf_attput, ncid, cold_pt_id, 'units', 'C'
  ncdf_attput, ncid, dTs_id, 'long_name', 'retrieved air temperature minus mission average'
  ncdf_attput, ncid, dTs_id, 'units', 'C'
  ncdf_attput, ncid, GV_press_alt_km_id, 'long_name', 'pressure altitude of aircraft'
  ncdf_attput, ncid, GV_press_alt_km_id, 'units', 'km'
  ncdf_attput, ncid, GV_temp_id, 'long_name', 'horizon brightness temperature (ie, OAT, similar to SAT); avg ch1 & ch2 & ch3'
  ncdf_attput, ncid, GV_temp_id, 'units', 'K'
  ncdf_attput, ncid, LST_id, 'long_name', 'local solar hour (hour UTC + lon/15)'
  ncdf_attput, ncid, LST_id, 'units', 'h'
  ncdf_attput, ncid, MRI_id, 'long_name', 'retrieval quality metric ranges 0-2, <1 is excellent'
  ncdf_attput, ncid, MTP_times_id, 'long_name', 'Julian Date of MTP cycle'
  ncdf_attput, ncid, MTP_times_id, 'units', 'days elapsed since Greenwich mean noon on 1 January 4713 BCE'
  ncdf_attput, ncid, MTP_Ts_id, 'long_name', 'MTP temperature'
  ncdf_attput, ncid, MTP_Ts_id, 'units', 'K'
  ncdf_attput, ncid, SETemp_id, 'long_name', 'standard error of retrieved air temperature from MTP'
  ncdf_attput, ncid, UTsec_id, 'long_name', 'elapsed UT seconds from 0 hours on date'
  ncdf_attput, ncid, x_km_id, 'long_name', 'x distance from center of disturbance'
  ncdf_attput, ncid, x_km_id, 'units', 'km'
  ncdf_attput, ncid, y_km_id, 'long_name', 'y distance from center of disturbance'
  ncdf_attput, ncid, y_km_id, 'units', 'km'
  ncdf_attput, ncid, zcold_pt_id, 'long_name', 'height of cold point in microwave temperature profile'
  ncdf_attput, ncid, zcold_pt_id, 'units', 'km'
  ncdf_control, ncid, /endef
  for imission= 0, nmissions-1 do begin
  
    pgi = t.pgi[imission]
    date = t.yyyymmdd[imission]
    hour = strmid(t.hhmm[imission],0,2)
    if is_stage(basedir+'PREDICT/MTP/', date, hour, stage, pgi=pgi) eq 0 then continue
    mission = pgi + date + '_' + t.hhmm[imission] + '00_' + whichAlt
    path = basedir+'PREDICT/MTP/'
    savfile = path + 'savfiles/' + mission + '.sav'
    restore, savfile
    if n_elements(old_alt_reqs) eq 0 then begin
      ncdf_varput, ncid, ALT_REQS_id, alt_reqs
    endif else if array_equal(alt_reqs, old_alt_reqs) ne 1 then stop
    old_alt_reqs = alt_reqs
    print, savfile
    
    ncdf_varput, ncid, dist2center_id, DIST2CENTER, offset=[0,0,imission]
    ncdf_varput, ncid, good_MTP_id, GOOD_MTP, offset=[0,imission]
    ncdf_varput, ncid, mission_id, MISSION, offset=[0,imission]
    ncdf_varput, ncid, mission_avg_Ts_id, MISSION_AVG_TS, offset=[0,imission]
    ncdf_varput, ncid, MRI_cutoff_id, MRI_CUTOFF, offset=[imission]
    ncdf_varput, ncid, cold_pt_id, TIME_SERIES_COLD_PT, offset=[0,imission]
    ncdf_varput, ncid, dTs_id, TIME_SERIES_DTS, offset=[0,0,imission]
    ncdf_varput, ncid, GV_press_alt_km_id, TIME_SERIES_GV_PRESS_ALT_KM, offset=[0,imission]
    ncdf_varput, ncid, GV_Temp_id, TIME_SERIES_GV_TEMP, offset=[0,imission]
    ncdf_varput, ncid, LST_id, TIME_SERIES_LST, offset=[0,imission]
    ncdf_varput, ncid, MRI_id, TIME_SERIES_MRI, offset=[0,imission]
    ncdf_varput, ncid, MTP_times_id, TIME_SERIES_MTPTIMES, offset=[0,imission]
    ncdf_varput, ncid, MTP_Ts_id, TIME_SERIES_MTP_TS, offset=[0,0,imission]
    ncdf_varput, ncid, SETemp_id, TIME_SERIES_SETEMP, offset=[0,0,imission]
    ncdf_varput, ncid, UTSec_id, TIME_SERIES_UTSEC, offset=[0,imission]
    ncdf_varput, ncid, x_km_id, TIME_SERIES_X_KM, offset=[0,0,imission]
    ncdf_varput, ncid, y_km_id, TIME_SERIES_Y_KM, offset=[0,0,imission]
    ncdf_varput, ncid, zcold_pt_id, TIME_SERIES_ZCOLD_PT, offset=[0,imission]
    
  endfor
  
  ncdf_close, ncid
  
end



;pro profile_curtain_by_radius
;  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/'
;  subpath='MTP/'
;  loadct, 39, /silent
;  if !D.NAME eq 'X' then device, decomposed=0
;  if !D.NAME eq 'X' then !P.CHARSIZE=2 else !P.CHARSIZE=1
;  !P.THICK=1
;  psfile = basedir+subpath+'curtain.ps'
;  MTP_AltType = '_GPSAlt'
;  if !D.NAME eq 'PS' then device, /close, ysize=5.5, xsize=6.5, /inches, filename=psfile
;  stages = ['non-developing','developing_gt2days_prior','developing_le2days_prior','developed']
;  stages = ['non-developing']
;  cloud_categories = ['lt-20C', 'ge-20C'];inoutclouds;['lt-20C', 'ge-20C']
;
;  !P.MULTI = [0,n_elements(cloud_categories),n_elements(stages)]
;  xcharsize=0.85
;
;  for istage = 0, n_elements(stages)-1 do begin
;    stage = stages[istage]
;    for icloud_category = 0, n_elements(cloud_categories)-1 do begin
;      cloud_category = cloud_categories[icloud_category]
;
;
;      files = file_search(basedir+subpath+'/'+stage+ MTP_AltType+'_?????m_IR10km_scatter.txt', count=nheights)
;      if nheights eq 0 then continue
;      heights = replicate(!VALUES.F_NAN, nheights)
;      height_ipos = strpos(file_basename(files),+'m_IR10km_scatter')-5
;      if n_elements(uniq(height_ipos)) gt 1 then stop
;      heights = strmid(file_basename(files),height_ipos[0],5)/1000
;      yrange = [min(heights),max(heights)]
;
;      dr = 50
;      rmin = 0
;      rmax = 500
;      nbins = (rmax-rmin)/dr + 1
;      ranges = rmin + lindgen(nbins)*dr
;      sum = replicate(0.,nbins,nheights)
;      n = replicate(0.,nbins,nheights)
;
;      for iAlt = 0, nheights-1 do begin
;        file = files[iAlt]
;        get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, GFS_mean, other=other
;        crud = TempC - other.meanMTP
;        for irange = 0, nbins-1 do begin
;          r1 = irange*dr
;          r2 = r1+dr
;          in = where(distance_km ge r1 and distance_km lt r2 and IR_range(IR, cloud_category), nin)
;          if nin gt 0 then begin
;            sum[irange, iAlt] = sum[irange, iAlt] + total(crud[in],/nan)
;            n[irange,iAlt] = n[irange,iAlt] + nin
;          endif
;        endfor
;
;      endfor
;      title= stage+" "+cloud_category
;      levels = [-0.8,-0.6,-0.4,-0.2,0,.2,.4,.6,.8,1,1.2,1.4]
;      xrange=[rmin,rmax]
;      contour, sum/n, ranges+dr/2., heights, ytickformat='(I0)', xrange=xrange, yrange=yrange, title=title, $
;        xstyle=1, position=pos, xtitle='km from center', ytitle='height (km)!C'+MTP_AltType,ystyle=1, levels=levels, /cell_fill
;      contour, sum/n, ranges+dr/2., heights, levels=levels, /follow, /overplot
;      xyouts, rebin(ranges+dr/2., nbins, nheights), rebin(transpose(long(heights)),nbins, nheights), $
;        string(sum/n,format='(F4.1)')+'!C'+string(n,format='(I0)'), align=0.5, charsize=0.8, noclip=0
;      if !P.MULTI[0] eq 3 then contourbar, levels, mycolors(levels), /vertical, position=[0.54,0.35,0.55,0.68], title='MTP temp deviation from mission average (deg C)'
;    endfor
;
;  endfor
;  !P.MULTI=0
;  ptimestamp, /right
;  if !D.NAME eq 'PS' then begin
;    device, /close
;    print, psfile
;  endif
;  !P.CHARSIZE=1
;
;end
;

;
;pro read_MTP_csv
;  file = basedir+'PREDICT/MTP/MP'+mtpdate+'.csv'
;
;  data = read_ascii(file, header=header, data_start=2, delimiter=',')
;  header_vars = strsplit(header[1], ',', /EXTRACT, COUNT=ncols)
;  if ncols eq 0 then message,'Header contains no columns'
;
;  unit  = replicate('', ncols)
;  miss  = replicate('', ncols)
;  scale = replicate('', ncols)
;  offset= replicate('', ncols)
;  fmt   = replicate('', ncols)
;  for ivar = 0, ncols-1 do begin
;    ipos = strpos(header_vars[ivar],'[')
;    info = strmid(header_vars[ivar],ipos)
;    header_vars[ivar] = strmid(header_vars[ivar],0,ipos)
;    keys = ['unit', 'miss', 'scale', 'offset', 'fmt']
;    for ikey = 0, n_elements(keys)-1 do begin
;      key = keys[ikey]
;      ipos = strpos(info, key)
;      if ipos ne -1 then begin
;        keyvalue = strmid(info, ipos)
;        value = (stregex(keyvalue,'.*=(".*")',/subexp,/extract))[1]
;        result = execute(key+'[ivar]='+value)
;      endif
;    endfor
;
;  endfor
;
;  tmpl = { VERSION: 1.,  $
;    DATASTART: 2L, $
;    DELIMITER: ',',  MISSINGVALUE: !VALUES.F_NAN, $
;    COMMENTSYMBOL:  '', FIELDCOUNT:  ncols, $
;    FIELDTYPES: [7,replicate(4,ncols-1)], $
;    FIELDNAMES: header_vars, $
;    FIELDLOCATIONS: lindgen(ncols), $
;    FIELDGROUPS: lindgen(ncols) }
;  data = read_ascii(file, template=tmpl)
;
;  for i = 0, ncols-1 do if miss[i] ne '' then data.(i) = replace_wnan(data.(i), miss[i])
;  year = strmid(data.time, 0, 4)
;  month = strmid(data.time, 4,2)
;  day = strmid(data.time, 6,2)
;  hour = strmid(data.time, 9,2)
;  minute = strmid(data.time, 11,2)
;  second = strmid(data.time, 13,2)
;  mtp_jday = julday(month, day, year, hour, minute, second)
;
;  irecords = uniq(mtp_jday)
;  nrecords  = n_elements(irecords)
;  cvcwc_interp = interpol_NAN(cvcwc,GV_juldays,mtp_jday[irecords])
;  iend = -1
;  new_mission=1
;  for irecord = 0, nrecords-1 do begin
;    istart = iend+1
;    iend   = irecords[irecord]
;    datestring = strmid(data.time[irecord],0,8)
;    timestring = strmid(data.time[irecord],9,8)
;    get_ir, IR, datestring, timestring, data.latitude[irecord], data.longitude[irecord], closest_imageJulian
;    mtm_center, pgi, datestring, timestring, clat, clon, best_track=best_track, mtm00=1, silent=1
;    if finite(clon) then dist2center = map_2points(clon,clat,data.longitude[irecord],data.latitude[irecord], /meters)/1000.
;
;    if dist2center ge 200 then continue
;
;
;    case cloud_category of
;      "in cloud" : qualifying_points = cvcwc_interp[irecord] ge 0.07
;      "out of cloud" : qualifying_points = cvcwc_interp[irecord] lt 0.07
;    else : if strpos(cloud_category,'IR') ne -1 then qualifying_points = IR_range(IR, cloud_category)
;  endcase
;  if qualifying_points then begin
;    dT = interpol_NAN(data.dtemp_mtp[istart:iend], data.PRESSURE_ALT[istart:iend], alt_reqs)
;    MRI = data.MRI[istart:iend]
;    MRI_cutoff = data.MRI_cutoff[istart:iend]
;    if min(MRI) ne max(MRI) then stop ; sanity check MRI should all be the same.
;    if MRI[0] lt MRI_cutoff[0] then begin
;      alldT = new_mission ? dT : [[alldT],[dT]]
;      new_mission=0
;    endif
;  endif
;endfor
;
;end
;
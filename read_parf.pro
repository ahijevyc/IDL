pro mapparf, x, y, z, color, title
  pos = [0.08, 0.13, 0.88, 0.9]
  limit = [25.5, -93, 39.5, -64]
  usersym, [-0.5, 0.5, 0.5, -0.5], [0.5, 0.5, -0.5, -0.5], /fill
  i = where(color eq 0, nz, complement=igood, ncomplement=ngood)
  map_set, 0, -85, 0, limit=limit, position=pos, /noborder, title=title, charsize=0.7, /cyl
  if ngood gt 0 then plots, x[igood], y[igood], z[igood], psym=8, color=color[igood], noclip=0, symsize=1.4
  map_set, 0, -85, 0, /cont, limit=limit, /usa, position=pos, /noborder, /noerase, /cyl
  ;    plots, [-95.50, -97.50, -80.00, -68.00, -73.00, -95.50], [32.50, 48.00, 46.00, 47.00, 32.50, 32.50], thick=5, noclip=1
  ;plots, [-96.2077, -96.2077, -68.8000, -79.4580, -96.2077], [ 31.8628, 46.6025, 45.5860, 31.8628, 31.8628 ], thick=5, noclip=1
  return
end



pro plot_fig, y, ytitle, field_names, ivip

  sz = size(y)
  ndims = sz[0]
  nx = sz[1]
  if ndims eq 1 then y = rebin(y, [sz[1], 1, 2]) ; make 2D array 3D because we refer to a 3D array later.
  if ndims eq 2 then y = rebin(y, [sz[1:2], 2]); repeat 2D array to make it 3D.
  
  nforecast_types = (size(y))[2]
  ylog = 0
  if ytitle eq 'bias' then ylog = 1
  yrange = [0,max(y[*, *, ivip], /nan)]
  if yrange[1] eq 0 then return
  if ytitle eq 'bias' then yrange = [0.01, 100]
  psym = 0
  if ytitle eq 'count' then begin & ylog = 1 & psym = 10 & yrange[0] = 1 & end
  
  
  
  
  
  plot, indgen(nx), y[*,0,ivip], yrange=yrange, ytitle=ytitle, /nodata, $
    title = + "VIP Level " + strtrim(ivip eq 0 ? 1 : 3, 2) + "+", xstyle=5, ylog=ylog, color=0, thick=2
  axis, xaxis=0, xrange=[0,1]
  for ifcst = 0, nforecast_types-1 do begin
    color = !D.N_colors * ifcst/nforecast_types-1
    y2 = y[*,ifcst,ivip]
    if min(y2) eq max(y2) and min(y2) eq 0 then continue
    oplot, y2, linestyle=ifcst, color=color, thick=3, psym=psym
    ylabel = max(y2, xlabel, /nan)
    if ytitle eq 'bias' then begin
      xlabel = min(where(y2 lt 2))
      ylabel = min([max(y2), 2])
    endif
    xyouts, xlabel, ylabel, field_names[ifcst], align=0.5, charsize=0.6, orientation=ytitle eq 'bias' ? -45 : 0, color=color
  endfor
  
  
end

pro read_header, filename, data_start, ilon, ilat
  openr, lun, filename, /get_lun
  a = ''
  line=0
  ifield = 0
  ilon = -1
  ilat = -1
  while ~ strcmp(a, '@data') do begin
    readf, lun, a
    if strmatch(a, '* Latitude *') then ilat = ifield
    if strmatch(a, '* Longitude *') then ilon = ifield
    if strmatch(a, '@attribute *') || strmatch(a, '@ignored *') then ifield = ifield+1
    line = line+1
  endwhile
  free_lun, lun
  data_start = line
  if ilon eq -1 or ilat eq -1 then stop
end

pro roc, x, y, field_names, ivip

  nforecast_types = (size(y))[2]
  psym = 0
  plot, [0,1], [0,1], yrange=[0,1], ytitle='hit/(hit+miss)', xtitle='fa/(fa+null)', /nodata, $
    title = + "VIP Level " + strtrim(ivip eq 0 ? 1 : 3, 2) + "+", xstyle=1, ystyle=1
    
  for ifcst = 0, nforecast_types-1 do begin
    color = !D.N_colors * ifcst/nforecast_types-1
    y2 = y[*,ifcst,ivip]
    x2 = x[*,ifcst,ivip]
    isort = sort(x2)
    x2 = x2[isort]
    y2 = y2[isort]
    if min(y2) eq max(y2) and min(y2) eq 0 then continue
    oplot, x2, y2, linestyle=ifcst, color=color, thick=3, psym=psym
    ilabel = min(where(x2 gt 0.15 and x2 lt 0.8, n))
    if n eq 0 then ilabel = n_elements(x2) - 2
    xyouts, x2[ilabel], y2[ilabel], field_names[ifcst], align=0.5, charsize=0.6, orientation= 45, color=color
  endfor
end

pro read_parf, hours_to_try=hours_to_try
  if ~keyword_set(hours_to_try) then hours_to_try = '??'
  if !D.NAME eq 'PS' then device, /close, /color, bits_per_pixel=8
  if !D.NAME eq 'X' then device, decomposed=0
  
  debug = 1
  loadct, 39
  close, /all
  openw, lun, 'read_parf.out', /get_lun
  parfdir = '/d3/fieldData/CoSPA/other/FcstPredictorData/RF_LAMP/'
  parfdir = '/mmmtmp/ahijevyc/'
  
  ;  VIPLevel3 = 132.5 digitalVIL and VIPLevel1 = 14.5 digitalVIL
  ; nres fcst thresholds (1-200 votes, 0-255 digitalVIL), 14 possible forecasts, 2 thresholds (1+ and 3+)
  nres = 101.
  nforecast_types = 11
  n_thresholds = 1
  n_probs = 20 ; obs_rel_freq is 1 larger to include the probability of 1.0.
  
  brier_score = fltarr(nforecast_types, n_thresholds)
  brier_score_ref = fltarr(n_thresholds)
  climatological_freq = [0.07, 0.008]
  nobserved_hits  = fltarr(n_probs+1, nforecast_types, n_thresholds)
  fcst_prob_count = fltarr(n_probs+1, nforecast_types, n_thresholds)
  field_names = strarr(nforecast_types)
  hit = fltarr(nres,nforecast_types, n_thresholds)
  fa = fltarr(nres,nforecast_types, n_thresholds)
  miss = fltarr(nres,nforecast_types, n_thresholds)
  null = fltarr(nres,nforecast_types, n_thresholds)
  a = fltarr(nres, nforecast_types)
  b = fltarr(nres, nforecast_types)
  c = fltarr(nres, nforecast_types)
  d = fltarr(nres, nforecast_types)
  
  
  files = file_search(parfdir+'/20??0???.'+hours_to_try+'00.ascii', count = nfiles)
  if !D.NAME eq 'PS' and strmatch(hours_to_try, "[012][0-9]") then device, filename = hours_to_try+'UTCtest7.ps'
  if !D.NAME eq 'PS' and hours_to_try eq '?[02468]' then device, filename = '200907-09evenhrs7.ps'
  
  print, "found "+ strtrim(nfiles, 2)+ " files in " + parfdir
  ;  nfiles = 50
  for ifile = 0, nfiles-1, 1 do begin
  
    filename = files[ifile]
    savfile='t.sav'
    if file_test(savfile) eq 1 then restore, savfile else begin
      print, "reading " + filename + "..." + STRING(ifile+1, nfiles, FORMAT='(I0," out of ", I0)')
      read_header, filename, data_start, ilon, ilat
      t = read_ascii(filename, data_start=data_start, header=header, count = count)
      print, count, ' lines of data'
      save, t, header, data_start, ilon, ilat, count, filename=savfile
    endelse
    ncolumns = (size(t.field01))[1]
    all_field_names = header[1:ncolumns]
    lon = t.field01[ilon, *] ; used to determine "good" points below.
    lat = t.field01[ilat, *] ; used to determine "good" points below.
    
    for iVIP = 0, n_thresholds-1 do begin
      ; iVIP eq 0 VIP 1+
      obs_thresh = iVIP
      ifcst=0; reset the foreacst type index. Increment it at the end of the loop
      for icolumn = 0, ncolumns-1 do begin
        field_name = all_field_names[icolumn]
        field_name = strsplit(field_name, /extract)
        ; Take 2nd word
        field_name = field_name[1]
        if strpos(field_name, 'class01fract') lt 0 then continue
        if field_name eq '"MITLL_VIL60m"' then iMITLL = ifcst
        fcst = t.field01[icolumn,*]
        obs  = t.field01[ncolumns-1,*] ; last column is the obs ; redefine obs here since we filter obs = obs[igood] below, and
        ; we don't want to do this more than once.
        
        ; defined above
        mapparf, lon, lat, fcst, bytscl(fcst, /nan), field_name
        
        fmax = !VALUES.F_NAN
        case field_name of
          '"MITLL_VIL60m"':       fmax = 255.
          '"MITLL_VIL120m"':      fmax = 255.
          ;          '"VIPLevel_1hrago"':    fmax = 7.
          ;          '"VIPLevel_2hrago"':    fmax = 7.
          '"VIPLevel1prob_1hr"':  fmax = 1.
          '"VIPLevel1prob_2hr"':  fmax = 1.
          '"VIPLevel3prob_1hr"':  fmax = 1.
          '"VIPLevel3prob_2hr"':  fmax = 1.
          '"VIPLevel1fract_1hr"': fmax = 1.
          '"VIPLevel1fract_2hr"': fmax = 1.
          '"VIPLevel3fract_1hr"': fmax = 1.
          '"VIPLevel3fract_2hr"': fmax = 1.
          '"LAMP_TSTM1-3h"':      fmax = 100.
          ;          '"LAMP_TSTM3-5h"':      fmax = 100.
          else: break
        endcase
        if not finite(fmax) then continue
        field_names[ifcst] = field_name
        
        
        igood = where(lon gt -94.5 and obs ne -999 and fcst ne -999 and fcst ne -9999 and obs ne -9999, ngood, complement=ibad)
        
        if debug then print, field_name +': found ', ngood, ' good fcst/obs pairs'
        
        if ngood eq 0 then begin
          ; Before, I continued without incrementing ifcst. That was a mistake. ifcst keeps track of the actual forecast index.
          ; Do not continue to the next column without incrementing the actual forecast index, ifcst.
          ; If we made it past the previous continue (after the finite test for fmax) then this is an actual forecast.
          ifcst = ifcst + 1 ; Keep track of the foreacst type index. Increment it now.
          continue
        endif
        
        ; Filter out any bad fcst/obs pairs.  This makes is easier to calculate the Brier scores (with NaN's it is hard).
        fcst = fcst[igood]  ; make sure you redefine fcst each time before you do this.
        obs = obs[igood] ;  make sure you redefine obs each time before you do this.
        
        
        ; normalize the forecast (scale from zero to one)
        fcst = fcst/fmax
        
        for iprob = 0, n_probs do begin
          i_fcst_probability_bin = where(fcst ge float(iprob)/n_probs and fcst lt float(iprob+1)/n_probs, nbin)
          if nbin gt 0 then begin
            fcst_prob_count[iprob, ifcst, iVIP] = fcst_prob_count[iprob, ifcst, iVIP] + nbin
            nobserved_hits[iprob, ifcst, iVIP] = nobserved_hits[iprob, ifcst, iVIP] + total( obs[i_fcst_probability_bin] ge obs_thresh)
          endif
        endfor
        
        ; The boolean (obs ge
        ; obs_thresh) can be treated as 0's
        ; (false) and 1's (true) in the
        ; numeric sense
        brier_score_increment = total((fcst - (obs ge obs_thresh))^2.)/float(ngood)
        brier_score[ifcst, iVIP] = brier_score[ifcst, iVIP] + brier_score_increment
        brier_score_ref_increment = total((climatological_freq[iVIP] - (obs ge obs_thresh))^2.)/float(ngood)
        ; You don't want to keep added to brier_score_ref for each forecast type. Just one will do.
        if ifcst eq 0 then brier_score_ref[iVIP] = brier_score_ref[iVIP] + brier_score_ref_increment
        if debug then print, 'iVIP', iVIP, field_name + ": Brier score incrmt ", brier_score_increment, ' Brier score(ref) increment ', brier_score_ref_increment
        
        for ifcst_thresh = 0, nres-1 do begin
          fcst_thresh = float(ifcst_thresh)/(nres-1)
          dummy = where(obs ge obs_thresh and fcst ge fcst_thresh, n)
          HIT[ ifcst_thresh, ifcst, ivip] = HIT[ ifcst_thresh, ifcst, ivip] + n
          a[ifcst_thresh, ifcst] = n
          dummy = where(obs lt obs_thresh and fcst ge fcst_thresh, n)
          FA[  ifcst_thresh, ifcst, ivip] = FA[  ifcst_thresh, ifcst, ivip] + n
          b[ifcst_thresh, ifcst] = n
          dummy = where(obs ge obs_thresh and fcst lt fcst_thresh, n)
          MISS[ifcst_thresh, ifcst, ivip] = MISS[ifcst_thresh, ifcst, ivip] + n
          c[ifcst_thresh, ifcst] = n
          dummy = where(obs lt obs_thresh and fcst lt fcst_thresh, n)
          NULL[ifcst_thresh, ifcst, ivip] = NULL[ifcst_thresh, ifcst, ivip] + n
          d[ifcst_thresh, ifcst] = n
        endfor
        
        
        ifcst = ifcst + 1 ; Keep track of the foreacst type index. Increment it now.
        
      endfor ; loop thru each column
      if n_elements(iMITLL) eq 0 then begin
        if !D.NAME eq 'PS' then device, /close
        stop
      endif
      
      r = (a + c) * (a + b) / (a+b+c+d)
      gss_tmp = (a-r)/(a+b+c-r)
      ; If another forecast is better than MITFCST060 then plot the chart.
      best_FCST060 = max(gss_tmp[*,iMITLL], /nan)
      if max(gss_tmp, /nan) gt best_FCST060+0.01 then begin
        print, "another fcst beat MITLL in " + filename
        plot_fig, gss_tmp, file_basename(filename) + " gss", field_names, ivip
      endif
      
      
      
    endfor ; loop through each VIP level threshold (1 and 3)
    
    
    
    
    
  endfor ; end file loop
  
  ; This loop should come after the file
  ; loop to get a final tally of the counts.
  
  for ifcst=0, nforecast_types-1 do begin
    for iVIP = 0, n_thresholds-1 do begin
      BSS = 1. - brier_score[ifcst, iVIP]/brier_score_ref[iVIP]
      if debug then print, iVIP, field_names[ifcst] + ' brier_score = ', brier_score[ifcst,iVIP], ' brier_score_ref=', brier_score_ref[iVIP]
      
      if iVIP eq 0 then begin
        plot_fig, fcst_prob_count[*,ifcst,iVIP], 'count', field_names[ifcst], iVIP
        xyouts, indgen(n_probs+1), fcst_prob_count[*, ifcst, iVIP], string(fcst_prob_count[*, ifcst, iVIP], format='(I0)'), align=0.5, color=20, charsize=0.8, font=1
        printf, lun, field_names[ifcst] + " histogram"
        for iprob = 0, n_probs do printf, lun, STRING(float(iprob)/n_probs, float(iprob+1)/n_probs, fcst_prob_count[iprob, ifcst, iVIP], FORMAT='(F4.2, "-", F4.2, ": ", I0)')
        
        
      endif
      
      observed_rel_freq = nobserved_hits[*,ifcst,iVIP]/fcst_prob_count[*, ifcst, iVIP]
      plot_fig, observed_rel_freq, 'observed relative frequency', field_names[ifcst]+ "!CBSS=" + string(BSS), iVIP
      xyouts, indgen(n_probs+1), observed_rel_freq, string(nobserved_hits[*,ifcst,iVIP], format='(I0)') + '/' + string(fcst_prob_count[*, ifcst, iVIP], format='(I0)'), align=0.5, color=20, font=1, charsize=0.8
      printf, lun, field_names[ifcst] + " observed relative frequency  ivip = " + STRTRIM(ivip,2)
      for iprob = 0, n_probs do printf, lun, STRING(float(iprob)/n_probs, float(iprob+1)/n_probs, observed_rel_freq[iprob], FORMAT='(F4.2, "-", F4.2, ": ", F6.4)')
      
    endfor
  endfor
  
  
  
  CSI = HIT / (HIT + MISS + FA)
  total = hit + fa + miss + null
  random_hits = (hit + miss) * (hit + fa) / total
  GSS = (HIT - random_hits )/(hit + fa + miss - random_hits)
  HSS = 2.*(HIT*NULL - FA*MISS) / ((HIT+miss)*(miss+NULL)+(HIT+fa)*(fa+NULL))
  BIAS = (hit + fa) / (hit + miss)
  TSS = (hit*null - fa*miss)/((hit+miss)*(fa+null))
  far = fa / (hit + fa)
  podY = hit / (hit + miss)
  podN = null / (fa + null)
  
  for ivip=0,n_thresholds-1 do begin
  
    plot_fig, hit, 'hits', field_names, ivip
    plot_fig, fa, 'false alarms', field_names, ivip
    plot_fig, miss, 'misses', field_names, ivip
    plot_fig, null, 'correct nulls', field_names, ivip
    plot_fig, random_hits, 'random hits', field_names, ivip
    plot_fig, total, 'total', field_names, ivip
    plot_fig, csi, 'csi', field_names, ivip
    plot_fig, gss, 'gss', field_names, ivip
    plot_fig, bias, 'bias', field_names, ivip
    plot_fig, hss, 'heidke skill score', field_names, ivip
    plot_fig, tss, 'true skill statistic', field_names, ivip
    plot_fig, far, 'false alarm ratio', field_names, ivip
    plot_fig, podY, 'prob. of detection Yes', field_names, ivip
    plot_fig, podN, 'prob. of detection No', field_names, ivip
    roc, fa/(fa+null), hit/(hit+miss), field_names, ivip
  endfor
  
  
  
  
  
  if !D.NAME eq 'PS' then device, /close
  close, /all ; for text output file
  
;  save, filename = 'idl.sav', /variables, /verbose
end


pro run_read_parf

  ; It was better to make this outer loop a separate routine that calls the other one with different hour arguments.
  ; Earlier, I forgot to reset the incrementing counters and variables that summed the brier score with each file.  When
  ; this loop was in the same routine, it was incorrectly adding on the sum or total from the previous hour's files.

  hours_to_try = ['00', '03', '06', '09', '12', '15', '18', '21']
  hours_to_try = ['06', '12']
  hours_to_try = ['?[02468]']
  for ihours_to_try = 0, n_elements(hours_to_try)-1 do main, hours_to_try[ihours_to_try]
  
end
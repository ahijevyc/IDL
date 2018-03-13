pro start_dot_plot, xoffset, yoffset, title, _extra=ex
  if ~keyword_set(position) then position=0
  crange=500; 500
  range_ring = 200; 200
  a = findgen(51)/50 * 2.*!PI
  
  plot, [0], [0], xrange=[-crange,crange]+xoffset, yrange=[-crange,crange]+yoffset,xstyle=1,ystyle=1, /nodata, _extra=ex , /isotropic, $
    ytitle='km', title=title , /norm, charsize=!P.CHARSIZE*1., charthick=!P.CHARTHICK*1.23
    
  plots, [0,0], !Y.crange, thick=0.4
  plots, !X.CRANGE, [0,0], thick=0.4
  plots, range_ring*cos(a), range_ring*sin(a), noclip=0
  
end

pro add_dt_contour, dtdrops, xdrops, ydrops, units, barpos=barpos
  if ~keyword_set(barpos) then barpos = [0.7,0.02,0.9,0.04]
  tvlct, oldct, /get
  red = transpose([219,139,139])
  tvlct, red, 13
  dkblue = transpose([10,10,255])
  tvlct, dkblue, 14
  blue = transpose([100,100,255])
  tvlct, blue, 3
  ltblue = transpose([180,180,255])
  tvlct, ltblue, 2
  igood = where(finite(dtdrops),ngood)
  colors=[14,  3,  2, 255, 13]
  levels=[-4, -3, -2,  -1,  0] ; in case there is no good points, we still can draw contourbar.
  if ngood gt 0 then begin
    sort_data = sort(dtdrops)
    ptile = dtdrops[sort_data[0.9*(ngood-1)]]
    levels=[-4, -3, -2,  -1,  0] + floor(ptile)
    contour, dtdrops[igood], xdrops[igood], ydrops[igood], /irr, /overplot, noclip=0, levels=levels, c_colors=colors, /fill
    xyouts, xdrops[igood], ydrops[igood], string(dtdrops[igood],format='(F+0.1)'), charsize=0.7, noclip=0
    ; repair the axes, which were overwritten by the filled contour
    axis, xaxis=0, xrange=!X.CRANGE, xstyle=1, xtickformat='(A1)'
    axis, xaxis=1, xrange=!X.CRANGE, xstyle=1, xtickformat='(A1)'
    axis, yaxis=0, yrange=!Y.CRANGE, ystyle=1, ytickformat='(A1)'
    axis, yaxis=1, yrange=!Y.CRANGE, ystyle=1, ytickformat='(A1)'
  endif
  contourbar, levels, colors, title = units, position=barpos, charsize=0.85, format= '(I+0)'
  tvlct, oldct
  ptimestamp
end

pro dots, z, x, y, units, barpos=barpos, levels=inlevels, barformat=barformat, _extra=ex
  if ~keyword_set(format) then format='(F+0.1)'
  if n_elements(ex) eq 0 then charsize=0.8 else if n_elements(ex.charsize) ne 0 then charsize=ex.charsize
  if ~keyword_set(barpos) then barpos = [!X.WINDOW[0]+(!X.WINDOW[1]-!X.WINDOW[0])*0.05,!Y.REGION[0]+(!Y.WINDOW[0]-!Y.REGION[0])*0.25,!X.WINDOW[0]+(!X.WINDOW[1]-!X.WINDOW[0])*0.95,!Y.REGION[0]+(!Y.WINDOW[0]-!Y.REGION[0])*0.33]
  tvlct, oldct, /get
  ; This particular IDL code block defines 7 colors. The colors are:
  ; dark red,  red,  light red,  dark blue
  ; blue,  light blue, and grey.
  
  dkred = transpose([190,20,20])
  tvlct, dkred, 8
  red = transpose([219,139,139])
  tvlct, red, 13
  ltred = transpose([255,195,195])
  tvlct, ltred, 7
  dkblue = transpose([1,1,245])
  tvlct, dkblue, 14
  blue = transpose([110,110,255])
  tvlct, blue, 3
  ltblue = transpose([205,205,255])
  tvlct, ltblue, 2
  gy = transpose([75,75,75])
  tvlct, gy, 23
  ; Now you can say
  ; contour, z, x, y, /follow, color=23
  ; and the contours will be grey.
  ; Feel free to change the [red,green,blue] components to make your own colors.
  ;  The choice of  8, 13, 7, 14, 3, 2, and 23 as color table indices was arbitrary.
  ; Hopefully the contour routine won't choose one of these custom colors as a fill color.
  ; If it does, use a different index. For example set dkblue to 15 instead of 14.
  ; You shouldn't have to worry about it.
  
  
  igood = where(finite(z),ngood)
  colors=[14,  3,  2, 255, 7, 13 ,8]
  if ~keyword_set(inlevels) then levels=[-4, -3,-2,-1,0,1,2] else levels=inlevels; in case there is no good points, we still can draw contourbar.
  ; Make a vector of 16 points, A[i] = 2pi/16:
  A = FINDGEN(17) * (!PI*2/16.)
  nlines = n_elements(strsplit(z[0],'!C'))
  dy = charsize*10*(2-nlines)*(!X.CRANGE[1]-!X.CRANGE[0])/1200
  symsize = ngood lt 400 ? 3.6 : !D.NAME eq 'X' ? 0.75 : 0.38
  if ngood gt 0 then begin
    dot_colors = replicate(0,ngood)
    txt_colors = replicate(0,ngood)
    sort_data = sort(z)
    ptile = z[sort_data[0.9*(ngood-1)]]
    levels = keyword_set(inlevels) ? inlevels : levels + floor(ptile)
    for idot=0,ngood-1 do begin
      ibin = max(where(levels lt (z[igood])[idot], nbin))
      if nbin eq 0 then ibin = 0
      dot_colors[idot] = colors[ibin]
      if ibin eq 0 || ibin eq 1 || ibin eq 6 then txt_colors[idot] = 255
    endfor
    USERSYM, COS(A), SIN(A), /fill
    plots, x[igood], y[igood], color=dot_colors, psym=8, symsize=symsize, noclip=0, _extra = ex
    iwhite = where(dot_colors eq 255, nwhite)
    if nwhite gt 0 then begin
      ; if the circle is white and there aren't too many circles, outline it.
      USERSYM, COS(A), SIN(A)
      if ngood lt 400 then  plots, (x[igood])[iwhite], (y[igood])[iwhite], psym=8, symsize=symsize, noclip=0, _extra = ex, color=23, thick=0.5
    endif
    if ngood lt 400 then xyouts, x[igood], y[igood]-dy, string(z[igood],format=format), charsize=charsize, charthick=!D.NAME eq 'PS' ? 3: 1, noclip=0, align=0.5, color=txt_colors, _extra = ex
    USERSYM, COS(A), SIN(A), /fill ; switch back to filled
  endif
  type = size(levels, /tname)
  if ~keyword_set(barformat) then barformat = type eq 'FLOAT' ? '(F+0.1)' : '(I+0)'
  contourbar, levels, colors, title = units, position=barpos, charsize=!P.CHARSIZE*0.71, format=barformat
  tvlct, oldct
  
end



function get_ranges
  return, [[0,200],[200,700]]
  ;return, [[0,700]]
  ;return, [[0,200]]
  ;return, [[0,500]]
end

function get_SST_ringrange
  return, [[0,200]]
end


pro IR_histograms, stage, first_time, last_time, expectedIRtotal, time_series_IR_hist, IR_hist_range, hist_binsize, levels
  if n_elements(stage) eq 0 then stage = ''
  ; important to use "CHI" and not "ChI" to print hours. We want 24-hour based integer. 
  firststring = string(first_time, format='(c(cyi4.4," ",cmoi2.2,"/",cdi2.2," ",CHI2.2,":",cmi2.2))')
  laststring  = string(last_time,  format='(c(cmoi2.2,"/",cdi2.2," ",CHI2.2,":",cmi2.2))')
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats/'
  files = file_search(basedir+'PGI*2010????_????00mtm00.txt', count=nfiles)
  if nfiles eq 0 then stop
  years = strmid(file_basename(files),6,4)
  months = strmid(file_basename(files),10,2)
  days = strmid(file_basename(files),12,2)
  hours = strmid(file_basename(files),15,2)
  minutes = strmid(file_basename(files),17,2)
  seconds = strmid(file_basename(files),19,2)
  IR_times = julday(months, days, years, hours, minutes, seconds)
  files = files[where(IR_times ge first_time and IR_times le last_time)]
  ranges = get_ranges()
  nranges = n_elements(ranges)/2
  old_charthick = !P.CHARTHICK
  if !D.NAME eq 'PS' then !P.CHARSIZE = 1.2
  if !D.NAME eq 'X' then !P.CHARTHICK = 1
  for ifile = 0, n_elements(files)-1 do begin
    file = files[ifile]
    m = read_ascii(file, data_start=1,header=header)
    nskip = 26
    nbins = (size(m.(0)))[1] - nskip
    bin_label_start = (strsplit(header, ' '))[nskip]-1
    bin_labels = strmid((strmid(header, bin_label_start)), indgen(nbins)*8,8)
    hist_nbins = round(1+(IR_hist_range[1]-IR_hist_range[0])/hist_binsize)
    ; Read column header of each histogram bin count.
    ; The header contains the range of each histogram bin. The bin range is >= min and < max.
    xval = IR_hist_range[0] + indgen(hist_nbins)*hist_binsize
    bins_string = string(xval,format='(I4)') + '-'+string(shift(xval,-1),format='(I3)')
    ; sanity check
    ; If you fail here it may be because hist_binsize was 1 when calc_rings.pro was executed, but it is 10 below.
    if array_equal(bin_labels, bins_string) ne 1 then stop
    
    if n_elements(expectedIRtotal) eq 0 then expectedIRtotal = replicate(0L,nbins,nranges)
    date = m.(0)[1,*]
    time = m.(0)[2,*]
    rmin = m.(0)[5,*]
    rmax = m.(0)[6,*]
    hist = m.(0)[nskip:nskip+nbins-1,*]
    for irange=0,nranges-1 do begin
      range = ranges[*,irange]
      if range[0] eq 0 then begin ; if it is a zero-to-whatever, just read one line
        line = where(rmin eq range[0] and rmax eq range[1], nlines)
        if nlines ne 1 then stop
        expectedIRtotal[*,irange] = expectedIRtotal[*,irange] + hist[*,line]
      endif else begin ; if it is a range not starting at r=0 then read a bunch of lines.
        lines = where(rmin ge range[0] and rmax le range[1], nlines)
        expectedIRtotal[*,irange] = expectedIRtotal[*,irange] + total(hist[*,lines],2)
      endelse
    endfor
  endfor
  for irange=0,nranges-1 do begin
    range = ranges[*,irange]
    range_string = string(range, format='(I0,"-",I0,"km")')
    gray = [150,150,150]
    expectedcolor=128
    tvlct, oldct, /get
    tvlct, transpose(gray), expectedcolor
    
    y = expectedIRtotal[*,irange]
    x = xval+hist_binsize/2.
    title = stage + " "
    plot, x, y/total(y), xticks=6, xrange=IR_hist_range, xstyle=1,title=title+range_string, xminor=1, /nodata, $
      yrange=[0,n_elements(y) gt 20 ? 0.1 : 0.4], ytitle=!P.MULTI[0] mod !P.MULTI[1] eq 0 ? 'relative fraction' :'', xticklen=1, xtickinterval=10, $
      xtitle = !P.MULTI[0] lt !P.MULTI[2] ? 'IR temperature !Z(00B0)C': '', YCHARSIZE=!P.CHARSIZE*1.3
    for ibin = 0, n_elements(xval)-1 do polyfill, [replicate(xval[ibin],2),replicate(xval[ibin]+hist_binsize,2)],[!Y.RANGE[0],replicate(y[ibin]/total(y),2),!Y.RANGE[0]],noclip=0, color=expectedcolor
    oplot, x, y/total(y), thick=!P.THICK*1.1, psym=10
    ;    print, range_string, y
    coldest_10 = x[min(where(total(y/total(y),/cumulative) gt 0.1))]
    coldest_1 = x[min(where(total(y/total(y),/cumulative) gt 0.01))]
    print, stage, coldest_10, coldest_1
    plots, coldest_10, !Y.CRANGE[0], psym=4 & xyouts, coldest_10, !Y.CRANGE[0]+0.007,string(coldest_10,format='(I0)')+'!C!C!C0.1!Care colder', align=0.5
    plots, coldest_1, !Y.CRANGE[0], psym=4 & xyouts, coldest_1, !Y.CRANGE[0]+0.007,string(coldest_1,format='(I0)')+'!C!C!C0.01!Care colder', align=0.5
    xyouts, x, y/total(y)+0.007, string(y,format='("(",I0,")")'), color=expectedcolor, align=0.5, charsize=!P.CHARSIZE*0.5, noclip=0
    xyouts, !X.WINDOW[0]+0.013, !Y.WINDOW[1]-0.06, 'All pixels '+range_string+ ' from center ('+string(total(y),format='(I0)')+')', $
      /norm, charsize=!P.CHARSIZE*1.25, color=expectedcolor, charthick=!P.CHARTHICK*2.5
      
    i700 = where(levels eq 700, n700)
    if n700 ne 1 then begin
      ;      print, "no 700mb level. skipping time_series_IR_hist analysis (mean IR within 10km of drops)"
      continue
    endif
    
    for ialt = i700[0], i700[0] do begin ; 5 is 700mb
      y = time_series_IR_hist[*,ialt,irange]
      oplot, x, y/total(y), psym=10, thick=!P.THICK*3
      print, range_string, y
      xyouts, x, 0, string(y,format='("(",I0,")")'), align=0.5, charsize=!P.CHARSIZE*0.54
    endfor
    xyouts, !X.WINDOW[0]+0.012, !Y.WINDOW[1]-0.03, 'Within 10 km of drops', color=dropscolor, /norm, charsize=!P.CHARSIZE*0.85
    tvlct, oldct
    
  endfor
  
  !P.CHARTHICK=old_charthick
end
pro SST_ring_composite, stage, first_time, last_time
  if n_elements(stage) eq 0 then stage = ''
  avg_time = (first_time+last_time)/2.
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/SST/pouch_stats/'
  files = file_search(basedir+'SSTPGI*2010????_????00mtm00.txt', count=nfiles)
  if nfiles eq 0 then stop
  years = strmid(file_basename(files),9,4)
  months = strmid(file_basename(files),13,2)
  days = strmid(file_basename(files),15,2)
  hours = strmid(file_basename(files),18,2)
  minutes = strmid(file_basename(files),20,2)
  seconds = strmid(file_basename(files),22,2)
  SST_times = julday(months, days, years, hours, minutes, seconds)
  dtime = min(abs(SST_times - avg_time), imin)
  if dtime gt 1d then stop
  file = files[imin]
  range = get_SST_ringrange()
  m = read_ascii(file, data_start=1,header=header)
  
  fields = strsplit(header, ' ', /extract)
  fields = idl_validname(fields, /convert_all)
  iyyyymmdd = where(fields eq 'yyyymmdd',n)
  if n eq 0 then message, 'could not find yyyymmdd column in '+file
  ihhmmss = where(fields eq 'hhmmss',n)
  if n eq 0 then message, 'could not find hhmmss column in '+file
  irmin = where(fields eq 'rmin',n)
  if n eq 0 then message, 'could not find rmin column in '+SSTfile
  irmax = where(fields eq 'rmax',n)
  if n eq 0 then message, 'could not find rmax column in '+SSTfile
  iavg  = where(fields eq 'avg')
  if n eq 0 then message, 'could not find avg column in '+SSTfile
  
  
  date = m.field01[iyyyymmdd,*]
  time = m.field01[ihhmmss,*]
  rmin = m.field01[irmin,*]
  rmax = m.field01[irmax,*]
  avg  = m.field01[iavg,*]
  line = where(rmin eq range[0] and rmax eq range[1], nlines)
  if nlines ne 1 then stop
  composite_SST = avg[*,line]
  print, stage, composite_SST
end

pro C2W_ratio_vs_range, stage, rg_locs, C_W_vs_range, levels
  !P.MULTI=[0,2,2,0,0]
  ranges = get_ranges()
  nranges = n_elements(ranges)/2
  for irange=0,nranges-1 do begin
    range = ranges[*,irange]
    range_string = string(range, format='(I0,"-",I0,"km")')
    ialt = where(levels eq 700, n700)
    if n700 ne 1 then stop
    C = float(C_W_vs_range[*,ialt,irange,0])
    W = float(C_W_vs_range[*,ialt,irange,1])
    plot, rg_locs, C,   xstyle=1, title=stage+' cold pixels ' + range_string, psym=10
    plot, rg_locs, W,   xstyle=1, title=stage+' warm pixels ' + range_string, psym=10
    igood = where(W ne 0, ngood)
    if ngood gt 0 then plot, rg_locs[igood], C[igood]/W[igood], xstyle=1, title=stage+' cold/warm pixel ratio ' + range_string, psym=10
    igood = where(C+W ne 0, ngood)
    if ngood gt 0 then plot, rg_locs[igood], C[igood]/(C[igood]+W[igood]), xstyle=1, title=stage+' cold/total pixel ratio ' + range_string, psym=10
  endfor
  !P.MULTI=0
  ptimestamp
end

pro run_cold_pool_analysis, start=start, stage0=stage0
  if ~keyword_set(start) then start = 0
  if ~keyword_set(stage0) then stage0 = 0
  basedir = '/Volumes/pecan2/ahijevyc/'
  cd, basedir
  units='mb'
  ;  file_delete, file_search('2*'+units+'_scatter.txt 2*'+units+'_counts.txt'),
  ;; In order to do reversible with 50% fallout, change fractional_fallout from 0 to 0.5
  fractional_fallout = 0.0
  entrainment_rates = [0, 2, 4, 6, 8, 10]
  entrainment_rates = [0]
  for ier = 0, n_elements(entrainment_rates)-1 do begin
    entrainment_rate = entrainment_rates[ier]
    stages = ['non-developing', 'developing_gt2days_prior', 'developing_le2days_prior', 'developed']
    ;stages = ['all']
;        stages = ['non-developing', 'developing_le2days_prior']
    histogram_each_mission = 0 ; set to 1 to do a histogram for each mission
    nstages=n_elements(stages)
    for istage = stage0, nstages-1 do begin
      stage = stages[istage]
      if n_elements(C_W_vs_range) gt 0 then C_W_vs_range[*] = 0L
      t = get_PREDICT_missions(count=nmissions)
      for imission=start,nmissions-1 do begin
        if t.pgi[imission] eq 'PGI34L' and t.yyyymmdd[imission] eq '20100831' and t.hhmm[imission] eq '0000' then continue ; separate DC8 flght for Tom - This is a Cat 1 Hurricane - don't include
        ; if stage is not defined, just get single mission.
        ; used to have these 2 lines outside imission loop, but not if you want a separate histogram for each mission.
        if histogram_each_mission eq 1 then begin
          if n_elements(expectedIRtotal) gt 0 then expectedIRtotal[*]=0L
          if n_elements(time_series_IR_hist) gt 0 then time_series_IR_hist[*]=0L
          stage = t.pgi[imission] + t.yyyymmdd[imission] + '_' +t.hhmm[imission] + '00'
        endif
        cold_pool_analysis, t.pgi[imission], t.yyyymmdd[imission], t.hhmm[imission]+'00', expectedIRtotal, time_series_IR_hist, $
          C_W_vs_range, debug=0, units=units, stage=stage, entrainment_rate=entrainment_rate, fractional_fallout = fractional_fallout
      endfor
      if !D.NAME eq 'PS' && strmatch(stage, 'PGI') ne 1 then begin
        outfile = (fstat(!D.UNIT)).NAME
        device, /close
        new = "~/"+stage + ".ps"
        ; if you are doing histograms and having trouble with stdin crap, set quicker_noGFS = 1 in cold_pool_analysis routine.
        if outfile ne '<stdin>' then file_copy, outfile, new, /overwrite, /verbose
      endif
      
    endfor; stage
  endfor
  print, 'remember to run sort_types.csh in each entrainment subdirectory prior to running cold_warm_vs_ir.pro'
end


pro cold_pool_analysis, pgi, date, time, expectedIRtotal, time_series_IR_hist, C_W_vs_range, $
    debug=debug, units=units, stage=stage, entrainment_rate = entrainment_rate, fractional_fallout = fractional_fallout
  if ~keyword_set(debug) then debug=0
  if ~keyword_set(entrainment_rate) then entrainment_rate = 0
  if ~keyword_set(fractional_fallout) then fractional_fallout = 0d
  
  basedir = '/Volumes/pecan2/ahijevyc/'
  mtm00 = 1
  best_track = 0
  quicker_noGFS = 0 ; if set to 1, IR 10km around drops will not be retrieved
  
  if n_elements(pgi)  eq 0 then pgi = 'PGI38L'
  if n_elements(date) eq 0 then date = '20100902'
  if n_elements(time) eq 0 then time = '180000'
  if ~keyword_set(units) then units = 'mb'
  if ~keyword_set(stage) then stage = pgi+date+"_"+time; 'all'
  ;  if pgi eq 'PGI44L' and date eq '20100917' then begin
  ; Karl had no mtm track for 20109018 00UTC, so you can't do 20100917 with mtm00=1. But I added it by hand. ahijevyc 20120511
  ;    pgi = 'KARL'&    mtm00 = 0&    best_track = 1&    print, "changing PGI44L mtm00 to KARL best track"
  ;  endif
  year   = strmid(date, 0, 4)
  month  = strmid(date, 4, 2)
  day    = strmid(date, 6, 2)
  hour   = strmid(time, 0, 2)
  minute = strmid(time, 2, 2)
  second = strmid(time, 4, 2)
  if debug gt 0 then print, year, month, day, hour, minute, second, format='(I4.4,"/",I0,"/",I0," ",I2.2,":",I2.2,":",I2.2)'
  get_julday = julday(month, day, year, hour, minute, second)
  
  
  ;  avgSST = get_SST_ring(pgi, date, mtm00, best_track)
  ;  if ~finite(avgSST) then print, 'no avgSST for '+pgi+date ; get average SST for this date. might be there.
  
  
  mean_gfs_search_radius = [200,1000]; 500.
  
  ; get points on a lat/lon grid within a radius of the sounding point.
  ; These will be plugged into the IR function to get IR temperatures at each point.
  neighbor_radius_m = 10000. ; radius of circle around lat/lon
  
  levels = get_levels(units)

  
  ;  alts_mb = [1000, 975, 925., 850, 800, 700., 600., 500., 400., 300, 250., 200]
  ;  alts_mb = -10*findgen(81) + 1000; every 10 mb
  ;  alts_mb = [975.,925.]
  ;  alts_m = [0, 250, 500, 1000, 2000., 3000, 4000., 5000., 6000, 7000, 8000, 9000, 10000, 11000, 12000]
  ;  levels = units eq 'mb' ? alts_mb : alts_m
  
  if debug gt 0 then print, 'requested levels:', levels, units
  nlevs = n_elements(levels)
  ranges = get_ranges()
  nranges = n_elements(ranges)/2
  IR_hist_range = [-90.,30.]
  hist_binsize = 1. ; 10 for figures. 
  hist_nbins = round(1+(IR_hist_range[1]-IR_hist_range[0])/hist_binsize)
  if n_elements(expectedIRtotal) eq 0 then expectedIRtotal = replicate(0L, hist_nbins, nranges)
  rg_binsize = 50.
  rg_locs = rg_binsize * findgen((700.-0.)/rg_binsize)  + rg_binsize/2.
  if n_elements(time_series_IR_hist) eq 0 then time_series_IR_hist   = replicate(0L, hist_nbins, nlevs, nranges)
  if n_elements(C_W_vs_range) eq 0 then C_W_vs_range   = replicate(0L, n_elements(rg_locs), nlevs, nranges, 2) ; one for cold pixel count and one for warm pixel count
  if is_stage(basedir+'PREDICT/analysis/', date, hour, stage) eq 0 then return
  
  allDfiles = get_dropsonde_files(basedir, GV_only=1, count=nDfiles, times=dropJulians)
  if nDfiles eq 0 then stop
  sounding_time_window = replicate(0, nDfiles)
  for ifile = 0,nDfiles-1 do begin
    file = allDfiles[ifile]
    dropJulian = dropJulians[ifile]
    ; don't use drop if it is more than 4.5 away from reference time.
    if abs(dropJulian - get_julday) gt 4.5* 1d/24. then continue
    if ~File_test(file) then begin
      if debug gt 0 then print, 'file '+file+' not found'
      continue
    endif
    sounding_time_window[ifile] = 1
  endfor
  isounding_time_window = where(sounding_time_window eq 1, nfiles)
  if nfiles eq 0 then begin
    if debug gt 0 then print, "did not find any sounding files for "+pgi+" "+date+" "+time
    return
  endif
  if debug gt 0 then print, 'found '+strtrim(nfiles,2)+' D* files for '+pgi+' '+date+time
  
  
  if (0) then begin
    SST_ring_composite, stage, dropJulians[min(isounding_time_window)], dropJulians[max(isounding_time_window)]
    return
  endif
  
  
  if !D.NAME eq 'X' then !P.CHARSIZE=1.5
  pos = [0.15,0.16,0.85,0.89]
  if !D.NAME eq 'PS' then begin
    device, /close, /color, ysize=6, yoffset=4, xsize=6.5, xoffset=1, /inches, bits=8
    outfile = basedir+'PREDICT/analysis/' + pgi + STRING(date,time,format='(I8.8,"_",I6.6)')$
      +(mtm00?"mtm00":"")+(best_track?"best_track":"")+ "_coldpool.ps"
    device, filename=outfile
  endif
  loadct, 39, silent=1
  
  !P.THICK = 2
  !P.CHARTHICK = 2
  
  ; Dfiles is just the ones +/- 4.5 hrs and not double counted with other mission or PGI48/51
  Dfiles = allDfiles[isounding_time_window]
  time_series_Times                = replicate(!VALUES.D_NAN, nfiles, nlevs)
  time_series_IR                   = replicate(!VALUES.F_NAN, nfiles, nlevs) ; why nlevs? because get IR for each req. alt (different lat/lon/times)
  time_series_SatIRtime            = replicate(!VALUES.D_NAN, nfiles, nlevs)
  time_series_sndg_T_C             = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_Td_C            = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_Tv_C            = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_P_mb            = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_Z_m             = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_rev_CAPE        = replicate(!VALUES.F_NAN, nfiles)
  time_series_sndg_pseudo_CAPE     = replicate(!VALUES.F_NAN, nfiles)
  time_series_sndg_rev_CIN         = replicate(!VALUES.F_NAN, nfiles)
  time_series_sndg_pseudo_CIN      = replicate(!VALUES.F_NAN, nfiles)
  time_series_sndg_rev_parcl_dT    = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_pseudo_parcl_dT = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_liquid_water    = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_ice_water       = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_U               = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sndg_V               = replicate(!VALUES.F_NAN, nfiles, nlevs)
  dist2center                      = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_sysU                 = replicate(!VALUES.F_NAN, nfiles)
  time_series_sysV                 = replicate(!VALUES.F_NAN, nfiles)
  time_series_x_km                 = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_y_km                 = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_SST_Reynolds         = replicate(!VALUES.F_NAN, nfiles, nlevs)
  time_series_SST_1km              = replicate(!VALUES.F_NAN, nfiles, nlevs)
  mean_gfs                         = replicate({u:!VALUES.F_NAN,v : !VALUES.F_NAN,t : !VALUES.F_NAN,$
    td: !VALUES.F_NAN,tv: !VALUES.F_NAN,rh: !VALUES.F_NAN,z : !VALUES.F_NAN,mse : !VALUES.F_NAN}, nfiles, nlevs)
  old_gfs_date = replicate('', nlevs)
  old_gfs_hour = replicate(!VALUES.F_NAN, nlevs)
  
  
  ; counters to test how many times the mode doesn't equal the mean
  total_IRs = 0.
  clash  = 0.
  cswitch = 0.
  for ifile = 0, nfiles-1 do begin
    file = Dfiles[ifile]
    datestring = strmid(file_basename(file), 1, 8)
    timestring = strmid(file_basename(file), 10, 6)
    ; do something with sounding. it is within 4.5 h of the requested reference time.
    dropsonde_data = get_closest_sonde_in_time(datestring,timestring,levels,debug=debug,units=units,entrainment_rate=entrainment_rate,nocape=quicker_noGFS)
    if quicker_noGFS eq 0 then print, file, dropsonde_data.rev_cape, dropsonde_data.rev_cin, dropsonde_data.pseudo_cape, dropsonde_data.pseudo_cin,$
      format='(A," rev:",F9.2," (",F6.2,") ps:",F9.2," (",F6.2,")")'
    lat = dropsonde_data.lat
    lon = dropsonde_data.lon
    ; Get the latitude, longitude, and Dsec for the sounding at each requested altitude.
    for ilev=0,nlevs-1 do begin
      dtemp_alt = levels[ilev]
      if ~finite(lon[ilev]) || ~finite(lat[ilev]) then begin
        ; this is okay, there may not be data at this level
        if debug gt 0 then message, 'get_closest_sonde_in_time returned NaN for '+datestring+ ' '+ timestring + " " $
          + string(dtemp_alt,units, format='(I0,A)')
        continue
      endif
      
      
      ; only assign T_C, Td_C, etc. if it passes the distance test. (used to assign them before this "for ilev" loop)
      time_series_sndg_T_C[ifile,ilev]          = dropsonde_data.t[ilev]
      time_series_sndg_Td_C[ifile,ilev]         = dropsonde_data.td[ilev]
      time_series_sndg_Tv_C[ifile,ilev]         = dropsonde_data.tv[ilev]
      time_series_sndg_P_mb[ifile,ilev]         = dropsonde_data.p[ilev]
      time_series_sndg_Z_m[ifile,ilev]          = dropsonde_data.z[ilev]
      time_series_sndg_rev_CAPE[ifile]          = dropsonde_data.rev_cape
      time_series_sndg_pseudo_CAPE[ifile]       = dropsonde_data.pseudo_cape
      time_series_sndg_rev_parcl_dT[ifile,ilev] = dropsonde_data.rev_parcl_dT[ilev]
      time_series_sndg_pseudo_parcl_dT[ifile,ilev] = dropsonde_data.pseudo_parcl_dT[ilev]
      time_series_sndg_liquid_water[ifile,ilev] = dropsonde_data.liquidwater[ilev]
      time_series_sndg_ice_water[ifile,ilev]    = dropsonde_data.icewater[ilev]
      time_series_sndg_U[ifile,ilev]            = dropsonde_data.u[ilev]
      time_series_sndg_V[ifile,ilev]            = dropsonde_data.v[ilev]
      
      
      dropyear = strmid(file_basename(file), 1, 4)
      dropmonth = strmid(file_basename(file), 5, 2)
      dropday   = strmid(file_basename(file), 7, 2)
      dropHour  = strmid(file_basename(file), 10, 2)
      dropMinute = strmid(file_basename(file), 12, 2)
      dropSecond = strmid(file_basename(file), 14, 2)
      ; Add dropsonde_data.Dsec to the drop time to get the actual time at the moment it crosses the requested level.
      ; This actual time will be used to determine the IR temps in the neighborhood.
      dropJulian = julday(dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond+dropsonde_data.Dsec[ilev])
      if abs(dropJulian - dropsonde_data.Julian[ilev]) gt 1e-9 then stop ; sanity check after I added .Julian to structure - 20120223
      time_series_Times[ifile,ilev] = dropJulian
      if there_is_a_closer_PREDICT_mission(dropJulian, get_julday) then begin
        if debug gt 0 then print, pgi, date, time, file, " was double counted in the past"
        continue
      endif
      ; first 5 drops of 9/30 mission were for PGI51L, rest for PGI48L
      ; PGI48LPGI51L function returns a string.  It will be 'PGI48L' 'PGI51L' or 'not PGI48L or PGI51L'
      if ( pgi eq 'PGI48L' or pgi eq 'PGI51L' ) and PGI48LPGI51L(pgi,dropJulian) ne pgi then continue
      
      caldat, dropJulian, dropmonth, dropday, dropyear, dropHour, dropMinute, dropSecond
      datestring = string(dropyear,dropmonth,dropday,format='(I4.4,I2.2,I2.2)')
      timestring = string(dropHour,dropMinute,dropSecond, format='(I2.2,I2.2,I2.2)')
      
      mtm_center, pgi, datestring, timestring, clat, clon, best_track=best_track, mtm00=1, silent=debug?0:1, u=u, v=v
      time_series_sysU[ifile] = u
      time_series_sysV[ifile] = v
      earth_radius_km = 6371.; thought about using !ATMOS.Re but it is 6375.
      if finite(clon) and finite(lon[ilev]) then begin
        ; these sanity checks are needed because there are a lot of bad lat lon data in the QCed dropsondes
        ; A lot, meaning, about 20 drops have at least one bad level.
        ; When possible, I've fixed the dropsonde file by hand and copied the old version to D*.eol_hide and the
        ; fixed version to D*.eol_fixed (note, they are not all *eol. Some are csv or fil or hrd.)
        if lat[ilev] lt 0. || lon[ilev] lt -130. then stop
        result = map_2points(clon,clat,lon[ilev],lat[ilev])
        dist2center[ifile,ilev] = result[0] * !DTOR * earth_radius_km
        if ifile gt 0 && abs(dist2center[ifile,ilev] - dist2center[ifile-1,ilev]) gt 4900. then stop
        az = result[1]
        time_series_x_km[ifile,ilev] = sin(az*!DTOR) * dist2center[ifile,ilev]
        time_series_y_km[ifile,ilev] = cos(az*!DTOR) * dist2center[ifile,ilev]
        if quicker_noGFS eq 0 then begin
          time_series_SST_Reynolds[ifile,ilev] = get_sst(dropJulian, lon[ilev], lat[ilev], /reynolds)
          time_series_SST_1km[ifile,ilev] = get_sst(dropJulian, lon[ilev], lat[ilev])
        endif
      endif
      
      
      if quicker_noGFS eq 0 then begin
        gfs_julday = round(dropJulian*4d)/4d ; get nearest multiple of 6 hours
        caldat, gfs_julday, gfs_month, gfs_day, gfs_year, gfs_hour
        gfs_date = string(gfs_year,gfs_month,gfs_day,format='(I4.4,I2.2,I2.2)')
        if old_gfs_date[ilev] eq gfs_date && old_gfs_hour[ilev] eq gfs_hour && finite(mean_gfs[ifile-1,ilev].mse) eq 1 then mean_gfs[ifile,ilev] = mean_gfs[ifile-1,ilev] else mean_gfs[ifile,ilev] = get_mean_gfs( pgi, gfs_date, gfs_hour, dtemp_alt, best_track, mtm00, mean_gfs_search_radius, units=units)
        old_gfs_date[ilev] = gfs_date
        old_gfs_hour[ilev] = gfs_hour
        ; used to end the if-block here but for mission IR histograms I didn't need IR either- just min and max times of mission
        
        ; get points on a lat/lon grid within a radius of the sounding point.
        ; These will be plugged into the IR function to get IR temperatures at each point.
        gspacing = 0.04 ; lat/lon grid spacing within the circle (in degrees) should be similar to satellite pixel spacing
        get_rlat_rlon, neighbor_radius_m, gspacing, lat[ilev], lon[ilev], rlat, rlon
        ; input rlat and rlon arrays to get_ir and get IR_temp array.
        get_ir, IR_temp, datestring, timestring, rlat, rlon, closest_imageJulian, next_closest=next_closest
        ;      print, IR_temp, " mean IR_temp=",mean(IR_temp)
        
        xs = IR_temp
        rd = dist2center[ifile,ilev]
        ; it does not make sense to ignore data below min=-80. so set data below -80 to -80
        ibelow = where(xs lt -80, nbelow)
        if nbelow gt 0 then xs[ibelow] = -80
        h = histogram(xs, binsize=40, min=-80., locations = bin_starting_locations)
        most = max(h, imode)
        ; find the bin with most elements in it
        ; set to the starting location of the bin
        time_series_IR[ifile,ilev] = bin_starting_locations[imode]
        ; or just use the mean value.
        time_series_IR[ifile,ilev] = mean(xs)
        for irange = 0, nranges-1 do begin
          range = ranges[*,irange]
          if rd ge range[0] and rd lt range[1] then begin
            time_series_IR_hist[*,ilev,irange] = time_series_IR_hist[*,ilev,irange] + histogram(xs,binsize=hist_binsize,min=IR_hist_range[0],max=IR_hist_range[1])
            junk = where(xs lt -20, ncold)
            junk = where(xs ge -20, nwarm)
            ibin = where(rg_locs - rg_binsize/2. le rd and rg_locs + rg_binsize/2. gt rd)
            C_W_vs_range[ibin,ilev,irange,0] = C_W_vs_range[ibin,ilev,irange,0] + ncold
            C_W_vs_range[ibin,ilev,irange,1] = C_W_vs_range[ibin,ilev,irange,1] + nwarm
          endif
        endfor
        if rd lt 700 && dtemp_alt eq 700 then begin
          if mean(xs) - bin_starting_locations[imode] gt 40 then clash=clash+1  ; just testng if the mode and mean are ever that different.
          total_IRs=total_IRs+1
          if mean(xs) gt -20 && mean(next_closest.IR_temp) le -20 || $
            mean(xs) le -20 && mean(next_closest.IR_temp) gt -20 then begin
            cswitch=cswitch+1
            openw, lun, basedir+'PREDICT/analysis/switched_classification.txt', /get_lun, /append
            printf, lun, datestring, ' ', timestring, ' ', mean(xs), mean(next_closest.IR_temp)
            free_lun, lun
          endif
        endif
        ;      print, datestring, ' ', timestring, ' mode IR_temp = ', bin_starting_locations[imode]
        time_series_SatIRtime[ifile,ilev] = closest_imageJulian
      endif
      
    endfor; Get the latitude, longitude, and Dsec for the sounding at each requested altitude.
  endfor; mission drop files (Dfiles) loop 
  print, pgi, date, time, cswitch, total_IRs, 100.*cswitch/total_IRs, format='(A,X,A,X,A,2X,I0,"/",I0,"=",F5.2,"%")
  
  IR_histograms, stage, dropJulians[min(isounding_time_window)], dropJulians[max(isounding_time_window)], $
    expectedIRtotal, time_series_IR_hist, IR_hist_range, hist_binsize, levels
  ;  C2W_ratio_vs_range, stage, rg_locs, C_W_vs_range, levels
    
  if quicker_noGFS eq 1 then return ; don't want to update scatter files with noGFS.
  
  
  ; There was a distance threshold check in the previous loop.
  ; it may have prevented time_series_Times from being populated with any finite numbers.
  ; check to see, first.  OR else end this program.
  if ~finite(min(time_series_Times, /nan)) then return
  caldat, min(time_series_Times,/nan), month, day, year, hour, minute, second
  first_time = string(hour, minute, format='(I2.2,":",I2.2)')
  caldat, max(time_series_Times,/nan), month, day, year, hour, minute, second
  last_time = string(hour, minute, format='(I2.2,":",I2.2)')
  title = pgi + " " + strmid(date,4,2) + "/" + strmid(date,6,2) + " " + first_time + "-" + last_time
  
  for ilev=0,nlevs-1 do begin
    dtemp_alt = levels[ilev]
    
    ; plot histograms of temperatures for low, mid, and high cloud segments
    
    dtemp_params = get_dtemp_hist_params(dtemp_alt, units=units)
    tbin = dtemp_params.tbin
    tloc = dtemp_params.tloc
    low_cloud_color = dtemp_params.low_cloud_color
    mid_cloud_color = dtemp_params.mid_cloud_color
    high_cloud_color = dtemp_params.high_cloud_color
    color_psym = replicate(0, nfiles)
    
    high_histogram = replicate(0, n_elements(tloc))
    mid_histogram = replicate(0, n_elements(tloc))
    low_histogram = replicate(0, n_elements(tloc))
    ; used to subtract avgSST, but not anymore 20111230.
    T_histogram_input        = time_series_sndg_T_C[*,ilev]
    Td_histogram_input       = time_series_sndg_Td_C[*,ilev]
    Tv_histogram_input       = time_series_sndg_Tv_C[*,ilev]
    P_histogram_input        = time_series_sndg_P_mb[*,ilev]
    Z_histogram_input        = time_series_sndg_Z_m[*,ilev]
    U_histogram_input        = time_series_sndg_U[*,ilev]
    V_histogram_input        = time_series_sndg_V[*,ilev]
    mse_histogram_input      = moist_static_energy(T_histogram_input+!CONST.T0, Td_histogram_input+!CONST.T0, P_histogram_input, Z_histogram_input)
    
    if max(T_histogram_input) ge max(tloc) then begin
      print, pgi, ' ', dtemp_alt, units, ' max histogram bin not high enough. max(input) =',max(T_histogram_input,/nan),' but max hist =', max(tloc)
      stop
    endif
    if min(T_histogram_input) lt min(tloc) then begin
      print, pgi, ' ', dtemp_alt, units, ' min histogram bin not low enough. min(input) =',min(T_histogram_input,/nan),' but hist min =', min(tloc)
      stop
    endif
    
    IR_block = time_series_IR[*,ilev]
    
    icat = where(IR_block lt -40, ncat)
    if ncat gt 0 then begin
      high_histogram = histogram(T_histogram_input[icat], binsize=tbin, min=min(tloc), max=max(tloc), locations=xloc)
      if ~array_equal(xloc, tloc) then stop
      high_mean = mean(T_histogram_input[icat])
      color_psym[icat] = high_cloud_color
    endif
    icat = where(IR_block ge -40 and IR_block lt 0, ncat)
    if ncat gt 0 then begin
      mid_histogram = histogram(T_histogram_input[icat], binsize=tbin, min=min(tloc), max=max(tloc), locations=xloc)
      mid_mean = mean(T_histogram_input[icat])
      color_psym[icat] = mid_cloud_color
    endif
    icat = where(IR_block ge 0, ncat)
    if ncat gt 0 then begin
      low_histogram = histogram(T_histogram_input[icat], binsize=tbin, min=min(tloc), max=max(tloc), locations=xloc)
      low_mean = mean(T_histogram_input[icat])
      color_psym[icat] = low_cloud_color
    endif
    
    count_label_size = 0.87
    tloc = tloc + tbin/2. ; before plotting change tloc from the starting location of each bin to the midpoint of each bin.
    plot, tloc, xrange=[min(tloc)-tbin/2.,max(tloc)+tbin/2.],yrange=[0,1], /nodata, position=pos, xstyle=1, ystyle=9, $
      xtitle=string('dropsonde',dtemp_alt,units,format='(A,1X,I0,A," temp")'), ytitle = 'fraction of total'
    ptimestamp
    xyouts, 0.5*(pos[0]+pos[2]), pos[3]+0.052, title, align=0.5, charsize=1.81, charthick=2.01, /norm
    scale = total(low_histogram)
    if scale gt 0 then begin
      oplot, tloc, low_histogram/scale, psym=10, color=low_cloud_color, thick=4
      xyouts, tloc, low_histogram/scale, strtrim(low_histogram,2), charsize=count_label_size, align=0.5, color=low_cloud_color
      plots, replicate(low_mean,2), !Y.CRANGE, thick=3.4, color=low_cloud_color, linestyle=1
    endif
    scale = total(mid_histogram)
    if scale gt 0 then begin
      oplot, tloc, mid_histogram/scale, psym=10,color=mid_cloud_color, thick=4.1
      xyouts, tloc, mid_histogram/scale, strtrim(mid_histogram,2), charsize=count_label_size, align=0.5,color=mid_cloud_color
      plots, replicate(mid_mean,2), !Y.CRANGE, thick=3.4, color=mid_cloud_color, linestyle=1
    endif
    scale = total(high_histogram)
    if scale gt 0 then begin
      oplot, tloc, high_histogram/scale, psym=10, color=high_cloud_color, thick=3.2
      xyouts, tloc, high_histogram/scale, strtrim(high_histogram,2), charsize=count_label_size, align=0.5,color=high_cloud_color
      plots, replicate(high_mean,2), !Y.CRANGE, thick=3.4, color=high_cloud_color, linestyle=1
    endif
    ymin = -85.
    ymax = 25.
    ; we convert IR to a scale from 0.0 to 1. so we plot on the original 0.0-1. axis.
    plots, T_histogram_input, (time_series_IR[*,ilev]-ymin)/(ymax-ymin), color = color_psym, thick=3, psym=4
    axis, !X.CRANGE[1], yaxis=1, yrange=[ymin,ymax], ystyle=1, /data, ytitle=string(neighbor_radius_m/1000,format='(I2.2)')+"km IR temperature C"
    xyouts, pos[2]+0.06, pos[3]-0.2, "high clouds", color=high_cloud_color, /norm, orientation=90
    xyouts, pos[2]+0.08, pos[3]-0.2, "mid clouds", color=mid_cloud_color, /norm, orientation=90
    xyouts, pos[2]+0.10, pos[3]-0.2, "no/low clouds", color=low_cloud_color, /norm, orientation=90
    
    
    scatter_file = basedir + 'PREDICT/analysis/entrainment' + string(entrainment_rate,format='(I2.2)')+'_ice/' + $
      (fractional_fallout gt 0 ? string(fractional_fallout, format='(F3.1,"rev_fallout/")') : '') + $
      date + '_' + string(time,dtemp_alt,units,round(neighbor_radius_m/1000),format='(I6.6,"_",I5.5,A,"_IR",I2.2,"km")')+'_scatter.txt'
    ; PGI51L and PGI48L share the same mission so don't overwrite the PGI48L contribution with the PGI51L contribution.
    if pgi eq 'PGI51L' then openu, lun, scatter_file, /get_lun, /append else openw, lun, scatter_file, /get_lun
    caldat, time_series_Times, time_series_month, time_series_day, time_series_year, time_series_hour, time_series_minute, time_series_second
    for ipoint = 0, nfiles-1 do begin
      ; only print the sounding and IR temperature if both are finite.
      if finite(T_histogram_input[ipoint]) and finite(time_series_IR[ipoint,ilev]) and finite(time_series_Times[ipoint,ilev]) then begin
        ;        if finite(mean_gfs[ipoint,ilev].MSE) eq 0 then stop
        printf, lun, pgi, T_histogram_input[ipoint], Td_histogram_input[ipoint], $
          Tv_histogram_input[ipoint], mse_histogram_input[ipoint], $
          dist2center[ipoint,ilev], time_series_IR[ipoint,ilev], $
          time_series_x_km[ipoint,ilev], $
          time_series_y_km[ipoint,ilev], $
          P_histogram_input[ipoint], Z_histogram_input[ipoint], $
          time_series_sndg_rev_CAPE[ipoint], $
          time_series_sndg_pseudo_CAPE[ipoint], $
          time_series_sndg_rev_parcl_dT[ipoint,ilev], $
          time_series_sndg_pseudo_parcl_dT[ipoint,ilev], $
          1000*time_series_sndg_liquid_water[ipoint,ilev], 1000*time_series_sndg_ice_water[ipoint,ilev], $
          U_histogram_input[ipoint], V_histogram_input[ipoint], $
          time_series_sysU[ipoint], time_series_sysV[ipoint], $
          mean_gfs[ipoint,ilev].T, mean_gfs[ipoint,ilev].Td, $
          mean_gfs[ipoint,ilev].Tv, $
          mean_gfs[ipoint,ilev].MSE, $
          mean_gfs[ipoint,ilev].Z, $
          mean_gfs[ipoint,ilev].U, $
          mean_gfs[ipoint,ilev].V, $
          time_series_SST_Reynolds[ipoint,ilev], $
          time_series_SST_1km[ipoint,ilev], $
          !VALUES.F_NAN, $ ; mission average MTP temperagture
          time_series_year[ipoint,ilev], $
          time_series_month[ipoint,ilev], $
          time_series_day[ipoint,ilev], $
          time_series_hour[ipoint,ilev], $
          time_series_minute[ipoint,ilev], $
          time_series_second[ipoint,ilev], $
          Dfiles[ipoint], format='(A10, 36F10.2, 1X, A-86)'
      endif
    endfor
    free_lun, lun
    
  endfor
  
  
  levs_mb = [1000.]
  nlevs = n_elements(levs_mb)
  
  barpos=[pos[0]+0.1*(pos[2]-pos[0]),0.031,pos[0]+0.9*(pos[2]-pos[0]),pos[1]/3.5]
  
  for ilev=0,nlevs-1 do begin
    drops_structure = get_mission_soundings(pgi,date,100.*levs_mb[ilev],best_track,mtm00,debug=debug,time=time)
    xoffsets = drops_structure.xoffset
    yoffsets = drops_structure.yoffset
    nmissions = n_elements(xoffsets)
    
    for imission=0,nmissions-1 do begin
      xoffset = xoffsets[imission]
      yoffset = yoffsets[imission]
      if (abs(xoffset) gt 1000 || abs(yoffset) gt 1000 ) then continue
      gfshour = 18
      if n_elements(drops_structure.dattims) gt 0 then gfshour = 6*round(strmid(drops_structure.dattims[imission], 9,2)/6.) mod 24
      
      ;      gfs_soundings_structure = get_gfs_soundings(pgi,date,gfshour,100.*levs_mb[ilev],best_track,mtm00,block_radius=300.,debug=debug,units='Pa')
      reference = get_mean_gfs( pgi, date, gfshour, 100.*levs_mb[ilev], best_track, mtm00, mean_gfs_search_radius, units='Pa')
      
      ; append the data from the drops to the data from the GFS
      T      = drops_structure.Tdrops[*,imission];,   gfs_soundings_structure.Tdrops]
      mse    = drops_structure.msedrops[*,imission];, gfs_soundings_structure.msedrops]
      xdrops = drops_structure.xdrops[*,imission];,   gfs_soundings_structure.xdrops]
      ydrops = drops_structure.ydrops[*,imission];,   gfs_soundings_structure.ydrops]
      lats   = drops_structure.lats[*,imission]
      lons   = drops_structure.lons[*,imission]
      
      delta_MSE = mse - reference.mse
      delta_T   = T  - (get_sst(get_julday, lons, lats)-!CONST.T0)
      
      
      title = stage+" "+pgi+'!Cref. time '+drops_structure.dattims[imission]+string(levs_mb[ilev],format='(2x,I4)')+"mb "+$
        string(gfshour,format='(I2.2,"Z GFS")')
      ;      start_dot_plot, xoffset, yoffset, title, position=pos
      ; .r cold_warm_vs_ir if get_xlevels() is undefined
      ;      dots, delta_MSE, xdrops, ydrops, 'MSE-mean GFS MSE ('+ string(reference.mse, format='(F5.1,"kJ/kg)")'), barpos=barpos, levels=get_xlevels('dMSE', 0)
      ;      xyouts, xdrops,ydrops+30,string(mse,format='(F0.1)'), align=0.5
        
      ;      add_dt_contour, delta_MSE, xdrops, ydrops, units, barpos=barpos
      ; if add_drops is not compiled, compile calc_rings
      ;      add_drops, drops_structure, imission, /stid
        
      title = stage+" "+pgi+'!Cref. time '+drops_structure.dattims[imission]+string(levs_mb[ilev],format='(2x,I4)')+"mb "
      start_dot_plot, xoffset, yoffset, title, position=pos
      dots, delta_T, xdrops, ydrops, 'T-SST', barpos=barpos, levels=get_xlevels('dTv', 0)
      
      
    endfor
  endfor
  if !D.NAME eq 'PS' then device, /close
  print, outfile
  !P.THICK=1.
  !P.CHARTHICK=1.0
  !P.CHARSIZE=1
end

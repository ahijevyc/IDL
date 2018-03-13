pro close_sondes
  !P.CHARSIZE=1.2
  if !D.NAME eq 'PS' then device, /close, /color, ysize=9, yoffset=1, /inch
  loadct, 39, /silent
  if !D.NAME eq 'X' then device, decomposed=0
  ; compare rawinsondes that are close in space and time to dropsondes.
  files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/dropsondes/*.csv', count=nfiles)
  if !D.NAME eq 'X' then window, 2
  ypos=220
  color=20
  psym=1
  sum_T = replicate(0., 11)
  n_T = replicate(0L, 11)
  erase
  for ifile=0,nfiles-1 do begin
    file = files[ifile]
    template = {$
      VERSION   : 1.00000,   $
      DATASTART : 2L,   $
      DELIMITER : ',',       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 10L,  $
      FIELDTYPES:    [7,7,4,4,4,4,4,4,4,4] ,  $ ; HEIGHT, PRESS used to be type long, but they couldn't take the value !VALUES.F_NAN later on
      FIELDNAMES:   ['STID', 'DATTIM', 'LAT', 'LON', 'HEIGHT', 'PRESS', 'U', 'V', 'TD', 'T'],$
      FIELDLOCATIONS: [0,5,12,19,22,25,27,40,50,60],$
      FIELDGROUPS:  indgen(10) }
      
    t = read_ascii(file, count=nlines, template=template)
    t.press = t.press/100.
    t.t = replace_wNAN(t.t, -9999)
    ih = where(t.press eq 300)
    x = transpose([[t.lon[ih]],[t.lat[ih]]])
    distance = distance_measure(x)
    clusters = cluster_tree(distance, linkdistance,linkage=1)
    if !D.NAME eq 'X' then wset, 0
    title=file_basename(file)
    label_names = t.stid[ih]
    ;    print, [label_names[clusters], string(transpose(linkdistance))], format='(3A10)'
    ;dendro_plot, clusters, linkdistance, orient=1, label_names = label_names, title=title, yrange=[0,10]
    if !D.NAME eq 'X' then window, 1
    iups = where(strmatch(label_names, '????'), nup, complement=idrop, ncomplement=ndrop)
    if nup eq 0 then continue ; if there is no up station...
    lon0 = x[0,iups]
    lat0 = x[1,iups]
    for iup=0,nup-1 do begin ; loop through each of the radiosondes, indices iups. find the distance to each dropsonde.
      i = iups[iup]
      upstation = label_names[i]
      pressures = t.press[where(t.stid eq upstation)]
      up_T = t.T[where(t.stid eq upstation)]
      title = upstation
      min_dist = 9999999
      for idrop = 0, ndrop-1 do begin
        lon1 = x[0,idrop]
        lat1 = x[1,idrop]
        distc = map_2points(lon0[iup], lat0[iup], lon1, lat1, /meters)/1000.
        if distc lt min_dist then begin
          min_dist = distc
          downstation = label_names[idrop]
          idown = where(t.stid eq downstation)
          d_T = t.T[idown]
          title = file_basename(file,'.csv')+" "+upstation+" "+downstation + string(distc, format='(I4,"km")')
        endif
      endfor
      if min_dist lt 250 and stregex(downstation, '^D.*', /bool) then begin ; used in Sun et al., 2013, JGR radiosonde-COSMIC comparison.
        print, title
        if !D.NAME eq 'X' then wset,2
        plot, [0],[0], yrange=[925,200], xrange=[-3,3], xstyle=1, /nodata, /noerase, ystyle=1, yticklen=1, position=[0.12,0.5,0.6,0.9], thick=2.5
        oplot, [0,0],!Y.CRANGE
        xyouts, !X.CRANGE[1]+0.1, ypos, title, color=color, width=width, charsize=0.8, charthick=1.5
        plots, !X.CRANGE[1]+4.6, ypos-7, psym=psym , color=color, thick=2
        plots, up_T-d_T, pressures, psym=-psym, noclip=0, color=color, thick=2
        sum_T = total([[sum_T],[up_T-d_T]],2,/nan)
        n_T   = n_T + finite(up_T-d_T)
        map_set, 0, -75, limit=[10, -82, 21, -55], /cont, /grid, /iso, /ortho, /noerase, position=[0.05,0.1,0.95,0.4], $
          /noborder, label=0
        map_grid, /box
        ;        plots, x, psym=psym, color=color, noclip=0
        plots, lon0[iup], lat0[iup], psym=psym, color=color, noclip=0, thick=2
        plots, [lon0[iup], t.lon[idown]], [lat0[iup],t.lat[idown]], color=color, noclip=0
        plots, t.lon[idown], t.lat[idown], psym=psym, color=color, noclip=0
        ;        xyouts, lon0, lat0, label_names[iups], align=0.5, color=220, charsize=1
        ;        xyouts, x[0,idrop], x[1,idrop], strmid(label_names[idrop], 3, /reverse), align=0.5, color=140, charsize=0.9
        ypos = ypos+35
        color = color gt 210 ? 39 : color+45
        psym = psym eq 7 ? 1 : psym+1
        
      endif
      
    endfor
  endfor ; .csv file
  if !D.NAME eq 'X' then wset,2
  plot, [0],[0], yrange=[925,200], xrange=[-3,3], xstyle=1, /nodata, /noerase, ystyle=1, yticklen=1, position=[0.12,0.5,0.6,0.9], $
    ytitle='pressure (mb)', xtitle = 'rawinsonde-dropsonde T (K)'
    
  plots, sum_T/n_T, pressures, thick=10, noclip=0
  if !D.NAME eq 'PS' then device, /close
end

pro GIV_vs_GV
  if !D.NAME eq 'X' then !P.CHARSIZE=2
  if !D.NAME eq 'PS' then device, /close, /color, ysize=9, yoffset=1, /inch
  loadct, 39, /silent
  date = '20100914'
  if !D.NAME eq 'X' then device, decomposed=0
  ; compare GIV to GV for pre-Karl Sep 14, 2010.
  GIV_files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/dropsondes/HRD/AVAPS/D'+date+'_*QC.eol', count=nGIV_files)
  GV_files =  file_search('/Volumes/pecan2/ahijevyc/PREDICT/dropsondes/D'+date+'_*QC.eol', count=nGV_files)
  
  plot, [0,0], [0,14000], xrange=[-1,1], xstyle=1, ystyle=1, yticklen=1, ytickformat='divide_by_1000', thick=3, xticklen=1, $
    ytitle='geometric height (m)', title = date+' GV - GIV', $
    xtitle = 'GV(n='+string(nGV_files,format='(I0)')+') - GIV(n='+string(nGIV_files,format='(I0)')+') T (K)'
  nlevels=30
  levels = 500.*findgen(nlevels)
  units = 'm'
  GIV_total = replicate(!VALUES.F_NAN, nlevels)
  GV_total  = replicate(!VALUES.F_NAN, nlevels)
  GIV_n     = replicate(0L, nlevels)
  GV_n      = replicate(0L, nlevels)
  GIV_sumx2 = replicate(!VALUES.F_NAN, nlevels)
  GV_sumx2  = replicate(!VALUES.F_NAN, nlevels)
  CI        = replicate(!VALUES.F_NAN, nlevels)
  for ifile = 0, nGV_files-1 do begin
    file = GV_files[ifile]
    date = strmid(file_basename(file),1,8)
    time = strmid(file_basename(file),10,6)
    t = get_closest_sonde_in_time( date, time, levels, /GV_only, units=units, /nocape)
    GV_n = GV_n + finite(t.T)
    GV_total = total([[GV_total], [t.T]], 2, /nan)
    GV_sumx2 = total([[GV_sumx2], [t.T^2.]], 2, /nan)
  endfor
  for ifile = 0, nGIV_files-1 do begin
    file = GIV_files[ifile]
    date = strmid(file_basename(file),1,8)
    time = strmid(file_basename(file),10,6)
    t = get_closest_sonde_in_time( date, time, levels, units=units, /nocape)
    GIV_n = GIV_n + finite(t.T)
    GIV_total = total([[GIV_total], [t.T]], 2, /nan)
    GIV_sumx2 = total([[GIV_sumx2], [t.T^2.]], 2, /nan)
  endfor
  confidence_level = 0.90
  dT = GV_total/GV_n - GIV_total/GIV_n
  for ilevel = 0, nlevels-1 do begin
    n1 = GIV_n[ilevel]
    n2 = GV_n[ilevel]
    var1 = (GIV_sumx2[ilevel] - GIV_total[ilevel]^2./n1) / n1
    var2 = (GV_sumx2[ilevel]  - GV_total[ilevel]^2./n2 ) / n2
    N_indep = min([n1, n2])
    if n_indep-1 gt 0 then begin
      t_cvf = t_cvf((1-confidence_level)/2.,N_indep-1)
      CI[ilevel] = t_cvf * sqrt(var1/n1 + var2/n2)
    endif
  endfor
  
  plots, dT, levels, thick=10, noclip=0
  plots, dT-CI, levels
  plots, dT+CI, levels
  xyouts, 0, !Y.CRANGE[1]+50, string(confidence_level*100,format='(I2,"% confidence interval")'), charsize=0.95, align=0.5
  
  if !D.NAME eq 'PS' then device, /close
  
end
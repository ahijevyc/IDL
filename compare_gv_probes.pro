; For Ana Ortiz's study:
; The cloud limits were chosen by finding the probe variable with the best skill at predicting in-cloud or 
; out-of-cloud conditions. In-cloud conditions were assumed to exist when the IR cloud top temperature was
; colder than the flight level temperature of the GV. Actually, several IR temperature thresholds were tested,
; ranging from 1 C colder to 13 C warmer than the flight level temperature.  The highest skill, in terms of
; Gilbert skill score (more commonly known as equitable threat score), was for CVCWC, using a threshold of
; 0.07 g/m^3 and an IR in-cloud threshold of GV + 4deg. (changed from 0.0722 to 0.07 on Dec 10, 2013 to appease Reviewer A)



function get_xtitle, ialt, alt_reqs, gfs
  if ialt eq -1 then begin
    xtitle = 'IR'
  endif else begin
    ; if gfs eq 0 then use mean mission temp at that height
    ; if gfs eq 1 then use mean gfs as reference
    if gfs eq 0 then xtitle = string(alt_reqs[ialt],format='(I5)')+' MTP - mission_avg'
    if gfs eq 1 then xtitle = string(alt_reqs[ialt],format='(I5)')+' MTP - GFS'
  endelse
  
  return, xtitle
  
end

pro outit, lun, r, x, y_in, time, pos, in_title, in_name, xtitle, desc, ylog=ylog
  ;  caldat, min(time), month, day, year, hour, minute, second
  ;  print, month, day, hour, minute, format='("min:",I0,"/",I0," ",I0,":",I0)'
  ;  caldat, max(time), month, day, year, hour, minute, second
  ;  print, month, day, hour, minute, format='("max:",I0,"/",I0," ",I0,":",I0)'

  name = in_name
  if ~keyword_set(ylog) then ylog=0
  y    = ylog eq 1 ?  alog10(y_in)        : y_in
  
  
  if strmatch(name, 'LON[C ]*') then return
  if strmatch(name, 'LAT[C ]*') then return
  if strmatch(name, 'CLAT_GP *') then return
  if strmatch(name, 'CLON_GP *') then return
  if strmatch(name, 'CALT_GP *') then return
  if strmatch(name, 'ALT_IRS2 *') then return
  if strmatch(name, 'ALT_G *') then return
  if strmatch(name, 'ALT *') then return
  if strmatch(name, 'GGLAT *') then return
  if strmatch(name, 'GGLON *') then return
  if strmatch(name, 'GGALT[ C]*') then return
  if strmatch(name, 'GGSECSDAY *') then return
  if strmatch(name, 'PALT[F ]*') then return ; NACA Pressure Altitude
  if strmatch(name, 'PALT_A *') then return ; ADC Pressure Altitude
  if strmatch(name, 'PCN_WCN *') then return ; WCN absolute pressure - uncorr'd raw
  if strmatch(name, 'PT_A *') then return ; ADC Total Pressure
  if strmatch(name, 'PS_A *') then return ; ADC Static Pressure
  if strmatch(name, 'PSXC *') then return; raw or corrected static pressure (140-1010 mb)
  if strmatch(name, 'PSC_GP *') then return; corrected static pressure, gust pod
  if strmatch(name, 'PSF[C ]*') then return; static pressure, fuselage
  if strmatch(name, 'TIME *') then return
  if strmatch(name, 'WS *') then return ; horizontal wind speed (not corrected version)
  if strmatch(name, '[UV]I *') then return ; wind vector, east/north component (not corrected version)
  if strmatch(name, '[UV]I_GP *') then return ; wind vector, east/north component (not GPS-corrected version)
  if strmatch(name, '[UV]X *') then return ; wind vector, longitudinal/latitudinal component (not corrected version)
  
  ylog_string = ylog eq 0 ? '' : 'log'
  printf, lun, r, in_title, xtitle, ylog_string, name, n_elements(x), format='(F8.4,1X,A21,1X,A23,2X,A3,1X,A-21,I6)'
  
  
  if not finite(r) || abs(r) lt 0.68 then return
  if strmatch(name, 'TCAB *') then return ; temperature in cabin at ADS rack
  if strmatch(name, 'PCAB *') then return ; cabin static pressure
  if strmatch(name, 'THETA*') then return
  if strmatch(name, 'TVIR*') then return
  if strmatch(name, 'TEMP*') then return
  if strmatch(name, 'GGEOIDHT *') then return ; Reference GPS Height of geoid (MSL) above WGS84 ellipsoid
  if strmatch(name, 'AT_A *') then return ; ambient air temperature
  if strmatch(name, 'ATX *') then return ;  air temperature, reference
  if strmatch(name, 'ATHR[12] *') then return ;  ambient temperature, deiced right
  if strmatch(name, 'ATFH[12] *') then return ;  ambient temperature, deiced left
  if strmatch(name, 'TTHR[12] *') then return ;  total temperature, deiced right, HARCO
  if strmatch(name, 'TTFH[12] *') then return ;  total temperature, deiced left, Rosemount
  if strmatch(name, 'TT_A *') then return ;  ADC total air temperature
  if strmatch(name, 'NOSETMP *') then return ;  radome env box temperature
  if strmatch(name, 'A2DTEMP_RAD *') then return ;  A2D temp
  if strmatch(name, 'A2DTEMP_LWI *') then return ;  A2D temp
  if strmatch(name, 'A2DTEMP_RWO *') then return ;  A2D temp
  if strmatch(name, '[AB]DIFRTEMP *') then return ;  temp of A/BDIFR probe
  if strmatch(name, '[AB]DIFRTEMP_GP *') then return ;  temp of A/BDIFR probe
  if strmatch(name, 'QCFTEMP *') then return ;  temp of QCF probe
  if strmatch(name, 'QCRTEMP *') then return ;  temp of QCR probe
  if strmatch(name, 'QCRTEMP_GP *') then return ;  temp of qcr probe
  if strmatch(name, 'PSX *') then return ;  raw static pressure, reference
  if strmatch(name, 'PS_GP *') then return ;  raw dynamic pressure, gust pod
  if strmatch(name, 'PDLR *') then return ; pressure
  if strmatch(name, 'PDUMPP[LR] *') then return ;  Pressure of instrument exhaust duct, left/right
  if strmatch(name, 'AT_VXL *') then return ; VCSEL Ambient Temperature
  if strmatch(name, 'TCC_VXL *') then return ; VCSEL thermoelectric cooler current
  if strmatch(name, 'PLGB_OPC *') then return ; SPP-200 low-gain baseline
  if strmatch(name, 'PREF_OPC *') then return ; SPP-200 laser reference voltage
  ;  if strmatch(name, 'PFLWS_OPC *') then return ; SPP-200 sheath flow
  if strmatch(name, 'PS_VXL *') then return ; VCSEL set temperature
  if strmatch(name, 'SETTMP_VXL *') then return ; VCSEL ambient pressure
  if strmatch(name, 'LSRTMP_VXL *') then return ; VCSEL laser temperature
  if strmatch(name, 'TTX bin*') then return ; Total Temperature, Reference
  if strmatch(name, 'QCX[C ]*') then return ; Raw/corrected Dynamic Pressure, Reference
  if strmatch(name, 'QC[RF][C ]*') then return ; Corrected Dynamic Pressure, Radome/Fuselage
  if strmatch(name, 'PACN_WCN *') then return ; WCN Absolute Pressure
  if strmatch(name, '*VEW *') then return ; ground speed vector, east component
  if strmatch(name, '*VNS*') then return ; ground speed vector, north component
  if strmatch(name, 'UPRESS_CVI *') then return ; UHSAS absolute pressure in canister - "good"
  if strmatch(name, 'USHFLW_CVI *') then return ; UHSAS air sheath flow - "good"
  if strmatch(name, 'USHFLW_LMO *') then return ; UHSAS air sheath flow - "bad"
  if strmatch(name, 'UBTMP_LMO *') then return ; UHSAS temperature in canister
  
  name = ylog eq 1 ? 'log10('+name+')' : name
  
  if total(finite(y)) lt 2 then return
  result = linfit(time,x,yfit=x_trend)
  result = linfit(time,y,yfit=y_trend)
  
  title = string(r,format='(F5.2)')+'!C'+in_title
  plot, time, x, xtickformat='LABEL_DATE',xtickunits=['Time','Time'],position=pos, title=title, ystyle=24,thick=!P.THICK*2, ytitle=xtitle, psym=1
  oplot, time, x_trend, thick=!P.THICK*3
  oplot, !X.CRANGE, [0,0]
  color=50
  plot, time, y, xrange=!X.CRANGE, xstyle=5, ystyle=20, position=pos, /noerase, color=color, thick=!P.THICK*2,psym=1
  oplot, time, y_trend, color=color, thick=!P.THICK*3
  axis, yaxis=1, ytitle=name, color=color, charthick=!P.CHARTHICK*2
  xyouts, 1, 0.98, desc, align=1, /norm, charsize=!P.CHARSIZE*0.5, color=color
  ptimestamp
  
  ; scatterplot
  plot, x, y, title=title, psym=1, ytitle=name, position=[0.15,0.1,0.75,0.9], ystyle=20, xtitle=xtitle
  result = linfit(x, y, yfit=yfit)
  oplot, x, yfit, color=color, thick=!P.THICK*3
  oplot, [0,0], !Y.CRANGE
  axis, yaxis=0, ytitle=name, color=color, charthick=!P.CHARTHICK*2
  axis, yaxis=1, ytitle=name, color=color, charthick=!P.CHARTHICK*2
  xyouts, 1, 0.98, desc, align=1, /norm, charsize=!P.CHARSIZE*0.5, color=color
  ptimestamp
end

pro accumulate_ov, ov, name, jbin, gfs, ialt, alt_reqs, x, y, ylog=ylog

  ; assumes other dimensions:
  ; 1) itype: function of IR, ialt, and GFS
  ; 2) ylog

  if ialt eq -1 then itype = 0 else begin
    if gfs eq 0 then itype = ialt+1
    if gfs eq 1 then itype = ialt+1+n_elements(alt_reqs)
  endelse
  
  ntypes = 1 + 2*n_elements(alt_reqs)
  to_add = {name:name, jbin:jbin, $
    sumx :replicate(0d, ntypes, 2), $
    sumy :replicate(0d, ntypes, 2), $
    sumx2:replicate(0d, ntypes, 2), $
    sumy2:replicate(0d, ntypes, 2), $
    sumxy:replicate(0d, ntypes, 2), $
    n    :replicate(0d, ntypes, 2)}
    
  to_add.sumx[itype,ylog]  = total(x,/double)
  to_add.sumy[itype,ylog]  = total(y,/double)
  to_add.sumx2[itype,ylog] = total(x^2d)
  to_add.sumy2[itype,ylog] = total(y^2d)
  to_add.sumxy[itype,ylog] = total(x*y,/double)
  to_add.n[itype,ylog]     = n_elements(x)
  
  fieldnbin = string(name,jbin,format='(A,I4.4)')
  if n_elements(ov) eq 0 then begin
    ov = create_struct(fieldnbin,to_add)
    return
  endif
  
  nfields = n_tags(ov)
  field_in_ov = 0
  for ifield = 0, nfields-1 do begin
    if ov.(ifield).name eq name && ov.(ifield).jbin eq jbin then begin
    
      ov.(ifield).sumx[itype,ylog]  = ov.(ifield).sumx[itype,ylog]  + total(x, /double)
      ov.(ifield).sumy[itype,ylog]  = ov.(ifield).sumy[itype,ylog]  + total(y, /double)
      ov.(ifield).sumx2[itype,ylog] = ov.(ifield).sumx2[itype,ylog] + total(x^2d)
      ov.(ifield).sumy2[itype,ylog] = ov.(ifield).sumy2[itype,ylog] + total(y^2d)
      ov.(ifield).sumxy[itype,ylog] = ov.(ifield).sumxy[itype,ylog] + total(x*y,/double)
      ov.(ifield).n[itype,ylog]     = ov.(ifield).n[itype,ylog]     + n_elements(x)
      
      field_in_ov = 1
      continue
    endif
  endfor
  if field_in_ov eq 0 then ov = create_struct(fieldnbin,to_add,ov)
  
end
function is_sort_interp, x_in, t1_in, t2_in
  x = x_in
  t1=t1_in
  t2=t2_in
  up = sort(t1)
  t1 = t1[up]
  x  = x[up]
  z = interpol_NAN(x, t1, t2,max_gap=1d/24/60)
  return, z
end


pro do_something_with_IR
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/'
  savfile = basedir+'GV/do_something_with_IR.sav'
  loadct, 39, /silent
  if !D.NAME eq 'X' then device, decomposed=0
  if file_test(savfile) then restore, savfile else begin
  
    GV_files = file_search(basedir+'GV/RF*.PNI.sav',count=nGV)
    for iGV=0,nGV-1 do begin
      GV_file = GV_files[iGV]
      restore, GV_file
      nx = n_elements(GVdata.juldays)
      GV_juldays = n_elements(GV_juldays) eq 0 ? GVdata.juldays : [GV_juldays, GVdata.juldays]
      cvcwc = n_elements(CvCwC) eq 0 ? GVdata.CvCwC : [CvCwC, GVdata.CvCwC]
      temp = n_elements(temp) eq 0 ? GVdata.AT_A : [temp, GVdata.AT_A]
      y = replicate( !VALUES.F_NAN, nx)
      if total(strcmp(tag_names(GVdata),'NACCEPT2DCA_LMI')) gt 0 then begin
        y = GVdata.NACCEPT2DCA_LMI
      endif
      x1 = n_elements(x1) eq 0 ? alog10(y) : [x1, alog10(y)]
      x2 = n_elements(x2) eq 0 ? reform(GVdata.CCDP_ROI[2,0,*]) : [x2, reform(GVdata.CCDP_ROI[2,0,*])]
      x3 = n_elements(x3) eq 0 ? GVdata.TDLSIGNAL : [x3, GVdata.TDLSIGNAL]
      x4 = n_elements(x4) eq 0 ? GVdata.H2OR : [x4, GVdata.H2OR]
    endfor
    cvcwc = alog10(cvcwc)
    x4 = alog10(x4)
    ; read IR time series
    ; First, concatenate the files
    spawn, 'cat ' + basedir+'MTP/IR_time_series_PGI*_GPSAlt.txt>t'
    tmpl = { VERSION: 1., $
      DATASTART: 0L, DELIMITER: '', $
      MISSINGVALUE: !VALUES.F_NAN, $
      COMMENTSYMBOL:  '', FIELDCOUNT:  7, $
      FIELDTYPES: [3,2,2,2,2,4,4], $
      FIELDNAMES: ['year','month','day','hour','minute','second','IR'], $
      FIELDLOCATIONS: [0,4,6,9,12,15,22], $
      FIELDGROUPS: lindgen(7) }
      
    t = read_ascii('t', template=tmpl)
    IR_juldays = julday(t.month,t.day,t.year,t.hour,t.minute,t.second)
    IR = t.IR
    save, IR_juldays, cvcwc, temp, GV_juldays, x1,x2,x3,x4,IR, filename=savfile
  endelse
  
  probe = is_sort_interp(cvcwc, GV_juldays, IR_juldays)
  temp = is_sort_interp(temp, GV_juldays, IR_juldays)
  
  dummy = LABEL_DATE(DATE_FORMAT=['%D-%M','%H:%I'])
  xrange = [julday(9,10,2010,9,0,0), julday(9,10,2010,13,0,0)]
  pos = [0.1,0.15,0.9,0.9]


  n_threshs = 50
  IR_offsets = [-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13]
  hits = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  misses = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  false_alarms = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  corrct_nulls = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  gss = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  csi = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  bias = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  pod = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  pofd= replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  tss = replicate(!VALUES.F_NAN, n_threshs,n_elements(IR_offsets))
  
  for iIR_offset = 0, n_elements(IR_offsets)-1 do begin
    IR_offset = IR_offsets[iIR_offset]
    plot, IR_juldays, IR, xstyle=1, xtickformat='LABEL_DATE', xtickunits=['Time','Time'], $
      position=pos,xrange=xrange, yrange=[-75,38], /nodata
    cloud = IR lt temp+IR_offset
    iclouds = uniq(cloud)
    nclouds = n_elements(iclouds)
    for icloud =  0, nclouds-1, 2 do begin
      if cloud[iclouds[icloud]] eq 1 then icloud = icloud+1
      if icloud ge nclouds-1 then continue
      left = iclouds[icloud]+1
      right = iclouds[icloud+1]
      x = [IR_juldays[left], IR_juldays[right]]
      polyfill, [x, reverse(x)], rebin(!Y.CRANGE, 4, /sample), color=45, noclip=0
    endfor
    oplot, IR_juldays, IR, psym=1
    ;  oplot, GV_juldays, alog10(probe)*30+40, color=130, psym=3
    oplot, IR_juldays, alog10(probe)*30+30, psym=3, color=190
    oplot, IR_juldays, temp, color=244, psym=3
    
    
    ; get 50 thresholds with equal number of elements in each one.
    y_threshs = (probe[sort(probe)])[findgen(n_threshs)/n_threshs*total(finite(probe))]
    for ithresh = 0, n_threshs-1 do begin
      thresh = y_threshs[ithresh]
      hit  = total(finite(probe) and probe ge thresh and cloud)
      miss = total(finite(probe) and probe ge thresh and ~cloud)
      false_alarm   = total(finite(probe) and probe lt thresh and cloud)
      corrct_null = total(finite(probe) and probe lt thresh and ~cloud)
      chance = (hit+miss)*(hit+false_alarm)/(hit+miss+false_alarm+corrct_null)
      csi[ithresh,iIR_offset]  = hit/(hit+miss+false_alarm)
      gss[ithresh,iIR_offset]  = (hit-chance)/(hit+miss+false_alarm-chance)
      bias[ithresh,iIR_offset] = (hit+false_alarm)/(hit+miss)
      pod[ithresh,iIR_offset]  = hit/(hit+miss)
      pofd[ithresh,iIR_offset] = false_alarm/(corrct_null+false_alarm)
      tss[ithresh,iIR_offset]  = (hit*corrct_null - false_alarm*miss)/(hit+miss)/(false_alarm+corrct_null)
      hits[ithresh,iIR_offset] = hit
      misses[ithresh,iIR_offset] = miss
      false_alarms[ithresh,iIR_offset] = false_alarm
      corrct_nulls[ithresh,iIR_offset] = corrct_null
    endfor
    
  endfor
  max = max(csi,imax,/nan)
  imax2d = array_indices(csi, imax)
  print, 'max csi=',max,' at probe=', y_threshs[imax2d[0]], string(IR_offsets[imax2d[1]],format='(" cloud=IR<GV",I+0)')
  print, 'hits=',hits[imax],' misses=',misses[imax],' false alarms=',false_alarms[imax],' correct nulls=',corrct_nulls[imax]
  max = max(gss,imax,/nan)
  imax2d = array_indices(gss, imax)
  print, 'max gss=',max,' at probe=', y_threshs[imax2d[0]], string(IR_offsets[imax2d[1]],format='(" cloud=IR<GV",I+0)')
  print, 'hits=',hits[imax],' misses=',misses[imax],' false alarms=',false_alarms[imax],' correct nulls=',corrct_nulls[imax]
  max = max(tss,imax,/nan)
  imax2d = array_indices(tss, imax)
  print, 'max tss=',max,' at probe=', y_threshs[imax2d[0]], string(IR_offsets[imax2d[1]],format='(" cloud=IR<GV",I+0)')
  print, 'hits=',hits[imax],' misses=',misses[imax],' false alarms=',false_alarms[imax],' correct nulls=',corrct_nulls[imax]
end


pro overall_graphs
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/
  restore, basedir+'GV/GV_overall_stats.sav'
  openw, lun, basedir+'GV/GV_overall_stats.txt', /get_lun
  ntypes = 1+2*n_elements(alt_reqs)
  tags = tag_names(ov)
  for ifield=0, n_tags(ov)-1 do begin
    for itype = 0, ntypes-1 do begin
      for ylog = 0, 1 do begin
        n      = ov.(ifield).n[itype,ylog]
        sumx   = ov.(ifield).sumx[itype,ylog]
        sumy   = ov.(ifield).sumy[itype,ylog]
        sumxy  = ov.(ifield).sumxy[itype,ylog]
        sumx2  = ov.(ifield).sumx2[itype,ylog]
        sumy2  = ov.(ifield).sumy2[itype,ylog]
        if itype eq 0 then begin
          ialt = -1
          gfs  = 0
        endif else begin
          gfs    = (itype-1)/n_elements(alt_reqs) ; ends up being 0 or 1
          ialt   = (itype-1)-gfs*n_elements(alt_reqs)
        endelse
        xtitle = get_xtitle(ialt, alt_reqs, gfs)
        title  = ' '
        
        if n gt 1 && (sumx2-sumx^2./n) gt 0 && (sumy2-sumy^2./n) gt 0 then begin
          r = ( sumxy - sumx*sumy/(n-1) ) / sqrt(sumx2 - sumx^2./n) / sqrt(sumy2 - sumy^2./n)
          outit, lun, r, replicate(0, n), [1], [0d], [0,0,1,1], title, tags[ifield], xtitle, '', ylog=ylog
        endif
      endfor
    endfor
  endfor
  free_lun, lun
end



pro compare_GV_probes
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/
  !P.THICK=2
  !P.CHARTHICK=1
  !P.CHARSIZE=1
  if !D.NAME eq 'X' then !P.CHARSIZE=2
  t = get_PREDICT_missions(count=nmissions)
  loadct, 39, /silent
  if !D.NAME eq 'PS' then device, /close, /color, filename = basedir+'GV/GVstats.ps'
  
  long_names_file = basedir+'GV/nc_long_names.txt'
  nlines = file_lines(long_names_file)
  long_names = strarr(nlines)
  openr, lun, long_names_file, /get_lun
  readf, lun, long_names
  free_lun, lun
  
  ovs = ['TDSIGNAL', 'CVCWC', 'log10(H2OR)']
  
  openw, lun, basedir+'GV/GV_stats.txt', /get_lun
  ; minus 2 because last mission is g4 for Tom (hurricane, not even tropical storm)
  for imission=0,nmissions-2 do begin
    savfile=basedir+'MTP/savfiles/'+t.pgi[imission]+ t.yyyymmdd[imission]+'_'+ t.hhmm[imission]+'00'+'_GPSAlt.sav'
    if file_test(savfile) eq 0 then continue
    title = t.pgi[imission]+ t.yyyymmdd[imission]+'_'+ t.hhmm[imission]+'00'+'_GPSAlt'
    ; restore: alt_reqs, time_series_dropsonde, time_series_SatIRtime, time_series_IR, dist2center, time_series_GV_temp, time_series_MRI,
    ; time_series_Tcp, time_series_Press_alt, time_series_SEtemp, time_series_Ts, time_series_UTSEC, time_series_dropTimes, time_series_mean_gfs,
    ; time_series_MTPTimes
    restore, savfile
    pos = [0.12,0.16,0.81,0.85]
    GV_files = file_search(basedir + "GV/RF*"+t.yyyymmdd[imission]+'*.PNI.nc',count=nfiles)
    if nfiles eq 0 then stop
    for ifile = 0, nfiles-1 do begin
      GV_file = GV_files[ifile]
      savfile = file_dirname(GV_file)+'/'+file_basename(GV_file, '.nc')+'.sav'
      if file_test(savfile) then restore, savfile else begin
        GVdata = get_GV(GV_file)
        save, GVdata, filename=savfile
      endelse
      nMTP = n_elements(time_series_MTPTimes)
      iGVonMTP = replicate(-1L, nMTP)
      for i = 0, nMTP -1 do begin
        dtime = GVdata.juldays - time_series_MTPTimes[i]
        iGV = where(abs(dtime) lt 0.5d/24d/3600d, nGV)
        if nGV ge 2 then stop
        if nGV eq 1 then iGVonMTP[i] = iGV
      endfor
      junk = label_date(date_format=['%H:%I','%N/%D'])
      ifound_gv = where(iGVonMTP ge 0 and time_series_GV_temp lt (-50+!CONST.T0), nfound_GV)
      if nfound_gv eq 0 then begin
        print, 'no matching GV probe times for '+GV_file
        continue
      endif
      
      names = tag_names(GVdata)
      for itag = 0, n_tags(GVdata)-1 do begin
        gv = GVdata.(itag)
        name = names[itag]
        idesc =  where(strpos(strupcase(long_names), string(9B)+strupcase(name)+":") ne -1, ndesc)
        if ndesc gt 0 then desc = strjoin(long_names[idesc],'!C') else desc=''
        ndims = (size(gv))[0]
        if ndims eq 0 then continue
        
        nbins=1
        if ndims eq 3 then nbins = (size(gv))[1]
        for jbin = 0, nbins-1 do begin
          best_r = 0
          for gfs = 0, 1 do begin ; use mission-average or GFS as reference to subtract
            for ialt = -1, n_elements(alt_reqs)-1 do begin
              xtitle = get_xtitle(ialt, alt_reqs, gfs)
              ;              xtitle = ialt eq -1 ? 'IR' : string(alt_reqs[ialt],format='(I0)') + " dTv"
              if ialt eq -1 then begin
                x = time_series_IR[ifound_GV,0]
              ;                xtitle = 'IR'
              endif else begin
                ; if gfs eq 0 then use mean mission temp at that height
                ; if gfs eq 1 then use mean gfs as reference
                if gfs eq 0 then begin
                  x = time_series_Ts[ifound_GV,ialt] -  mean(time_series_Ts[ifound_GV,ialt])
                ;                  xtitle = 'MTP - mission_avg'
                endif
                if gfs eq 1 then begin
                  x = time_series_Ts[ifound_GV,ialt] - (time_series_mean_gfs.Tv)[ifound_GV,ialt]
                ;                  xtitle = 'MTP - GFS'
                endif
              endelse
              if ndims eq 3 then y = gv[jbin,0,iGVonMTP[ifound_GV]] else y = gv[iGVonMTP[ifound_GV]]
              time = time_series_MTPTimes[ifound_GV]
              igood = where(finite(x) and finite(y), ngood)
              if ngood ge 100 then begin
                x = x[igood]
                y = y[igood]
                time = time[igood]
                r = p_correlate(x,y,time, /double);  ; account for linear time trend
                accumulate_ov, ov, name, jbin, gfs, ialt, alt_reqs, x, y, ylog=0
                if abs(r) gt abs(best_r) then begin
                  best_r = r
                  best_x = x
                  best_y = y
                  best_time = time
                  best_name = name+string(jbin, format='(" bin ",I0)')
                  best_xtitle = xtitle
                  ylog = 0
                endif
                ipos = where(y gt 0, npos)
                if npos ge 100 then begin
                  x = x[ipos]
                  y = y[ipos]
                  time = time[ipos]
                  r = p_correlate(x,alog10(y),time, /double);  ; account for linear time trend
                  accumulate_ov, ov, name, jbin, gfs, ialt, alt_reqs, x, y, ylog=1
                  if abs(r) gt abs(best_r) then begin
                    best_r = r
                    best_x = x
                    best_y = y
                    best_time = time
                    best_name = name+string(jbin, format='(" bin ",I0)')
                    best_xtitle = xtitle
                    ylog = 1
                  endif
                endif
              endif
            endfor ; new alt_reqs
          endfor
          if abs(best_r) ge 0.0 then outit, lun, best_r, best_x, best_y, best_time, pos, title, best_name, best_xtitle, desc, ylog=ylog
        endfor ; new bin for 3D fields
        
      endfor; new nc variable field
    endfor; new file
  endfor; new mission
  
  free_lun, lun
  
  if !D.NAME eq 'PS' then device, /close
  
  save, ov, alt_reqs, filename =basedir+'GV/GV_overall_stats.sav'
  
  
  !P.THICK=1
  !P.CHARTHICK=1
  !P.CHARSIZE=1
  
end
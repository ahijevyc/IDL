pro read_satcast, dp=dp
  loadct, 39, /silent
  if !D.NAME eq 'X' then device, decomposed=0
  
  ;  ny = 2600
  ;  nx = 3800
  ;  basedir = '/mmmtmp/ahijevyc'
  ;  files = file_search(basedir+'/2*.dat', count=nfiles)
  ;  scale = 3.4
  ;;  xinteranimate, set=[nx/scale, ny/scale, nfiles], /showload
  ;  for ifile = 0, nfiles-1 do begin
  ;    t = read_binary(files[ifile],data_type=[2],data_dims=[nx,ny])
  ;    t[where(t gt 0)] = 1
  ;    image = n_elements(image) gt 0 ? image + congrid(t, nx/scale, ny/scale) : congrid(t, nx/scale, ny/scale)
  ;;    xinteranimate, frame=i, image = image
  ;;    erase, 255
  ;    tvscl, image
  ;    xyouts, 0.5, 0.99, file_basename(files[ifile]),/norm, align=0.5
  ;  endfor
  ;;  xinteranimate
  
  
  
  ; read *.LR files and do histogram
  ; totally different crap than commented stuff above
  
  basedir = '/pecan/ahijevyc/NASA_ROSES/SATCAST_NWP'
  
  binsize=1.
  ; match percentiles
  if ~keyword_set(dp) then dp = 0.01
  percentiles = findgen(100./dp + 1) / (100./dp)
  np = n_elements(percentiles)
  
  
  
  LRsavfile = basedir+'/LRs.sav'
  files = file_search(basedir+'/2*.LR', count=nfiles)
  if file_test(LRsavfile) then restore, LRsavfile else begin
    for ifile = 0, nfiles-1 do begin
      file = files[ifile]
      t = read_ascii(file)
      lr = t.field1
      LRs = n_elements(LRs) eq 0 ? [lr] : [LRs, lr]
    endfor
    save, LRs, filename = LRsavfile
  endelse
  
  RFsavfile = basedir+'/RFs.sav'
  files = file_search(basedir+'/2*.all', count=nfiles)
  if file_test(RFsavfile) then restore, RFsavfile else begin
    for ifile = 0, nfiles-1 do begin
      file = files[ifile]
      t = read_ascii(file)
      flag = t.field01[0,*]
      RF = reform(t.field01[33,*])
      RFs = n_elements(RFs) eq 0 ? [rf] : [RFs, rf]
    endfor
    save, RFs, filename = RFsavfile
  endelse
  
  
  ; Cut LRS and RFs down to good values.
  igood = where(LRs ne -99 and LRs ne -1, ngood)
  if ngood eq 0 then stop
  LRs = LRs[igood] & RFs = RFs[igood]
  
  ;  print, min(LRs), max(LRs), min(RFs), max(RFs)
  
  RF_color = 210
  newRF_color = 194
  
  h = histogram(RFs, min=0, max=400, binsize=binsize, locations=xlocs)
  plot, xlocs+binsize/2., h, psym=10, xtitle='RF votes', ytitle='count', charsize=2, xstyle=1
  
  
  LR_h = histogram(LRs, min=0, max=100, binsize=binsize, locations=xlocs)
  plot, xlocs+binsize/2., LR_h, psym=10, xtitle='% chance of CI', ytitle='count', charsize=2, xstyle=1, title=string(np, dp,format='(I0," ",F4.2)')+'-wide percentile bins'
  
  h = histogram(RFs/4., min=0, max=100, binsize=binsize, locations=xlocs)
  oplot, xlocs+binsize/2., h, psym=10, color=RF_color
  
  n = n_elements(LRs)
  if n ne n_elements(RFs) then stop
  
  newRFs = fltarr(n)
  
  
  savfile = basedir+'/vote2prob_dp'+string(dp,format='(F4.2)')+'.sav'
  if file_test(savfile) then begin
    LR_file_info = file_info(LRsavfile)
    RF_file_info = file_info(RFsavfile)
    savfile_info = file_info(savfile)
    ; make sure the save file has a greater mtime (is more recently modified) than original LR and RF save files.
    if savfile_info.mtime gt LR_file_info.mtime and savfile_info.mtime gt RF_file_info.mtime then begin
      restore, savfile
    endif else begin
      print, LRsavfile+' or '+RFsavfile+' was modified more recently than savfile '+savfile
      stop
    endelse
  endif else begin
    vote2prob = fltarr(2,np)
    for ip = 0, np-1 do begin
      p = percentiles[ip]
      i = p*(n-1)
      LR = (LRs[sort(LRs)])[i]
      RF = (RFs[sort(RFs)])[i]
      vote2prob[*,ip] = [RF, LR]
    endfor
    save, vote2prob, filename = savfile
  endelse
  
  ;  print, vote2prob
  
  newRFs = interpol(vote2prob[1,*], vote2prob[0,*], RFs)
  
  h = histogram(newRFs, min=0, max=100, binsize=binsize, locations=xlocs)
  oplot, xlocs+binsize/2., h, psym=10, color=newRF_color
  
  
  ; This is an estimate of how jagged the new RF distribution is. 
  ; sum of the absolute differences in histogram count between LR and new RF. smaller is better. 
  ; somewhat reduced with smaller percentile bin size.  But the difference at 0.5% is almost as small as 0.01%. 
  print, savfile, total(abs(h-LR_h))
  
  legend_x = 55
  xyouts, legend_x, 5.0e4, 'LR'
  xyouts, legend_x, 4.5e4, 'RF', color=RF_color
  xyouts, legend_x, 4.0e4, 'RF calibrated to LR', color=newRF_color
  
  
  ; create the *.newRF files. Just 1 column of RF votes. Should match the *.all files.
  if (0) then begin
    files = file_search(basedir+'/2*.all', count=nfiles)
    for ifile = 0, nfiles-1 do begin
      file = files[ifile]
      outfile = file+'.newRF'
      openw, lun, outfile, /get_lun
      t = read_ascii(file)
      flag = t.field01[0,*]
      RF = reform(t.field01[33,*])
      newRFs = interpol(vote2prob[1,*], vote2prob[0,*], RF)
      printf, lun, newRFs, format='(F10.2)'
      free_lun, lun
    endfor
  endif
  
end



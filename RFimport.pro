pro RFimport
; Plot a scatterplot of importance vs predictor on a logarithmic scale.
; The scatterplot can have a different y-position for different sets (e.g. set1 - set5), 
; different markers for different sampling distributions (e.g. dst90_10), or different VIP level thresholds.
; The plot symbol or marker can be changed accordingly.
; A slight y-offset is applied to the marker to differentiate the set #.
; The output from this program is meant to illustrate the variability of importance
; among predictors, sets, odd/even Julian days, sampling distributions, and other tunable parameters.  

  LOaDCT, 28
  
  ; Name the PostScript output file.  Used if set_plot = 'PS'
  psfile = 'importWxType20s80s.ps'
  prefix = '004'
  psfile = 'RF_importance_'+prefix+'_2009.ps'
  IF !d.NAME EQ 'PS' THEN DEVICE, /COLOR, /close, bits=8, ysize=10, xsize=7.5, yoffset=.5, xoffset=.5, /inches, filename=psfile
  
  pos = [0.2,0.05,0.99,0.95]
  ascii_temp = { $
    VERSION:       1.00000,$
    DATASTART:     1L,$
    DELIMITER:     0B,$
    MISSINGVALUE:  !VALUES.F_NAN,$
    COMMENTSYMBOL: '',$
    FIELDCOUNT:    4L,$
    FIELDTYPES:    [4,4,4,7], $
    FIELDNAMES:    ['Imp','ZSc','Sig','label'],$
    FIELDLOCATIONS:  [0, 7, 14, 25],$
    FIELDGROUPS:  indgen(4) }
    
  ascii_fastimport = { $
    VERSION:       1.00000,$
    DATASTART:     1L,$
    DELIMITER:     0B,$
    MISSINGVALUE:  !VALUES.F_NAN,$
    COMMENTSYMBOL: '',$
    FIELDCOUNT:    2L,$
    FIELDTYPES:    [4,7], $
    FIELDNAMES:    ['Imp','label'],$
    FIELDLOCATIONS:  [0, 7],$
    FIELDGROUPS:  indgen(2) }
    
  types = ['isVIP_back1hr1', 'isVIP_back2hr1', 'isInit1_40_1h', 'isInit1_40_2h', 'isVIP_back1hr3', 'isVIP_back2hr3', 'isInit3_40_1h', 'isInit3_40_2h', 'isExNil3_40_1h', 'isExNil3_40_2h']
  
  ; Outer loop controls whether regular importance files are used or "fast" importance.
  for import_type = 0, 0 do begin
  
    fast = ''
    ascii_template = ascii_temp
    xtitle = 'Importance'
    if import_type eq 1 then begin
      fast = 'fast'
      ascii_template = ascii_fastimport
      xtitle = 'dGINI'
    endif
    
    ; Next loop cycles through directories or types.
    for itype = 0, n_elements(types)-1 do begin
      type = types[itype]
      
      infiles = FILE_SEARCH('/pecan/ahijevyc/CoSPA/randomForest/'+type+'*/'+prefix+'_nt*importance*txt', count=nfiles)
      
      if nfiles eq 0 then begin
        print, 'no ' +type+' files found'
        continue
      endif
        
      filename = infiles[0]

      d = read_ascii(filename,template = ascii_template)
      ordered = sort(d.label)
      imp = d.imp[ordered]; If you change this here, change in the loop below also.
      if n_elements(labels) eq 0 then labels = d.label[ordered]
      nlabels = n_elements(labels)
      plot, imp, indgen(nlabels), ystyle=3, xrange=[0.01,10], xtitle = xtitle, xstyle=1, psym=1, symsize=0.5, /xlog, yticks=1, ytickinterval=10, position=pos, /nodata, ytickformat='(A1)', yticklen=0.5
      colors = randomu(-0L, nlabels)*255.
      
      ; Write y-axis label for each predictor.
      for ilabel = 0, nlabels-1 do xyouts, (convert_coord([!x.window[0]-0.2,0], /normal, /to_data))[0], ilabel-0.12, STRing(ilabel,format='(I3)')+'  '+labels[ilabel], charsize=0.6, color=colors[ilabel]

      ; Write title.
      title = file_basename(file_dirname(infiles)) & title = title[uniq(title)] & title = strjoin(title, ' ')
      xyouts, (pos[0]+pos[2])/2., pos[3]+(1-pos[3])/2., title, charsize=0.9, charthick=0.9, /norm, align=0.5

      ; Loop through all the files in this directory or type.
      for ifile = 0, nfiles-1 do begin
        filename = infiles[ifile]
        ; Grab the digit 1-6 from the directory.
        marker = strmid(file_basename(file_dirname(filename)),strlen(type)-1,1)
        if strmatch(type, '*isInit*') then marker = strmid(file_basename(file_dirname(filename)), 6, 1)
        
        result = stregex(file_basename(filename), 'dst[0-9]+_([0-9]+)_set([0-9]+)_', /extract, /subexpr)
      
        dst = result[1]
        marker = dst ; make the marker the sampling distribution percentage instead of the VIP level threshold
        setnumber = result[2]
        yoffset = (setnumber-3.) * 0.1 ; Based on having 5 sets, set 3 is centered on the y-axis tick mark. Higher set # means a greater positive y-axis offset.

        d = read_ascii(filename,template = ascii_template, count = n) ; don't count nlabels again, it may be different. You need to keep track of the original amount of fields. 
        ordered = sort(d.label)
        imp = d.imp[ordered]
        for ilabel = 0, n-1 do begin
          ; Does not require all fields to be present, but no extra ones (or else inew = -1).  
          inew = where(labels eq (d.label[ordered])[ilabel], n)
          if n eq 0 then continue
          xyouts, imp[ilabel], inew-0.12+yoffset, marker, charsize=0.5, color=colors[inew]  
          if imp[ilabel] lt 0.01 then xyouts, 0.008, inew-0.12, '<'+marker, charsize=0.5, color=colors[inew]
        endfor
        for ilabel = 0, nlabels-1 do begin
          i = where(d.label eq labels[ilabel], present)
          if present eq 0 then print, filename + " missing " + labels[ilabel]
        endfor
      endfor
      
    end
  end
  
  
  if !D.NAME eq 'PS' then device, /close
  if !D.NAME eq 'PS' then print, 'created ' + psfile
end

pro ADECK_boxplot
  ; used to make original boxplot for MPAS TC paper. But wanted to change to maximum vmax along the track
  ; instead of vmax for each 6-h forecast period. So instead of a pair for each matched forecast time, a
  ; single pair for each forecast track/observed track.
  ;basedir = '/glade/work/ahijevyc/METv4.1/out/2014/'
  basedir = '/glade/work/ahijevyc/met-5.2/out/2014/'
  year = file_basename(basedir)
  if year eq 2014 then models = ['MPS4','GFSO']
  if year eq 2016 then models = ['MPS4','GFSO']
  column = 'AMAX_WIND' ; 'TK_ERR' 'ABS(AMAX_WIND-BMAX_WIND)'
  ; deal with 'tracker' mode and 'tcgen' mode and 'fiorino' variations
  ; 'tracker' mode and 'fiorino' will have '.tracker' and '.fiorino' suffixes.
  ; 'tcgen' will have '.tcgen'.
  misc_str = '.tcgen' 
  max_vmax_comparison = 0 ; 0=all points along track; 1=one per track
  event_equal = 1 ; event_equal=1 is homogenous
  units = 'm/s'
  ;units = 'knots'
  if event_equal then begin
    event_equal_file = basedir+'event_equal_'+strjoin(models,"_")+'_gfdl_1.0d_minimum.tcst'
    if file_test(event_equal_file) eq 0 then begin
      cd, basedir
      ; sanity check. assumes first models entry is variable MPAS and 2nd is GFS
      if ~strmatch(models[0],"MP*") or models[1] ne "GFSO" then stop
      cmd = "tc_stat -lookin " + models[0] + "_0.500deg_025km_gfdl_origmeshTrue_1.0d_minimum"+misc_str+".tcst "
      GFSgrid = '_0.500deg' 
      if year eq 2016 then GFSgrid = '_0.250deg'
      spawn, cmd + "GFSO"+GFSgrid+"_gfdl_origmeshFalse_1.0d_minimum"+misc_str+$
        ".tcst -amodel "+strjoin(models,",")+ " -event_equal TRUE -job filter -dump_row "+event_equal_file
    endif
    t = read_tc_stat(event_equal_file)
  endif
  tracker = 'gfdl'
  foreach model, models do begin
    origmesh_str = strmatch(model,'GFS*') ? '_origmeshFalse' : '_origmeshTrue'
    model_str = atcf_modelname(model)
    grid_details = '_0.500deg_025km'
    if model eq 'GFSO' then begin
      grid_details = '_0.500deg'
      if year eq 2016 then grid_details = '_0.250deg'
    endif
    tc_stat_file = basedir + model + grid_details + '_' + tracker + origmesh_str+'_1.0d_minimum'+misc_str+'.tcst'
    t = event_equal ? t : read_tc_stat(tc_stat_file)
    ytitle=model_str+" Intensity (knots)"
    xtitle='Observed Intensity (knots)'
    if max_vmax_comparison then begin
      ; from /glade/work/ahijevyc/METv4.1/max_vmax_comparison
      t = read_max_vmax_comparison('/glade/work/ahijevyc/METv4.1/t')
      xtitle='Observed Maximum Wind Along Track (knots)'
      ytitle=model+" Maximum Wind Along Track (knots)"
    endif
    foreach basin, ['EP'] do begin ; can use '*' for global
      ofile = basedir+idl_validname('tc_stat_'+model+'_basin'+basin+'_'+column,/convert_all)+'.png'
      ;p = scatterplot(t.BMAX_WIND, t.AMAX_WIND)
      boxes = list()
      storms = list()
      xmin =  20.
      xmax = 140. 
      binsize=10.
      xloc = xmin + binsize*findgen((xmax-xmin)/binsize + 1) + binsize/2.
      for bmax_wind=xmin,xmax,binsize do begin
        inbin = where(finite(t.amax_wind) and strmatch(t.amodel, model, /fold) and $
          strmid(t.init,5,/reverse) eq '000000' and $ ; just 00Z initializations
          strmatch(t.basin, basin,/fold) and t.bmax_wind ge bmax_wind and t.bmax_wind lt bmax_wind+binsize,/null,ninbin)
        if ninbin lt 5 then begin
          ; make sure this only happens at the end. Im not sure if it plots right if there are missing bins in the middle.
          print, '<5 samples in '+string(bmax_wind,bmax_wind+binsize,format='("[",I0,"-",I0,")")')+". skipping"
          continue
        endif
        boxes.add, t.amax_wind[inbin]
        sid = t.storm_id[inbin]
        storms.add, uniq(sid[sort(sid)]) ; remember to sort! or else you get interleaved ep17 and ep18 and count too many "unique" storms!
      endfor
      if boxes.where(!NULL) ne !NULL then stop
      ; assumes none of the bins are empty
      h = boxplot(indgen(boxes.count()), createboxplotdata(boxes,mean_values=means, outlier_values=outlier_values), $
        title=year+" "+model_str+" basin:"+basin+" "+column+" event_equal:"+strtrim(event_equal,2),$
        fill_color='beige',mean_values=means,symbol_means='x',symbol_outliers='o',$
        xtickinterval=binsize,notch=1,xminor=0,xrange=[0,xmax/binsize-1]-0.5,xstyle=1,xticklen=0.,$
        name=basin, ytitle=ytitle, yticklen=0.02, xtitle=xtitle,yrange=[0,145])
      iraw = where(finite(t.amax_wind) and strmatch(t.amodel, model, /fold) and strmatch(t.basin, basin, /fold),/null)
      ;raw = scatterplot((t.bmax_wind[iraw]-xmin-0.5*binsize)/binsize, t.amax_wind[iraw], overplot=h, symbol="o", $
      ;  sym_filled=1, sym_size=0.2, sym_color='red', xrange=h.xrange)
      ax = h.axes

      ; convert boxplot coordinates [-1, 12] to wind speed [10, 140]
      ax[0].coord_transform = [xmin+0.5*binsize, binsize]

      z = plot(h.xrange, xmin + binsize*(h.xrange)+binsize/2,linestyle='dashed',overplot=h,name='one2one',yrange=h.yrange)
      yref = plot(h.xrange, [64,64], thick=3, overplot=h, name='64 kt')
      yref_lab = text(h.xrange[1]+0.1, 64, yref.name, align=0,font_size=ax[1].tickfont_size, /data, clip=0,vertical_align=0.5)
      h.title.translate, 0, 0.025, /normal

      for w=0,boxes.count()-1 do count = text(w, (h.yrange)[1]+4,strtrim(n_elements(boxes[w]),2), align=0.5,$
        font_size=7,/data,clip=0)
      count_lab = text((h.position)[2], (count.position)[1],'# forecasts', align=0,font_size=7,clip=0)
      for w=0,boxes.count()-1 do count = text(w, (h.yrange)[1]+1,strtrim(n_elements(storms[w]),2), align=0.5,$
        font_size=7,/data,clip=0)
      count_lab = text((h.position)[2], (count.position)[1],'# storms', align=0,font_size=7,clip=0)


      junk = timestamp_text()
      h.window.save, ofile, resolution=175
      print, "created "+ofile
    endforeach ; basin
  endforeach ; model

end


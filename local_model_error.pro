pro local_model_error
  mpas = mpas_mesh('mpas_ep')
  fh_threshold = 0
  basedir='/glade/scratch/ahijevyc/'+mpas.name+'/'
  foreach basin, ['EP'] do begin
    foreach HMF, ['false_alarm'] do begin
    
      ofile = basedir + mpas.name + '_' + basin + '_fh.ge.' + string(fh_threshold,format='(I3.3)') + '_' + HMF+'_bias.txt'
      ;if file_test(ofile) eq 1 then stop ; are you sure you want to redo this?
      ;lun = -1
      openw, lun, ofile, /get_lun
      
      ; read hit1, miss1, and false_alarm1. these were created by ~/get_first_hit.csh
      if HMF eq 'false_alarm' then begin
        hm_file = basedir+'false_alarm1'
        t = read_atcf(hm_file)
        t.TECH = atcf_modelname(t.TECH)
        v = t.init_yyyymmddhh
        month = strmid(v,4,2)
        day = strmid(v,6,2)
        year = strmid(v,0,4)
        hour = strmid(v,8,2)
        valid_yyyymmddhh = string(julday(month,day,year,hour) + t.TAU/24d, format='(c(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
        t = create_struct('hm', replicate('false_alarm',n_elements(t.lon)), 'valid_yyyymmddhh', valid_yyyymmddhh, t)
      endif else begin
        hm_file=basedir+HMF+'1'
        t = read_ascii(hm_file, template=hits_and_misses_template())
        ; make sure all forecast hour strings start with 'f'
        junk = where(strmid(t.TAU,0,1) ne 'f',/null)
        if junk ne !NULL then stop
        t.TAU = long(strmid(t.TAU,1))
      endelse
      i = where( t.basin eq basin and t.TECH eq mpas.name and t.hm eq HMF and t.TAU ge fh_threshold, npt, /null)
      if i eq !NULL then stop
      lons = t.lon[i]
      lats = t.lat[i]
      init_dates = t.init_yyyymmddhh[i]
      valid_dates = t.valid_yyyymmddhh[i]
      fhrs = strmid(t.TAU[i],1)
      month = strmid(init_dates,4,2)
      day = strmid(init_dates,6,2)
      year = strmid(init_dates,0,4)
      hour = strmid(init_dates,8,2)
      
      
      for ipt=0,npt-1 do begin
        init_date = init_dates[ipt]
        fh = fhrs[ipt]
        valid_time = julday(month[ipt],day[ipt],year[ipt],hour[ipt],0,0)+fh/24d
        valid_date = string(valid_time,format='(c(CYI4.4,CMOI2.2,CDI2.2,CHI2.2))')
        if valid_date ne valid_dates[ipt] then stop
        nearestCells = mpas_nearest_cell(lons[ipt], lats[ipt], mpas)
        print, init_date, valid_time, format='("init ",A,", valid ",C(),2x,A)'
        ; used to initialize vitals outside for ipt loop. but it needs to initialized every iteration.
        ; or else the data are carried on to the next iteration.
        data = replicate(!VALUES.D_NAN, 1)
        vitals = {$
          a0:{field:'relhum_850hPa',range:[0, 125],op:'mean',data:data},$
          a1:{field:'relhum_850hPa',range:[0, 250],op:'mean',data:data},$
          a2:{field:'relhum_850hPa',range:[0, 500],op:'mean',data:data},$
          a3:{field:'relhum_850hPa',range:[0,1000],op:'mean',data:data},$
          
          b0:{field:'relhum_700hPa',range:[0, 125],op:'mean',data:data},$
          b1:{field:'relhum_700hPa',range:[0, 250],op:'mean',data:data},$
          b2:{field:'relhum_700hPa',range:[0, 500],op:'mean',data:data},$
          b3:{field:'relhum_700hPa',range:[0,1000],op:'mean',data:data},$
          
          c0:{field:'relhum_500hPa',range:[0, 125],op:'mean',data:data},$
          c1:{field:'relhum_500hPa',range:[0, 250],op:'mean',data:data},$
          c2:{field:'relhum_500hPa',range:[0, 500],op:'mean',data:data},$
          c3:{field:'relhum_500hPa',range:[0,1000],op:'mean',data:data},$
          
          d0:{field:'precipw',range:[0, 125],op:'mean',data:data},$
          d1:{field:'precipw',range:[0, 250],op:'mean',data:data},$
          d2:{field:'precipw',range:[0, 500],op:'mean',data:data},$
          d3:{field:'precipw',range:[0,1000],op:'mean',data:data},$
          
          e0:{field:'t2',range:[0, 125],op:'mean',data:data},$
          e1:{field:'t2',range:[0, 250],op:'mean',data:data},$
          e2:{field:'t2',range:[0, 500],op:'mean',data:data},$
          e3:{field:'t2',range:[0,1000],op:'mean',data:data},$
          
          f0:{field:'q2',range:[0, 125],op:'mean',data:data},$
          f1:{field:'q2',range:[0, 250],op:'mean',data:data},$
          f2:{field:'q2',range:[0, 500],op:'mean',data:data},$
          f3:{field:'q2',range:[0,1000],op:'mean',data:data},$
          
          h0:{field:'speed10',range:[0, 125],op:'mean',data:data},$
          h1:{field:'speed10',range:[0, 250],op:'mean',data:data},$
          h2:{field:'speed10',range:[0, 500],op:'mean',data:data},$
          h3:{field:'speed10',range:[0,1000],op:'mean',data:data},$
          
          g0:{field:'mse2',range:[0, 125],op:'mean',data:data},$
          g1:{field:'mse2',range:[0, 250],op:'mean',data:data},$
          g2:{field:'mse2',range:[0, 500],op:'mean',data:data},$
          g3:{field:'mse2',range:[0,1000],op:'mean',data:data},$
          
          i0:{field:'speed10',range:[0,250],op:'max',data:replicate(!VALUES.D_NAN,1)} $
        }
        
        ivitals = vitals
        ; Fill in vitals from the analysis that equals the fcst valid time.
        ; Often the analysis doesn't exist at the fcst valid time, because the analysis
        ; is often only at 00 UTC, while the forecast valid time may by 6, 12 or 18 UTC.
        ; In this case, fill_vitals will return an empty sting for 'model_file'.
        fill_vitals, mpas, nearestCells, valid_date, valid_time, ivitals, model_file=model_file
        if model_file eq '' then continue
        ; Fill in vitals from the forecast
        fill_vitals, mpas, nearestCells, init_date, valid_time, vitals, model_file=model_file
        if model_file eq '' then continue
        for itag = 0,n_tags(vitals)-1 do printf, lun, vitals.(itag).field+"."+vitals.(itag).op, vitals.(itag).range, vitals.(itag).data, $
          ivitals.(itag).data,vitals.(itag).data-ivitals.(itag).data,format='(A32, I3," - ",I4,"km ", G11.5," ",G11.5," =",G11.5)
      endfor ; points along track
      free_lun, lun
    endforeach; HMF
  endforeach; basin
  
end


function template
  return,  {$
    VERSION   : 1.00000,   $
    DATASTART : 0,   $
    DELIMITER : ' ',       $
    MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '',   $
    FIELDCOUNT: 8L,  $
    FIELDTYPES:   [7, 4, 7, 4, 4, 4, 7, 4] ,  $
    FIELDNAMES:     ['field','rangemin','dash1','rangemax','var1','var2','equals','diff'], $
    FIELDLOCATIONS: indgen(8),$
    FIELDGROUPS:  indgen(8) }
    
end

pro some_plots
  mpas = mpas_mesh('mpas_ep')
  basedir='/glade/scratch/ahijevyc/'+mpas.name+'/'
  rs = list(250) ; disk radii
  ;fus = list(['mse2.mean','moist static energy / C$_p$ ($\circ$ C)'],['speed10.mean','10-m mean wind speed (m/s)'],['q2.mean','2-m mixing ratio (kg/kg)'],['t2.mean','2-m temperature ($\circ$ C)'],['precipw.mean','precipitable water (mm)'],['relhum_850hPa.mean','850hPa relative hum. (fraction or %)'],['relhum_700hPa.mean','700hPa relative hum. (fraction or %)'],['relhum_500hPa.mean','500hPa relative hum. (fraction or %)'])
  fus = list(['speed10.max','10-m max wind speed (m/s)'])
  f_i_d = 'forecast' ; 'forecast' 'analysis' or 'forecast-analysis'
  foreach basin,['WP','AL','EP'] do begin
  
    hit_bias = read_ascii(basedir+mpas.name+'_'+basin+ '_fh.ge.000_hit_bias.txt', template=template())
    miss_bias = read_ascii(basedir+mpas.name+'_'+basin+'_fh.ge.000_miss_bias.txt', template=template())
    false_alarm_bias = read_ascii(basedir+mpas.name+'_'+basin+ '_fh.ge.000_false_alarm_bias.txt', template=template())
    plot_list = list(hit_bias, miss_bias, false_alarm_bias)
    
    foreach fu, fus do begin
      field = fu[0]
      units = fu[1]
      boxes = list()
      tickname=!NULL
      ofile = basedir+mpas.name+'_'+basin+'_'+field+"."+f_i_d+".png"
      
      foreach r, rs do begin
        foreach str, plot_list do begin
          i = where(str.field eq field and str.rangemax eq r, n, /null)
          if f_i_d eq 'forecast' then result = str.var1[i]
          if f_i_d eq 'analysis' then result = str.var2[i]
          if f_i_d eq 'forecast-analysis' then result = str.diff[i]
          boxes.add, result
        endforeach
        tickname = [tickname, string(r,format='(I0)')]
      endforeach
      xloc = indgen(boxes.count())
      h = boxplot(xloc[0:*:plot_list.count()], createboxplotdata(boxes[0:*:plot_list.count()],mean_values=means,outlier_values=outlier_values), $
        title=mpas.name+" "+basin+" "+field,fill_color='sea green',mean_values=means,symbol_means='x',xtickinterval=2,$
        xminor=0,xrange=[-0.5,boxes.count()-1+0.5],xstyle=1,xticklen=0, name='hit', ytitle=f_i_d+" "+units, xtitle='range from observed tropical storm',buffer=1)
      ax = h.axes
      ax[0].tickvalues = findgen(rs.count())*plot_list.count() + (plot_list.count()-1.)/2
      ax[0].tickname = '0-'+tickname+'km'
      ax[2].hide=1
      z = plot(h.xrange,[0,0],overplot=h,name='')
      m = boxplot(xloc[1:*:plot_list.count()], createboxplotdata(boxes[1:*:plot_list.count()],mean_values=means,outlier_values=outlier_values), $
        fill_color='cornflower',mean_values=means,symbol_means='x', overplot=h, name='miss')
      f = boxplot(xloc[2:*:plot_list.count()], createboxplotdata(boxes[2:*:plot_list.count()],mean_values=means,outlier_values=outlier_values), $
        fill_color='brown',mean_values=means,symbol_means='x', overplot=h, name='false alarm')
      l = legend(target=[h,m,f])
      for w=0,boxes.count()-1 do count = text(w, (h.yrange)[1],strtrim(n_elements(boxes[w]),2), align=0.5,$
        font_size=7,/data,clip=0)
      junk = timestamp_text()
      h.window.save, ofile, resolution=150
      print, "created "+ofile
      spawn, 'scp "'+ofile+'" nebula.mmm.ucar.edu:/web/htdocs/imagearchive/mpas/gallery-images/.', result
      print, result
      
    endforeach ; field
  endforeach ; basin
end

function get_color, height
  return, (height-20.)/10.*75+50
end
function get_thick, height
  thick = (height/5.)^1.4
  if !D.NAME eq 'X' then thick = thick/2.9
  return, thick
end
pro atm_CR_analysis, model=model, suffix=suffix, plot_p_threshold=plot_p_threshold


  loadct, 38, /silent
  if !D.NAME eq 'PS' then device, /close, /color, bits=8
  !P.CHARTHICK=1
  !P.THICK=1
  
  if ~keyword_set(model) then model = 'AFWA'
  if ~keyword_set(suffix) then suffix = 'thru08260.50'
  member_req = stregex(suffix, '.*_(mem([0-9]+(\.[0-9]*)?))_0\.[0-9]+$', /FOLD_CASE, /extract, /subexpr)
  model_lead_time = member_req[2]
  
  close, /all
  
  case model of
    'CoSPA' : begin
      nx = 38.
      ny = 28.
      leads = (1+lindgen(6))*3600
    end
    'AFWA' : begin
      nx = 48.
      ny = 28.
      leads = lindgen(31)*3600
    end
  endcase
  if model eq 'AFWA' then leads = ((lindgen(30)+1)*1)*3600
  
  
  n = long(nx*ny)
  binsize = 0.2d
  nbins = round(1+1./binsize)
  bins=(findgen(nbins)+0.5)*binsize
  p_thresholds = bins - 0.5*binsize
  if ~keyword_set(plot_p_threshold) then plot_p_threshold = 1
  
  nleads = n_elements(leads)
  heights = ['30','40','20']
  nheights=n_elements(heights)
  if !D.NAME eq 'PS' then device, ysize=5.5, yoffset=4.5, xsize=7, xoffset=0.75, /inches
  
  dirs = ['EW']
  ndirs = n_elements(dirs)
  paths = '/pecan/ahijevyc/faa/atm/ascii/'+model+['/']
  
  if !D.NAME eq 'PS' then device, filename='/pecan/ahijevyc/faa/atm/ascii/'+model+'/'+model+suffix+string(p_thresholds[plot_p_threshold]*100,format='("_p",I0,".ps")')
  
  
  fieldcount = 5+8*nbins+2
  plate = {$
    version:1,$
    datastart:0L,$
    delimiter:' ',$
    missingvalue:!VALUES.F_NAN,$
    commentsymbol:"",$
    fieldcount:fieldcount,$
    fieldtypes:[7,replicate(5,fieldcount-1)],$
    fieldnames:['filename','valid_hr','n_valid_forecasts','event_thresh','binsize',$
    replicate('hit',nbins),replicate('miss',nbins),replicate('false_alarm',nbins),replicate('corrct_null',nbins),$
    'brier_score','MSE_avg_fcst',$
    replicate('hist_event_prob_fcst',nbins),replicate('hist_obs',nbins),replicate('hist_avg_forecasts',nbins),replicate('obs_count_given_p',nbins)],$
    fieldlocations:lindgen(fieldcount),$
    fieldgroups:   [0, 1, 2, 3, 4, replicate(5,nbins), replicate(5+nbins,nbins), replicate(5+2*nbins,nbins), replicate(5+3*nbins,nbins), 6+3*nbins, 7+3*nbins, $
    replicate(8+3*nbins,nbins), replicate(8+4*nbins,nbins), replicate(8+5*nbins,nbins), replicate(8+6*nbins,nbins)]$
    }
    
  pos= [0.12,0.16,0.83,0.9]
  
  if !D.NAME eq 'X' then begin
    device, decomposed=0
    window,0,xsize=500,ysize=400
    window,1,xsize=500,ysize=400
    window,2,xsize=800,ysize=400
    window,3,xsize=800,ysize=400,ypos=900
    window,4,xsize=500,ysize=400,xpos=1500,ypos=950
    window,5,xsize=500,ysize=400,ypos=460
    window,6,xsize=800,ysize=400,xpos=0,ypos=400
    window,7,xsize=800,ysize=400,xpos=0,ypos=0
    wset,0
  endif
  
  
  brier_score = fltarr(nleads,nheights,ndirs)
  rmse_avg_forecasts   = fltarr(nleads,nheights,ndirs)
  csi = fltarr(nbins,nleads,nheights,ndirs)
  gss = fltarr(nbins,nleads,nheights,ndirs)
  bias= fltarr(nbins,nleads,nheights,ndirs)
  climatological_event_prob = fltarr(nbins,nleads,nheights,ndirs)
  pod = fltarr(nbins,nleads,nheights,ndirs) ; hit rate
  pofd = fltarr(nbins,nleads,nheights,ndirs) ; false alarm rate
  hist_event_prob_fcst = lonarr(nbins,nleads,nheights,ndirs)
  hist_obs = lonarr(nbins,nleads,nheights,ndirs)
  hist_avg_forecasts = lonarr(nbins,nleads,nheights,ndirs)
  obs_count_given_p = fltarr(nbins,nleads, nheights,ndirs)
  csi_EW_NS = fltarr(nbins,nleads,nheights)
  
  
  valid_time_range = [15,16]
  hit_valid_times_x_y         = fltarr(nbins,nheights,ndirs)
  miss_valid_times_x_y        = fltarr(nbins,nheights,ndirs)
  false_alarm_valid_times_x_y = fltarr(nbins,nheights,ndirs)
  corrct_null_valid_times_x_y = fltarr(nbins,nheights,ndirs)
  csi_valid_times_x_y         = fltarr(nbins,nheights,ndirs)
  events_valid_times_x_y      = fltarr(nbins,nheights,ndirs)
  
  
  for ipath = 0, n_elements(paths)-1 do begin
    path = paths[ipath]
    for iheights = 0, nheights-1 do begin
      height = heights[iheights]
      color = get_color(height)
      thick = get_thick(height)
      for idirection = 0,ndirs-1 do begin
        dir = dirs[idirection]
        for ilead = 0, nleads-1 do begin
          lead = strtrim(leads[ilead],2)
          infile = path + model+'_WAF'+height+'_'+lead+'_'+dir+'CR_'+suffix+'.txt'
          if file_test(infile, /zero_length) ne 1 then t = read_ascii(infile,template=plate,count=count) else continue
          validHH = t.valid_hr/10000L
          dates = strmid(file_basename(t.filename,'.ascii'),5,8)
          if n_elements(old_dates) eq 0 then old_dates = dates else old_dates = ([old_dates,dates])[uniq([old_dates,dates],sort([old_dates,dates]))]
          ; you can uncomment these histogram plots and see the distribution of times-of-day or # of ensemble members, but it will mess up your previously drawn plot window.
          if !D.NAME eq 'X' then begin
            wset, 1
            plot, xval, histogram(validHH, locations=xval, min=0, max=23, binsize=1),xstyle=1,psym=10,title=file_basename(infile), position=pos,xtitle='ensemble valid time [hours UTC]',ytitle='count'
            wset, 4
            valid_minus_leadtime_hr = (240+validHH - lead/3600.) mod 24
            plot, xval, histogram(valid_minus_leadtime_hr, locations=xval, min=0, max=23, binsize=1),xstyle=1,psym=10,title=file_basename(infile), position=pos,xtitle='ensemble valid time - ensemble forecast lead time [hr UTC]',ytitle='count'
            wset, 5
            erase
            ;  If suffix is not '', then model_lead_time is based on the member number embedded in the string suffix.
            if model_lead_time ne '' then begin
              model_init_time_hr = (240+validHH - model_lead_time) mod 24
              ; Plot histogram of model initialization times. 
              plot, xval, histogram(model_init_time_hr, locations=xval, min=0, max=23, binsize=1),xstyle=1,psym=10,title=file_basename(infile), position=pos, xtitle="ensemble valid time - model lead time [hr UTC]",ytitle='count'
            endif
            window, 8, xsize = 500, ysize=400, xpos=1000, ypos=1000
            wset, 8
            plot, xval, histogram(t.n_valid_forecasts, locations=xval, min=0, max=15, binsize=1),xstyle=1,psym=10,title=file_basename(infile), position=pos, xtitle="number of members in forecast ensemble",ytitle='count'
            wset, 0
          endif
          ; sanity check. make sure all the probability bin sizes and event thresholds are the same.
          if n_elements(event_thresh) eq 0 then event_thresh = t.event_thresh[0] else if min(t.event_thresh) ne event_thresh or max(t.event_thresh) ne event_thresh then stop
          if min(t.binsize) ne binsize or max(t.binsize) ne binsize then stop
          
          hit  = count gt 1 ? total(t.hit,2) : t.hit
          miss = count gt 1 ? total(t.miss,2) : t.miss
          false_alarm = count gt 1 ? total(t.false_alarm,2) : t.false_alarm
          corrct_null = count gt 1 ? total(t.corrct_null,2) : t.corrct_null
          hist_obs[*,ilead,iheights,idirection] = count gt 1 ? total(t.hist_obs,2) : t.hist_obs
          hist_avg_forecasts[*,ilead,iheights,idirection] = count gt 1 ? total(t.hist_avg_forecasts,2) : t.hist_avg_forecasts
          hist_event_prob_fcst[*,ilead,iheights,idirection] = count gt 1 ? total(t.hist_event_prob_fcst,2) : t.hist_event_prob_fcst
          obs_count_given_p[*,ilead,iheights,idirection] = count gt 1 ? total(t.obs_count_given_p,2) : t.obs_count_given_p
          brier_score[ilead,iheights,idirection] = mean(t.brier_score)
          rmse_avg_forecasts[ilead,iheights,idirection]   = sqrt(mean(t.mse_avg_fcst))
          
          chance = (hit+miss)*(hit+false_alarm)/(hit+miss+false_alarm+corrct_null)
          csi[*,ilead,iheights,idirection] = hit/(hit+miss+false_alarm)
          gss[*,ilead,iheights,idirection] = (hit-chance)/(hit+miss+false_alarm-chance)
          bias[*,ilead,iheights,idirection] = (hit+false_alarm)/(hit+miss)
          pod[*,ilead,iheights,idirection] = hit/(hit+miss)
          pofd[*,ilead,iheights,idirection] = false_alarm/(corrct_null+false_alarm)
          climatological_event_prob[*,ilead,iheights,idirection] = (hit+miss)/(false_alarm+corrct_null)
          if ndirs eq 2 and dir eq 'NS' then csi_EW_NS[*,ilead,iheights] =  csi[*,ilead,iheights,1]-csi[*,ilead,iheights,0]
          
          
          
          ; group chunks of valid time in the range of x to y
          if valid_time_range[0] le valid_time_range[1] then begin
            ivalid_hr = where(validHH ge valid_time_range[0] and validHH le valid_time_range[1], n_valid_hr)
          endif else begin
            ivalid_hr = where(validHH ge valid_time_range[0] or validHH le valid_time_range[1], n_valid_hr)
          endelse
          if n_valid_hr gt 0 then begin
            hit  = n_valid_hr gt 1 ? total(t.hit[*,ivalid_hr],2) : t.hit[*,ivalid_hr]
            miss = n_valid_hr gt 1 ? total(t.miss[*,ivalid_hr],2) : t.miss[*,ivalid_hr]
            false_alarm = n_valid_hr gt 1 ? total(t.false_alarm[*,ivalid_hr],2) : t.false_alarm[*,ivalid_hr]
            corrct_null = n_valid_hr gt 1 ? total(t.corrct_null[*,ivalid_hr],2) : t.corrct_null[*,ivalid_hr]
            hit_valid_times_x_y[*,iheights,idirection] = hit_valid_times_x_y[*,iheights,idirection] + hit
            miss_valid_times_x_y[*,iheights,idirection] = miss_valid_times_x_y[*,iheights,idirection] + miss
            false_alarm_valid_times_x_y[*,iheights,idirection] = false_alarm_valid_times_x_y[*,iheights,idirection] + false_alarm
            corrct_null_valid_times_x_y[*,iheights,idirection] = corrct_null_valid_times_x_y[*,iheights,idirection] + corrct_null
          endif
          
        endfor ; new lead (lead0, lead2, lead6, etc.)
        
        ; Plot CSI, GSS as a function of ensemble lead time
        plot, leads/3600, leads, yrange=[0.0,0.6], ystyle=1, xstyle=1, xtitle='ensemble lead time [h]', $
          xminor=1, ytitle='', title=model+" "+suffix, /nodata, position=pos, charsize=2, charthick=!D.NAME eq 'PS' ? 4 : 2, /noerase
        ;        oplot, leads/3600, rmse_avg_forecasts[*,iheights,idirection], linestyle=1, thick=thick, psym=-1
        ;        xyouts, leads[nleads-1]/3600, rmse_avg_forecasts[nleads-1,iheights,idirection], height+dir+" RMSE"
        oplot, leads/3600, csi[plot_p_threshold,*,iheights,idirection], linestyle=iheights, thick=thick, psym=-1,color=color
        xyouts, leads[nleads-1]/3600, csi[plot_p_threshold,nleads-1,iheights,idirection], height+"kft "+dir+" CSI"+(ipath eq 1 ? ' old' : ' ')+string(100*p_thresholds[plot_p_threshold],format='("!Cp=",I0,"%")'),color=color,charthick=!D.NAME eq 'PS' ? 2 : 1
;        oplot, leads/3600, gss[plot_p_threshold,*,iheights,idirection], linestyle=iheights, thick=thick*1.1,psym=-1,color=color
;        xyouts, leads[0]/3600, gss[plot_p_threshold,0,iheights,idirection], height+dir+" GSS"+(ipath eq 1 ? ' old' : ' ')+string(100*p_thresholds[plot_p_threshold],format='("!Cp=",I0,"%")'), align=1, color=color,charthick=!D.NAME eq 'PS' ? 2 : 1
        
        ; I think this is calculated outside the "new lead" loop because this variable is a compilation of ALL leads.
        inonzero = where(hit_valid_times_x_y+miss_valid_times_x_y+false_alarm_valid_times_x_y ne 0, nnonzero)
        if nnonzero gt 0 then csi_valid_times_x_y[inonzero] = hit_valid_times_x_y[inonzero]/(hit_valid_times_x_y[inonzero]+miss_valid_times_x_y[inonzero]+false_alarm_valid_times_x_y[inonzero])
        
      endfor ; new direction 'NS' or 'EW'
    endfor ; new waf height '20' '30' or '40'
  endfor ; new path '' vs 'old'
  
  
  
  if !D.NAME eq 'X' then wset,1
  
  plot, leads/3600, leads, yrange=[0.0,2],ystyle=1, xstyle=1, xtitle='ensemble lead time [h]', $,
  xminor=1, ytitle='',title=model+" "+suffix,/nodata,position=pos, charsize=2, charthick=!D.NAME eq 'PS' ? 4 : 2
  bias_scale = 1
  oplot, [leads[0],leads[nleads-1]]/3600, replicate(bias_scale,2), linestyle=2
  for iheights = 0, nheights-1 do begin
    height = heights[iheights]
    color = get_color(height)
    thick = get_thick(height)
    for idirection = 0,ndirs-1 do begin
      dir = dirs[idirection]
      oplot, leads/3600, brier_score[*,iheights,idirection], linestyle=0, thick=1, psym=-1, color=color
      xyouts, leads[nleads-1]/3600, brier_score[nleads-1,iheights,idirection], height+"kft "+dir+ " Brier", charthick=!D.NAME eq 'PS' ? 2 : 1
      oplot, leads/3600, bias[plot_p_threshold,*,iheights,idirection]*bias_scale, linestyle=1, thick=thick, psym=-1, color=color
      xyouts, leads[nleads-1]/3600, bias[plot_p_threshold,nleads-1,iheights,idirection]*bias_scale, $
        height+"kft "+dir+"!Cbias*"+string(bias_scale,100*p_thresholds[plot_p_threshold],format='(F3.1,"!Cp=",I0,"%")'), charthick=!D.NAME eq 'PS' ? 2 : 1,color=color
    endfor ; new direction 'NS' or 'EW'
  endfor ; new waf height '20' '30' or '40'
  
  if !D.NAME eq 'X' then wset,4
  
  plot, leads/3600, leads, yrange=[-0.02,.02],ystyle=1, xstyle=1, xtitle='ensemble lead time [h]', $,
  xminor=1, ytitle='',title=model+ " " + suffix,/nodata,position=pos, charsize=2, charthick=!D.NAME eq 'PS' ? 4 : 2
  plots, [leads[0],leads[nleads-1]]/3600, [0,0], thick=2
  for iheights = 0,nheights-1 do begin
    height=heights[iheights]
    color = get_color(height)
    oplot, leads/3600, csi_EW_NS[plot_p_threshold,*,iheights], linestyle=iheights, thick=get_thick(height),psym=-1,color=color
    xyouts, leads[nleads-1]/3600, csi_EW_NS[plot_p_threshold,nleads-1,iheights], height+"kft EW-NS!Cp="+string(100*p_thresholds[plot_p_threshold],format='(I0,"%")'), align=0,color=color,charthick=!D.NAME eq 'PS' ? 2 : 1
  endfor
  
  hpos0 = 0.7*pos[0]
  dx = (0.95-hpos0)/nleads
  dy = (pos[3]-pos[1])/3.
  hpos1 = 0.7*pos[1] + dy*(3-nheights)
  if !D.NAME eq 'X' then wset,2
  erase
  xyouts, 0.5*(pos[2]+pos[0]), pos[3], model+" "+suffix+' observed capacity' , align=0.5,/norm,charsize=1.5
  for iheights = 0, nheights-1 do begin
    height = heights[iheights]
    color = get_color(height)
    for idirection = 0,ndirs-1 do begin
      dir = dirs[idirection]
      for ilead = 0, nleads-1 do begin
        hpos = [hpos0+dx*ilead, hpos1+dy*iheights, hpos0+dx*(0.8+ilead), hpos1+dy*(0.631+iheights)]
        h = hist_obs[*,ilead,iheights,idirection]
        plot, bins, h, yrange=[1e0,1e7], position=hpos, /norm, /noerase, /ylog, psym=10, charsize=2.606/nleads, xtitle = iheights eq 0 ? 'forecasted!Cevent prob' : '',$
          title = string(height,dir,leads[ilead]/3600,format='(A,"kft ",A," ",I0,"h")'), ytitle = ilead eq 0 ? 'count' : ''
        for ibin = 0, nbins-1 do polyfill, [replicate(bins[ibin]-binsize/2.,2),replicate(bins[ibin]+binsize/2.,2)],$
          [!Y.RANGE[0],replicate(h[ibin],2),!Y.RANGE[0]],color=color,noclip=0
        xyouts, bins, h, string(h, format='(I0)'), charsize=1.92/nleads, align=0.5, charthick=0.5
      endfor
    endfor
  endfor
  if !D.NAME eq 'X' then wset,3
  erase
  xyouts, 0.5*(pos[2]+pos[0]), pos[3], model+" "+suffix+' average forecasted capacity', align=0.5,/norm,charsize=1.5
  for iheights = 0, nheights-1 do begin
    height = heights[iheights]
    color = get_color(height)
    for idirection = 0,ndirs-1 do begin
      dir = dirs[idirection]
      for ilead = 0, nleads-1 do begin
        hpos = [hpos0+dx*ilead, hpos1+dy*iheights, hpos0+dx*(0.8+ilead), hpos1+dy*(0.631+iheights)]
        h = hist_avg_forecasts[*,ilead,iheights,idirection]
        plot, bins, h, yrange=[1e0,1e7], position=hpos, /norm, /noerase, /ylog, psym=10, charsize=2.606/nleads, xtitle = iheights eq 0 ? 'forecasted!Cevent prob' : '',$
          title = string(height,dir,leads[ilead]/3600,format='(A,"kft ",A," ",I0,"h")'), ytitle = ilead eq 0 ? 'count' : ''
        for ibin = 0, nbins-1 do polyfill, [replicate(bins[ibin]-binsize/2.,2),replicate(bins[ibin]+binsize/2.,2)],$
          [!Y.RANGE[0],replicate(h[ibin],2),!Y.RANGE[0]],color=color,noclip=0
        xyouts, bins, h, string(h, format='(I0)'), charsize=1.92/nleads, align=0.5, charthick=0.5
      endfor
    endfor
  endfor
  if !D.NAME eq 'X' then wset,6
  erase
  xyouts, 0.5*(pos[2]+pos[0]), pos[3], model+" "+suffix+' probability of capacity <='+string(event_thresh,format='(F4.2)'), align=0.5,/norm,charsize=1.5
  for iheights = 0, nheights-1 do begin
    height = heights[iheights]
    color = get_color(height)
    for idirection = 0,ndirs-1 do begin
      dir = dirs[idirection]
      for ilead = 0, nleads-1 do begin
        hpos = [hpos0+dx*ilead, hpos1+dy*iheights, hpos0+dx*(0.8+ilead), hpos1+dy*(0.631+iheights)]
        h = hist_event_prob_fcst[*,ilead,iheights,idirection]
        plot, bins, h, yrange=[1e0,1e7], position=hpos, /norm, /noerase, /ylog, psym=10, charsize=2.606/nleads, xtitle = iheights eq 0 ? 'forecasted!Cevent prob' : '', $
          title = string(height,dir,leads[ilead]/3600,format='(A,"kft ",A," ",I0,"h")'), ytitle = ilead eq 0 ? 'count' : ''
        for ibin = 0, nbins-1 do polyfill, [replicate(bins[ibin]-binsize/2.,2),replicate(bins[ibin]+binsize/2.,2)],$
          [!Y.RANGE[0],replicate(h[ibin],2),!Y.RANGE[0]],color=color,noclip=0
        ; print Brier score in upper-left corner
        ;        xyouts, 0.05, 1e7, string(brier_score[ilead,iheights,idirection], format = '("brier score!C",F4.2)'), align=0, charsize=5.1/(nleads+nheights)
        xyouts, bins, h, string(h, format='(I0)'), charsize=1.92/nleads, align=0.5, charthick=0.5
        inonzero = where(h ne 0, nnonzero)
        if nnonzero gt 0 then plot, p_thresholds[inonzero], obs_count_given_p[inonzero,ilead,iheights,idirection]/h[inonzero], /noerase, position=hpos, yrange=[0,1], ystyle=5, psym=-4, symsize=6./nleads, linestyle=2, thick=18./nleads, xstyle=5, xrange=!X.CRANGE
        oplot, p_thresholds, climatological_event_prob[*,ilead,iheights,idirection], linestyle=1, thick=2
        oplot, [0.,1],[0.,1.]
        if ilead eq nleads-1 then axis, !X.CRANGE[1],0,0,yax=1,/data, ytitle='observed event freq.!Cfor given fcst prob.', charsize=2.606/nleads
      endfor
    endfor
  endfor
  if !D.NAME eq 'X' then wset, 7
  erase
  xyouts, 0.5*(pos[2]+hpos0), pos[3], model+" "+suffix+' ROC curve probability of capacity <='+string(event_thresh,format='(F4.2)'), align=0.5,/norm,charsize=1.5
  for iheights = 0, nheights-1 do begin
    height = heights[iheights]
    color = get_color(height)
    for idirection = 0,ndirs-1 do begin
      dir = dirs[idirection]
      for ilead = 0, nleads-1 do begin
        hpos = [hpos0+dx*ilead, hpos1+dy*iheights, hpos0+dx*(0.8+ilead), hpos1+dy*(0.631+iheights)]
        plot, [0,1], [0,1], xrange=[0,1], yrange=[0,1], position=hpos, /norm, /noerase, charsize=2.606/nleads, xtitle = iheights eq 0 ? 'prob of!Cfalse detection' : '', $
          ytitle = ilead eq 0 ? 'prob of detection' : '', title = string(height,dir,leads[ilead]/3600,format='(A,"kft ",A," ",I0,"h")')
        oplot, pofd[*,ilead,iheights,idirection],pod[*,ilead,iheights,idirection], psym=-4, color=color, thick=4, symsize=0.18
        xyouts, pofd[*,ilead,iheights,idirection],pod[*,ilead,iheights,idirection],string(p_thresholds*100,format='("p=",I0,"%")'), align=0.5, charsize=3.07/(nleads+nheights), charthick=!P.CHARTHICK/3.
        area = 0.
        for ibin = 1, nbins-1 do area = area + 0.5*(pod[ibin-1,ilead,iheights,idirection]+pod[ibin,ilead,iheights,idirection])*(pofd[ibin-1,ilead,iheights,idirection]-pofd[ibin,ilead,iheights,idirection])
        xyouts, 0.05, 0.9, string(area*100.,format='(I0,"% area!Cunder curve")'), align=0, charsize=4.1/(nleads+nheights)
      endfor
    endfor ; new direction 'NS' or 'EW'
  endfor ; new waf height '20' '30' or '40'
  
  
  
  
  ; Plot scores as function of ensemble valid time (0-23 UTC)
  if !D.NAME eq 'X' then begin
    wset,5
  endif
  plot_lead= (where(leads eq 6L*3600,np))[0]
  if np eq 0 then plot_lead = min([3,nleads-1])
  plot, indgen(24), indgen(24), yrange=[0.0,0.6], ystyle=1, xstyle=1, xtitle='ensemble valid time UTC [h]', $,
  xminor=1, ytitle='',title=model+" "+suffix,$
    /nodata, position=pos, charsize=2, charthick=!D.NAME eq 'PS' ? 4 : 2
  brier_score = fltarr(nleads,nheights,ndirs,24)
  rmse_avg_forecasts   = fltarr(nleads,nheights,ndirs,24)
  csi = fltarr(nbins,nleads,nheights,ndirs,24)
  gss = fltarr(nbins,nleads,nheights,ndirs,24)
  bias= fltarr(nbins,nleads,nheights,ndirs,24)
  hist_event_prob_fcst = lonarr(nbins,nleads,nheights,ndirs,24)
  hist_obs = lonarr(nbins,nleads,nheights,ndirs,24)
  hist_avg_forecasts = lonarr(nbins,nleads,nheights,ndirs,24)
  
  ; Initialize variables to hold scores as a function of date. Later we filter them to only include a particular range of valid times (12-00 UTC)
  ndates = n_elements(old_dates)
  hit_all_leads_12_00         = fltarr(nbins,nheights,ndirs,ndates)
  miss_all_leads_12_00        = fltarr(nbins,nheights,ndirs,ndates)
  false_alarm_all_leads_12_00 = fltarr(nbins,nheights,ndirs,ndates)
  corrct_null_all_leads_12_00 = fltarr(nbins,nheights,ndirs,ndates)
  csi_all_leads_12_00         = fltarr(nbins,nheights,ndirs,ndates)
  events_all_leads_12_00      = fltarr(nbins,nheights,ndirs,ndates)
  
  for ipath = 0, n_elements(paths)-1 do begin
    path = paths[ipath]
    for iheights = 0, nheights-1 do begin
      height = heights[iheights]
      color=get_color(height)
      thick = get_thick(height)
      for idirection = 0,ndirs-1 do begin
        dir = dirs[idirection]
        for ilead = 0, nleads-1 do begin
          lead = strtrim(leads[ilead],2)
          infile = path + model+'_WAF'+height+'_'+lead+'_'+dir+'CR_'+suffix+'.txt'
          if file_test(infile, /zero_length) ne 1 then t = read_ascii(infile,template=plate,count=count) else continue
          validHH = t.valid_hr/10000L
          for valid_hr = 0, 23 do begin
            ivalid_hr = where(validHH eq valid_hr, n_valid_hr)
            if n_valid_hr gt 0 then begin
              hit  = n_valid_hr gt 1 ? total(t.hit[*,ivalid_hr],2) : t.hit[*,ivalid_hr]
              miss = n_valid_hr gt 1 ? total(t.miss[*,ivalid_hr],2) : t.miss[*,ivalid_hr]
              false_alarm = n_valid_hr gt 1 ? total(t.false_alarm[*,ivalid_hr],2) : t.false_alarm[*,ivalid_hr]
              corrct_null = n_valid_hr gt 1 ? total(t.corrct_null[*,ivalid_hr],2) : t.corrct_null[*,ivalid_hr]
              
              for idate = 0, ndates-1 do begin
                valid_date = old_dates[idate]
                dates = strmid(file_basename(t.filename,'.ascii'),5,8)
                ifound_date = where(dates eq valid_date and validHH eq valid_hr, nfound_dates)
                ; Find scores as a function of date but only include a particular range of valid times (12-00 UTC)
                if valid_hr eq 0 or valid_hr ge 12 and nfound_dates gt 0 then begin
                  ;                  print, 'found line for '+valid_date+": "+t.filename[ifound_date]
                  hit_all_leads_12_00[*,iheights,idirection,idate] = hit_all_leads_12_00[*,iheights,idirection,idate] + t.hit[*,ifound_date]
                  miss_all_leads_12_00[*,iheights,idirection,idate] = miss_all_leads_12_00[*,iheights,idirection,idate] + t.miss[*,ifound_date]
                  false_alarm_all_leads_12_00[*,iheights,idirection,idate] = false_alarm_all_leads_12_00[*,iheights,idirection,idate] + t.false_alarm[*,ifound_date]
                  corrct_null_all_leads_12_00[*,iheights,idirection,idate] = corrct_null_all_leads_12_00[*,iheights,idirection,idate] + t.corrct_null[*,ifound_date]
                endif
              endfor
              
              
              hist_obs[*,ilead,iheights,idirection] = n_valid_hr gt 1 ? total(t.hist_obs[*,ivalid_hr],2) : t.hist_obs[*,ivalid_hr]
              hist_avg_forecasts[*,ilead,iheights,idirection] = n_valid_hr gt 1 ? total(t.hist_avg_forecasts[*,ivalid_hr],2) : t.hist_avg_forecasts[*,ivalid_hr]
              hist_event_prob_fcst[*,ilead,iheights,idirection] = n_valid_hr gt 1 ? total(t.hist_event_prob_fcst[*,ivalid_hr],2) : t.hist_event_prob_fcst[*,ivalid_hr]
              obs_count_given_p[*,ilead,iheights,idirection] = n_valid_hr gt 1 ? total(t.obs_count_given_p[*,ivalid_hr],2) : t.obs_count_given_p[*,ivalid_hr]
              brier_score[ilead,iheights,idirection,valid_hr] = mean(t.brier_score[*,ivalid_hr])
              rmse_avg_forecasts[ilead,iheights,idirection,valid_hr]   = sqrt(mean(t.mse_avg_fcst[*,ivalid_hr]))
              
              chance = (hit+miss)*(hit+false_alarm)/(hit+miss+false_alarm+corrct_null)
              csi[*,ilead,iheights,idirection,valid_hr] = hit/(hit+miss+false_alarm)
              gss[*,ilead,iheights,idirection,valid_hr] = (hit-chance)/(hit+miss+false_alarm-chance)
              bias[*,ilead,iheights,idirection,valid_hr] = (hit+false_alarm)/(hit+miss)
              
            endif
          endfor ; new time of day (valid_hr)
          
          
        endfor ; new lead (lead0, lead2, lead6, etc.)
        
        ; Print a text file for each p_threshold containing a list of dates sorted by CSI
        for ibin=0,nbins-1 do begin
          pintofile = path + 'csi_all_leads_12_00_WAF'+height+'_'+dir+'CR_'+string(p_thresholds[ibin]*100,format='(I0,"pct.txt")')
          openw, pintolun, pintofile, /get_lun
          printf, pintolun, string(p_thresholds[ibin]*100,format='("p=",I3,"%")')+' height = ', height, ' direction = ', dir, ' path=',path
          for idate=0, ndates-1 do begin
            hit_miss_fa = (hit_all_leads_12_00[*,iheights,idirection,idate]+miss_all_leads_12_00[*,iheights,idirection,idate]+false_alarm_all_leads_12_00[*,iheights,idirection,idate])
            ihit_miss_fa = where(hit_miss_fa gt 0, nhit_miss_fa)
            if nhit_miss_fa gt 0 then csi_all_leads_12_00[ihit_miss_fa,iheights,idirection,idate] = hit_all_leads_12_00[ihit_miss_fa,iheights,idirection,idate]/hit_miss_fa[ihit_miss_fa]
            if nhit_miss_fa gt 0 then events_all_leads_12_00[ihit_miss_fa,iheights,idirection,idate] = hit_all_leads_12_00[*,iheights,idirection,idate]+miss_all_leads_12_00[*,iheights,idirection,idate]
          endfor
          iorder = reverse(sort(csi_all_leads_12_00[ibin,iheights,idirection,*]))
          for idate=0,ndates-1 do printf, pintolun, ((csi_all_leads_12_00[ibin,iheights,idirection,*])[iorder])[idate], $
            (old_dates[iorder])[idate],            ((events_all_leads_12_00[ibin,iheights,idirection,*])[iorder])[idate], $
            format='(F6.3," ",A," ",I4)'
          free_lun, pintolun
        ;          print, 'made '+pintofile
        endfor
        
        ; Plot scores as function of valid time for a particular lead time.
        oplot, indgen(24), csi[plot_p_threshold,plot_lead,iheights,idirection,*], linestyle=iheights, thick=thick,psym=-1,color=color
        xyouts, 23, csi[plot_p_threshold,plot_lead,iheights,idirection,23], height+dir+" CSI"+(ipath eq 1 ? ' old' : ' ')+$
          string(leads[plot_lead]/3600, 100*p_thresholds[plot_p_threshold],format='("!Clead time ",I0,"h!Cp=",I0,"%")'),$
          color=color, charthick=!D.NAME eq 'PS' ? 2 : 1
          
        ; Plot scores filtered by only including data from a particular range of valid times.
        ; Considered plotting on first window (with CSI as function of ensemble lead time), but wrong x-axis
        ivalid_hour = valid_time_range[0]
        for ivalid_hour = valid_time_range[0], valid_time_range[1] ge valid_time_range[0] ? valid_time_range[1] : valid_time_range[1]+24 do $
          plots, ivalid_hour mod 24, csi_valid_times_x_y[plot_p_threshold,iheights,idirection], linestyle=iheights, thick=thick, psym=1, color=color
        xyouts, mean(valid_time_range), csi_valid_times_x_y[plot_p_threshold,iheights,idirection], $
          string(valid_time_range[0],valid_time_range[1],format='("valid ",I0,"-",I0)'), color=color, align=0.5
        
      endfor ; new direction 'NS' or 'EW'
    endfor ; new waf height '20' '30' or '40'
  endfor ; new path '' vs 'old'
  
  
  
  if !D.NAME eq 'PS' then device, /close
end
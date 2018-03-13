pro do_image, x, map, lat, lon, title, nolabel=nolabel
  ; if nolabel=1, don't write the value in the grid box
  if ~keyword_set(nolabel) then nolabel=0
  lon2D = map.lon1 + map.dlon*(lindgen(map.nx)#replicate(1,map.ny))
  lat2D = map.lat1 + map.dlat*(lindgen(map.ny)##replicate(1,map.nx))
  
  limit = [map.lat1-map.dlat/2.,map.lon1-map.dlon/2.,max(lat)+map.dlat/2.,max(lon)+map.dlon/2.]
  dlat = map.dlat
  dlon = map.dlon
  labels = x*100
  pos = [0.1,0.1,0.9,0.9]
  levels = 100*[0, 0.01, 0.2, 0.4, 0.61, 0.81, 1.01]
  colors = [0,   70, 146, 189, 205, 254, 255]
  precision = 100
  color = lonarr(n_elements(x))
  for i = 0, n_elements(levels)-2 do begin
    c = where(round(labels*precision) ge round(levels[i]*precision) and round(labels*precision) lt round(levels[i+1]*precision), n)
    ;    print, "if x > "+string(long(levels[i]*precision), format='(I0)')+ " and x <= "+string(long(levels[i+1]*precision), format='(I0)')+", color = "+string(colors[i], format='(I0)')
    if n gt 0 then color[c] = colors[i]
  endfor
  
  lat_font = -0.13
  
  map_set, 0, -90, limit=limit, position=pos, /usa, /noborder
  for ibox = 0, n_elements(x)-1 do begin
    if x[ibox] gt 0 then polyfill, [lon[ibox]-dlon/2.,lon[ibox]+dlon/2.,lon[ibox]+dlon/2.,lon[ibox]-dlon/2.],$
      [lat[ibox]-dlat/2.,lat[ibox]-dlat/2.,lat[ibox]+dlat/2.,lat[ibox]+dlat/2.],$
      x[ibox], color=color[ibox]
    if x[ibox] gt 0 and nolabel eq 0 then xyouts, lon[ibox], lat[ibox]+lat_font, string(labels[ibox], format='(I0)'), align=0.5,$
      charsize=!D.NAME eq 'X' ? 1 : 0.4
  endfor
  for ibox = 0, n_elements(lon2D)-1 do plots, lon2D[ibox], lat2D[ibox], psym=map.is_covered[ibox] eq 1 ? 3 : 7
  xyouts, pos[1],pos[3]+(1-pos[3])*0.8, title, align=0, /norm, charsize=0.84
  xyouts, pos[3],pos[3]+(1-pos[3])*0.8, string(mean(labels),n_elements(x), format='("mean: ",F6.2," n=",I0)'),align=1, /norm, charsize=0.84
  map_grid, /box, charsize=0.75
  if nolabel eq 0 then contourbar, levels, colors, charsize=0.91
  erase
  return
end
pro atm_read_ascii
  force_new=0
  loadct, 39
  if !D.NAME eq 'PS' then device, /color, bits=8, /close
  capacity_thresh = 0.50 ; an event is when capacity is less or equal to this.
  
  model = 'CoSPA' 
  case model of
    'CoSPA' : begin
      map = {$
        lon1 : -109.433,$
        lat1 : 26.3162,$
        dlon : 1.146485,$
        dlat : 0.878181,$
        nx : 38.,$
        ny : 28.,$
        is_covered : fltarr(38,28)$
        }
      leads   = lindgen(7)*3600
      ; You can request specific members of the ensemble, like MEM6.5 or MEM1
      ; yes, the 3 has a .0 on the end of it. But has no members
      members = '_MEM' + ['8','7.5','6.5','6','5.5','5','4.5','4','3.5','2.5','2', '1.5','1','0.5']+'_'
    end
    'AFWA' : begin
      map = {$
        lon1 : -119.657,$
        lat1 : 24.6865,$
        dlon : 1.07791,$
        dlat : 0.890308,$
        nx : 48.,$
        ny : 28.,$
        is_covered : fltarr(48,28)$
        }
      leads   = lindgen(31)*3600
      members = string(indgen(12)+1, format='("_mem",I0,"_")')
      print, 'REMEMBER: run "clean-out_afwa_beforeJul7.csh" before processing new data'
    end
  endcase
  ; request each member in the members list separately, as defined earlier, but first do all of them together (member = '')
;  members = ['', members]
  members = ['']

  nmembers = n_elements(members)
  ; These x and y variables do not exactly match the x- and y- indices in the .ascii files. They are in the same order, but they are
  ; one less. The ascii files use indices that start at one, not zero.  But these zero-based variables work out to calculate lat and lon.
  ; Later on, we make sure the x and y indices are in the order we expect in the .ascii file.
  x = lindgen(map.nx)#replicate(1,map.ny)
  y = lindgen(map.ny)##replicate(1,map.nx)
  lon = map.lon1 + map.dlon*x
  lat = map.lat1 + map.dlat*y
  is_covered = radar_coverage(lat, lon, 220)
  map.is_covered = is_covered
  
  
  n = long(map.nx*map.ny)
  path = '/pecan/ahijevyc/faa/atm/ascii/'+model+'/'
  binsize=0.2d
  nbins = round(1./binsize+1)
  for ileads = 0, n_elements(leads)-1 do begin
    lead = strtrim(leads[ileads],2)
    heights = [ '20','30','40']
    for iheights = 0, n_elements(heights)-1 do begin
      height = heights[iheights]
      dirs = ['EW','NS']
      for idirection = 0,n_elements(dirs)-1 do begin
        dir = dirs[idirection]
        for imember = 0, nmembers-1 do begin
          member = members[imember]
;          suffix = 'CR_thru0826'+member+ string(capacity_thresh, format='(F4.2)')+'.txt'
          suffix = 'CR_0805'+member+ string(capacity_thresh, format='(F4.2)')+'.txt'
          openw, lun, path + model+'_WAF'+height+'_'+lead+'_'+dir+suffix, /get_lun
          
;          files = file_search(path + 'WAF_'+height+'/100/70/'+lead+'/'+dir+'CR_2011'+['070[89]','07[123]?','08[01]?','082[0123456]']+'*.ascii', count = nfiles)
          files = file_search(path + 'WAF_'+height+'/100/70/'+lead+'/'+dir+'CR_20110805*.ascii', count = nfiles)
          for ifile = 0, nfiles-1 do begin
            file = files[ifile]
            savfile = strmid(file,0,strlen(file)-6) +'.sav'
            if file_test(savfile) and force_new eq 0 then restore, savfile else begin
              t = read_ascii(file, num_records=0, data_start=1, header=header)
              header = strmid(header,2); shave off first 2 chars, which should be "# "
              fields = strsplit(header, /extract, count=nfields)
              fields = idl_validname(fields, /convert_all)
              ; this ascii template is formed after finding out what 'nfields' equals. so you can't put it at the start of the program.
              plate = {$
                version:1,$
                datastart:0L,$
                delimiter:' ',$
                missingvalue:!VALUES.F_NAN,$
                commentsymbol:"#",$
                fieldcount:nfields,$
                fieldtypes:replicate(5,nfields),$
                fieldnames:[fields[0:7],replicate('FCST',nfields-8)],$
                fieldlocations:lindgen(nfields),$
                fieldgroups:   [lindgen(8),replicate(9,nfields-8)]$
                }
                
              t = read_ascii(file, template=plate)
              save, t, nfields, fields, plate, filename=savfile
              print, 'saved '+savfile
            endelse
            
            valid_hr = t.time
            
            for ivalid_hr = 0, 23 do begin
              req_valid_hr = string(ivalid_hr, format='(I2.2,"0000")')
              ireq_valid_hr = where(valid_hr eq req_valid_hr, n_req_valid_hr)
              if n_req_valid_hr eq 0 then continue

              x = t.x_index[ireq_valid_hr]
              y = t.y_index[ireq_valid_hr]
              lon = map.lon1 + map.dlon*(x-1)
              lat = map.lat1 + map.dlat*(y-1)
              ; We make sure the x and y indices are in the order we expect in the .ascii file.
              xtest = 1+lindgen(map.nx)#replicate(1,map.ny); note, we add one because the x indices start at 1, not zero.
              ytest = 1+lindgen(map.ny)##replicate(1,map.nx); same with y.
              if array_equal(x, xtest) ne 1 then stop
              if array_equal(y, ytest) ne 1 then stop

              ; used to be confused about where to apply radar coverage. I think it is here.
              ireq_valid_hr = ireq_valid_hr[where(is_covered eq 1, n_req_valid_hr)]
              if n_req_valid_hr eq 0 then stop

              ; if EW minus NS obs does not exist create it - remember by file and time of day
              if ifile+ivalid_hr+idirection+imember eq 0 then EW_NS_obs = replicate(!VALUES.F_NAN,nfiles,24,nmembers,n_req_valid_hr)
              nmem = nfields-8
              date = t.date[ireq_valid_hr]
              if date[0] eq '20110700' then stop ; sanity check - if read as floats, date is wrong
              
              leadtimes  = t.lead_time_sec_[ireq_valid_hr]
              waf_thresh = t.threshold[ireq_valid_hr]
              box_size   = t.box_size_km_[ireq_valid_hr]
              obs  = t.obs[ireq_valid_hr]
              lon  = lon[ireq_valid_hr]
              lat  = lat[ireq_valid_hr]
              ; if this is the first direction (EW) then save obs. If 2nd direction (NS) then subtract obs from it.
              EW_NS_obs[ifile,ivalid_hr,imember,*] = idirection eq 0 ? obs : EW_NS_obs[ifile,ivalid_hr,imember,*] - obs
              if idirection eq 1 then begin
                ; if you find a lot of suspicious minus ones, the program died in the middle of the day and NS ascii file has more times than EW.
                isuspicious = where(EW_NS_obs[ifile,ivalid_hr,*] eq -1, nsuspicious)
                if nsuspicious gt 100 then print, file + ' is suspicious. Does it have '+req_valid_hr+' and EW version does not?'
              endif
              member_req = stregex(suffix, '.*_(mem[0-9]+(\.[0-9]*)?)_0\.[0-9]+\.txt$', /FOLD_CASE, /extract, /subexpr)
              if member_req[1] ne '' then begin
                imember_req = where(fields[8:*] eq idl_validname(member_req[1], /convert_all), nmem_match)
                if nmem_match eq 0 then begin
                  print, 'requested_fcst = '+member_req[1]+" not found in "+strjoin(fields[8:*]," ")
                  continue
                endif
                fcst = t.fcst[imember_req[0],ireq_valid_hr]
              endif else fcst = t.fcst[*,ireq_valid_hr]
              
              ; As of 20110817, some rows of NS CoSPA capacity are mysteriously > 1.
              iwacko = where(obs gt 1, nwacko)
              if nwacko gt 0 then obs[iwacko] = 1d
              iwacko = where(fcst gt 1, nwacko)
              if nwacko gt 0 then fcst[iwacko] = 1d
              
              if max(fcst) eq -1 then continue
              invalid_obs      = obs eq -1
              if total(invalid_obs) gt 0 then begin
                print, "found ",total(invalid_obs)," invalid obs with ",file
                continue
              endif
              valid_forecast = fcst ne -1
              n_valid_forecasts = total(valid_forecast, 1)
              if min(n_valid_forecasts) ne max(n_valid_forecasts) then stop
              
              event_obs     = obs le capacity_thresh
              event_not_obs = obs gt capacity_thresh
              event_fcst   = fcst le capacity_thresh
              avg_forecasts   = total(      fcst*valid_forecast,1)/n_valid_forecasts
              event_prob_fcst = total(event_fcst*valid_forecast,1)/n_valid_forecasts
;              if ivalid_hr eq 12 and height eq 30 and max(event_prob_fcst) eq 1 then stop
              hits         = fltarr(nbins,n_req_valid_hr)
              misses       = fltarr(nbins,n_req_valid_hr)
              false_alarms = fltarr(nbins,n_req_valid_hr)
              corrct_nulls = fltarr(nbins,n_req_valid_hr)
              csi          = replicate(!VALUES.F_NAN,nbins,n_req_valid_hr)
              obs_count_given_p = fltarr(nbins)
              
              for ibin = 0, nbins-1 do begin
                prob_thresh = ibin*binsize
                hits[ibin,*]         = event_obs and event_prob_fcst ge prob_thresh
                misses[ibin,*]       = event_obs and event_prob_fcst lt prob_thresh
                false_alarms[ibin,*] = event_not_obs and event_prob_fcst ge prob_thresh
                corrct_nulls[ibin,*] = event_not_obs and event_prob_fcst lt prob_thresh
              endfor
              nhits = total(hits,2)
              nmiss = total(misses,2)
              nfalse_alarm = total(false_alarms,2)
              ncorrct_null = total(corrct_nulls,2)
              if array_equal(nhits + nmiss + nfalse_alarm + ncorrct_null, replicate(n_req_valid_hr,nbins)) ne 1 then stop
              igood = where(nhits+nfalse_alarm+nmiss ne 0, ngood)
              if ngood gt 0 then csi[igood] = float(nhits[igood])/(nhits+nfalse_alarm+nmiss)[igood]
              hmfn = .3*hits + .1*misses + .9*false_alarms
              hist_event_prob_fcst = histogram(event_prob_fcst, min=0d, max=1d, binsize=double(binsize), reverse_indices = r)
              for ibin = 0, nbins-1 do if r[ibin] ne r[ibin+1] then obs_count_given_p[ibin] = total(event_obs[r(r[ibin]:r[ibin+1]-1)])
              hist_obs             = histogram(obs, min=0d, max=1d, binsize=double(binsize)); critical for binsize and min and max to be same type as obs (double)
              hist_avg_forecasts   = histogram(avg_forecasts,   min=0d, max=1d, binsize=double(binsize))
              if !D.window ne -1 then wdelete, !D.window
              title = string(model,height,dir,date[0],req_valid_hr,format='("!C",A," ",A,",000ft ",A,"!C",I8.8," ",I6.6)') + $
                ' leadtime:'+strtrim(long(leadtimes[0]),2)+" capacity thresh:"+$
                string(capacity_thresh, member, n_valid_forecasts[0], format='(F4.2," ",A," Members:",I0)')
              ; mean(SE_event_prob_fcst) is same as Brier score
              brier_score = mean((event_obs-event_prob_fcst)^2.)
              SE_avg_fcst = (obs-avg_forecasts)^2.
              if date[0] eq 20110805 and ivalid_hr eq 12 and height eq '30' and lead ge '43200' then begin
                do_image, 1-obs, map, lat, lon, '1-obs (capacity reduction)' + title
                do_image, is_covered[ireq_valid_hr], map, lat, lon,  'is_covered' + title, /nolabel
                do_image, event_prob_fcst, map, lat, lon, 'forecasted prob. of event (capacity < thresh)' + title
                do_image, 1-avg_forecasts, map, lat, lon, '1-avg_fcst (mean forecasted capacity reduction)' + title
                do_image, event_obs, map, lat, lon, 'event_obs '+title
                for ibin = 0, nbins-1 do begin
                  do_image, hmfn[ibin,*], map, lat, lon, 'hits,misses,false_alarms p='+string(ibin*binsize*100,csi[ibin],format='(I0,"% CSI:",F5.2)')+title, /nolabel
;                  do_image, hits[ibin,*], map, lat, lon, 'hits '+title
                endfor
                do_image, (event_obs-event_prob_fcst)^2., map, lat, lon, '(event_obs-event_prob_fcst)^2 event probability Brier score'+title
                do_image, (obs-avg_forecasts)^2., map, lat, lon, '(obs-avg_forecasts)^2 squared capacity error'+title
              endif
              printf, lun, string(strmid(file,strpos(file, 'WAF')), req_valid_hr, n_valid_forecasts[0],$
                capacity_thresh, binsize,$
                nhits, nmiss, nfalse_alarm, ncorrct_null, brier_score, mean(SE_avg_fcst), $
                hist_event_prob_fcst, hist_obs, hist_avg_forecasts, obs_count_given_p,$
                format='(A,A7,I3,F6.3,F5.1,'+string(nbins*4,format='(I0)')+'I6,F8.5,F8.5,'+string(nbins*4,format='(I0)')+'I6)')
                
            endfor ; new valid_hr in file
          endfor ; new file
          free_lun, lun
        endfor ; new ensemble member
      endfor ; new direction 'NS' or 'EW'
      for imember = 0, nmembers-1 do begin
        member = members[imember]
        for ifile = 0, nfiles-1 do begin
          for ivalid_hr = 0,23 do begin
            title = string(model,height,format='("!C",A," ",A,",000ft ")') + $
              ' leadtime:'+strtrim(long(leadtimes[0]),2)+" capacity thresh:"+$
              string(capacity_thresh, member, format='(F4.2," ",A)')
            array = EW_NS_obs[ifile,ivalid_hr,imember,*]
            isuspicious = where(array eq -1, nsuspicious)
            if nsuspicious gt 400 then continue ; don't plot this valid_hr. it is probably bad because EW is missing.
            inonzero = where(array ne 0, nnonzero)
            ; If there are a number of EW-NS capacity differences then plot it.
            if nnonzero gt 10 then if mean(abs(array[inonzero])) gt .22 then begin
;              do_image, array, map, lat, lon, 'EW-NS observed capacity'+'!C'+files[ifile]+string(ivalid_hr,format='("  ", I2.2," UTC")')+title
            endif
          endfor
        endfor
      endfor ; new ensemble member
    endfor ; new waf height '20' '30' or '40'
  endfor ; new lead (lead0, lead2, lead6, etc.)
  if !D.NAME eq 'PS' then device, /close
  stop
end
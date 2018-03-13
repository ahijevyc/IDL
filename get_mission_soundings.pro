function get_mission_soundings,pgi,date,level,best_track,mtm00,debug=debug,cloud_drift=cloud_drift, time=time

  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(cloud_drift) then cloud_drift = 0
  if ~keyword_set(time) then hour_str = '' else hour_str = '_'+strmid(time,0,2)

  basedir='/Volumes/pecan2/ahijevyc/'
  ; this was written before get_closest_sounding.
  ; it used to take a list of dropsonde files as input instead of finding them itself via date and time.
  ; used to rely on list of cloud_drift wind files too, but now uses cloud_drift keyword.
  dropfiles = cloud_drift ? file_search(basedir+'PREDICT/GOES_cloud_drift/clouddrift' + date + hour_str+'.csv', count=nmissions) : $  
                            file_search(basedir+'PREDICT/dropsondes/eol' + date + hour_str+'*.csv', count=nmissions)
  if debug gt 0 then print, 'found ' + strtrim(nmissions,2) + ' csv files '+'for '+date+hour_str
  if debug then print, dropfiles
  

  nmissions=n_elements(dropfiles)
  if nmissions eq 0 then stop
  
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
    
  maxdrops = 1000
  xoffset = replicate(0, nmissions)
  yoffset = replicate(0, nmissions)
  ndrops  = replicate(0, nmissions)
  dattims   = replicate('', nmissions)
  xdrops    = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  ydrops    = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  radiusdrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  stiddrops   = replicate('',            maxdrops, nmissions)
  udrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  vdrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  tdrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  tddrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  tvdrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  rhdrops = replicate(!VALUES.F_NAN, maxdrops, nmissions)
  msedrops= replicate(!VALUES.F_NAN, maxdrops, nmissions)
  pdrops= replicate(!VALUES.F_NAN, maxdrops, nmissions)
  zdrops= replicate(!VALUES.F_NAN, maxdrops, nmissions)
  droplats = !VALUES.F_NAN
  droplons = !VALUES.F_NAN
    
  for imission = 0, nmissions-1 do begin
    file = dropfiles[imission]
    if file eq '' then continue; file_search returns '' if it does not find any files
    dropdata = read_ascii(file, template=template)
    for ifield=2,n_tags(dropdata)-1 do begin
      imsg = where(dropdata.(ifield) eq -9999, nmsg)
      if nmsg gt 0 then dropdata.(ifield)[imsg] = !VALUES.F_NAN
    endfor
    dattim = dropdata.DATTIM[0]
    dattims[imission] = dattim
    istids = where(dropdata.PRESS eq level, n700) ; thought about hardwiring to 700mb, so it doesn't choke on non-standard levels, but need to adapt to cloud drift winds, which could have nothing.
    ndrops[imission] = n700
    if n700 eq 0 then begin
      if debug gt 0 then print, 'no drops found at '+pgi+' '+file+' '+string(level)
      continue
    end
    stids = dropdata.STID[istids]
    droplons = dropdata.LON[istids]
    droplats = dropdata.LAT[istids]
    mtm_center, pgi, strmid(dattim,0,8), strmid(dattim,9,4), clat, clon, best_track=best_track, mtm00=mtm00, silent=1
    if NOT FINITE(clat) then begin
      print, "no mtm center found for "+dattim+". skipping time"
      continue
    endif
    
    for idrop = 0, ndrops[imission]-1 do begin
      lon = droplons[idrop]
      lat = droplats[idrop]
      stid = stids[idrop]
      result = map_2points(clon, clat, lon, lat)
      ; az is degrees east of north (meteorological)
      A = 6371.
      r = result[0] * !DTOR * A
      az = result[1]
      x = sin(az*!DTOR) * r
      y = cos(az*!DTOR) * r
      xdrops[idrop,imission] = x
      ydrops[idrop,imission] = y
      radiusdrops[idrop,imission] = r
      stiddrops[idrop,imission] = stid
      ilevel = where(dropdata.PRESS eq level, n)
      if n ne ndrops[imission] then stop  ; used to have a check here, but when I request a non-standard level, and it can't find it, it breaks.
      u = (dropdata.U[ilevel])[idrop]
      v = (dropdata.V[ilevel])[idrop]
      t = (dropdata.T[ilevel])[idrop]
      td= (dropdata.TD[ilevel])[idrop]
      p = (dropdata.PRESS[ilevel])[idrop]
      p_mb = p/100.
      z = (dropdata.HEIGHT[ilevel])[idrop]
      udrops[idrop,imission] = u
      vdrops[idrop,imission] = v
      tdrops[idrop,imission] = t
      tddrops[idrop,imission] = td
      tvdrops[idrop,imission] = t_virtual(t+!CONST.T0,p_mb,e_h2o(td+!CONST.T0))-!CONST.T0
      rhdrops[idrop,imission] = rh(td+!CONST.T0,t+!CONST.T0)
      zdrops[idrop,imission] = z
    endfor
    ; only calculate the mean x and y offset for soundings within 1000 km of the center.
    within_reason = where(sqrt(xdrops[*,imission]^2.+ydrops[*,imission]^2.) lt 1000, nwr)
    if nwr gt 0 then begin
      xmean = mean((xdrops[*,imission])[within_reason],/nan)
      xoffset[imission] = abs(xmean)^0.9 * abs(xmean) / xmean
      ymean = mean((ydrops[*,imission])[within_reason],/nan)
      yoffset[imission] = abs(ymean)^0.9 * abs(ymean) / ymean
    endif
  endfor
  MSEdrops = moist_static_energy(tdrops+!CONST.T0,tddrops+!CONST.T0,replicate(level/100.,maxdrops, nmissions),zdrops)

  return, {ndrops:ndrops, xdrops:xdrops, ydrops:ydrops, stiddrops:stiddrops, tdrops:tdrops, msedrops:msedrops, zdrops:zdrops, $
    tddrops:tddrops, tvdrops:tvdrops, rhdrops:rhdrops, udrops:udrops, vdrops:vdrops, radiusdrops:radiusdrops, PRESS:level, $
    dattims:dattims, xoffset:xoffset, yoffset:yoffset, lats:droplats, lons:droplons}
    
    
end

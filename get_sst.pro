function get_SST, jdays_in, lons_in, lats_in, Reynolds=Reynolds
  if ~keyword_set(Reynolds) then Reynolds = 0
  lats = lats_in
  lons = lons_in
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/SST/'
  ndrops = n_elements(lats_in)
  
  jdays = rebin([jdays_in], ndrops)
  
  ssts = replicate(!VALUES.F_NAN, ndrops)
  
  
  subdir = Reynolds ? 'Reynolds/[12][90][0-9][0-9]*-NCDC-L4LRblend-GLOB*OI.nc' : 'JPL_OUROCEAN-L4UHfnd-GLOB-G1SST/20*.nc'
  files = file_search(basedir+subdir, count=nfiles)
  
  for idrop = 0, ndrops-1 do begin
    jday = jdays[idrop]
    lon  = lons[idrop]
    lat  = lats[idrop]
    dtime = 1.1d
    for ifile = 0, nfiles-1 do begin
      file = files[ifile]
      yyyymmdd = strmid(file_basename(file),0,8)
      year = strmid(yyyymmdd,0,4)
      month=strmid(yyyymmdd,4,2)
      day=strmid(yyyymmdd,6,2)
      file_julday = julday(month, day, year, 0,0,0)
      if abs(file_julday-jday) lt dtime then begin
        dtime = abs(file_julday-jday)
        closest_file = file
      endif
    endfor
    if dtime ge 1.d then continue
    ; The closest day for this drop is most likely to be the same day as previous drop.
    ; Only open the file if it is new.
    if idrop eq 0 || closest_file ne old_closest_file then begin
      ncid = NCDF_OPEN(closest_file)
      NCDF_VARGET, ncid, "lat", lat_sst
      NCDF_VARGET, ncid, "lon", lon_sst
      NCDF_VARGET, ncid, "time", time
      NCDF_VARGET, ncid, "analysed_sst", image
      NCDF_ATTGET, ncid, "analysed_sst", "add_offset", add_offset
      NCDF_ATTGET, ncid, "analysed_sst", "scale_factor", scale_factor
      NCDF_ATTGET, ncid, "analysed_sst", "_FillValue", FillValue
      ibad = where(image eq FillValue, nbad)
      image = image * scale_factor + add_offset ; Kelvin
      if nbad gt 0 then image[ibad] = !VALUES.F_NAN
      NCDF_CLOSE, ncid
    endif
    
    ix = where(abs(lon_sst-lon) le 0.125)
    iy = where(abs(lat_sst-lat) le 0.125)
;    print, lat_sst[iy], lon_sst[ix]
    if reynolds eq 1 then SSTs[idrop] = image[ix[0],iy[0]] else begin
      d_km = image & d_km[*] = !VALUES.F_NAN ; create an array the same size as image, but all NaN.
      for iix = 0, n_elements(ix)-1 do begin
        for iiy = 0, n_elements(iy)-1 do begin
          d_km[ix[iix],iy[iiy]] = map_2points(lon_sst[ix[iix]],lat_sst[iy[iiy]],lon,lat,/meters)/1000.
        endfor
      endfor
      i = where(d_km lt 10.)
      SSTs[idrop] = mean(image[i])
    endelse
    old_closest_file = closest_file
  endfor
  
  return, ssts
end


function get_SST_ring, pgi, date, mtm00, best_track
  basedir = '/Volumes/pecan2/ahijevyc/'
  SSTfile = basedir+'PREDICT/SST/pouch_stats/SST'+pgi+date+'_000000'+(mtm00 eq 1 ?"mtm00":"")+(best_track eq 1 ?"best_track":"")+'.txt'
  if file_test(SSTfile) ne 1 then begin
    print, 'could not find ' + SSTfile
    return, !VALUES.F_NAN
  endif
  t = read_ascii(SSTfile, data_start=1, header=header)
  
  ;  print, header
  fields = strsplit(header, ' ', /extract)
  fields = idl_validname(fields, /convert_all)
  irmin = where(fields eq 'rmin',n)
  if n eq 0 then message, 'could not find rmin column in '+SSTfile
  irmax = where(fields eq 'rmax',n)
  if n eq 0 then message, 'could not find rmax column in '+SSTfile
  iavg  = where(fields eq 'avg')
  if n eq 0 then message, 'could not find avg column in '+SSTfile
  rmin = t.field01[irmin,*]
  rmax = t.field01[irmax,*]
  avg  = t.field01[iavg,*]
  
  iline = where(rmin eq 0 and rmax eq 300, nline)
  if nline ne 1 then message, 'could not extract 300km disk SST stats from '+SSTfile
  avgSST = (avg[iline])[0]
  return, avgSST
  
  
  
end

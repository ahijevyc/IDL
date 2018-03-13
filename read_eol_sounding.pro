function read_eol_sounding, closest_file, debug=debug, force_new=force_new, use_first_line=use_first_line
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(force_new) then force_new = 0
  if ~keyword_set(use_first_line) then use_first_line = 0
  
  savfile = file_dirname(closest_file) + '/savfiles/' + file_basename(closest_file) + '.sav'
  ; test if savfile is newer than ascii file
  if force_new eq 0 && file_test(savfile) eq 1 then begin
    closest_file_info = file_info(closest_file)
    savfile_info = file_info(savfile)
    ; make sure the save file has a greater mtime (is more recently modified) than original closest file.
    if savfile_info.mtime gt closest_file_info.mtime then begin
      restore, savfile
    endif else begin
      print, 'get_closest_sonde_in_time: original file '+closest_file+' was modified more recently than savfile '+savfile
      stop
    endelse
  endif else begin
  
  
    openr, lun, closest_file, /get_lun
    if debug gt 1 then print, 'opened '+closest_file
    junk = ''
    data_start = 0
    repeat begin
      readf, lun, junk
      data_start = data_start+1
    endrep until strpos(junk, '-----') eq 0
    free_lun, lun
    
    eol_template = {$
      VERSION   : 1.00000,   $
      DATASTART : data_start,   $
      DELIMITER : 0B,       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 13L,  $
      FIELDTYPES:   [4,7,4,4,4,7,4,4,7,4,4,4,4] ,  $
      ;      FIELDNAMES:  ['Time_sec','UTC_hh','UTC_mm','UTC_ss','Press','T','Td','RH','Uwind','Vwind','Wspd','Dir','dZ','GeoPoAlt_m','lon','lat','HGHT'], $
      FIELDNAMES:         ['Time_sec','junk',                    'Press','T','Td','XX','U',    'V',    'junk2',          'GeoPoAlt_m','lon','lat','GPSAlt'], $
      ;      FIELDLOCATIONS: [0,8,11,14,20,28,36,44,52,60,68,76,84,92,101,113,125],$
      FIELDLOCATIONS:        [0,8,      20,28,36,44,52,60,68,      92,101,113,125],$
      FIELDGROUPS:  indgen(13) }
      
      
    ; started ignoring first record Dec 3, 2013. It is the first point of the dropsonde file. This is not a dropsonde value  - it is from the aircraft and should be ignored.
    ; If you treat interpolated points between large vertical gaps in the observations by prescribing a small max_gap threshold value, like 200m, you also effectively 
    ; ignore the first point. That is because there is a large gap between the first valid point and the next one, due to the
    ; time to equilibriate temperature (eol QC throws away the first 30 seconds for this reason).  But if you use a large max_gap value, you may start
    ; using the 1st point, and this results in some big differences. 
    t = read_ascii(closest_file, template=eol_template,count=nlevs, record_start=use_first_line?0:1)
    
    ; Note we read the GeoPoAlt_m, not the GPSAlt.  This is true even when running microwave_temp_profile with GPSAlt specified.
    ; It's confusing.  There are two heights in MTP files (pressure altitude and GPS geometric altitude),
    ; and 2 heights in the dropsonde files (geopotential altitude and GPS geometric altitude).  We would just use GPS geometric altitude
    ; from the dropsonde files, but it is not good. GPS geometric altitude has dropouts and wasn't QC'ed according to Brigitte.
    ; Therefore, we read GeoPoAlt_m and convert it to geometric altitude with MJ's "exact" theoretical conversion (function of latitude).
    ; assign Ts, Tds, Presss, lons, and lats. Replace -999 with NaN
    ; if undefined .r replace_wNAN
    t.Time_sec    = replace_wNAN(t.Time_sec, -999)
    t.T           = replace_wNAN(t.T, -999)
    t.GeoPoAlt_m  = replace_wNAN(t.GeoPoAlt_m, -999)
    t.Td          = replace_wNAN(t.Td, -999)
    t.Press       = replace_wNAN(t.Press, -999)
    t.U           = replace_wNAN(t.U, -999)
    t.V           = replace_wNAN(t.V, -999)
    t.lon         = replace_wNAN(t.lon, -999)
    t.lat         = replace_wNAN(t.lat, -999)
    
    save, t, nlevs, filename=savfile
  endelse
  
  return, t
end
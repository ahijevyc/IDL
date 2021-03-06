function read_gempak_fil, file, datastart=datastart
  ; copied from tuna Jul 6, 2017
  ; altered for GEMPAK request from Chris Davis.
  ; see email attachment iop8_dropsonde.dat from Jul 6, 2017
  if n_elements(file) eq 0 then file = '~/t.dat'

  if ~keyword_set(datastart) then begin
    ; define datastart (lines of header before numeric data start)
    datastart = 0
    openr, lun, file, /get_lun
    line = ''
    while ~eof(lun) do begin
      readf, lun, line
      if strmid(line, 0, 9) eq "SNPARM = " then fields = (strtrim(strmid(line, 9),2)).split(",")
      ; If line has two 4-character words, all capital letters and no digits,
      ; assume it is the fields line
      if stregex(line,' +[A-Z][A-Z][A-Z][A-Z] +[A-Z][A-Z][A-Z][A-Z] +') ne -1 and ~strmatch(line, "*[0-9=]*") then fields = (line.split(" +"))[1:-1] ; 
      ; If line has a digit and no letters or equal signs, assume it is data, not header
      if strmatch(line, '*[0-9]*') and ~strmatch(line, "*[a-z=]*", /fold) then break
      datastart = datastart + 1
    endwhile
    free_lun, lun
  endif
  
  nfields = n_elements(field)
  fil_template = {$
    VERSION   : 1.00000,   $
    DATASTART : datastart,   $
    DELIMITER : " ",       $
    MISSINGVALUE: !VALUES.F_NAN,$
    COMMENTSYMBOL:  '',   $
    FIELDCOUNT:     fields.LENGTH,  $
    FIELDTYPES:     replicate(4,fields.LENGTH) ,  $
    FIELDNAMES:     fields,$
    FIELDLOCATIONS: indgen(fields.LENGTH),$
    FIELDGROUPS:    indgen(fields.LENGTH) }
  t = read_ascii(file, template=fil_template, header=header, count=nz)
  if nz eq 0 then  return, !NULL
  
  t.PRES   = replace_wNAN(t.PRES, -9999)
  t.TMPC   = replace_wNAN(t.TMPC, -9999)
  t.UWND   = replace_wNAN(t.UWND, -9999)
  t.VWND   = replace_wNAN(t.VWND, -9999)
  if where('RELH' eq fields) gt 0 then begin
    t.RELH   = replace_wNAN(t.RELH, -9999)
    DwptCs = rh2tdew(t.TMPC, t.RELH)
  endif
  if where('DWPC' eq fields) gt 0 then begin
    DwptCs = replace_wNAN(t.DWPC, -9999)
  endif

  ; If long and lati are missing in the sounding array, then use station lat/lon from the gempak header
  ipos = where(strmatch(header, '*SLON*'),/NULL)
  slon = strmid(header[ipos], strpos(header[ipos], 'SLON=')+5,9)
  ipos = where(strmatch(header, '*SLAT*'),/NULL)
  slat = strmid(header[ipos], strpos(header[ipos], 'SLAT=')+5,9)
  ipos = where(strmatch(header, '*STID*'),/NULL)
  stid = header[ipos]
  msg = replicate(!VALUES.F_NAN,n_elements(t.PRES))
  t = {Time_sec:replicate(0,n_elements(t.PRES)), Press:t.PRES, T:t.TMPC, Td:DwptCs, U:t.Uwnd, V:t.Vwnd, $
    GPSAlt:msg, GeoPoAlt_m:t.HGHT, lon:msg, lat:msg, slon:slon, slat:slat, stid:stid}


  return, t
  cape_sound, t.press, t.t+!CONST.T0, mixr_sat(t.td, t.press)/1000., debug=1, kpar=0, capep=capep, parcel_params=parcel_params, entrainment_rate = 10.
  print, max(capep, /nan)

end

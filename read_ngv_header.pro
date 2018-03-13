function read_NGV_header, infile, lun=lun, primary=primary, aux=aux

  ; === read header lines of *.NGV file ===
  
  openr, lun, infile, /get_lun
;  print, 'opened '+infile
  readf, lun, header_lines, nsomething
  junk = ''
  repeat readf, lun, junk until junk eq 'PREDICT' or junk eq 'MPEX'
  readf, lun, testa, testb
  if testa ne 1 or testb ne 1 then stop
  readf, lun, FLT_year, FLT_month, FLT_day, REDUCTION_year, REDUCTION_month, REDUCTION_day, flight_number
  readf, lun, remote_sensing_altitude, UT_seconds_from_0
  if remote_sensing_altitude ne 0 or UT_seconds_from_0 ne 0 then stop
  readf, lun, junk
  readf, lun, junk
  readf, lun, NV
  if NV ne 4 then stop
  readf, lun, scale1, scale2, scale3, scale4
  if scale1 ne 1.0 or scale2 ne 1.0 or scale3 ne 1.0 or scale4 ne 1e+21 then stop
  readf, lun, miss1, miss2, miss3, miss4
  primary = {count:NV, scale:[scale1,scale2,scale3,scale4], missing:[miss1,miss2,miss3,miss4],long_name:strarr(NV)}
  for i=0,NV-1 do begin
    readf, lun, junk
    primary.long_name[i] = junk
  endfor
  readf, lun, n_aux_var
  if n_aux_var ne 15 then stop
  aux = {count:n_aux_var, scale:dblarr(n_aux_var), missing:fltarr(n_aux_var), long_name:strarr(n_aux_var)}
  readf, lun, junk
  aux.scale = strsplit(junk, ' ', /extract)
  readf, lun, junk
  aux.missing = strsplit(junk, ' ',/extract)
  for i=0,n_aux_var-1 do begin
    readf, lun, junk
    aux.long_name[i] = junk
  endfor
  readf, lun, nspec ; number of special header lines
  iline = 0L
  repeat begin
    readf, lun, junk
    iline = iline+1
  endrep until iline eq nspec
  readf, lun, nnorm ; number of normally included comment lines
  iline = 0L
  repeat begin
    readf, lun, junk
    iline = iline+1
  endrep until iline eq nnorm
  
  return, julday(FLT_month, FLT_day, FLT_year, 0, 0, 0)

end
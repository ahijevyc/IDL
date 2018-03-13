function get_GV, file

  ncid = NCDF_OPEN(file,/nowrite)            ; Open The NetCDF file
  glob = ncdf_inquire(ncid)
  t = {filename:file}
  
  for i=0,glob.nvars-1 do begin
    info = ncdf_varinq(ncid, i)
    ncdf_varget, ncid, i, x
    if info.datatype eq 'FLOAT' then begin
      att = ncdf_attinq(ncid, i, '_FillValue')
      if att.length eq 1 then begin
        ncdf_attget, ncid, i, '_FillValue', FV
        iFV = where(x eq FV, nFV)
        if nFV gt 0 then x[iFV] = !VALUES.F_NAN
      endif
    endif
    result = execute('t=create_struct("'+info.name+'",x,t)')
  endfor
  ncdf_attget, ncid, 'Time', 'units', units
  
  
  NCDF_CLOSE, ncid      ; Close the NetCDF file
  
  
  year = strmid(units, 14,4)
  month=strmid(units,19,2)
  day = strmid(units,22,2)
  hour = strmid(units, 25,2)
  minute = strmid(units, 28,2)
  second = strmid(units,31,2)
  tzone = long(strmid(units,34,5))
  tzone_hh = tzone/100
  tzone_mm = tzone - tzone_hh*100
  juldays = julday(month, day, year, hour+tzone_hh, minute+tzone_mm, second+t.Time)
  t = create_struct("juldays",juldays,t)
  
  
  return, t
end

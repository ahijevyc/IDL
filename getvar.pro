function getvar, ncid, varstring, varid=varid
  ; returns a float array
  ; apply scale_factor
  ; set _FillValue and elements < valid_min and > valid_max to NaN.
  varid = ncdf_varid(ncid,varstring)
  if varid eq -1 then begin
    print, 'getvar.pro: ' + varstring + ' does not exist'
    return, !NULL
  endif
  ncdf_varget, ncid, varstring, var
  varinq = ncdf_varinq(ncid, varstring)
  scale_factor = 1.0
  nbad = 0
  nlow = 0
  nhgh = 0
  for attnum = 0, varinq.natts-1 do begin
    attname = ncdf_attname(ncid, varstring, attnum)
    if attname eq 'scale_factor' then ncdf_attget, ncid, varstring, 'scale_factor', scale_factor
    if attname eq '_FillValue' then begin
      ncdf_attget, ncid, varstring, '_FillValue', FillValue
      ibad = where(var eq FillValue, nbad)
    endif
    if attname eq 'valid_min' then begin
      ncdf_attget, ncid, varstring, 'valid_min', valid_min
      ilow = where(var lt valid_min, nlow)
    endif
    if attname eq 'valid_max' then begin
      ncdf_attget, ncid, varstring, 'valid_max', valid_max
      ihgh = where(var gt valid_max, nhgh)
    endif
    if attname eq 'valid_range' then begin
      ncdf_attget, ncid, varstring, 'valid_range', valid_range
      ilow = where(var lt valid_range[0], nlow)
      ihgh = where(var gt valid_range[1], nhgh)
    endif
  endfor
  var = var*scale_factor
  if nbad gt 0 then var[ibad] = !VALUES.F_NAN
  if nlow gt 0 then var[ilow] = !VALUES.F_NAN
  if nhgh gt 0 then var[ihgh] = !VALUES.F_NAN
  return, var
end


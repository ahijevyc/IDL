pro ncdf_replace_fillvalue, ncid, variable_string, variable

  ncdf_attget,ncid,variable_string,"_FillValue",fillvalue
  imsg = where(variable eq fillvalue, nmsg)
  if nmsg gt 0 then variable[imsg] = !VALUES.F_NAN
  
end

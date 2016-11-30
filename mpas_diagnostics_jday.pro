function mpas_diagnostics_jday, infile

  file = file_basename(infile)

  if strmatch(file, 'diagnostics.*') then begin
    year = strmid(file,12,4)
    month = strmid(file,17,2)
    day = strmid(file,20,2)
    hour = strmid(file,23,2)
    minute = strmid(file,26,2)
    second = strmid(file,29,2)
  endif else if strmatch(file, 'diag.*') then begin
    year = strmid(file,5,4)
    month = strmid(file,10,2)
    day = strmid(file,13,2)
    hour = strmid(file,16,2)
    minute = strmid(file,19,2)
    second = strmid(file,22,2)
  endif else if strcmp(file, 'init.nc') then begin
    idate = strpos(infile,'/201')
    year = strmid(infile,idate+1,4)
    month = strmid(infile,idate+5,2)
    day = strmid(infile,idate+7,2)
    hour=0
    minute=0
    second=0
  endif else if strmatch(infile, '*GFS004/*') then begin
    ; this is for the 0.5-deg GFS files in directories /glade/scratch/ahijevyc/GFS004/201*
    year = strmid(file,0,4)
    month = strmid(file,4,2)
    day = strmid(file,6,2)
    ; find the initialization hour and forecast hour substrings
    junk = stregex(file,'[0-9]_i([012][0-9])_f([0-9]+)_', /extract, /sub)
    ; and add them together
    hour = junk[1]+junk[2]
    minute = 0
    second = 0
  endif else if strmatch(file, 'f[0-9][0-9][0-9].nc') then begin
    ncid = ncdf_open(infile)
    if ncdf_varid(ncid,'xtime') ne -1 then begin
      ncdf_varget, ncid, ncdf_varid(ncid,'xtime'), xtime
      year = string(xtime[0:3])
      month = string(xtime[5:6])
      day = string(xtime[8:9])
      hour = string(xtime[11:12])
      minute = string(xtime[14:15])
      second = string(xtime[17:18])
    endif else begin
      year = 0
      month = 0
      day = 0
      hour = strmid(file,1,3)
      minute = 0
      second = 0
    endelse
    ncdf_close, ncid
  endif

  jday = julday(month, day, year, hour, minute, second)
  return, jday
end
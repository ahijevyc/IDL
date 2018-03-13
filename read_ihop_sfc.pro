function togempak, lines, stnfile
  n = n_elements(lines)
  newlines = strarr(n)
  stids = strmid(lines, 51, 8)
  lats = strmid(lines,69,2) + strmid(lines,72,2)
  lons = strmid(lines,79,4) + strmid(lines,84,2)
  elevs = strmid(lines,94,4)
  for i = 0L, n-1 do begin
    stid = stids[i]
    
    if strmatch(stid,'[0-9][0-9][0-9]     ') then stid = 'F'+stid+'    '
    
    icr = strpos(stid, ': ')
    if icr ge 0 then stid = strmid(stid,0,icr) + 'XX' + strmid(stid,icr+2)
    
    icr = strpos(stid, '_')
    if icr ge 0 then stid = strmid(stid,0,icr) + 'X' + strmid(stid,icr+1)
    
    if strmatch(stid,'*[0-9A-Z] [0-9A-Z]*', /fold_case) then begin
      icr = strpos(stid, ' ')
      stid = strmid(stid,0,icr) + 'X' + strmid(stid,icr+1)
    endif
    stids[i] = STRUPCASE(stid)
  endfor
  
  
  ih = uniq(stids, sort(stids))
  stnfile = stnfile
  openw, sunit, stnfile, /get_lun
  for i = 0, n_elements(ih)-1 do begin
    lat = lats[ih[i]]
    lon = lons[ih[i]]
    elev = elevs[ih[i]]
    stid = stids[ih[i]]
    printf, sunit, stid, lat, lon, elev,format='(A8," 999999 XXX",30X,"-- XX ",I5,1X,I6,1X,I5)'
  endfor
  free_lun, sunit
  print, "wrote station file " + stnfile
  for i = 0L, n-1 do begin
    line = lines[i]
    stid = stids[i]
    time = strmid(line, 2,2) + strmid(line, 5, 2) + strmid(line, 8,2) + "/" + strmid(line, 11,2)+ strmid(line, 14,2)
    pres = strmid(line, 102, 7)
    pmsl = strmid(line, 112, 7)
    tmpc = strmid(line, 132, 7)
    dwpc = strmid(line, 142, 7)
    sped = strmid(line, 152, 7)
    wdir = strmid(line, 162, 7)
    prec = strmid(line, 172, 7)
    newlines[i] = stid + '   ' + time + '   ' + pres + '  ' + pmsl + '  ' + tmpc + '  ' + dwpc + '  ' + sped + '  ' + wdir + '  ' + prec
    
  endfor
  return, newlines
end


pro read_ihop_sfc

  file = '/pecan/ahijevyc/IHOP_WRF/hrly_sfc/datafVdgwj'
  file = '/pecan/ahijevyc/BaMEx/sfc/all.qcf'
  
  ; Assumes 3rd line is asterisks that delimit columns.
  SPAWN, 'head -3 ' + file + '| tail -1 ', stars
  fieldlocs = STRSPLIT(stars," ",COUNT=nfields, length=length)
  
  template = { $
    VERSION:1, $
    DATASTART:LONG(3), $
    DELIMITER:'|', $
    MISSINGVALUE:!VALUES.F_NAN, $
    COMMENTSYMBOL:'', $
    FIELDCOUNT: 1, $
    FIELDTYPES: 7, $
    FIELDNAMES: 'line', $
    FIELDLOCATIONS:0, $
    FIELDGROUPS:0 }
    
  data=read_ascii(file,data_start=3, template=template, header=header)
  
  
  fn = strarr(nfields)
  for i = 0, nfields-1 do fn[i] = strmid(header[0], fieldlocs[i], length[i]) +strmid(header[1], fieldlocs[i], length[i])
  
  
  ipres = where(fn eq 'STN    Press  ', n)
  if n eq 0 then stop
  good_pres = strmid(data.line, fieldlocs[ipres+1], 1) EQ 'G'
  
  islp =  where(fn eq 'Sea LvlPress  ', n)
  if n eq 0 then stop
  good_slp = strmid(data.line, fieldlocs[islp+1], 1) EQ 'G'
  
  itemp = where(fn eq 'DrybulbTemp   ', n)
  if n eq 0 then stop
  qc = strmid(data.line, fieldlocs[itemp+1], 1)
  ; added questionable (D) to get stations ahead of line at 20020613 1800UTC. -ahij 20110307
  good_t   =  STRMATCH(qc, '[GD]')
  
  itd =   where(fn eq 'Dewpnt Temp   ', n)
  if n eq 0 then stop
  qc = strmid(data.line, fieldlocs[itd+1], 1)
  good_td  = STRMATCH(qc, '[GD]')
  
  ispd = where(fn eq 'Wind   Speed  ', n)
  if n eq 0 then stop
  good_spd = strmid(data.line, fieldlocs[ispd+1], 1) eq 'G'
  
  idir = where(fn eq 'Wind   Dir    ', n)
  if n eq 0 then stop
  good_dir = strmid(data.line, fieldlocs[idir+1], 1) eq 'G'
  
  ipcp = where(fn eq 'Total  Prcp   ', n)
  if n eq 0 then stop
  good_pcp = strmid(data.line, fieldlocs[ipcp+1], 1) eq 'G'
  
  igood = where(good_t and good_td and good_pres and good_pcp)
  outputfile = '/pecan/ahijevyc/IHOP_WRF/qc.txt'
  outputfile = file_dirname(file)+'/'+file_basename(file, 'qcf')+'fil'
  OPENW, unit, outputfile, /get_lun
  printf, unit, 'PARM = PRES;PMSL;TMPC;DWPC;SPED;DRCT;P01M'
  printf, unit, ''
  printf, unit, '   STID    YYMMDD/HHMM      PRES     PMSL     TMPC     DWPC     SPED     DRCT     P01M'
  stnfile = '/pecan/ahijevyc/IHOP_WRF/ihop_1.tbl'
  stnfile = file_dirname(file)+'/stn.tbl'
  printf, unit, togempak(data.line[igood],stnfile), format='(A)'
  close, unit, /all
  print, "wrote "+outputfile
  print, "rm sfc.dat"
  print, "gempak"
  print, "sfcfil"
  print, "maybe sfprm=/pecan/ahijevyc/IHOP_WRF/sfsyn_plus_P01M.pack"
  print, "sfedit"
  print, "use IHOP_gifs or BAMEX_etc. control #stations with FILTER="
end

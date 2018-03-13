function is_stage, basedir, date, hour, stage, pgi=pgi
; pgi keyword added to filter PGI48 and 51. when the stage doesn't match pgi, return 0 (false).
  tmpl = { VERSION: 1., $
    DATASTART: 0L, DELIMITER: '', $
    MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  1L, $
    FIELDTYPES: [7], $
    FIELDNAMES: ['yyyymmdd_hh'], $
    FIELDLOCATIONS: [0], $
    FIELDGROUPS: indgen(1) }
  n = n_elements(date)
  return_vector = replicate(0, n)
  
  
  
  ; note my mission list is based on center time of time window. Will Komaromi's is based on the time of the first image in the time window.

  ; On 20130409 I allowed stage to be a PGI mission with date and time. (Not just "all" "non-developing" "developed_le2days", etc. )
  
  stage_is_mission = stregex(stage, '(PGI[0-9]+L)(2010[0-1][0-9][0-3][0-9]_[012][0-9])', /extract, /subexpr)
  if stage_is_mission[0] ne '' then begin
    t = {yyyymmdd_hh : stage_is_mission[2]} 
    if keyword_set(pgi) && stage_is_mission[1] ne pgi then return, return_vector ; pgi keyword added to filter PGI48 and 51. when the stage doesn't match pgi, return 0 (false).
  endif else begin
    t = read_ascii(basedir + stage+'_missions.txt', template=tmpl, count=nmissions, comment='#')
  endelse
  
  date_hh = string(date,format='(I8.8,"_")') + (n_elements(hour) eq 0 ? '*' : hour)
  ;  if hour ne '*' then hour = string(hour, format='(I2.2)') ; reserve use of '*' for cfad_mtp.pro MTP stuff
  for i=0,n-1 do begin
    if total(strmatch(t.yyyymmdd_hh, '*'+date_hh[i]+'*')) gt 0 then return_vector[i]=1
  endfor
  return, return_vector
end

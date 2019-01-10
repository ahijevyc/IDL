
function read_tc_stat, file

  ; pre processing examples
  ; egrep "^.{90}00.*" out_tcmpr.tcst | egrep -v "^.{139}NA.*" | grep MPAS > t
  ;
  ; tc_stat -job summary -lookin out_tcmpr.tcst -by AMODEL,LEAD,BASIN -column TK_ERR -event_equal TRUE -out TK_ERR.tc_stat
  ;tc_stat -job summary -lookin out_tcmpr.tcst -by LEAD -by amodel -column AMAX_WIND-BMAX_WIND -out t -by storm_name

  ; see /glade/work/ahijevyc/METv4.1/filter_matches.csh for more pre processing examples.

  openr, lun, file, /get_lun
  header=!NULL
  line = ''
  datastart = 1L
  readf, lun, line
  free_lun, lun
  fieldlocations = strsplit(line)
  fieldnames = strsplit(line, /extract, count=fieldcount)

  template = { $
    VERSION:1, $
    DATASTART:datastart, $
    DELIMITER:' ', $
    MISSINGVALUE:!VALUES.F_NAN, $
    COMMENTSYMBOL:'DEBUG', $
    FIELDCOUNT: fieldcount, $
    FIELDTYPES: replicate(4,fieldcount), $
    FIELDNAMES: fieldnames, $
    FIELDLOCATIONS:fieldlocations, $
    FIELDGROUPS:lindgen(fieldcount) }

  float_template = template
  ; assume floats except for some strings
  istring = where(fieldnames eq 'VERSION' or STRMATCH(fieldnames, '*MODEL') or strmatch(fieldnames, 'STORM*') $
    or fieldnames eq 'BASIN' or strmatch(fieldnames, 'INIT*') or strmatch(fieldnames, 'VALID*') $
    or fieldnames eq 'LINE_TYPE' or fieldnames eq 'LEVEL' or fieldnames eq 'WATCH_WARN' or fieldnames eq 'INITIALS')
  float_template.fieldtypes[istring] = 7

  spawn, "column -t "+file+" > tmp; mv tmp "+file ; line up columns or else read function will not work.
  t = read_ascii(file, template=float_template)

  if max(t.amax_wind, /nan) gt 500 then stop ; sanity check
  if max(t.bmax_wind, /nan) gt 500 then stop ; sanity check
  if max(t.amslp, /nan) lt 700 then stop ; sanity check
  if max(t.amslp, /nan) gt 1100 then stop ; sanity check
  if max(t.bmslp, /nan) lt 700 then stop ; sanity check
  if max(t.bmslp, /nan) gt 1100 then stop ; sanity check

  ; convert lead time to float.
  leads = t.lead
  hours = leads/10000L
  minutes = (leads - floor(leads)*10000L)/100L
  seconds = minutes - floor(minutes)*100L
  t.lead = hours + minutes/60. + seconds/3600.

  ;  t.mean = replace_wNAN(t.mean, 'NA')
  ;  t.mean_ncl = replace_wNAN(t.mean_ncl, 'NA')
  ;  t.mean_ncu = replace_wNAN(t.mean_ncu, 'NA')

  ;  t = create_struct(t, 'job_list', job_list)
  return, t
end

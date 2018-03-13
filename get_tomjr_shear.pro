pro get_tomjr_shear, pgi, date, hour, ushear, vshear
  ushear = !VALUES.F_NAN
  vshear = !VALUES.F_NAN
  file = '/Volumes/pecan2/ahijevyc/PREDICT/tomjr/'+pgi+'-deepshear.file'
  if file_test(file) ne 1 then return
  t = read_ascii(file, data_start=1, delimiter=',')
  years = string(t.field1[0,*], format='(I4)')
  months = string(t.field1[1,*], format='(I2.2)')
  days = string(t.field1[2,*], format='(I2.2)')
  hours = string(t.field1[3,*], format='(I2.2)')
  ushears = t.field1[6,*]
  vshears = t.field1[7,*]
  i = where(years + months + days eq date and hours eq hour, n)
  if n eq 0 then print, "failed to get tomjr shear for "+pgi+" "+date+" "+string(hour) else begin
    ushear = ushears[i]
    vshear = vshears[i]
  endelse
end


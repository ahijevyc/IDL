pro average_track
  ;members = {ids:['03','05','07','10','13'],name:'LEFT'}
  members = {ids:['02','09','12','14','19'],name:'RGHT'}
  fhr=24
  initdate='2016082700'
  month=strmid(initdate,4,2)
  day=strmid(initdate,6,2)
  year=strmid(initdate,0,4)
  hour=strmid(initdate,8,2)
  initjday=julday(month,day,year,hour)
  basin = 'WP'
  cy = 12
  dt = 6
  files = '/glade/p/work/ahijevyc/GFS/Lionrock/AP'+members.ids+'.'+initdate+'.adeck'
  lonsum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  latsum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  vmaxsum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  mslpsum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  rad1sum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  rad2sum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  rad3sum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  rad4sum = replicate(!VALUES.F_NAN, 240/dt+1, files.length)
  for ifile= 0, files.length-1 do begin
    file = files[ifile]
    t = read_atcf(file, GFDL_warmcore_only=0)
    if total(t.basin ne basin) then stop
    if total(t.cy ne cy) then stop
    if total(t.rad ne 34) then stop
    if total(t.windcode ne 'NEQ') then stop
    for itime = 0, t.julday.length-1 do begin
      fhr = 24d*(t.julday[itime] - initjday)
      it = fhr/dt
      if long(it) ne it then stop ; no decimals
      lonsum[it,ifile] = t.lon[itime]
      latsum[it,ifile] = t.lat[itime]
      vmaxsum[it,ifile] = t.vmax[itime]
      mslpsum[it,ifile] = t.mslp[itime]
      rad1sum[it,ifile] = t.rad1[itime]
      rad2sum[it,ifile] = t.rad2[itime]
      rad3sum[it,ifile] = t.rad3[itime]
      rad4sum[it,ifile] = t.rad4[itime]
    endfor
  endfor
  lons = mean(lonsum, /nan, dimension=2)
  lats = mean(latsum, /nan, dimension=2)
  vmaxs = mean(vmaxsum, /nan, dimension=2)
  mslps = mean(mslpsum, /nan, dimension=2)
  rad1s = mean(rad1sum, /nan, dimension=2)
  rad2s = mean(rad2sum, /nan, dimension=2)
  rad3s = mean(rad3sum, /nan, dimension=2)
  rad4s = mean(rad4sum, /nan, dimension=2)
  for i = 0, 240/dt do begin
    print, string(basin, cy, initdate, format='(A2,", ",I2,", ",A10)')+', 03, '+$
      string(members.name, i*dt, round(10*lats[i]), round(10*lons[i]), round(vmaxs[i]), round(mslps[i]), $
      round(rad1s[i]), round(rad2s[i]), round(rad3s[i]), round(rad4s[i]), $
      format='(A4,", ",I3.3,", ",I3,"N, ",I4,"E, ", I3,", ", I4,", XX,  34, NEQ, ",I4.4,", ",I4.4,", ",I4.4,", ",I4.4)')
  endfor
end
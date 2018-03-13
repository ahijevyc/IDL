pro mtm_hovm, pgi, percentile, best_track=best_track, half_best_track=half_best_track, mtm00=mtm00, type=type, disk=disk
  ;  pgi = 'PGI38L'
  ;  percentile = 'p10'
  hatch = 0 ; set to 1 if you want hatched contour surrounding low standard deviation area
  if ~keyword_set(best_track) then best_track = 0
  if ~keyword_set(half_best_track) then half_best_track = 0
  if ~keyword_set(mtm00) then mtm00 = 0
  if ~keyword_set(type) then type=''
  if ~keyword_set(disk) then disk = 0
  if best_track && half_best_track then stop ; You can't have both. What would this mean?
  if type ne '' and type ne 'SST' and type ne 'CMORPH' then stop ; must have correct type
  
  ; best_track = 1 will use the best track location if it is available.
  ; half_best_track will use the mtm_track locations for hours 00-12 and best_track for hours 13-23, if available.
  
  
  output_dir = '/Volumes/pecan2/ahijevyc/PREDICT/hovm/'
  output_filename = pgi+percentile+(mtm00?"_mtm00":"")+(best_track?"_best_track":"")+(half_best_track?"_half_best_track":"")+type+(disk?"disk":"")
  
  if not strmatch(pgi,'PGI[2345][0-9]L') then stop
  dir = '/Volumes/pecan2/ahijevyc/PREDICT/mtm_track/'
  files = file_search(dir+type+pgi+ '[12][01239][0-9][0-9][0123][0-9][0-9][0-9]_[012][0-9][0-9][0-9]00'+(mtm00?"mtm00":"")+'.txt', count=nfiles)
  
  files = files[sort(files)] ; the order can change, and if you need to restart with a higher file index, then you want constancy
  
  if pgi eq 'PGI38L' then begin
    ifiles = where(files ge dir+type+pgi+'20100831_000000.txt' and files lt dir+type+pgi+'20100909_000000.txt')
    files = files[ifiles]
    nfiles = n_elements(files)
  endif
  
  loadct, 39, /silent
  
  if !D.NAME eq 'PS' then device, /close, /color, ysize=11, yoffset=-0.5, xsize=8.5, /inches, bits_per_pixel=8, filename=output_dir+output_filename+'.ps'
  ;  if !D.NAME eq 'X' and !D.WINDOW ne -1 then wdelete
  if !D.NAME eq 'X' then device, decomposed=0 ; got this idea from mycolorbar.pro
  
  
  basenames = file_basename(files)
  idate = strpos(basenames[0],'2010')
  years = strmid(basenames,idate+0,4)
  months = strmid(basenames,idate+4,2)
  days = strmid(basenames,idate+6,2)
  hours = strmid(basenames,idate+9,2)
  minutes = strmid(basenames,idate+11,2)
  seconds = strmid(basenames,idate+13,2)
  jdays = julday(months,days,years,hours,minutes,seconds)
  
  
  block_minutes = 120; maximum time block that one satellite image can represent.
  if type eq 'SST' then block_minutes = 24*60
  if type eq 'CMORPH' then block_minutes = 3*60
  units = 'deg'
  if type eq 'CMORPH' then units = 'CMORPH precip in mm/h'
  ntimes = (max(jdays)-min(jdays)) * 24*60
  ntimes = round(ntimes) ; you can have a number like DOUBLE 2.0000, but it is really 1.99999999999. so fix(x) = 1
  best_track_arr = replicate(!VALUES.F_NAN,1,ntimes)
  nr = 36
  dr = 20.
  radii = findgen(nr)*dr+dr/2.
  time_series = replicate(!VALUES.F_NAN, nr, nfiles)
  hovm = replicate(!VALUES.F_NAN,nr,ntimes) ; chris asked me to relace NaNs with "missing value", but I'll wait until I output text file
  hovm2 = replicate(!VALUES.F_NAN,nr,ntimes)
  for ifile = 0,nfiles-1 do begin
    file = files[ifile]
    mtm_file        = STRMID(file,0,strlen(file)-4)+"mtm00.txt"
    file_best_track = STRMID(file,0,strlen(file)-4)+"best_track.txt" ; make sure you define file_best_track before redefining file because of mtm00=1
    if mtm00 then if file_test(mtm_file) then file = mtm_file
    if best_track then if file_test(file_best_track) then file = file_best_track
    if half_best_track && hours[ifile] gt 12 then if file_test(file_best_track) then file = file_best_track
    if ~File_test(file) then message, 'file '+file+' not found'
    if query_ascii(file) eq 0 then continue ; is it a valid ascii file?
    m = read_ascii(file, data_start=1,header=header)
    date = m.field01[1,*]
    time = m.field01[2,*]
    rmin = m.field01[3,*]
    rmax = m.field01[4,*]
    itime = round((jdays[ifile] - min(jdays) ) *24*60)
    min_itime = itime - block_minutes/2
    if min_itime lt 0 then min_itime = 0
    if n_elements(max_itime) gt 0 then if max_itime ge min_itime then min_itime=(old_itime+itime)/2
    
    max_itime = itime + block_minutes/2 -1
    if max_itime gt ntimes-1 then max_itime = ntimes-1
    
    r = (rmin + rmax)/2.
    rs = r[uniq(r,sort(r))]
    fields = strsplit(header,/extract)
    ip = where(fields eq percentile)
    i_nvalid = where(fields eq 'nvalid', n_nvalid)
    old_itime = itime
    for itime = min_itime, max_itime do begin
      for iradius = 0, nr-1 do begin
        radius = radii[iradius]
        iband = where(rmin eq radius - dr/2. and rmax eq radius + dr/2., nband)
        if disk then iband = where(rmin eq 0 and rmax eq radius + dr/2., nband)
        if nband eq 0 or nband gt 2 then stop
        if nband eq 2 then begin
          ; found multiple lines that have the requested ranges.
          ; This can happen when the disk and ring statistics are in the same file.
          ; As a sanity check, make sure they have the same data and cut the second one.
          ; sanity check won't work if there are NaNs. But I tested it and it seems to work okay.
          ;          if array_equal(m.field01[*,iband[0]], m.field01[*,iband[1]]) eq 0 then stop
          iband = iband[0]
        endif
        time_series[iradius,ifile] = m.field01[ip,iband]
        hovm[iradius,itime]  = m.field01[ip,iband]
        hovm2[iradius,itime] = m.field01[where(fields eq 'std'),iband]
        ; if there are fewer than 20 points in this ring or disk then it is bad.
        if n_nvalid gt 0 && m.field01[i_nvalid, iband] lt 20 then hovm[iradius,itime] = !VALUES.F_NAN
      endfor
      if file eq file_best_track then best_track_arr[itime]=1
    endfor
  endfor
  xsize = 0.5-ntimes/60000.
  pos = [0.15,0.2,0.3+xsize,0.9]
  
  nx = !D.X_SIZE * (pos[2]-pos[0])
  ny = !D.Y_SIZE * (pos[3]-pos[1])
  mintemp = -90
  maxtemp =  30
  format  = '(I3,"C")'
  if type eq 'SST' then begin
    mintemp = 26.5
    maxtemp = 31.5
    format  = '(F4.1,"C")'
  endif
  if type eq 'CMORPH' then begin
    mintemp = 0
    maxtemp = 30
    format  = '(I2,"mm/h")'
    loadct, 26, /silent
  endif
  if stregex(percentile, '^-?[0-9]+\.?', /boolean) then begin
    hovm = hovm*100
    time_series = time_series*100
    mintemp = 0
    maxtemp = 100
    units   = 'percent coverage'
    format  = '(I3,"%")'
  endif
  dummy = label_date(DATE_FORMAT=['%M %D'])
  plot, radii, replicate(0,nr), xrange=[min(radii)-10.,max(radii)+10.], yrange = [min(jdays), max(jdays)], ytickunits='Time', ytickformat='LABEL_DATE',pos=pos, ystyle=5,xstyle=5, $
    title=pgi+" "+percentile+"!C"+(hatch eq 1 ? "!Chatched is region of low standard deviation in ":"")+(disk?"disk":"range ring")
  ; xsize and ysize are ignored by X device. THat's why I use congrid in the TV command below.
  if !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0
  tv, bytscl(!D.NAME eq 'X' ? congrid(hovm,nx,ny) : hovm,min=mintemp,max=maxtemp,/nan),pos[0],pos[1],xsize=pos[2]-pos[0],ysize=pos[3]-pos[1], /norm
  if !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
  
  plot, radii, replicate(0,nr), xrange=[min(radii)-dr/2.,max(radii)+dr/2.], yrange = [min(jdays), max(jdays)], ytickunits='Time', ytickformat='LABEL_DATE',pos=pos, ystyle=1,xstyle=1, /noerase , $
    xtitle='km from center'+$
    (best_track?"!Cusing best track, if available":"")+(half_best_track?"!Cusing best track for mtm forecast hours > 12, if available":"")+$
    (mtm00?"!Cuse only 00 UTC consensus mtm model analysis and interpolate between them":"")+$
    "!Cblank horizontal lines indicate closest satellite image > "+string(block_minutes/2,format='(I0)')+" minutes away", yticklen=0.1,ytickinterval=1,yminor=2,$
    charthick=3.01, charsize=2.248
    if hatch eq 1 then contour, smooth(hovm2,[3,180],/nan), /noerase, levels=[10], position=pos, xstyle=5, ystyle=5, thick=2.014, /follow, /downhill
  if best_track || half_best_track then begin
    best_track_color = 206
    if !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0
    tv, best_track_color * (!D.NAME eq 'X' ? congrid(best_track_arr,nx/25,ny) : best_track_arr), pos[2]+0.01, pos[1], xsize=0.03, ysize=pos[3]-pos[1], /norm, /nan
    if !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
    xyouts, pos[2]+0.02, pos[1]-0.11, 'used best track', orientation=90, /norm
  endif
  mycolorbar,range=[mintemp,maxtemp], /vertical,position=[pos[2]+0.05,pos[1],pos[2]+0.1,pos[3]],title=units,/right,format=format, charthick=2.6, charsize=2.411
  if !D.NAME eq 'PS' then device, /close
  ; output the grid in ascii format
  close, /all
  openw, outtxt, output_dir + output_filename + ".txt", /get_lun
  caldat, min(jdays), month, day, year, hour, minute
  printf, outtxt, "start time= "+string(year,month,day,hour,minute, format='(I4.4,I2.2,I2.2," ",I2.2,I2.2," UTC")')
  printf, outtxt, "time increment=1 minute"
  printf, outtxt, "radii="
  printf, outtxt, radii, format='('+strtrim(nr,2)+'F8.2)'
  printf, outtxt, "hovm="
  hovm_with_missing = hovm
  imsg = where(finite(hovm) eq 0, nmsg)
  if nmsg gt 0 then hovm_with_missing[imsg] = -999.
  printf, outtxt, hovm_with_missing, format='('+strtrim(nr,2)+'F8.2)'
  close, outtxt

  if !D.NAME eq 'PS' then device, /close, /color, ysize=5, yoffset=4, xsize=7, /inches, bits_per_pixel=8, filename=output_dir+output_filename+'time_series.ps'  
  pos = [0.1,0.12,0.9,0.9]
  dummy = label_date(DATE_FORMAT=['%M %D'])
  plot, jdays, time_series[0,*], xtickunits='Time', xtickformat='LABEL_DATE', xtickinterval=1, xminor=2,$
    title=pgi+" "+units+" "+percentile+" in "+(disk?"disk":"range ring"), position=pos, ytitle=units, xtitle='!CTime',$
    xcharsize=0.99, /nodata, xstyle=1, charthick=2.3, ycharsize=1.4, yrange=[mintemp, maxtemp], ystyle=1
  time_series_radii = [190]
  for iseries = 0, n_elements(time_series_radii)-1 do begin
    radius = time_series_radii[iseries]
    colors = [10, 90, 180]
    oplot, jdays, time_series[where(radii eq radius),*], linestyle=iseries, color = colors[iseries], thick=3+radius/1000., noclip=1
    if disk then range = string(radius+dr/2,format='(I0)')+"km" else range = string(radius-dr/2,radius+dr/2, format='(I0,"-",I0,"km")') 
    xyouts, jdays[nfiles-1], time_series[where(radii eq radius),nfiles-1], range, color = colors[iseries]
  endfor
  if !D.NAME eq 'PS' then device, /close
end
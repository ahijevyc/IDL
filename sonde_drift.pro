pro sonde_drift
  ; Jeff Trapp said some model-sonde differences could be from sonde drift.
  ; 
  basedir='/glade/p/work/ahijevyc/mpex/May19Upsondes/'
  basedir='/Volumes/pecan2/ahijevyc/mpex/soundings/May19Upsondes/'
  files = file_search(basedir+'*_201305*[0-9]', count=nfiles)
  pwin = getwindows()
  if pwin ne !NULL then pwin.erase
  for ifile=0,nfiles-1 do begin
    file = files[ifile]
    eol = read_cls_sounding(file, use_first_line=1)
    color = ifile/(nfiles-1)*255
    
    jtime = min(eol.jday, ipt, /nan) ; use first time - not average time in sounding time series
    lon0 = eol.lon[ipt]
    lat0 = eol.lat[ipt]
    d = !NULL
    az = !NULL
    ;print, file_basename(file), min(eol.lon), max(eol.lon), min(eol.lat), max(eol.lat)
    for i=0,n_elements(eol.lon)-1 do begin
      if ~finite(eol.lat[i]) || ~finite(eol.lon[i]) then continue
      result = map_2points(lon0, lat0, eol.lon[i], eol.lat[i])
      d = [d, result[0]]
      az = [az, result[1]]
      ;if i eq n_elements(eol.lon)-1 then stop
    endfor
    d = d*!CONST.R_EARTH*!DTOR/1000.
    ;print, file_basename(file), min(d), max(d), min(az), max(az)
    theta = !DTOR*(90.-az)
    p = polarplot(d,theta,title=file_dirname(file),xrange=[-7,87],yrange=[-7,87], axis_style=0, $
      aspect_ratio=1,rgb_table=46,vert_colors=bytscl(eol.Press,min=100,max=1000), /current,overplot=p,name=file_basename(file))
    t = text(d[-1]*cos(theta[-1]),d[-1]*sin(theta[-1]),p.name,/data,font_size=9,clip=0)
  endfor
  cb = colorbar(target=p,orientation=1,range=[100,1000],tickinterval=100,title='hPa')
  n = 80
  for d = 0,125,5 do begin
    r = polarplot(replicate(d,n),findgen(n)*!PI*2/n,overplot=p, axis_style=0)
    t = text(d, 0, string(d,format='(I0,"km")'), /data, font_size=9, alignment=0.5,vertical_alignment=1.+d/50.)
  endfor
end
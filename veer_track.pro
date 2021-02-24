; copied from HPC Dec 9, 2020

pro veer_track
  ;track = read_atcf('/glade/work/ahijevyc/atcf/bal112015.dat')
  stop
  track = read_atcf('/Users/ahijevyc/bal142018.dat')
 
  ; The angles really add up.  
  ; So you may skip the first [skip] track segments.
  ; and wait until you are closer to landfall to start veering.
  skip=0 ; # of track segments to skip
  if skip then begin
      foreach key, track.keys() do begin
        ;print, key
        if key eq "TWOD" or key eq "ID" then begin
            ; TODO: figure out syntax. TWOD not needed. 
            ;foreach key2, track[key].keys() do (track[key])[key2] = ((track[key])[key2])[skip:*]
        endif else begin
            track[key] = (track[key])[skip:*]
        endelse
      endforeach
  endif
  stop
  m = map('Mollweide', fill_color='alice blue', limit=[min(track.lat)-10,min(track.lon)-10,max(track.lat)+10,max(track.lon)+10],margin=[0.1,0.1,0.2,0.1],/current)
  grid = m.MAPGRID & grid.thick=0 & grid.linestyle = 'dotted' & grid.LABEL_POSITION = 0
  m1 = mapcontinents(fill_color='beige', /continents)
  max_vmax = max(track.vmax,istormname)
  p = plot(track.lon, track.lat,overplot=m,/data,thick=3,title=track.basin[0]+track.cy[0]+" "+track.yyyy[0]+" "+track.stormname[istormname] )
  lb = text(track.lon[0], track.lat[0], track.init_yyyymmddhh[0], target=m, font_size=8, $
    vertical_alignment=0.5, alignment=0,/data)


  foreach alpha, [-3,-2,-1,0,1,2,3] do begin; deg per day
    print, "alpha", alpha
    ; initialize perturbed track with consensus (control) track.
    perturbed_lon = track["lon"]
    perturbed_lat = track["lat"]
    for i=0, n_elements(track.lon)-2 do begin
      ; define control segment from time i to time i+1
      lon0 = track.lon[i]
      lat0 = track.lat[i]
      lon1 = track.lon[i+1]
      lat1 = track.lat[i+1]
      ; days since start of track
      dt = track.valid_time[i+1] - track.valid_time[0]
      result = map_2points(lon0, lat0, lon1, lat1)
      ; get distance and heading of control track segment
      d = result[0]
      az = result[1]
      ; get heading of perturbed segment
      new_az = az + alpha * dt

      ; start of perturbed segment is end of previous perturbed segment
      lon0 = perturbed_track.lon[i]
      lat0 = perturbed_track.lat[i]
      ; head off in new direction for the distance of control segment.
      result = ll_arc_distance([lon0,lat0], d*!DTOR, new_az, /degrees)
      ; new lon/lat is end of perturbed segment
      perturbed_track.lon[i+1] = result[0]
      perturbed_track.lat[i+1] = result[1]
    endfor
    p1 = plot(perturbed_track.lon, perturbed_track.lat,overplot=m,alpha gt 0?'orange':'dark green',/data,thick=1)
    lb = text(perturbed_track.lon[-1], perturbed_track.lat[-1], string(alpha,format='(I+0)')+" deg/day", target=m, font_size=8, $
      fill_background=1, vertical_alignment=0.5*(1-cos(az*!DTOR)), alignment=0.5*(1+sin(-az*!DTOR)),/data)
  endforeach
  junk = timestamp_text()
  ofile = "/Users/ahijevyc/veer_track_plots/"+strcompress(p.title.string,/rem)+track.init_yyyymmddhh[0]+".png"
  p.window.save, ofile, resolution=180
  print, "saved "+ofile
  p.window.close
end

function atcf_basin, inlons, lats, subregion=subregion, px=px, py=py
  lons = inlons
  if n_elements(lats) ne n_elements(lons) then stop
  
  iwrap = where(lons ge 180., /null)
  if iwrap ne !NULL then lons[iwrap] = lons[iwrap] - 360.
  
  
  

  
  ; http://www.nrlmry.navy.mil/atcf_web/docs/database/new/database.html
  basins = replicate('XX', n_elements(lats))
  for i=0,n_elements(lats)-1 do begin
    lat = lats[i]
    lon = lons[i]
    case 1 OF
      (lat ge   0. && lat lt 60. &&   lon ge  100 && lon lt  180) : basin='WP'
      (lat ge   0. && lat lt 60. &&   lon ge   40 && lon lt  100) : basin='IO'
      (lat ge   0. && lat lt 60. &&   lon ge -180 && lon lt -140) : basin='CP'
      (lat ge   0. && lat lt 60. &&   lon ge -140 && lon lt    0) : begin
        if lat ge 30. then basin = lon lt -110 ? 'EP' : 'AL'
        if lat ge 10. && lat lt 30. then basin = lon lt -80 - 3./2.*(lat-10.) ? 'EP' : 'AL'
        if lat lt 10. then basin = lon lt  -80 ? 'EP' : 'AL'
      end
      (lat ge -60. && lat lt  0. && ((lon ge   20 && lon lt  180) || (lon ge -180 && lon lt -120) )) : basin='SH'
      (lat ge -60. && lat lt  0. &&   lon ge  -60 && lon lt    0) : basin='LS'
      else : basin = 'XX'
    endcase
    basins[i]=basin
  endfor
  
  case basin of
    'WP' : subregion = 'W'
    'IO' : subregion = lon lt 84 ? 'A' : 'B'
    'EP' : subregion = 'E'
    'CP' : subregion = 'C'
    'AL' : subregion = 'L'
    'SH' : subregion = (lon ge 20 && lon lt 135) ? 'S' : 'P'  
    'LS' : subregion = 'Q'
    else : subregion = ''
    
  endcase

  ; define region of interest with px, py vectors.
  ; only good for single-element arrays (inlons and lats). Just to get basin boundary.
  
  py = [60., 60., 0, 0, 60] ; latitudes of box
  ; longitudes of box
  if basin eq 'WP' then px = [ 100, 180, 180, 100, 100]
  if basin eq 'IO' then px = [  40, 100, 100,  40,  40]
  if basin eq 'CP' then px = [-180,-140,-140,-180,-180]
  if basin eq 'AL' then begin
    px = [-110,   0,   0,  -80, -80, -110,-110]
    py = [  60,  60,   0,    0,  10,   30,  60]  
  endif
  if basin eq 'EP' then begin
    px = [-140,-110,-110, -80, -80, -140,-140]
    py = [  60,  60,  30,  10,   0,    0,  60]
  endif

  return, basins
end

pro atcf_basin_figure
  m = map('Equirectangular',limit=[-5, -180, 70, 180], fill_color='light blue')
  grid = m.MAPGRID
  grid.thick=0
  grid.GRID_LATITUDE = 10
  grid.grid_longitude = 10
  grid.linestyle = 'dotted'
  grid.LABEL_POSITION = 0
  m1 = mapcontinents(fill_color='beige', /continents, thick=0.5)
  mc = mapcontinents()
  ml = m.limit
  print, ml
  m.refresh, /disable
  ; bunch of equally spaced labels showing the basin name ("AL", "WP", "IO", etc.)
  ; for i = ml[1],ml[3],9 do for j=ml[0],ml[2],9 do junk = text(i,j,atcf_basin(i,j),/data,alignment=0.5,vertical_alignment=0.5,font_size=5)
  regions = list([-120,20],[-90,25],[-150,25],[170,25],[60,20])
  colors = ['blue','red','green','purple','orange']
  thick=4
  foreach region, regions, i do begin
    basin = atcf_basin(region[0],region[1],px=px,py=py)
    basin_outline = plot(px, py, thick=thick, color=colors[i], overplot=m, name=basin,transparency=35)
    roi = Obj_New('IDLanROI', px, py)
    result = roi->ComputeGeometry( centroid = centroid)
    Obj_Destroy, roi
    tmp = text(centroid[0],centroid[1],basin, font_size=20, /data, color=colors[i], alignment=0.5, vertical_alignment=0.5)
  endforeach
  m.refresh
  m.window.save, '~/map.png'
end
  
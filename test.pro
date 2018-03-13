pro test

  lats = 180d*randomu(seed, 38)-90d
  lons = 360d*randomu(seed, 38)-180d
;  lats = [0d]
;  lons = [0d]
  time = julday(8, 23, 2013, 0, 0, 0)
  basedir = '/glade/scratch/ahijevyc/'
  mpas_name = 'GFS004'
  mpas = mpas_mesh(mpas_name)

  lons = -120 + findgen(50) # replicate(1,15)
  lats = replicate(1,50) # (5 + 3*findgen(15))
  lons = lons[0:*:5]
  lats = lats[0:*:5]

  latmin=-5 & latmax=85
  lonmin=-180 & lonmax=0

  map = map("mollweide", limit=[latmin, lonmin, latmax, lonmax])
;  grid = map.mapgrid
;  grid.linestyle="dotted"
  m1 = mapcontinents()

  forward_function TC_symbol
  ;t = text(lons, lats, strtrim(atcf_basin(lons, lats),2), /data, font_size=8, align=0.5)  
  storms = plot([-120], [5], sym_object=TC_symbol(35), overplot=map, sym_size=4)
;  lats = 180d*randomu(seed, 3)-90d
;  lons = 360d*randomu(seed, 3)-180d
;  storms = plot(lons, lats, sym_object=TC_symbol("hurricane"), overplot=map, sym_size=2)

  iCells = mpas_nearest_cell(lons, lats, mpas)
  iCell_neighbors = list()
;  for iCell = 0, n_elements(iCells)-1 do iCell_neighbors.add, mpas_neighbors(iCells[iCell], mpas.lonCell, mpas.latCell, mpas.nEdgesOnCell, mpas.cellsOnCell, range=900.)
;  for iCell = 0, n_elements(iCells)-1 do begin
;    ineighbors = iCell_neighbors[iCell]
;    ; filter for range
;    ineighbors = ineighbors[where(ineighbors.range gt 600 and ineighbors.range lt 800, /null)]
;    p = plot(mpas.lonCell(ineighbors.icell), mpas.latCell(ineighbors.icell), color='blue', linestyle=6, symbol='.', overplot=map)
;  endfor  
;  quadrant = floor((360+n.az) / 90) mod 4
;
;  n = n[where(quadrant eq 1)]


  
end

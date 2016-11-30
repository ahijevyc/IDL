pro mpas_to_latlon, file, delta=delta
  if n_elements(file) eq 0 then file = '/glade/scratch/wrfrt/mpas_al/2014101400/diagnostics.2014-10-20_00.00.00'
  if ~keyword_set(delta) then delta = 1.
  loadct, 39, /silent
  field = 'z_isobaric'
  f = mpas_read(file, field=field)
  nCells = n_elements(f.(0))
  
  mpas_name = nCells eq 535554L ? 'mpas_al' : 'mpas'
  mpas = mpas_mesh(mpas_name)
    
  ffield = f.(0)
  map_set, title=string(file,format='(a)'), /iso, limit=[-80, -180, 80, 180], /noborder
  dlon = delta
  zin = ffield[4,*,*]
  stride=1e8 / nCells
  plots,  mpas.lonCell[0:*:stride], mpas.latCell[0:*:stride], psym=1, color=bytscl(zin[0:*:stride])
  map_continents
  map_grid, /box

  stop
end
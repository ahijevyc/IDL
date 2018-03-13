pro read_mul4

  ; Read and plot multiple sensor precip analysis
  ; After converting to netCDF with ncl_convert2nc *.grb
  ; Add a .grb suffix to the input GRiB files before using ncl_convert2nc.


  if (!D.NAME eq 'PS') then device, /color, /close, bits_per_pixel=8
  loadct, 39
  pos = [0.05,0.05,0.95,0.95]
  
  
  infile = '/pecan/ahijevyc/trier/ubr4/u.nc'
  ncid = NCDF_OPEN(infile)            ; Open The NetCDF file
  
  NCDF_VARGET, ncid,  'gridlat_240', lat
  NCDF_VARGET, ncid,  'gridlon_240', lon
  NCDF_VARGET, ncid,  'A_PCP_240_SFC_acc24h' , pcp
  ncdf_attget, ncid, 'A_PCP_240_SFC_acc24h', '_FillValue', FillValue
  NCDF_CLOSE, ncid      ; Close the NetCDF file
  
    
  limit = [23, -120, 50, -73]
  map_set, 45, -98,  limit=limit, position = pos, /noborder, /lamber
  plots, lon, lat, color=bytscl(pcp, min=0, max=12), psym=3
  map_set, 45, -98,  /grid, limit=limit, position = pos, /noborder, /lamber, /usa, /noerase
  
  
  if (!D.NAME eq 'PS') then device, /close
  
  
end

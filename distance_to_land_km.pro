function distance_to_land_km, inlons, inlats

  get_lons = (inlons + 360d) mod 360 ; ensure 0-360
  get_lats = inlats
  n = n_elements(get_lons)
  if n_elements(get_lats) ne n then stop
  dland = replicate(!VALUES.D_NAN, n)
  
  dland_file = '/glade/work/ahijevyc/bin/6m.nc'
  ncid = ncdf_open(dland_file)
  did = ncdf_varid(ncid, 'dland')
  ncdf_varget, ncid, did, dland
  ncdf_attget, ncid, did, 'units', units
  if string(units) ne 'nm' then stop
  dland = dland * 1.852 ; to km
  latid = ncdf_varid(ncid,'lat')
  lonid = ncdf_varid(ncid,'lon')
  ncdf_varget, ncid, latid, lat
  ncdf_varget, ncid, lonid, lon
  ncdf_close, ncid
  
  is = value_locate(lon, get_lons)
  js = value_locate(lat, get_lats)

  return, dland[is,js]
end
pro test_eos
  file_name = '/mmmtmp/ahijevyc/AIRS.2013.05.07.006.L2.RetStd.v6.0.7.0.G13127162017.hdf'
  file_id = eos_sw_open(file_name)
  swath_id = eos_sw_attach(file_id, 'L2_Standard_atmospheric&surface_product')
  result = eos_sw_inqdatafields(swath_id, fieldlist, rank, numbertype)
  status = eos_sw_readfield(swath_id, 'H2OMMRSat', data)
  status = eos_sw_readfield(swath_id, 'Longitude', lon)
  status = eos_sw_readfield(swath_id, 'Latitude', lat)
  status = eos_sw_detach(swath_id)
  status = eos_sw_close(file_id)
  map_set, /grid, /contin
  contour, data[2,*,*], lon, lat, /overplot, nlevels=20, /cell_fill
  stop
end

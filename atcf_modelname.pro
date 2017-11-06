function atcf_modelname, inname
  out = inname
  for i=0,n_elements(inname)-1 do begin
    case inname[i] of
      'mpas': model = 'MPAS'
      'mpas_al' : model = 'MPS2'
      'mpas_wp' : model = 'MPS3'
      'mpas_ep' : model = 'MPS4'
      'mpas15_3': model = 'MPS5'
      'al' : model = 'MPS2'
      'wp' : model = 'MPS3'
      'ep' : model = 'MPS4'
      'GFS004' : model = 'GFSO'
      'GFS' : model = 'GFSO'
      'GFS_00Z' : model = 'GFSO'
      'mpas_old': model = 'MPAS'
      'MPS2': model = 'mpas_ep'
      'MPS3': model = 'mpas_wp'
      'MPS4': model = 'mpas_ep'
      'GFSO': model = 'GFS'
      'MPAS': model = 'MPAS'
      '4km': model = 'MPAS'
      'uni': model = 'MPAS'
      'hwt2017': model = 'MPAS'
      else : stop
    endcase
    out[i] = model
  endfor
  return, out
end


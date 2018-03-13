pro doBamex
  set_plot, 'ps'
  type = 'BOW'
  bdir = '/pecan/ahijevyc/BaMEx/system_rel/'
  !PATH = !PATH + ':' + bdir
;  subtypes = ['']
;  subtypes = ['IOP07A']
;  subtypes = ['sfc', 'elev', 'hghshr', 'lowshr']
  subtypes = ['', 'IOP01',  'IOP02',  'IOP04b', 'IOP06', 'IOP07A', 'IOP07B', 'IOP09', 'IOP10', 'IOP12', 'IOP13', 'IOP17b', 'sfc', 'elev', 'hghshr', 'lowshr']
  for isubtype=0, n_elements(subtypes)-1 do begin
    read_sondes, type, subtype=subtypes[isubtype], range=250
    cd, bdir+'byIOP'
    spawn, 'source da'
    cd, '../'
    spawn, ['make_grid', '-f', type], /noshell
    file_delete, ['wnd.sav', 'anom.sav', 'env.sav'], /allow_nonexistent
    asc2idl, type=type, subtype=subtypes[isubtype]+ " " + type + " "
    file_move, 'idl.ps', bdir+type+subtypes[isubtype]+'.ps', /overwrite
    km_per_deg = 6378.*2.*!PI/360. ; for equatorial radius = 6378 km, km_per_deg = 111.317 km
    yavg = STRMATCH(subtypes[isubtype], 'IOP*') ? 100. : 75.
    asc2idl, yavg = yavg/km_per_deg, type=type, subtype=subtypes[isubtype]+ " " + type + " "
    file_move, 'idl.ps', bdir+type+subtypes[isubtype] + STRING(yavg, FORMAT='(I3.3)')+'km.ps', /overwrite
  endfor
end

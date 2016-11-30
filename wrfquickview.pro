function nice_num, x_in
  s = string(x_in, format='(E7.1)')
  e = fix(strmid(s,2,/rev))
  m = strmid(s,0,3)
  s = (m lt 1.5) * 1 + (m ge 7.5) * 10 + (m ge 1.5 and m lt 3.5) * 2 + $
    (m ge 3.5 and m lt 7.5) * 5
  return, s * 10.^e
end


function WRFmap, file, xlong=xlong, xlat=xlat, _extra=_extra
  ncid = ncdf_open(file)
  ncdf_varget, ncid, ncdf_varid(ncid, 'XLAT'), xlat
  ncdf_varget, ncid, ncdf_varid(ncid, 'XLONG'), xlong
  
  ncdf_attget, ncid, 'CEN_LAT', cen_lat, /global
  ncdf_attget, ncid, 'CEN_LON', cen_lon, /global
  ncdf_attget, ncid, 'TRUELAT1', truelat1, /global
  ncdf_attget, ncid, 'TRUELAT2', truelat2, /global
  ncdf_attget, ncid, 'MAP_PROJ', imap_proj, /global
  if imap_proj eq 1 then map_proj = 'Lambert Conformal Conic'
  
  limit = [min(xlat),min(xlong),max(xlat),max(xlong)]
  map = map(map_proj, center_longitude=cen_lon, center_latitude=cen_lat, $
    standard_par1=truelat1,standard_par2=truelat2, limit=limit, title=file_basename(file),$
    _extra=_extra, /current, margin=[0.2,0.2,0.1,0.15])
  grid=map.mapgrid
  grid.linestyle='dotted'
  grid.label_position=0
  m1 = mapcontinents(/usa)
  map.window.refresh, /disable
  ; Open the states Shapefile in the examples directory
  myshape = OBJ_NEW('IDLffShape', '/glade/u/ssg/ys/opt/idl/idl84envi52/envi52/classic/data/vector/counties.shp')
  ; Get the number of entities so we can parse through them
  myshape->GetProperty, N_ENTITIES=num_ent
  ; Parsing through the entities and only plotting the state of
  ; Colorado
  FOR x=1, (num_ent-1) DO BEGIN
    ; Get the Attributes for entity x
    attr = myshape->GetAttributes(x)
    ; See if 'Colorado' is in ATTRIBUTE_1 of the attributes for
    ; entity x
    IF attr.ATTRIBUTE_1 EQ 'Oklahoma' || attr.ATTRIBUTE_1 EQ 'Texas' THEN BEGIN
      ; Get entity
      ent = myshape->GetEntity(x)
      lon = reform((*ent.VERTICES)[0, *])
      lat = reform((*ent.VERTICES)[1, *])
      if max(lon) lt (map.limit)[1] || min(lon) gt (map.limit)[3] || max(lat) lt (map.limit)[0] || min(lat) gt (map.limit)[2] then print, '' else begin
        ; Plot entity
        ;c = polyline(lon, lat, COLOR='red', target=map, /data, thick=0.5)
      endelse
      ; Clean-up of pointers
      myshape->DestroyEntity, ent
    ENDIF
  ENDFOR
  OBJ_DESTROY, myshape
  m1.order, /bring_to_front
  map.window.refresh
  return, map
end


pro WRFquickview
  ; Read and plot WRF2 netCDF landmask field with a simple tvscl command (i.e. no warping).
  ; Show the projection is correct by overlaying state and continent boundaries.
  
  atmos_const
  stride=1
  clobber=1
  debug=1
  
  basedir='/glade/p/work/ahijevyc/mpex/ensf/2013051915/mem*[6]/'
  basedir='/glade/scratch/ahijevyc/mpex/2013051915/mem[69]/'
  basedir='/glade/p/work/ahijevyc/NCAR_ENSEMBLE_2015/dev/DOMAINS/BMIN_OUTPUT/'
  infiles = file_search(basedir+'wrfout_d01*_18*00', count=nfiles)
  ;infiles = file_search(basedir+'tmp.nc', count=nfiles)
  for ifile=0,nfiles-1 do begin
    infile = infiles[ifile]
    print, infile
    outfile = file_dirname(infile,/mark)+file_basename(infile,'.nc')+'_bmin3.nc'
    if clobber eq 0 && file_test(outfile) eq 1 then continue
    spawn, 'cp '+infile+' '+outfile, result
    
    ncid = NCDF_OPEN(outfile,/write)            ; Open The NetCDF file
    
    NCDF_VARGET, ncid,  ncdf_varid(ncid,'LANDMASK'), LANDMASK      ; Read in variable 'LANDMASK'
    ncdf_varget, ncid, ncdf_varid(ncid, 'P'), P
    iNaN = where(p eq 9.96921e36, /null)
    if iNaN ne !NULL then p[iNaN] = !VALUES.D_NAN
    ncdf_varget, ncid, ncdf_varid(ncid, 'PB'), PB
    P = ( P + PB )* 0.01 ; convert from Pa to mb
    ncdf_varget, ncid, ncdf_varid(ncid, 'T'), T
    T = T+300.
    t = t*(p/1000.)^(!ATMOS.Rd/!ATMOS.Cpd) ; convert from potential temp (K) to temp (K)
    
    ncdf_varget, ncid, ncdf_varid(ncid, 'PH'), PH
    ncdf_varget, ncid, ncdf_varid(ncid, 'PHB'), PHB
    Z = (PH+PHB)/!ATMOS.g
    ncdf_varget, ncid, ncdf_varid(ncid, 'HGT'), terrain_hgt
    ncdf_varget, ncid, ncdf_varid(ncid, 'QVAPOR'), R
    ncdf_varget, ncid, ncdf_varid(ncid, 'XLAT'), xlat
    ncdf_varget, ncid, ncdf_varid(ncid, 'XLONG'), xlong
    ncdf_varget, ncid, ncdf_varid(ncid, 'Times'), ts
    j1970 = julday(1,1,1970,0,0,0)
    days_since_1970 = julday(string(ts[5:6]),string(ts[8:9]),string(ts[0:3]),string(ts[11:12]),string(ts[14:15]),string(ts[17:18])) - j1970
    
    ncdf_attget, ncid, 'CEN_LAT', cen_lat, /global
    ncdf_attget, ncid, 'CEN_LON', cen_lon, /global
    ncdf_attget, ncid, 'TRUELAT1', truelat1, /global
    ncdf_attget, ncid, 'TRUELAT2', truelat2, /global
    ncdf_attget, ncid, 'DX', dx, /global
    ncdf_attget, ncid, 'DY', dy, /global
    ncdf_attget, ncid, 'MAP_PROJ', imap_proj, /global
    if imap_proj eq 1 then map_proj = 'lambert'
    
    weid = ncdf_dimid(ncid, 'west_east')
    snid = ncdf_dimid(ncid, 'south_north')
    btid = ncdf_dimid(ncid, 'bottom_top')
    timedimid = ncdf_dimid(ncid, 'Time')
    
    ncdf_diminq, ncid, weid, junk, nx
    ncdf_diminq, ncid, snid, junk, ny
    ncdf_diminq, ncid, btid, junk, nz
    ncdf_diminq, ncid, timedimid, junk, ntime
    
    limit = [min(xlat),min(xlong),max(xlat),max(xlong)]
    ;    map = map('Lambert Conformal Conic', center_longitude=cen_lon, center_latitude=cen_lat, $
    ;      standard_par1=truelat1,standard_par2=truelat2, limit=limit, dimensions=[1000,800],title=file_basename(infile),$
    ;      layout=[2,2,ifile+1],/current)
    ;    m1 = mapcontinents(/usa)
    ;    grid=map.mapgrid
    ;    grid.linestyle='dotted'
    ;    grid.label_position=0
    ;    dots = plot(xlong, xlat, symbol='square', sym_filled=1, overplot=map, sym_size=1, vert_colors=bytscl(terrain_hgt[*]), $
    ;      rgb_table=66, linestyle=' ')
    ;    c = colorbar(target=dots,/border, range=[min(terrain_hgt),max(terrain_hgt)],orientation=1)
    
    entrainment_rates = [0.] & nerates=n_elements(entrainment_rates)
    fractional_fallouts = [0] & nfractional_fallouts=n_elements(fractional_fallouts)
    parcel_layers_mb = [0.] & nparcel_layer_thicks=n_elements(parcel_layers_mb)
    
    ; get the order of the dimensions right. I had ntime first, but this messed up ncdf_varput
    bmin3_rev   =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bmin3_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bmin_rev    =replicate(!VALUES.D_NAN,nx,ny,   nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bmin_pseudo =replicate(!VALUES.D_NAN,nx,ny,   nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    mcape_rev   =replicate(!VALUES.D_NAN,nx,ny,   nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    mcape_pseudo=replicate(!VALUES.D_NAN,nx,ny,   nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bminmax_rev   =replicate(!VALUES.D_NAN,nx,ny,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bminmax_pseudo=replicate(!VALUES.D_NAN,nx,ny,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bmax_rev   =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    bmax_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    cape_rev   =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    cape_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    cin_rev    =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    cin_pseudo =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    press_lcl  =replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    dTv_rev    =replicate(!VALUES.D_NAN,nx,ny,nz,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    dTv_pseudo =replicate(!VALUES.D_NAN,nx,ny,nz,nz,nerates,nfractional_fallouts,nparcel_layer_thicks,ntime)
    
    for itime=0,ntime-1 do begin
      for iew=104,nx-stride,stride do begin
        for ins=99,ny-stride,stride do begin
         
          p_profile = p[iew,ins,*,itime]
          t_profile = t[iew,ins,*,itime]
          r_profile = r[iew,ins,*,itime]
          
          for ilayer=0,nparcel_layer_thicks-1 do begin
            parcel_layer_mb = parcel_layers_mb[ilayer]
            for iff=0,nfractional_fallouts-1 do begin
              fractional_fallout = fractional_fallouts[iff]
              for ier=0,nerates-1 do begin
                entrainment_rate = entrainment_rates[ier]
                ;cape_sound_fast
                cape_sound, p_profile, t_profile, r_profile, CAPEP=CAPEP,TVPDIF=TVPDIF,$
                  CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
                  entrainment_rate=entrainment_rate, ice=ice, plcl=plcl, $
                  parcel_layer_mb = parcel_layer_mb, kpar=0,$
                  bminr=bmin3r,bminp=bmin3p, bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout
                if debug then begin
                  map_set, cen_lat, cen_lon, /Lambert, standard_par=[truelat1,truelat2], limit=[limit[0]+7,limit[1]+21,limit[2]-8,limit[3]-13], /noerase, title=file_basename(infile),/usa,pos=[0,.6,.25,.9], /cont
                  plots, xlong[iew,ins], xlat[iew,ins], psym=2
                endif
                ; replicate values across x/y stride window
                for ixs=0,stride-1 do begin
                  for iys=0,stride-1 do begin
                    bmin3_rev[     iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=bmin3r
                    bmin3_pseudo[  iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=bmin3p
                    bminmax_rev[   iew+ixs,ins+iys,    ier,iff,ilayer,itime]=max(bmin3r,/nan)
                    bminmax_pseudo[iew+ixs,ins+iys,    ier,iff,ilayer,itime]=max(bmin3p,/nan)
                    bmax_rev[      iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=bmaxr
                    bmax_pseudo[   iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=bmaxp
                    cape_rev[      iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=caper
                    cape_pseudo[   iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=capep
                    mcape_rev[     iew+ixs,ins+iys,    ier,iff,ilayer,itime]=max(caper,iz,/nan)
                    bmin_rev[      iew+ixs,ins+iys,    ier,iff,ilayer,itime]=bmin3r[iz]
                    mcape_pseudo[  iew+ixs,ins+iys,    ier,iff,ilayer,itime]=max(capep,iz,/nan)
                    bmin_pseudo[   iew+ixs,ins+iys,    ier,iff,ilayer,itime]=bmin3p[iz]
                    cin_rev[       iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=cinr
                    cin_pseudo[    iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=cinp
                    dTv_rev[       iew+ixs,ins+iys,*,*,ier,iff,ilayer,itime]=tvrdif
                    dTv_pseudo[    iew+ixs,ins+iys,*,*,ier,iff,ilayer,itime]=tvpdif
                    press_lcl[     iew+ixs,ins+iys,  *,ier,iff,ilayer,itime]=plcl
                  endfor; iys
                endfor; ixs
              endfor ;ier
            endfor ;iff
          endfor ; ilayer (parcel layer thickness)
        endfor ;ins
      endfor ;iew
    endfor ; itime
    
    xwin=960
    ywin=550
    vars = ['press_lcl',$
      'bmin3_rev',   'bmin3_pseudo',$
      'bmin_rev',    'bmin_pseudo', $
      'bminmax_rev', 'bminmax_pseudo', $
      'bmax_rev',    'bmax_pseudo', $
      'cape_rev',    'cape_pseudo', $
      'mcape_rev',   'mcape_pseudo', $
      'dTv_rev',     'dTv_pseudo', $
      'cin_rev',     'cin_pseudo' ]
      
    ncdf_control, ncid, /redef
    
    par_dimid = ncdf_dimid(ncid,'parcel_level')
    if par_dimid eq -1 then par_dimid = ncdf_dimdef(ncid, 'parcel_level', nz)
    
    er_dimid = ncdf_dimid(ncid,'entrainment_rate')
    if er_dimid eq -1 then er_dimid = ncdf_dimdef(ncid, 'entrainment_rate', nerates)
    er_id = ncdf_varid(ncid, 'entrainment_rate')
    if er_id eq -1 then er_id = ncdf_vardef(ncid,'entrainment_rate',[er_dimid], /double)
    ncdf_attput, ncid, er_id, 'units', '%/km'
    
    pt_dimid = ncdf_dimid(ncid,'parcel_averaging_layer_thickness')
    if pt_dimid eq -1 then pt_dimid = ncdf_dimdef(ncid, 'parcel_averaging_layer_thickness', nparcel_layer_thicks)
    pt_id = ncdf_varid(ncid, 'parcel_averaging_layer_thickness')
    if pt_id eq -1 then pt_id = ncdf_vardef(ncid,'parcel_averaging_layer_thickness',[pt_dimid], /double)
    ncdf_attput, ncid, pt_id, 'units', 'mb'
    ncdf_attput, ncid, pt_id, 'long_name', 'parcel averaging layer thickness centered on origin'
    
    ff_dimid = ncdf_dimid(ncid,'fractional_fallout')
    if ff_dimid eq -1 then ff_dimid = ncdf_dimdef(ncid, 'fractional_fallout', nfractional_fallouts)
    ff_id = ncdf_varid(ncid, 'fractional_fallout')
    if ff_id eq -1 then ff_id = ncdf_vardef(ncid,'fractional_fallout',[ff_dimid], /double)
    ncdf_attput, ncid, ff_id, 'long_name', 'fraction of precipitate that falls out of parcel instantly'
    
    
    
    time_vid = ncdf_varid(ncid, 'Time')
    if time_vid eq -1 then time_vid = ncdf_vardef(ncid,'Time',[timedimid], /double)
    ncdf_attput, ncid, time_vid, 'units', 'days since 1970-1-1 0:0:0'
    ncdf_attput, ncid, time_vid, 'long_name', 'time'
    
    ncdf_control, ncid, /endef
    
    for ivar=0,n_elements(vars)-1 do begin
      vid = ncdf_varid(ncid, vars[ivar])
      if vid eq -1 then begin
        ncdf_control, ncid, /redef
        dims = [weid,snid,btid,er_dimid,ff_dimid,pt_dimid,timedimid]
        if strmatch(vars[ivar], 'dT*') then dims = [weid,snid,btid,par_dimid,er_dimid,ff_dimid,pt_dimid,timedimid]
        if strmatch(vars[ivar], 'bminmax_*') || $
          strmatch(vars[ivar], 'mcape*') || $
          strmatch(vars[ivar], 'bmin_*') then dims = [weid,snid,er_dimid,ff_dimid,pt_dimid,timedimid]
        vid = ncdf_vardef(ncid, vars[ivar], dims, /double)
        ncdf_attput, ncid, vid, '_FillValue', !VALUES.D_NAN
        ncdf_attput, ncid, vid, 'missing_value', !VALUES.D_NAN
        
        ncdf_control, ncid, /endef
      endif
      string = "ncdf_varput, ncid, vid, " + vars[ivar]
      print, string
      status = execute(string)
      if status eq 0 then stop
    endfor
    
    ncdf_varput, ncid, er_id, entrainment_rates
    ncdf_varput, ncid, ff_id, fractional_fallouts
    ncdf_varput, ncid, pt_id, parcel_layers_mb
    ncdf_varput, ncid, time_vid, days_since_1970
    
    
    NCDF_CLOSE, ncid      ; Close the NetCDF file
    print, 'created '+outfile
  endfor ; ifile
end

pro closest_model_skewT, lon, lat, jday, var, var2=var2, entrainment_rate=entrainment_rate, kpar=kpar, debug=debug
  loadct, 39, /silent
  if ~keyword_set(debug) then debug=0
  
  basedir='/glade/p/work/ahijevyc/mpex/ensf/2013051915/mem6/'
  ;basedir='/glade/scratch/ahijevyc/mpex/2013051915/mem9/'
  basedir='/glade/scratch/ahijevyc/mpex/2013051915/mem6/'
  
  files = file_search(basedir+'wrfout_d03_2013-0?-??_??:??:00')
  ipos = transpose(strpos(files, '_d0')+5)
  year = strmid(files,ipos,4)
  month = strmid(files, ipos+5, 2)
  day = strmid(files, ipos+8, 2)
  hour = strmid(files, ipos+11, 2)
  min = strmid(files, ipos+14,2)
  sec = strmid(files, ipos+17,2)
  jdays = julday(month, day, year, hour, min, sec)
  dtime = min(abs(jdays-jday), iclosest)
  file = files[iclosest]
  ncid = NCDF_OPEN(file)            ; Open The NetCDF file
  atmos_const
  ncdf_varget, ncid, ncdf_varid(ncid, 'P'), P
  iNaN = where(p eq 9.96921e36, /null)
  if iNaN ne !NULL then p[iNaN] = !VALUES.D_NAN
  ncdf_varget, ncid, ncdf_varid(ncid, 'PB'), PB
  P = ( P + PB )* 0.01 ; convert from Pa to mb
  ncdf_varget, ncid, ncdf_varid(ncid, 'T'), T
  T = T+300.
  t = t*(p/1000.)^(!ATMOS.Rd/!ATMOS.Cpd) ; convert from potential temp (K) to temp (K)
  
  ncdf_varget, ncid, ncdf_varid(ncid, 'PH'), PH
  ncdf_varget, ncid, ncdf_varid(ncid, 'PHB'), PHB
  Z = (PH+PHB)/!ATMOS.g
  ncdf_varget, ncid, ncdf_varid(ncid, 'HGT'), terrain_hgt
  ncdf_varget, ncid, ncdf_varid(ncid, 'QVAPOR'), R
  ncdf_varget, ncid, ncdf_varid(ncid, 'XLAT'), xlat
  ncdf_varget, ncid, ncdf_varid(ncid, 'XLONG'), xlong
  weid = ncdf_dimid(ncid, 'west_east')
  snid = ncdf_dimid(ncid, 'south_north')
  btid = ncdf_dimid(ncid, 'bottom_top')
  timedimid = ncdf_dimid(ncid, 'Time')
  ncdf_attget, ncid, 'CEN_LAT', cen_lat, /global
  ncdf_attget, ncid, 'CEN_LON', cen_lon, /global
  ncdf_attget, ncid, 'TRUELAT1', truelat1, /global
  ncdf_attget, ncid, 'TRUELAT2', truelat2, /global
  ncdf_attget, ncid, 'DX', dx, /global
  ncdf_attget, ncid, 'DY', dy, /global
  ncdf_attget, ncid, 'MAP_PROJ', imap_proj, /global
  if imap_proj eq 1 then map_proj = 'lambert'
  
  ncdf_diminq, ncid, weid, junk, nx
  ncdf_diminq, ncid, snid, junk, ny
  ncdf_diminq, ncid, btid, junk, nz
  ncdf_diminq, ncid, timedimid, junk, ntime
  ncdf_close, ncid
  
  ; 42,31 (aad)
  ; 42,36 (aae)
  dist = min(sqrt((xlong-lon)^2+(xlat-lat)^2),i2d)
  result = array_indices(xlong, i2d)
  iew = result[0]
  ins = result[1]
  itime=0
  if debug then print, strmid(file,40), xlong[i2d], xlat[i2d], 100*dist, lon, lat, dtime*24*60, $
    format='(A,x,F8.3,F7.3,F6.2," from",F8.3, F7.3, F5.1,"min")'
  p_profile = p[iew,ins,*,itime]
  t_profile = t[iew,ins,*,itime]
  r_profile = r[iew,ins,*,itime]
  
  parcel_layer_mb = 50
  
  fractional_fallout = 0
  
  
  if n_elements(entrainment_rate) eq 0 then entrainment_rate = 0
  
  cape_sound, p_profile, t_profile, r_profile, CAPEP=CAPEP,TVPDIF=TVPDIF,$
    CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
    entrainment_rate=entrainment_rate, ice=ice, plcl=plcl, $
    parcel_layer_mb = parcel_layer_mb, parcel_params=parcel_params,  $
    TLVP=TLVP, kpar=kpar, $
    bminr=bminr,bminp=bminp, bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout
    
  MSE_par = (!ATMOS.Cpd*parcel_params.T + !ATMOS.g*Z + !ATMOS.LV0*parcel_params.R)/!ATMOS.Cpd
  
  bminmax = max(bminp,k)
  if debug then begin
    plot_skewt, TLVP[k,*] - !CONST.T0, !NULL, p_profile, thick=4, col_t=204, linestyle=2
    xyouts, !X.CRANGE[1], p_profile[k], 'Bmin '+string(bminmax,format='(F0.2)') , align=0, /data
    
    tvlct, oldct, /get
    tvlct, transpose(var.color), 0
    xyouts, 0.5, 0.95, strmid(file,45) + string(lon, lat, format='(x, F6.1,x,F5.1)'), align=0.5, /norm
    tvlct, oldct
  endif
  
  if n_elements(var) then begin
    result = execute('model = min('+var.name+',/nan)')
    if result ne 1 then stop
    if debug then xyouts, 0.5, 0.92, string(var.name,model,format='(A,"=",F0.1)'), align=0.5, /norm
    var.data = model
  endif
  if n_elements(var2) then begin
    result = execute('var2.data = min('+var2.name+',/nan)')
    if result ne 1 then stop
  endif
  
end




pro skewT_obs
  loadct, 39, /silent
  seedx = 0
  seed=seedx
  if !D.NAME eq 'PS' then device, bits=8, /color, /close
  debug=0
  
  if !D.NAME eq 'X' && debug gt 0 then begin
    device, decomposed=0
    window, 0, xsize=900, ysize=1100
    window, 1, xsize=900, ysize=1100
  endif
  
  basedir='/glade/p/work/ahijevyc/mpex/May19Upsondes/'
  atmos_const
  syms = list()
  
  parcel_layer_mb = 50
  
  fractional_fallout = 0
  
  entrainment_rate = 0
  
  CIN  = {name:'-cinp', range:[-95.,10], long_name:'CIN', units: 'J kg$^{-1}$', data:!VALUES.D_NAN, color:indgen(3)}
  Bmin = {name:'bminp', range:[-4.0, 0.2], long_name:'Bmin', units:'K', data:!VALUES.D_NAN, color:indgen(3)}
  MSE = {name:'MSE_par', range:[334., 350], long_name:'MSE of parcel / c$_p$', units:'K', data:!VALUES.D_NAN, color:indgen(3)}
  vars = [CIN, Bmin, MSE]
  nvars = n_elements(vars)
  
  pwin = getwindows('Map')
  if pwin ne !NULL then begin
    pwin.erase
    pwin.SetCurrent
  endif
  
  map = WRFmap('/glade/p/work/ahijevyc/mpex/ensf/2013051915/mem6/wrfout_d02_2013-05-19_18:30:00_dense_bmin3.nc',$
    font_size=14, current=pwin ne !NULL)
    
  ; erase these windows.
  wins = ['Plot','xy','xy2']
  for iwin = 0,n_elements(wins)-1 do begin
    pwin = getwindows(wins[iwin])
    if pwin ne !NULL then pwin.erase
  endfor
  
  
  for ivar=0,nvars-1 do begin
    seed = seedx ; repeat "random" colors with same seed
    var = vars[ivar]
    var2 = vars[ivar-1]
    
    pwin = getwindows('Plot')
    if pwin ne !NULL then pwin.SetCurrent
    p = plot(var.range,var.range, xtitle='Mobile Upsonde', title=var.long_name+' ('+var.units+')', $
      ytitle='WRF', xticklen=0.5, xsubticklen=0.05, layout=[ceil(sqrt(nvars)),ceil(sqrt(nvars)),ivar+1], $
      yticklen=0.5, ysubticklen=0.05, aspect_ratio=1, yrange=var.range, xstyle=1, ystyle=1, current=pwin ne !NULL)
      
    pwin = getwindows('xy')
    if pwin ne !NULL then pwin.SetCurrent
    p2 = plot(var.range,var.range, xtitle=var.long_name + ' ('+var.units+')', title='Mobile Upsonde', $
      ytitle=var2.long_name + ' (' + var2.units +')', xticklen=0.5, xrange=var.range,  xsubticklen=0.05, $
      layout=[ceil(sqrt(nvars)),ceil(sqrt(nvars)),ivar+1], yticklen=0.5, ysubticklen=0.05, yrange=var2.range, $
      xstyle=1, ystyle=1, current=pwin ne !NULL, /nodata)
    p2.window.name='xy'
    
    pwin = getwindows('xy2')
    if pwin ne !NULL then pwin.SetCurrent
    p3 = plot(var.range,var.range, xtitle=var.long_name + ' ('+var.units+')', title='WRF', $
      ytitle=var2.long_name + ' (' + var2.units +')', xticklen=0.5, xrange=var.range,  xsubticklen=0.05, $
      layout=[ceil(sqrt(nvars)),ceil(sqrt(nvars)),ivar+1], yticklen=0.5, ysubticklen=0.05, yrange=var2.range, $
      xstyle=1, ystyle=1, current=pwin ne !NULL, /nodata)
    p3.window.name='xy2'
    
    files = file_search(basedir+'*_201305*[0-9]', count=nfiles)
    for ifile=0,nfiles-1 do begin
      file = files[ifile]
      eol = read_cls_sounding(file, use_first_line=1)
      
      p_profile = eol.press
      t_profile = eol.T + !CONST.T0
      r_profile = mixr_sat(eol.Td, eol.press)/1000.
      
      kpar = 0
      if !D.NAME eq 'X' then wset, 0
      cape_sound, p_profile, t_profile, r_profile, CAPEP=CAPEP,TVPDIF=TVPDIF,$
        CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
        entrainment_rate=entrainment_rate, ice=ice, plcl=plcl, $
        parcel_layer_mb = parcel_layer_mb, kpar=kpar, parcel_params=parcel_params, $
        bminr=bminr,bminp=bminp, bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout
        
      MSE_par = (!ATMOS.Cpd*parcel_params.T + !ATMOS.g*eol.geopoalt_m + !ATMOS.LV0*parcel_params.R)/!ATMOS.Cpd
      
      name = file_basename(file)
      ipos = strpos(name,'20130519')
      name = strmid(name,0,ipos-1)+' '+strmid(name,ipos+8)
      name = strmid(name,0,strlen(name)-2)
      result = execute('obs = min('+var.name+',/nan)')
      if result ne 1 then stop
      result = execute('obs2 = min('+var2.name+',/nan)')
      if result ne 1 then stop
      
      color = fix(randomu(seed,3)*255B)
      tvlct, oldct, /get
      tvlct, transpose(color), 0
      if debug then xyouts, 0.5, 0.95, name +string(var.name, obs,format='(x,A,"=",F0.1)'), align=0.5, /norm, charthick=2
      var.color = color
      tvlct, oldct
      
      if !D.NAME eq 'X' && debug gt 0 then wset, 1
      first_time = 1
      mean_time = ~first_time
      if first_time then begin
        jtime = min(eol.jday, ipt, /nan) ; use first time - not average time in sounding time series
        lon = eol.lon[ipt]
        lat = eol.lat[ipt]
      endif
      if mean_time then begin
        jtime = mean(eol.jday, /nan) ; use average time in sounding time series
        lon = mean(eol.lon, /nan)
        lat = mean(eol.lat, /nan)
      endif
      closest_model_skewT, lon, lat, jtime, var, var2=var2, entrainment_rate=entrainment_rate, kpar=kpar, debug=debug
      model = var.data
      model2 = var2.data
      if debug then print, name, lon, lat, obs, model, format='(A12,F8.3,F7.3," obs:",F8.2," wrf:",F8.2)'
      
      sym_junk = {sym_color:color, sym_fill_color:color, sym_filled:1, data:1, label_font_size:7, sym_transparency:10}
      sym = symbol([obs], [model], symbol='star', clip=0, _extra=sym_junk, target=p, sym_size=2, label_string=name)
      print, obs, model
      sym = symbol([obs], [obs2], symbol='star', clip=0, _extra=sym_junk, target=p2, sym_size=2)
      sym = symbol([model], [model2], symbol='star', clip=0, _extra=sym_junk, target=p3, sym_size=2)
      if ivar eq 0 then begin
        pwin=GetWindows('Plot')
        (getwindows('Map')).SetCurrent
        launch = symbol(lon, lat, 'Star', _extra=sym_junk, target=map, sym_size=2.5, label_string=name)
        pwin.SetCurrent
      endif
    endfor
  endfor
  
  
  if !D.NAME eq 'PS' then device, /close
end

pro plot_bmin
  win = GetWindows(/current)
  if win ne !NULL then win.erase
  
  basedir='/glade/p/work/ahijevyc/mpex/ensf/2013051915/mem6/'
  basedir='/glade/scratch/ahijevyc/mpex/2013051915/mem[69]/'
  files = file_search(basedir+'wrfout_d03_2013-05-19_19:00:00_small_bmin3.nc',count=nfiles)
  fieldnames = ['bmin_pseudo', 'mcape_pseudo']
  for ifield = 0, n_elements(fieldnames)-1 do begin
    fieldname=fieldnames[ifield]
    c_value = findgen(9)/10
    ct = colortable(65)
    yrange=[0,4000]
    p=list()
    if strmatch(fieldname,'bmin*') then begin
      c_value = findgen(11)/10-1
      yrange = [-2,.5]
      ct = colortable(1)
      ct[208:*,*] = rebin(mean(ct[208:*,*],dim=2),48,3)
    endif
    c_color = bytscl(c_value)
    nrows = 1
    ncols = n_elements(fieldnames)
    for ifile=0,nfiles-1 do begin
      file = files[ifile]
      
      ncid = NCDF_OPEN(file)            ; Open The NetCDF file
      ncdf_varget, ncid, ncdf_varid(ncid, fieldname), field
      ncdf_close, ncid
      ;field = smooth(field,[5,5,1],/edge_truncate)
      x0 = 310-158
      y0 = 370-200
      dx = 250
      dy = 150
      box = replicate(!VALUES.F_NAN,dx,dy)
      for ie = 0,1 do begin
        name = file_basename(file_dirname(file))+(ie ? '+entrain.': '')

        ;map = WRFmap(file,xlong=xlong,xlat=xlat, dimensions=280*[ncols,nrows],title=name, $
        ;  font_size=6, layout=[ncols,nrows,ifield+nfiles*ncols*ifile+ncols*ie+1], /current, label_show=0)
        a = atan(-1./3)
        if ifile eq 1 then a = a + 5*!DTOR        
        for ix=-dx/2,dx/2-1 do begin
          for iy=-dy/2,dy/2-1 do begin
            bx = round(ix+dx/2.)
            by = round(iy+dy/2.)
            fx = round(x0 + cos(a)*ix - sin(a)*iy)
            fy = round(y0 + sin(a)*ix + cos(a)*iy)
            if ix eq -dx/2 and iy eq -dy/2 then b1 = [fx,fy]
            if ix eq dx/2-1 and iy eq -dy/2 then b2 = [fx,fy]
            if ix eq dx/2-1 and iy eq dy/2-1 then b3 = [fx,fy]
            if ix eq -dx/2 and iy eq dy/2-1 then b4 = [fx,fy]
            ;print, "bx,by=",bx,by, "fx,fy=",fx,fy,field[fx,fy,ie],format='(A,2I4,A,2I4,F5.1)'
            box[bx,by] = field[fx,fy,ie]
          endfor
        endfor
        ;blon = [xlong[b1[0],b1[1]], xlong[b2[0],b2[1]], xlong[b3[0],b3[1]], xlong[b4[0],b4[1]]]
        ;blat = [xlat[b1[0],b1[1]], xlat[b2[0],b2[1]], xlat[b3[0],b3[1]], xlat[b4[0],b4[1]]]
        
        ;dots = contour(field[*,*,ie], xlong, xlat, /fill, overplot=map, grid_units='deg', c_color=c_color, $
        ;  rgb_table=ct, c_value=c_value*nice_num(4.6*stddev(field,/nan)),transparency=30)
        ;box_line = polygon(blon, blat, /data, target=map, fill_background=0)
        ;cb = colorbar(target=dots, orientation=0, title=fieldname)
        
        ;lines = contour(field[*,*,ie], xlong, xlat, overplot=map,grid_units='deg', c_value=dots.c_value, $
        ;   color='black', c_label_show=[1,0,0], transparency=60, font_size=8, c_label_interval=1.)
        ;dots = contour(box, /fill, grid_units='deg', c_color=c_color, layout=[ncols,nrows,ifield+nfiles*ncols*ifile+ncols*ie+1], $
        ;  rgb_table=ct, c_value=c_value*nice_num(4.6*stddev(field,/nan)),transparency=30,/current,aspect_ratio=1, $
        ;  margin=[0.35,0.1,0.3,.1], title=fieldname+' '+name)
        
        
        linestyle = ie ? '__' : '-'
        color = ifile ? 'blue' : 'red'
        p.add, plot(mean(box,dim=2), layout=[ncols,nrows,(nrows-1)*ncols+ifield+1],/current, overplot=ifile || ie ? p[-1] : 0, $
          thick=ie?2:3, color=color,linestyle=linestyle, name=name, xrange=[ifield?0:0,dx], yrange=yrange, margin=[ifield?.12:.12,0.1,0.1,0.1])
      endfor ; entrainment
    endfor ; file
    if ifield eq 0 then l = legend(target=p,/relative,position=[1,1],vertical_spacing=0.01,font_size=9)
  endfor ; field
end


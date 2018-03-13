pro WRFquickview
  ; Read and plot WRF2 netCDF landmask field with a simple tvscl command (i.e. no warping).
  ; Show the projection is correct by overlaying state and continent boundaries.
  
  atmos_const
  stride=10
  force_new=0
  debug=0
  
  basedir='/Volumes/pecan2/ahijevyc/mpex/wrfout/2013051915/mem6/'
  infiles = file_search(basedir+'*d02*2013-*:00_small.nc', count=nfiles)
  ;infiles = file_search(basedir+'t.nc', count=nfiles)
  for ifile=0,nfiles-1 do begin
    infile = infiles[ifile]
    ncid = NCDF_OPEN(infile,/write)            ; Open The NetCDF file
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
    days_since_1970 = julday(strmid(ts,5,2),strmid(ts,8,2),strmid(ts,0,4),strmid(ts,11,2),strmid(ts,14,2),strmid(ts,17,2)) - j1970
    
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
    var_id = ncdf_varid(ncid, 'Time')
    if var_id ne -1L && force_new eq 0 then begin
      ncdf_close, ncid
      continue
    endif
    
    
    entrainment_rates = [0.,5,10] & nerates=n_elements(entrainment_rates)
    fractional_fallouts = [0.,0.5] & nfractional_fallouts=n_elements(fractional_fallouts)
    parcel_layer_mb = 50.
    
    ; get the order of the dimensions right. I had ntime first, but this messed up ncdf_varput
    bmin_rev=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    bmin_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    bmax_rev=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    bmax_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    cape_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    cape_rev=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    cin_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    cin_rev=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    press_lcl=replicate(!VALUES.D_NAN,nx,ny,nz,nerates,nfractional_fallouts,ntime)
    dTv_pseudo=replicate(!VALUES.D_NAN,nx,ny,nz,nz,nerates,nfractional_fallouts,ntime)
    dTv_rev=replicate(!VALUES.D_NAN,nx,ny,nz,nz,nerates,nfractional_fallouts,ntime)
    
    MSE = (!ATMOS.Cpd + r*!ATMOS.Cl)*T + !ATMOS.LV0*r + (1.+r)*!ATMOS.g *z
    toohigh = where(Z - rebin(terrain_HGT,nx,ny,nz) ge 3000., /null)
    if toohigh ne !NULL then MSE[toohigh] = 0.
    
    
    for itime=0,ntime-1 do begin
      for iew=0,nx-stride,stride do begin
        for ins=0,ny-stride,stride do begin
        
          p_profile = p[iew,ins,*,itime]
          t_profile = t[iew,ins,*,itime]
          r_profile = r[iew,ins,*,itime]
          MSE_profile = MSE[iew,ins,*,itime]
          
          junk = max(MSE_profile,k)
          ;parcel_origin_P = p_profile[k]
          ;parcel_origin_T = mean(interpol(T_profile, P_profile, [parcel_origin_P+dP/2.,parcel_origin_P-dP/2.]))
          ;parcel_origin_R = mean(interpol(R_profile, P_profile, [parcel_origin_P+dP/2.,parcel_origin_P-dP/2.]))
          
          
          for iff=0,nfractional_fallouts-1 do begin
            fractional_fallout = fractional_fallouts[iff]
            for ier=0,nerates-1 do begin
              entrainment_rate = entrainment_rates[ier]
              cape_sound, p_profile, t_profile, r_profile, CAPEP=CAPEP,TVPDIF=TVPDIF,$
                CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
                entrainment_rate=entrainment_rate, ice=ice, plcl=plcl, $
                parcel_layer_mb = parcel_layer_mb, $
                ;              parcel_origin_P=parcel_origin_P,$
                ;              parcel_origin_T=parcel_origin_T,$
                ;              parcel_origin_R=parcel_oritin_R,$
                bminr=bminr,bminp=bminp, bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout
                                
              ; copy values across x/y stride window
              for ixs=0,stride-1 do begin
                for iys=0,stride-1 do begin
                  bmin_rev[   iew+ixs,ins+iys,  *,ier,iff,itime]=bminr
                  bmin_pseudo[iew+ixs,ins+iys,  *,ier,iff,itime]=bminp
                  bmax_rev[   iew+ixs,ins+iys,  *,ier,iff,itime]=bmaxr
                  bmax_pseudo[iew+ixs,ins+iys,  *,ier,iff,itime]=bmaxp
                  cape_pseudo[iew+ixs,ins+iys,  *,ier,iff,itime]=capep
                  cape_rev[   iew+ixs,ins+iys,  *,ier,iff,itime]=caper
                  cin_pseudo[ iew+ixs,ins+iys,  *,ier,iff,itime]=cinp
                  cin_rev[    iew+ixs,ins+iys,  *,ier,iff,itime]=cinr
                  press_lcl[  iew+ixs,ins+iys,  *,ier,iff,itime]=plcl
                  dTv_pseudo[ iew+ixs,ins+iys,*,*,ier,iff,itime]=tvpdif
                  dTv_rev[    iew+ixs,ins+iys,*,*,ier,iff,itime]=tvrdif
                endfor; iys
              endfor; ixs
            endfor ;ier
          endfor ;iff
        endfor ;ins
      endfor ;iew
    endfor ; itime
    
    xwin=960
    ywin=550
    vars = ['press_lcl','bmin_rev', 'bmin_pseudo', 'bmax_rev', 'bmax_pseudo', 'cape_pseudo','cape_rev','dTv_pseudo','dTv_rev','cin_pseudo','cin_rev']
    
    ncdf_control, ncid, /redef
    par_dimid = ncdf_dimid(ncid,'parcel_level')
    if par_dimid eq -1 then par_dimid = ncdf_dimdef(ncid, 'parcel_level', nz)
    
    er_dimid = ncdf_dimid(ncid,'entrainment_rate')
    if er_dimid eq -1 then er_dimid = ncdf_dimdef(ncid, 'entrainment_rate', nerates)
    er_id = ncdf_varid(ncid, 'entrainment_rate')
    if er_id eq -1 then er_id = ncdf_vardef(ncid,'entrainment_rate',[er_dimid], /double)
    ncdf_attput, ncid, er_id, 'units', '%/km'
    ncdf_attput, ncid, /global, 'parcel_averaging_layer_thickness_mb', parcel_layer_mb
    
    ff_dimid = ncdf_dimid(ncid,'fractional_fallout')
    if ff_dimid eq -1 then ff_dimid = ncdf_dimdef(ncid, 'fractional_fallout', nfractional_fallouts)
    ff_id = ncdf_varid(ncid, 'fractional_fallout')
    if ff_id eq -1 then ff_id = ncdf_vardef(ncid,'fractional_fallout',[ff_dimid], /double)
    
    time_vid = ncdf_varid(ncid, 'Time')
    if time_vid eq -1 then time_vid = ncdf_vardef(ncid,'Time',[timedimid], /double)
    ncdf_attput, ncid, time_vid, 'units', 'days since 1970-1-1 0:0:0'
    ncdf_attput, ncid, time_vid, 'long_name', 'time'
    
    ncdf_control, ncid, /endef
    
    for ivar=0,n_elements(vars)-1 do begin
      vid = ncdf_varid(ncid, vars[ivar])
      if vid eq -1 then begin
        ncdf_control, ncid, /redef
        dims = [weid,snid,btid,er_dimid,ff_dimid,timedimid]
        if strmatch(vars[ivar], 'dT*') then dims = [weid,snid,btid,par_dimid,er_dimid,ff_dimid,timedimid]
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
    ncdf_varput, ncid, time_vid, days_since_1970
    
    
    NCDF_CLOSE, ncid      ; Close the NetCDF file
  endfor ; ifile
end

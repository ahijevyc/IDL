; copied to yellowstone Oct 28,2014

pro add_latlon_field, infile, field
  file=infile
  ; copy the field and its dimensions to new file.
  ; Assumes dimensions have coordinate variables,
  ; i.e. corresponding variables with same names.
  ncid = ncdf_open(file)
  names = !NULL
  sizes = list()
  dimv = list()
  dimids = list()
  dimvarids = list()
  result = ncdf_varinq(ncid,ncdf_varid(ncid,'height_500hPa'))
  for idim=0,result.ndims-1 do begin
    dimid = result.dim[idim]
    ncdf_diminq, ncid, dimid, name, vsize
    names=[names,name]
    sizes.add, vsize
    ncdf_varget, ncid, ncdf_varid(ncid,name), v
    dimv.add, v
  endfor
  ncdf_close, ncid
  file = file_dirname(file,/mark)+field.name+strmid(file_basename(file),11)
  ncid = ncdf_create(file,/clobber)
  for idim=0,result.ndims-1 do begin
    dimids.add, ncdf_dimdef(ncid,names[idim],sizes[idim])
    dimvarids.add, ncdf_vardef(ncid,names[idim],dimids[-1])
  endfor
  
  varid = ncdf_vardef(ncid, field.name, indgen(result.ndims), /double)
  ncdf_attput, ncid, varid,  'long_name', field.long_name
  ncdf_control, ncid, /endef
  ncdf_varput, ncid, varid, field.data
  for idim=0,result.ndims-1 do ncdf_varput, ncid, dimids[idim], dimv[idim]
  ncdf_close, ncid
  
end


pro meanT
  mpas_domain = 'mpas'
  basedir = '/glade/scratch/ahijevyc/'+mpas_domain+'/2013*00/latlon_0.500deg_025km/'
  year = strmid(basedir, strpos(basedir,'/201')+1 ,4)
  files = file_search(basedir+'diagnostics.201*025km.nc', count=nfiles)
  for ifile = 0, nfiles-1, 1 do begin
    file = files[ifile]
    ncid = ncdf_open(file)
    
    if ncdf_varid(ncid, 'lat') ne -1L then begin
      ncdf_varget, ncid, ncdf_varid(ncid, 'lat'), lat
      ncdf_varget, ncid, ncdf_varid(ncid, 'lon'), lon
      latCell = lat ## replicate(1,n_elements(lon))
      lonCell = replicate(1,n_elements(lat)) ## lon
      nx = n_elements(lon)
      ny = n_elements(lat)
    endif else begin
      mpas = mpas_mesh(mpas_domain)
      latCell = mpas.latCell[0:*:3]
      lonCell = mpas.lonCell[0:*:3]
    endelse
    ncdf_varget, ncid, ncdf_varid(ncid, 'height_200hPa'), height_200hPa
    ncdf_varget, ncid, ncdf_varid(ncid, 'height_500hPa'), height_500hPa
    ncdf_varget, ncid, ncdf_varid(ncid, 'relhum_200hPa'), relhum_200hPa
    ncdf_varget, ncid, ncdf_varid(ncid, 'relhum_500hPa'), relhum_500hPa
    ncdf_varget, ncid, ncdf_varid(ncid, 'temperature_200hPa'), temperature_200hPa
    ncdf_varget, ncid, ncdf_varid(ncid, 'temperature_500hPa'), temperature_500hPa
    if year ge 2014 then begin
      ncdf_varget, ncid, ncdf_varid(ncid, 't_iso_levels'), t_iso_levels
      ncdf_varget, ncid, ncdf_varid(ncid, 't_isobaric'), t_isobaric
      ncdf_varget, ncid, ncdf_varid(ncid, 'z_iso_levels'), z_iso_levels
      ncdf_varget, ncid, ncdf_varid(ncid, 'z_isobaric'), z_isobaric
      ncdf_varget, ncid, ncdf_varid(ncid, 'meanT_500_300'), meanT_500_300
    endif
    
    ncdf_close, ncid
    
    atmos_const
    eps = !ATMOS.Rd/!ATMOS.Rv ; epsilon
    
    debug = 0 ; 1=text messages 2=plots too.
    GFDL_tave_bias = !NULL
    lin_bias = !NULL
    lin_dTv1_bias = !NULL
    esat_bias = !NULL
    esat_dTv1_bias = !NULL
    esat_dTv2_bias = !NULL
    est = fltarr(nx,ny)
    dest = fltarr(nx,ny)
    est1 = est
    dest1 = dest
    est2 = est
    dest2 = dest
    for i = 0, nx-1, 1 > debug*35 do begin
      for j = 0, ny-1, 1> debug*35 do begin
        if n_elements(mpas) gt 0 then begin
          dlon = 2.5
          dlat = 2.5
          inlon = i * dlon + min(lonCell)
          inlat = j * dlat + min(latCell)
          dx = atan(sin(!DTOR*(lonCell-inlon)), cos(!DTOR*(latCell-inlat)))*!RADEG
          d = sqrt(dx^2. + (latCell-inlat)^2.)
          mind = min(d, iCell)
        endif
        title = string(latCell[i,j], lonCell[i,j], format='(F6.2,"N",F8.2,"E")')
        
        dZ_actual = height_200hPa[i,j] - height_500hPa[i,j]
        T25 = [temperature_200hPa[i,j], temperature_500hPa[i,j]]
        RH25 = [relhum_200hPa[i,j], relhum_500hPa[i,j]]
        if n_elements(meanT_500_300) gt 0 then mT53 = meanT_500_300[i,j]
        P25 = [200,500.]
        qsat = MIXR_SAT(T25, P25)/1000. ; saturation mixing ratio
        w = qsat * RH25
        Tv = T25 * (1. + w/eps) / (1. + w)
        
        avg_Tv = total(Tv*alog(P25))/total(alog(P25)) ; pressure-weighted virtual T (avg_Tv)
        dZ = !ATMOS.Rd * alog(500./200.) * avg_Tv / !CONST.Gn
        dTv1 = dZ_actual*!CONST.Gn / !ATMOS.Rd / alog(500./200.) - avg_Tv
        if debug then print, dZ_actual, format='("actual 500-200hPa thickness = ",F6.1, "m")'
        if debug then print, dZ, dTv1, format='("estimated dZ from avg_Tv = ",F6.1, "m (need to add ",F4.1,"K to avg_Tv)")'
        
        lin = interpol(T25,alog(P25),alog(400.))
        if n_elements(mT53) gt 0 then begin
          lin_bias = [lin_bias, lin - mT53]
          dest[i,j] = lin - mT53
          lin_dTv1_bias = [lin_dTv1_bias, lin + dTv1 - mT53]
        endif
        
        est[i,j] = lin
        
        theta = T25 * (1000./P25)^(!ATMOS.Rd/!ATMOS.Cpd) ; potential temp
        thetae = theta * exp(!ATMOS.LV0*qsat/!ATMOS.Cpd/T25) ; saturation equivalent potential temp
        dte = thetae[0]-thetae[1]
        if debug then print, p25, thetae, dte, format='(2(I3,"hPa "),x,2(F6.1,"K"),3x,"delta-thetae = ",F4.1, "K")'
        thetae400 = thetae[1] + dte /(alog(200)-alog(500)) * (alog(400)-alog(500))
        if debug then print, thetae400, format='("interpolated thetae at 400hPa = ",F6.1, "K")'
        
        T400 = TSA(thetae400, 400.)
        if n_elements(mT53) gt 0 then esat_bias = [esat_bias, T400 - mT53]
        dP = 50
        p_interp = 200 + dp*findgen(300/dP+1) + dP/2.
        thetae_interp = interpol(thetae,alog(P25),alog(p_interp))
        T_interp = TSA(thetae_interp, P_interp)
        RH_interp = interpol(RH25,alog(P25),alog(p_interp))
        q_interp = RH_interp * mixr_sat(T_interp,p_interp)/1000.
        Tv_interp = T_interp * (1. + q_interp/eps) / (1. + q_interp)
        if debug gt 1 then begin
          skewt, [-25,10], prange=[510.,190], title = title
          plot_skewt, T_interp-!CONST.T0, !NULL, P_interp, thick=1, col_T=[255,165,0]
          plot_skewt, T25-!CONST.T0, !NULL, P25, col_T=[0,0,255], thick=1
          plot_skewt, [T25[0],reform(t_isobaric[i,j,*])]-!CONST.T0, !NULL, [200,t_iso_levels/100.], col_T=[0,127,0],thick=1
          plot_skewt, [mT53,mT53]-!CONST.T0, !NULL, [400.,400], col_T=[0,127,0], psym=2
          plot_skewt, replicate(mean(t_isobaric[i,j,*]),2)-!CONST.T0, !NULL, [400.,400], col_T=[255,255,0], psym=2
          plot_skewt, Tv_interp-!CONST.T0, !NULL, P_interp, thick=1, col_T=[255,165,0],linestyle=1
        endif
        
        dlnP = alog((p_interp-dp/2.)/shift(p_interp-dp/2.,-1))
        dZ = -!ATMOS.Rd * Tv_interp * dlnP / !CONST.Gn
        dZ_est = total(dZ[0:-2])
        dTv2 = (dZ_est - dZ_actual) * !CONST.Gn / !ATMOS.Rd / alog(200/500.)
        if debug then print, dZ_est, dTv2, format='("estimated dZ from thetae_interp avg_Tv = ",F6.1, "m (need to add ",F4.1,"K to avg_Tv)")'
        double_check = total( -!ATMOS.Rd * (Tv_interp[0:-2] + dTv2) * dlnP[0:-2] / !CONST.Gn )
        if abs(dZ_actual - double_check) gt 0.01 then stop
        
        
        if 0 then begin
          ; Hypsometric height using the best available levels in 2014 (500-300-50,200 hPa T, 500,200 RH)
          ; Does it match 200-500hPa dZ?
          if array_equal(t_iso_levels,[30000,35000,40000,45000,50000]) ne 1 then stop
          p_model = [200., t_iso_levels/100]
          t_model = [T25[0], reform(t_isobaric[i,j,*])]
          p_interp_model_layer = [250, 325., 375, 425, 475]
          t_interp_model_layer = interpol(t_model, alog(p_model), alog(p_interp_model_layer))
          q_interp_model_layer = interpol(RH25,alog(P25),alog(p_interp_model_layer)) * mixr_sat(t_interp_model_layer,p_interp_model_layer)/1000.
          Tv_model_layer = t_interp_model_layer * (1. + q_interp_model_layer/eps) / (1. + q_interp_model_layer)
          dlnP_model_layer = alog(p_model/[300,350,400,450,500])
          dZest_model_layer = -!ATMOS.Rd * Tv_model_layer * dlnP_model_layer / !CONST.Gn
          if debug then print, p_model, total(dZest_model_layer), format='("estimated dZ from model T,RH at",6(I4,","),"hPa = ",F6.1, "m")'
        endif
        if n_elements(mT53) gt 0 then begin
          gfdl_tave_bias = [gfdl_tave_bias, mean(t_isobaric[i,j,*]) - mT53]
          
          ; dTv1 based on fixing T profile; dTv2 based on fixing interpolated theta-e profile.
          esat_dTv2_bias = [esat_dTv2_bias, T400 + dTv2 - mT53]
          esat_dTv1_bias = [esat_dTv1_bias, T400 + dTv1 - mT53]
          dest1[i,j] = T400 + dTv1 - mT53
          dest2[i,j] = T400 + dTv2 - mT53
        endif
        est1[i,j] = T400 + dTv1
        est2[i,j] = T400 + dTv2
        
        
        if debug gt 1 then begin
          pwin = GetWindows('Plot')
          if pwin ne !NULL then begin
            pwin.SetCurrent
            pwin.erase
          endif
          pnew = plot(t_isobaric[i,j,*], alog(t_iso_levels/100.), 'g-*', /current, yrange=alog([500,200]), $
            xticklen=1, xsubticklen=0.025, axis_style=1, yticklen=1, ysubticklen=1, thick=2, sym_size=2,$
            name='300-500-50hpa', sym_thick=2, title=title)
          pold = plot(T25,alog(P25), 'b-', overplot=pnew, yminor=0, ymajor=0, thick=2, sym_thick=2, $
            name='200-500hPa')
          s = symbol(lin+dTv1,alog(400.),'Star',/data,sym_color='blue', sym_size=2)
          ax = pnew.axes
          ax[1].tickvalues = alog([500.,400.,300,200])
          ax[1].tickname = string(exp(ax[1].tickvalues),format='(I0)')
          
          pold.xrange=[mT53-8, mT53+8]
          pold.yrange=alog([500,300])
          s = symbol(T400,alog(400.),'Star',/data,sym_color='orange',/sym_filled,sym_size=2,$
            label_string='$\theta_{es}$ int', label_position='T', label_color='orange')
          s = symbol(T400+dTv1,alog(400.),'Star',/data,sym_color='orange',sym_size=2)
          
          s = symbol(mT53,alog(400.),'Star',/data, label_color='green', sym_color='green', $
            /sym_filled,sym_size=2, label_string='T$_{500-300}$', label_position='B')
            
          s = symbol(mean(t_isobaric[i,j,*]),alog(400.),'Star',/data, sym_color='black', sym_fill_color='yellow', $
            /sym_filled,sym_size=2, label_string='tave', label_position='T')
          l = legend(target=[pnew,pold])
        endif
        if debug then stop
      endfor ; inlon
    endfor ; inlat
    series = list(gfdl_tave_bias, lin_bias, esat_bias, lin_dTv1_bias, esat_dTv1_bias, esat_dTv2_bias)
    Te_str = '$interpolated \theta_e^* in \itln(p)\rm$'
      T_str = '$interpolated T in \itln(p)\rm$'
      ytickname = ['$\Sigma T(500:300:-50)/5$', T_str, Te_str, T_str, Te_str, Te_str]
      ytickname[3:4] = ytickname[3:4] + ' + $\DeltaT_{v,T}$'
      ytickname[5] = ytickname[5] + ' + $\DeltaT_{v,\theta_e^*}$'
    if (debug gt 0) then begin
      pwin = GetWindows('BoxPlot')
      if pwin ne !NULL then begin
        pwin.SetCurrent
        pwin.erase
      endif
      bpd = createboxplotdata(series)
      errtitle='error (K)'
      b = boxplot(bpd, current = pwin ne !NULL, /horizontal, xtitle=errtitle, yminor=0, margin=[0.35,0.12,0.05,0.12], $
        title=file, background_color='linen', fill_color='burlywood',ytickname=ytickname, yticklen=0, $
        yrange=[series.count(),-1], ytickvalues=indgen(series.count()), layout=[2,3,2], dimensions=[1200,1000], $
        xtickinterval=1)
      z = plot([0,0],b.yrange,color='burlywood',overplot=b) ; zero-line
      mcrud = {grid_units:'degrees', image_location:[-180,-5], image_dimensions:[360,55],map_projection:'Mercator',$
        limit:[-5,-180,50,180], linestyle:'none', label_show:0, current:1}
      scrud = {min_value:248,max_value:260}
      dcrud = {min_value:-4,max_value:4,title:file}
      imgs = list()
      imgs.add, image(est, rgb_table=colortable(74,/reverse), _extra=create_struct(mcrud,scrud), title=ytickname[1], layout=[2,4,3])
      imgs.add, image(dest, rgb_table=colortable(70,/reverse), _extra=create_struct(mcrud,dcrud), layout=[2,4,4])
      imgs.add, image(est1, rgb_table=colortable(74,/reverse), _extra=create_struct(mcrud,scrud), title=ytickname[4], layout=[2,4,5])
      imgs.add, image(dest1, rgb_table=colortable(70,/reverse), _extra=create_struct(mcrud,dcrud), layout=[2,4,6])
      imgs.add, image(est2, rgb_table=colortable(74,/reverse), _extra=create_struct(mcrud,scrud), title=ytickname[5], layout=[2,4,7])
      imgs.add, image(dest2, rgb_table=colortable(70,/reverse), _extra=create_struct(mcrud,dcrud), layout=[2,4,8])
      imgs.add, image(meanT_500_300, rgb_table= colortable(74,/reverse), _extra=create_struct(mcrud,scrud),title='actual '+file, layout=[2,3,1])
      for img=0,imgs.count()-1 do begin
        imgs[img].Select
        cb = colorbar(target=imgs[img], title=img mod 2 ?  errtitle :'$T_{500-300} (K)$')
          mc = mapcontinents()
      endfor
    endif
    add_latlon_field, file, {data:est2, name:'est2_meanT_500_300', long_name:ytickname[5]}
    add_latlon_field, file, {data:est1, name:'est1_meanT_500_300', long_name:ytickname[4]}
  endfor
end


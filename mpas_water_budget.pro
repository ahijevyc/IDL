pro mpas_water_budget, date=date, model_name=model_name, debug = debug
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(model_name) then model_name = 'wp' ; 'mpas' or 'mpas2'
  if ~keyword_set(date) then date = '2016070300' ; try 4-digit year for seasonal average
  basedir = '/glade/p/nmmm0024/'+model_name+'/'+date+'/'
  basedir = '/glade/scratch/mpasrt/'+model_name+'/'+date+'/'
  if model_name eq "GFS" then basedir = '/glade/scratch/ahijevyc/GFS/2014082300/'
  atmos_const

  model = mpas_mesh(model_name)
  latCell = model.latCell
  lonCell = model.lonCell
  areaCell = model.areaCell
  ;landmask = model.landmask

  latbound = 90
  title = model.name + "  latitude range " + string(177b) + string(latbound, format='(F4.1)') + '!Z(00B0)'
  imask = where(abs(latCell) ge latbound)
  areaCell[imask] = !VALUES.D_NAN
  files = file_search([basedir+'diag*.*_'+(debug?'00':'??')+'.00.00.nc'], count=nfiles)
  if strlen(date) eq 4 then files = file_search('/glade/p/work/ahijevyc/mpas_plots/'+model.name+'/f???.nc', count=nfiles)
  xtimes = replicate(!VALUES.D_NAN, nfiles)
  fieldnames=['precipw','rain','lh']
  nfields=n_elements(fieldnames)
  ffields = replicate(!VALUES.F_NAN, nfiles,nfields) ; times x fields array

  for ifile = 0, nfiles-1 do begin
    file = files[ifile]
    xtimes[ifile] = mpas_diagnostics_jday(file)
    f = mpas_read(file)
    for ifield=0,nfields-1 do begin
      fieldname=fieldnames[ifield]
      ffield = get_structure_tag(f,fieldname)
      ffield[imask] = !VALUES.F_NAN
      ; Weight by area
      global_ffield = total(areaCell*ffield,/nan)/total(areaCell,/nan)
      if fieldname eq 'lh' then begin
        global_ffield = global_ffield * 3600.*24./!atmos.Lv0 ; 0.0345 ; convert latent heat flux from W/m^2 to mm/day
      endif
      print, file, global_ffield
      ffields[ifile,ifield] = global_ffield
    endfor

  endfor ; ifile loop (every 00 UTC file or every hourly file, etc.)

  ; get CMORPH
  cmorphs = !NULL
  cmorph_times = !NULL
  for xtime = min(xtimes), max(xtimes) do begin
    date = string(xtime,format='(C(CYI4.4,CMOI2.2,CDI2.2))')
    year = string(xtime,format='(C(CYI4.4))')
    cmorph_file = '/glade/p/rda/data/ds502.1/cmorph_v0.x/0.25deg_3hly/netcdf/'+year+'/CMORPH_V0.x_0.25deg-3HLY_'+date+'.nc'
    ; I don't know the difference - but there is a difference at 2nd to last time on 20150826.
    ;cmorph_file = '/glade/p/rda/data/ds502.0/cmorph_v0.x/netcdf/'+year+'/cmorph.3hr-025deg.'+date+'.nc'
    if file_test(cmorph_file) ne 1 then stop
    ncid = ncdf_open(cmorph_file)
    ncdf_varget, ncid, ncdf_varid(ncid,'cmorph_precip'), cmorph
    ncdf_attget, ncid, ncdf_varid(ncid,'cmorph_precip'), "units", units
    ncdf_varget, ncid, ncdf_varid(ncid,'time'), cmorph_time
    ncdf_varget, ncid, ncdf_varid(ncid,'lat'), cmorph_lat
    ncdf_varget, ncid, ncdf_varid(ncid,'lon'), cmorph_lon
    cmorph_time = julday(1, 1, 1970, 0, 0, cmorph_time)
    cmorph_times = [cmorph_times, cmorph_time]
    cmorph = mean(cmorph, dimension=1); take zonal average
    areaWeight = rebin(cos(transpose(cmorph_lat)*!DTOR), n_elements(cmorph_lat), n_elements(cmorph_time))
    factor = 1
    if string(units) eq "mm (3 hr)^-1" then factor = 8
    if string(units) eq "mm hr^-1" then factor = 24
    global_cmorph = total(areaWeight*cmorph,1)/total(areaWeight,1) * factor
    cmorphs = [cmorphs, global_cmorph]
    ncdf_close, ncid
  endfor
  ; special treatment for rain and precipw; we want rate of change.
  dt = xtimes - shift(xtimes,1)
  precipw = ffields[*,0]
  dprecipw = (ffields[*,0] - shift(ffields[*,where(fieldnames eq 'precipw',/null)],1))/dt
  rain = (ffields[*,1] - shift(ffields[*,1],where(fieldnames eq 'rain',/null)))/dt
  if model.name eq 'GFS' then rain = ffields[*,1]*4 ; not accumulated since model start time.
  rain[0] = !VALUES.F_NAN ; accumulated rain not defined at t=0
  dprecipw[0] = !VALUES.F_NAN ; delta-precipw not defined either at t=0
  lh = ffields[*,where(fieldnames eq 'lh',/null)]
  if lh[0] eq 0 then lh[0] = !VALUES.F_NAN

  skip=0; first 2 times are goofy .skip first time?
  x = xtimes[skip:*]
  precipw = plot(x, precipw[skip:*], xticklen=1, xsubticklen=0.05, title=title, color='green', thick=3, $
    ytitle='precipitable water (mm)', name='precipw', xtickformat='(C(CMoA,CDI))', xminor=0, xtickunits='days', $
    xtickinterval=1, xtitle=string(mean(x),format='(C(CYI))'))

  dprecipw_p = plot(x, dprecipw[skip:*], xticklen=1, xsubticklen=0.05, title=title, color='green', thick=3, $
    ytitle='mm day$^{-1}$', name='$\Delta$precipw', xtickformat='(C(CMoA,CDI))', xminor=0, xtickunits='days', $
    xtickinterval=1, xtitle=string(mean(x),format='(C(CYI))'))
  rain_p = plot(x, rain[skip:*], /overplot, color='blue', thick=4, name=model.name+' model rain rate')
  cmorph_p = plot(cmorph_times, cmorphs, /overplot, color='blue', linestyle='dotted', thick=4, name='CMORPH')
  lh_p = plot(x, lh[skip:*], /overplot, color='red', thick=1, name='latent heat flux')
  lh_rain = plot(x, (lh-rain)[skip:*], /overplot, color='black', thick=1, name='latent heat flux - rain')
  zero = plot(lh_p.xrange,[0,0],/overplot)
  residue = plot(x, (lh-rain-dprecipw)[skip:*], name='latent heat flux - rain!C - $\Delta$precipw',xticklen=1, xsubticklen=0.05, title=title, $
    ytitle='mm day$^{-1}$', xtickformat='(C(CMoA,CDI))', xminor=0, xtickunits='days', xtickinterval=1, $
    linestyle='dotted', /overplot, xtitle=dprecipw_p.xtitle )
  l = legend(target=[lh_p, rain_p, cmorph_p, lh_rain, dprecipw_p, residue], /relative, position=[0.95,0.62],font_size=9)
  ; if this is a yearly average, replace dates with generic "day0", "day1", etc.
  foreach p, [precipw, lh_p] do begin
    p.xrange = [min(xtimes), max(xtimes)]
    if strlen(date) eq 4 then begin
      ax = p.axes
      ax[0].tickname = "day"+strtrim(sindgen(max(xtimes)-min(xtimes)+1),2)
      p.window.save, file_dirname(files[0])+'/'+p.name+'.'+date+'.png', resolution=150
    endif
  endforeach
  lh_p.yrange = [-1, 4]

end

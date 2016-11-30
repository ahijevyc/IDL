pro stuff_about_mpas
  basedir = '/glade/scratch/'
  force_new = 1
  savfile = '/tmp/t.sav'
  if force_new ne 1 && file_test(savfile) then restore, savfile else begin
    tmpf = '/tmp/fort.66'
    
    spawn, 'cat '+basedir+'wrfrt/mpas/201408*/latlon_0*/gfdl*/fort.66>'+tmpf
    mpas = read_atcf(tmpf)
    spawn, 'cat '+basedir+'wrfrt/mpas_wp/201408*/latlon_0*/gfdl*/fort.66>'+tmpf
    mpas_wp = read_atcf(tmpf)
    spawn, 'cat '+basedir+'wrfrt/mpas_al/201408*/latlon_0*/gfdl*/fort.66>'+tmpf
    mpas_al = read_atcf(tmpf)
    
    spawn, 'cat '+basedir+'ahijevyc/GFS/20148*/gfdl*/fort.66>'+tmpf
    gfs = read_atcf(tmpf)
    save, mpas, mpas_wp, mpas_al, gfs, filename=savfile
  endelse
  
  
  mpas = mpas_wp
  
  dland_thresh = 0; set to -!D.VALUES_INFINITY for no land threshold
  dland = distance_to_land_km(mpas.lonew,mpas.latns)
  mpas_ocean = where(dland gt dland_thresh, /null)
  dland = distance_to_land_km(gfs.lonew,gfs.latns)
  gfs_ocean = where(dland gt dland_thresh, /null)
  
  mslp = {name:'mslp', units:'hPa', binsize:4, minval:950}
  vmax = {name:'vmax', units:'knots', binsize:3 ,minval:5}
  var = vmax
  ivar = where(strmatch(tag_names(mpas), var.name, /fold), /null)
  for fh=192,192 do begin
    y1 = histogram(mpas.(ivar)[where(mpas.tau le fh and mpas_ocean)], binsize=var.binsize, locations=xval, min=var.minval)
    mpas_p = plot(xval, y1, color='red', xtitle=var.name+' ('+var.units+')', ytitle='count', $
      name=string(fh,format='("0-",I0,"h ")')+'MPAS', /stairstep, overplot=mpas_p, title=var.name)
      
    y2 = histogram(gfs.(ivar)[where(gfs.tau le fh and gfs_ocean)], binsize=var.binsize, locations=xval, min=var.minval)
    gfs_p = plot(xval, y2, /stairstep, $
      name=string(fh,format='("0-",I0,"h ")')+'GFS', overplot=mpas_p)
    l = legend()
    if finite(dland_thresh) then begin
      t2 = text(0.9, 0.85, 'dist to land > '+string(dland_thresh,format='(I0," km")'), /relative, font_size=12, $
        alignment=1, target=mpas_p)
    endif
    ratio = plot(xval, float(y2)/y1, /stairstep, xrange=mpas_p.xrange, xtitle=mpas_p.xtitle, $
      title='ratio of '+gfs_p.name+' count to '+mpas_p.name+' count')
    unity = plot(ratio.xrange, [1,1], overplot=ratio)

  endfor

  
end
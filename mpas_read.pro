function mpas_read, file, field=field, ncid=ncid
  ; ncid is the optional keyword tha has the netcdf id of the currently open file.
  if n_elements(ncid) ne 0 && ncid eq 0 then stop
  if keyword_set(ncid) eq 0 then ncid = ncdf_open(file)
  atmos_const

  varids = ncdf_varidsinq(ncid)
  var_names = list()
  for i=0,n_elements(varids)-1 do var_names.add, (ncdf_varinq(ncid, varids[i])).name

  if keyword_set(field) and var_names.where(field) eq !NULL and field ne 'speed10' then begin
    ; speed10 can be derived later. don't panic yet.
    print, field, " not found in ",file
    return, !NULL
  endif

  if var_names.where('u10') ne !NULL and not strmatch(file, '*init.nc') then begin
    if keyword_set(field) then begin
      if strmatch(field,'speed10',/fold) then begin
        ncdf_get, file, ['u10','v10'], t, /struct, /quiet
        values = sqrt(t.u10.value^2.+t.v10.value^2.)
      endif else if strmatch(field,'mse2',/fold) then begin
        NCDF_VARGET, ncid, ncdf_varid(ncid,'t2'), tempK
        NCDF_VARGET, ncid, ncdf_varid(ncid,'q2'), q
        values = !ATMOS.Cpd*tempK + !CONST.Gn*2. + !ATMOS.LV0*q
        values = values/!ATMOS.Cpd ; divide by Cpd to get units of C
      endif else ncdf_varget, ncid, ncdf_varid(ncid,field), values
      if ~keyword_set(ncid) then ncdf_close, ncid
      return, values
    endif
    ncdf_get, file, ['u10','v10','xtime','t2','q2','rainc','rainnc','lh','precipw','relhum_500hPa',$
      'relhum_700hPa','relhum_850hPa'], t, /struct, gatt=gatt

  endif else if strmatch(file, '*init.nc') then begin
    ncdf_get, file, ['latCell','lonCell','xCell','yCell','zCell','indexToCellID','nEdgesOnCell',$
      'areaCell', 'cellsOnCell', 'ter', 'landmask', 'xtime','skintemp','precipw'], t, gatt=gatt
    t["latCell","value"] = t["latCell","value"] * !RADEG
    t["lonCell","value"] = t["lonCell","value"] * !RADEG

  endif else if strmatch(file, '*GFS*', /fold) then begin

    ; get speed_10m
    ; get 3rd dimension (vertical) of wind at specified height level above ground (m)
    dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'VGRD_P0_L103_GLL0'))).dim[2]
    ncdf_diminq,ncid,dimid,dimname,dummy
    ; get vertical dimension array
    ncdf_varget, ncid, dimname, lv
    ; look for derived speed variable already in file.
    if ncdf_varid(ncid, 'SGRD_P0_L103_GLL0') ne -1 then begin
      NCDF_VARGET, ncid, ncdf_varid(ncid,'SGRD_P0_L103_GLL0'), data
      ; make sure 3rd dimension is vertical dimension
      if (ncdf_varinq(ncid,ncdf_varid(ncid,'SGRD_P0_L103_GLL0'))).dim[2] ne dimid then stop
      speed10 = data[*,*,where(lv eq 10, /null)]
    endif else begin
      ; if derived speed doesn't already exist, derive it.
      NCDF_VARGET, ncid, ncdf_varid(ncid,'UGRD_P0_L103_GLL0'), u
      NCDF_VARGET, ncid, ncdf_varid(ncid,'VGRD_P0_L103_GLL0'), v
      ; make sure 3rd dimension is lv
      if (ncdf_varinq(ncid,ncdf_varid(ncid,'VGRD_P0_L103_GLL0'))).dim[2] ne dimid then stop
      u10 = u[*,*,where(lv eq 10, /null)] ; first layer is 10m above ground
      v10 = v[*,*,where(lv eq 10, /null)]
      speed10 = sqrt(u10^2.+v10^2.)
    endelse

    f = HASH("speed10",speed10)

    ; get t2m, derive mse2
    if 0 then begin
      ncdf_varget,ncid,ncdf_varid(ncid,'TMP_P0_L103_GLL0'),tempK
      ; get 3rd dimension (vertical) of TMP
      dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'TMP_P0_L103_GLL0'))).dim[2]
      ncdf_diminq,ncid,dimid,dimname,dummy
      ncdf_varget,ncid,dimname,lv
      tempK = tempK[*,*,where(lv eq 2, /null)]
      f["t2"] = tempK
      ncdf_varget,ncid,ncdf_varid(ncid,'SPFH_P0_L103_GLL0'),shum
      ; get 3rd dimension (vertical) of SPFH
      dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'SPFH_P0_L103_GLL0'))).dim[2]
      ncdf_diminq,ncid,dimid,dimname,dummy
      ncdf_varget,ncid,dimname,lv
      ; specific humidity to mixing ratio
      q = shum/(1.-shum)
      q = q[*,*,where(lv eq 2, /null)]
      f["q2"] = q
      mse2 = !ATMOS.Cpd*tempK + !CONST.Gn*2. + !ATMOS.LV0*q
      mse2 = mse2/!ATMOS.Cpd ; divide by Cpd to get units of C
      f["mse2"] = mse2

      ncdf_varget,ncid,ncdf_varid(ncid,'PWAT_P0_L200_GLL0'), precipw
      f["precipw"] = precipw
      ncdf_varget,ncid,ncdf_varid(ncid,'TMP_P0_L100_GLL0'), temperature
      ncdf_varget,ncid,ncdf_varid(ncid,'HGT_P0_L100_GLL0'), height
      ncdf_varget,ncid,ncdf_varid(ncid,'RH_P0_L100_GLL0'), relhum
      foreach fn, ['relhum_850hPa', 'relhum_700hPa', 'relhum_500hPa'] do begin
        ; get 3rd dimension (vertical)
        dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'RH_P0_L100_GLL0'))).dim[2]
        ncdf_diminq,ncid,dimid,dimname,dummy
        ncdf_varget,ncid,dimname,lv
        lvl = strmid(fn, 5, 3, /reverse) * 100.
        ; insert single element into hash
        f[fn] = relhum[*,*,where(lv eq lvl, /null)]
      endforeach

      foreach lvl, [85000, 70000, 50000] do begin
        ; get 3rd dimension (vertical)
        dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'TMP_P0_L100_GLL0'))).dim[2]
        ncdf_diminq,ncid,dimid,dimname,dummy
        ncdf_varget,ncid,dimname,lv
        key = 'temperature_'+string(lvl/100,format='(I3.3)')
        f[key] = temperature[*,*,where(lv eq lvl, /null)]
        ; get 3rd dimension (vertical)
        dimid = (ncdf_varinq(ncid,ncdf_varid(ncid,'HGT_P0_L100_GLL0'))).dim[2]
        ncdf_diminq,ncid,dimid,dimname,dummy
        ncdf_varget,ncid,dimname,lv
        key = 'height_'+string(lvl/100,format='(I3.3)')
        f[key] = height[*,*,where(lv eq lvl, /null)]
      endforeach

      ; Kind of a kludge - GFS forecasts are a mixture of 3-h and 6-h averages/accumulations
      ; depending on whether the forecast hour is a multiple of 6 or not.
      ; Instead of figuring out how to deal with 3-h averages/accumulations, just get 6-h accum.
      ; Another difference is this is accumulated precipitation since 6 hours ago, unlike MPAS
      ; rain, which is accumulation since model initializatoin time.  Must deal with this later,
      ; outside this function.
      rain6h = !VALUES.F_NAN
      if ncdf_varid(ncid, 'APCP_P8_L1_GLL0_acc6h') ne -1 then ncdf_varget,ncid,ncdf_varid(ncid,'APCP_P8_L1_GLL0_acc6h'),rain6h
      f['rain'] = rain6h
      rainc6h = !VALUES.F_NAN
      if ncdf_varid(ncid, 'ACPCP_P8_L1_GLL0_acc6h') ne -1 then ncdf_varget,ncid,ncdf_varid(ncid,'APCP_P8_L1_GLL0_acc6h'),rainc6h
      f['rainc'] = rainc6h
      lh6h = !VALUES.F_NAN
      if ncdf_varid(ncid, 'LHTFL_P8_L1_GLL0_avg6h') ne -1 then ncdf_varget,ncid,ncdf_varid(ncid,'LHTFL_P8_L1_GLL0_avg6h'),lh6h
      f['lh'] = lh6h

    endif

    ncdf_varget, ncid, ncdf_varid(ncid,'PRMSL_P0_L101_GLL0'), mslp
    ; at one time, perhaps before the rerun of 2014, mpas produced mslp in hPa
    ; but now it is in Pa, same as GFS grib files. Everything downstream, especially print_atcf
    ; assumes it is getting mslp in Pa now.
    f["mslp"] = mslp


    if keyword_set(ncid) eq 0 then ncdf_close, ncid
    return, keyword_set(field) ? f[field] : f

  endif else begin
    print, "file",file," does not meet expectations"
    if keyword_set(field) then print, "asked for ", field
    print, "found", var_names
    stop
  endelse

  if keyword_set(ncid) eq 0 then NCDF_CLOSE, ncid

  ; Extract parent_id attribute
  parent_id = strsplit(gatt["parent_id"], string(10b), /extract) ; split at newlines \n or string(10b)
  parent_id = parent_id[-1]
  t['parent_id'] = parent_id

  if keyword_set(field) then return, t[field]

  return, t

end

pro mpas_plot
  t = mpas_read('/glade/scratch/ahijevyc/mpas2/2013090100/init.nc')
  fieldname ='skintemp'
  map = map('Geographic', limit=[-90,0,90,360])
  ct = colortable(72, /reverse)
  z = t.skintemp - 273.15
  x = t.loncell
  y = t.latcell
  c = contour(z, x, y, overplot=map, rgb_table=ct, /fill)
  m1 = mapcontinents()
  cb = colorbar()


end

pro draw_domain, ifield, lonCell, latCell, color=color
  if ~keyword_set(color) then color=0
  oldp = !P
  oldx = !X
  oldy = !Y
  dx = !X.WINDOW[1]-!X.WINDOW[0]
  dy = !Y.WINDOW[1]-!Y.WINDOW[0]
  map_set, /noerase, position=[!X.WINDOW[0]+0.2*dx,!Y.WINDOW[0]+0.2*dy,!X.WINDOW[0]+0.5*dx,!Y.WINDOW[0]+0.4*dy], /iso, /cont
  i = where(finite(ifield),n)
  if n eq 0 then stop
  oplot, lonCell[i], latCell[i], color=color, psym=3, nsum=2
  !P = oldp
  !X = oldx
  !Y = oldY
end



pro run_mpas_read, date=date, mpas_name=mpas_name, debug = debug
  if ~keyword_set(debug) then debug = 0
  if ~keyword_set(mpas_name) then mpas_name = 'mpas2' ; 'mpas' or 'mpas2'
  if ~keyword_set(date) then date = '20130929'
  field = 'lh' & units = 'mm'
  index = round((date - 20130814L)/2) + 1
  basedir = '/glade/scratch/ahijevyc/'+mpas+'/'+date+'00/'
  loadct, 39, rgb_table=rgb_table
  loadct, 39
  if !D.NAME eq 'X' then device, decomposed=0
  if !D.NAME eq 'PS' then device, /close, /color, bits=8

  mpas = mpas_mesh(mpas_name)
  nCells = n_elements(mpas.areaCell)
  latbound = 90
  title = mpas.name + " " + date + " " + string(177b) + string(latbound, format='(F4.1)') + '!Z(00B0)'
  imask = where(abs(mpas.latCell) ge latbound,nmask)
  iyear = strmid(date,0,4)
  imonth = strmid(date,4,2)
  iday  = strmid(date,6,2)

  savfile = '/glade/scratch/ahijevyc/savfile/'+date+'.'+field+'.sav'
  if file_test(savfile) then restore, savfile else begin

    files = file_search(basedir+'diagnostics.*_'+(debug?'00':'??')+'.00.00.nc', count=nfiles)
    xtimes = replicate(!VALUES.D_NAN, nfiles)
    ffields = replicate(!VALUES.F_NAN, nfiles)
    for ifile = 1, nfiles-1 do begin ; perhaps skip the first one because is all zero.
      file = files[ifile]
      fhour = round((mpas_diagnostics_jday(file) - julday(imonth,iday,iyear,0,0,0))*24.d)
      LST = fhour + lonCell/15d
      xtimes[ifile] = mpas_diagnostics_jday(file)
      f = mpas_read(file)
      ffield = f[field] ; assumes hash-type
      if nmask gt 0 then ffield[imask] = !VALUES.F_NAN
      ; special treatment for rain, because it is accumulated.
      if strmatch(field, 'rain*') or field eq 'precipw' then begin
        if n_elements(ffield_accum_old) eq 0 then begin
          ffield_accum_old = ffield
          dt = 0
        endif else begin
          ffield_accum = ffield
          ffield = ffield - ffield_accum_old
          ffield_accum_old = ffield_accum
          dt = fhour - fhour_old
          ; normalize by dividing by the delta-time since the old accumulation
          ffield = ffield/dt * 24. ; mm/day
        endelse
        fhour_old = fhour
        ; special case. normally omax is chosen automatically using field values in first time, but accumulated rain is all 0's in first time.
        omax = 25.
        units = 'mm/day'
      endif
      if (0) then begin ; plot histograms
        if strmatch(field, 'relhum*') then omax=1.1
        color = reform(rgb_table[round(fhour*254./240.),*])
        h = histogram(ffield, nbins=nbins, min=omin, max=omax, omin=omin, omax=omax, locations=xlocs, /nan, reverse_indices=ri)
        ; counts are normalized by area of each cell
        for j = 0L,n_elements(h)-1 do if ri[j+1] gt ri[j] then h[j] = h[j] * mean(mpas.areaCell[ri[ri[j]:ri[j+1]-1]])/typical_uniform_area
        p = plot(xlocs, h, /histogram, thick=3, overplot=ifile, xticklen=1, xsubticklen=0.05, ylog=1, min_value=1,title=title,xtitle=field, yrange=[1e4,1e6], $
          color=color, name=string(fhour, format='(I0,"h")'), current=1, layout=[4,1,index], dimensions=[1340,400])
        if ifile eq 0 then ps = [p] else ps = [ps,p]
        ; for sanity, plot locations of finite points that were used. You may have applied a mask you didn't remember.
        if ifile eq 0 and latbound lt 90 and debug then draw_domain, ffield, mpas.lonCell, mpas.latCell, color=color
      endif
      global_ffield = total(mpas.areaCell*ffield,/nan)/total(mpas.areaCell,/nan)
      if field eq 'lh' then begin
        global_ffield = global_ffield * 0.0345 ; convert latent heat flux from W/m^2 to mm/day
        units = 'mm/day'
      endif

      print, file, global_ffield
      ffields[ifile] = global_ffield

      h = histogram((LST+0.5) mod 24, min=0., binsize=1, locations=xlocs, /nan, reverse_indices=ri) ; the half hour is to fix the way histogram handles things.
      ;    print, min(xlocs), max(xlocs), min(LST), max(LST)
      for iLST = 0, n_elements(h)-1 do begin
        j = iLST ; 0 LST
        if ri[j+1] gt ri[j] then begin
          i = [ri[ri[j]:ri[j+1]-1]]
          if n_elements(fsum)   eq 0 then fsum   = replicate(0d, ncells,n_elements(h))
          fsum[i,iLST] = fsum[i,iLST] + ffield[i]
          if n_elements(fcount) eq 0 then fcount = replicate(0d, ncells,n_elements(h))
          fcount[i,iLST] = fcount[i,iLST] + 1
          z = fsum[i,iLST]/fcount[i,iLST]
          x = mpas.lonCell[i]
          y = mpas.latCell[i]
          ; Maybe plot field on world map
          ;        map_set, /cont, /grid,/label, ;title=string(iLST,format='(I2.2," LST   ")')+file
          ;        contour, z, x, y, /irr, /foll, /xstyle, /ystyle, /overplot, max=25
          ;        plots, x, y, color=(z+50.)/900*255.;, psym=3
        endif
      endfor

    endfor ; ifile loop (every 00 UTC file or every hourly file, etc.)
    save, fsum, fcount, xtimes, xlocs, ffields, units, filename=savfile
  endelse

  for iLST = 0,23,3 do begin
    z = fsum[*,iLST]/fcount[*,iLST]
    lonmin=-180
    lonmax=180
    latmin=-90
    latmax=90
    map_set, /cont, /grid,/label, title=string(iLST,format='(I2.2," LST")'), /iso, limit=[latmin, lonmin, latmax, lonmax], /noborder
    dlon = .15
    junk = 1800/dlon^2.
    zin = z[0:junk]
    triangulate, mpas.lonCell[0:junk], mpas.latCell[0:junk], tr, sphere=s, fvalue=zin, /degrees
    r=trigrid(zin, sphere=s, [dlon,dlon], [lonmin,latmin,lonmax,latmax], /degrees)
    if !D.NAME eq 'PS' then tv, (r+90)/800.*255., !X.WINDOW[0], !Y.WINDOW[0], /norm, xsize=!X.WINDOW[1]-!X.WINDOW[0], ysize=!Y.WINDOW[1]-!Y.WINDOW[0] else $
      tv, map_patch((r+90)/800.*255., xstart=x0, ystart=y0), x0, y0
    map_continents, /coasts, color=!P.BACKGROUND
    map_grid, /box
    ;    plots, lonCell[0:nCells*0.3], latCell[0:nCells*0.3], color=(z+50.)/900*255., psym=3
  endfor
  ; write legend
  if n_elements(p) gt 0 then l = legend(target = ps, auto_text_color=1)

  p = plot(xtimes[2:*], ffields[2:*], xticklen=1, xsubticklen=0.05, title=title, /overplot, $
    ytitle=field+" "+units, name=field, xtickformat='(C(CDI," ",CMoA))', xminor=0, xtickunits='days', xtickinterval=1)

  if !D.NAME eq 'PS' then device, /close
end


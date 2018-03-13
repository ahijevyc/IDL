function decompress, ncid, varname, stride=stride
  if n_elements(stride) eq 0 then stride = 1
  varid = ncdf_varid(ncid,varname)
  if varid eq -1 then stop
  ncdf_varget, ncid, varid, var, stride=replicate(stride,(ncdf_varinq(ncid, varid)).ndims)
  flipbyte = typename(var) eq 'BYTE'
  ;  print, min(var), max(var), format = '("min(var):", F12.7, " max(var):", F12.7)'
  scale_factor = 1d
  add_offset = 0d
  result = ncdf_varinq(ncid, varid)
  varatts = list()
  for iatt=0,result.natts-1 do varatts.add, ncdf_attname(ncid, varid, iatt)
  if varatts.Where('_FillValue') ne !NULL then ncdf_attget, ncid, varid, '_FillValue', FillValue
  if varatts.Where('scale_factor') ne !NULL then ncdf_attget, ncid, varid, 'scale_factor', scale_factor
  if varatts.Where('add_offset') ne !NULL then ncdf_attget, ncid, varid, 'add_offset', add_offset
  ibad = where(var eq FillValue, nbad)
  if flipbyte then begin
    var = fix(var)
    ;  print, min(var), max(var), format = '("min(var):", F12.7, " max(var):", F12.7)'
    ; The IDL byte has values 0-255.  The netCDF byte has values from -128 to 127. So if var >= 128 then var = var-256.
    iflip = where(var ge 128B, nflip)
    if nflip gt 0 then var[iflip] = var[iflip] - 256
  endif
  var = var* scale_factor + add_offset
  ; Change fill values (defined before flipbyte clause) to NaN
  if nbad gt 0 then var[ibad] = !VALUES.D_NAN
  ;  print, add_offset, scale_factor, max(var), format = '("add_offset:", F12.7, " scale_factor:", F12.7, " max:", F12.5)'

  return, var
end

function get_triangle, time
  date = strmid(time,0,8)
  case date of
    '20150606': t = [[40.52,-98.95],[38.14,-97.42],[38.59,-100.09]]
    '20150626': t = [[40.52,-98.95],[38.94,-99.56],[39.36,-101.37]]
    '20150704': t = [[38.94,-99.56],[37.61,-99.28],[38.45,-101.75]]
    '20150705': t = [[39.36,-101.37],[38.94,-99.56],[37.61,-99.28]]
    '20150710': t = [[37.61,-99.28],[38.14,-97.43],[36.61,-97.49]]
    '20150716': t = [[40.52,-98.95],[39.36,-101.37],[38.58,-99.03]]
    '20150715': t = [[38.48,-100.90],[40.20,-100.65],[39.79,-98.79]]
    '20150605': t = [[40.52,-98.51],[37.61,-99.28]]
    else: t=!NULL
  endcase
  return, t
end

function RFmap, p
  ; from cospa7:$STATS_HOME/archive/params/FcstPredictorData.HRRR on Jun 2 2015.
  return, plot([ -124.4, -124.4, -67,-67,-124.4], [25, 48, 48, 25, 25], overplot=p, thick=4)
end

pro plot_mdv_nc, file=file

  ;pwin = GetWindows(/current)
  ;if pwin ne !NULL then pwin.erase

  ; Used Mdv2Netcdf on cospa5 to create input.
  ; script called ~/bin/Mdv2IDL.csh
  ; MCS-I field (Truth)
  MCS_I_varname = 'LCS_CI_grid45' ; 'LCS_CI_grid55' in April 2015
  MCS_persist_varname = 'LCS_persist45' ;

  if !D.NAME eq 'PS' then device, /close, /color, ysize=5, yoffset=5, /inches
  basedir = '/pecan/ahijevyc/faa/MCS-I/'
  ; another Subdomain  - for stats/plotting/everything
  limit = [26,-103,47.,-71];
  limit = [25,-124.4,48,-67]; used for Fig. 1
  limit = [34,-108,43.7,-89.5] ; 20130614 Fig. 11
  ;limit = [27.5,-105,41.1,-79.] ; 20130805 Fig. 10

  loadct, 41, file="/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl" ; radar reflectivity
  tvlct, ct, /get
  ct[0,*] = [255,255,255]
  ; switch to greyscale colortable
  ;ct = colortable(0,/reverse)

  date_hours = !NULL
  foreach date, ['0614','0627','0714','0802','0803','0805'] do begin
    for hour=4,23 do begin
      date_hours = [date_hours,'2013'+date+'_'+string(hour, format="(I02.2)")]
    endfor
  endforeach

  date_hours = ['20130614_21']

  foreach hour, date_hours do begin
    ofiles = ''
    rasters = list()
    rasters.add, {Name: 'ncf_1', File : basedir+'RF_'+hour+'.nc', mask:0, c_value:findgen(11)/10,fill:1}
    ;rasters.add, {Name: 'LCS_CI_grid55', File: basedir+'HRRR_MCS-I.f04_'+hour+'.nc',fill:1}
    ;rasters.add, {Name: 'VIL', File: basedir+'VIL_'+hour+'.nc', mask:0,c_value:[0,0.01,0.02,0.03,0.05,1,2.,3.5,5,10,20,50],fill:1}
    ; rasters.add, {Name: 'mosaicked_refl', File: basedir+'brightBand_'+hour+'.nc', mask:0, c_value:[0,5],fill:1}
    ;rasters.add, {Name: 'REFC_EATM', File : basedir+'hrrr.f04_'+hour+'.nc', mask:0, c_value:findgen(15)*5, fill:1} ; f05 in Fig10; f04 in Fig11
    ;rasters.add, {Name: 'mosaicked_reflA', File: basedir+'smooth_brightBand_'+hour+'.nc', mask:0, c_value:[-99,-5], fill:0} ; panel d overlay
    MCS_File = basedir+'mask_'+hour+'.nc'

    ; This is the TRUTH file. Observed MCS-I.
    file = basedir+'MCS-I_'+hour+'.nc'
    if ~File_test(file) then message, 'file '+file+' not found'
    ncid = NCDF_OPEN(file)            ; Open The NetCDF file
    NCDF_VARGET, ncid,  NCDF_VARID(ncid,'time'), time      ; Read in variable 'time'
    NCDF_ATTGET, ncid, 'time', 'comment', time_comment2
    time_comment2 = string(time_comment2)
    NCDF_VARGET, ncid,  NCDF_VARID(ncid,'x0'), truthx0      ; Read in variable 'x0'
    NCDF_VARGET, ncid,  NCDF_VARID(ncid,'y0'), truthy0      ; Read in variable 'y0'
    truthvar = decompress(ncid, MCS_I_varname)
    MCS_persist = decompress(ncid, MCS_persist_varname)
    NCDF_CLOSE, ncid      ; Close the NetCDF file
    if max(truthvar) eq 0 then begin
      print, hour + " no MCS-I events"
      ;continue
    endif



    foreach raster,rasters do begin
      RasterName = raster.Name
      RasterFile = raster.File
      ; Predictor Field
      ncid = ncdf_open(rasterFile)
      if ncid eq -1 then stop
      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'time'), time      ; Read in variable 'time'
      ; Process attributes of variable 'time'
      result = ncdf_varinq(ncid,ncdf_varid(ncid,'time'))
      for ni=0,result.Natts-1 do begin
        name = ncdf_attname(ncid, ncdf_varid(ncid,'time'), ni)
        if name eq 'string' then begin
          NCDF_ATTGET, ncid, 'time', 'string', time_comment
          time_comment = string(time_comment)
        endif
        if name eq 'comment' then begin
          NCDF_ATTGET, ncid, 'time', 'comment', time_comment
          time_comment = string(time_comment)
        endif
      endfor
      if ncdf_varid(ncid,'forecast_period') ne -1 then begin
        ncdf_varget, ncid, ncdf_varid(ncid,'forecast_period'), forecast_period
        time_comment = string(forecast_period/3600., format='(I0,"h forecast!C")')+time_comment
      endif
      rasterArray = decompress(ncid, RasterName)
      ncdf_attget, ncid, RasterName, 'units', RasterUnits & RasterUnits = string(RasterUnits)
      if RasterName eq 'ncf_1' then begin
        RasterName = 'likelihood'
        RasterUnits = '0-1'
        rasterArray = rasterArray/200.
      endif

      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'x0'), lon0      ; Read in variable 'x0'
      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'y0'), lat0      ; Read in variable 'y0'
      grid_mapping_0 = ncdf_varid(ncid,'grid_mapping_0') ne -1L ? 'grid_mapping_0':'grid_mapping0'
      NCDF_ATTGET, ncid,  grid_mapping_0, 'grid_mapping_name', projection
      projection = string(projection)
      if projection eq "lambert_conformal_conic" then begin
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'lat0'), lat0
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'lon0'), lon0
        NCDF_ATTGET, ncid,  grid_mapping_0, 'longitude_of_central_meridian', center_longitude
        NCDF_ATTGET, ncid,  grid_mapping_0, 'latitude_of_projection_origin', center_latitude
        NCDF_ATTGET, ncid,  grid_mapping_0, 'standard_parallel', STANDARD_PARALLEL
        NCDF_ATTGET, ncid,  grid_mapping_0, 'false_easting', false_easting
        NCDF_ATTGET, ncid,  grid_mapping_0, 'false_northing', false_northing
        triangulate, lon0, lat0, tr
        xout=-120+findgen(480)/8
        yout=20+findgen(240)/8
        rasterArray = griddata(lon0,lat0,rasterArray,/grid,xout=xout,yout=yout,/deg,triangle=tr,method='NearestNeighbor')
        lon0=xout
        lat0=yout
      endif
      if projection eq "lambert_azimuthal_equal_area" then begin
        NCDF_ATTGET, ncid,  grid_mapping_0, 'longitude_of_projection_origin', center_longitude
        NCDF_ATTGET, ncid,  grid_mapping_0, 'latitude_of_projection_origin', center_latitude
        NCDF_ATTGET, ncid,  grid_mapping_0, 'false_easting', false_easting
        NCDF_ATTGET, ncid,  grid_mapping_0, 'false_northing', false_northing
        NCDF_ATTGET, ncid,  grid_mapping_0, 'earth_radius', earth_radius
        x = lon0/earth_radius &  y = lat0/earth_radius
        ; from http://mathworld.wolfram.com/LambertAzimuthalEqual-AreaProjection.html on jun 10 2015
        rho = sqrt(x^2.+y^2.)
        c = 2*asin(rho/2.)
        phi1 = !DTOR*center_latitude
        lat0 = asin(cos(c)*sin(phi1)+y*sin(c)*cos(phi1)/rho)
        lon0 = !DTOR*center_longitude + atan(x*sin(c)/(rho*cos(phi1)*cos(c)-y*sin(phi1)*sin(c)))
        lon0 = !RADEG*lon0
        lat0 = !RADEG*lat0
        ; for some reason tr is a [3,3518] array. seems too small. when plugged into griddata
        ; rasterArray becomes all NaNs.
        print, 'cannot handle '+projection+' projection'
        stop
        triangulate, lon0, lat0, tr
        xout=-120+findgen(480)/8
        yout=20+findgen(240)/8
        rasterArray = griddata(lon0,lat0,rasterArray,/grid,xout=xout,yout=yout,/deg,triangle=tr,method='NearestNeighbor')
        lon0=xout
        lat0=yout

      endif

      NCDF_CLOSE, ncid      ; Close the NetCDF file

      ; Mask rasterArray (unless it is VIL).
      if raster.mask eq 1 then begin
        ; mask stuff
        ; mask #1 persistent MCSs
        ncid = ncdf_open(MCS_File)
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'x0'), x0      ; Read in variable 'x0'
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'y0'), y0      ; Read in variable 'y0'
        mask = decompress(ncid, 'LCS_persist80')
        NCDF_CLOSE, ncid      ; Close the NetCDF file
        x2d = x0#replicate(1,n_elements(y0))
        y2d = y0##replicate(1,n_elements(x0))
        triangulate, x2d, y2d, tr
        ; Interpolate mask field to same lat-lon grid as rasterArray.
        mask = griddata(x2d,y2d,mask[*],/grid,xout=lon0,yout=lat0,/deg,triangle=tr,method='NearestNeighbor')
        imask = where(mask ne 0, nmask) ; use non-zeros as mask (1's and 2's from MCS-I field).
        if nmask gt 0 then rasterArray[imask] = !VALUES.D_NAN

      endif

      ; Mask #2 (static radar coverage)
      static_radar_coverage_file = basedir+'static_radar_mask.nc'
      if file_test(static_radar_coverage_file) then begin
        ncid = ncdf_open(static_radar_coverage_file)
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'x0'), x0      ; Read in variable 'x0'
        NCDF_VARGET, ncid,  NCDF_VARID(ncid,'y0'), y0      ; Read in variable 'y0'
        mask = decompress(ncid, 'VIL')
        NCDF_CLOSE, ncid      ; Close the NetCDF file
        x2d = x0#replicate(1,n_elements(y0))
        y2d = y0##replicate(1,n_elements(x0))
        imask = where(x2d lt limit[1] or x2d gt limit[3] or y2d lt limit[0] or y2d gt limit[2],nmask)
        if nmask gt 0 then mask[imask] = !VALUES.D_NAN
        triangulate, x2d, y2d, tr
        mask = griddata(x2d,y2d,mask[*],/grid,xout=lon0,yout=lat0,/deg,triangle=tr,method='NearestNeighbor')
        imask = where(finite(mask) eq 0, nmask)
        if nmask gt 0 then rasterArray[imask] = !VALUES.D_NAN
      endif else begin
        print, 'no static radar mask found'
        stop
      endelse

      margin = [0.065,0.22,0.2,0.17]
      margin = [0.065,0.22,0.17,0.27] ; for Fig. 1
      title=RasterName+' '+time_comment + '        MCS '+time_comment2
      julday = julday(1,1,1970,0,0,0)+time/24/3600.
      paper_figure_title_string = string(julday, format='(c(CDI0,x,CMoA,x,CYI4.4,x,CHI2.2,":",CMI2.2," UTC"))')
      title = paper_figure_title_string

      vi = contour(rasterArray, lon0, lat0, rgb_table=ct, grid_units=2, margin=margin, transparency=0,$
        title=title, font_size=11, rgb_indices=bytscl(indgen(n_elements(raster.c_value)), top=220), $
        map_projection='Cylindrical Equal Area',buffer=n_elements(date_hours)gt 2, fill=raster.fill, c_value=raster.c_value, $
        overplot=raster.fill eq 0)
      vi.title.font_size=14
      proj = vi.MAPPROJECTION
      proj.limit = limit
      grid = vi.mapgrid
      grid.linestyle='dot'
      grid.label_position=0
      grid.clip=0
      grid.GRID_LATITUDE=grid.grid_longitude
      m = mapcontinents(/usa,thick=0.5)
      if raster.fill then cb = colorbar(target=vi,position = [0.15,0.07,0.7,0.1], title=rasterName+' ['+rasterUnits+']',/border) else begin
        ;for Fig. 10 and Fig. 11 HRRR refl dBZ overlay (lower right panel)
        vi.c_thick=3
        vi.c_color=['black','gold']
        vi.name = '-5 dBZ!Cextrapolated!Creflectivity'
        c0_box = polygon((vi.position)[2]+[0.02,0.06,0.06,0.02],(vi.position)[3]-[.1,.1,.15,.15],thick=vi.c_thick,color=(vi.c_color)[*,1])
        c0_label = text((c0_box.position)[0],(c0_box.position)[1]-0.01, vertical_alignment=1, vi.name)
      endelse

      ;  Plot MCS-I observations (truth) on RF votes
      if raster.name eq "ncf_1" then begin
        c0 = contour(MCS_persist,truthx0,truthy0,c_thick=2,grid_units=2,color='black',c_value=[1],$
          name='ongoing MCS',overplot=vi,c_label_show=0)
        c0_box = polygon((vi.position)[2]+[0.01,0.06,0.06,0.01],(vi.position)[3]-[0,0,0.05,0.05],thick=c0.c_thick,color=c0.color)
        c0_label = text((c0_box.position)[0],(c0_box.position)[1]-0.01, vertical_alignment=1, c0.name)
        if max(truthvar) gt 0 then begin
          c1 = contour(truthvar,truthx0,truthy0,c_thick=5,grid_units=2,color='magenta',c_value=[1],$
            name='MCS-I',overplot=vi,c_label_show=0)
          c1_box = polygon((vi.position)[2]+[0.01,0.06,0.06,0.01],(vi.position)[3]-[0.1,0.1,0.15,0.15],thick=c1.c_thick,color=c1.color)
          c1_label = text((c1_box.position)[0],(c1_box.position)[1]-0.01, vertical_alignment=1, c1.name)
        endif
      endif

      ; Mask out area with NO RADAR coverage.
      if limit[0] lt 30 then begin
        no_radar_color =  'dark slate gray'
        no_radar = contour(not finite(mask), lon0, lat0, grid_units=2,name='no radar', /fill, color=no_radar_color, $
          overplot=vi, margin=margin, c_label_show=0)
        no_radar_box = polygon((cb.position)[2]+[0.03,0.08,0.08,0.03],[(cb.position)[1],(cb.position)[1],(cb.position)[3],(cb.position)[3]],fill_background=1,fill_color=no_radar_color)
        no_radar_label = text((no_radar_box.position)[0],(no_radar_box.position)[1]-0.002, vertical_alignment=1, 'no radar!Ccoverage')
      endif


      ;l = legend(target=[c0,c1,no_radar], position=m.mapforward(-54,39),/data)
      ; b = rfmap(c1) ; Draw Border of RF domain.

      if rasterName ne 'VIL' && 0 then begin
        x2d = truthx0#replicate(1,n_elements(truthy0))
        y2d = truthy0##replicate(1,n_elements(truthx0))
        triangulate, x2d, y2d, tr
        var = griddata(x2d,y2d,truthvar[*],/grid,xout=lon0,yout=lat0,/deg,triangle=tr,method='NearestNeighbor')
        ;l = legend(target=c1)

        threshs = findgen(101)*(max(rasterArray,/nan)-min(rasterArray,/nan))/101+min(rasterArray,/nan) & nthresh = n_elements(threshs)
        hits = replicate(!VALUES.F_NAN,nthresh)
        miss = replicate(!VALUES.F_NAN,nthresh)
        fa   = replicate(!VALUES.F_NAN,nthresh)
        null = replicate(!VALUES.F_NAN,nthresh)
        foreach thresh,threshs,ithresh do begin
          hits[ithresh] = total(rasterArray ge thresh and var gt 0)
          miss[ithresh] = total(rasterArray lt thresh and var gt 0)
          fa[ithresh]   = total(rasterArray ge thresh and var eq 0)
          null[ithresh] = total(rasterArray lt thresh and var eq 0)
        endforeach
        rand = (hits+fa)*(hits+miss)/(hits+fa+miss+null)
        ets = (hits-rand)/(hits+miss+fa-rand)
        maxets = max(ets,imaxets)
        csi = hits/(hits+miss+fa)
        p = plot(threshs, csi, '-b',name='CSI',layout=[5,4,20],/current,font_size=7,margin=[0.1,0.1,0.06,0.05])
        pets = plot(threshs, ets, '-r', overplot=p,name='ETS')
        l = legend(target=[p,pets], vertical_spacing=0,vertical_alignment='center',/relative,sample_width=0.1,position=[1,0.6],shadow=0)
        l.font_size = p.font_size

        vi.title.string = vi.title.string + "!CETS = "+string(maxets,rasterName,threshs[imaxets],format='(F4.2," at ",A,"=",F8.2)')
        hmfn = (rasterArray ge threshs[imaxets] and var gt 0 ) * 1 + $
          (rasterArray lt threshs[imaxets] and var gt 0 ) * 2 + $
          (rasterArray ge threshs[imaxets] and var eq 0 ) * 3 + $
          (rasterArray lt threshs[imaxets] and var eq 0 ) * 4
        ct0 = congrid([[255,255,255],[0,127,0],[0,0,255],[255,0,0],[255,255,255]],3,256)
        ihmfn = image(byte(hmfn), lon0, lat0, grid_units=2,overplot=vi,transparency=75,rgb_table=ct0,min_value=-0.5,max_value=4.5)
        a = colorbar(target=ihmfn,border=1,orientation=1)
        a.ticklen=0
        a.tickname=['','hit!C('+string(hits[imaxets],format='(I0)')+')','miss!C('+string(miss[imaxets],format='(I0)')+')','false!Calarm!C('+string(fa[imaxets],format='(I0)')+')','']
        c1.hide=0 ; hide CI contours?
      endif
      if rastername eq 'VIL' then cb.title='VIL (kg m$^{-2}$)' ; If VIL
      if rastername eq 'REFC_EATM' then cb.title='HRRR reflectivity (dBZ)' ; Fix incorrect "dB" units.
      ofile = rasterFile + rasterName+".png"
      vi.save, ofile, resolution=245
      print, "created "+ofile
      ofiles = ofiles+" "+ofile
    endforeach ; rasters
    cd, basedir
    ;When you use SPAWN, IDL creates a new process that inherit the IDL session environment variables. This means that what it could be happening is that "convert" is picking up IDL's version of zlib, which it seems as it doesn't work well with ImageMagick.
    ;The environment variable is  DYLD_LIBRARY_PATH, so the idea is to try to point that variable to the right zlib that it's use by "convert". Maybe you could try setting that variable before calling convert, and then spawning "convert". Then of course you will need to set the variable back to the original value so IDL will continue working correctly.
    cmd = 'setenv o $DYLD_LIBRARY_PATH ;setenv DYLD_LIBRARY_PATH /opt/local.lion.new/local; /opt/local/bin/montage -geometry 70% '+ofiles+' montage_'+hour+'.png; setenv DYLD_LIBRARY_PATH $o'
      print, cmd
    spawn, cmd, result
    print, result
  endforeach ; date_hours
  if !D.NAME eq 'PS' then device, /close

end

pro simple ; useful for Stans Nov 2015 paper on turbulence off northeast U.S. coast
  ; and PECAN 2015 (STEP)
  if !D.NAME eq 'PS' then device, /close, /color, ysize=5, yoffset=5, /inches ; for some reason it outputs Courier after Sep 2017.Looks okay in window but not ps file.
  ; even if I specify a font in the device call, like /helvetica
  basedir = '/sysdisk1/ahijevyc/trier/nsslMosaic/'
  ; found nssl mosaics in hpss:/RAPDMG/CWX/nsslMosaics/
  ; found instructions in ahijevyc@cospa7.rap.ucar.edu:for_stan/README

  limit = [34,-100,50.,-57]; northeast U.S.
  center_longitude = -70
  limit =  [35,-105,42.5,-94.]; PECAN 2015
  limit = [36,-105.5,42,-100]; Stan's Glen work
  stride=2
  loadct, 41, file="/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl" ; radar reflectivity
  tvlct, ct, /get
  ct[0,*] = [255,255,255]


  date_hours = ['20111114_2200','20111114_2300','20111115_0000','20111115_0100','20111115_0200','20111115_0300',$
    '20111115_0400','20111115_0500','20111115_0600','20111115_1200','20111115_1600','20111115_1800']

  ; pecan 2015
  date_hours = ['20150606_0300', '20150606_0330', '20150606_0530', '20150626_0300', '20150626_0430', '20150626_0600',$
    '20150704_0500', '20150704_0630', '20150704_1000', $
    '20150705_0300','20150705_0400','20150705_0600','20150705_0700','20150710_0430','20150710_0600','20150710_0730',$
    '20150716_0200','20150716_0430','20150716_0600','20150715_0300','20150715_0700']
  date_hours = ['20150606_030037','20150606_033038','20150606_053038','20150606_070038',$
    '20150626_000038','20150626_030039','20150626_043040','20150626_060039',$
    '20150704_030036','20150704_050036','20150704_063034','20150704_100033',$
    '20150705_030037','20150705_040039','20150705_060040','20150705_070039',$
    '20150710_040033','20150710_043039','20150710_060039','20150710_073037',$
    '20150716_020039','20150716_043038','20150716_060033',$
    '20150715_030034','20150715_070040']
  date_hours = ['20150605_000040','20150605_030036','20150605_060036']
  
  date_hours = ['20150603_224038','20150603_230037','20150603_232035','20150603_234039',$
    '20150604_000035','20150604_010038','20150604_012037','20150604_014039','20150604_020039']
  date_hours = ['20150604_040037']
  date_hours = ['20150604_180038','20150604_190038','20150604_200040', '20150604_210035', '20150604_220037', '20150604_230033', '20150605_000040']
  ; Stan and Jim's case/paper
  ; date_hours = ['20150704_030036','20150704_040037','20150704_050036','20150704_060034','20150704_070041'] 

  foreach hour, date_hours do begin
    ofiles = ''
    rasters = list()
    c_value = indgen(15)*5-5
    if strmatch(hour, '20??????_??????') then rasters.add, {Name: 'MREF', File: basedir + strmid(hour,0,8) + '/g_' + strmid(hour,9) +$
      '/ncfdataf_00000000.nc', mask:0, c_value:c_value,fill:1,proj:'Equirectangular'} $
    else rasters.add, {Name: 'mrefl_mosaic', File: basedir+'ncfdata'+hour+'00.nc', mask:0, c_value:c_value,fill:1,proj:'Lambert Conformal'}

    foreach raster,rasters do begin
      RasterName = raster.Name
      RasterFile = raster.File
      ; Predictor Field
      ncid = ncdf_open(rasterFile)
      if ncid eq -1 then stop
      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'time'), time      ; Read in variable 'time'
      ; Process attributes of variable 'time'
      result = ncdf_varinq(ncid,ncdf_varid(ncid,'time'))
      for ni=0,result.Natts-1 do begin
        name = ncdf_attname(ncid, ncdf_varid(ncid,'time'), ni)
        if name eq 'string' then begin
          NCDF_ATTGET, ncid, 'time', 'string', time_comment
          time_comment = string(time_comment)
        endif
        if name eq 'comment' then begin
          NCDF_ATTGET, ncid, 'time', 'comment', time_comment
          time_comment = string(time_comment)
        endif
      endfor
      rasterArray = decompress(ncid, RasterName, stride=stride)
      ncdf_attget, ncid, RasterName, 'units', RasterUnits & RasterUnits = string(RasterUnits)

      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'x0'), lon0, stride=stride    ; Read in variable 'x0'
      NCDF_VARGET, ncid,  NCDF_VARID(ncid,'y0'), lat0, stride=stride    ; Read in variable 'y0'
      grid_mapping_0 = ncdf_varid(ncid,'grid_mapping_0') ne -1L ? 'grid_mapping_0':'grid_mapping0'
      NCDF_ATTGET, ncid,  grid_mapping_0, 'grid_mapping_name', projection
      projection = string(projection)
      NCDF_CLOSE, ncid      ; Close the NetCDF file

      margin = [0.2,0.26,0,0.28] ; for northeast turbulence
      margin = [0.15,0.22,0.1,0.1]
      title=RasterName+' '+time_comment
      julday = julday(1,1,1970,0,0,0)+time/24/3600.
      paper_figure_title_string = string(julday, format='(c(CDI0,x,CMoA,x,CYI4.4,x,CHI2.2,":",CMI2.2,":",CSI2.2," UTC"))')
      title = paper_figure_title_string

      ; don't pop up a window for more than 2 times.
      vi = contour(rasterArray, lon0, lat0, rgb_table=ct, grid_units=2, margin=margin, transparency=0,$
        title=title, font_size=11, rgb_indices=bytscl(indgen(n_elements(raster.c_value)), top=220), $
        map_projection=raster.proj,buffer=n_elements(date_hours)gt 2, fill=raster.fill, c_value=raster.c_value, $
        overplot=raster.fill eq 0, aspect_ratio=1) ; aspect_ratio=1 ensures north-south scale is same as east-west scale
      proj = vi.MAPPROJECTION
      proj.limit = limit
      proj.standard_parallel=0.
      if strmid(hour,0,8) eq '20150710' then proj.limit = limit + [-1,0,-1,0]
      if raster.proj eq 'Lambert Conformal' then begin
        proj.center_longitude=center_longitude
        proj.standard_par1 = 30.
        proj.standard_par2 = 60.
      endif
      grid = vi.mapgrid
      grid.linestyle='dot'
      grid.thick = 0.25
      grid.label_position=0
      grid.clip=0
      grid.GRID_LATITUDE=grid.grid_longitude
      grid.box_axes=1
      grid.box_antialias = 1
      m = mapcontinents(/usa,thick=0.5)
      if raster.fill then cb = colorbar(target=vi,position = [0.5,0.07,0.9,0.1], title=rasterName+' ['+rasterUnits+']', $
        /border)
      p3 = get_triangle(hour)
      if 0 and p3 ne !NULL then begin
        if hour eq '20150605_030036' then pts = plot(p3[1,*],p3[0,*], overplot=m, /data, color='blue', symbol='*', $
          linestyle='none', sym_thick=2)
        if strmid(hour,0,8) ne '20150605' then tri = polygon(reverse(p3), target=m, /data, fill_background=0, color='blue', thick=4)
      endif

      ofile = rasterFile + rasterName+".ps"
      vi.save, ofile, resolution=245
      print, "created "+ofile
      ofiles = ofiles+" "+ofile
    endforeach ; rasters
  endforeach ; date_hours
  if !D.NAME eq 'PS' then device, /close



end


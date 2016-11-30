; Copied to yellowstone Feb 17, 2015 so I could access MPAS grids.

function adjust_ascat, aws
  ; from JGR, K.-H. Chou, C.-C. Wu, and S.-Z. Lin 2013
  ; Fig. 8a, page 9018.
  return, 0.014 * aws^2. + 0.821 * aws + 0.961
end

function barb_color, speed
  return, min([254,speed*12])
end

pro mymapset, limit, title, pos, _extra=e
  tvlct, oldct, /get
  loadct, 39, /silent
  lon0 = mean(limit[1:*:2])
  if limit[1] eq 0 && limit[3] eq 360 then lon0=-135 ; if global shift so -135E is in the center.
  map_set, mean(limit[0:*:2]), lon0, 0, /cylind, limit=limit, /iso, title=title, position=pos, /continents, /noborder, _extra=e
  map_grid
  tvlct, oldct
end

pro plot_time, time, lon, lat, limit, title, pos, _extra=e
  skip = 10
  mymapset, limit, title, pos, _extra=e
  plots, lon, lat, color=bytscl(time,top=!D.N_colors-1), /data, psym=3
end
pro plot_windbarbs, ws, dir, lon, lat, limit, title, pos, _extra=e
  skip = 12 ; increment
  mymapset, limit, title, pos, _extra=e
  ; Make a vector of 16 points, A[i] = 2pi/16:
  A = FINDGEN(17) * (!PI*2/16.)
  ; Define the symbol to be a unit circle with 16 points,
  ; and set the filled flag:
  USERSYM, COS(A), SIN(A)

  for i = 0, n_elements(ws)-1, skip do if finite(ws[i]) and finite(dir[i]) then begin
    wind_barb, ws[i], dir[i], lon[i], lat[i], /nocircle, noclip=0, /data, size=e.bad[i] eq 0 ? 0.21*skip^.4 : 0.15*skip^.4, color=e.bad[i] eq 0 ? barb_color(ws[i]) : 0, thick=skip
    if total(strcmp(tag_names(e), 'CIRCLE')) gt 0 and e.circle[i] eq 1 then plots, lon[i], lat[i], psym=8, symsize=0.2*skip^.5, thick=skip^.5
    if total(strcmp(tag_names(e), 'XSPOT'))  gt 0 and e.xspot[i] eq 1 then plots, lon[i], lat[i], psym=7, symsize=0.35*skip^.5, thick=skip^.5
  endif
  lon_copy = lon; for some reason "contour" changes the lat and lon arguments internally
  lat_copy = lat
  ;  contour, ws, lon_copy, lat_copy, /irr, /overplot, /follow
end


function getflags, ncid, varstring
  NCDF_VARGET, ncid,  varstring, int
  flags = {wvc_quality_flag : int}
  ncdf_attget, ncid, varstring, 'flag_masks', flag_masks
  ncdf_attget, ncid, varstring, 'flag_meanings', flag_meanings
  flag_meanings = strsplit(string(flag_meanings), ' ', /extract)
  if n_elements(flag_masks) ne n_elements(flag_meanings) then message, "flag_masks & flag_meanings different lengths"
  for iflag = 0,n_elements(flag_masks)-1 do begin
    ; flag_masks has values like 64, 128, 256, etc.
    ; flag will be set to one if the bit at the flag_masks value is set to one.
    flag = logical_true(int and flag_masks[iflag]) ; bitwise operation
    flags = create_struct( flag_meanings[iflag],flag, flags)
  endfor
  return, flags
end

pro all_figs
  basedir = '/glade/p/work/ahijevyc/ascat/2014/'
  ;plot_ascat_winds, basedir+'Fig1a.ps', file_search(basedir+'QS_XWGRD3_200824[3]*'), 32., 36., 142., 148.5
  ;plot_ascat_winds, basedir+'fig1b.ps', file_search(basedir+'QS_XWGRD3_2008244*'), 35.5, 39.5, 142., 148.5
  ;plot_ascat_winds, basedir+'panels_c_d.ps', file_search(basedir+'ascat_2008090[12]_*.nc'), 32.,36.,149,155.5
  ;plot_ascat_winds, basedir+'fig2a1.ps', file_search(basedir+'ascat_20080908_2*.nc'), 26.5,31.,145.5, 152.5
  ;plot_ascat_winds, basedir+'fig2a2.ps', file_search(basedir+'ascat_20080909_2*.nc'), 27.5,32.,142, 149.
  ;plot_ascat_winds, basedir+'fig2b.ps', file_search(basedir+'ascat_20080910_1100*.nc'), 28.5,33.,141, 148.
  ; What's the point of 90-deg segments? Well, they can be run in parallel and the savfiles
  ; may be combined later by h2d.pro
  ;plot_ascat_winds, '', file_search(basedir+'ascat_2014*_*_metop?_*_250_*.nc'), -30,30.,0, 90.
  ;plot_ascat_winds, '', file_search(basedir+'ascat_2014*_*_metop?_*_250_*.nc'), -30,30.,90, 180.
  ;plot_ascat_winds, '', file_search(basedir+'ascat_2014*_*_metop?_*_250_*.nc'), -30,30.,180, 270.
  ;plot_ascat_winds, '', file_search(basedir+'ascat_2014*_*_metop?_*_250_*.nc'), -30,30.,270, 360.
  ;plot_ascat_winds, '', file_search(basedir+'ascat_2014*_*_metop?_*_250_*.nc'), 17,18,360-30, 360-29.
  plot_ascat_winds, '', file_search(basedir+'ascat_[12]???????_??????_metop?_*_250_*.nc'), -30,30,0.,360., fcst_day=5, model='mpas_ep'
end

pro plot_ascat_winds, inpsfile, files, latmin, latmax, lonmin, lonmax, fcst_day=fcst_day, model=model
  ; Scatterplot of model vs ASCAT wind speed
  ; And global map of differences on lat-lon grid.
  if n_elements(inpsfile) eq 0 then inpsfile=''
  psfile=inpsfile
  if ~keyword_set(model) then model = 'GFS'
  model=mpas_mesh(model)
  time_window_min=90
  just00Z = 0 ; just00Z intended to replicate the fcst-init composites, which only use 00Z.
  ; But that's still not going to do it. The fcst-init composites use every day, while ASCAT
  ; overpasses only will have every other day for any particular point on Earth.
  if n_elements(fcst_day) eq 0 then fcst_day = 5; forecast day
  if n_elements(files) eq 0 then files =  file_search('/glade/p/work/ahijevyc/ascat/2014/ascat_[12][0-9][0-9][0-9]*_*_metop?_*_250_*.nc')
  if n_elements(latmin) eq 0 then latmin=-30
  if n_elements(latmax) eq 0 then latmax=30
  if n_elements(lonmin) eq 0 then lonmin=0
  if n_elements(lonmax) eq 0 then lonmax=360
  if psfile eq '' then psfile = file_dirname(files[0])+'/'+model.name+string(time_window_min,fcst_day,lonmin,lonmax,format='("_",I2.2,"min_fd",I0,"_",I3.3,"-",I3.3,"E")')+(just00Z?"_just00Z":"")+'.ps'
  set_plot, 'PS'
  set_plot, 'NULL'
  ;set_plot, 'X'
  if !D.NAME eq 'X' then device, decomposed=0
  if !D.NAME ne 'NULL' then begin
    loadct, 6, /sil
    tvlct, prism, /get
    tvlct, reverse(prism)
  endif
  if !D.NAME eq 'PS' then device, /color, bits=8, /close, filename=psfile
  pos = [0.08,0.57,0.82,0.8]

  bin1=0.25 ; wind speed histogram bin width, x-axis
  bin2=0.25 ; wind speed histogram bin width, y-axis
  max1=20. ; wind speed histogram max bin x-axis
  max2=20.

  ; Allocate global lat-lon grid to hold count, sums.
  dlon = 1. ; longitude bin width
  dlat = 1. ; latitude bin height
  n_latlon       = replicate(0,360./dlon,180./dlat)
  sum_latlon     = float(n_latlon)
  adj_sum_latlon = sum_latlon

  nfiles = n_elements(files) ; loop through ASCAT files
  for ifile = 0, nfiles-1 do begin
    file = files[ifile]
    if strmatch(file, '*.nc') eq 1 then begin
      ncid = NCDF_OPEN(file)

      flags = getflags(ncid, 'wvc_quality_flag')
      time= getvar(ncid, 'time')
      jday = julday(1,1,1990,0,0,time)
      lat = getvar(ncid, 'lat')
      lon = getvar(ncid, 'lon')
      wvc_index   = getvar(ncid, 'wvc_index')
      model_speed = getvar(ncid, 'model_speed')
      model_dir   = getvar(ncid, 'model_dir')
      ice_prob    = getvar(ncid, 'ice_prob')
      ice_age     = getvar(ncid, 'ice_age')
      wind_speed  = getvar(ncid, 'wind_speed')
      wind_dir    = getvar(ncid, 'wind_dir')
      bs_distance = getvar(ncid, 'bs_distance')

      NCDF_CLOSE, ncid      ; Close the NetCDF file
    endif else if strmatch(file_basename(file), 'QS_XWGRD3*') then begin
      resolve_routine, 'read_qscat3', /no_recompile, /compile_full_file
      ; Open the input file and initialize the SD interface
      sd_id=HDF_SD_START(file,/READ)
      ;Get the x-dimension in order to read an entire SDS
      index=HDF_SD_NAMETOINDEX(sd_id,'asc_wvc_count')
      sds_id=HDF_SD_SELECT(sd_id,index)
      HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
        type=data_type,caldata=cal
      slab_size=strtrim(dims(1),2)
      ir=0

      ; Read the SDSs - compile read_qscat3.pro
      asc_avg_wind_speed= get_sds(sd_id,'asc_avg_wind_speed',ir,slab_size)
      des_avg_wind_speed= get_sds(sd_id,'des_avg_wind_speed',ir,slab_size)
      asc_wind_speed=asc_avg_wind_speed
      des_wind_speed=des_avg_wind_speed
      asc_avg_wind_vel_u= get_sds(sd_id,'asc_avg_wind_vel_u',ir,slab_size)
      des_avg_wind_vel_u= get_sds(sd_id,'des_avg_wind_vel_u',ir,slab_size)
      asc_avg_wind_vel_v= get_sds(sd_id,'asc_avg_wind_vel_v',ir,slab_size)
      des_avg_wind_vel_v= get_sds(sd_id,'des_avg_wind_vel_v',ir,slab_size)
      asc_wind_dir = 90.-!RADEG*atan(asc_avg_wind_vel_v,asc_avg_wind_vel_u)
      des_wind_dir = 90.-!RADEG*atan(des_avg_wind_vel_v,des_avg_wind_vel_u)
      asc_wvc_count= get_sds(sd_id,'asc_wvc_count',ir,slab_size)
      des_wvc_count= get_sds(sd_id,'des_wvc_count',ir,slab_size)
      asc_wvc_index = asc_wvc_count
      des_wvc_index = des_wvc_count
      asc_time_frac= get_sds(sd_id,'asc_time_frac',ir,slab_size)
      des_time_frac= get_sds(sd_id,'des_time_frac',ir,slab_size)
      asc_rain_prob= get_sds(sd_id,'asc_rain_prob',ir,slab_size)
      des_rain_prob= get_sds(sd_id,'des_rain_prob',ir,slab_size)
      asc_rain_flag= get_sds(sd_id,'asc_rain_flag',ir,slab_size)
      des_rain_flag= get_sds(sd_id,'des_rain_flag',ir,slab_size)

      hdf_sd_attrinfo, sd_id, hdf_sd_attrfind(sd_id,'date_of_average'), data=time
      year = strmid(file_basename(file),10,4)
      doy  = strmid(file_basename(file),14,3)
      asc_jday = doy-1 + julday(1,1,year,0,0,0) + asc_time_frac
      des_jday = doy-1 + julday(1,1,year,0,0,0) + des_time_frac

      ; see file:///Volumes/pecan2/ahijevyc/cdavis/ascat/qscat_L3.html
      asc_flags = {knmi_quality_control_fails  :asc_rain_flag ge 4,$
        variational_quality_control_fails  :asc_rain_flag eq -1,$
        rain_detected                      :asc_rain_flag eq 2}
      des_flags = {knmi_quality_control_fails  :asc_rain_flag ge 4,$
        variational_quality_control_fails  :asc_rain_flag eq -1,$
        rain_detected                      :asc_rain_flag eq 2}


      ; Select latitude and longitudes
      lat1=latmin
      lat2=latmax
      if ((lat1 lt -90) or (lat2 lt -90) or (lat1 gt 90) or $
        (lat2 gt 90)) then begin
        print,'ERROR: Latitudes must be between -90 and 90'
        stop
      endif

      ; Make sure that lat2 is greater than lat1
      if (lat1 gt lat2) then begin
        itmp=lat1
        lat1=lat2
        lat2=itmp
      endif

      ; The last grid point is in cell 719.  Reduce lat 90. to 89.9
      if (lat2 eq 90.) then begin
        lat2=89.9
      endif

      lon1=lonmin
      lon2=lonmax
      if ((lon1 lt 0) or (lon2 lt 0) or (lon1 gt 360) or $
        (lon2 gt 360)) then begin
        print,'ERROR: Longitudes must be between 0 and 360'
        stop
      endif

      ; Make sure that lon2 is greater than lon1
      if (lon1 gt lon2) then begin
        itmp=lon1
        lon1=lon2
        lon2=itmp
      endif

      ; The last grid point is in cell 1439.  Wrapping is not done here,
      ; so 360, must be reduced to 359.9.
      if (lon2 eq 360.) then begin
        lon2=359.9
      endif

      ; Determine grid points from the latitudes and longitudes
      dx=(360./1440.)
      ii1=fix(lon1/dx)
      ii2=fix(lon2/dx)

      dy=(180./720.)
      jj1=fix((lat1+90.)/dy)
      jj2=fix((lat2+90.)/dy)
      lon = rebin(findgen(1440)/1440.*360.,1440,720)
      lat = rebin(transpose((findgen(720)/720.*2.-1.)*90.),1440,720)

      HDF_SD_END,sd_id
    endif else begin

      message, 'are you sure you want level 2B quickscat ? '+file
      ;Read the Time Tags contained in the VDATA
      fid=HDF_OPEN(file,/READ)
      wvc_row_time= get_vdata(fid,'wvc_row_time')
      HDF_CLOSE,fid

      ;Read the Scientific Data Sets
      sd_id=HDF_SD_START(file,/READ)

      ;Get the x-dimension in order to read an entire SDS
      index=HDF_SD_NAMETOINDEX(sd_id,'wvc_lat')
      sds_id=HDF_SD_SELECT(sd_id,index)
      HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
        type=data_type,caldata=cal
      slab_size=strtrim(dims(1),2)
      nr_wvc= strtrim(dims(0),2)
      ir=0

      ;; The L2B SDSs are read in their entirety.  An example of
      ;; reading the SDSs in slabs of size 1 is shown below.
      ;;
      ;;  slab_size=1
      ;;  for ir=irec1-1,irec2-1 do begin
      ;;    wvc_row=get_sds(sd_id,'wvc_row',ir,slab_size)
      ;;    ...
      ;;    print,'TIME: ',string(wvc_row_time(*,ir))
      ;;    print,'WVC Row: ',wvc_row(ir)
      ;;  endfor

      ; Read the SDSs
      wvc_row= get_sds(sd_id,'wvc_row',ir,slab_size)
      wvc_lat= get_sds(sd_id,'wvc_lat',ir,slab_size)
      wvc_lon= get_sds(sd_id,'wvc_lon',ir,slab_size)
      lat=wvc_lat
      lon=wvc_lon
      wvc_index= get_sds(sd_id,'wvc_index',ir,slab_size)
      num_in_fore= get_sds(sd_id,'num_in_fore',ir,slab_size)
      num_in_aft= get_sds(sd_id,'num_in_aft',ir,slab_size)
      num_out_fore= get_sds(sd_id,'num_out_fore',ir,slab_size)
      num_out_aft= get_sds(sd_id,'num_out_aft',ir,slab_size)
      wvc_quality_flag= get_sds(sd_id,'wvc_quality_flag',ir,slab_size)
      atten_corr= get_sds(sd_id,'atten_corr',ir,slab_size)
      model_speed= get_sds(sd_id,'model_speed',ir,slab_size)
      model_dir= get_sds(sd_id,'model_dir',ir,slab_size)
      num_ambigs= get_sds(sd_id,'num_ambigs',ir,slab_size)
      wind_speed= get_sds(sd_id,'wind_speed',ir,slab_size)
      wind_dir= get_sds(sd_id,'wind_dir',ir,slab_size)
      wind_speed_err= get_sds(sd_id,'wind_speed_err',ir,slab_size)
      wind_dir_err= get_sds(sd_id,'wind_dir_err',ir,slab_size)
      max_likelihood_est=get_sds(sd_id,'max_likelihood_est',ir,slab_size)
      wvc_selection= get_sds(sd_id,'wvc_selection',ir,slab_size)
      wind_speed_selection= get_sds(sd_id,'wind_speed_selection',ir,slab_size)
      wind_dir_selection= get_sds(sd_id,'wind_dir_selection',ir,slab_size)
      mp_rain_probability= get_sds(sd_id,'mp_rain_probability',ir,slab_size)
      nof_rain_index= get_sds(sd_id,'nof_rain_index',ir,slab_size)
      srad_rain_rate= get_sds(sd_id,'srad_rain_rate',ir,slab_size)
      wind_dir = wind_dir_selection
      wind_speed = wind_speed_selection

      HDF_SD_END,sd_id
      print, string(wvc_row_time(*,ir))
      flags = {knmi_quality_control_fails  :logical_true(wvc_quality_flag and 2^9B),$
        variational_quality_control_fails  :logical_true(wvc_quality_flag and 2^1B),$
        rain_detected                      :mp_rain_probability gt 0}

    endelse

    limit=[latmin,lonmin,latmax,lonmax]
    ; According to ASCAT user manual: It is recommended not to use WVCs with the monitoring flag,
    ; the KNMI quality control flag or the variational quality control flag set.
    ibox = where(lon ge lonmin and lon le lonmax and lat ge latmin and lat le latmax $
      and not flags.PRODUCT_MONITORING_EVENT_FLAG $
      and not flags.KNMI_QUALITY_CONTROL_FAILS $
      and not flags.VARIATIONAL_QUALITY_CONTROL_FAILS, nbox)

    ; This is ugly but I had to come up with a way to plot both ascending and descending passes
    ; in QS Quickscat files. They are named differently and everything.
    ; For example, ascending (asc_wind_speed) and descending (des_wind_speed) wind speed.
    for ascend_descend = 0, n_elements(asc_wind_speed) eq 0 ? 0 : 1 do begin
      if nbox gt 0 then begin
        if n_elements(asc_wind_speed) gt 0 then begin
          wind_speed_box = ascend_descend eq 0 ? asc_wind_speed[ibox] : des_wind_speed[ibox]
          wind_dir_box   = ascend_descend eq 0 ? asc_wind_dir[ibox]   : des_wind_dir[ibox]
          wvc_index_box  = ascend_descend eq 0 ? asc_wvc_index[ibox]  : des_wvc_index[ibox]
          jday_box       = ascend_descend eq 0 ? asc_jday[ibox]       : des_jday[ibox]
          flags          = ascend_descend eq 0 ? asc_flags            : des_flags
        endif else begin
          wind_speed_box = wind_speed[ibox]
          wind_dir_box   = wind_dir[ibox]
          jday_box       = jday[ibox]
        endelse
        lon_box        = lon[ibox]
        lat_box        = lat[ibox]
        wind_dir_box   = wind_dir_box+180 ; difference between FROM a direction and TO a direction.
        ; The wind directions are in oceanographic rather than meteorological convention
        ;(see section 6.1 http://www.knmi.nl/scatterometer/publications/pdf/ASCAT_Product_Manual.pdf)
        if n_elements(model_speed) gt 0 then begin
          model_speed = model_speed[ibox]
          model_dir   = model_dir[ibox]+180 ; difference between FROM a direction and TO a direction.
        endif
        ;        bs_distance = bs_distance[i]
        if n_elements(jday_box) gt 0 then print, 'average ascat time ', mean(jday_box), format='(A,C())'
        average_time = string(mean(jday_box),format='(c())')
        title = file_basename(file)+"!Caverage time of wind in plot bounds: "+average_time
        noerase = file_basename(psfile) eq 'fig2a.ps' ? 1 : 0
        ;noerase = 1
        if !D.NAME ne 'NULL' then plot_windbarbs, wind_speed_box, wind_dir_box, lon_box, lat_box, limit,  $
          "wind "+title, pos, bad=flags.knmi_quality_control_fails[ibox], $
          circle=flags.variational_quality_control_fails[ibox], xspot=flags.rain_detected[ibox], noerase=noerase
        xyouts, pos[2]+0.01, pos[3], $
          "small black circle indicates!Cvariational_quality_control_fails!Ci.e. wind is spatially inconsistent"+$
          "!C!Cblack wind barb indicates!Cknmi quality_control_fails"+ $
          "!C!CX indicates rain detected", $
          charsize=0.5, /norm
        if n_elements(asc_wind_speed) gt 0 then xyouts, pos[0], pos[1]-0.05, ascend_descend eq 0 ? "ascending" : "descending", /norm
        ;if !D.NAME ne 'NULL' then plot_time, jday_box, lon_box, lat_box, limit, "time "+title, pos, noerase=noerase

        ; scatterplot of model vs ASCAT wind speed
        ; Find matching model forecast with appropriate lead time.
        init_date = floor(mean(jday_box)-fcst_day-0.5+0.125)+0.5 ; round up to next date if hour>21 or round down otherwise
        valid_time = round(mean(jday_box)*4)/4. ; round to closest 6h
        if (just00Z) then begin
          valid_time = round(mean(jday_box)-0.5)+0.5 ; round to closest 00Z
        endif
        ; If GFS (not GFS_00Z) and you want to use 6-hrly initializations . . .
        if model.name eq 'GFS' && not just00Z then init_date = floor(mean(jday_box)*4+0.5)/4.-fcst_day
        init_date = string(init_date,format='(C(CYI4.4,CMOI2.2,CDI2.2,CHI2.2))')

        ; Eliminate ASCAT points more than time_window_min from model time.
        itime = where(abs(jday_box-valid_time) lt time_window_min/24d/60, nbox, /null)
        if itime eq !NULL then begin
          print, file_basename(file) + ' has no times within '+strtrim(time_window_min,2)+'min of potential model time. skipping'
          continue ; nothing good within time window
        endif
        ; Continue with ASCAT points within time window.
        lon_box = lon_box[itime]
        lat_box = lat_box[itime]
        jday_box = jday_box[itime]
        wind_speed_box=wind_speed_box[itime]

        nearestCells = mpas_nearest_cell(lon_box, lat_box, model)
        vitals = {max_spd10m:{field:'speed10',range:[0,0],op:'max',data:replicate(!VALUES.D_NAN, nbox)}}
        fill_vitals, model, nearestCells, init_date, valid_time, vitals, model_file=model_file
        print, init_date, valid_time, model_file, format='("init ",A,", valid ",C(),2x,"model file=",A)'
        if model_file eq '' then begin
          print, "no model file"
          continue ; didn't have this before Mar23, 2016. I wonder why. . . .
        endif
        if !D.NAME ne 'NULL' then plot_time, jday_box, lon_box, lat_box, limit, "ASCAT points in time window "+title, pos, noerase=noerase

        model_s10 = vitals.max_spd10m.data
        ascat = wind_speed_box
        stride=2 ; for scatterplot
        xtitle='ASCAT wind speed (m/s)'
        plot, ascat[0:*:stride], model_s10[0:*:stride], psym=3,/iso, ytitle=model.name+' '+init_date+'/'+$
          string(valid_time,format='(C(CYI4.4,CMOI2.2,CDI2.2,"_",CHI2.2))'), xtitle=xtitle, $
          position=[pos[0],0.09,0.6,pos[1]], /noerase, xrange=[0,max1], yrange=[0,max2]
        oplot, !X.CRANGE, !X.CRANGE ; 1:1 line
        xyouts, !X.CRANGE[0]+1, !Y.CRANGE[1]-2, string(nbox,time_window_min, format='(I0," ASCAT points within ",I1," min of!C")')$
          +model.name, charsize=0.5
        plot, [0,0],[max1,max2], position=[!X.WINDOW[1]+0.05,!Y.WINDOW[0],0.9,pos[1]],xrange=[0,max1], /iso, $
          xtitle='cumulative '+xtitle, /noerase

        ; There are nbox ascat points in the spatial domain and temporal window.
        ; Spatial domain is usually 30S-30N 180W-180E, but could be smaller for
        ; zoomed in plots.
        ; For every ascat point in the spatial and temporal window (nbox),
        ; map its position to the global lat/lon grid.
        ;   glon = grid x-index
        ;   glat = grid y-index
        ; Keep track of 2 wind differences (regular and adjusted)
        ; and the count in each grid box.
        for ii=0,nbox-1 do begin
          glon = floor(lon_box[ii]/dlon) - 0
          glat = floor(lat_box[ii]/dlat) - (-90)
          if finite((model_s10-ascat)[ii]) then begin
            sum_latlon[glon,glat] = sum_latlon[glon,glat] + (model_s10-ascat)[ii]
            adj_sum_latlon[glon,glat] = adj_sum_latlon[glon,glat] + (model_s10-adjust_ascat(ascat))[ii]
            n_latlon[glon,glat] = n_latlon[glon,glat] + 1
          endif
        endfor
        igood = where(finite(ascat) and finite(model_s10),/null) ; avoid warning
        if igood ne !NULL then begin
          h2d     = (n_elements(h2d) eq 0 ? 0 : h2d)         + hist_2d(ascat[igood],               model_s10[igood], bin1=bin1, bin2=bin2, max1=max1, max2=max2)
          adj_h2d = (n_elements(adj_h2d) eq 0 ? 0 : adj_h2d) + hist_2d(adjust_ascat(ascat[igood]), model_s10[igood], bin1=bin1, bin2=bin2, max1=max1, max2=max2)
          if !D.NAME ne 'NULL' then tv, bytscl(alog10(h2d>1)), !X.WINDOW[0], !Y.WINDOW[0], XSIZE=!X.WINDOW[1]-!X.WINDOW[0], /norm
          oplot, !X.CRANGE, !X.CRANGE ; 1:1 line
        endif



        ;      if n_elements(model_speed) gt 0 then plot_windbarbs, model_speed, model_dir, lon, lat, limit, "model!C"+title, pos
        for iflag = 0, n_tags(flags)-1 do begin
          ;        mymapset, limit, (tag_names(flags))[iflag]+"!C"+title, pos
          ;z = flags.(iflag)[ibox]
          ;ione = where(z, none)
          ;        if none gt 0 then plots, lon_box[ione], lat_box[ione], psym=1
        endfor
      endif ; if nbox>0 (nbox=points in lat/lon box)
      ;  mymapset, limit, 'backscatter distance!C'+title, pos
      ;  contour, bs_distance, lon_box, lat_box, /irr, /overplot, /cell_fill, min_value=0.1
      ;  contour, bs_distance, lon_box, lat_box, /irr, /overplot, /follow, levels=[0.01]
    endfor ; loop thru 1) ascending and 2) descending files, if necessary
  endfor ; loop thru ascat files.
  ; Save 2D histogram and global grid for plotting later by plot_h2d.
  ; It plots 2D histogram and global grid nicely with IDL function graphics.
  save, h2d, adj_h2d, bin1, bin2, max1, max2, model, sum_latlon, adj_sum_latlon, n_latlon, files, $
    filename=file_dirname(psfile,/mark)+file_basename(psfile, '.ps')+'_h2d.sav'

  !P.CHARSIZE=1
  if !D.NAME eq 'PS' then device, /close
  if !D.NAME eq 'PS' then print, 'made '+psfile
end

function spatial_bias, x
  g1 = image(x,shift(findgen(360)-180+0.5,180),findgen(180)-90+0.5,$
    margin=0.1,rgb_table=colortable(69,/reverse),map_projection='Equirectangular',grid_units=2,$
    limit=[-40, 0, 40, 360], center_longitude = 180., background_color="light grey",xtickdir=1)
  g1.min_value=-3.25
  g1.max_value= 3.25
  grid = g1.MAPGRID
  grid.linestyle = 'dotted'
  grid.grid_longitude = 30.
  grid.grid_latitude = 30.
  grid.LABEL_POSITION = 0
  grid.horizon_linestyle='solid_line'
  grid.label_angle = '0'
  m1 = mapcontinents()


  c = colorbar(target=g1,title='$m s^{-1}$',border=1)
  c.translate,0,5 ; in function window, -20 is good. but -30 is better in png output
  c.scale,1.5,0.6
  return, g1
end

pro h2d, fcst_day=fcst_day
  ct = colortable(41)
  just00Z = 0
  year = '2014'
  time_window = '90min'
  if ~keyword_set(fcst_day) then fcst_day = 5; forecast day
  models = ['mpas','GFS']
  foreach model,models,imodel do begin
    lonss = ['180-270','270-360','000-090','090-180']
    if file_test('/glade/p/work/ahijevyc/ascat/'+year+'/'+model+'_'+time_window+'_fd'+strtrim(fcst_day,2)+'_000-360E'+(just00Z eq 1?'_just00Z':'')+'_h2d.sav') then lonss = ['000-360']
    ;lonss = ['180-270','270-360','000-090','090-180']; uncomment if you want to use the 'old' GFS without the 6, 12, and 18Z included
    foreach lons, lonss, ifile do begin
      file = '/glade/p/work/ahijevyc/ascat/'+year+'/'+model+'_'+time_window+'_fd'+strtrim(fcst_day,2)+'_'+lons+'E'+(just00Z eq 1?'_just00Z':'')+'_h2d.sav'
      fi = file_info(file)
      if ifile gt 0 then old_sum_latlon = sum_latlon
      if ifile gt 0 then old_adj_sum_latlon = adj_sum_latlon
      if ifile gt 0 then old_n_latlon = n_latlon
      restore, file
      print, 'restored '+fi.name +' '+systime(0,fi.mtime)
      if ifile gt 0 then sum_latlon = sum_latlon + old_sum_latlon
      if ifile gt 0 then adj_sum_latlon = adj_sum_latlon + old_adj_sum_latlon
      if ifile gt 0 then n_latlon = n_latlon + old_n_latlon
      if n_elements(bin1) eq 0 then bin1=0.25
      if n_elements(bin2) eq 0 then bin2=0.25
      if n_elements(max1) eq 0 then max1=20.
      if n_elements(max2) eq 0 then max2=20.
      ; x and y are used to label axes on scatterplot. They are the axis values.
      ; They are not meant to be lon/lat values.
      x = findgen(max1/bin1+1)*bin1
      y = findgen(max2/bin2+1)*bin2
      g0 = image(alog10(adj_h2d>1),x,y,rgb_table=ct, axis_style=2, dimensions=[600,500], margin=[0.07,0.25,0.15,0.1], $
        xtitle='ASCAT (m s$^{-1}$)!C0.014 * aws$^2$ + 0.821 * aws + 0.961!C(aws=unadjusted ASCAT wind speed)',$
        ytitle=model.name+' (m s$^{-1}$)',title=file_basename(file,'.sav'),layout=[2,2,ifile+1],current=ifile gt 0,font_size=9)
      g0.xrange=[0,max1]
      g0.yrange=[0,max2]
      z = plot(g0.xrange,g0.xrange,overplot=g0) ; 1:1 line
      c = colorbar(target=g0,orientation=1,title='frequency')
      c.tickname='10$^{'+c.tickname+'}$'
      c.scale, 0.7, 0.7
    endforeach

    if imodel eq 0 then begin ; needed for one minus the other
      first_sum_latlon = sum_latlon
      first_adj_sum_latlon = adj_sum_latlon
      first_n_latlon = n_latlon
    endif

    for ascat_bias_correction=0,1 do begin
      minimum_pts = 50
      x = ascat_bias_correction eq 0 ? sum_latlon/n_latlon : adj_sum_latlon/n_latlon
      x[where(n_latlon lt minimum_pts)] = !VALUES.D_NAN
      g1 = spatial_bias(x)
      g1.title=strmid(file_basename(file),0,strpos(file_basename(file),'h2d',/reverse_search)-1) +(just00Z eq 1?'_just00Z':'')+ $
        "-ASCAT (" + (ascat_bias_correction eq 1 ? "" : "no ") + "ascat bias correction)!C"+year
      proj = g1.mapprojection
      grid = g1.mapgrid
      junk = timestamp_text()
      junk.string = junk.string + "!Call grid boxes with "+strtrim(minimum_pts,2)+" or more points"

      g1.window.save, file_dirname(file,/mark)+idl_validname(g1.title.string,/convert_all)+".png",resolution=150
    endfor

    ; spatial plot of sample counts
    if (1) then begin
      n_latlon[where(n_latlon eq 0)] = !VALUES.D_NAN ; is this a good idea?
      g1 = image(n_latlon,shift(findgen(360)-180+0.5,180),findgen(180)-90+0.5,margin=0.1,$
        rgb_table=colortable(41), grid_units=2, title=g1.title.string, background_color="light grey")
      m1 = mapcontinents()
      c = colorbar(target=g1,title='matches per 1x1-deg box')
      c.scale, 1, 0.6

    endif


  endforeach ; models
  ; This doesn't match the forecast minus init spatial plots exactly ( even over limited areas)
  ; fcst-init plots use every model run while Ascat comparisons only happen every other day.
  ; A particular point on the Earth is scanned about every 2 days by ASCAT.
  ; Still I'm surprised they are that different.
  g1 = image(first_sum_latlon/first_n_latlon-sum_latlon/n_latlon,shift(findgen(360)-180+0.5,180),findgen(180)-90+0.5,$
    margin=0.1, rgb_table=colortable(69,/reverse), grid_units=2, background_color="light grey", $
    title=strmid(g1.title.string,strpos(g1.title.string,'min_fd')-2,strpos(g1.title.string,'(')-8)+' '+ $
    models[0]+'-'+models[1]+" (no ascat bias correction)!C"+year)
  g1.min_value=-3.25
  g1.max_value=3.25
  m1 = mapcontinents()
  junk = timestamp_text()

  c = colorbar(target=g1,title='m s$^{-1}$')
  c.translate,0,-25


end

function get_vert, name
  if strmid(name, 2, /reverse) eq 'hPa' then begin
    result = stregex(name, '.*_([0-9]+)hPa', /extract, /sub)
    return, result[1] * 100L
  endif
  if name eq 'q2' then return, 2
  if strmid(name, 0, 2) eq 't2' then return, 2
  if name eq 'u10' then return, 10
  if name eq 'v10' then return, 10

end

pro mpas_basin_rmse

  ; grabs the mpas*/2*/field_f-i.nc created by run_fcst-init.sh
  ; and averages them all together.

  normalize_err = 0 ; divide RMSE and bias by the standard deviation
  year = '2016'
  debug=0
  ; On Jun 28, 2016, ahijevych realized he was calculating 
  ; 1) diff in the avgsqr file and 
  ; 2) sqdiff in the avg file.
  ; But these are equivalent. I don't think the sqdiff field is ever used here, actually!
  ; It is referred to in fcst-init_composite.ncl when compositing vect10
  ; Therefore ahijevych is eliminating it from the fcst-init.ncl script except when doing vect10.
  operations = ['avgsqr', 'avg']
  mpass = ['mpas15_3']
  nmpass = n_elements(mpass)
  ndays = 10
  xminor = 0 ; # minor tick marks
  if where(strmatch(mpass, 'GFS*'), /null) ne !NULL then begin
    ndays = 8 ; is GFS a member?
  endif
  if where(strmatch(mpass, 'mpas??_3'), /null) ne !NULL then begin
    ndays = 5
  endif
  ; Only assume every 6 hours if model='GFS' not 'GFS_00Z'
  if where(mpass eq 'GFS', /null) ne !NULL then begin
    xminor = 3 ; connote every 6 hours.
  endif


  ; vector and scalar quantities must be done separately. but not if they are in a list!
  ; vector fields and scalar fields
  fields = list('temperature_700hPa', 'temperature_850hPa', 'temperature_500hPa', 'q2','precipw',$
    't2','skintemp','relhum_850hPa','relhum_700hPa','relhum_500hPa', $
    'uzonal_200hPa', 'umeridional_200hPa', 'height_500hPa', 'height_200hPa', $
    ['u10','v10'],['uzonal_850hPa', 'umeridional_850hPa'],['uzonal_500hPa', 'umeridional_500hPa'],$
    ['uzonal_200hPa', 'umeridional_200hPa'])
  fields = list('height_200hPa','height_500hPa')
  ;if mpass[0] eq 'mpas_ep-mpas' then fields = list('meanT_500_300','uzonal_500hPa','temperature_500hPa', ['u10','v10'])

  basins = ['global']

  GFS = mpas_mesh('GFS') ; use for normalization factor

  for ib = 0,n_elements(basins)-1 do begin
    basin = basins[ib]
    for ifield = 0, fields.count()-1 do begin
      field = fields[ifield]
      ndim = n_elements(field)
      whatminuswhat = nmpass eq 1 ? mpass[0] : mpass[1]+'-'+mpass[0]
      ofile = '/glade/work/ahijevyc/rmse_comparisons/'+$
        strjoin(field,'_')+'_'+basin+'_basin_'+strjoin(mpass,'.')+(normalize_err ? '_normalized' : '') + '_'+year +'.png'

      ; Plot RMSE and RMSE difference on left side of page.
      x_stuff = {nodata:1, xtitle:"forecast day", xtickinterval:1, xminor:xminor, buffer:debug eq 0, $
        title:field, margin:[0.27,0.25,0.05,0.21]}
      nrow = 1 ; or nmpass if the difference plot were working
      rmse_p = plot([0],xrange=[0,ndays],ytitle=(normalize_err ? 'normalized ' : '')+'RMSE', _extra=x_stuff, layout=[2,nrow,1])
      if nrow gt 1 then rmse_diff_p = plot([0],xrange=rmse_p.xrange,ytitle=whatminuswhat, _extra=x_stuff, /current, layout=[2,nrow,3])
      ; if field is scalar (ndim=1), plot mean diff on the right. Don't bother with speed mean diff for vector field.
      if ndim eq 1 then begin
        me_plot = plot([0],xrange=rmse_p.xrange, ytitle=(normalize_err ? 'normalized ' : '')+"bias", _extra=x_stuff, /current, layout=[2,nrow,2])
        if nrow gt 1 then me_diff_p = plot([0],xrange=rmse_p.xrange, ytitle=whatminuswhat, _extra=x_stuff, /current, layout=[2,nrow,4])
      endif

      ;rmse_p.window.refresh, disable=debug eq 0
      l = legend(target=rmse_p, position=[(rmse_p.position)[0],(rmse_p.position)[1]-0.08], vertical_alignment='top', horizontal_alignment='left', vertical_spacing=0.002)
      latmin=-3 & latmax=81
      lonmin=-180 & lonmax=180
      thick = 3

      map = map('Cylindrical', limit=[latmin, lonmin, latmax, lonmax], font_size=font_size, thick=0.5, $
        /current, position=[(rmse_p.position)[2]+0.01,(rmse_p.position)[1]-0.18,0.95,(rmse_p.position)[1]-0.08], buffer=buffer)
      grid = map.MAPGRID
      grid.font_size = 5
      m1 = mapcontinents(fill_color='beige', /continents)

      ; Fix y-axes
      if normalize_err eq 1 and mpass[0] ne 'debug' then begin
        rmse_p.yrange=[0,1.8]
        if ndim eq 1 then me_plot.yrange=[-1,1.]
      endif else begin
        if strmid(field,0,6) eq 'height' then begin
          rmse_p.yrange=[0,65]
          me_plot.yrange=[0,15]
        endif
      endelse
      

;      regions = ['tropics','extratropics']
      regions = ['NH']
      nregions = n_elements(regions)
      for iregion = 0, nregions-1 do begin
        region = regions[iregion]
        color = strmatch(region,"extra*",/fold) ? "dark blue" : "red"

        for impas = 0, nmpass-1 do begin
          mpas_name = mpass[impas]
          mpas = mpas_mesh(mpas_name)
          per_day = strmatch(mpas_name, 'GFS') ? 4 : 1
          ntimes = ndays * per_day + 1
          if mpas_name eq 'mpas_ep-mpas' then begin
            per_day = 4
            ntimes = 41
          endif

          basedir = strmatch(mpas.name, 'GFS*') ? '/glade/scratch/ahijevyc/'+mpas.name+'/' : '/glade/p/nmmm0024/'+mpas.name+'/'
          ; Sanity Check - make sure longitude is -180 to 180 and not 0-360
          over180 = where(mpas.lonCell ge 180, /null)
          if over180 ne !NULL then mpas.lonCell[over180] = mpas.lonCell[over180] - 360.

          print, "mpas.name="+mpas.name+" basin="+basin+" region="+ region
          print, field
          ibasin = in_mybasin(mpas.lonCell, mpas.latCell, basin + '_' + region, px=px,py=py)
          izonal = in_mybasin(GFS.lonCell, GFS.latCell, region)
          uv = dblarr(ntimes, ndim, n_elements(operations))
          uv = reform(uv, ntimes, ndim, n_elements(operations)) ; preserve size-1 dimensions.
          time_variance = 0.
          for idim=0,ndim-1 do begin

            ; run ~/bin/stddev.csh first to create this. Use GFS variance to normalize all models
            avgsqr_file = file_dirname(ofile) + '/GFS/GFS_avgsqr.nc'
            nc2 = ncdf_open(avgsqr_file)
            ; Both ahijevyc and wrfrt had GFS directories
            ; Some subdirectories were links; some not.
            ; You may have an old and new file with the same name
            ; and think they are in the same path, but they are not.
            ; Make sure GFS wrfrt dir isn't where stddev.nc is.
            ; Force to grab normalization factor from GFS - Nov 2015.
            vertdimname=!NULL
            varid = ncdf_varid(nc2,GFS_equiv(field[idim],model=GFS))
            NCDF_VARGET, nc2, varid, avgsqr
            ; extract appropriate vertical level
            junk = ncdf_varinq(nc2,varid)
            for i=0,junk.ndims-1 do begin
              ncdf_diminq, nc2, junk.dim[i], dimname, dimsize
              if strmatch(dimname, 'lv_*') then vertdimname=dimname
            endfor
            ncdf_varget, nc2, ncdf_varid(nc2,vertdimname), vert_lvls
            ncdf_close, nc2
            k = where(vert_lvls eq get_vert(field[idim]), /null)
            avgsqr = avgsqr[*,*,k]
            ; if wind vector, ndim=2, add V-component to already gotten U-component
            ; or if it's a scalar, just assign avgsqr to time_variance.
            time_variance = time_variance + avgsqr


            for ioperation = 0, n_elements(operations)-1 do begin
              operation = operations[ioperation]
              nc_file = file_dirname(ofile)+'/'+mpas_name+'/'+GFS_equiv(field[idim],model=mpas)+'_'+operation+'_'+year+'.nc'
              if mpas_name eq 'mpas_ep-mpas' then nc_file = file_dirname(nc_file)+'/'+mpas_name+'.'+operation+'.nc'
              composite = file_info(nc_file)
              if file_test(nc_file) then begin
                ncid = ncdf_open(nc_file)
                print, "opened "+nc_file
                ; Check history attribute for components that are newer than composite.
                ; Assumes nco tool has written history attribute.
                ncdf_attget, ncid, 'history', history, /global
                history = strsplit(string(history), /extract, count=nparts)
                if file_test(basedir+history[-2]) eq 1 then history=basedir+history
                part_mtime = composite.mtime
                for ipart = 3, nparts-2 do begin
                  part_of_composite = file_info(history[ipart])
                  if part_of_composite.mtime gt part_mtime then part_mtime = part_of_composite.mtime
                endfor
              endif
              ; if composite needs to be made or is old, do it.
              if file_test(nc_file) eq 0 || composite.mtime lt part_mtime then begin
                command = 'nces -O '+(operation eq 'ttl' ? '':'-x ')+'-v nfile -y ' +operation+' '+basedir+year+'*/'+GFS_equiv(field[idim],model=mpas)+'_f-i.nc ' + nc_file
                print, command
                spawn, command, exit_status=status
                if status ne 0 then stop
                ncid = ncdf_open(nc_file)
              endif
              ; after doing ncea -y avgsqr 201*/uzonal_850hPa_f-i.nc t.nc
              ; If there is a "diff" variable use it. Otherwise use field[idim] (think this is just for mpas_ep-mpas).
              op_diff_name = ncdf_varid(ncid,'diff') ne -1L ? 'diff' : field[idim]
              op_diff = getvar(ncid,op_diff_name)
              if strmatch(mpas_name,'GFS*') eq 0 && strmatch(field[idim],"relhum_*") then op_diff=op_diff*(operation eq 'avg' ? 100. : 100^2.) ; mpas->RH%
              if debug ge 1 then print, "read variable "+op_diff_name

              ; Extract the correct vertical level, if needed.
              vertdimname=!NULL
              junk = ncdf_varinq(ncid,op_diff_name)
              for i=0,junk.ndims-1 do begin
                ncdf_diminq, ncid, junk.dim[i], dimname, dimsize
                if strmatch(dimname, 'lv_*') then vertdimname = dimname
              endfor
              if vertdimname ne !NULL then begin
                ncdf_varget, ncid, ncdf_varid(ncid,vertdimname), vert_lvls
                k = where(vert_lvls eq get_vert(field[idim]), /null)
                op_diff = op_diff[*,*,k,*]
              endif

              NCDF_CLOSE, ncid

              if strmatch(mpas_name, 'GFS*') || mpas_name eq 'mpas_ep-mpas' then begin
                ; need to combine lat/lon dimensions to be 1-D, like cellsOnCell
                ; BUT this makes the indices in ibasin irrelevant
                dims = size(op_diff, /dim)
                op_diff = reform(op_diff, [dims[0]*dims[1], ntimes])
              endif

              ; remove degenerate (1-element) dimensions
              op_diff = reform(op_diff)
              ; Get average op_diff weighted by area of cell
              ; recall op_diff is either the average (over multiple model runs) of the square of the f-i differences or the
              ; average (over multiple model runs) of the f-i differences (all as a function of lead time).
              for itime = 0, ntimes-1 do begin
                uv[itime,idim,ioperation] = total(op_diff[ibasin,itime]*mpas.areacell[ibasin]) / total(mpas.areacell[ibasin])
              endfor
            endfor ; operation (avgsqr and avg)


            ; added 'ttl' to get nfile. don't need it otherwise. Used to be in operations loop. nice because it created file
            ; if it didn't exist. But it was buried too deep and caused bugs when I needed simulation_times updated.
            nc_file = file_dirname(ofile)+'/'+mpas_name+'/'+GFS_equiv(field[idim],model=mpas)+'_ttl_'+year+'.nc'
            if mpas_name eq 'mpas_ep-mpas' then nc_file = file_dirname(nc_file)+'/'+mpas_name+'.avgsqr.nc'
            ncid = ncdf_open(nc_file)
            print, "opened "+nc_file
            if n_elements(nfile) gt 0 then old_nfile = nfile
            NCDF_VARGET, ncid,  ncdf_varid(ncid,'nfile'), nfile
            if n_elements(old_nfile) gt 0 && ~array_equal(nfile,old_nfile) then print, 'nfile changed from',old_nfile,' to',nfile
            simulation_times = mpas_name eq 'mpas_ep-mpas' ? findgen(41)*0.25 : getvar(ncid, "lead_time")
            NCDF_CLOSE, ncid
            ; plot number of files above graph. just for the first region, cause all regions have the same nfiles.
            if iregion eq 0 then begin
              rmse_p.window.refresh
              ; if you plot "count" before you refresh the text may fly off the page when the y-axis is expanded.
              ax = rmse_p.axes
              ax[2].showtext=1
              tickname = ax[2].tickname + '!C'
              ; redo upper X-axis with counts. Allow for simulation times that don't match or start at same value.
              ; for example, GFS is 0-8 by 0.25. MPAS is 1-10 by 1.0
              foreach tickvalue, ax[2].tickvalues, itick do begin
                iitick = where(simulation_times eq tickvalue, /null)
                tickname[itick] = tickname[itick] + (iitick ne !NULL ? string(nfile[iitick],format='(I0)') : '')
              endforeach
              ax[2].tickname = tickname
              ax[2].tickfont_size=6
              rmse_p.title.translate, 0, 1.8*ax[2].tickfont_size
              ax[2].title='samples'
            endif


          endfor ; ndim (2 for wind vector, 1 for simple scalars)


          ; Sum the average square of the differences in 2nd dimension. Dimension 2 is 2-elements for vectors (U and V wind),
          ;  or just 1 element for scalars (e.g. temperature or mixing ratio).
          mse      = total(reform(uv[*,*,0],ntimes,ndim),2) ;  reform prevents dimension 2 from being collapsed even if it is size one
          ; Sum the average differences across dimension 2. No effect for scalars (where dimension 2 is 1 element)
          mean_err = total(reform(uv[*,*,1],ntimes,ndim),2)

          ; normalize MSE and mean error if normalize_err = 1.
          ; This is the average temporal AND area-weighted SPATIAL variance.
          ; Switched from basin-average to 0-360 longitude zonal average (ibasin -> izonal)
          timespace_variance = normalize_err ? total(time_variance[izonal]*GFS.areaCell[izonal]) $
            / total(GFS.areaCell[izonal]) : 1.
          print, " normalizing factor=", timespace_variance
          rmse = sqrt(mse)/sqrt(timespace_variance)
          mean_err = mean_err/sqrt(timespace_variance)

          ; Remember diff of each model so differences can be plotted later.
          if impas eq 0 then begin
            tmp0 = rmse
            tmp2 = mean_err
          endif
          if impas eq 1 then begin
            tmp1 = rmse
            tmp3 = mean_err
          endif

          ; Plot RMSE
          linestyle = strmatch(mpas.name, "*_*") ? "dashed" : "solid"
          if strmatch(mpas.name, "GFS*") then linestyle = "dotted"
          _extra = {linestyle:linestyle, color:color, thick:thick}
          l.add, plot(simulation_times, rmse, overplot=rmse_p, _extra=_extra, name = mpas.name+" "+region+" "+year )


          ; Plot mean_err if scalar field
          if ndim eq 1 then begin
            junk = plot(simulation_times, mean_err, overplot=me_plot, _extra=_extra)
            junk = plot(junk.xrange, [0,0], overplot=me_plot) ; Zero-line
          endif

        endfor ; mpass

        ; doesn't work with different simulation_times (like for GFS and MPAS)
        ;        if nmpass eq 2 then begin ; if there are 2 models to compare
        ;          ; Plot difference between models.
        ;          junk = plot(simulation_times, tmp1-tmp0, color=color, sym_thick=2, name = region+" diff",$
        ;            overplot=rmse_diff_p, _extra=_extra)
        ;          junk = plot(junk.xrange, [0,0], overplot=rmse_diff_p) ; Zero-line
        ;          if ndim eq 1 then begin
        ;            junk = plot(simulation_times, tmp3-tmp2, color=color, sym_thick=2, name = region+" diff", $
        ;              overplot=me_diff_p, _extra=_extra)
        ;            junk = plot(junk.xrange,[0,0], overplot=me_diff_p) ; Zero-line
        ;          endif
        ;        endif
        if n_elements(map) gt 0 then begin
          ; Outline and label basin with in_mybasin(). Get ibox uses custom definitions of basins.
          ; There is another program, atcf_basin, which uses strict ATCF definitions of basins.
          ; atcf_basin is used by get_all_model_vitals.pro to get hits and false alarms, but not here.
          ; Here, in mpas_basin_rmse.pro, we use in_mybasin.
          basin_outline = plot(px, py, thick=thick, color=color, overplot=map, transparency=25)
        endif


      endfor ; region (tropics/extratropics)


      rmse_p.window.refresh
      ; remove first 3 characters of sample counts (these are the old axis labels 0-8)
      ax[2].tickname = tickname
      ax[2].tickname = strmid(tickname,3)
      l.font_size=7 ; for some reason setting this when l is first created doesn't stick.
      tmstmp = text(0.005,0.005, 'made '+systime(), /normal, font_size=4)
      rmse_p.window.save, ofile, resolution=175
      print, 'created '+ofile
      rmse_p.window.close
    endfor ; field
  endfor ; basin


end


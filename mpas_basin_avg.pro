pro mpas_basin_avg

  ; time series of mpas basin averages
  
  year = '2014'
  debug=1
  mpass = ['ep']
  mpass = ['mpas']
  nmpass = n_elements(mpass)
  ndays = 10
  junk = where(strmatch(mpass, 'GFS*'), nmatch) ; is GFS a member?
  if nmatch gt 0 then ndays = 8
  ct = colortable(41)
  
  ; vector and scalar quantities must be done separately. but not if they are in a list!
  ; vector fields and scalar fields
  fields = list('precipw')
  
  basins = ['AL']
  
  for ib = 0,n_elements(basins)-1 do begin
    basin = basins[ib]
    for ifield = 0, fields.count()-1 do begin
      field = fields[ifield]
      ndim = n_elements(field)
      margin = [0.15,0.25,0.1,0.1]
      
      regions = basin + '_' + ['tropics','extra_tropics']
;      regions = basin + '_test'
      nregions = n_elements(regions)
      for iregion = 0, nregions-1 do begin
        region = regions[iregion]
        for impas = 0, nmpass-1 do begin
        
          mpas_name = mpass[impas]
          mpas = mpas_mesh(mpas_name)
          per_day = mpas_name eq 'GFS' ? 4 : 8
          ntimes =ndays * per_day + 1
          
          basedir = mpas.name eq 'GFS' ? '/glade/scratch/ahijevyc/GFS/' : '/glade/p/nmmm0024/'+mpas.name+'/'
          if mpas_name eq 'mpas_ep' && file_test(basedir+'2014090200/diagnostics.2014-09-09_00.00.00.nc') then stop ; problem with this file. relhum* fields = 0. should be hidden.
          ; Sanity Check - make sure longitude is -180 to 180 and not 0-360
          over180 = where(mpas.lonCell ge 180, /null)
          if over180 ne !NULL then mpas.lonCell[over180] = mpas.lonCell[over180] - 360.
          
          _extra = {thick:1, xtitle:"date", xtickformat:'(c(CMOI,"/",CDI))', title:mpas.name+' mesh - '+field, xstyle:1}
          p = plot([0], [0], /nodata, ytitle='%', margin=margin, _extra=_extra, rgb_table=ct)
          l = legend(position=[margin[0],margin[1]-0.08], vertical_alignment='top', horizontal_alignment='left', vertical_spacing=0.01)
          latmin=-1 & latmax=51
          lonmin=-180 & lonmax=180
          thick = 3
          
          map = map('Mercator', limit=[latmin, lonmin, latmax, lonmax], $
            font_size=font_size, /current, position=[0.4,0.05,(p.position)[2],margin[1]-0.08], buffer=buffer)
          grid = map.MAPGRID
          grid.font_size = 5
          m1 = mapcontinents(fill_color='beige', /continents)
          
          ; Outline and label basin with in_mybasin(). Get ibox uses custom definitions of basins.
          ; There is another program, atcf_basin, which uses strict ATCF definitions of basins.
          ; atcf_basin is used by get_all_model_vitals.pro to get hits and false alarms, but not here.
          ; Here, in mpas_basin_rmse.pro, we use in_mybasin.
          ibasin = in_mybasin(mpas.lonCell, mpas.latCell, region,px=px,py=py)
          basin_outline = plot(px, py, thick=thick, color='red', overplot=map, transparency=25)
          
          init_times = list()
          init_vals = list()
          dates = file_search(basedir+'20150'+['82[789]','83?','90[1234]']+'??',count = ndates)
          dates = file_search(basedir+'201411????',count = ndates)
         foreach date, dates do begin
            nc_files = file_search(date+'/diag*_00*.nc', count = ndiag_files)
            if ndiag_files eq 0 then begin
              print, 'no diagnostic files for '+date
              continue
            endif
            simulation_times = list()
            init_times.add,  mpas_diagnostics_jday(nc_files[0])
            uv = list()
            color = round(255*randomu(seed,3))
            foreach nc_file, nc_files do begin
              ncid = ncdf_open(nc_file)
              simulation_times.add, mpas_diagnostics_jday(nc_file)
              op_diff = getvar(ncid,field)
              if strmatch(GFS_equiv(field,model=mpas),'relhum*') then op_diff=op_diff*100. ; relhum is % in GFS. Allow mpas to match that.
              vert_id = !NULL
              varids = ncdf_varidsinq(ncid)
              for i=0,n_elements(varids)-1 do if strmatch((ncdf_varinq(ncid, varids[i])).name, 'lv_*') then vert_id=varids[i]
              if vert_id ne !NULL then begin
                ncdf_varget, ncid, vert_id, vert
                k = where(vert eq get_vert(field), nmatch)
                if nmatch eq 0 then stop
                op_diff = op_diff[*,*,k,*]
              endif
              NCDF_CLOSE, ncid
              ; Get average op_diff weighted by area of cell
              uv.add, total(op_diff[ibasin]*mpas.areacell[ibasin]) / total(mpas.areacell[ibasin])
              
            endforeach
            init_vals.add, uv[0]
            p = plot(simulation_times.toarray(), uv.toarray(), '-', overplot=p, color=color, _extra=_extra, name=file_basename(date) )
          endforeach
          l.add, plot(init_times.toarray(), init_vals.toarray(), '-', overplot=p, color='black', thick=4, name='init. value')
          tmstmp = text(0.005, 0.005, 'made '+systime(), /normal, font_size=4)
          new_xrange = p.xrange
          new_xrange[1] = new_xrange[1]-ndays
          p.xrange = new_xrange
          ofile = '/glade/p/work/ahijevyc/mpas_plots/'+mpas.name+'/'+strjoin(field,'_')+'_'+mpas.name+'_mesh_'+region+'_'+year +'.png'
          if debug eq 0 then begin
            p.window.save, ofile, resolution=175
            p.window.close
          endif
          
        endfor ; mpass
        
      endfor ; region
      
    endfor ; field
  endfor ; basin
  
  
end


pro bias_as_function_of_mag
  win = GetWindows(/current)
  if win ne !NULL then win.erase

  model_name='ep'
  basedir = '/glade/p/nmmm0024/'+model_name+'/'
  model = mpas_mesh(model_name)
  varname = 's10'
  region = 'EP_tropics'
  ibasin = in_mybasin(model.lonCell, model.latCell, region, px=px,py=py)
  ;ibasin = [ibasin, in_mybasin(model.lonCell, model.latCell, 'WP_tropics'), in_mybasin(model.lonCell, model.latCell, 'AL_tropics')]
  ;region = 'EP WP AL tropics'

  condition_on = 'init_fcst_mean' ; 'fcst' or 'init' or 'init_fcst_mean'

  lead_time=5
  x = !NULL
  y = !NULL
  year = 2015
  if year eq 2014 then begin
     jstart = julday(8,lead_time+1,year,0)
     jend   = julday(11,13,year,0) 
  endif
  if year eq 2015 then begin
    jstart = julday(7,lead_time+1,year,0) ; 7/18/2015
    jend   = julday(10,30,year,0) ; 10/30/2015
  endif
  
  ; when using julday, specify hour=0; default is 12
  for jd = jstart, jend do begin
    date = string(jd,format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
    diag = 'diag*.'+strmid(date,0,4)+'-'+strmid(date,4,2)+'-'+strmid(date,6,2)+'_'+strmid(date,8,2)+'.00.00.nc'
    init  = file_search(basedir+date+'/'+diag,count=nfiles)
    if nfiles eq 0 then begin
      print, "init file "+init+" not found"
      continue
    endif
    bias = basedir+date+'/'+varname+'_f-i.nc'
    if file_test(bias) eq 0 then continue
    fcst_init_date = string(jd-lead_time,format='(C(CYI4.4,CMoI2.2,CDI2.2,CHI2.2))')
    fcst = file_search(basedir+fcst_init_date+'/'+diag,count=nfiles)
    if nfiles eq 0 then continue

    if strmid(condition_on,0,4) eq 'init' then d = init
    if condition_on eq 'fcst' then d = fcst
    ncid = ncdf_open(d)
    NCDF_VARGET, ncid,  ncdf_varid(ncid,varname), var
    ncdf_close, ncid

    if condition_on eq 'init_fcst_mean' then begin
      ncid = ncdf_open(fcst)
      NCDF_VARGET, ncid,  ncdf_varid(ncid,varname), var2
      ncdf_close, ncid
      var = (var+var2)/2.
    endif

    ncid = ncdf_open(bias)
    NCDF_VARGET, ncid,  ncdf_varid(ncid,'diff'), diff
    ncdf_close, ncid

    x = [x,var[ibasin]]
    ct = colortable(47, /transpose)
    y = [y,diff[ibasin,*,lead_time]]
    print, date, min(y), max(y)
  endfor
  color=ct[*,lead_time*25]
  s= plot(x[0:*:100],y[0:*:100],' ',name=string(lead_time,format='("day ",I0)'),symbol='.',$
    color=color,xrange=[0,20],yrange=[-12,12])
  l = legend(target=s)
  boxes = list()
  xloc = !NULL
  maxw=10
  for w=0,maxw do begin
    xloc = [xloc, w+0.5]
    boxes.add, y[where(x ge w and x lt w+1)]
  endfor
  b = boxplot(xloc, createboxplotdata(boxes,mean_values=means,outlier_values=outlier), $
    title="day "+strtrim(lead_time,2)+" "+region+" "+model_name+" "+string(year,format='(I4)'), $
    fill_color=color, yrange=[-12,12], /notch, mean_values=means, symbol_means='*', $
    xticklen=0.5,xtickinterval=1,xgridstyle=1,xminor=0,xtitle=condition_on+' wind speed (m s$^{-1}$)', $
    ytitle='forecast - init (m s$^{-1}$)',xrange=[0,maxw+1], yticklen=0.02)
  z = plot(b.xrange,[0,0],overplot=b)
  for w=0,maxw do count = text(xloc[w], (b.yrange)[1],strtrim(n_elements(boxes[w]),2), align=0.5,$
    font_size=7,/data,clip=0)

  map = map('Mercator', limit=[-10, -180, 60, 180], thick=0.5, /current, position=[0.17,0.15,0.45,0.26],title=region)
  grid = map.MAPGRID & grid.hide=1
  m1 = mapcontinents(fill_color='beige', /continents)
  basin_outline = plot(px, py, fill_color=color, overplot=map, fill_background=1, transparency=25)

  ofile = "/glade/work/ahijevyc/mpas_plots/"+model.name+"/"+varname+"_condition_on_"+condition_on+$
    "_day"+string(lead_time,format='(I2.2)')+"_"+idl_validname(region,/convert_spaces)+".png"
  b.window.save, ofile, resolution=180
  print, "created "+ofile
end



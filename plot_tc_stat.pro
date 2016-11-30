function read_max_vmax_comparison, file
  template = { $
    VERSION:1, $
    DATASTART:0, $
    DELIMITER:' ', $
    MISSINGVALUE:!VALUES.F_NAN, $
    COMMENTSYMBOL:'', $
    FIELDCOUNT: 14, $
    FIELDTYPES: [7,7,7,7,7,4,4,7,7,7,7,7,4,4], $
    FIELDNAMES: ['F1','COL_NAME','INIT','STORM_ID','AMODEL','Min_AMAX_WIND','Max_AMAX_WIND','F8','F9','F10','F11','F12','Min_BMAX_WIND','Max_BMAX_WIND'], $
    FIELDLOCATIONS:lindgen(14), $
    FIELDGROUPS:lindgen(14) }

  u = read_ascii(file, template=template)
  if not array_equal(u.INIT, u.F10) then stop ; init
  if not array_equal(u.STORM_ID, u.F11) then stop ; stormid
  if not array_equal(u.AMODEL, u.F12) then stop ; amodel
  t = {amax_wind: u.max_amax_wind,  storm_id: u.storm_id, amodel:u.amodel, BMAX_WIND:u.max_bmax_wind, $
    basin:strmid(u.storm_id,0,2), init:u.init}
  return, t
end

pro plot_tc_stat, basin=basin, storm_name=storm_name, buffer=buffer
  basedir = '/glade/p/work/ahijevyc/'
  column = 'TK_ERR' ; 'TK_ERR' 'ABS(AMAX_WIND-BMAX_WIND)'
  ; if tc_stat_file doesn't exist then try running run_plot_tc_stat below.
  tc_stat_file = basedir + 'METv4.1/' + column +'.tc_stat'
  if ~keyword_set(basin) then basin = ''
  if ~keyword_set(storm_name) then storm_name=''
  if ~keyword_set(buffer) then buffer=0
  t = read_tc_stat(tc_stat_file)
  ofile = basedir+'tc_stat_'+basin+storm_name+'_'+t.column[0]+'.png'

  xdim = {name:'LEAD',units:'h'}
  units = 'knots'
  if t.column[0] eq 'TK_ERR' then units = 'nm'
  ydim = {name:'MEAN',units:units}
  zdim = 'AMODEL'


  tags = tag_names(t)
  ixdim = where(tags eq xdim.name, /null)
  iydim = where(tags eq ydim.name, /null)
  izdim = where(tags eq zdim, /null)
  incl  = where(tags eq ydim.name+'_NCL', /null)
  incu  = where(tags eq ydim.name+'_NCU', /null)
  inval = where(tags eq 'VALID', /null)
  zs = t.(izdim)
  zs = zs[uniq(zs, sort(zs))]
  colors = ['blue', 'red', 'green', 'black', 'orange', 'brown', 'pink','yellow','grey','light blue', 'dark green']
  p = plot([0],/nodata, margin=[0.15,0.15,0.1,0.15], xtitle=xdim.name+' '+xdim.units, ytitle=ydim.name+' '+ydim.units, title=basin+storm_name+" "+t.column[0],buffer=buffer)
  ax = p.axes
  ax[0].tickinterval = 24
  ax[0].minor=1
  ax[0].style=1
  ax[2].tickinterval = 24
  ax[2].minor=1
  ax[2].style=1
  xvalid=list()
  valid= list()
  color=list()
  for iz = 0, n_elements(zs)-1 do begin
    z = zs[iz]
    if keyword_set(basin) then extra_filter = t.basin eq basin
    if keyword_set(storm_name) then extra_filter = t.storm_name eq storm_name
    izs = where(t.(izdim) eq z and extra_filter, /null)
    if izs eq !NULL then continue
    x   = double((t.(ixdim))[izs])
    y   = double((t.(iydim))[izs])
    g = where(finite(y) and finite(y), /null)
    xvalid.add, x[g]
    valid.add, ((t.(inval))[izs])[g]
    color.add, colors[0] & colors=shift(colors,-1)
    p = plot(x[g], y[g], overplot=p, color=color[-1], name=z, thick=2)
    if incl ne !NULL then begin
      ncl = double((t.(incl))[izs])
      ncu = double((t.(incu))[izs])
      yerror = transpose([[y-ncl],[ncu-y]])
      ;p = errorplot(x, y, Yerror, errorbar_color=color,overplot=p,  _extra=keywords)
      g = where(finite(ncl) and finite(ncu), /null)
      if g ne !NULL then poly = polygon([x[g],reverse(x[g])],[ncu[g],reverse(ncl[g])], /data, /fill_background, fill_color=color[-1], color=color[-1], transparency=86, target=p)
    endif
  endfor
  p.refresh, /disable
  p.title.translate, 0, valid.count()*0.025, /normal
  ;  p.yrange=[0,500]
  p.xrange=[0,192]
  p.scale, 1, 0.9, 1
  l = legend(/relative, position=[0.23,0.9])
  p.translate, 0, -0.03, /normal

  for ia=0,valid.count()-1 do a=axis('x', location= p.convertcoord(0, 1+ia*0.03, /relative, /to_data), $
    tickname=valid[ia], color=color[ia], /data, target=p, $
    textpos=1, tickfont_size=4, clip=0, minor=0, ticklen=0, tickvalues=xvalid[ia], thick=0)
  p.refresh
  p.window.save, ofile, resolution=175
  print, 'created '+ofile


end

pro run_plot_tc_stat
  ;[ahijevyc@yslogin6 METv4.1]$ tc_stat -job summary -lookin out_tcmpr.tcst -by LEAD -by amodel -column TK_ERR -out t -by basin -event_equal TRUE
  basins = ['AL','WP','EP']
  ;[ahijevyc@yslogin6 METv4.1]$ tc_stat -job summary -lookin out_tcmpr.tcst -by LEAD -by amodel -column TK_ERR -out t -by storm_name
  ;storm_names = ['ERIN', 'KONG-REY', 'HUMBERTO', 'GABRIELLE']

  ;for i = 0, n_elements(storm_names)-1 do plot_tc_stat, storm_name=storm_names[i], buffer=1
  for i = 0, n_elements(basins)-1 do plot_tc_stat, basin=basins[i], buffer=1
end
pro plot_atcf

  storm = 'al112015'
  obsfile = '/glade/p/work/ahijevyc/atcf/b'+storm+'.dat'
  obsfile = '/glade/p/work/ahijevyc/ECMWF/atcf/b'+storm+'.dat'
  obs = read_atcf(obsfile)
  
  outdir = '/glade/p/work/ahijevyc/tracking_gfdl/'
  thick=4
  vmax = max(obs.vmax,imax)
  stormname = obs.stormname[imax]
  if (max(obs.yyyy) ne min(obs.yyyy)) then stop
  year = obs.yyyy[0]
  plot_list = list()
  xrange=julday([9,10], [28,4], 2015, 0,0,0)
  plot_list.add, plot(obs.julday, obs.vmax, title=stormname, xrange=xrange, yticklen=0.5, ysubticklen=0.05, xticklen=0.5, xsubticklen=0.05, xminor=3, xtickformat='(c(CDI,x,CMoA))', ytitle='Maximum sustained wind (knots)', thick=thick, xtitle=year, name='best track')
  ;adeck = read_atcf('/glade/scratch/ahijevyc/mpas_conv/a'+storm+'.dat_0.500deg_025km_gfdl_1.0d_minimum')
  adeck = read_atcf(file_dirname(obsfile)+'/a'+storm+'.dat',tech='EMX')
  colors = ['pink','blue','grey','yellow green','red','purple','orange']
  init_dates = ['2015092800','2015092900','2015093000','2015100100','2015100200','2015100300','2015100400']
  foreach init_date, init_dates, idate do begin
    ii = where(adeck.init_yyyymmddhh eq init_date and adeck.vmax gt 0, /null)
    if ii ne !NULL then plot_list.add, plot(adeck.julday[ii], adeck.vmax[ii], name = init_date, overplot=plot_list[0], color=colors[idate], thick=thick-1)
  endforeach
  l = legend(target=plot_list, /relative, position=[0.78,.45])
  plot_list[0].window.save, outdir+storm+'_vmax.png', resolution=175


  plot_list = list()
  plot_list.add, plot(obs.julday, obs.mslp, title=stormname, xrange=xrange, yticklen=0.5, ysubticklen=0.05, xticklen=0.5, xsubticklen=0.05, xminor=3, xtickformat='(c(CDI,x,CMoA))', ytitle='Minimum sea level pressure (mb)', thick=thick, xtitle=year, name='best track')
  foreach init_date, init_dates, idate do begin
    ii = where(adeck.init_yyyymmddhh eq init_date and adeck.mslp gt 0, /null)
    if ii ne !NULL then plot_list.add, plot(adeck.julday[ii], adeck.mslp[ii], name = init_date, overplot=plot_list[0], color=colors[idate], thick=thick-1)
  endforeach
  l = legend(target=plot_list, /relative, position=[0.35,.45])


  plot_list[0].yrange = [950,1010]
  plot_list[0].window.save, outdir+storm+'_mslp.png', resolution=175


end
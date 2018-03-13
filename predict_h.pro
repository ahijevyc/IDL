pro hist2, y, range, binsize
  h = histogram(y, min=range[0],max=range[1], binsize=binsize,locations=xval)
  oplot, xval+binsize/2., h, psym=10
  nbins = round((range[1]-range[0])/binsize)
  for ibin=0,nbins-1 do polyfill, [xval[ibin],xval[ibin],xval[ibin+1],xval[ibin+1]], [0,h[ibin],h[ibin],0], /line_fill, orientation=40
end

pro predict_h
  if !D.NAME eq 'PS' then begin
    device, /close
    !P.THICK=2
    !P.CHARTHICK=2
  endif
  
  !P.MULTI=[0,0,2]
  
  pos = [0.1,0.1,0.9,0.9]
  dTrange=[-4.5,4.5]
  levs = [1000,850,500,300]
  IRtypes = ['all','lt-20C']
  for iIRtype = 0, n_elements(IRtypes)-1 do begin
    IRtype = IRtypes[iIRtype]
    nlevs = n_elements(levs)
    for ilev = 0, nlevs-1 do begin
      level = string(levs[ilev],format='(I5.5,"mb")')
      
      
      
      ; ====== 0-200km vs 200-300km ======
      file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/developing_le2days_prior_'+level+'_IR10km_scatter.txt'
      ; if get_scatter is undefined, then compile cold_warm_vs_ir
      get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
      i = where(distance_km lt 200 and IR_range(IR, IRtype))
      dTv200  = TvC[i]       - GFS_mean.Tv[i]
      dMSE200 = other.MSE[i] - GFS_mean.MSE[i]
      i = where(distance_km lt 300 and distance_km ge 200 and IR_range(IR, IRtype))
      thin     = TvC[i] - GFS_mean.Tv[i]
      dMSE200_300 = other.MSE[i] - GFS_mean.MSE[i]
      
      binsize=0.5
      h =histogram(dTv200, min=dTrange[0],max=dTrange[1], binsize=binsize, locations=xval)
      plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle = 'dTv (C)', ytitle = 'frequency', title=level+', '+IRtype+' IR, <=2d, 0-200km (thick), 200-300km (hatch)'
      hist2, thin, dTrange, binsize
      
      binsize=level eq '00300mb' ? 1 : 2
      hrange =level eq '00300mb' ? [-6,6]: [-15,18]
      h =histogram(dMSE200, min=hrange[0],max=hrange[1], binsize=binsize, locations=xval)
      plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle = 'dMSE (kJ/kg)', ytitle = 'frequency', title=level+', '+IRtype+' IR, <=2d, 0-200km (thick), 200-300km (hatch)'
      hist2, dMSE200_300, hrange, binsize
      ptimestamp
      
      ihi = where(level eq '01000mb' and (other.MSE-GFS_mean.MSE) gt 5 and distance_km lt 200, nhi)
      if nhi gt 0 then print, (other.pgi)[ihi] + string(other.MSE[ihi]) + string(GFS_mean.MSE[ihi]) + string((other.MSE-GFS_mean.MSE)[ihi]) + (other.filename)[ihi]
    endfor
  endfor
  
  
  !P.MULTI=0
  
  level = '00925mb'
  IRtype = 'lt-20C'
  nlevs = n_elements(levs)
  ; ==== <=2d vs non-developing =====
  file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/developing_le2days_prior_'+level+'_IR10km_scatter.txt'
  get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
  i = where(distance_km lt 200 and IR_range(IR, IRtype))
  dTv200     = TvC[i] - GFS_mean.Tv[i]
  file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/non-developing_'+level+'_IR10km_scatter.txt'
  get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
  i = where(distance_km lt 200 and IR_range(IR, IRtype))
  dT_nd     = TvC[i] - GFS_mean.Tv[i]
  
  binsize=0.5
  h =histogram(dTv200, min=dTrange[0],max=dTrange[1], binsize=binsize, locations=xval)
  plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle = 'dTv (C)', ytitle = 'frequency', title=level+', 0-200km, '+IRtype+' IR, <=2d (thick), non-dev (hatch)'
  hist2, dT_nd, dTrange, binsize
  
  
  level = '00400mb'
  file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/developing_gt2days_prior_'+level+'_IR10km_scatter.txt'
  get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
  i = where(distance_km lt 200 and IR_range(IR,IRtype))
  dTv200     = TvC[i] - GFS_mean.Tv[i]
  i = where(distance_km ge 200 and distance_km lt 700 and IR_range(IR, IRtype))
  thin     = TvC[i] - GFS_mean.Tv[i]
  
  binsize=0.5
  h =histogram(dTv200, min=dTrange[0],max=dTrange[1], binsize=binsize, locations=xval)
  plot, xval+binsize/2., h, psym=10, xstyle=1, yrange=[0,14], thick=10, xtitle = 'dTv (C)', ytitle = 'frequency', title=level+', '+IRtype+' IR, >2d, 0-200km (thick), 200-700km (hatch)'
  hist2, thin, dTrange, binsize
  ptimestamp
  
  !P.MULTI=[0,0,3]
  levs = ['00800mb', '00500mb']
  IRtypes = ['lt-20C','ge-20C']
  for iIRtype = 0, n_elements(IRtypes)-1 do begin
    IRtype = IRtypes[iIRtype]
    for ilev = 0, n_elements(levs)-1 do begin
      level = levs[ilev]
      file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/developing_le2days_prior_'+level+'_IR10km_scatter.txt'
      get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
      rh = e_h2o(DwptC+!CONST.T0)/e_h2o(TempC+!CONST.T0)*100.
      i = where(distance_km lt 200 and IR_range(IR, IRtype))
      thick = RH[i] - GFS_mean.RH[i]
      RH_thick = RH[i]
      GFS_thick = GFS_mean.RH[i]
      
      format = '(A)'
      ;      ihi = where(level eq '00500mb' and distance_km lt 200 and IR_range(IR, IRtype), nhi)
      ;      if nhi gt 0 then print, file, (other.pgi)[ihi]+string(RH[ihi]) + string(GFS_mean.RH[ihi])+' '+(strmid(other.filename,41))[ihi],format=format
      
      file = '/Volumes/pecan2/ahijevyc/PREDICT/analysis/non-developing_'+level+'_IR10km_scatter.txt'
      get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
      i = where(distance_km lt 200 and IR_range(IR, IRtype))
      thin = RH[i] - GFS_mean.RH[i]
      RH_thin = RH[i]
      GFS_thin = GFS_mean.RH[i]
      
      binsize=10
      histrange = [-70,70]
      h =histogram(thick, min=histrange[0],max=histrange[1], binsize=binsize, locations=xval)
      plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle='dRH %', ytitle = 'frequency', title=level+', 0-200km, '+IRtype+' IR, <=2d (thick), non-dev (hatch)'
      hist2, thin, histrange, binsize
      
      histrange = [-10,110]
      h =histogram(RH_thick, min=histrange[0],max=histrange[1], binsize=binsize, locations=xval)
      plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle='RH %', ytitle = 'frequency', title=level+', 0-200km, '+IRtype+' IR, <=2d (thick), non-dev (hatch)'
      hist2, RH_thin, histrange, binsize
      ;      ihi = where(level eq '00500mb' and distance_km lt 200 and IR_range(IR, IRtype), nhi)
      ;      if nhi gt 0 then print, file, (other.pgi)[ihi]+string(RH[ihi]) + string(GFS_mean.RH[ihi])+' '+(strmid(other.filename,41))[ihi],format=format
      
      h =histogram(GFS_thick, min=histrange[0],max=histrange[1], binsize=binsize, locations=xval)
      plot, xval+binsize/2., h, psym=10, xstyle=1, thick=10, xtitle='RH %', ytitle = 'frequency', title=level+', 0-200km, '+IRtype+' IR, GFS, <=2d (thick), non-dev (hatch)'
      hist2, GFS_thin, histrange, binsize
      
      ptimestamp
    endfor
  endfor
  
  
  
  if !D.NAME eq 'PS' then begin
    outfile = (fstat(!D.UNIT)).NAME
    device, /close
    pwd
    print, outfile
  endif
  !P.THICK=1
  !P.CHARTHICK=1
end
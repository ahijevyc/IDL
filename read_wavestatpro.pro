pro do_plot, t, field, thresh, pos
  TILES = t.field14[uniq(t.field14, sort(t.field14))] ; Get names of all the tiles (TILE1 - TILE_TOT)
  xleg = pos[2] + 0.017
  yleg = pos[3] + 0.08
      
  for itile = 0, n_elements(tiles)-1 do begin
    color = itile * 255. / n_elements(tiles)
    iline = where(t.field14 eq TILES[itile] and t.field17 eq thresh)
    x = t.field27[iline[1:*]]
    psym = ((itile+1) mod 7) + 1
    if field eq 'I-S' then y = t.field29[iline[1:*]]
    if field eq 'MSE' then y = t.field28[iline[1:*]] 
;    oplot, x, y, color = color, linestyle=1, thick=0.1
    plots, x, y, psym=psym, color = color
    yleg = yleg - 0.012
    plots, xleg, yleg, /normal, psym=psym, color = color
    xyouts, xleg+0.01, yleg-0.004, TILES[itile], /normal, color = color, charsize=0.75
  endfor  


end

pro read_wavestatpro
  
  
  infile = '/pecan/ahijevyc/Vx/subj_eval/WaveletStat_output/geom005/wavelet_flag_0_2/wavelet_stat_240000L_20050601_000000V_isc.txt'
  
  if (!D.NAME eq 'PS') then device, /close, /color, bits=8, filename=infile + '.ps', xoffset=0.5, yoffset=0.5, xsize=7.5, ysize=10, /inches
  loadct, 39
  oldD = !D.NAME & set_plot, 'X'
  t = read_ascii(infile, header=header, data_start=1, template=ascii_template(infile))
  if n_tags(t) ne 33 then stop, 'wavestat output format has changed'
  set_plot, oldD 
  field = strsplit(header, /extract)

  pos = [0.1, 0.1, 0.85, 0.8]     
  
  threshs = ['>0.000', '>13.000']
  for ithresh = 0, n_elements(threshs)-1 do begin
    thresh = threshs[ithresh]
    iline = where(t.field14 eq 'TILE_TOT' and t.field17 eq thresh)
    x = t.field27[iline[1:*]]
        
    ; Plot I-S Skill score
    y = FLOAT(t.field29[iline[1:*]]) ; skip stats for binary error field in the first (zeroth) position.
    plot, x, y, xrange = [1, 10], yrange=[-5., 1.2], ystyle=1, xtickname=STRtrim(StRING(4*2^(x-1), FORMAT='(A0)'),2), xtickinterval=1, xstyle=2, xminor=1, position = pos, xtitle='km', ytitle='I-S skill score', /nodata
    xyouts, 0.5, 0.95, 'Intensity-Scale skill score '+thresh+ 'mm/h!C' +stRmid(infile, STRPOS(infile, 'geom')), /norm, align=0.5
    axis, 0, !Y.CRANGE[1],  xaxis=1, xstyle=2, xrange = [1, 10], xtickname= STRtrim(StRING(2^(x-1), FORMAT='(A0)'),2), xtickinterval=1, xtitle='points', xminor=1
    axis, 0, 1.62, xaxis=1, xstyle=2, xrange = [1, 10], xtickname= STRtrim(StRING(x, FORMAT='(A0)'),2), xtickinterval=1, xtitle='iscale', xminor=1
    hss = mean(y)
    oplot, !X.CRANGE, [hss, hss], linestyle = 2, thick=4
    xyouts, !X.CRANGE[0]+0.03, hss, 'Heidke skill score!C(HSS) = ' + STRING(hss, FORMAT='(F6.3)'), charsize=0.8, alignment=0.5
    do_plot, t, 'I-S', thresh, pos
    oplot, x, y, thick=5, psym=10

    ; Plot MSE
    y = FLOAT(t.field28[iline[1:*]]) ; skip stats for binary error field in the first (zeroth) position.
    yrange = max(y) lt 0.02 ? [0, 0.02] : [0, 0.04]
    plot, x, y, xrange = [1, 10], yrange=yrange, ystyle=1, xtickname=STRtrim(StRING(4*2^(x-1), FORMAT='(A0)'),2), xtickinterval=1, xstyle=2, xminor=1, position = pos, xtitle='km', ytitle='MSE', /nodata
    xyouts, 0.5, 0.95, 'MSE '+thresh+"mm/h!C" + strmid(infile, STRPOS(infile, 'geom')), /norm, align=0.5
    axis, 0, !Y.CRANGE[1],  xaxis=1, xstyle=2, xrange = [1, 10], xtickname= STRtrim(StRING(2^(x-1), FORMAT='(A0)'),2), xtickinterval=1, xtitle='points', xminor=1
    axis, 0, 1.1*!Y.CRANGE[1], xaxis=1, xstyle=2, xrange = [1, 10], xtickname= STRtrim(StRING(x, FORMAT='(A0)'),2), xtickinterval=1, xtitle='iscale', xminor=1
    B = FLOAT(t.field33[iline[0]]) & s = float(t.field32[iline[0]]) & L = float(t.field26[iline[0]])
    MSErand = B * s * (1 - s) + s * (1 - B * s)
    MSErand = MSErand/(L+1)
    oplot, !X.CRANGE, [MSErand, MSErand], linestyle = 2, thick=4
    xyouts, !X.CRANGE[0]+0.03, MSErand, 'MSE!Drand!N = ' + STRING(MSErand, FORMAT='(F6.3)'), charsize=0.8, alignment=0.5
    do_plot, t, 'MSE', thresh, pos
    oplot, x, y, thick=5, psym=10
  
  endfor
  if (!D.NAME eq 'PS') then device, /close, /color
end
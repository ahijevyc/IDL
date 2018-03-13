pro plot_mtm_track


  map_set, 0, -75, limit=[7, -97, 24, -12], /cont, /grid, /iso, /hires, /ortho, /label
  
  ; LSC-I domain for RF training 2011
;  map = map('Mercator', limit=[22, -113, 50, -64], fill_color='light_blue')
;  grid = map.MAPGRID
;  grid.linestyle="dotted"
;  grid.label_position=0
; m1 = MAPCONTINENTS(/cont, fill_color="MOCCASIN")
;m2 = MAPCONTINENTS(/USA, COMBINE = 0, $
;   FILL_COLOR = "MOCCASIN")
;m4 = MAPCONTINENTS(/LAKES, FILL_COLOR="light blue")
;  o = plot( [-110,-110,-67,-67,-75,-110],[25,48,48,37,25,25], overplot=map, thick=3)

  
  textfile = '/Volumes/pecan2/ahijevyc/PREDICT/mtm_track/mtm_track_plot.txt'
  close, /all
  openw, unit, textfile, /get_lun
  
  pgis = [38, 44, 46]
  for ipgi = 0, n_elements(pgis)-1 do begin
    pgi = pgis[ipgi]
    case pgi of
    ; note the last date is one day later than the last date in the run_calc_rings.pro.  
      38:  dates = ['0829','0830','0831','0901','0902','0903','0904','0905','0906','0907','0908','0909','0910', '0911', 'GASTON']
      44:  dates = ['0909','0910','0911','0912','0913','0914','0915','0916', '0917', 'KARL']
      46:  dates = ['0916','0917','0918','0919','0920','0921','0922','0923','0924','0925', '0926', 'MATTHEW']
    endcase
    for idate=0,n_elements(dates)-2 do begin
      for ihour = 0, 23 do begin
        pgistring = 'PGI'+strtrim(pgi,2)+'L'
        datestring = '2010'+dates[idate]
        timestring = string(ihour, format='(I2.2,"0000")')
        mtm_center, pgistring, datestring, timestring, clat, clon, mtm00=1
        if finite(clon) and finite(clat) then printf, unit, pgistring, " ", datestring, " ", timestring, string(clon, clat, format='(F7.2)')
        if ihour eq 0 then plots, clon, clat, continue = idate, thick=3, psym=-1., symsize=2
        if idate eq 0 and ihour eq 0 then xyouts, clon, clat-0.9, '!C'+dates[n_elements(dates)-1], align=0.5, charsize=2.0, charthick=2.0
        if ihour eq 0 then xyouts, clon, clat-0.9, string(strmid(dates[idate],0,2),format='(I0)')+"/"+string(strmid(dates[idate],2,2),format='(i0)'), align=0.5, charsize=1.5, charthick=1.5
      endfor

    endfor
  endfor
  close, unit
  
  
end
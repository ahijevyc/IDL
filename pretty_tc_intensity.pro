pro pretty_TC_intensity, lineplot, toplot=toplot, color=color
  if ~keyword_set(toplot) then toplot = 'vmax'
  lineplot.Refresh, /disable
  axes = lineplot.axes
  xax = axes[0]
  xax2 = axes[2]
  yax = axes[1]
  yax2 = axes[3]
  yax2.hide = 1
  xax.tickfont_size = 11.
  xax.tickformat='(C(CDI0, " ", CMoA3))'
  xax.text_orientation=0
  yax.minor = 0
  yax2.minor = 0
  foreach a, [xax,xax2] do begin
      a.subticklen=0.02
      a.tickunits='Days'
      a.tickinterval=1d
      a.minor=3
      a.ticklen=1
      a.thick=0
  endforeach

  yax.ticklen=1
  yax.thick=0
  fill_color = lineplot.mapgrid.fill_color
  if toplot eq 'vmax' then begin
    ts = 34.
    cat1 = 64.
    cat2 = 83.
    cat3 = 96.
    cat4 = 113.
    cat5 = 137.
    x = xax.xrange[0] + 0.03*(xax.xrange[1] - xax.xrange[0])
    objs = list()

    if color then begin
        ; started with python module atcf variable atcf.colors
        cNONTD = [255.0, 255.0, 255.0]
        cTD = [209.10000000000002, 237.66000000000003, 247.35000000000002]
        cTS = [229.5, 244.8, 221.34000000000003]
        cCAT1 = [253.47, 251.94, 216.75000000000003]
        cCAT2 = [254.49, 242.25000000000003, 206.55]
        cCAT3 = [252.45, 227.46, 214.20000000000002]
        cCAT4 = [255.0, 207.57000000000002, 207.57000000000002]
        cCAT5 = [243.27, 208.59, 243.27]
    endif else begin
        cTS = lineplot.background_color
        cCAT1 = fill_color
        cCAT2 = lineplot.background_color
        cCAT3 = fill_color
        cCAT4 = lineplot.background_color
        cCAT5 = fill_color
    endelse
    objs.add, text(x, (ts+cat1)/2., vertical_alignment=0.5, "TS", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    objs.add, text(x, (cat1+cat2)/2., vertical_alignment=0.5, "CAT1", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    objs.add, text(x, (cat2+cat3)/2., vertical_alignment=0.5, "CAT2", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    objs.add, text(x, (cat3+cat4)/2., vertical_alignment=0.5, "CAT3", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    objs.add, text(x, (cat4+cat5)/2., vertical_alignment=0.5, "CAT4", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    objs.add, text(x, cat5+4., "CAT5", color=lineplot.background_color, /data, target=lineplot, font_size=9)
    xs = [xax.xrange[0], xax.xrange[1], xax.xrange[1], xax.xrange[0]]
    objs.add, polygon(xs, [0, 0, ts, ts], /data, $
      /fill_background, fill_color=cTD, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [ts, ts, cat1, cat1], /data, $
      /fill_background, fill_color=cTS, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [cat1, cat1, cat2, cat2], /data, $
      /fill_background, fill_color=cCAT1, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [cat2, cat2, cat3, cat3], /data, $
      /fill_background, fill_color=cCAT2, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [cat3, cat3, cat4, cat4], /data, $
      /fill_background, fill_color=cCAT3, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [cat4, cat4, cat5, cat5], /data, $
      /fill_background, fill_color=cCAT4, target=lineplot, linestyle='none')
    objs.add, polygon(xs, [cat5, cat5, 999, 999], /data, $
      /fill_background, fill_color=cCAT5, target=lineplot, linestyle='none')
    foreach obj, objs do obj.Order, /SEND_TO_BACK
  endif

  lineplot.Refresh
end

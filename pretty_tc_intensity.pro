pro pretty_TC_intensity, lineplot
  lineplot.Refresh, /disable
  axes = lineplot.axes
  xax = axes[0]
  xax2 = axes[2]
  xax2.hide = 1
  yax = axes[1]
  yax2 = axes[3]
  yax2.hide = 1
  xax.tickfont_size = 11.
  xax.tickformat='(C(CMoI,"/",CDI0))'
  xax.text_orientation=90
  yax.minor = 0
  yax2.minor = 0
  xax.subticklen=0.02
  xax.tickunits='Days'
  xax.tickinterval=1d
  xax.minor=3
  xax.ticklen=1
  yax.ticklen=1
  yax.thick=0
  xax.thick=0
  x = xax.xrange[0] + 0.03*(xax.xrange[1] - xax.xrange[0])
  objs = list()
  fill_color = "light blue"
  ts = 34.
  cat1 = 64.
  cat2 = 83.
  cat3 = 96.
  cat4 = 113.
  cat5 = 137.
  objs.add, text(x, (ts+cat1)/2., vertical_alignment=0.5, "TS", color=lineplot.background_color, /data, target=lineplot, font_size=9)
  objs.add, text(x, (cat1+cat2)/2., vertical_alignment=0.5, "CAT1", color=fill_color, /data, target=lineplot, font_size=9)
  objs.add, text(x, (cat2+cat3)/2., vertical_alignment=0.5, "CAT2", color=lineplot.background_color, /data, target=lineplot, font_size=9)
  objs.add, text(x, (cat3+cat4)/2., vertical_alignment=0.5, "CAT3", color=fill_color, /data, target=lineplot, font_size=9)
  objs.add, text(x, (cat4+cat5)/2., vertical_alignment=0.5, "CAT4", color=lineplot.background_color, /data, target=lineplot, font_size=9)
  objs.add, text(x, cat5+4., "CAT5", color=fill_color, /data, target=lineplot, font_size=9)
  xs = [xax.xrange[0], xax.xrange[1], xax.xrange[1], xax.xrange[0]]
  objs.add, polygon(xs, [ts, ts, cat1, cat1], /data, $
    /fill_background, fill_color=fill_color, target=lineplot, linestyle='none')
  objs.add, polygon(xs, [cat2, cat2, cat3, cat3], /data, $
    /fill_background, fill_color=fill_color, target=lineplot, linestyle='none')
  objs.add, polygon(xs, [cat4, cat4, cat5, cat5], /data, $
    /fill_background, fill_color=fill_color, target=lineplot, linestyle='none')
  foreach obj, objs do obj.Order, /SEND_TO_BACK
  lineplot.Refresh
end
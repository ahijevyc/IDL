pro geometric_geopotential_plot


h = findgen(30)
p = plot(h)
phi = plot(geometric_to_geopotential(h,45), /overplot)



end
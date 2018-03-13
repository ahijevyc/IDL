pro get_gfs_shear, pgi, date, hour, ushear, vshear, best_track, mtm00
  ushear = !VALUES.F_NAN
  vshear = !VALUES.F_NAN
  top = get_gfs_soundings(pgi,date,hour,20000.,best_track,mtm00,block_radius=block_radius,debug=debug,units='Pa')
  bot = get_gfs_soundings(pgi,date,hour,85000.,best_track,mtm00,block_radius=block_radius,debug=debug,units='Pa')
  
  ushears = top.udrops - bot.udrops
  vshears = top.vdrops - bot.vdrops
  radius = sqrt(top.xdrops^2. + top.ydrops^2.)
  
  iradius = where(radius lt 500., nradius)
  if nradius gt 0 then begin
    ushear = mean(ushears[iradius])
    vshear = mean(vshears[iradius])
  endif
end

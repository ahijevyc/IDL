pro mtp_locales, lon, lat, _extra=_extra
  oldpMULTI=!P.MULTI
  !P.MULTI=0
  map_set, /cylindrical, 0, 0, /noborder, charsize=1.3, $
  /isotropic, limit=[5, -87, 29, -37], /grid, /continent, e_continents={fill:13},e_grid={box:1}
  plots, lon, lat, psym=3, _extra=_extra, thick=2
  !P.MULTI=oldpmulti

end
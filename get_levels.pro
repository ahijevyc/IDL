function get_levels, units

  alts_mb = [1000, 975, 925., 850, 800, 700., 600., 500., 400., 300, 250., 200]
  alts_mb = -10*findgen(81) + 1000; every 10 mb
  alts_mb = [[975.,925.],alts_mb]
  alts_m = [0, 250, 500, 1000, 2000., 3000, 4000., 5000., 6000, 7000, 8000, 9000, 10000, 11000, 12000]
  alts_m = [10]
  alts_m = 250. * findgen(53)
;  dm = 5.
;  alts_m = 250. + dm * findgen((13000-150)/dm+1); attempt to kludge for drops? or mean GFS profile not defined near surface? 
  levels = units eq 'mb' ? alts_mb : alts_m
  if units eq 'CFAD_MTP' then levels = 6000. + 500*findgen(14000/500+1)
  if units eq 'CFAD_MTP' then levels = 6000. + 250*findgen(14000/250+1)
  if units eq 'MTP' then levels = 6000. + reverse(findgen(14001))
  return, levels
end


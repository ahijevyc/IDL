function moist_static_energy, TempK, DewptK, p_mb, Z_m
  atmos_const
  g  = !CONST.Gn
  Cp = !ATMOS.Cpd
  Lv = !ATMOS.LV0
  q = replicate(!VALUES.F_NAN,n_elements(z_m))
;  if p_mb lt 500 then q[*] = 0. ; don't calculate MSE below 500mb if dwpt is missing, otherwise just assume mixing ratio is negligible
  igood = where(finite(DewptK), ngood)
  if ngood gt 0 then q[igood] = mixr_sat(DewptK[igood], p_mb[igood]) ; mixing ratio g/kg
  
  mse = Cp*TempK + g*z_m + Lv*q

  return, mse/1000. ; kJ/kg
end

pro read_bamex_fils

  ;  for GEMPAK request from Chris Davis.
  ; see email attachment iop8_dropsonde.dat from Jul 6, 2017
  atmos_const
  infile = '~/iop8_dropsonde.dat'
  if ~ file_test(infile) then stop
  outfile = infile + '.out'
  close, /all
  openw, lun, outfile, /get_lun
  prefix = '/tmp/' + file_basename(infile)
  spawn, 'rm '+prefix+'*; csplit -z --prefix='+prefix + " " + infile + " '/STID=/' '{*}'", result
  files = file_search(prefix+'*', count=nfiles)
  foreach file, files, ifile do begin
    t = read_gempak_fil(file)
    if t eq !NULL then continue
    ;if ifile ne 18 then continue
    TK = t.t + !CONST.T0 ; convert C to K
    theta = TK * (1000./t.press)^(!ATMOS.Rd/!ATMOS.Cpd) ; potential temp
    Rkgkg = mixr_sat(t.td, t.press)/1000.
    thetae = theta * exp(!ATMOS.LV0*Rkgkg/!ATMOS.Cpd/TK) ; saturation equivalent potential temp
    ; Find level of lowest thetae imin
    junk =  min(thetae,imin)
    ; Find level of max thetae below imin (kpar)
    junk = max(thetae[0:imin],kpar,/nan)
    print, file,t.stid
    cape_sound, t.press, TK, Rkgkg, debug=2, $
      pout=pout, rout=rout, tout=tout, kpar=kpar, capep=capep, caper=caper, $
      tvpdif=tvpdif, tvrdif=tvrdif, parcel_params=parcel_params, entrainment_rate = 10., parcel_layer_mb=50.
    printf, lun, file, t.stid, max(capep, /nan), max(caper,/nan)
    for i=0,n_elements(pout)-1 do printf, lun, string(pout[i])+string(tvpdif[kpar,i])+string(tvrdif[kpar,i])

  endforeach
  free_lun, lun, exit_status = status
  if status ne 0 then stop
end
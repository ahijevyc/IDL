pro run_cape_sound
  files = file_search("/Users/ahijevyc/*apr05_deep_*.snd")
  foreach entrainment_rate, [0, 5, 10] do begin

    foreach parcel_layer_mb, [0, 50, 100] do begin
      kpar = 0 ; !NULL ; !NULL if you want most unstable
      foreach file, files do begin
        t = read_gempak_fil(file)
        psfile = file_basename(file, ".snd") + string(parcel_layer_mb, entrainment_rate, '(".parcel_layer",I0,"mb.",I02,"%")')+ ".ps"
        if !D.NAME eq 'PS' then device, filename=psfile, /color
        ;print, "making ", psfile
        cape_sound, t.press, t.t+!CONST.T0, mixr_sat(t.td, t.press)/1000., debug=1, kpar=kpar, parcel_layer_mb=parcel_layer_mb, capep=capep, parcel_params=parcel_params, entrainment_rate = entrainment_rate
        print, psfile, kpar, max(capep, /nan)
        xyouts, 0.5, 0.95, file_basename(psfile, ".ps") + "!Ccapep:" + string(max(capep, /nan)), /normal, align=0.5
        if !D.NAME eq 'PS' then device, /close
      endforeach
    endforeach
  endforeach

end
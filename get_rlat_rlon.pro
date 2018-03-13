pro get_rlat_rlon, neighbor_radius_m, gspacing, lat, lon, rlat, rlon

  rlon_exist = 0
  for ilat=-4,4 do for ilon=-5,5 do begin
    testlon = lon + ilon*gspacing
    testlat = lat + ilat*gspacing
    ; had to fix /Volumes/pecan2/ahijevyc/PREDICT/dropsondes/HRD/fil/D20100913_125025.fil
    ; at 740mb the lon and lat need to be fixed. 
    if map_2points( lon, lat, testlon, testlat, /meters) le neighbor_radius_m then begin
      if rlon_exist eq 0 then begin
        rlon = testlon
        rlat = testlat
        rlon_exist = 1
      endif else begin
        rlon = [rlon, testlon]
        rlat = [rlat, testlat]
      endelse
    endif
  endfor
  
end
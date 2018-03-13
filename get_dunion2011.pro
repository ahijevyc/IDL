function get_Dunion2011, height, type=type
  if ~keyword_set(type) then type = 'MT'
  ; assumes height is pressure in mb
  ;Dunion, Jason P., 2011: Rewriting the Climatology of the Tropical North Atlantic and Caribbean Sea Atmosphere. 
  ; J. Climate, 24, 893–908. (see Table 2, it really does not matter which one). 
  
  case type of
    'MT': itype=0 ; maritime tropical
    'SAL':itype=1 ; saharan air layer
    'MLDAI':itype=2 ; mid-latitude dry air intrusion
    else: message, 'unknown sounding type '+type+' can be MT SAL or MLDAI'
  endcase
  
  pressure_mb = [50,100,150,200,250,300,400,500,600,700,850,925,1000.]
  Z_m = [ [20726, 20733, 20696], [16590, 16593, 16572], [14203, 14196, 14182], [12418, 12413, 12395], [10949, 10947, 10930], [9690, 9693, 9676], [7596, 7604, 7590], [5887, 5897, 5883], [4437, 4451, 4430], [3178, 3190, 3173], [1541, 1553, 1542], [810, 823, 815], [124, 138, 133]]
  Z_m = Z_m[itype,*]
  Temperature_Dewpoint_C =[ $
    [[-63.0,-73.8],[ -63.0,-72.2],[ -63.2,-75.4]],$
    [[-74.5,-81.3],[ -73.7,-80.4],[ -75.2,-83.1]],$
    [[-67.2,-74.7],[ -66.8,-74.5],[ -66.6,-75.6]],$
    [[-54.3,-63.2],[ -54.7,-64.2],[ -54.5,-65.3]],$
    [[-42.3,-52.4],[ -43.1,-54.2],[ -43.1,-55.9]],$
    [[-32.3,-43.5],[ -33.1,-47.4],[ -33.3,-48.5]],$
    [[-17.1,-28.7],[ -17.5,-35.1],[ -17.8,-37.2]],$
    [[ -6.6,-16.9],[  -6.4,-26.4],[  -6.3,-28.5]],$
    [[  1.6, -7.1],[   1.5,-16.5],[   2.2,-18.9]],$
    [[  8.9,  2.5],[   9.4, -6.6],[   9.2, -9.5]],$
    [[ 17.6, 13.8],[  17.5, 11.3],[  16.4, 10.6]],$
    [[ 21.9, 19.0],[  21.6, 18.2],[  20.6, 17.1]],$
    [[ 26.5, 23.3],[  26.7, 22.7],[  25.8, 21.1]]]
  T_C = Temperature_Dewpoint_C[0,itype,*]
  Td_C = Temperature_Dewpoint_C[1,itype,*]
  
  rh = e_h2o(Td_C+!CONST.T0)/e_h2o(T_C+!CONST.T0)*100.
  w = rh2mixr(rh, height, T_C+!CONST.T0)/1000.
  Tv_C = (T_C+!CONST.T0) * (1.+0.61*w) - !CONST.T0   
  
  return, {T:interpol_NAN( T_C,pressure_mb,height), $
          Td:interpol_NAN(Td_C,pressure_mb,height), $
          Tv:interpol_NAN(Tv_C,pressure_mb,height), $
           Z:interpol_NAN( Z_m,pressure_mb,height)} 
end
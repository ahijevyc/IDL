function mpas_nearest_cell, inlons, inlats, mpas, start=start

  lonCell = mpas.lonCell
  iwrap = where(lonCell ge 180., /null)
  if iwrap ne !NULL then lonCell[iwrap] = lonCell[iwrap] - 360.
  latCell = mpas.latCell
  nEdgesOnCell = mpas.nEdgesOnCell
  cellsOnCell = mpas.cellsOnCell
  if ~keyword_set(start) then start = 0L
  debug = 0
  
  lons = inlons ; don't transpose lons! Transpose a copy. (The input becomes the output)!
  lats = inlats
  transposed = 0
  if n_elements(lons) gt 1 then begin
    tl = transpose(lons)
    ; see if transposing results in less distance between consequtive points.
    if mean(lons[*] - shift(lons[*],1), /nan) gt mean(tl[*] - shift(tl[*],1), /nan) then begin
      ; Since mcv_lons and mcv_lats arrays are tracks and the dimension along each track is the 2nd one, not the first, transpose
      ; so the points vary slowly when you use only 1 subscript.  You want mcv_lons[*] to vary slowly.  Remember to transpose the result!
      lons = transpose(lons)
      lats = transpose(lats)
      transposed = 1
    endif
  endif
  ; return a long integer variable with same dimensions as lons
  ; ensure same dimensions.
  iclosests = long(lons)
  ; and initialize to -1L
  iclosests[*] = -1L
  
  ifinites = where(finite(lons) and finite(lats), nfinite)

  if debug then begin
    map_set, /continents
    xyouts, lons[ifinites], lats[ifinites], strtrim(sindgen(nfinite),2), color=120, align=0.5, size=4
  endif

  
  for ifinite = 0, nfinite-1 do begin
    i = ifinites[ifinite]
    lon = lons[i]
    lat = lats[i]
    iclosest = ifinite eq 0 ? start : iclosest
    min_d = !VALUES.D_INFINITY
    min_d_neighbor = 0d
    while 1 do begin 
      if debug then begin
        print
        print, lon, lat, min_d/1000., format='("goal:",F8.2,"E,",F7.2,"N distance:",F10.3,"km")'
      endif
      neighbors = cellsOnCell[*, iclosest] - 1 ; something about cellOnCell being 1-based in Fortran while IDL is 0-based
      min_d_neighbor = !VALUES.D_INFINITY
      for iNeighbor = 0, nEdgesOnCell[iclosest]-1 do begin
        neighbor = neighbors[iNeighbor]
        d = map_2points(lon, lat, lonCell[neighbor], latCell[neighbor], /meters)
        if debug then print, iNeighbor, neighbor, lonCell[neighbor], latCell[neighbor], d/1000, $
           format='("neighbor",I3," (",I0," ",F8.2,"E,",F7.2,"N)",F10.3,"km")'
        if d lt min_d_neighbor then begin
          min_d_neighbor = d
          iclosest_neighbor = neighbor
        endif
      endfor
      if min_d_neighbor ge min_d then break
      iclosest = iclosest_neighbor
      min_d = min_d_neighbor
      if debug then print, iclosest, format='("cell ",I0," is closer")'
      if debug then begin
        plots, lonCell[iclosest], latCell[iclosest], /continue
        print, iclosest, lonCell[iclosest], latCell[iclosest], min_d/1000., format='("closest cell:",I0,F8.2,"E,",F7.2,"N",F10.3,"km")'
      endif

    endwhile
    iclosests[i] = iclosest
  endfor
  
  return, transposed ? transpose(iclosests) : iclosests
end


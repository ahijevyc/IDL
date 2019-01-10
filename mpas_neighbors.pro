function mpas_neighbors, neighbors, lonCell, latCell, nEdgesOnCell, cellsOnCell, range=range, $
  interior=interior, lon0=lon0, lat0=lat0
  ; 20181113 - added lon0 and lat0 keywords to use as origin when computing distance
  ; before I used the nearest model node
  forward_function mpas_neighbors

  ; return neighboring cell indicies within range of center (lon0, lat0) or a surrounding array of MPAS cells.
  ;
  ; runs recursively. first it runs on one cell, then it runs on each neighbor, then
  ; it runs on each neighbor of each neighbor, and so on, until it expands outward from the original
  ; cell so far that none of the added neighbors are within range of the original lon0,lat0.


  if n_elements(neighbors) eq 1 then neighbors = {iCell:neighbors[0], range:0d, az:0d} ; store iCell, range, and azimuth in structure
  ; Quadrants are defined as NE (0-90), SE (90-180), SW (180-270), and NW (270-0).
  ; so index 0 is NE, index 1 is SE, index 2 is SW, and index 3 is NW
  ; iquadrant is the imaginary component of neighbors, and has integers 0-3.
  ; Ellipsoid [CLARKE 1866]  Semi-Major Axis (Equatorial Radius)
  a = 6378.2064d
  ;
  ;

  ; range is in km - used to be meters before jan 20 when I added azimuth (to get quadrant).
  if ~keyword_set(range) then range=0
  ; neighbors are added in layers starting from the inside and moving out.
  ; Each neighbor is in turn searched for neighbors.  But you don't have to do search for neighbors of cells in the interior layers.
  ; This parameter helps speed things up. These interior cells don't need to be searched for neighbors again.
  if ~keyword_set(interior) then interior = 0
  nin = n_elements(neighbors)

  n = !NULL
  ; Get neighbors of neighbors. (note the interior parameter can speed things up here).
  for i=interior,nin-1 do n = [n, cellsOnCell[0:nEdgesOnCell[neighbors[i].iCell]-1, neighbors[i].iCell]-1]
  ; Get a unique list of neighbors of neighbors.
  n = n[uniq(n, sort(n))]
  ; see if each neighbor of neighbors has been added already or is out of range
  for i=0,n_elements(n)-1 do begin
    ; if this element of n is already a member of neighbors then total(neighbors eq n[i]) will be greater than zero.
    if total(neighbors.iCell eq n[i]) gt 0 then continue
    ; Get distance and azimuth from center cell to neighbor candidate
    ; Tried to add dpath keyword to specify maximum angular distance b/t points. but has to do with path points
    result = map_2points(lon0, lat0, lonCell[n[i]], latCell[n[i]])
    range_km = result[0]*!DTOR*a
    if range_km le range then begin
      neighbors = [neighbors, {iCell:n[i], range:range_km, az:result[1]}]
    endif
  endfor ; candidate neighbor array n

  ; if nothing has been added, then we are done. Return the neighbors array as is.
  if n_elements(neighbors) eq nin then return, neighbors; nothing was added so we are done.


  ; Since we reached here, we are still searching for neighbors of neighbors within range. Continue with another
  ; round of processing with the latest array of neighbors.  You can speed things up by setting interior to
  ; nin, the number of cells that entered this iteration.  This works because a) these cells come first in the neighbors array
  ; and b) they are in the interior, hence, their neighbors have already been added.
  return, mpas_neighbors(neighbors, lonCell, latCell, nEdgesOnCell, cellsOnCell, range=range, $
    interior=nin, lon0=lon0, lat0=lat0)

end


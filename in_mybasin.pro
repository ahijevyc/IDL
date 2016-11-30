function in_mybasin, inlon, lat, region, px=px, py=py
  lon = inlon
  ; ensure -180 to 180 longitude
  iwrap = where(lon gt 180,nwrap)
  if nwrap gt 0 then lon[iwrap]=lon[iwrap]-360

  ; this function is similar to atcf_basin() but this one returns the array
  ; indices of MPAS variables within a basin
  ; and atcf_basin() takes a lat/lon pair and returns the basin
  
  
  ; define region of interest with px, py vectors.
  
  px = [-180, 180, 180, -180, -180]
  py = [45., 45., 0, 0, 45] ; latitudes of box
  ; longitudes of box
  if strmatch(region, 'wp*', /fold) then px = [110, 170, 170, 110., 110]
  if strmatch(region, 'al*', /fold) then px = [-90., -30., -30., -90, -90]

  
  ; new latitudes of box for tropics and extratropics
  if strmatch(region, '*tropics', /fold) then      py = [23.5, 23.5, 0, 0, 23.5]
  if strmatch(region, '*extratropics', /fold) then py = [45., 45., 23.5, 23.5, 45.]
  if strmatch(region, '*_NH', /fold) then py = [80,80,20,20,80]
  
  ; if global, return indices of every finite element
  if strmatch(region, 'global', /fold) then return, where(finite(lon))
  
  ; Special case for EP
  if  strmatch(region, 'ep*', /fold) then begin
    ; created in Google Earth, saved to .kml, added comma.  3rd coordinate is elevation.
    google_earth = [ -180,0,0, -80,0,0, -80,7,0, -105.,23.5,0, -120,45,0, -180,45,0, -180.,0,0]
    if strmatch(region, '*_tropics', /fold) then begin
      google_earth = [ -180,0,0, -80,0,0, -80,7,0, -105.,23.5,0, -180,23.5,0, -180.,0,0]
    endif
    if strmatch(region, '*_extratropics', /fold) then begin
      google_earth = [ -180,23.5,0, -105.,23.5,0, -130,45,0, -180,45,0, -180.,23.5,0]
    endif
    if strmatch(region, '*_test', /fold) then begin
      google_earth = [ -150,5,0, -120,5,0, -120.,12.5,0, -150,12.5,0, -150.,5,0]
    endif
    px = google_earth[0:*:3] ; every 3rd index starting with index 0
    py = google_earth[1:*:3] ; every 3rd index starting with index 1    
  endif

  ; debug stuff. put after defining tropics and extra-tropics.
  if strmatch(region, 'debug*') then begin
    px = [-180,-179,-179,-180,-180]
    py = [50,50,49,49,50]
  endif


  
  ; Thanks to Andy Loughe! (found his ROI code on google groups)
  roi = Obj_New('IDLanROI', px, py)
  result = roi->ContainsPoints(lon,lat)
  Obj_Destroy, roi
  ; exterior = where(result eq 0, /null)
  ; interior = where(result eq 1, /null)
  ; edge = where(result eq 2, /null)
  ; vertex = where(result eq 3, /null)
  return, where(result gt 0, /null) ; either inside, on edge, or on vertex.
  
end

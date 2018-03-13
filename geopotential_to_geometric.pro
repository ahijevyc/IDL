function geopotential_to_geometric, geopotential_in, latitude_in

  ;This function takes in a given geopotential altitude (m) and latitude
  ;(degrees N) and returns a geometric altitude (m). 
  ;   Edited Kelly Schick, May 2013
  ; Put in IDL by Dave Ahijevych may 2013
  ; Change input and output from kilometers to meters

  geopotentials = [geopotential_in]/1000.
  latitudes = [latitude_in]
  geometrics = replicate(!VALUES.F_NAN, n_elements(geopotentials))
  
  rpd=!DTOR ; 1.74532925199433E-2 ; %constant of radians per degree
  
  for i = 0, n_elements(geopotentials)-1 do begin
    geopotential = geopotentials[i]
    latitude = latitudes[i]
    
    if (geopotential gt 100.) then begin ;sanity check here to make sure data is in km, not m
      message, string(geopotential, format='("The altitude you entered appears to be in meters.", F0)')
    end
    ;First, establish the vertical variation in normal gravity
    
    cos2l=cos(2 * rpd * latitude);
    cos4l=cos(4 * rpd * latitude);
    
    dgdz= -( (3.085462e-6) + (2.27e-9)*cos2l - (2e-12)*cos4l) ;
    
    ;this value will be used in later calculations
    
    ;Next, we establish the normal gravity
    ;cos2l is defined above
    
    gn=9.80616 * (1 - 0.0026373*cos2l + 0.0000059*(cos2l^2));
    
    ;Next we calculate the Ad Hoc radius
    
    R = -2* gn / dgdz / 1000;  %not sure about the 1000 here, but keeping from source code
    
    
    ;Finally we convert geopotential altitude into geometric
    
    geometric= R * geopotential/ (gn *(R/9.80665)-geopotential);
    geometrics[i] = geometric
  endfor
  return, geometrics*1000.
  
end

pro p2m
  loadct, 39, /silent
  if !D.NAME eq 'X' then begin
    device, decomposed=0
    !P.CHARSIZE = 2
    !P.THICK=2
  endif
  files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/dropsondes/D*_PQC.eol', count=nfiles)
  xtitle = 'geopotential height (km)'
  ; to get function divide_by_1000 .r cfad_mtp
  plot, [0,1], xrange=[-10,13900], xstyle=1, /nodata, yrange=[-60,60], xtickformat='divide_by_1000', xtitle = xtitle, $
    ytitle='meters'
  oplot, !X.CRANGE, [0,0]
  
  ; mean profile
  evenspacing = findgen(2800)*5
  ndz = replicate(0L, n_elements(evenspacing))
  sumdz = replicate(0., n_elements(evenspacing))
  maxdz = 1000
  
  for ifile = 0, nfiles-1 do begin
    closest_file = files[ifile]
    
    if strmatch(closest_file, '*.eol') then t = read_eol_sounding(closest_file)
    Time_secs = replace_wnan(t.Time_sec, -999)
    Ts        = replace_wnan(t.T, -999)
    GeoPot    = replace_wnan(t.GeoPoAlt_m, -999)
    Tds       = replace_wnan(t.Td, -999)
    Presss    = replace_wnan(t.Press, -999)
    Us        = replace_wnan(t.U, -999)
    Vs        = replace_wnan(t.V, -999)
    lons      = replace_wnan(t.lon, -999)
    lats      = replace_wnan(t.lat, -999)
    Geometric = replace_wnan(t.GPSAlt, -999)
    
    n = n_elements(lats)
    for k=0,n-1 do GeoPot[k] = geopotential_to_geometric(GeoPot[k], lats[k])
    color = 255./nfiles * ifile
    
    ;        plot, GeoPot, Geometric, xrange=[-1,14000], yrange=[-1,14000], /iso, xstyle=1, ystyle=1, /nodata, xtickformat='divide_by_1000', $
    ;        ytickformat='divide_by_1000', xtitle=xtitle, ytitle='GPS geometric height (km)'
    ;        oplot, !X.CRANGE, !X.CRANGE
    ;        oplot, GeoPot, Geometric, psym=3, color=color
    
    dz = Geometric-GeoPot
    igood = where(finite(Geometric) and finite(GeoPot), ngood)
    oplot, GeoPot, dz, psym=3, color=color
    
    igood = where(abs(dz) lt maxdz, ngood)
    if ngood eq 0 then continue
    bias = mean(dz[igood], /nan)
    
    ; histogram
    
    if finite(bias) then begin
      binsize=5
      mm = 65 ; minmax
      if n_elements(h) eq 0 then h = histogram([bias], binsize=binsize, locations = xval, min=-mm, max=mm) else $
        h = h + histogram([bias], binsize=binsize, min=-mm, max=mm)
    endif
    plot, xval, h, psym=10, xtitle='meters', xrange=[-mm,mm], xstyle=1
    ; mean profile
    dz = interpol_NAN(dz,GeoPot,evenspacing,max_gap=50)
    good = finite(dz) and abs(dz lt maxdz)
    igood = where(good, ngood)
    if ngood eq 0 then continue
    
    ndz = ndz + good
    
    sumdz[igood] = sumdz[igood] + dz[igood]
    if max(abs(dz),/nan,imax) gt maxdz then print, closest_file, evenspacing[imax], dz[imax]
  ;      idebug=1083
  ;      print, dz[idebug], sumdz[idebug], ndz[idebug], sumdz[idebug]/ndz[idebug]
  ;      oplot, GeoPot[igood], Geometric[igood]-GeoPot[igood], color=color
    
  endfor
;  igood = where(ndz gt 0)
;  oplot, evenspacing, sumdz[igood]/ndz[igood]
end
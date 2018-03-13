function geometric_to_geopotential, geometric_in, latitude_in

  ;This function takes in a given geometric altitude (m) and latitude
  ;(degrees N) and returns a geopotential altitude (m).
  ; copied from geopotential_to_geometric May 2014
  ; by Dave Ahijevych
  ;
  ; INPUT: geopotential height in meters
  ; OUTPUT: geometric height in meters
  ;
  ; Internally, use kilometers

  geometrics = [geometric_in]/1000.
  latitudes = [latitude_in]
  if n_elements(latitudes) eq 1 then latitudes = replicate(latitudes, n_elements(geometrics))
  geopotentials = replicate(!VALUES.F_NAN, n_elements(geometrics))

  rpd=!DTOR ; 1.74532925199433E-2 ; %constant of radians per degree

  for i = 0, n_elements(geometrics)-1 do begin
    geometric = geometrics[i]
    latitude = latitudes[i]

    if (geometric gt 100.) then begin ;sanity check here to make sure data is in km, not m
      message, string(geometric, format='("The altitude you entered appears to be in meters.", F0)')
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


    ;Finally we convert geometric altitude into geopotential

    geopotential= R * geometric * gn /9.80665/(R+geometric);

    geopotentials[i] = geopotential
  endfor
  return, geopotentials*1000.

end




function compute_geopotential_height, H, lat

  ; from SoYoung in Jan 24, 2017 email and converted to IDL from fortran
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;!
  ;!    compute_geopotential_height
  ;!    subroutine converts geometric height to geopotential height
  ;!
  ;!    Input:
  ;!    H   -- input real value geometric height [m]
  ;!    lat -- latitude in degree
  ;!
  ;!    Output:
  ;!    Z -- output real value geopotential height [m]
  ;!
  ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ;  Parameters below from WGS-84 model software inside GPS receivers.
  semi_major_axis = 6378.1370d3    ; (m)
  semi_minor_axis = 6356.7523142d3 ; (m)
  grav_polar = 9.8321849378        ; (m/s2)
  grav_equator = 9.7803253359      ; (m/s2)
  earth_omega = 7.292115d-5        ; (rad/s)
  grav_constant = 3.986004418d14   ; (m3/s2)
  grav = 9.80665d0                 ; (m/s2) WMO std g at 45 deg lat
  eccentricity = 0.081819d0        ; unitless
  pi2 = 3.14159265358979d0/180.d0

  ;  Derived geophysical constants
  flattening = (semi_major_axis-semi_minor_axis) / semi_major_axis

  somigliana = (semi_minor_axis/semi_major_axis)*(grav_polar/grav_equator)-1.d0

  grav_ratio = (earth_omega*earth_omega * $
    semi_major_axis*semi_major_axis * semi_minor_axis)/grav_constant

  ;  Sanity Check
  if lat gt 90 or lat lt -90 then begin
    print, 'compute_geopotential_height: Latitude is not between -90 and 90 degrees: ',lat
    stop
  endif

  latr = lat * (pi2)        ; in radians
  sin2  = sin(latr) * sin(latr)
  termg = grav_equator * ( (1.d0+somigliana*sin2) / $
    sqrt(1.d0-eccentricity*eccentricity*sin2) )
  termr = semi_major_axis / (1.d0 + flattening + grav_ratio - 2.d0*flattening*sin2)

  compute_geopotential_height = (termg/grav)*((termr*H)/(termr+H))
  return, compute_geopotential_height

end;  function compute_geopotential_height


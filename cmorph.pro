function cmorph, date, time, type



  ;ftp://ftp.cpc.ncep.noaa.gov/precip/global_CMORPH/README.cmorph.025deg_3-hourly

  ;The data are compressed using the standard Unix compress function (files have a
  ;suffix of ".Z"). Each file is composed of 16 direct access binary
  ;("big_endian") records that are defined as follows:
  ;
  ;Record 1: contains the merged microwave precipitation only for 00 UTC
  ;Record 2: contains the "CMORPH" precipitation estimates for 00 UTC
  ;
  ;Record 3: contains the merged microwave precipitation only for 03 UTC
  ;Record 4: contains the "CMORPH" precipitation estimates for 03 UTC
  ;.
  ;.
  ;.
  ;Record 15: contains the merged microwave precipitation only for 21 UTC
  ;Record 16: contains the "CMORPH" precipitation estimates for 21 UTC
  ;
  ;All units are "mm/hr".  Missing data are denoted by values of "-9999."
  ;Each record contains a 1440 x 480 REAL*4 array of data which is oriented
  ;from 0.125E EASTward and from 59.875N SOUTHward, with a grid increment
  ;of 0.25 degrees of latitude and longitude.  Thus, the grid locations
  ;are the centers od a 0.25 degree lat/lon grid box.  Note that these
  ;estimates represent spatial averages, so the data are grid-centered,
  ;rather than lattice-centered.
  ;
  ;For example (1,1) is 0.125E, 59.875N
  ;            (2,2) is 0.375E, 59.625N, etc.
  ;
  ;There is an associated GrADS ctl file with this data set:
  ;CMORPH+MWCOMB_025deg-3hr.ctl
  ;

  IRMAX=1440
  JRMAX=480

  basedir = '/Volumes/pecan2/ahijevyc/'
  dir = basedir+'PREDICT/CMORPH/'
  filename = dir + date + '_'+type+'-025deg_cpc+comb'
  if type eq '3hr' then nrecords = 16
  if type eq 'dly' then nrecords = 2 
  data = read_binary(filename, data_type=4, data_dims=[irmax,jrmax,nrecords], endian='big')
  if time ne '000000' and time ne '030000' and time ne '060000' and time ne '090000' and $
    time ne '120000' and time ne '150000' and time ne '180000' and time ne '210000' then begin
    print, 'cmorph: invalid time "'+time+'"'
    print, 'time must be a multiple of 3 hours'
    stop
  endif
  data = data[*,*,time/030000L * 2 + 1]
  ; replace missing data with NaN (-9999 is missing in CMORPH data, as mentioned above)
  imsg = where(data eq -9999., nmsg)
  if nmsg gt 0 then data[imsg] = !VALUES.F_NAN
  lon = findgen(irmax)*0.25 + 0.125
  iwrap =where(lon gt 180) 
  lon[iwrap] = lon[iwrap] - 360.
  lat = findgen(jrmax)*(-0.25) + 59.875
  return, {data:data, lon:lon, lat:lat, date:date, time:time}
  
end
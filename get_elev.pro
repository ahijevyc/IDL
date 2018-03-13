FUNCTION get_elev,lat,lon

  ; Returns sfc elevation, given lat lon arrays.
  
  if (MIN(lat,/NAN) lt -10 || MAX(lat,/NAN) gt 90 || MIN(lon,/NAN) lt -140 || MAX(lon,/NAN) ge -60 ) THEN BEGIN
    HELP, lat, lon
    MESSAGE, 'out of bounds'
  ENDIF
  
  good = WHERE(FINITE(lat) and FINITE(lon),/null)
  
  if good eq !NULL then message, 'no good lat/lons'
  
  dempath = '/Volumes/pecan2/ahijevyc/DEM/'
  nrows = 6000L
  ncols = 4800L
  temp = { VERSION   :  1.00000,$
    TEMPLATENAME    : '',$
    ENDIAN          : 'big',$
    FIELDCOUNT      : 1,$
    TYPECODES       : 2,$
    NAMES           : 'e',$
    OFFSETS         : '>0',$
    NUMDIMS         : 2,$
    DIMENSIONS      : STRARR(1, 8),$
    REVERSEFLAGS    : BYTARR(1, 8),$
    ABSOLUTEFLAGS   : 0B,$
    RETURNFLAGS     : [1],$
    VERIFYFLAGS     : [0],$
    DIMALLOWFORMULAS: 1,$
    OFFSETALLOWFORMULAS: 1,$
    VERIFYVALS      : ''$
  }
  temp.dimensions[0,[0,1]] = [ncols,nrows]
  
  e = INTARR(9600,12000)
  r = read_binary(dempath+'W140N90.DEM',template=temp)
  e[0:4799,0:5999] = r.e
  r = read_binary(dempath+'W100N90.DEM',template=temp)
  e[4800:9599,0:5999] = r.e
  r = read_binary(dempath+'W140N40.DEM',template=temp)
  e[0:4799,6000:11999] = r.e
  r = read_binary(dempath+'W100N40.DEM',template=temp)
  e[4800:9599,6000:11999] = r.e
  
  
  ULXMAP = -139.99583333333334d
  ULYMAP = 89.99583333333334d
  
  XDIM   = 0.00833333333333d
  YDIM   = 0.00833333333333d
  
  i = (lon[good] - ULXMAP) / XDIM
  j = (ULYMAP - lat[good]) / YDIM
  i2 = ROUND(i)
  j2 = ROUND(j)
  ;i3 = MIN([i2,2*ncols-1])
  ;j3 = MIN([j2,2*nrows-1])
  
  
  ;help, lat,lon,e, file,i,i2,j,j2
  e1 = lon & e1[*] = !VALUES.F_NAN
  e1[good] = e[i2,j2]
  return, e1
  
END

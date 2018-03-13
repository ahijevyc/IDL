pro trmm_read
  basedir = '/mmmtmp/ahijevyc/'
  filename = basedir+'3B31.20130801.7.HDF'
  if !D.NAME eq 'X' then device, decomposed=0
  if !D.NAME eq 'PS' then device, /color, /close, bits=8
  
  sd_id=HDF_SD_START(filename)
  HDF_SD_FILEINFO,sd_id,NumSDS,attributes
  
  ; Find out about the first SDS
  ; use index 0 or 6?
  sds_id=HDF_SD_SELECT(sd_id,0)
  HDF_SD_GETINFO,sds_id,NDIMS=NDIMS,LABEL=LABEL,DIMS=DIMS,TYPE=TYPE,$
    coordsys=coordsys, format=format, hdf_type=hdf_type, name=name, natts=natts, $
    unit=unit, /noreverse
  HDF_SD_GETDATA,sds_id,r
  r = transpose(r)
  z = r
  
  ;
  ; Close down the HDF file
  ;
  HDF_SD_ENDACCESS,sds_id
  HDF_SD_END,sd_id
  ;
;  help,NDIMS,LABEL,DIMS,TYPE,coordsys,format,hdf_type,name, natts,unit
  loadct,39,/silent
  ; fill values taken care of with min_value in map_image function
  fill = -9999.90
  ifill = where(r eq fill, nfill)
  if nfill gt 0 then r[ifill] = !VALUES.F_NAN
  
  tvlct, oldct, /get
  white=transpose([255,255,255])
  ltblue=transpose([100,100,254])
  blue = transpose([0,0,140])
  aqua = transpose([0,184,184])
  green = transpose([0,120,0])
  yellow = transpose([254,254,0])
  orange = transpose([254,160,0])
  red = transpose([254,0,0])
  tvlct, [white,ltblue,blue,aqua,green,yellow,orange,red], 2
  i = where(r le 0, n)
  if n gt 0 then z[i] = 2
  i = where(r gt 0 and r lt 50, n)
  if n gt 0 then z[i] = 3
  i = where(r ge 50 and r lt 100, n)
  if n gt 0 then z[i] = 4
  i = where(r ge 100 and r lt 200, n)
  if n gt 0 then z[i] = 5
  i = where(r ge 200 and r lt 300, n)
  if n gt 0 then z[i] = 6
  i = where(r ge 300 and r lt 400, n)
  if n gt 0 then z[i] = 7
  i = where(r ge 400 and r lt 500, n)
  if n gt 0 then z[i] = 8
  i = where(r ge 500, n)
  if n gt 0 then z[i] = 9
  
  
  latmin=-40 & latmax=40 & lonmin=-180 & lonmax=180
  map_set, 0,0, limit=[latmin,lonmin,latmax,lonmax], /isotropic, /noborder, title=name
  result = map_image(z, startx, starty, xsize, ysize, latmin=latmin, latmax=latmax, lonmin=lonmin, $
    lonmax=lonmax)
  TV, z, startx, starty, xsize=xsize, ysize=ysize, /nan
  tvlct, oldct
  dlat=0.5
  lats = dlat*findgen(160)+latmin+dlat/2.
  areaCell = replicate(1,720)#cos(!DTOR*lats)
  areaCell[ifill] = !VALUES.F_NAN
  ; set 35-40deg N and S to missing. (matched paper by so and so). 
  areaCell[*,0:9] = !VALUES.F_NAN
  areaCell[*,150:159] = !VALUES.F_NAN
  wr = total(r*areaCell,/nan)/total(areaCell,/nan)
  map_grid, londel=10, linestyle=0, color=!P.BACKGROUND
  map_continents, /coast, color=!P.BACKGROUND
  map_grid, /box, /nogrid, londel=30, latdel=20
  if !D.NAME eq 'PS' then device, /close
  stop
end
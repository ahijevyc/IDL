pro read_stageIV

; Read and plot Stage IV analysis from NCEP/CPC
; After converting to netCDF with ncl_convert2nc ST4*.grb
; Add a .grb suffix to the input GRiB files before using ncl_convert2nc.  


if (!D.NAME eq 'PS') then device, /color, /close
loadct, 39
levels=[1,2.5,5,7.5,10,15,20,40]
colors = mycolors(levels)
pos = [0.25,0.15,0.75,0.9]
ifiles = FILE_SEARCH('/pecan/ahijevyc/trier/ST4/ST4.*.nc', count = nfiles) 


pcp_mean = fltarr(nfiles)
pcp_pctcov = fltarr(nfiles)
jday = dblarr(nfiles)



for ifile = 0, nfiles-1 do begin
  infile = ifiles[ifile]
  ncid = NCDF_OPEN(infile)            ; Open The NetCDF file


  NCDF_VARGET, ncid,  'g5_lat_0', lat    
  NCDF_VARGET, ncid,  'g5_lon_1', lon
  NCDF_VARGET, ncid,  'A_PCP_GDS5_SFC_acc1h' , pcp
  ncdf_attget, ncid, 'A_PCP_GDS5_SFC_acc1h', '_FillValue', FillValue



  NCDF_CLOSE, ncid      ; Close the NetCDF file
  
  
  
  
  ; Change the Fill values to NaN's.
  iFillValue = where(pcp eq FillValue, nFillValue)
  if (nFillValue gt 0) then pcp[iFillValue] = !VALUES.F_NAN
  
  limit = [33, -103, 41, -94]
  ibox = where(lat gt 33.65 and lat lt 40.65 and lon gt -102.5 and lon lt -94.3)
  ; Plot every 12th time.
  if ifile mod 12 eq 0 then begin
    map_set, 45, -98,  /grid, limit=limit, position = pos, /noborder, /lamber, title = infile, /usa
    
    x1 = 300 & x2 = 700
    y1 = 200 & y2 = 550
    d = 2  ; Skip every d points when contouring. Using every one can take minutes.
    z = pcp[x1:x2:d, y1:y2:d]
    x = lon[x1:x2:d, y1:y2:d]
    y = lat[x1:x2:d, y1:y2:d]
     
    contour, z, x, y, /irr, /overplot, /cell_fill,  levels= levels, c_colors=colors
    contourbar, levels, colors
    plots, [-94.3, -102.5, -102.5, -94.3, -94.3], [33.65, 33.65, 40.65, 40.65, 33.65], color=colors[n_elements(colors)-1]
  endif
  ; Find non-missing pcp values.
  good_pcp = (pcp[ibox])[where(finite(pcp[ibox]))]
  pcp_mean[ifile] = mean(good_pcp)
  coverage = where(good_pcp ge 1, ncov)
  pcp_pctcov[ifile] = 100. * float(ncov) / n_elements(good_pcp) 
  year = strmid(file_basename(infile), 4, 4)
  mon = strmid(file_basename(infile), 8, 2)
  day = strmid(file_basename(infile), 10, 2)
  hour = strmid(file_basename(infile), 12, 2)
  jday[ifile] = julday( mon, day, year, hour, 0, 0)
  ibad = where(finite(pcp[ibox]) eq 0, nbad)
  if nbad gt 0 then plots, (lon[ibox])[ibad], (lat[ibox])[ibad], psym=3, color=colors[n_elements(colors)-1]
  print, '.', format='(A,$)'
endfor

; Plot time series of hourly precip and % coverage.
plot, jday, pcp_mean, xtickunits = ['Days'], xticklen=1., xtickinterval=1, title = 'hourly average precip in mm', xminor=4
plot, jday, pcp_pctcov, xtickunits = ['Days'], xticklen=1., xtickinterval=1, title = 'hourly % coverage of 1 mm or greater', xminor=4 

if (!D.NAME eq 'PS') then device, /close

; Print data value after time index for each time.
for ifile = 0, nfiles-1 do print, ifile+1, pcp_mean[ifile]
for ifile = 0, nfiles-1 do print, ifile+1, pcp_pctcov[ifile]

end

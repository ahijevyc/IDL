; Filename:  read_qscat3.pro
;
; Usage:
;
;   To run this programs, use the following command:
;
;   IDL> read_qscat3
;
; Description:
;  
;   This file contains one (1) IDL procedure and three (3) functions
;   used to read the QuikSCAT Level 3 data in Hierarchical Data 
;   Format (HDF).  The routines are as follows.
;   
;   1. correct_uint16: a function to correct variables 
;                      stored as 16-bit unsigned integers.
;
;   2. correct_uint32: a function to correct variables 
;                      stored as 32-bit unsigned integers.
;
;   3. get_sds: a function to read the data which are 
;               stored as Scientific Data Sets.
;
;   4. read_qscat3: the main procedure. Calls get_vdata and 
;                    get_sds.
;
; Notes:
;
; 1. The directory on your local system which contains the QuikSCAT
;    L3 data must be input in program read_qscat3.pro.  (Search on
;    "data_dir" to find this line quickly.)
;
; 2. The L3 data are read in their entirety.  If you would like
;    to read these data in slabs, change the value of "slab_size".
;
; 3. The correction of Unsigned Integers is currently hard-coded for
;    versions of IDL prior to 5.2.
;
; 4. Please send all comments and questions concerning these routines
;    to qscat@podaac.jpl.nasa.gov.
;
;
; 10/4/99 K.L. Perry, C.S. Hsu, R.S. Dunbar
; 12/10/03 T. Lungu 
; Revisions:
;
; 5/5/2000 Updated code to read asc_time_frac, des_time_frac,
;          asc_rain_prob, des_rain_prob, asc_rain_flag, and
;          des_rain_flag.  K.L. Perry
; 12/10/03 Replaced pickfile by dialog_pickfile, since the former is
;          no longer supported by IDL 6.0.
;==================================================================
;   Copyright (c) 1999, California Institute of Technology
;==================================================================

;==================================================================
; correct_uint16: a function to correct data which are stored
;                 as 16-bit unsigned integers for versions of
;                 IDL prior to 5.2
;==================================================================
function correct_uint16,sds_data
    w=where(sds_data lt 0,cnt)
    if (cnt gt 0) then begin
      sds_data(w)=sds_data(w)+65536.
      w2=where(sds_data lt 0,cnt2)
    endif
return,sds_data
end

;==================================================================
; correct_uint32: a function to correct data which are stored
;                 as 32-bit unsigned integers for versions of
;                 IDL prior to 5.2
;==================================================================
function correct_uint32,sds_data
    w=where(sds_data lt 0,cnt)
    if (cnt gt 0) then begin
      sds_data(w)=sds_data(w)+4294967296.
      w2=where(sds_data lt 0,cnt2)
    endif
return,sds_data
end

;==================================================================
; get_sds: a function to read the data which are stored
;          as Scientific Data Sets.  This function also
;          multiplies the data by the calibration and 
;          subtracts the offset.
;==================================================================
function get_sds,sd_id,sds_name,slab_start,slab_size

    index=HDF_SD_NAMETOINDEX(sd_id,sds_name)
    sds_id=HDF_SD_SELECT(sd_id,index)
    HDF_SD_GETINFO,sds_id,ndims=ndims,dims=dims,caldata=cal,type=data_type

; make sure edge, start and stride have correct array sizes
    edge=intarr(ndims)
    start=intarr(ndims)
    stride=intarr(ndims)

    for i=0,ndims-1 do begin
      edge(i)=dims(i)
      start(i)=0
      stride(i)=1
    endfor
    edge(ndims-1)=slab_size
    start(ndims-1)=slab_start

    HDF_SD_GETDATA,sds_id,data,stride=stride,start=start,count=edge

;Correct Unsigned Integers
;;  note: Versions of IDL prior to 5.2 do not handle unsigned 
;;        integers properly.  These versions also do not identify 
;;        DFNT_UINT's using HDF_SD_GETINFO.  Therefore, the 
;;        following hard code will be included until IDL 5.2 is 
;;        "standard". --KLP.
;;

;; UINT16
    if ((sds_name eq "asc_avg_wind_speed") or $
        (sds_name eq "des_avg_wind_speed") or $
	(sds_name eq "asc_time_frac") or $
	(sds_name eq "des_time_frac") or $
	(sds_name eq "asc_rain_prob") or $
	(sds_name eq "des_rain_prob")) then begin
      data=correct_uint16(float(data))
    endif

;; UINT32
    if ((sds_name eq "asc_avg_wind_speed_sq") or $
        (sds_name eq "des_avg_wind_speed_sq")) then begin
      data=correct_uint16(float(data))
    endif

; Apply the scale and offset
    rdata = data*cal.cal - cal.offset
    HDF_SD_ENDACCESS,sds_id

return,rdata
end


;==================================================================
; read_qscat3: the main procedure in this file.  Calls are made 
;              to get_sds to read the data.  The results are then 
;              printed to the screen.
;==================================================================
pro read_qscat3

;Select a Level 3 file
;***** Change data_dir to suit your local system
  data_dir="/Volumes/pecan2/ahijevyc/cdavis/ascat/"
  filename=dialog_pickfile(filter='QS_XWGRD3*', PATH = data_dir, /READ)
  print,' '
  print,'FILENAME: ',filename
  print,' '

;Read the Scientific Data Sets
  sd_id=HDF_SD_START(filename,/READ)

;Get the x-dimension in order to read an entire SDS
  index=HDF_SD_NAMETOINDEX(sd_id,'asc_wvc_count')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                 type=data_type,caldata=cal
  slab_size=strtrim(dims(1),2)   
  ir=0

; Read the SDSs
  asc_avg_wind_speed= get_sds(sd_id,'asc_avg_wind_speed',ir,slab_size)
  des_avg_wind_speed= get_sds(sd_id,'des_avg_wind_speed',ir,slab_size)
  asc_avg_wind_vel_u= get_sds(sd_id,'asc_avg_wind_vel_u',ir,slab_size)
  des_avg_wind_vel_u= get_sds(sd_id,'des_avg_wind_vel_u',ir,slab_size)
  asc_avg_wind_vel_v= get_sds(sd_id,'asc_avg_wind_vel_v',ir,slab_size)
  des_avg_wind_vel_v= get_sds(sd_id,'des_avg_wind_vel_v',ir,slab_size)
  asc_avg_wind_speed_sq= get_sds(sd_id,'asc_avg_wind_speed_sq',ir,slab_size)
  des_avg_wind_speed_sq= get_sds(sd_id,'des_avg_wind_speed_sq',ir,slab_size)
  asc_wvc_count= get_sds(sd_id,'asc_wvc_count',ir,slab_size)
  des_wvc_count= get_sds(sd_id,'des_wvc_count',ir,slab_size)
  asc_time_frac= get_sds(sd_id,'asc_time_frac',ir,slab_size)
  des_time_frac= get_sds(sd_id,'des_time_frac',ir,slab_size)
  asc_rain_prob= get_sds(sd_id,'asc_rain_prob',ir,slab_size)
  des_rain_prob= get_sds(sd_id,'des_rain_prob',ir,slab_size)
  asc_rain_flag= get_sds(sd_id,'asc_rain_flag',ir,slab_size)
  des_rain_flag= get_sds(sd_id,'des_rain_flag',ir,slab_size)

; Select latitude and longitudes
  read,'Enter the minimum and maximum latitudes [-90,90]:',lat1,lat2
  if ((lat1 lt -90) or (lat2 lt -90) or (lat1 gt 90) or $
      (lat2 gt 90)) then begin
     print,'ERROR: Latitudes must be between -90 and 90'
     stop
  endif

; Make sure that lat2 is greater than lat1
  if (lat1 gt lat2) then begin
    itmp=lat1
    lat1=lat2
    lat2=itmp
  endif

; The last grid point is in cell 719.  Reduce lat 90. to 89.9
  if (lat2 eq 90.) then begin
    lat2=89.9
  endif

  read,'Enter the minimum and maximum longitudes [0,360]:',lon1,lon2
  if ((lon1 lt 0) or (lon2 lt 0) or (lon1 gt 360) or $
      (lon2 gt 360)) then begin
     print,'ERROR: Longitudes must be between 0 and 360'
     stop
  endif

; Make sure that lon2 is greater than lon1
  if (lon1 gt lon2) then begin
    itmp=lon1
    lon1=lon2
    lon2=itmp
  endif

; The last grid point is in cell 1439.  Wrapping is not done here,
; so 360, must be reduced to 359.9.
  if (lon2 eq 360.) then begin
    lon2=359.9
  endif

; Determine grid points from the latitudes and longitudes
  dx=(360./1440.)
  ii1=fix(lon1/dx)
  ii2=fix(lon2/dx)

  dy=(180./720.)
  jj1=fix((lat1+90.)/dy)
  jj2=fix((lat2+90.)/dy)

; Print results to screen
  print,'ASCENDING PASS (DAYTIME)'
  print,'   LON       LAT       SPD      U       V     SPD2 COUNT  TIME   RAIN PROB  RAIN FLAG'
  for i=ii1,ii2 do begin
    for j=jj1,jj2 do begin

; Calculate the center lat and lon of the grid box    
      lon=i*dx+(dx/2)
      lat=((j*dy)-90.)+(dy/2)
      if (asc_wvc_count(i,j) gt 0) then begin	
        print,lon,lat,asc_avg_wind_speed(i,j),asc_avg_wind_vel_u(i,j), $
	  asc_avg_wind_vel_v(i,j),asc_avg_wind_speed_sq(i,j), $
          asc_wvc_count(i,j),asc_time_frac(i,j),asc_rain_prob(i,j), $
	  asc_rain_flag(i,j), $
	  format='(f9.5,x,f9.5,x,f7.2,x,f7.2,x,f7.2,x,f7.2,2x,f2.0,f9.5,x,f7.3,7x,f2.0)'
      endif
    endfor
  endfor

  print,' '
  print,'DESCENDING PASS (NIGHTTIME)'
  print,'   LON       LAT       SPD      U       V     SPD2 COUNT  TIME   RAIN PROB  RAIN FLAG'
  for i=ii1,ii2 do begin
    for j=jj1,jj2 do begin

; Calculate the center lat and lon of the grid box    
      lon=i*dy+(dx/2)
      lat=((j*dy)-90.)+(dy/2)
      if (des_wvc_count(i,j) gt 0) then begin	
        print,lon,lat,des_avg_wind_speed(i,j),des_avg_wind_vel_u(i,j), $
	  des_avg_wind_vel_v(i,j),des_avg_wind_speed_sq(i,j), $
          des_wvc_count(i,j),des_time_frac(i,j),des_rain_prob(i,j), $
	  des_rain_flag(i,j), $
	  format='(f9.5,x,f9.5,x,f7.2,x,f7.2,x,f7.2,x,f7.2,2x,f2.0,f9.5,x,f7.3,7x,f2.0)'
      endif
    endfor
  endfor

end


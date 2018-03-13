; Filename:  read_qscat2b_cp12.pro
;
; Usage:
;
;   To run this programs, use the following command:
;
;   IDL> read_qscat2b_cp12
;
; Description:
;  
;   This file contains one (1) IDL procedure and four (4) functions
;   used to read the QuikSCAT Level 2B data in Hierarchical Data 
;   Format (HDF).  The routines are as follows.
;   
;   1. correct_uint8: a function to correct variables 
;                     stored as 8-bit unsigned integers.
;
;   2. correct_uint16: a function to correct variables 
;                      stored as 16-bit unsigned integers.
;
;   3. get_vdata: a function to read the timetags which 
;                 are stored as VDATA.
;
;   4. get_sds: a function to read the data which are 
;               stored as Scientific Data Sets.
;
;   5. read_qscat2b: the main procedure. Calls get_vdata and 
;                    get_sds.
;
; Notes:
;
; 1. The directory on your local system which contains the QuikSCAT
;    L2B data must be input in program read_qscat2b.pro.  (Search on
;    "data_dir" to find this line quickly.)
;
; 2. The L2B data are read in their entirety.  If you would like
;    to read these data in slabs, change the value of "slab_size".
;
; 3. The correction of Unsigned Integers is currently hard-coded for
;    versions of IDL prior to 5.2.
;
; 4. Please send all comments and questions concerning these routines
;    to qscat@podaac.jpl.nasa.gov.
;
;
; 4/18/99 K.L. Perry, C.S. Hsu, R.S. Dunbar
;
; Modifications:
;
; 11/18/1999 Added capability to read new SDSs,
;            wind_speed_selection and wind_dir_selection.
;            K.L. Perry
;
;  1/10/2000 Added correct_uint8 function and capability to 
;            read new SDSs, mp_rain_probability and nof_rain_index.  
;            K.L. Perry
;
;  08/08/2004 Replaced pickfile by dialog_pickfile, since the former 
;  is no longer supported by IDL 6.0 .
;            Ted Lungu
;  07/13/2006 Changes related to the 12.5km product and sdrad_rain_rate
;            Ted Lungu
;==================================================================
;   Copyright (c) 1999-2006, California Institute of Technology
;==================================================================

;==================================================================
; correct_uint8: a function to correct data which are stored
;                as 8-bit unsigned integers for versions of
;                IDL prior to 5.2
;==================================================================
function correct_uint8,sds_data
    w=where(sds_data lt 0,cnt)
    if (cnt gt 0) then begin
      sds_data(w)=sds_data(w)+256
    endif
return,sds_data
end

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
; get_vdata: a function to read the timetags which are stored as
;            VDATA.
;==================================================================
function get_vdata,fid,vdata_name
  result=HDF_VD_FIND(fid,vdata_name)
  vid=HDF_VD_ATTACH(fid,result)
  result=HDF_VD_READ(vid,vdata)
  HDF_VD_DETACH,vid
return,vdata
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

;; UINT8
    if (sds_name eq "nof_rain_index") then begin
      data=correct_uint8(float(data))
    endif

;; UINT16
    if ((sds_name eq "wvc_lon") or $
        (sds_name eq "wvc_quality_flag") or $
        (sds_name eq "model_dir") or $
        (sds_name eq "wind_dir") or $
        (sds_name eq "wind_dir_selection")) then begin
      data=correct_uint16(float(data))
    endif

; Apply the scale and offset
    rdata = data*cal.cal - cal.offset
    HDF_SD_ENDACCESS,sds_id

return,rdata
end


;==================================================================
; read_qscat2b: the main procedure in this file.  Calls are made 
;               to get_vdata to read the timetags and get_sds to 
;               read the data.  The results are then printed to 
;               the screen.
;==================================================================
pro read_qscat2b

;Select a Level 2B file
;***** Change data_dir to suit your local system
  data_dir="./"
  filename=dialog_pickfile(filter='QS_S2B*', PATH = data_dir, /READ)

;  filename=pickfile(/READ,path=data_dir,filter='QS_S2B*', $
;                    title='QuikSCAT L2B')

  print,' '
  print,'FILENAME: ',filename
  print,' '

;Read the Time Tags contained in the VDATA
  fid=HDF_OPEN(filename,/READ)
  wvc_row_time= get_vdata(fid,'wvc_row_time')
  HDF_CLOSE,fid

;Read the Scientific Data Sets
  sd_id=HDF_SD_START(filename,/READ)

;Get the x-dimension in order to read an entire SDS
  index=HDF_SD_NAMETOINDEX(sd_id,'wvc_lat')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                 type=data_type,caldata=cal
  slab_size=strtrim(dims(1),2)
  nr_wvc= strtrim(dims(0),2)   
  ir=0

;; The L2B SDSs are read in their entirety.  An example of
;; reading the SDSs in slabs of size 1 is shown below.
;;
;;  slab_size=1
;;  for ir=irec1-1,irec2-1 do begin
;;    wvc_row=get_sds(sd_id,'wvc_row',ir,slab_size)
;;    ...
;;    print,'TIME: ',string(wvc_row_time(*,ir))
;;    print,'WVC Row: ',wvc_row(ir)
;;  endfor  

; Read the SDSs
  wvc_row= get_sds(sd_id,'wvc_row',ir,slab_size)
  wvc_lat= get_sds(sd_id,'wvc_lat',ir,slab_size)
  wvc_lon= get_sds(sd_id,'wvc_lon',ir,slab_size)
  wvc_index= get_sds(sd_id,'wvc_index',ir,slab_size)
  num_in_fore= get_sds(sd_id,'num_in_fore',ir,slab_size)
  num_in_aft= get_sds(sd_id,'num_in_aft',ir,slab_size)
  num_out_fore= get_sds(sd_id,'num_out_fore',ir,slab_size)
  num_out_aft= get_sds(sd_id,'num_out_aft',ir,slab_size)
  wvc_quality_flag= get_sds(sd_id,'wvc_quality_flag',ir,slab_size)
  atten_corr= get_sds(sd_id,'atten_corr',ir,slab_size)
  model_speed= get_sds(sd_id,'model_speed',ir,slab_size)
  model_dir= get_sds(sd_id,'model_dir',ir,slab_size)
  num_ambigs= get_sds(sd_id,'num_ambigs',ir,slab_size)
  wind_speed= get_sds(sd_id,'wind_speed',ir,slab_size)
  wind_dir= get_sds(sd_id,'wind_dir',ir,slab_size)
  wind_speed_err= get_sds(sd_id,'wind_speed_err',ir,slab_size)
  wind_dir_err= get_sds(sd_id,'wind_dir_err',ir,slab_size)
  max_likelihood_est=get_sds(sd_id,'max_likelihood_est',ir,slab_size)
  wvc_selection= get_sds(sd_id,'wvc_selection',ir,slab_size)
  wind_speed_selection= get_sds(sd_id,'wind_speed_selection',ir,slab_size)
  wind_dir_selection= get_sds(sd_id,'wind_dir_selection',ir,slab_size)
  mp_rain_probability= get_sds(sd_id,'mp_rain_probability',ir,slab_size)
  nof_rain_index= get_sds(sd_id,'nof_rain_index',ir,slab_size)
  srad_rain_rate= get_sds(sd_id,'srad_rain_rate',ir,slab_size)

; Select wind vector cell rows to be written to the screen
  read,'Enter the first and last wvc row rec numbers to be written: ',irec1,irec2
  if (irec1 gt irec2) then begin
    itmp=irec1
    irec1=irec2
    irec2=itmp
  endif
  if ((irec1 lt 1) or (irec2 gt slab_size)) then begin
    print,'ERROR: wvc rows must be between 1 and ',slab_size
    stop
  endif

; Subract 1 from irec1 and irec2 to adjust for IDL running 
; from 0 instead of 1 (so wvc_row 1 matches input of 1)

  for ir=irec1-1,irec2-1 do begin

; Print results to screen
    print,' '
    print,'TIME: ',string(wvc_row_time(*,ir))
    print,'WVC Row: ',wvc_row(ir)
    print,'WVC#  WVC_Qual  WVC_Latitude/Longitude  '+ $
          'Selected Wind Vector   NWP Wind Vector Num/Sel Ambig  '+ $
          'DRE Wind Vector    MUDH     NOF  SRR'

    for j=0,nr_wvc-1 do begin
      if (num_ambigs(j,ir) gt 0) then begin
        iamb=wvc_selection(j,ir)-1
        print,wvc_index(j,ir),wvc_quality_flag(j,ir),wvc_lat(j,ir), $
              wvc_lon(j,ir),wind_speed(iamb,j,ir),wind_dir(iamb,j,ir), $
              model_speed(j,ir),model_dir(j,ir), $
              num_ambigs(j,ir),wvc_selection(j,ir), $
	      wind_speed_selection(j,ir),wind_dir_selection(j,ir), $
              mp_rain_probability(j,ir),nof_rain_index(j,ir), $
              srad_rain_rate(j,ir),$
        format='(i2,5x,"0X",z4.4,8x,f6.2,3x,f6.2,6x,f6.2,3x,f6.2,6x,f6.2,3x,f6.2,5x,i2,3x,i2,3x,f6.2,3x,f6.2,3x,f6.2,3x,f6.2, 2x, f6.2)
      endif
    endfor
  endfor
  HDF_SD_END,sd_id
end







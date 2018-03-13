; Filename:  read_qscat_info.pro
;
; Usage:
;
;   To run these programs, use the following command:
;
;   IDL> read_qscat_info
;
; Description:
;  
;   This file contains three (3) IDL procedures and one (1) 
;   function used to read the attributes contained in all levels
;   of QuikSCAT data in Hierarchical Data Format (HDF).  The 
;   routines are as follows.
;   
;   1. getTextAfterNewline: a function to correct variables read
;                           the global attributes which are stored
;                           in arrays.
;
;   2. read_local_attributes: a procedure to read the local
;                             attributes or the "header" information
;                             pertaining to the data stored as SDSs.
;
;   3. read_global_attributes: a procedure to read the global 
;                              attributes or the "header" information
;                              pertaining to the entire QuikSCAT file.
;
;   4. read_qscat_info: the main procedure. Calls 
;                       read_local_attributes and 
;                       read_global_attributes
;
; Notes:
;
; 1. The directory on your local system which contains the QuikSCAT
;    L2A data must be input in program read_qscat.pro.  (Search on
;    "data_dir" to find this line quickly.)
;
; 2. Correction of Unsigned Integers is currently hard-coded for
;    versions of IDL prior to 5.2.
;
; 3. Please send all comments and questions concerning these routines
;    to qscat@podaac.jpl.nasa.gov.
;
;
; 4/18/99 K.L. Perry, R.S. Dunbar, C.S. Hsu
;
; Modifications:
;
; 9/15/1999 read_global_attributes was modified to read attributes
;           which are given in the form of 2D arrays.  The format
;           for the output were also changed. K.L. Perry
;
;==================================================================
;   Copyright (c) 1999, California Institute of Technology
;==================================================================

;==================================================================
; getTextAfterNewline: a function which extracts all characters 
;                      between two new lines from a string.
;==================================================================
function getTextAfterNewline,attr_data,nreps
  new_line=string(10B)

; If attr_data is not reassigned, attr_data will be affected
; outside of the function.
  fattr_data=attr_data
  for i=0,nreps-1 do begin
    data_len=strlen(fattr_data)
    locate_nl=strpos(fattr_data,new_line)
    fattr_data=strmid(fattr_data,locate_nl+1,data_len)
    locate_nl2=strpos(fattr_data,new_line)
    ret_attr_data=strtrim(strmid(fattr_data,0,locate_nl2),2)
  endfor
  
return,ret_attr_data
end

;==================================================================
; read_local_attributes: a procedure to read the local attributes
;                        and print the results to the screen
;==================================================================
pro read_local_attributes,sd_id,n_datasets

; Read all local attributes
  if (n_datasets gt 0) then begin
    for i=0,n_datasets-1 do begin
      sds_id=HDF_SD_SELECT(sd_id,i)
      HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                     type=data_type,caldata=cal

; Initialize the dimsizes array
      dim_sizes=intarr(3)
      for irank=0,rank-1 do begin
        dim_sizes(irank)=dims(irank)
      endfor

; Get the Calibration and Offset
      calibration=cal.cal
      offset=cal.offset

; Print the information to the screen
      print,i,strtrim(name,2),strtrim(rank,2),strtrim(dim_sizes(0),2),$
            strtrim(dim_sizes(1),2),strtrim(dim_sizes(2),2), $
            calibration,offset,data_type, $
            format='(i2,2x,a21,4(i5,x),2x,2(f11.9,2x),a6)
    endfor
  endif
end

;==================================================================
; read_global_attributes: a procedure to read the global attributes
;                         and print the information to the screen.
;==================================================================
pro read_global_attributes,sd_id,n_glob_attrs

; Read all global attributes
  if (n_glob_attrs gt 0) then begin
    for i=0,n_glob_attrs-1 do begin
      HDF_SD_ATTRINFO,sd_id,i,name=attr_name,data=attr_data

; Obtain number of items in data type array
      reps=getTextAfterNewline(attr_data,1)

; Check for 2D attributes
      xcomma=strpos(reps,',')
      if (xcomma ge 0) then begin
	x1=strmid(reps,0,xcomma)
        xlen=strlen(reps)
	x2len=xlen-xcomma+1
	x2=strmid(reps,xcomma+1,x2len)
        reps=fix(x1)*fix(x2)
      endif else begin
	reps=fix(reps)
      endelse

; Obtain needed information and print to screen
      for k=0,reps-1 do begin
        ret_attr_data=' '
        ret_attr_data=getTextAfterNewline(attr_data,k+2)

        attr_name_len=strlen(strtrim(attr_name))
        sattr_name_len=strtrim(string(attr_name_len),2)
        xspace=strtrim(string(30-attr_name_len),2)
	ret_attr_data_len=strlen(strtrim(ret_attr_data,2))
	sret_attr_data_len=strtrim(string(ret_attr_data_len),2)

        if (k eq 0) then begin
          xformat=('(i2,x,a'+sattr_name_len+','+xspace+ $
	         'x,a'+sret_attr_data_len+')')
          print,i,strtrim(attr_name,1),strtrim(ret_attr_data,1), $
                format=xformat
        endif else begin
	  xformat=('(33x,a'+sret_attr_data_len+')')
          print,strtrim(ret_attr_data,2),format=xformat
        endelse
      endfor
    endfor
  endif
end

;==================================================================
; read_qscat_info: the main procedure in this file.  Calls are
;                  made to read_local_attributes and 
;                  read_global_attributes.
;==================================================================
pro read_qscat_info

;Select a QuikSCAT file
;*****Change data_dir to suit your local system
  data_dir="/Volumes/pecan2/ahijevyc/cdavis/ascat/"
  filename=pickfile(/READ,path=data_dir,filter='QS_*', $
                    title='QuikSCAT Attributes')

; Open the input file and initialize the SD interface
  sd_id=HDF_SD_START(filename,/READ)

; Retrieve the number of datasets and the number of global
; attributes
  HDF_SD_FILEINFO,sd_id,n_datasets,n_glob_attrs

; Print the number of datasets and global attributes to screen
  print,'HDF info for file: ',filename
  print,' '
  print,'Number of Scientific Data Sets: ',strtrim(n_datasets,2)
  print,'Number of Global Attributes: ',strtrim(n_glob_attrs,2)
  print,' '
  print,'Index     Dataset Name  Rank    Dimensions          Scale       Offset   Data Type '
  print,'----------------------------------------------------------------------------------'

; Read the local attributes
  read_local_attributes,sd_id,n_datasets

; Read the global attributes
  print,' '
  print,'Index  Attribute Name            Value'
  print,'----------------------------------------------------------------------------------'
  read_global_attributes,sd_id,n_glob_attrs
  
end

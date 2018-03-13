
function getFile, fullpath, suffix=suffix
; func_description
; This function returns the filename name from the full path.
; Inputs:    fullpath - full directory+file path
; Keyword:   /suffix:   include inptu suffix in output file name
; Outputs:   file - filename
; Example Call: file = getFile(fullpath)

; Retrieve the postion at which the first '/' character occurs from
; the end of the string.
dirlen = rstrpos(fullpath, '/')

; Retrieve the full length of the original string.
len = strlen(fullpath)

; Retrieve the filename.
fullfile = strmid(fullpath, dirlen+1, len)

; Retrieve the position at which the first '.' character occurs from
; the end of the string.
len = -1
if not(keyword_set(suffix)) then len = rstrpos(fullfile, '.')
if (len EQ -1) then len = strlen(fullfile)

; Retrieve the file.
file = strmid(fullfile, 0, len)

; Return the file name.
return, file

; End function.
end

function DO_PNGS, base, data, red, green, blue, secs

   nrec = size(data)
   nrec = nrec[4]

   for rec=0, nrec-1 do begin
      outfile=base+string(format='("_",i9.9,".png")', secs[rec])
      WRITE_PNG, '/mmmtmp/ahijevyc/'+outfile, data[*,*,0,rec],red,green,blue,TRANSPARENT=[0,255]
	PRINT, format='(".",$)'
   endfor

   return, 0
end ; end function do_pngs




PRO netCDF2png, infile
; pro_description, infile
; This procedure reads a given netCDF
; file into IDL.
; It loops through all 2-D byte matrices for the lowest elevation, assuming
; the first 2 dimensions are lat and lon, the third dimension is elevation,
; and the final fourth dimension is the record dimension 'record'.
; Creates a png image for each one.
; The default output files are named XXgridx_ddd.png where 
; XXgridx is the variable name and ddd is a sequential index. 
; Inputs:           infile  - full path to netCDF file of interest
; Outputs:          XXgridx_ddd - png files from netCDF data
;			output directory is /mmmtmp/ahijevyc/
; Example:  netCDF2png, '/mmmtmp/ahijevyc/file.nc'



; Ensure that the netCDF format is supported on the current platform.
if not(ncdf_exists()) then begin 
   print, "The Network Common Data Format is not supported on this platform."
   return
endif


; Open the netcdf file for reading.
ncid = NCDF_OPEN(strcompress(infile, /remove_all))
if (ncid EQ -1) then begin
   print, "The file "+infile+" could not be opened, please check the path."
   return
endif

; Retrieve general information about this netCDF file.
ncidinfo = NCDF_INQUIRE(ncid)

; Place the desired variables in local arrays.
for i=0, ncidinfo.Nvars-1 do begin 
   vardata = NCDF_VARINQ(ncid, i)
   varname = vardata.Name
   v1 = strmid(varname,0,2)
   v2 = strmid(varname,2,4)
   if ( strlen(varname) EQ 7 and v2 EQ "grid" ) then begin	
      grid = strmid(varname,2,5)

      case v1 of

         'DZ' :  NCDF_VARGET, ncid, varname, dz  
         'VE' :  NCDF_VARGET, ncid, varname, ve
  
      else: PRINT, "invalid variable "+v1
      endcase

   endif
endfor

NCDF_VARGET,ncid,'record',secs
NCDF_ATTGET,ncid,/GLOBAL,'elevation',elevation_float
elevation = STRING(elevation_float, FORMAT='("el",F3.1)')
PRINT,elevation_float,elevation
NCDF_CLOSE, ncid

;do this operation for dz
dz=(dz+128) MOD 256
ind=where(dz eq 0)
dz=BYTSCL(dz, MAX=198, MIN=93, TOP=250) ; equivalent to -35 and 66 dBZ
dz[ind]=255 ; max(dz)+1

nrec = size(dz)
nrec = nrec[4]



LOADCT,41,file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; radar reflectivity
outfile=getFile(infile)
outfile=strmid(outfile,0,13)+'DZ'+grid+elevation
TVLCT, red, green, blue, /GET
ret=DO_PNGS(outfile,dz, red, green, blue,secs)




;do this operation for ve
ve=(ve+128) MOD 256
ind=where(ve eq 0 or ve eq 1)
ve[ind]=max(ve)+1
;ve[ind]=255
; spread the values out more
;ve=hist_equal(ve) 

ve=BYTSCL(ve)

LOADCT,43, file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; radial velocity
outfile=getFile(infile)	
outfile=strmid(outfile,0,13)+'VE'+grid+elevation
TVLCT, red, green, blue, /GET
ret=DO_PNGS(outfile,ve, red, green, blue,secs)

PRINT, "do a tips search for 'draw' to see the next step"
; end procedure netCDF2png 
end

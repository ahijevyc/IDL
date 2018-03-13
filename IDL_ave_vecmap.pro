; Filename:  ave_vecmap.pro
;
; Purpose:
;
;   To produce a map of wind vectors from a selected 
;   QuikSCAT Level 3 file.
;
; Usage:
; 
;   To run this program, use the following command:
;   
;   IDL> ave_vecmap
;
; Description:
;
;   This file contains four (4) IDL procedures and three (3)
;   functions in order to plot a QuikSCAT Level 3 data file in
;   Hierarchical Data Format (HDF).  The routines are as follows.
;
;   1. correct_uint16: a function to correct variables stored
;                      as 16-bit unsigned integers for versions
;                      of IDL prior to 5.2
;
;   2. correct_uint32: a function to correct variables stored
;                      as 32-bit unsigned integers for versions
;                      of IDL prior to 5.2
;
;   3. get_sds: a function to read the data which are stored as
;               Scientific Data Sets.
;
;   4. read_l3_rebin: a procedure to resize the arrays
;
;   5. varrows: a procedure to plot the wind vectors
;
;   6. colorbar: a procedure to create and plot the
;                colorbar
;
;   7. ave_vecmap: the main procedure.  Calls read_l3_rebin to resize
;                  the arrays.  Computes the average of the passes, 
;                  plots a given land mask, and calls varrows and
;                  colorbar to plot the wind vectors and colorbar.
; 
; 9/1999 R.S. Dunbar
;
; Notes:
;
; 3/2000 Code modified to use non-integer factors (now uses congrid 
;        as well as rebin). Routine now plots every wind vector 
;        instead of every tenth wind vector. More comments added.
;        Included all necessary subroutines in one file in order 
;        to have a self-contained example program.  K.L. Perry
;
; 5/2000 Code modified to allow uint16 correction for 
;        asc_time_frac,des_time_frac,asc_rain_prob and des_rain_prob
;        K.L. Perry
;10/2001 Added caution note regarding averaging ascending and 
;        descending passes. 
;        T. Lungu
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
; read_l3_rebin: a procedure to resize the arrays.
;
; 8/1999 R.S. Dunbar
;
; Notes:
;
; 2/2000 Added congrid option for non-integer amount array
;        re-sizing.  Added comments.  K.L. Perry
;==================================================================
pro read_l3_rebin,filename,factor,asc_spd,des_spd, $
	asc_u,des_u,asc_v,des_v,asc_spdsq,des_spdsq, $
	asc_cnt,des_cnt,xdim,ydim

;Read the Scientific Data Sets
  sd_id = HDF_SD_START(filename,/READ)

;Get the x-dimension in order to read an entire SDS
  index=HDF_SD_NAMETOINDEX(sd_id,'asc_wvc_count')
  sds_id=HDF_SD_SELECT(sd_id,index)
  HDF_SD_GETINFO,sds_id,ndims=rank,dims=dims,label=name, $
                 type=data_type,caldata=cal
  slab_size=strtrim(dims(1),2)   
  ir=0

;Determine size of new array
  xdim=dims(0)/factor
  ydim=dims(1)/factor

  print,"New Array Size is: ",fix(xdim)," x ",fix(ydim)

;Read and Resize the SDSs

;If new array size is integer amount of original array, use rebin
  if ((xdim eq fix(xdim)) and (ydim eq fix(ydim))) then begin
    print,'Using REBIN'
    asc_spd=rebin(get_sds(sd_id,'asc_avg_wind_speed',ir,slab_size),xdim,ydim)
    des_spd=rebin(get_sds(sd_id,'des_avg_wind_speed',ir,slab_size),xdim,ydim)
    asc_u=rebin(get_sds(sd_id,'asc_avg_wind_vel_u',ir,slab_size),xdim,ydim)
    des_u=rebin(get_sds(sd_id,'des_avg_wind_vel_u',ir,slab_size),xdim,ydim)
    asc_v=rebin(get_sds(sd_id,'asc_avg_wind_vel_v',ir,slab_size),xdim,ydim)
    des_v=rebin(get_sds(sd_id,'des_avg_wind_vel_v',ir,slab_size),xdim,ydim)
    asc_spdsq=rebin(get_sds(sd_id,'asc_avg_wind_speed_sq',ir,slab_size),xdim,ydim)
    des_spdsq=rebin(get_sds(sd_id,'des_avg_wind_speed_sq',ir,slab_size),xdim,ydim)
    asc_cnt=rebin(get_sds(sd_id,'asc_wvc_count',ir,slab_size),xdim,ydim)
    des_cnt=rebin(get_sds(sd_id,'des_wvc_count',ir,slab_size),xdim,ydim)

;If new array size is NOT integer amount of original array, use congrid
  endif else begin 
    print,'Using CONGRID'
    xdim=fix(xdim)
    ydim=fix(ydim)
    asc_spd=congrid(get_sds(sd_id,'asc_avg_wind_speed',ir,slab_size),xdim,ydim)
    des_spd=congrid(get_sds(sd_id,'des_avg_wind_speed',ir,slab_size),xdim,ydim)
    asc_u=congrid(get_sds(sd_id,'asc_avg_wind_vel_u',ir,slab_size),xdim,ydim)
    des_u=congrid(get_sds(sd_id,'des_avg_wind_vel_u',ir,slab_size),xdim,ydim)
    asc_v=congrid(get_sds(sd_id,'asc_avg_wind_vel_v',ir,slab_size),xdim,ydim)
    des_v=congrid(get_sds(sd_id,'des_avg_wind_vel_v',ir,slab_size),xdim,ydim)
    asc_spdsq=congrid(get_sds(sd_id,'asc_avg_wind_speed_sq',ir,slab_size),xdim,ydim)
    des_spdsq=congrid(get_sds(sd_id,'des_avg_wind_speed_sq',ir,slab_size),xdim,ydim)
    asc_cnt=congrid(get_sds(sd_id,'asc_wvc_count',ir,slab_size),xdim,ydim)
    des_cnt=congrid(get_sds(sd_id,'des_wvc_count',ir,slab_size),xdim,ydim)
  endelse
HDF_SD_END,sd_id
end

;==================================================================
; VARROWS.pro
; 
; PURPOSE: To plot vectors.
;
; INPUTS:
;       U = The X component of the two dimensional field.
;       V = The Y component of the two dimensional field.
;       X = Abcissae values.
;       Y = Ordinate values.
; Keyword Input Parameters:
;       Length = Length factor
;       Title =  Optional plot title
;       Color =  Color index used for the plot.
;       Thick =  Line thickness.
;
;  1999 R.S. Dunbar
;
;  Notes:
;
;  2/2000 Set value of maxmag to 30m/s to ensure that different
;         input files would be plotted on the same scale. Set all
;         wind vectors greater than maxmag to maxmag for plotting
;         purposes.  Added new comments and edited old ones.  
;         K.L. Perry
;
;==================================================================
pro varrows,u,v,x,y, Length = length,Title = title,Color=color,Thick=thick
        if n_elements(length) le 0 then length = 0.1
	if n_elements(thick) le 0 then thick = 1

; Determine the number of dimensions in u 
;; Number of dimensions is the first element returned by size
	n = size(u)

;If the number of dimensions is not 0 then set ilim to the total
;number of elements minus one. ilim is a longword integer. 
;;Total number of elements is the last element returned by size
	ilim = 0
	if (n(0) ne 0) then ilim = long(n(n(0)+2) - 1)

;Calculate the magnitude of the vector
        mag = sqrt(u^2+v^2)

;Declare maxmag as 30m/s.  This ensures that the color coding will 
;be the same for different input files
        maxmag=30.0

;Set winds higher than maxmag to maxmag for display purposes
        w=where(mag gt maxmag,cnt)
        if (cnt gt 0) then begin
          mag(w)=maxmag
        endif

;;Uncomment the following line to adjust the color coding to the 
;;data in the input only
;;        maxmag = max(mag)

;Scale the max speed of the color bar to maxmag
	col = byte(mag*!d.n_colors/maxmag) 

;Get scaling of longitudes and latitudes
        x0 = min(x)
        x1 = max(x)
        y0 = min(y)
        y1 = max(y)

;Determine the scaled values of u and v 
        sina = length/maxmag*u
        cosa = length/maxmag*v

;Select the length of arrow head
        r = .2

;Determine the angle of arrow head
;; !dtor=pi/180=0.0174533 converts degrees to radians
        angle = 22.5 * !dtor

;Compute the sin/cos of 22.5 degs * length of head
        st = r * sin(angle)
        ct = r * cos(angle)

;Plot the wind vectors at each point
        for i=0L,ilim do begin

;Get coordinates of the start and end points
;;Note: the geographic coordinate which were read by this procedure,
;;x(i) and y(i), will be in the *middle* of the wind vector. x0 is
;;the tail, and x1 is the arrow head.  This was done for aesthetic
;;purposes (no visual discontinuities in the swath edge).
                dx = sina(i)
                x0 = x(i) - dx/2.
                x1 = x0 + dx
                dy = cosa(i)
                y0 = y(i) - dy/2.
                y1 = y0 + dy

;Plot the wind vector and arrow segments
                plots,[x0,x1,x1-(ct*dx-st*dy),x1,x1-(ct*dx+st*dy)], $
                      [y0,y1,y1-(ct*dy+st*dx),y1,y1-(ct*dy-st*dx)], $
		      color=col(i)
        endfor
return
end

;==================================================================
; COLORBAR.pro
;
; PURPOSE: To create and plot a colorbar.
;
; INPUTS:
;       MIN = minimum value
;       MAX = maximum value
;       CINC= number of labelled increments
;       XP  = x-position of the lower left corner of the colorbar
;             on the plot
;       YP  = y-position of the lower left corner of the colorbar
;             on the plot
;       TITL= title of the colorbar
;
; 1999 R.S. Dunbar
;
; Notes: 
;
; 2/2000 Added border around color bar.  Added and edited 
;        comments.  K.L. Perry
;
;==================================================================
pro colorbar,min,max,cinc,xp,yp,titl

;Determine the number of tick marks in the colorbar
n=(max-min)/cinc
n=fix(n+0.5)
tick=fltarr(n+1)
for i=0,n do begin
   tick(i)=min+(i*cinc)
endfor

;xaxis=0,xcharsize=2.0,$

;Create colorbar array
nc=!d.n_colors
r3=bindgen(nc)
r3t=bytarr(nc,1)
r3t(*,0)=r3(*)

;Make the colorbar a 512 by 20 pixel array
r3=rebin(r3,nc*3,1*20)

;Make border around color bar
r3(0,*)=255
r3(*,0)=255
r3(511,*)=255
r3(*,19)=255

;Plot the tick marks and labels on the colorbar
plot,[min,max],[min,max],position=[xp,yp,xp+3*nc-1,yp],/device,/nodata,$
xticks=n,xtickv=tick,xrange=[min,max],xminor=5,xstyle=1,xtitle=titl, $
ystyle=4,/noerase

;Display the colorbar
tv,r3,xp,yp,/device

end

;==================================================================
; pro ave_vecmap: the main procedure in this file.  Calls
;                 are made to read_l3_rebin, varrows and colorbar.
;                 A wind vector map of a user defined array size
;                 is plotted to the screen.
;==================================================================
pro ave_vecmap

;Select a Level 3 file
;***** Change data_dir to suit your local system
  data_dir="/Volumes/pecan2/ahijevyc/cdavis/ascat/"
  filename=pickfile(/READ,path=data_dir,filter='QS_XWGRD3*', $
                    title='QuikSCAT L3')
;;  filename="QS_XWGRD3_1999361.20000032359"

  print,' '
  print,'FILENAME: ',filename
  print,' '

;Re-size the output array by an integer amount.
;  Note: factor of 1 keeps the original array size'
;        factor of 2 cuts the array size to half the original size
;        factor of 3 cuts the array size to one third the original...'

  read,factor,prompt='Resize the 1440 x 720 array.  Divide by: '
;;  factor=1

;Make sure the array is "cut" by numbers of 1 and larger
  if (factor lt 1) then begin
    print,"Array must be divided by a number greater than or equal to 1"
    stop
  endif

;Rebin or congrid the L3 parameters into the user selected array size
  read_l3_rebin,filename,factor,asc_spd,des_spd,asc_u,des_u, $
	  asc_v,des_v,asc_spdsq,des_spdsq,asc_cnt,des_cnt,xdim,ydim

;Compute the average the ascending and descending passes
;  Note:

;  Caution should be used when interpreting the results produced by the IDL
;  program ave_vecmap.pro; some artifacts may affect the map.  Artifacts may
;  be due to the following causes:

;    o the binning algorithm leaves some isolated cells empty, because grid
;  size and data spacing are almost, but not exactly, identical.  Variables
;  affected are asc_u, des_u, asc_v, and des_v.

;    o when the ascending and descending grids are averaged, a later empty
;  cell in a later neighborhood acquires the wind vector of the earlier cell,
;  about 1/2 day earlier. This happens because Level 3 files contain only the
;  latest measurement for each day. Since QuikSCAT orbit is sun-synchronous,
;  the local time of the samples is about the same every revisit.

;  Easy 'solutions' to the problem would be to 'fill' the isolated empty
;  cells with some average of their neighbors, or to discard cells for
;  which either ascending or descending values are missing.

  ave_u=asc_u*asc_cnt + des_u*des_cnt
  ave_v=asc_v*asc_cnt + des_v*des_cnt
  tot_cnt=asc_cnt + des_cnt

  jj=where(tot_cnt ne 0.)
  ave_u(jj) = ave_u(jj)/tot_cnt(jj)
  ave_v(jj) = ave_v(jj)/tot_cnt(jj)

;Create a window for the display of the graphics
  device,decomposed=0,pseudo=8
  window,xsize=900,ysize=600,title='QuikSCAT Level 3',colors=256

;Load a defined color table
  loadct,39,/silent

;Read the land mask
;;NOTE:  The provided example land mask,lmask.800.400.dat, 
;;       was generated for wind vector arrays on an 800 by
;;       400 grid.  However, it will work for other selected
;;       grid sizes provided the window size remains  900
;;       by 600.  If the window size is changed or if the
;;       user is interested in data near land for higher 
;;       resolution grids, a new land mask should be
;;       generated by the user.
  openr,1,'/Volumes/pecan2/ahijevyc/cdavis/ascat/lmask.800.400.dat'
  land=bytarr(800,400)
  readu,1,land
  close,1

  land2=bytarr(800,400)
  for j=0,400-1 do begin
    land2(*,j)=land(*,j)
  endfor

;Plot the land mask
  tv,land2*!d.n_colors*.9999,50,150,/device

;Plot the axes and titles/subtitles
  plot,[0.,360.],[-90.,90.],pos=[50,150,850,550],/device,$
   /noerase,/nodata,color=255, $
   title='QuikSCAT Level 3 Average Wind Vectors',subtitle=st,$
   xticks=8,xtickv=[0.,45.,90.,135.,180.,225.,270.,315.,360.],xminor=9,$
   yticks=12,$
   ytickv=[-90.,-75.,-60.,-45.,-30.,-15.,0.,15.,30.,45.,60.,75.,90.],$
   yminor=3,ytitle='Latitude',xtitle='E. Longitude'

;Determine the latitude and longitude of each grid box
  lats=fltarr(xdim,ydim)
  lons=fltarr(xdim,ydim)
  dx=360./xdim
  dy=180./ydim
  for j=0,ydim-1 do begin
    for i=0,xdim-1 do begin
      lons(i,j)=dx*(i+0.5)
      lats(i,j)=dy*(j+0.5)-90.
    endfor
  endfor

;Calculate the wind speed given the average u and v components
  spd=sqrt(ave_u*ave_u + ave_v*ave_v)
  k=where(spd gt 0.,cnt)

;Plot the Wind Vectors using varrows
  varrows,ave_u(k),ave_v(k),lons(k),lats(k),length=10,color=255-!p.background,thick=1

;Plot the color bar using colorbar
  xp = (!d.x_size - 3*!d.n_colors)/2
  colorbar,0.,30.,5.,xp,70,'Wind Speed (m/s)'

end

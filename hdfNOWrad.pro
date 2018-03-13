pro hdfNOWrad, files, region = region, debug=debug
  ; very similar to hdfNOWrad.pro

  ;+
  ; NAME:
  ;	HDFNOWrad
  ;
  ; PURPOSE:
  ;	Quick view of WSI NOWrad MASTER 15 in HDF, or binary
  ;
  ; CATEGORY:
  ;	General graphics.
  ;
  ; CALLING SEQUENCE:
  ;	hdfnowrad, hdffile
  ;
  ; INPUTS:
  ;	hdffile:	The HDF NOWrad MASTER15 file.
  ;
  ;-

  ; Note: color translation error introduced in IDL 5.5 on Linux, Solaris X86 and Tru64 UNIX
  ; when running in the 24-bit TrueColor and 24-bit DirectColor visual classes. The
  ; problem causes unexpected colors and vertical striping when the TV and TVSCL
  ; routines are used to display images.
  ; Previously, RSI recommended that users work around the problem with TV and TVSCL
  ; by issuing the following commands at the start of the IDL session (for instance,
  ; in an IDL start-up file):



  IF ~KEYWORD_SET(debug) THEN debug = 0
  
  IF ~KEYWORD_SET(region) THEN region = ['national']
  latmin=20 & lonmin=-130 & latmax=53 & lonmax=-60
  
  
  custom_ct = '/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl'
  IF FILE_TEST(custom_ct) THEN LOADCT, 41, FILE=custom_ct, SILENT = ~debug else loadct, 39, silent = ~debug
  
  if debug then print, 'region:', region
  
  
  IF !D.NAME eq 'X' then window, xsize=768, ysize=576
  
  for iregion = 0, n_elements(region)-1 do begin
    CASE region[iregion] OF
      'cent_missvly'  : limit = [34.1, -95.4,42.0,-83.9]
      'cent_plains'   : limit = [34.7,-105.0,43.2,-92.6]
      'national'      : limit = [24.4,-125.0,50.3,-66.5]
      'north_plains'  : limit = [39.7,-105.3,49.8,-90.5]
      'south_missvly' : limit = [28.5, -96.0,36.6,-85.0]
      'south_plains'  : limit = [25.8,-107.0,37.7,-90.9]
      'upper_missvly' : limit = [40.1, -98.5,49.6,-85.3]
      'all'           : limit = [20.,230.,53.,300.]
      'hovm'          : limit = [38, -100, 42, -95]
    ELSE               : message, 'bad region ' + region[iregion]
  ENDCASE
  
  
  for ifile=0,n_elements(files)-1 do begin
    file=files[ifile]
    if debug then print, 'file: ', file
    time = strmid(file,STRPOS(file,'U')+1,4)
    if time ne file then begin
      yy   = strmid(file,STRPOS(file,'U')+6,2)
      if (yy gt 70) then yyyy = '19'+yy else yyyy = '20'+yy
      jday = strmid(file,STRPOS(file,'U')+9,3)
      
      caldat, julday(1, 1, yyyy, 0, 0, 0) + jday - 1, month, day
      
      
      HDF_DFR8_RESTART
      HDF_DFR8_GETIMAGE, file, a
    endif else begin
      yyyy = '1970'
      month = 1
      day = 1
      a = read_binary(file, data_dims=[3661,1837])
    endelse
    sz = size(a)			;Size of image
    if sz[0] lt 2 then message, 'Parameter not 2D'
    
    a = a-16*long(a/16) ; subtract multiples of 16
    ; Don't be cute and flip order in TV command. It will screw up projection.
    a = rotate(a,7)
    
    title = STRUPCASE(region[iregion]) + ' ' + yyyy + STRING(month, day, format='(I2.2,I2.2)') + ' ' + time + ' UTC'
    ofile = region[iregion] + '_' + yyyy + STRING(month, day, format='(I2.2,I2.2)') + '_' + time
    
    IF !D.NAME EQ 'PS' THEN DEVICE, /COLOR, BITS=8, FILENAME = ofile + '.ps'
    if !D.NAME EQ 'PS' then if debug then print, 'output file: ', ofile+'.ps'
    
    
    
    
    if debug then begin
      z = 2
      a[0:4,*]=z & a[1828:1832,*]=z & a[3656:3660,*]=z & a[*,0:4]=z & a[*,916:920]=z & a[*,1832:1836]=z
    endif
    MAP_SET, 36.5, (lonmin+lonmax)/2., LIMIT=limit, TITLE=title, /NOBORDER, /ISOTROPIC
    
    
    ; higher scale means more resolution 0.02 is default
    ; COMPRESS=1 is most accurate
    ; MISSING=0 sets the values outside of the domain to 0.
    ; If the current device is 'PS' it gets set to 255 by default
    ; MAP_PATCH is much slower than MAP_IMAGE - 20070703
    result = MAP_IMAGE(16*a, Startx, Starty, Xsize, Ysize, COMPRESS=1, SCALE=0.1, LATMIN=latmin, LONMIN=lonmin, LATMAX=latmax, LONMAX=lonmax)
    
    
    IF !D.NAME EQ 'PS' THEN TVLCT, 255, 255, 255, 0
    TV, result, Startx, Starty, XSIZE=Xsize, YSIZE=ysize
    IF !D.NAME EQ 'PS' THEN TVLCT, 0, 0, 0, 0
    MAP_CONTINENTS, /USA
    contourbar, 5*(1+indgen(15)), 16*(1+indgen(15)), position=[0.045,0.1,0.055,0.9], title='dBZ', format='(i2)', /vertical
    
    IF !D.NAME EQ 'PS' THEN DEVICE, /CLOSE
    
    IF !D.NAME EQ 'X' THEN write_gif, ofile + '.gif', tvrd()
    
  endfor
  
endfor

end

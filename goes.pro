function GOES, image
    debug = 0
    ; from http://www.oso.noaa.gov/goes/goes-calibration/gvar-conversion.htm
    ;    b = 15.6854
    ;    m = 5.2285
    ;    radiance = (image*4-b)/m
    ;    c1 = 1.191066e-5 ; [mW/(m2-sr-cm-4)]
    ;    c2 = 1.438833 ; (K/cm-1)
    ;    n = 937.23
    ;    a = -0.386043
    ;    b = 1.001298
    ;    Teff = (c2 * n)/alog(1+(c1*n^3)/radiance)
    ;    zeta = a + b * Teff
    
    if size(image, /tname) eq 'INT' then begin
      ; try again - not sure if image counts are correct . they are supposed to be 0-1023 . that's why I multiplied by 4.
;            data_start = 4115 ; Ch4/Det1
;            lut = read_ascii('~/IDLWorkspace/Default/mcidas/goes13_img_side1_lut.asc', data_start=data_start, num_records=1024)
;            Temp_lut = lut.field1[2,*]
;            ModeA_lut = lut.field1[3,*]
      ;
;            data_start = 3
;            lut = read_ascii('~/IDLWorkspace/Default/mcidas/imager.calibration.txt', data_start=data_start, num_records=256)
;            Temp_lut = lut.field1[0,*]
;            ModeA_lut = lut.field1[1,*]
      ;
;            zeta = float(image)
;            for modeA = 0,255 do begin
;              imodeA = where(image eq modeA, n)
;              if n gt 0 then begin
;                iTemp = where(ModeA_lut eq modeA, n)
;                if n eq 0 then stop
;                zeta[imodeA] = Temp_lut[mean(iTemp)] -!CONST.T0
;              endif
;            endfor
;            if debug gt 0 and n_elements(image) eq 1 then print, 'image=',image, ' zeta = ', zeta
      ; try again, by trial and error I found this matched histograms for 20100820_0015 in IDV and IDL
      ;      zeta = image*3.+17. - !CONST.T0
      ; did a linear regression for first 65000 points of 20100902_0015
      
      ; found similar coefficient and constant for WV channel on GOES13 and GOES15 on 20130614 .
      
      zeta = image*3.2 + 1.9 - !CONST.T0
      ibad = where(image eq 0, nbad)
      if nbad gt 0 then zeta[ibad] = !VALUES.F_NAN
      if debug gt 0 and n_elements(image) eq 1 then print, 'or zeta = ', zeta
      
    endif

    if size(image, /tname) eq 'BYTE' then begin
      ; Try yet again.
      Tscene = 418 - image
      iwarm = where(Tscene ge 242,nwarm)
      if nwarm gt 0 then Tscene[iwarm] = (660 - image[iwarm]) /2.
      zeta = Tscene - !CONST.T0
    endif
    return, zeta
end
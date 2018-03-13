function get_wsi, file
   ; Even though this is fast IDL code, it is not quite as fast as calling compiled program wsi2raw with SPAWN.
    raw = bytarr(3661*1837L, /nozero)
    openr, lun, file, /get_lun
    header = bytarr(192, /nozero)
    readu,  lun, header
    nline = fix(1)
    b3 = bytarr(3, /nozero)
    b1 = byte(1)
    before = byte(1)
    after = byte(1)

    readu, lun, b3
    iend = 0
    
    while ~ iend do begin
      readu, lun, nline
      nel = 0
    
      while (1) do begin
      
        readu, lun, b1
        ecode = ishft(b1, -4)
        dcolr = b1 and 15B
;        print, "ecode ", ecode, " dcolr ", dcolr
        
        case ecode of 
          15: begin
            drun = 0
            end
          14: begin
            drun = byte(1)
            readu, lun, drun
            end
          13: begin
            drun = fix(1)
            readu, lun, drun
            end
              
          else: drun = ecode + 1
            
        endcase
         
;        print, "drun ", drun
        startp = (nline-1) * 3661L + nel
        endp = startp + drun
        raw[startp:endp] = dcolr
        nel = nel+drun
        
        readu, lun, b3
;        print, 'b3 = ', b3
        if b3[0] eq 0 && b3[1] eq 240 && b3[2] eq 12 then break
        
        if b3[0] eq 0 && b3[1] eq 240 && b3[2] eq 2 then begin
          iend = 1
          break
        endif
        point_lun, -lun, a
        point_lun, lun, a-3
        
        
      endwhile
      
    endwhile
    free_lun, lun

    wsi = reform(raw, 3661, 1837)
    wsi = wsi * 5.
    return, wsi

end


pro read_wsi
  close, /all
  ;files = file_search('/data1a/pd/ahijevyc/200107/200107?[23459].22??.MASTER15', count=nfiles)
  files = file_search('/pecan/ahijevyc/LDM_NOWrad/2003/20030706.0400.MASTER15', count=nfiles)
  loadct, 41, file="/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl" ; radar reflectivity
  if !D.NAME eq 'PS' then device, /close, /color, bits_per_pixel=8
  if !D.NAME eq 'X' then device, decomposed=0
  pos = [0.1, 0.1, 0.9, 0.9]


  raw = bytarr(3661*1837L, /nozero)
  for ifile = 0, nfiles-1 do begin
    print, 'decoding ' + files[ifile]

; Sounds like I need to add some comments to the *.pro program.
;Remark that the size of t he header is dependant on the type of the
;machine, ie 32 or 64 bits.-  Eric Defer email on 2/10/2012
;... the fortran code converts the data quite well, but the size
;of the binary file is larger than the actual physical size of bytes to
;store. There is a small header of 4 or 8 bytes (according to the machine) to
;read first before reading the 3661 x 1937 array of nexrad records. This small
;header is not consider in the .pro program that is given as example in
;the software directory available on David's website.

;The header is included by the executable when the program starts
;writing in binary format. Note that a second header - I am not sure we can call
;it header - is also present at the end of file, with the same number of
;bytes. So in my case, the length of the file created by Dave's fortran
;program is 8 + (nx * ny) *4 + 8 bytes if I am correct, where nx * ny is
;the total number of NEXRAD grid boxes, each grid box value being stored
;in 4 bytes. 



    spawn, ['/users/ahijevyc/src/wsi2raw', files[ifile]], /noshell, unit=lun
    readu, lun, raw 
    wsi = reform(raw, 3661, 1837)
    wsi = wsi * 5.
    levels = indgen(16)*5
    free_lun, lun
;    wsi = get_wsi(files[ifile]) ; slower than compiled program wsi2raw.c



    total = n_elements(total) eq 0 ? wsi : total+wsi
    
    title = file_basename(files[ifile])
    
    wsi = congrid(wsi, 1000, 500)
    wsi[where(wsi eq 0)] = !VALUES.F_NAN
    map_set, 20, -100, 0, limit = [25, -120, 48, -73], /lambert,  position=pos, /noborder, /iso, title=title
    tv, map_patch(wsi/5*16, lon0=-130, lon1=-60, lat0=20, lat1=53, xstart=x0, ystart=y0, xsize=xsize, ysize=ysize), x0, y0, xsize=xsize, ysize=ysize
    map_continents, /usa, /countries
    contourbar, levels, levels/5*16

  endfor

;  wsi = reform(total/nfiles, 3661, 1837)
    
;  wsi[where(wsi eq 0)] = !VALUES.F_NAN

;  wsi = wsi[1800:2300,800:990]
;  radius = 3
;  kernel = shift(dist(1+radius*2,1+radius*2),radius,radius) le radius ; this is a circle
;  print, kernel
;  wsi=convol(wsi,kernel)

;  title = ''
   
;  contour, rotate(wsi,7), /cell_fill, xstyle=5, ystyle=5, levels=levels, c_colors=colors, title=title, position=pos
;  map_set, 0, 0, 0, limit = [20, -130, 53, -60], /usa, /cylind, /noerase, position=pos
;  contourbar, levels, colors, format='(I2)'

  if !D.NAME eq 'PS' then device, /close
end

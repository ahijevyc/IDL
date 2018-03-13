; Copied from tuna.mmm.ucar.edu:./IDLWorkspace/Default Dec 2 2020



pro get_a_whole_bunch
  ; use timegen to generatate a range of times with constant time step size.
  juldays = timegen(start=julday(4,1,2009,0,0,0), final=julday(7,31,2009,0,0,0), units="Hours", step_size=24)
  regions = ['20110629']
  regions = ['pecan']
  regions = ['cent_Rockies', 'cent_missvly', 'cent_plains', 'mid_atlantic', 'national', 'new_england', $
    'north_plains', 'south_missvly', 'south_plains', 'southeast', $
    'southwest', 'upper_missvly']
  regions = [ 'cent_Rockies' ] 

  for iregion=0,n_elements(regions)-1 do for i=0, n_elements(juldays)-1 do get_nexrad_composite_png, julday=juldays[i], region=regions[iregion]
end

pro ps2png, psfile, force_new=force_new
  if ~keyword_set(force_new) then force_new=0
  outimg = strmid(psfile,0,strlen(psfile)-7) +'.png' ; remove '_n0q.ps' and add '.png'
  if file_test(outimg) eq 1 and ~force_new then begin
    print, 'ps2png:'+outimg +' exists already.'
    return
  endif
  cmd = 'convert -density 140% -flatten '+psfile+' '+outimg
  print, cmd
  spawn, cmd ; Something wrong with running this here. bad parameters to zlib etc.  do outside idl
  print, 'created '+outimg
end

pro get_nexrad_composite_png, julday=julday, type=type, region=region

  if ~keyword_set(julday) then julday = julday(6, 5, 2019, 5, 25, 0)

  if ~keyword_set(region) then region = 'cent_plains' ; string(year,month,day,format='(I4.4,I2.2,I2.2)')

  case region of
    '20110629' : begin
      latmin=31.5
      latmax=41.
      lonmin=-105.2
      lonmax=-90.6
      loncent=-96
    end
    '20020613' : begin
      latmin=33.5
      latmax=39.
      lonmin=-103.2
      lonmax=-96.6
      loncent=-96
    end
    '20111114' : begin
      latmin=36
      latmax=48.
      lonmin=-90
      lonmax=-60
      loncent=-70
    end
    '19980726' : begin
      latmin=34.7
      latmax=43.1
      lonmin=-105.
      lonmax=-93.
    end
    'cent_Rockies' : begin
      latmin=36.
      latmax=45.0
      lonmin=-115.4
      lonmax=-100
    end
    'cent_missvly' : begin
      latmin=34.1
      latmax=42.0
      lonmin=-95.4
      lonmax=-83.9
    end
    'cent_plains' : begin
      latmin=34.7
      latmax=43.2
      lonmin=-105
      lonmax=-92.6
    end
    'custom' : begin
      latmin=33.
      latmax=40
      lonmin=-103
      lonmax=-92.6
    end
    'mid_atlantic' : begin
      latmin=34
      latmax=42.3
      lonmin=-87.8
      lonmax=-70.2
      loncent = -90
    end
    'national' : begin
      latmin=24
      latmax=49
      lonmin=-123.4
      lonmax=-67.7
      loncent = -100
    end
    'new_england' : begin
      latmin=38.8
      latmax=47.5
      lonmin=-83.3
      lonmax=-67.5
      loncent = -100
    end
    'north_plains' : begin
      latmin=39.7
      latmax=49.8
      lonmin=-105.3
      lonmax=-90.5
    end
    'pecan' : begin
      latmin=34.5
      latmax=42
      lonmin=-105
      lonmax=-94.
    end
    'southeast' : begin
      latmin=24.5
      latmax=35
      lonmin=-90
      lonmax=-75.5
    end
    'south_missvly' : begin
      latmin=28.5
      latmax=36.6
      lonmin=-96
      lonmax=-85
    end
    'south_plains' : begin
      latmin=25.8
      latmax=37.7
      lonmin=-107
      lonmax=-90.9
    end
    'southeast' : begin
      latmin=25.8
      latmax=37.7
      lonmin=-87
      lonmax=-77.9
    end
    'southwest' : begin
      latmin=31.5
      latmax=42.
      lonmin=-124.5
      lonmax=-109
    end
    'upper_missvly' : begin
      latmin=40.1
      latmax=49.6
      lonmin=-98.5
      lonmax=-85.3
    end
  endcase


  ; plot national radar composite in custom projection (see map_set bwlow) and domain.
  ; if the file does not exist locally, then download from web.
  ; http://mesonet.agron.iastate.edu' + '/archive/data/'+datedir+'GIS/uscomp/n0q'...

  localdir = '/Users/ahijevyc/LDM_NOWrad/'
  datedir = string(julday, format='(c(CYI4.4,"/",CMOI2.2,"/",CDI2.2,"/"))')
  localdir = localdir + datedir

  ; n0r images are 5dBZ x 0.01 * 0.01 degrees and n0q are 0.5dBZ x 0.005 * 0.005 degrees
  ; Thanks Li, Xingong, lixi@ku.edu
  ; email on August 11, 2012 10:20 AM
  ; archive includes n0q on 2010/11/13 and later, otherwise, just n0r
  if ~keyword_set(type) then type = 'n0q' ; n0q may be higher resolution (0.5dbz) but color scale is horrible for images prior to
  ;   1945 UTC Feb 12, 2011 - yellow for 20dBZ? this is fixed down below.
  if datedir lt '2010/11/13/' then type = 'n0r'



  ;====================================================

  pos=[0.1,0.2,0.9,0.9]
  if !D.NAME eq 'X' then begin
    !P.CHARSIZE  = 2.0
    !P.CHARTHICK = 2
  endif
  if !D.NAME EQ 'PS' THEN begin
    !P.CHARTHICK=3
    !P.CHARSIZE=1
  endif
  ;loadct, 41, file='/users/ahijevyc/IDLWorkspace/Default/resource/colors/colors1.tbl', ncolors=22
  file = type+'_'+string(julday, format='(c(CYI4.4,CMOI2.2,CDI2.2,CHI2.2,CMI2.2))')+'.png'
  psfile = localdir + region+'_'+string(julday,type, format='(c(CYI4.4,CMOI2.2,CDI2.2,CHI2.2,CMI2.2),"_",A)') + '.ps'

  if !D.NAME eq 'PS' and file_test(psfile) eq 1 then begin
    print, 'found '+psfile+'. Skipping.'
    ps2png, psfile
    return
  endif
  if file_test(localdir) eq 0 then file_mkdir, localdir
  if !D.NAME eq 'PS' then device, /close, filename=psfile, /color, bits_per_pixel=8, ysize=5, xsize=7, /inches
  loadct, 39, /silent

  if !D.NAME eq 'X' then device, decomposed =1
  erase, 'FFFFFF'x
  map_set, 20,n_elements(loncent)?loncent:mean([lonmin,lonmax]), 0, limit = [latmin,lonmin, latmax,lonmax], /noborder, $
    position=pos, /iso, /cyli, /noerase
  if !D.NAME eq 'X' then device, decomposed =0


  URL = 'http://mesonet.agron.iastate.edu' + '/archive/data/'+datedir+'GIS/uscomp/'+file
  localfile = localdir+file
  if file_test(localfile) eq 0 then begin
    file_mkdir, localdir
    oUrl = OBJ_NEW('IDLnetUrl')
    result = oUrl->Get(filename=localfile,url=url)
    obj_destroy, oUrl
  endif
  dummy = query_png(localfile, info)
  nx = info.dimensions[0]
  ny = info.dimensions[1]
  if info.has_palette ne 1 then stop

  read_png, localfile, png, r, g, b, transparent=transparent, /verbose
  ;  print, r[0:30]
  ;  print, g[0:30]
  ;  print, b[0:30]
  ;
  ; override color table in png file.
  ; When I tried rip_mosaic with June 18-22, 2011, it was black and white. Perhaps only works on older plots (n0r).
  ;  if (type eq 'n0r') then rip_mosaic, r, g, b

  ; n0q may be available
  ; prior to 1945 UTC Feb 12, 2011 but color scale is horrible- yellow for 20dBZ?
  if type eq 'n0q' && julday lt julday(2,12,2011,19,45,0) then n0q_ctable, r, g, b

  tvlct, r, g, b
  ;      print, r[0:30]
  ;      print, g[0:30]
  ;      print, b[0:30]
  url = strmid(url,0,strlen(url)-3)+'wld'
  localfile = strmid(localfile,0,strlen(localfile)-3)+'wld'
  if file_test(localfile) eq 0 then begin
    file_mkdir, localdir
    oUrl = OBJ_NEW('IDLnetUrl')
    result = oUrl->Get(filename=localfile,url=url)
    obj_destroy, oUrl
  endif
  
  
  u=read_ascii(localfile)
  wld = double(u.field1)
  ;  A world file file is a plain ASCII text file consisting of six values separated by newlines.
  ; The format is:
  ;
  ; pixel X size
  ; rotation about the Y axis (usually 0.0)
  ; rotation about the X axis (usually 0.0)
  ; negative pixel Y size
  ; X coordinate of upper left pixel center
  ; Y coordinate of upper left pixel center
  
  dx = wld[0]
  dy = -wld[3]
  latmax0=wld[5]+dy/2.
  lonmin0=wld[4]-dx/2.
  lonmax0=lonmin0+nx*dx
  latmin0=latmax0-ny*dy
  
  
  t = map_image(png, startx, starty, xsize, ysize, latmin=latmin0, latmax=latmax0, lonmin=lonmin0, lonmax=lonmax0, scale=0.05, compress=1)
  tvlct, oldct, /get
  tvlct, 255,255,255,0
  tv, t, startx, starty, xsize=xsize, ysize=ysize
  tvlct, oldct

  map_grid, latdel=5, londel=5, /box
  map_continents, /usa


  ; title could also have region in it. Or be in UTC.
  title = string(julday, region, format='(c(CYI4.4,"/",CMOI2.2,"/",CDI2.2," ",CHI2.2,":",CMI2.2), " UTC", 2x,A)')
  if region eq '19980726' then title = string(julday-5d/24, format='(c(CYI4.4,"/",CMOI2.2,"/",CDI2.2," ",CHI2.2,":",CMI2.2), " Central (UTC -5:00)")')
  xyouts, (pos[2]+pos[0])/2., pos[3] + (1-pos[3])*0.5, title, /norm, align=0.5, charsize=!P.CHARSIZE*1.5
  case type of
    'n0r' : type_desc = type+' base reflectivity dBZ'
    'n0q' : type_desc = type+' base reflectivity 0.5 dBZ resolution'
    else:stop
  endcase
  
  ; When doing Lance's plots for June 18-22, 2011, I needed to set vertical to zero. Maybe I had it at 1 for Stan's RIP-style plots.
  ; If you get black and white plots, make sure you are not using the rip_mosaic procedure above.
  vertical = 0
  colorbar_POSITION = vertical ? [!X.WINDOW[1]+0.4*(1.-!X.WINDOW[1]), !Y.WINDOW[0], !X.WINDOW[1]+0.5*(1.-!X.WINDOW[1]),!Y.WINDOW[1]] : $
    [pos[0]+0.2*(pos[2]-pos[0]),pos[1]-0.11,pos[0]+0.8*(pos[2]-pos[0]),pos[1]-0.09]
  if type eq 'n0r' then begin
    levels = indgen(16)*5
    colors = indgen(16)+6
    contourbar, levels, colors, TITLE=type_desc, position=colorbar_position, FORMAT='(I0)', charsize=!P.CHARSIZE*0.9, color1=[255,255,255],vertical=vertical
  endif
  if type eq 'n0q' then begin
    ; .r  '~/IDLWorkspace/Default/colorbar.pro'
    colorbar, position=colorbar_position,title=type_desc, range=[-30,100], divisions=13 ,FORMAT='(I0)', charthick=1, charsize=!P.CHARSIZE*0.9
  endif

  xyouts, !X.WINDOW[1],0.032,"data from Iowa State U. Dept of Agronomy!C"+file_dirname(url)+"!Cplotted "+systime()+' ahijevyc@ucar.edu',$
    charthick=!P.CHARTHICK*0.4, align=1, /norm, charsize=!P.CHARSIZE*0.4
  if !D.NAME eq 'PS' then begin
    device, /close
    print, 'created '+psfile
    ps2png, psfile
  endif

  ;  http://mesonet.agron.iastate.edu/docs/nexrad_composites/
  ;  The IEM receives a feed of NEXRAD Level III products from the Unidata Internet Data Distribution.
  ;  This feed includes the Base Reflectivity (N0R) and Net Echo Top (NET) products which are saved to disk for later processing.
  ;  Every 5 minutes, a script runs a GEMPAK program called nex2img.
  ;  nex2img searches the on-disk data for current (within 15 minutes of runtime) products and then composites them into a large image.
  ;  During the summertime, only RADARs in precipitation mode are considered for compositing.
  ;
  ;  The result of the nex2img process is a simple raster image with size of 6000 pixels in width by 2600 pixels in height.
  ;  This raster image is then compared with a similiar raster of NET to remove any suspected false echos.
  ;  In the wintertime, this comparison is skipped for most of the country.
  ;  The final result is a PNG formatted image looking much like the example image shown above.
  !P.CHARSIZE=1
  !P.CHARTHICK=1
end

pro rip_mosaic, r, g, b
  print, 'rip_mosaic'
  ; kept in /pecan/ahijevyc/LDM_NOWrad/2002/rip_mosaic.tbl
  ;!
  ;! Color table for RIP Reflectivity composite
  ;!
  ;!Color name    Abrev   Red  Green   Blue  X color name
  ;WHITE           WHI    255    255    255  white
  ;CYAN            CYN    128    255    255  cyan
  ;LTBLUE          LTB     64    192    255  light.blue
  ;BLUE            BLU      0      0    255  blue
  ;DKBLUE          DKB     64    128    192  dark.blue
  ;LTGREEN         LTG     64    255     64  light.green
  ;DKGREEN         DKG      0    192      0  dark.green
  ;LTORANGE        LTO    255    224     64  light.orange
  ;ORANGE          ORG    255    160     64  orange
  ;RED             RED    255     64      0  red
  ;MDRED           MDR    255     25     25  med.red
  ;DKRED           DKR    192     25     25  dark.red
  ;PLUM                   160     64     96
  ;VIOLET                 160     64    160  violet
  ;LTVIOLET               192    128    192  light.violet
  ;WHITE           WHI    255    255    255  white
  v = bytarr(255,3)
  v[0,*] =   [255,    255,    255];  white
  v[1,*] =   [128,    255,    255];  cyan
  v[2,*] =    [64,    192,    255];  light.blue
  v[3,*] =     [0,      0,    255];   blue
  v[4,*] =    [64,    128,    192];  dark.blue
  v[5,*] =    [64,    255,     64];  light.green
  v[6,*] =     [0,    192,      0];  darkgreen
  v[7,*] =   [255,    224,     64];  light.orange
  v[8,*] =   [255,    160,     64];  orange
  v[9,*] =   [255,     64,      0];  red
  v[10,*] =  [255,     25,     25];  med.red
  v[11,*] =  [192,     25,     25];  dark.red
  v[12,*] =  [160,     64,     96];
  v[13,*] =  [160,     64,    160];  violet
  v[14,*] =  [192,    128,    192];  light.violet
  v[15,*] =  [255,    255,    255];  white
  v = shift(v, 6) ; don't know why, but first 6 levels are junk in png
  r = v[*,0]
  g = v[*,1]
  b = v[*,2]
end


pro n0q_ctable, r, g, b
  restore, '~/share/idl/n0q_rgb.sav'
  return
end

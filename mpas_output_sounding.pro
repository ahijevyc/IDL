function cosmic_sounding, file
  ncid = ncdf_open(file)
  TempC = getvar(ncid, 'Temp')
  Vp = getvar(ncid, 'Vp')
  Press = getvar(ncid, 'Pres')
  lat = getvar(ncid,'Lat')
  lon = getvar(ncid,'Lon')
  ncdf_attget, ncid, /global, 'year', year
  ncdf_attget, ncid, /global, 'month', month
  ncdf_attget, ncid, /global, 'day', day
  ncdf_attget, ncid, /global, 'hour', hour
  ncdf_attget, ncid, /global, 'minute', minute
  ncdf_attget, ncid, /global, 'second', second
  ncdf_close, ncid
  time = julday(month, day, year, hour, minute, second)


  ; about 10% of the wetProfs have an error in
  ; the water vapor pressure (it suddenly drops to zero somewhere in the
  ; lowest 8km). If you discard the ones with Vp<1.e-5 you should kick
  ; most of them out.

  ;This error occurs in wetProfs from ALL retrievals (cosmic2013,
  ;cosmicrt, cosmic)
  if total(Vp lt 1.e-5) gt 0 then stop


  RH = Vp/esat(TempC)*100.
  TempK = TempC + !CONST.T0
  TdK = rh2tdew(TempK,RH)
  return, {T:TempK, Td:TdK, Press:Press, lat:lat, lon:lon, time:time}
end

pro mpas_output_sounding
  ; Only works on mpas.output.* files.

  if !D.NAME eq 'PS' then device, /close, /color, bits=8
  pressure_base = [99679.7671753452, 98958.7179167597, 98073.8763669486, 97023.5740939265,95807.2233995675, 94425.3726222841, 92879.750377646, 91173.2972887114,89310.1838935226, 87295.8135942779, 85136.8097301372, 82840.9861111009,80417.3006401791, 77875.791969982, 75227.4994813933, 72484.367228339,69659.132854945, 66765.2028498076, 63816.5158463071, 60827.3959972158,57812.3987357378, 54786.1514734919, 51763.1919696394, 48757.8072264241,45783.8758183425, 42854.7165393213, 39982.946147416, 37180.3487860502,34457.7593465477, 31824.9626237838, 29290.6097505894, 26862.1533978118,24545.8097373381, 22346.5415980646, 20268.0565460153, 18312.8227895728,16482.1014577639, 14775.9957423308, 13193.516970864, 11732.6663078735,10390.5296509925, 9168.87686187817, 8078.07688373948, 7116.57944802507,6269.52458677235, 5523.28802982832, 4865.86902538226, 4286.69647377236,3776.45813281423, 3326.9502783455, 2930.94544197601, 2582.07607186362,2274.73218064325, 2003.97125845547, 1765.43893495823 ]

  pressure_p =[1669.09408032288, 1765.33497571337, 1881.05757860653, 2015.77807100055,2170.69944966266, 2346.90612248994, 2543.38801116629, 2758.87775235253,2993.04929680295, 3245.43519211024, 3512.53718819377, 3787.8745431979,4064.62116352578, 4336.29488190302, 4596.50443322949, 4839.09686302578,5058.57251387401, 5249.90082711058, 5407.9652085541, 5528.0495320681,5606.29196740638, 5638.8449471641, 5624.18506497416, 5564.83560467035,5461.22140125843, 5308.80411639596, 5104.43554391484, 4849.10339819621,4549.24235014728, 4213.60295982463, 3847.54424830062, 3452.01848770289,3028.93532429031, 2582.62484780582, 2114.18647585392, 1630.5726660232,1150.23949356035, 697.634540793037, 298.65433512169, -40.0531498663465,-336.813986584837, -594.257659608428, -785.996372399109,-895.837456778319, -945.994799216948, -961.503054962084,-951.163159985518, -920.031091883471, -875.006477055021,-820.499846214858, -760.94944584268, -700.521606537737,-642.093338233945, -586.348424402221, -533.794651540337 ]

  theta = [290.409504873704, 290.414095330959, 290.479589972526, 291.200646916218,292.849023682195, 294.471338478207, 296.200870196722, 298.400185959752,301.024145521815, 303.888028961917, 306.251735673472, 307.807314735976,308.90526698893, 309.88347906739, 310.682821157061, 311.543740178678,312.566404363943, 313.669761559306, 314.704558430643, 315.846537076142,317.086254194765, 318.274188607, 319.911165417048, 322.180498684863,324.218886725832, 325.656618419556, 327.021026002408, 328.474484695449,330.867225688643, 333.605171809315, 336.395975111441, 338.33801300424,340.423045946937, 341.881295880381, 342.319317738186, 343.427903041687,346.844603413711, 353.776789725643, 365.942096478497, 376.744100506965,382.964837814069, 393.337650613388, 416.093879947324, 449.439409819306,472.656718605721, 499.808929611422, 525.672586655819, 560.835099111286,591.107451993751, 626.88099376537, 661.532368087163, 694.079292451847,728.416803061683, 762.338453799729, 793.967506814041 ]

  rh =[81.3363988755792, 82.5408310488429, 84.7576951682584, 87.6884620915274,91.414733114904, 95.1916643181484, 97.8731606833515, 97.962632680013,72.3460432634592, 27.4165930624288, 15.9252830392214, 10.4489986557315,5.62973623422196, 2.87794236687881, 3.53742612259505, 10.3784816755781,19.4818224517495, 30.99506811102, 49.8817293438578, 66.7886790396884,67.5258510052755, 51.1537591832415, 30.3592368355723, 11.1725172827251,3.55844284582527, 5.85700992968898, 11.5842160791897, 18.2416989984367,22.6003771350865, 20.267041466144, 15.3136961597958, 9.64556409596083,11.7902287058392, 17.188749013452, 17.1073461161011, 21.5613439861222,34.9069556461113, 43.5517139255111, 31.4546627176049, 61.2686498650923,93.116142935159, 99.2272674661658, 55.3641465401525, 25.1540005158473,9.50997196641989, 5.99799473038957, 5.22169099659576, 3.08465004282159,1.46372364216967, 0.870327094554017, 0.605883612470646,0.406985772918287, 0.319784277417369, 0.238336980171881, 0.155769616365893 ]
  atmos_const


  mpas = mpas_mesh('mpas_ep')

  ; for GFS - warm bias is at 284E, 18.5S
  ; for MPAS 850 cold bias at 260E, 0S
  lon0 = 259.3 & lat0 = -2
  lon0 = 260.3
  ; lon0= 230 & lat0 = -70
  ;lon0=360-112.3 & lat0=-6.7 & cosmic_file='/glade/work/ahijevyc/cosmic/wetPrf/2014.242/wetPrf_C001.2014.242.21.45.G29_2014.2860_nc'
  ;lon0=360-101.8 & lat0=-2.6 & cosmic_file='/glade/work/ahijevyc/cosmic/wetPrf/2014.241/wetPrf_C002.2014.241.07.42.G21_2014.2860_nc'
  ;lon0=360-100.7 & lat0=-2. & cosmic_file='/glade/work/ahijevyc/cosmic/wetPrf/2014.241/wetPrf_C002.2014.241.06.11.G32_2014.2860_nc'
  ;lon0=360-99.3 & lat0=-1.2 & cosmic_file='/glade/work/ahijevyc/cosmic/wetPrf/2014.244/wetPrf_C001.2014.244.08.10.G07_2014.2860_nc'
  ; lon0 = 270 & lat0=26.5
  
  search_radius_km=15
  icells = mpas_neighbors(mpas_nearest_cell(lon0, lat0, mpas),mpas.lonCell,mpas.latCell,mpas.nEdgesOnCell,mpas.cellsOnCell, range=search_radius_km)
  icells = icells.icell
  title = "within "+string(search_radius_km, mean(mpas.lonCell[icells]), mean(mpas.latCell[icells]), format='(I0,"km of ",F0.1,"E, ",F5.1,"N")')
  files = file_search('/glade/p/nmmm0024/'+mpas.name+'/2014082500/mpas.output*.nc', count=nfiles)
  files = file_search('/glade/scratch/mpasrt/2014/2014082500/mpas.output*.nc', count=nfiles)
  if nfiles eq 0 then print, 'No files found!'
  title = title + '  ' +strmid(file_dirname(files[0]),strpos(files[0],'scratch')+8)

  for ifile = 0, nfiles-1 do begin
    skewt, [-35, 35], title=title
    file = files[ifile]
    ipos = strpos(file_basename(file),".20")
    year = strmid(file_basename(file),ipos+1,4)
    month = strmid(file_basename(file),ipos+6,2)
    day = strmid(file_basename(file),ipos+9,2)
    valid_time = julday(month,day,year,0,0,0)
    col_dewpt = [50, 255.*(ifile+1)/nfiles, 50]
    col_t = [20+235.*(ifile+1)/nfiles, 50, 150.-(ifile*10)]
    thick = 3.

    i201 = strpos(file,'/201',/reverse_search)
    init_file = strmid(file,0,i201+1)+strmid(file,i201+24,4)+strmid(file,i201+29,2)+$
      strmid(file,i201+32,2)+strmid(file,i201+35,2)+'/'+file_basename(file)

    foreach file2,[file,init_file],i2 do begin
      if file_test(file2) ne 1 then begin
        print, file2 + ' not found. no comparison to '+init_file
        continue
      endif
      ncid = ncdf_open(file2)
      ncdf_diminq, ncid, ncdf_dimid(ncid,"nVertLevels"), junk, nVertLevels ; get # of vertical levels for "count" array
      foreach icell, icells, iicel do begin
        count = [nVertLevels, 1, 1]
        offset = [0,icell,0]
        if ncdf_varid(ncid,'pressure') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'pressure'), pressure, count =count, offset=offset else begin
          ncdf_varget, ncid, ncdf_varid(ncid,'pressure_base'), pressure_base, count =count, offset=offset
          ncdf_varget, ncid, ncdf_varid(ncid,'pressure_p'), pressure_p, count =count, offset=offset
          pressure = pressure_base + pressure_p
        endelse
        p = pressure/100.
        ncdf_varget, ncid, ncdf_varid(ncid,'theta'), theta, count =count, offset=offset
        if ncdf_varid(ncid,'rh') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'rh'), rh, count =count, offset=offset
        if ncdf_varid(ncid,'relhum') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'relhum'), relhum, count =count, offset=offset
        if ncdf_varid(ncid,'qv') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'qv'), qv, count =count, offset=offset
        if ncdf_varid(ncid,'qc') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'qc'), qc, count =count, offset=offset
        if ncdf_varid(ncid,'qi') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'qi'), qi, count =count, offset=offset
        if ncdf_varid(ncid,'rthcuten') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'rthcuten'), rthcuten, count=count, offset=offset ; potential temp tendency from CPS
        if ncdf_varid(ncid,'rthblten') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'rthblten'), rthblten, count=count, offset=offset ; from PBL
        if ncdf_varid(ncid,'rthratensw') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'rthratensw'), rthratensw, count=count, offset=offset ; from SW rad
        if ncdf_varid(ncid,'rthratenlw') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'rthratenlw'), rthratenlw, count=count, offset=offset ; from LW rad
        ncdf_varget, ncid, ncdf_varid(ncid,'w'), w, count =[count[0]+1,count[1:2]], offset=offset
        if ncdf_varid(ncid,'cldfrac') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'cldfrac'), cldfrac, count =count, offset=offset
        if ncdf_varid(ncid,'tend_theta') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'tend_theta'), tend_theta, count =count, offset=offset
        if ncdf_varid(ncid,'euler_tend_theta') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'euler_tend_theta'), euler_tend_theta, count =count, offset=offset
        ncdf_varget, ncid, ncdf_varid(ncid,'rt_diabatic_tend'), rt_diabatic_tend, count =count, offset=offset
        if ncdf_varid(ncid,'refl10cm_max') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'refl10cm_max'), refl10cm_max, count=count[1:2], offset=offset[1:2]
        ncdf_varget, ncid, ncdf_varid(ncid,'hfx'), hfx, count=count[1:2], offset=offset[1:2]
        if ncdf_varid(ncid,'qfx') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'qfx'), qfx, count=count[1:2], offset=offset[1:2]
        if ncdf_varid(ncid,'lh') ne -1L then ncdf_varget, ncid, ncdf_varid(ncid,'lh'), lh, count=count[1:2], offset=offset[1:2]
        if ncdf_varid(ncid,'precipw') ne -1 then ncdf_varget, ncid, ncdf_varid(ncid,'precipw'), precipw, count=count[1:2], offset=offset[1:2]
        ncdf_varget, ncid, ncdf_varid(ncid,'surface_pressure'), surface_pressure, count =count[1:2], offset=offset[1:2]
        surface_pressure = surface_pressure/100.
        TK = theta *(p/1000.)^(!ATMOS.Rd/!ATMOS.Cpd)
        TC = TK-!CONST.T0
        if n_elements(qv) ne 0 then begin
          rh_from_qv = 100.*1000.*qv/mixr_sat(TK,p)
          TDK = rh2tdew(TK,rh_from_qv)
        endif else TDK = rh2tdew(TK,relhum*100) ; unfortunately relhum=0 for initial time
        ;      qv_from_rh = mixr_sat(TK,p)*RH/100. ; think this is wrong - RH is wrt ice not water
        plot_skewt, TK-!CONST.T0, TDK-!CONST.T0,p,thick=thick,linestyle=1*(file2 ne init_file)
        if (file2 ne init_file) then begin
          ;xyouts, replicate(!X.CRANGE[1]+1,n_elements(qc)), p, string((qc+qi)*1000.),charsize=0.24,charthick=0.25
          ;xyouts, !X.CRANGE[1]+2, 10^!Y.CRANGE[0], string(refl10cm_max),charsize=0.25
          if 0 then begin
            scale = 5./0.005
            oplot, !X.CRANGE[1]+5+rt_diabatic_tend*scale, p, noclip=1
            if n_elements(tend_theta) gt 0 then begin
              oplot, !X.CRANGE[1]+5+tend_theta*scale, p, noclip=1, color=90
              xyouts, !X.CRANGE[1]+5, surface_pressure+45, '!Ctend_theta', align=0.5, color=90, charsize=0.5
            endif
            xyouts, !X.CRANGE[1]+5, surface_pressure+45, 'rt_diabatic_tend', align=0.5, charsize=0.4

            oplot, !X.CRANGE[1]+15+w*50., [surface_pressure,p+(p-shift(p,-1))/2.] , noclip=1
            xyouts, !X.CRANGE[1]+15, surface_pressure+60, 'w', align=0.5
            oplot, replicate(!X.CRANGE[1]+15,2), [surface_pressure,10.^!Y.CRANGE[1]], noclip=1, linestyle=1
            xyouts, !X.CRANGE[1]+1, 1220, string(hfx,lh,format='("hfx=",F7.2," lh=",E8.1)'),charsize=0.7
            if n_elements(precipw) gt 0 then xyouts, !X.CRANGE[1]+1, 1220, string(precipw,format='("!Cprecipw=",F5.2)'),charsize=0.7
          endif
          ; Plot cloud fraction.
          loadct,60,/silent
          xs = -1.5*float([iicel,iicel+1,iicel+1,iicel])/n_elements(icells)
          for ip=0,n_elements(cldfrac)-2 do polyfill, !X.CRANGE[1]+xs, p[ip]+[(p[ip+1]-p[ip])/2.,(p[ip+1]-p[ip])/2.,(p[ip-1]-p[ip])/2.,(p[ip-1]-p[ip])/2.], color=255-cldfrac[ip]*255, /data, noclip=0
          loadct,0,/silent
          xyouts, !X.CRANGE[1]-0.2, 1050, 'cloud fract', align=1, orientation=90, charsize=0.5
          scale = 1.5e5
          offset = 15
          ref = 5. ; K/day
          ref = ref/3600/24. ; convert from K/day to K/s
          oplot, !X.CRANGE[1]+offset+[-ref,-ref,ref,ref]*scale, [!Y.CRANGE[1], surface_pressure, surface_pressure, !Y.CRANGE[1]], noclip=1
          ; vertical reference lines
          for xref=-ref,ref,ref do xyouts, !X.CRANGE[1]+offset+xref*scale, surface_pressure+60, string(xref*3600*24,format='(I2,"K day!E-1!N")'), align=0.5, charsize=0.5
          loadct,39,/silent
          oplot, !X.CRANGE[1]+offset+rthcuten*scale, p, noclip=1, color=90
          xyouts, !X.CRANGE[1]+offset, surface_pressure+150, '!C!C!Crthcuten', align=0.5, color=90, charsize=.7
          oplot, !X.CRANGE[1]+offset+rthblten*scale, p, noclip=1, color=170
          xyouts, !X.CRANGE[1]+offset, surface_pressure+150, '!C!Crthblten', align=0.5, color=170, charsize=.7
          oplot, !X.CRANGE[1]+offset+rthratensw*scale, p, noclip=1, color=250
          xyouts, !X.CRANGE[1]+offset, surface_pressure+150, '!Crthratensw', align=0.5, color=250, charsize=.7
          oplot, !X.CRANGE[1]+offset+rthratenlw*scale, p, noclip=1, color=50
          xyouts, !X.CRANGE[1]+offset, surface_pressure+150, 'rthratenlw', align=0.5, color=50, charsize=.7
          loadct,0,/silent

        endif
      endforeach ; cell near target
      ncdf_close,ncid

    endforeach ; forecast file

    ; little globe plot with asterisk at sounding location
    map_set, /cylind, limit=[-60,-180,60,180],/cont, position=[0,0.66,0.25,0.82], /noerase
    oplot, mpas.lonCell[icells], mpas.latCell[icells], psym=6, color=100, noclip=1, symsize=0.34, thick=0.5

    if n_elements(cosmic_file) gt 0 then begin
      cosmic = cosmic_sounding(cosmic_file)
      igood = where(finite(cosmic.T) and finite(cosmic.Press) and abs(cosmic.time-valid_time) le 0.5d,ngood)
      if ngood gt 0 then plot_skewt, cosmic.T[igood]-!CONST.T0, cosmic.Td[igood]-!CONST.T0, cosmic.Press[igood], col_t=[100,100,255], col_dewpt=[100,100,255]
      igood = where(finite(cosmic.lon) and finite(cosmic.lat) and finite(cosmic.T), ngood)
      tvlct, transpose([100,100,255]), 0
      oplot, cosmic.lon[igood[0:*:10]], cosmic.lat[igood[0:*:10]], color=0, noclip=1,psym=2, symsize=0.05
      xyouts, min(cosmic.lon,/nan), min(cosmic.lat,/nan), string(cosmic.time,format='("!C",C(CYI4.4,CMoI2.2,CDI2.2,"!C",CHI2.2,":",CMI2.2))'), charsize=0.34, align=1
      tvlct, transpose([0,0,0]), 0
    endif
    xyouts, !X.WINDOW[0], !Y.WINDOW[0], "!Cforecast (dotted) =!C"+strmid(file,i201+1)+$
      "!Canalysis (solid, if available) =!C"+ strmid(init_file,i201+1), charsize=!D.NAME eq 'PS' ? 0.45 : 1, /norm

  endfor

  if !D.NAME eq 'PS' then device, /close
end

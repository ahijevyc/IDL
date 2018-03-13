pro drops_SST_10m
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/'
  subpath='analysis/entrainment10_ice/0.5rev_fallout/'
  SST_type = 'Reynolds' ; 'Reynolds' or 'JPL 1-km'
  nolabel=0
  if !D.NAME eq 'X' then !P.CHARSIZE=2 else !P.CHARSIZE=1
  !P.THICK=1
  psfile = basedir+subpath+'SST_10m.ps'
  if !D.NAME eq 'PS' then device, /close, ysize=5.5, xsize=6.5, /inches, filename=psfile
  stages = ['non-developing','developing_gt2days_prior','developing_le2days_prior','developed']
  
  
  dr = 50
  rmin = 0
  rmax = 500
  nbins = (rmax-rmin)/dr
  
  ; structure to hold data and description of a variable.
  struct = {name:'', range:[0d,0d], title:'', mean:replicate(!VALUES.F_NAN, nbins), serr:replicate(!VALUES.F_NAN, nbins)}
  
  ; Create structure and define some metadata for each field.
  dT_struct = struct
  dT_struct.name = 'dT'
  dT_struct.range = [-2,5]
  dT_struct.title = SST_type + ' SST - 10m air T (!Z(00B0)C)'
  
  spd_struct = struct
  spd_struct.name = 'spd'
  spd_struct.range = [0,15]
  spd_struct.title = '10m wind speed (ms!E-1!N)'
  
  Tflux_struct = struct
  Tflux_struct.name = 'Tflux'
  Tflux_struct.range = [-5,40]
  Tflux_struct.title = 'windspeed*(SST-T!D10m!N) (ms!E-1!N !Z(00B0)C)'
  
  qflux_struct = struct
  qflux_struct.name = 'qflux'
  qflux_struct.range = [-0.03,0.12]
  qflux_struct.title = 'windspd*(q!Dsat,SST!N-q!D10m!N) (ms!E-1!N !Z(00B0)C)'
  
  ; Array of structures (remember it is an array, so /str keyword won't work with help, fields)
  fields = [dT_struct, spd_struct, Tflux_struct, qflux_struct]
  
  ; # columns = number of fields
  ; # rows = number of development stages
  !P.MULTI = [0,n_elements(fields),n_elements(stages)]
  for istage = 0, n_elements(stages)-1 do begin
    stage = stages[istage]
    file = file_search(basedir+subpath+stage+'_00010m_IR10km_scatter.txt', count=nfiles)
    get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, GFS_mean, other=other

    if SST_type eq 'Reynolds' then SST_C = other.SST_Reynolds - !CONST.T0
    if SST_type eq 'JPL 1-km' then SST_C = other.SST - !CONST.T0
    SST_air_diff = SST_C - TempC ; see emails to Chris D. Dec 5-7 2012 about Reynolds vs. 1km JPL SST (we settled on Reynolds)
    spd_drops = sqrt(other.U_earth_rel^2. + other.V_earth_rel^2.)
    Tflux = SST_air_diff * spd_drops
    RV=461.5d  ; gas constant water vapor = R*/M_H2O
    RD=287.04d ; gas constant dry air = R*/M_dry
    EPS=RD/RV
    qsat_SST = EPS*ESAT(SST_C)/(P-ESAT(SST_C)) ; ESA() returns hPA; P better be hPa too.
    q = EPS*ESAT(DwptC)/(P-ESAT(DwptC))
    dq = qsat_SST - q
    qflux = dq * spd_drops
    
    t_cvfs = replicate(!VALUES.F_NAN,nbins)
    n      = replicate(0,nbins)
    x      = replicate(0,nbins)
    
    confidence_level=0.9
    for ibin = 0, nbins-1 do begin
      irange = ibin*dr
      range = [irange,irange+dr]
      x[ibin] = mean(range)
      min_distance_km = range[0]
      max_distance_km = range[1]
      in = where(distance_km ge min_distance_km and distance_km lt max_distance_km, nin)
      if nin gt 0 then begin
        n[ibin]        = nin
        
        fields[0].mean[ibin] = mean( SST_air_diff[in], /nan)
        fields[1].mean[ibin] = mean(    spd_drops[in], /nan)
        fields[2].mean[ibin] = mean(        Tflux[in], /nan)
        fields[3].mean[ibin] = mean(        qflux[in], /nan)
        
        if nin gt 1 then begin
          t_cvfs[ibin]        = t_cvf((1-confidence_level)/2.,nin-1)
        
          fields[0].serr[ibin] = stddev( SST_air_diff[in], /nan) / sqrt(nin)
          fields[1].serr[ibin] = stddev(    spd_drops[in], /nan) / sqrt(nin)
          fields[2].serr[ibin] = stddev(        Tflux[in], /nan) / sqrt(nin)
          fields[3].serr[ibin] = stddev(        qflux[in], /nan) / sqrt(nin)
          
        endif
      endif
      
    endfor
    
    charsize0 = 2./sqrt(total(!P.MULTI[1:2]))
 
    for ifield = 0, n_elements(fields)-1 do begin
      field = fields[ifield]
      plot, [0],[0], xrange=[0,500], yrange=field.range, title=stage + ' '+ field.name, xticklen=1, xgridstyle=1, xtickinterval=dr, $
        xcharsize=0.85, xtitle='distance from center (km)', ytitle=field.title
      ci = t_cvfs*field.serr
      xyouts, x[0], !Y.CRANGE[0]+0.9*(!Y.CRANGE[1]-!Y.CRANGE[0]), string(round(confidence_level*100),format='(I0,"% confidence interval")'), charsize=charsize0
      xyouts, x, !Y.CRANGE[0], string(n,format='(I0)'), align=0.5, charsize=charsize0
      oplot, x, field.mean, psym=-1
      oplot, x, field.mean+ci
      oplot, x, field.mean-ci
    endfor
    
    
  endfor
  
  if nolabel eq 0 then ptimestamp, /right
  !P.MULTI=0
  if !D.NAME eq 'PS' then device, /close
  if !D.NAME eq 'PS' then print, psfile
  
end

pro little_box, field, stuff, pos
  if max(field) eq 0 then return
  heights = stuff.heights
  yrange  = stuff.yrange
  title  = stuff.title
  ; fill box with white
  polyfill, [pos[0], pos[2], pos[2], pos[0]], [pos[1], pos[1], pos[3], pos[3]], /norm, color=255
  plot, field, heights, position=pos, yrange=yrange, ystyle=1, /noerase, charsize=!P.CHARSIZE*0.6,xtickformat='(I0)', $
    xrange=[min(field[where(field gt 0)]),max(field)], title=title, xminor=1, thick=!P.THICK*0.9,xticks=3, ytickformat='(I0)'
  tvlct, oldct, /get
  tvlct, transpose([210,222,250]), 0
  polyfill, [0, 0, field], [[max(yrange),min(yrange)], heights], /data, noclip=0
  tvlct, oldct
  for iheight=0,n_elements(heights)-1 do plots, !X.CRANGE, replicate(heights[iheight],2), linestyle=1, $
    noclip=0, thick=!P.THICK*0.25
    
  return
end

function get_xlevels, field_string, xrange
  case field_string of
    'dMSE' : xlevels = round([-10,-6,-3  ,-1  ,+1  ,+3  ,+6])
    'dTv'  : xlevels =       [ -5,-3,-1.5,-0.5,+0.5,+1.5,+3]
    'B'    : xlevels = [-0.25, -0.20, -0.15, -0.05, 0.05, 0.15, 0.2]
    'rev_parcl_dT'    : xlevels = [-2,-1,0,+1,+2,+3,+4]
    'pseudo_parcl_dT' : xlevels = [-2,-1,0,+1,+2,+3,+4]
    else: xlevels = round(findgen(7)/6*(xrange[1]-xrange[0])+xrange[0])
  endcase
  return, xlevels
end


function d_f_, s1, n1, s2, n2
  ;return, min([n1,n2])-1 ; uncomment for shortcut
  ; degrees of freedom for unequal sample sizes, unequal variance
  ; Welch's t-test is an adaptation of Student's t-test intended for use with 2 samples having possibly unequal variances.
  ;
  numerator = (s1^2./n1 + s2^2./n2)^2.
  denominator = (s1^2./n1)^2./(n1-1) + (s2^2./n2)^2./(n2-1)
  return, numerator/denominator
  
end


pro cold_IR_by_time
  if !D.NAME eq 'PS' then device, /close
  
  ; run this first in /Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats :
  ; grep -h "    0  700 " PGI??L2010????_????00mtm00.txt | cut -c 1-39,51-58,147-202 > 0-700km.stats
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/GOES/pouch_stats/'
  file = basedir+'0-550km.stats'
  if file_test(file) ne 1 then stop
  t = read_ascii(file)
  yyyymmdd = t.field01[1,*]
  hhmmss = t.field01[2,*]
  hh = floor(hhmmss/10000)
  mm = (hhmmss-hh*10000)/100
  clon = t.field01[3,*]
  clat = t.field01[4,*]
  hhd = hh+mm/60.
  LST = hhd + clon/15.
  LST = (LST+24) mod 24
  nvalid = t.field01[5,*]
  minus80 = t.field01[6,*]
  minus70 = t.field01[7,*]
  minus60 = t.field01[8,*]
  minus50 = t.field01[9,*]
  minus40 = t.field01[10,*]
  minus30 = t.field01[11,*]
  minus20 = t.field01[12,*]
  ; Just look at points on MTP days.
  binsize=1
  iMTP = where(is_stage('/Volumes/pecan2/ahijevyc/PREDICT/MTP/',yyyymmdd,'*','all') eq 1)
  ;  plot, LST[iMTP], minus60[iMTP], psym=1,xrange=[0,24],xstyle=1
  h = histogram(LST[iMTP],locations=xvals,binsize=binsize,reverse_indices=r)/250.
  nx = n_elements(xvals)
  ;  oplot, xvals, h, psym=10
  
  zs = ['minus80','minus70','minus60','minus50','minus40','minus30','minus20']
  nthresholds = n_elements(zs)
  avg = replicate(!VALUES.F_NAN, nx)
  ;    erase
  for ithreshold = 0, nthresholds-1 do begin
    result = execute('z='+zs[ithreshold])
    for i = 0, n_elements(xvals)-1 do avg[i] = mean((z[iMTP])[r[r[i]:r[i+1]-1]],/nan)
    plot, xvals+binsize/2., avg, xrange=[0,24], xstyle=1, /noerase, thick=2,linestyle=ithreshold, yrange=[0,0.45], $
      /ystyle, title=file_basename(file), yticklen=1, xticklen=1, xtickinterval=6, xminor=6
    oplot, [-0.5,0.5],[avg[nx-1],avg[0]]
    oplot, [23.5,24.5],[avg[nx-1],avg[0]]
    yref = !Y.CRANGE[1]*0.75
    xref = 3.74
    xyouts, xref, yref*0.993+ithreshold*0.009, zs[ithreshold],align=1.04
    plots, [xref+0.05,xref+1.8], replicate(yref+ithreshold*0.009,2), linestyle=ithreshold
  endfor
  if !D.NAME eq 'PS' then device, /close
end

pro drops_avgIR
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/'
  subpath='analysis/'
  nolabel=1
  if !D.NAME eq 'X' then !P.CHARSIZE=2 else !P.CHARSIZE=1
  !P.THICK=1
  psfile = basedir+subpath+'histogram_IR.ps'
  if !D.NAME eq 'PS' then device, /close, ysize=5.5, xsize=6.5, /inches, filename=psfile
  stages = ['non-developing','developing_gt2days_prior','developing_le2days_prior','developed']
  !P.MULTI = [0,2,2]
  for istage = 0, n_elements(stages)-1 do begin
    stage = stages[istage]
    file = file_search(basedir+subpath+'/'+stage+'_00700mb_IR10km_scatter.txt', count=nfiles)
    get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, GFS_mean
    
    ranges=[[0,700],[0,200]]
    for irange = 0, n_elements(ranges)/2-1 do begin
      range = ranges[*,irange]
      min_distance_km = range[0]
      max_distance_km = range[1]
      min_max_distance_string = string(min_distance_km,max_distance_km,format='(I0,"-",I3.3,"km")')
      noerase = min_distance_km eq 0 && max_distance_km le 300
      IRmin = -90
      IRmax = 30
      icold = where(IR lt IRmin, ncold)
      if ncold gt 0 then IR[icold] = IRmin
      ihot = where(IR gt IRmax, nhot)
      if nhot gt 0 then IR[ihot] = IRmax
      in = where(distance_km ge min_distance_km and distance_km lt max_distance_km, nin)
      title= stage
      green = [170,223,180]
      red   = [253,94,94]
      gry   = [150,150,150]
      if nin gt 0 then begin
        IR = IR[in]
        binsize=10
        h = histogram(IR, locations=xval, binsize=binsize, min=IRmin, max=IRmax)
        yrange = stage eq 'all' ? [0,125]:[0,50]
        if noerase eq 0 then begin
          plot, xval+binsize/2., h, psym=10, title=title, xticks=6, xrange=[min(xval),max(xval)], xstyle=1, yrange=yrange, $
            xtitle= 'avg IR temperature !Z(00B0)C!Cwithin 10km', $
            ytitle=!P.MULTI[0] mod 4 eq 0 ? 'frequency of drops with!Cvalid 700mb temperature' : '', ycharsize=!P.CHARSIZE*1.5, xcharsize=1
          oplot, xval+binsize/2., h, psym=10, thick=!P.THICK*4
          ;          tvlct, oldct, /get
          ;          tvlct, transpose([red]), 0
          ;          for ibin = 0, n_elements(xval)-1 do polyfill, [replicate(xval[ibin],2),replicate(xval[ibin]+binsize,2)],[!Y.RANGE[0],replicate(h[ibin],2),!Y.RANGE[0]],noclip=0
          ;          tvlct, oldct
          count = string(nin,format='(x,"(",I0,")")')
          xyouts, -80, !Y.CRANGE[1]-0.11*(!Y.CRANGE[1]-!Y.CRANGE[0]), min_max_distance_string+count, align=0.,charsize=2*!P.CHARSIZE/!P.MULTI[2]
        endif
        if noerase eq 1 then begin
          oplot, xval+binsize/2., h, psym=10
          tvlct, oldct, /get
          tvlct, transpose([gry]), 0
          for ibin = 0, n_elements(xval)-1 do polyfill, [replicate(xval[ibin],2),replicate(xval[ibin]+binsize,2)],[!Y.RANGE[0],replicate(h[ibin],2),!Y.RANGE[0]],noclip=0
          count = string(nin,format='(x,"(",I0,")")')
          xyouts, -80, !Y.CRANGE[1]-0.24*(!Y.CRANGE[1]-!Y.CRANGE[0]), min_max_distance_string+count, align=0., $
            charthick=!P.CHARthick*2, charsize=2.3*!P.CHARSIZE/!P.MULTI[2]
          tvlct, oldct
        endif
        ; fix axis, which was covered by polyfill
        axis, xaxis=0, xrange=!X.CRANGE, xstyle=1, xtickformat='(A1)', xticks=6, xminor=1
      endif
    endfor
    if nolabel eq 0 then ptimestamp, /right
  endfor
  
  for istage = 0, n_elements(stages)-1 do begin
    stage = stages[istage]
    file = file_search(basedir+subpath+'/'+stage+'_00700mb_IR10km_scatter.txt', count=nfiles)
    get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, GFS_mean, other=other
    binsize=50
    TempRanges = ['lt-20C', 'ge-20C']
    for iTempRange= 0, n_elements(TempRanges)-1 do begin
      TempRange = TempRanges[iTempRange]
      in = where(IR_range(IR, TempRange), nin)
      if nin gt 0 then begin
        distance_km = distance_km[in]
        h = histogram(distance_km, locations=xval, binsize=binsize, min=0, max=700)
        plot, xval+binsize/2., h, psym=10, thick=!P.THICK*4, title=stage + " " + TempRange, xstyle=1
        count = string(nin, format='(x,"(",I0,")")')
      endif
      
    endfor
    
  endfor
  !P.MULTI=0
  if !D.NAME eq 'PS' then device, /close
  if !D.NAME eq 'PS' then print, psfile
  
end

pro get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
  zero_length = file_test(file, /zero)
  expected_fields = 36 ; plus 2 string fields
  tmpl = { VERSION: 1., $
    DATASTART: 0L, DELIMITER: ' ', $
    MISSINGVALUE: !VALUES.F_NAN, $
    COMMENTSYMBOL:  '', FIELDCOUNT:  expected_fields+2, $
    FIELDTYPES: [7,replicate(4,expected_fields),7], $
    FIELDNAMES: ['pgi',replicate('field01',expected_fields),'filename'], $
    FIELDLOCATIONS: lindgen(expected_fields+2), $
    FIELDGROUPS: [0,replicate(1,expected_fields),2] }
    
  if zero_length then t = {field01:replicate(!VALUES.F_NAN,expected_fields)} else t = read_ascii(file, template=tmpl)
  nfields = (size(t.field01))[1]
  if nfields ne expected_fields then message, 'unexpected #fields in scatter file '+file
  TempC  = t.field01[0,*]
  DwptC  = t.field01[1,*]
  TvC    = t.field01[2,*]
  mse    = t.field01[3,*]
  distance_km = t.field01[4,*]
  IR     = t.field01[5,*]
  x_km   = t.field01[6,*]
  y_km   = t.field01[7,*]
  P      = t.field01[8,*]
  Z      = t.field01[9,*]
  rev_cape        = t.field01[10,*]
  pseudo_cape     = t.field01[11,*]
  rev_parcl_dT    = t.field01[12,*]
  pseudo_parcl_dT = t.field01[13,*]
  liquid_water    = t.field01[14,*]
  ice_water       = t.field01[15,*]
  U      = t.field01[16,*]
  V      = t.field01[17,*]
  usys   = t.field01[18,*]
  vsys   = t.field01[19,*]
  gfs_T  = t.field01[20,*]
  gfs_Td = t.field01[21,*]
  gfs_Tv = t.field01[22,*]
  gfs_mse = t.field01[23,*]
  gfs_Z   = t.field01[24,*]
  gfs_U  = t.field01[25,*]
  gfs_V  = t.field01[26,*]
  SST_Reynolds    = t.field01[27,*]
  SST    = t.field01[28,*]
  meanMTP= t.field01[29,*]
  year   = t.field01[30,*]
  month  = t.field01[31,*]
  day    = t.field01[32,*]
  hour   = t.field01[33,*]
  minute = t.field01[34,*]
  second = t.field01[35,*]
  Dfile  = t.filename
  
  
  gfs_rh = e_h2o(gfs_Td+!CONST.T0)/e_h2o(gfs_T+!CONST.T0)*100.
  ;  sanity_gfs_mse = moist_static_energy(gfs_T+!CONST.T0, gfs_Td+!CONST.T0, p, gfs_Z)
  g = 9.81
  TvK = TvC + !CONST.T0
  ; B = g *(Tpar-Tenv)/Tenv  where Tenv is the virtual temperature of the environment in Kelvin.
  rev_buoyancy    = g *    rev_parcl_dT / TvK
  pseudo_buoyancy = g * pseudo_parcl_dT / TvK
  
  
  ; dropsondes/HRD/fil/*.fil files have already had system motion subtracted
  ; don't subtract it again.
  iHRDfil = where(strmatch(Dfile,'*dropsondes/HRD/fil/*.fil') eq 1, nHRDfil, complement = ieol, ncomplement=neol)
  ; Subtract system motion if it is not a *.fil file.
  if neol gt 0 then begin
    ;make u and v system relative
    U[ieol] = U[ieol] - usys[ieol]
    V[ieol] = V[ieol] - vsys[ieol]
    gfs_U[ieol] = gfs_U[ieol] - usys[ieol]
    gfs_V[ieol] = gfs_V[ieol] - vsys[ieol]
  endif
  
  U_earth_rel = U + usys
  V_earth_rel = V + vsys
  
  gfs_mean = {T :gfs_T, Td:gfs_Td, Tv:gfs_Tv, rh:gfs_rh, mse:gfs_mse, U:gfs_U, V:gfs_V}
  other = {pgi: t.pgi, filename: Dfile, mse:mse, x_km:x_km, y_km:y_km, rev_cape:rev_cape, $
    liquid_water:liquid_water, ice_water:ice_water, meanMTP:meanMTP, sst_reynolds:sst_reynolds, $
    rev_parcl_dT:rev_parcl_dT, rev_buoyancy:rev_buoyancy, pseudo_cape:pseudo_cape, pseudo_parcl_dT:pseudo_parcl_dT, $
    pseudo_buoyancy:pseudo_buoyancy, sst:sst, U_earth_rel:U_earth_rel, V_earth_rel:V_earth_rel, jday:julday(month, day, year, hour, minute, second) }
    
end


pro do_cold_warm_vs_IR
  ers = ['00','02','04','06','08','10']
  ers = ['00']
  ers = '/entrainment'+ers+'_ice' ; + '/0.5rev_fallout'
  for ier = 0,n_elements(ers)-1 do begin
    er_dir = ers[ier]
    for itst_profile = 3, 4 do begin ; 0-9  ['ge+10C','lt-10C','ge-10C','lt-20C','ge-20C','convection-40-60C', 'convection-60C', 'convection','mid','clear','all','GFS','zero']
      ranges=[[0,200],[200,700]]
      for irange = 0,1 do begin ; was 0, 3
        for ifield = 4,4,1 do begin ; 0-14 ['dTv', 'dT', 'dRH', 'dMSE', 'RH','U','V','spd','MSE','Tv','rev_parcl_dT',pseudo_parcl_dT, LW, IW, 'LWIW']
          ;        cold_warm_vs_IR, 'm', ifield, itst_profile, subpath='analysis', range=ranges[*,irange]
          cold_warm_vs_IR, 'mb', ifield, itst_profile, subpath='analysis'+er_dir, range=ranges[*,irange], inner_outer=0
        endfor
      ;      cold_warm_vs_IR, 'm', 1, itst_profile, subpath='MTP/with_single_chnl_retrieval', range=ranges[*,irange]
      ;      cold_warm_vs_IR, 'm', 1, itst_profile, subpath='MTP/only_single_chnl_retrieval', range=ranges[*,irange]
      ;      cold_warm_vs_IR, 'm', 1, itst_profile, subpath='MTP/no_single_chnl_retrieval', range=ranges[*,irange]
      endfor
    endfor
  endfor
  print, 'run sort_types.csh'
end

pro cold_warm_vs_IR, vert_units, ifield, itst_profile, subpath=subpath, range=range, inner_outer=inner_outer
  ; vert_units can be 'm' or 'mb'. If 'm' the reference profile is mean of clear soundings (the old way).
  ; If 'mb' then reference is mean GFS.
  ; inner_outer = 0 ; do regular reference profile
  ; inner_outer = 1 ; use drops in outer range ring as reference profile
  basedir = '/Volumes/pecan2/ahijevyc/PREDICT/'
  lesslabel = 1
  xticks = lesslabel eq 1 ? 4 : 6
  lag1 = 0
  if ~keyword_set(subpath) then subpath = 'analysis/entrainment00_ice'
  
  ; if you are using MTP, an altitude type must be chosen
  MTP_AltType=''
  if strmatch(subpath, '*MTP*') then MTP_AltType = '_GPSAlt' ; '_press_alt' or '_GPSAlt'
  if n_elements(vert_units) eq 0 then vert_units = 'mb'
  if n_elements(ifield) eq 0 then ifield = 0
  field_strings = ['dTv', 'dT', 'dRH', 'dMSE', 'RH','U','V','spd','MSE','Tv','rev_parcl_dT','pseudo_parcl_dT',$
    'LW','IW','LWIW']
  field_string = field_strings[ifield]
  xranges = [[-2.0,2.0],[-2.6,2.0],[-80,50],[-10,10],[0,100],[-5,5],[-5,5],[1,12],[330,352],[-2,6],[-0.2,0.2],$
    [-2,6],[-2,6],[0,16],[0,12],[0,20]]
  xrange  = xranges[*,ifield]
  xlevels = get_xlevels(field_string, xrange)
  
  xtickformats= ['(I+0,"!Z(00B0)C")','(I+0,"!Z(00B0)C")','(I+0,"%")',lesslabel eq 0 ? '(I+0,"!UkJ/kg!N")':'(I+0)',$
    '(I0,"%")','(I0,"ms!E-1!N")','(I0,"ms!E-1!N")','(I0,"ms!E-1!N")','(I0,"!UkJ/kg!N")','(I0,"!Z(00B0)C")',$
    lesslabel eq 0 ? '(F+6.1,"ms!E-1!N")' : '(F+5.2)', '(I+0,"!Z(00B0)C")','(I+0,"!Z(00B0)C")','(I0,"g/kg")',$
    '(I0,"g/kg")','(I0,"g/kg")']
  xtickformat=xtickformats[ifield]
  
  IRprofiles = ['ge+10C','lt-10C','ge-10C','lt-20C','ge-20C','convection-40-60C', 'convection-60C', 'convection','mid',$
    'clear','all','GFS','zero','Dunion2011']
  if n_elements(itst_profile) eq 0 then itst_profile = 3
  tst_profile_str = IRprofiles[itst_profile]
  
  ref_profiles = IRprofiles
  iref_profile  = vert_units eq 'm' ? where(ref_profiles eq 'clear') : where(ref_profiles eq 'GFS')
  if field_string eq 'RH' || field_string eq 'U' || field_string eq 'V' || field_string eq 'spd' $
    || field_string eq 'MSE' || field_string eq 'B' || field_string eq 'rev_parcl_dT' || $
    field_string eq 'pseudo_parcl_dT' || field_string eq 'LW' || field_string eq 'IW' || $
    field_string eq 'LWIW' $
    then iref_profile = where(ref_profiles eq 'zero')
  if field_string eq 'Tv' then iref_profile = where(ref_profiles eq 'Dunion2011')
  ref_profile_str = inner_outer eq 1 ? tst_profile_str : ref_profiles[iref_profile]
  ylog = 0
  if field_string eq 'B' || field_string eq 'rev_parcl_dT' || field_string eq 'pseudo_parcl_dT' then ylog = 1
  
  
  if ~keyword_set(inner_outer) then inner_outer = 0
  if ~keyword_set(range) then begin
    min_distance_km = 000
    max_distance_km = 200
  endif else begin
    min_distance_km = range[0]
    max_distance_km = range[1]
  endelse
  min_max_distance_string = string(min_distance_km,max_distance_km,format='(I3.3,"-",I3.3,"km")')
  
  ; if this is a zero-to whatever range, then make the reference profile the same thing except the outer range ring.
  if inner_outer eq 1 then begin
    min_distance2_km = max_distance_km
    max_distance2_km = 700.
    min_max_distance_string = min_max_distance_string + '-' + string(min_distance2_km,max_distance2_km,format='(I3.3,"-",I3.3,"km")')
  endif else begin
    min_distance2_km = min_distance_km
    max_distance2_km = max_distance_km
  endelse
  
  psfile=basedir+subpath+'/'+field_string+"_vs_"+tst_profile_str+"IR_"+min_max_distance_string+MTP_AltType+"_"+vert_units+".ps"
  if !D.NAME eq 'PS' then device, /close, /color, bits=8, filename = psfile, ysize=6, yoffset=4, xsize=6.5, xoffset=1, /inches
  if !D.NAME eq 'PS' then !P.THICK=3 else !P.THICK=1
  if !D.NAME eq 'PS' then !P.CHARTHICK = lesslabel eq 0 ? 2 : 3.5
  if !D.NAME eq 'PS' then !P.CHARSIZE  = lesslabel eq 0 ? 1 : 2
  if !D.NAME eq 'X' then device, decomposed=0
  ; Make a vector of 16 points, A[i] = 2pi/16:
  A = FINDGEN(17) * (!PI*2/16.)
  ; Define the symbol to be a unit circle with 16 points,
  ; and set the filled flag:
  USERSYM, COS(A), SIN(A), /FILL
  
  !P.MULTI=0
  loadct, 39, /silent
  pos = [0.14,0.15,0.79,0.92]
  confidence_level = 0.95
  
  
  ; Combine all the scatter plot points for each category (non-developing, developing > 2days prior, etc.)
  ; Each drop is weighted equally, so missions with more drops will have more weight.
  xtitle = field_string+': '+tst_profile_str+' - '+ref_profile_str+' reference env'
  stages = ['non-developing','developing_gt2days_prior','developing_le2days_prior','developed']
  for istage = 0, n_elements(stages)-1 do begin
    stage = stages[istage]
    title = field_string+" "+stage + (lesslabel eq 1 ? "" : "!C" + min_max_distance_string)
    files = file_search(basedir+subpath+'/'+stage+(strmatch(subpath,'*MTP*')? MTP_AltType:'')+'_?????'+vert_units+'_IR10km_scatter.txt', count=nheights)
    if nheights eq 0 then continue
    heights = replicate(!VALUES.F_NAN, nheights)
    height_ipos = strpos(file_basename(files),vert_units+'_IR10km_scatter')-5
    if n_elements(uniq(height_ipos)) gt 1 then stop
    heights = strmid(file_basename(files),height_ipos[0],5)
    yrange = [min(heights),max(heights)]
    ;    if vert_units eq 'm' then yrange[1] = min([yrange[1], 12000.])
    if vert_units eq 'mb' then yrange = reverse(yrange)
    plot, replicate(0,nheights), heights, ytickformat='(I0)', xrange=xrange, yrange=yrange, /nodata, title=title, $
      xstyle=1, position=pos, ytitle='altitude ['+vert_units+']!C'+MTP_AltType, xtitle=lesslabel?'':xtitle, $
      xtickformat=xtickformat, xticks=xticks, ylog=ylog, ystyle=1
    if lesslabel eq 0 then ptimestamp
    
    nref_profile   = replicate(0, nheights)
    ntst_profile   = replicate(0, nheights)
    ref_profile    = replicate(!VALUES.F_NAN, nheights)
    tst_profile    = replicate(!VALUES.F_NAN, nheights)
    ref_prof_std   = replicate(!VALUES.F_NAN, nheights)
    tst_prof_std   = replicate(!VALUES.F_NAN, nheights)
    deg_freedom    = replicate(0, nheights)
    t_cvfs         = replicate(0., nheights); remember the decimal point (must be float)
    p_value        = replicate(!VALUES.F_NAN, nheights)
    serr           = replicate(!VALUES.F_NAN, nheights)
    diff           = replicate(!VALUES.F_NAN, nheights)
    t_statistic    = replicate(!VALUES.F_NAN, nheights)
    in_out_diff    = replicate(!VALUES.F_NAN, nheights) ; are the inner and outer means significantly different
    dots_prof      = replicate(!VALUES.F_NAN, nheights, 1000)
    xdrops_prof    = replicate(!VALUES.F_NAN, nheights, 1000)
    ydrops_prof    = replicate(!VALUES.F_NAN, nheights, 1000)
    stid_prof      = replicate(           '', nheights, 1000)
    d_debug_prof   = replicate(           '', nheights, 1000)
    for iheight = 0, nheights-1 do begin
      file = files[iheight]
      height = heights[iheight]
      get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
      GFS = 0.
      case field_string of
        "dT"   : begin
          var2plot = TempC
          GFS = GFS_mean.T
        end
        "dTv"  : begin
          var2plot = TvC
          GFS = GFS_mean.Tv
        end
        "dRH"  : begin
          var2plot = tdew2rh(DwptC+!CONST.T0,TempC+!CONST.T0)
          GFS = GFS_mean.rh
        end
        "dMSE" : begin
          var2plot = other.mse
          GFS = GFS_mean.MSE
        end
        "RH"   : begin
          var2plot = tdew2rh(DwptC+!CONST.T0,TempC+!CONST.T0)
          GFS      = GFS_mean.rh
        end
        "U"    : var2plot = U
        "V"    : var2plot = V
        "spd"  : var2plot = sqrt(U^2.+V^2.)
        "rev_B"        : var2plot = other.rev_buoyancy
        "pseudo_B"     : var2plot = other.pseudo_buoyancy
        "rev_parcl_dT" : var2plot = other.rev_parcl_dT
        "pseudo_parcl_dT" : var2plot = other.pseudo_parcl_dT
        "LW"   : var2plot = other.liquid_water
        "IW"   : var2plot = other.ice_water
        "LWIW"   : var2plot = other.liquid_water + other.ice_water
        "MSE"  : begin
          var2plot = other.MSE
          GFS      = GFS_mean.MSE
        end
        "Tv"   : var2plot = TvC
        else:stop
      endcase
      if tst_profile_str eq 'GFS' then begin
        var2plot = GFS
      endif
      if ref_profile_str eq 'zero' then GFS = 0.
      if ref_profile_str eq 'Dunion2011' then GFS = (get_Dunion2011(height)).Tv
      
      ; get these for system-relative plot
      idots = where(IR_range(IR,tst_profile_str), ndots)
      if ndots gt 0 then begin
        dots_prof[iheight,0:ndots-1]   = var2plot[idots]-GFS[idots]
        xdrops_prof[iheight,0:ndots-1] = other.x_km[idots]
        ydrops_prof[iheight,0:ndots-1] = other.y_km[idots]
        stid_prof[iheight,0:ndots-1]   = other.filename[idots]
        d_debug_prof[iheight,0:ndots-1] = string(var2plot[idots],format='(F0.1)')+"-"+string(GFS[idots],format='(F0.1)')+"!C"
      endif
      
      ; if GFS is the reference then
      if ref_profile_str eq 'GFS' || ref_profile_str eq 'Dunion2011' then begin
        iref = where(IR_range(IR,tst_profile_str) and finite(var2plot) and distance_km lt max_distance2_km and distance_km ge min_distance2_km, nref)
        if nref ge 1 then ref_profile[iheight] = mean(GFS[iref])
        if nref ge 2 then ref_prof_std[iheight] = stddev(var2plot[iref] - GFS[iref])
      endif else begin
        ; this is the old method where you used an IR classification (e.g. convective) as the reference.
        iref = where(IR_range(IR,ref_profile_str) and finite(var2plot) and distance_km lt max_distance2_km and distance_km ge min_distance2_km, nref)
        if nref ge 1 then ref_profile[iheight] = mean(var2plot[iref] - (inner_outer eq 1 ? GFS[iref]:0))
        if nref ge 2 then ref_prof_std[iheight] = stddev(var2plot[iref] - (inner_outer eq 1 ? GFS[iref]:0) )
        if ref_profile_str eq 'zero' then begin
          ref_profile[iheight] = 0.
          ref_prof_std[iheight] = 0.
        endif
      endelse
      if nref ge 2 then begin
        lag_1_autocorrelation = a_correlate(var2plot[iref],1)
        if lag1 eq 0 then lag_1_autocorrelation = 0.
        ; don't allow a negative lag-1 autocorrelation. That would be a negative variance inflation factor.
        ; The effective samples would go up and exceed the actual.
        effective_nref = min([nref, nref*(1.-lag_1_autocorrelation)/(1.+lag_1_autocorrelation)])
      endif else effective_nref=1
      nref_profile[iheight] = nref
      itst = where(IR_range(IR,tst_profile_str) and finite(var2plot) and distance_km lt max_distance_km and distance_km ge min_distance_km, ntst)
      ntst_profile[iheight] = ntst
      if ntst ge 1 then tst_profile[iheight] = mean(var2plot[itst] - (inner_outer eq 1 ? GFS[itst]:0))
      if ntst ge 2 then begin
        tst_prof_std[iheight] = stddev(var2plot[itst] - (inner_outer eq 1 ? GFS[itst]:0))
        nlag = min([10, ntst-2]); no greater than ntst-2 or 10
        nlag = max([1, nlag]); no less than 1
        lag = 1+indgen(nlag)
        lag_autocorrelation = a_correlate(var2plot[itst],lag)
        lag_1_autocorrelation = lag_autocorrelation[0]
        if lag1 eq 0 then lag_1_autocorrelation = 0.
        if lesslabel eq 0 then xyouts, !X.CRANGE[1], height, strjoin(string(lag,format='("!4q!X!D",I0,",tst!N=")')+string(lag_autocorrelation,format='(F5.2)'),'!C'), align=1.1, charsize=!P.CHARSIZE*0.2, charthick=!P.CHARTHICK*0.2
        ; don't allow a negative lag-1 autocorrelation that would make the effective samples greater than the actual
        effective_ntst = min([ntst, ntst*(1.-lag_1_autocorrelation)/(1.+lag_1_autocorrelation)])
      endif else effective_ntst = 1
      if ref_profile_str eq 'GFS' then begin
        ; GFS is paired with the dropsonde, so these differences are paired.
        ; The standard error and degrees of freedom are calculated differently depending on if they are paired or not.
        serr[iheight] = ref_prof_std[iheight]/sqrt(effective_ntst)
        deg_freedom[iheight] = min([effective_nref,effective_ntst])-1
      endif else begin
        ; if not GFS then these are not paired differences. The standard error of the mean is larger then.
        ; think Dunion2011 belongs here too.
        serr[iheight] = sqrt(tst_prof_std[iheight]^2./effective_ntst + ref_prof_std[iheight]^2./effective_nref) ; used to do in array format outside of loop and after loop , but nref and ntst are scalars and stuck at last values (top height)
        deg_freedom[iheight] = d_f_(tst_prof_std[iheight], effective_ntst, ref_prof_std[iheight], effective_nref)
      endelse
      diff[iheight] = tst_profile[iheight] - ref_profile[iheight]
      
      if serr[iheight] gt 0 then t_statistic[iheight]   = diff[iheight]/serr[iheight]
      if deg_freedom[iheight] gt 0 then begin
        t_cvfs[iheight]  = t_cvf((1-confidence_level)/2.,deg_freedom[iheight])
        ; read in IDL help: The p-value is between 0 and 1 and is the probability of observing
        ; data that would yield a test statistic as extreme or more extreme under the assumption of the null hypothesis.
        ; Hence, a small p-value is evidence for the rejection of the null hypothesis.
        ; for large t-statistics (above 10) you get a floating underflow arithmetic error. Unless you use double().
        p_value[iheight] = 2*(1-t_pdf(abs(double(t_statistic[iheight])),deg_freedom[iheight]))
      endif
    endfor
    ; draw confidence interval and legend
    ci = t_cvfs*serr
    tvlct, oldct, /get
    conf_color = 179 ;
    green = [170,223,180]
    red   = [253,94,94]
    gray  = [190,190,190]
    redpurple = [220,94,199]
    if min_distance_km eq 0 then begin
      linestyle = 0
      color = gray
    endif else begin
      color = gray*1.2
      linestyle = 5
    endelse
    dark_conf_color = conf_color+1
    tvlct, transpose(color), conf_color
    tvlct, transpose(color*0.5), dark_conf_color
    ici = where(finite(diff) and finite(ci), n_ici)
    if n_ici gt 0 then begin
      x = [diff[ici]-ci[ici] , reverse(diff[ici]+ci[ici]) ]
      y = [heights[ici], reverse(heights[ici])]
      polyfill, x, y, color=conf_color, noclip=0
      plots, x, y, color=dark_conf_color, noclip=0, linestyle=linestyle
      ; draw thick line for overall difference between means
      oplot, diff[ici], heights[ici], thick=11, noclip=0, color=dark_conf_color, linestyle=linestyle
    endif
    
    tvlct, oldct
    ; draw zero line
    plots, replicate(0,2), !Y.CRANGE
    hx = (!X.CRANGE[1]-!X.CRANGE[0])/5.
    if lesslabel eq 0 then begin
      xyouts, -1,-1, string(confidence_level*100,format='(I0,"% confidence interval")'), width=w, /norm, charsize=!P.CHARSIZE*0.65, noclip=0
      xyouts, (pos[0]+pos[2])*0.5, pos[3]+!P.CHARSIZE*0.001, string(confidence_level*100,format='(I0,"% confidence interval")'), align=0.5, /norm, charsize=!P.CHARSIZE*0.65
      polyfill, (pos[0]+pos[2])*0.5+[-w/2,w/2,w/2,-w/2], pos[3]+!P.CHARSIZE*[0.000,0.000,0.012,0.012], color=conf_color, /norm
      ; make table of data counts, z-score, degrees of freedom and probability of non-zero difference (p).
      xyouts, pos[2]+0.025, pos[3]+0.05, field_string+" data!Ccounts!C"+tst_profile_str+"/"+ref_profile_str, charsize=!P.CHARSIZE*0.6, /norm, align=0.5
      hy=vert_units eq 'm' ? 300 : -20
      xyouts, !X.CRANGE[1]+0.84*hx, !Y.CRANGE[1]+hy, "t", charsize=!P.CHARSIZE*0.6,align=1
      xyouts, !X.CRANGE[1]+1.14*hx, !Y.CRANGE[1]+hy, "df", charsize=!P.CHARSIZE*0.6,align=1
      xyouts, !X.CRANGE[1]+1.59*hx, !Y.CRANGE[1]+hy, "p", charsize=!P.CHARSIZE*0.6,align=1
    endif
    In_out_diff_savfile = basedir+subpath+'/savfiles/'+stage+file_basename(psfile, 'ps')+'sav'
    ; if you get an error here about a non-existent save file, you probably added a field
    ; but have not run inner_outer=1 yet. Then you have to go into the savfile/ directory
    ; and create symbolic links with make_links.pl with the new field's save files.
    if inner_outer eq 0 then restore, In_out_diff_savfile else in_out_diff = p_value
    if inner_outer eq 1 then save, in_out_diff, filename=In_out_diff_savfile
    
    for iheight=0,nheights-1 do begin
      height = heights[iheight]
      hy = vert_units eq 'm' ? -90 : (ylog ? 6*height/600.: 6)*!P.CHARSIZE
      
      ; draw dashed lines and label levels of altitude - If significant, draw a black filled circle
      if nheights lt 20 || (height mod 50) eq 0 then begin
        plots, !X.CRANGE, replicate(height,2), linestyle=1, thick=!P.THICK*0.5
        ; add tick labels to ylog axis
        if ylog && (height mod 100) eq 0 && height ne 10^!Y.CRANGE[0] then xyouts, !X.CRANGE[0], height+hy, string(height, format='(I0)'), align=1.17
      endif
      if 1-in_out_diff[iheight] gt confidence_level then plots, !X.CRANGE[1]+0.2*hx, height, psym=8, symsize=nheights lt 20 ? 1.2 : 0.5
      if lesslabel eq 0 then begin
        xyouts, !X.CRANGE[1], height, string(ntst_profile[iheight],nref_profile[iheight],$
          format='(I0,"!C",I0)'), charsize=!P.CHARSIZE*0.6,align=-0.05
        ; continue table
        xyouts, !X.CRANGE[1]+0.84*hx, height+hy, string(t_statistic[iheight],format='(F5.1)'), charsize=!P.CHARSIZE*0.65, align=1
        xyouts, !X.CRANGE[1]+1.14*hx, height+hy, string(deg_freedom[iheight],format='(I0)'), charsize=!P.CHARSIZE*0.65, align=1
        xyouts, !X.CRANGE[1]+1.59*hx, height+hy, string(p_value[iheight],format='(F5.3)'), charsize=!P.CHARSIZE*0.65, align=1
      endif
    endfor
    stuff = {heights:heights,yrange:yrange,title:'degrees freedom   '}
    if lesslabel eq 0 then little_box, deg_freedom, stuff, [pos[0]+0.08*(pos[2]-pos[0]), pos[1]+(strmid(field_string,1,2,/rev) eq 'RH' $
      ? 0.14:0.6)*(pos[3]-pos[1]), pos[0]+0.22*(pos[2]-pos[0]), pos[1]+(strmid(field_string,1,2,/rev) eq 'RH' ? 0.49 : 0.95)*(pos[3]-pos[1])]
      
    ; === system relative plot at each level Uses dots procedure.
    for iheight=0,nheights-1 do begin
      height = heights[iheight]
      lvl_str = string(height,format='(I0)')
      if height ne 1000 && height ne 925 && height ne 400 && height ne 500 then continue
      dots = dots_prof[iheight,*]
      xdrops = xdrops_prof[iheight,*]
      ydrops = ydrops_prof[iheight,*]
      duh    = d_debug_prof[iheight,*]
      stids  = file_basename(stid_prof[iheight,*])
      
      d500925 = 0
      if d500925 eq 1 then begin
        i500 = where(heights eq 500)
        i925 = where(heights eq 925)
        dots[*] = !VALUES.F_NAN
        xdrops[*] = !VALUES.F_NAN
        ydrops[*] = !VALUES.F_NAN
        stids500 = file_basename(stid_prof[i500,*])
        stids925 = file_basename(stid_prof[i925,*])
        for in500 = 0, n_elements(stids500)-1 do begin
          if stids500[in500] eq '' then continue
          in925 = where(stids925 eq stids500[in500], nn)
          if nn gt 1 then stop
          if nn eq 1 then begin
            xdrops[in500] = (xdrops_prof[i500,in500] + xdrops_prof[i925,in925])/2.
            ydrops[in500] = (ydrops_prof[i500,in500] + ydrops_prof[i925,in925])/2.
            dots[in500]   = dots_prof[i500,in500] - dots_prof[i925,in925]
          endif
        endfor
        lvl_str = '500-925'
      endif
      
      d400925 = 0
      if d400925 eq 1 then begin
        i400 = where(heights eq 400)
        i925 = where(heights eq 925)
        dots[*] = !VALUES.F_NAN
        xdrops[*] = !VALUES.F_NAN
        ydrops[*] = !VALUES.F_NAN
        stids400 = file_basename(stid_prof[i400,*])
        stids925 = file_basename(stid_prof[i925,*])
        for in400 = 0, n_elements(stids400)-1 do begin
          if stids400[in400] eq '' then continue
          in925 = where(stids925 eq stids400[in400], nn)
          if nn gt 1 then stop
          if nn eq 1 then begin
            xdrops[in400] = (xdrops_prof[i400,in400] + xdrops_prof[i925,in925])/2.
            ydrops[in400] = (ydrops_prof[i400,in400] + ydrops_prof[i925,in925])/2.
            dots[in400]   = dots_prof[i400,in400] - dots_prof[i925,in925]
          endif
        endfor
        lvl_str = '400-925'
      endif
      if d500925 && d400925 then stop
      
      ; only calculate the mean x and y offset for soundings within 1000 km of the center.
      within_reason = where(sqrt(xdrops^2.+ydrops^2.) lt 1000, nwr)
      if nwr gt 0 then begin
        xmean = mean(xdrops[within_reason],/nan)
        xoffset = 0;abs(xmean)^0.9 * abs(xmean) / xmean
        ymean = mean(ydrops[within_reason],/nan)
        yoffset = 0;abs(ymean)^0.9 * abs(ymean) / ymean
        ; if start_dot_plot is undefined, then compile cold_pool_analysis
        start_dot_plot, xoffset, yoffset, xtitle+string(stage,format='("!C",A)')+' '+lvl_str+vert_units , position=pos
        dotsize = 2*0.97; when plot range is +/- 500km 0.97 is 10km radius circle ; 10*total(finite(dots))^(-0.4)
        ;xyouts, xdrops, ydrops, strmid(stids,0,9)+'!C'+strmid(stids,10), align=0.5, charsize=0.4, charthick=0.1
        dots, dots, xdrops, ydrops, xtitle, levels=xlevels, barformat=xtickformat, thick=dotsize, symsize=dotsize, charsize=lesslabel eq 1 ? 0.01: 1, charthick=0.5
        if lesslabel eq 0 then ptimestamp
      endif
    endfor
    
    
    ; Now calculate the vertical profile for each mission and then take the average of all missions.
    ; This weights each mission equally even if they have different numbers of drops.
    ; use the ./sort_types.csh program to create a list of missions for each category (developed, non-developing, etc).
    nd_T_profile = replicate(0, nheights)
    avg_dtemp_across_all_dates = replicate(!VALUES.F_NAN, nheights)
    for iheight = 0, nheights-1 do begin
      height = heights[iheight]
      ; now just work on one height .
      ; find the average temperature difference between the high and low  cloud drops for each date.
      t = read_ascii(basedir+subpath+'/'+stage+(strmatch(subpath,'*MTP*')? MTP_AltType:'')+'_missions.txt', count=nmissions, template={version:1,datastart:0,delimiter:'',$
        missingvalue:!VALUES.F_NAN,commentsymbol:'',fieldtypes:[7],fieldcount:1,fieldnames:['missions'],fieldlocations:[0],fieldgroups:[0]})
      files = file_search(basedir+subpath+'/'+t.missions+'0000'+(strmatch(subpath,'*MTP*')?MTP_AltType:'')+'_'+string(height,format='(I5.5)')+vert_units+'_IR10km_scatter.txt')
      if iheight eq 0 then begin
        d_T=replicate(!VALUES.F_NAN, nmissions, nheights)
        dates=replicate('', nmissions)
        ntstdrops = replicate(0, nmissions)
        nrefdrops = replicate(0, nmissions)
      endif
      for imission = 0, nmissions-1 do begin
        mission = t.missions[imission]
        ;        if mission eq '20100916_21' then continue
        ;        if mission eq '20100917_18' then continue
        file = files[imission]
        date = strmid(t.missions[imission],4,7)
        dates[imission] = date
        if file_test(file, /zero) eq 1 then continue ; there are lots of zero-size files.
        get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
        case field_string of
          "dT"   : var2plot = TempC
          "dTv"  : var2plot = TvC
          "dRH"  : var2plot = tdew2rh(DwptC+!CONST.T0,TempC+!CONST.T0)
          "RH"   : var2plot = tdew2rh(DwptC+!CONST.T0,TempC+!CONST.T0)
          "dMSE" : var2plot = other.mse
          "U"    : var2plot = U
          "V"    : var2plot = V
          "spd"  : var2plot = sqrt(U^2.+V^2.)
          "rev_B"           : var2plot = other.rev_buoyancy
          "pseudo_B"        : var2plot = other.pseudo_buoyancy
          "rev_parcl_dT"    : var2plot = other.rev_parcl_dT
          "pseudo_parcl_dT" : var2plot = other.pseudo_parcl_dT
          "LW"   : var2plot = other.liquid_water
          "IW"   : var2plot = other.ice_water
          "LWIW"   : var2plot = other.liquid_water + other.ice_water
          "MSE"  : var2plot = other.mse
          "Tv"   : var2plot = TvC
          else:stop
        endcase
        ref_T = !VALUES.F_NAN
        if ref_profile_str eq 'GFS' || tst_profile_str eq 'GFS' || ref_profile_str eq 'Dunion2011' then begin
          iref = where(IR_range(IR,tst_profile_str) and finite(var2plot) and distance_km lt max_distance2_km and distance_km ge min_distance2_km, nref)
          if nref gt 0 then begin
            case field_string of
              "dT"   : ref_T = mean(gfs_mean.T[iref],/nan)
              "dTv"  : ref_T = mean(gfs_mean.Tv[iref],/nan)
              "dRH"  : ref_T = mean(gfs_mean.RH[iref],/nan)
              "dMSE" : ref_T = mean(gfs_mean.MSE[iref],/nan)
              "RH"   : ref_T = 0.
              "U"    : ref_T = 0.
              "V"    : ref_T = 0.
              "spd"  : ref_T = 0.
              "MSE"  : ref_T = mean(gfs_mean.MSE[iref],/nan)
              "Tv"   : ref_T = (get_Dunion2011(height)).Tv
              else:stop
            endcase
          endif
          if tst_profile_str eq 'GFS' then var2plot = ref_T
        endif else begin
          iref = where(IR_range(IR,ref_profile_str) and finite(var2plot) and distance_km lt max_distance2_km and distance_km ge min_distance2_km, nref)
          if nref gt 0 then ref_T = mean(var2plot[iref],/nan)
        endelse
        if ref_profile_str eq 'zero' then ref_T = 0.
        ;IR_range is defined above in the case statement of tst_profile_str but IR array is different.
        itst = where(IR_range(IR,tst_profile_str) and finite(var2plot) and distance_km lt max_distance_km and distance_km ge min_distance_km, ntst)
        tst_T = ntst gt 0 ? mean(var2plot[itst],/nan) : !VALUES.F_NAN
        d_T[imission, iheight] = tst_T-ref_T
        nd_T_profile[iheight] = nd_T_profile[iheight] + finite(d_T[imission, iheight])
        if ntst gt ntstdrops[imission] then ntstdrops[imission] = ntst ; greatest number of valid drops at any level
        if nref gt nrefdrops[imission] then nrefdrops[imission] = nref ; greatest number of valid drops at any level
      endfor
      ; average temperature difference across all dates
      if total(finite(d_T[*,iheight])) gt 0 then avg_dtemp_across_all_dates[iheight] = total(d_T[*,iheight],/nan)/total(finite(d_T[*,iheight]))
      
    endfor
    
    plot, replicate(0,nheights), heights, ytickformat='(I0)', xrange=xrange, yrange=yrange, /nodata, title=title, ylog=ylog, $
      xstyle=1, position=pos, ytitle='altitude ['+vert_units+']!C'+MTP_AltType, xtitle=xtitle, xticks = xticks, xtickformat=xtickformat
    if lesslabel eq 0 then ptimestamp
    plots, replicate(0,2), !Y.CRANGE
    xyouts, pos[2]-0.02, pos[3]+0.01, "missions!Cw/ valid "+field_string, orientation=39, charsize=!P.CHARSIZE*0.6, charthick=!P.CHARTHICK*0.7, /norm
    for iheight=0,nheights-1 do begin
      plots, !X.CRANGE, replicate(heights[iheight],2), linestyle=1, thick=!P.THICK*0.5
      xyouts, !X.CRANGE[1], heights[iheight]+hy, string(nd_T_profile[iheight],format='(I2)'), charsize=!P.CHARSIZE*0.6
    endfor
    legendx = pos[2]+0.027
    legendy = pos[3]
    xyouts, legendx+0.04, legendy, "         max  max!Cmission "+strmid(tst_profile_str,0,5)+" "+strmid(ref_profile_str,0,5), charsize=!P.CHARSIZE*0.7, /norm
    ; average temperature difference across all dates
    oplot, avg_dtemp_across_all_dates, heights, thick=21
    for imission = 0, nmissions-1 do begin
      color = 1+imission*(254/nmissions)
      oplot, d_T[imission,*], heights, color = color, psym=1, thick=!P.THICK*2, noclip=1
      oplot, d_T[imission,*], heights, color = color, linestyle=2, thick=!P.THICK*0.3, noclip=1
      xyouts, legendx, legendy-(2+imission)*!P.CHARSIZE*0.0154, string(imission+1,dates[imission],ntstdrops[imission],nrefdrops[imission],$
        format='(I2,")",A8,I4,I5)'), color=color, charsize=!P.CHARSIZE*0.7, /norm
    ;      if lesslabel eq 0 then xyouts, d_T[imission,*], heights, string(lag_1_autocorrelation[imission,*],format='(F4.2)'), charsize=!P.CHARSIZE*0.6, align=0
    endfor
    
    
    
  endfor
  
  
  
  
  if !D.NAME eq 'PS' then begin
    device, /close
    print, "created "+psfile
  endif
  !P.THICK = 1
  !P.CHARTHICK = 1
  !P.CHARSIZE =1
end



pro plot_d_f_
  s1 = 50
  s2 = 50
  n2 = 10
  plot, [1,200],[0,199], xrange=[0,200], yrange=[0,250], xtitle='n1', ytitle='d', /nodata
  for n1 = 5, 200 do begin
    plots, n1, d_f_(s1, n1, s2, n2), psym=2
    plots, n1, min([n2,n1])-1, psym=1, color=200
  endfor
  
  
  
end
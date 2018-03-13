pro cdavis_cape_test
  loadct, 39, /silent
  files = file_search('/pecan/ahijevyc/RIP4_dave/*davetest/wrfout_d01*', count=nfiles)
  for ifile = 0, nfiles-1 do begin
    if ifile ne 1 then continue
    file = files[ifile]
    t = get_wrf(file)
    Ts        = t.T
    Rs        = t.Q/(1d - t.Q)
    mu        = t.psfc - t.pt
    Presss    = t.ETA1 * mu + t.pt
    lons      = t.GLON
    lats      = t.GLAT
    raw_levels = Presss
    igood = where(finite(raw_levels) and finite(Ts) and finite(Rs),ngood)
    if ngood gt 1 then begin
      ihi2loP = reverse(sort(Presss[igood]))
      hi2loP = (Presss[igood])[ihi2loP]
      hi2loT = (Ts[igood])[ihi2loP]
      hi2loR= (Rs[igood])[ihi2loP]
      skewt, [-20.,36.9], title=file, everyT=10, everyDA=10
      ;      plot_skewt, hi2loT, hi2loTd, hi2loP
      ers = [0]
      
      for ice = 1, 1 do begin
        for ientrainment_rate = 0,n_elements(ers)-1 do begin
          entrainment_rate = ers[ientrainment_rate]
          
          cape_sound,hi2loP,hi2loT,hi2loR,tvrdif=rev_parcl_dT,tvpdif=pseudo_parcl_dT, ice=ice,$
            tlp=tlp,tlvp=tlvp,tlr=tlr,tlvr=tlvr,caper=caper,capep=capep,entrainment_rate=entrainment_rate, cinp=cinp, cinr=cinr
          plot_skewt, hi2loT*(1.+hi2loR/0.622)/(1.+hi2loR)-!CONST.T0, hi2loTd, hi2loP,thick=1
          rev_cape = max(caper, imax, /nan)
          rev_cin  = cinr[imax]
          plot_skewt, tlvr[imax,*]-!CONST.T0, hi2loTd, hi2loP, col_t=183, thick=(1+ientrainment_rate)*2*ice
          pseudo_cape = max(capep, imax, /nan)
          pseudo_cin  = cinp[imax]
          plot_skewt, tlvp[imax,*]-!CONST.T0, hi2loTd, hi2loP, col_t=208, thick=(1+ientrainment_rate)*2*ice
          print, file_basename(file), entrainment_rate, ice, rev_cape, pseudo_cape, format='(A20," ",I2,"% ice=",I1, " rev:",F8.2," ps:",F8.2)'
        endfor
      endfor
    endif
  endfor
end

pro cdavis_cape_test2
  loadct, 39, /silent
  files = file_search('/Volumes/pecan2/ahijevyc/cdavis/cape_molinari/02*.edit', count=nfiles)
  files = file_search('/Volumes/pecan2/ahijevyc/PREDICT/'+['MTP_dropsonde_composite','dropsondes']+'/D20100817_10*_PQC.eol', count=nfiles)
  
  for ifile = 0, nfiles-1 do begin
    file = files[ifile]
    d = get_sonde(file)
    Ts        = d.T
    Alts      = d.z
    Tds       = d.Td
    Presss    = d.P
    
    igood = where(finite(Presss) and finite(Ts),ngood)
    if ngood le 1 then continue
    
    ; on Jul 17 2012, cape_sound was 2x faster than cape_field
    ;  cape1 = cape_field(reform(Presss[igood],1,ngood),reform(Ts[igood]+!CONST.T0,1,ngood),mixing_ratio_kgkg)
    
    dz = min(abs(Alts-100.), /nan, iz)
    parcel_origin_p = Presss[iz]
    if ~finite(parcel_origin_p) then stop
    if ~finite(dz) then parcel_origin_p = 1000.
    ihi2loP = reverse(sort(Presss[igood]))
    hi2loP = (Presss[igood])[ihi2loP]
    hi2loT = (Ts[igood])[ihi2loP]
    hi2loTd= (Tds[igood])[ihi2loP]
    
    hi2loR = mixr_sat(hi2loTd,hi2loP)/1000.
    inan = where(finite(hi2loR) eq 0, nnan)
    if nnan gt 0 then hi2loR[inan] = 0.

    skewt, [-31.,36.9], title=file, everyT=10, everyDA=10
    plot_skewt, hi2loT, hi2loTd, hi2loP
    ers = [0,5,10]
    
    for ice = 1, 1 do begin
      for ientrainment_rate = 0,n_elements(ers)-1 do begin
        entrainment_rate = ers[ientrainment_rate]
        cape_sound,hi2loP,hi2loT+!CONST.T0,hi2loR,parcel_origin_p=parcel_origin_p,tvrdif=rev_parcl_dT,tvpdif=pseudo_parcl_dT, ice=ice,$
          tlp=tlp,tlvp=tlvp,tlr=tlr,tlvr=tlvr,caper=caper,capep=capep,entrainment_rate=entrainment_rate, cinp=cinp, cinr=cinr
        ; Virtual temperature profile
        plot_skewt, (hi2loT+!CONST.T0)*(1.+hi2loR/0.622)/(1.+hi2loR)-!CONST.T0, hi2loTd, hi2loP,thick=1
        rev_cape = max(caper, imax, /nan)
        rev_cin  = cinr[imax]
        plot_skewt, tlvr[imax,*]-!CONST.T0, hi2loTd, hi2loP, col_t=183, thick=(1+ientrainment_rate)*2*ice
        pseudo_cape = max(capep, imax, /nan)
        pseudo_cin  = cinp[imax]
        plot_skewt, tlvp[imax,*]-!CONST.T0, hi2loTd, hi2loP, col_t=208, thick=(1+ientrainment_rate)*2*ice
        print, file_basename(file), entrainment_rate, ice, rev_cape, pseudo_cape, format='(A20," ",I2,"% ice=",I1, " rev:",F8.2," ps:",F8.2)'
      endfor
    endfor
  endfor
end

function get_wrf, file
  ncid = NCDF_OPEN(file)            ; Open The NetCDF file
  
  NCDF_VARGET, ncid,  0, Times      ; Read in variable 'Times'
  
  NCDF_VARGET, ncid,  1, TOYVAR      ; Read in variable 'TOYVAR'
  
  NCDF_VARGET, ncid,  2, LU_INDEX      ; Read in variable 'LU_INDEX'
  
  NCDF_VARGET, ncid,  3, HBM2      ; Read in variable 'HBM2'
  
  NCDF_VARGET, ncid,  4, HBM3      ; Read in variable 'HBM3'
  
  NCDF_VARGET, ncid,  5, VBM2      ; Read in variable 'VBM2'
  
  NCDF_VARGET, ncid,  6, VBM3      ; Read in variable 'VBM3'
  
  NCDF_VARGET, ncid,  7, SM      ; Read in variable 'SM'
  
  NCDF_VARGET, ncid,  8, SICE      ; Read in variable 'SICE'
  
  NCDF_VARGET, ncid,  9, PD      ; Read in variable 'PD'
  
  NCDF_VARGET, ncid,  10, FIS      ; Read in variable 'FIS'
  
  NCDF_VARGET, ncid,  11, RES      ; Read in variable 'RES'
  
  NCDF_VARGET, ncid,  12, T      ; Read in variable 'T'
  
  NCDF_VARGET, ncid,  13, Q      ; Read in variable 'Q'
  
  NCDF_VARGET, ncid,  14, U      ; Read in variable 'U'
  
  NCDF_VARGET, ncid,  15, V      ; Read in variable 'V'
  
  NCDF_VARGET, ncid,  16, DX_NMM      ; Read in variable 'DX_NMM'
  
  NCDF_VARGET, ncid,  17, ETA1      ; Read in variable 'ETA1'
  
  NCDF_VARGET, ncid,  18, ETA2      ; Read in variable 'ETA2'
  
  NCDF_VARGET, ncid,  19, PDTOP      ; Read in variable 'PDTOP'
  
  NCDF_VARGET, ncid,  20, PT      ; Read in variable 'PT'
  
  NCDF_VARGET, ncid,  21, PBLH      ; Read in variable 'PBLH'
  
  NCDF_VARGET, ncid,  22, USTAR      ; Read in variable 'USTAR'
  
  NCDF_VARGET, ncid,  23, Z0      ; Read in variable 'Z0'
  
  NCDF_VARGET, ncid,  24, THS      ; Read in variable 'THS'
  
  NCDF_VARGET, ncid,  25, QS      ; Read in variable 'QS'
  
  NCDF_VARGET, ncid,  26, TWBS      ; Read in variable 'TWBS'
  
  NCDF_VARGET, ncid,  27, QWBS      ; Read in variable 'QWBS'
  
  NCDF_VARGET, ncid,  28, PREC      ; Read in variable 'PREC'
  
  NCDF_VARGET, ncid,  29, APREC      ; Read in variable 'APREC'
  
  NCDF_VARGET, ncid,  30, ACPREC      ; Read in variable 'ACPREC'
  
  NCDF_VARGET, ncid,  31, CUPREC      ; Read in variable 'CUPREC'
  
  NCDF_VARGET, ncid,  32, LSPA      ; Read in variable 'LSPA'
  
  NCDF_VARGET, ncid,  33, SNO      ; Read in variable 'SNO'
  
  NCDF_VARGET, ncid,  34, SI      ; Read in variable 'SI'
  
  NCDF_VARGET, ncid,  35, CLDEFI      ; Read in variable 'CLDEFI'
  
  NCDF_VARGET, ncid,  36, TH10      ; Read in variable 'TH10'
  
  NCDF_VARGET, ncid,  37, Q10      ; Read in variable 'Q10'
  
  NCDF_VARGET, ncid,  38, PSHLTR      ; Read in variable 'PSHLTR'
  
  NCDF_VARGET, ncid,  39, TSHLTR      ; Read in variable 'TSHLTR'
  
  NCDF_VARGET, ncid,  40, QSHLTR      ; Read in variable 'QSHLTR'
  
  NCDF_VARGET, ncid,  41, Q2      ; Read in variable 'Q2'
  
  NCDF_VARGET, ncid,  42, AKHS_OUT      ; Read in variable 'AKHS_OUT'
  
  NCDF_VARGET, ncid,  43, AKMS_OUT      ; Read in variable 'AKMS_OUT'
  
  NCDF_VARGET, ncid,  44, ALBASE      ; Read in variable 'ALBASE'
  
  NCDF_VARGET, ncid,  45, ALBEDO      ; Read in variable 'ALBEDO'
  
  NCDF_VARGET, ncid,  46, CNVBOT      ; Read in variable 'CNVBOT'
  
  NCDF_VARGET, ncid,  47, CNVTOP      ; Read in variable 'CNVTOP'
  
  NCDF_VARGET, ncid,  48, CZEN      ; Read in variable 'CZEN'
  
  NCDF_VARGET, ncid,  49, CZMEAN      ; Read in variable 'CZMEAN'
  
  NCDF_VARGET, ncid,  50, EPSR      ; Read in variable 'EPSR'
  
  NCDF_VARGET, ncid,  51, GLAT      ; Read in variable 'GLAT'
  
  NCDF_VARGET, ncid,  52, GLON      ; Read in variable 'GLON'
  
  NCDF_VARGET, ncid,  53, MXSNAL      ; Read in variable 'MXSNAL'
  
  NCDF_VARGET, ncid,  54, RADOT      ; Read in variable 'RADOT'
  
  NCDF_VARGET, ncid,  55, SIGT4      ; Read in variable 'SIGT4'
  
  NCDF_VARGET, ncid,  56, TGROUND      ; Read in variable 'TGROUND'
  
  NCDF_VARGET, ncid,  57, CWM      ; Read in variable 'CWM'
  
  NCDF_VARGET, ncid,  58, F_ICE      ; Read in variable 'F_ICE'
  
  NCDF_VARGET, ncid,  59, F_RAIN      ; Read in variable 'F_RAIN'
  
  NCDF_VARGET, ncid,  60, F_RIMEF      ; Read in variable 'F_RIMEF'
  
  NCDF_VARGET, ncid,  61, CLDFRA      ; Read in variable 'CLDFRA'
  
  NCDF_VARGET, ncid,  62, SR      ; Read in variable 'SR'
  
  NCDF_VARGET, ncid,  63, CFRACH      ; Read in variable 'CFRACH'
  
  NCDF_VARGET, ncid,  64, CFRACL      ; Read in variable 'CFRACL'
  
  NCDF_VARGET, ncid,  65, CFRACM      ; Read in variable 'CFRACM'
  
  NCDF_VARGET, ncid,  66, ISLOPE      ; Read in variable 'ISLOPE'
  
  NCDF_VARGET, ncid,  67, DZSOIL      ; Read in variable 'DZSOIL'
  
  NCDF_VARGET, ncid,  68, SLDPTH      ; Read in variable 'SLDPTH'
  
  NCDF_VARGET, ncid,  69, CMC      ; Read in variable 'CMC'
  
  NCDF_VARGET, ncid,  70, GRNFLX      ; Read in variable 'GRNFLX'
  
  NCDF_VARGET, ncid,  71, PCTSNO      ; Read in variable 'PCTSNO'
  
  NCDF_VARGET, ncid,  72, SOILTB      ; Read in variable 'SOILTB'
  
  NCDF_VARGET, ncid,  73, VEGFRC      ; Read in variable 'VEGFRC'
  
  NCDF_VARGET, ncid,  74, SH2O      ; Read in variable 'SH2O'
  
  NCDF_VARGET, ncid,  75, SMC      ; Read in variable 'SMC'
  
  NCDF_VARGET, ncid,  76, STC      ; Read in variable 'STC'
  
  NCDF_VARGET, ncid,  77, PINT      ; Read in variable 'PINT'
  
  NCDF_VARGET, ncid,  78, W      ; Read in variable 'W'
  
  NCDF_VARGET, ncid,  79, ACFRCV      ; Read in variable 'ACFRCV'
  
  NCDF_VARGET, ncid,  80, ACFRST      ; Read in variable 'ACFRST'
  
  NCDF_VARGET, ncid,  81, SSROFF      ; Read in variable 'SSROFF'
  
  NCDF_VARGET, ncid,  82, BGROFF      ; Read in variable 'BGROFF'
  
  NCDF_VARGET, ncid,  83, RLWIN      ; Read in variable 'RLWIN'
  
  NCDF_VARGET, ncid,  84, RLWTOA      ; Read in variable 'RLWTOA'
  
  NCDF_VARGET, ncid,  85, ALWIN      ; Read in variable 'ALWIN'
  
  NCDF_VARGET, ncid,  86, ALWOUT      ; Read in variable 'ALWOUT'
  
  NCDF_VARGET, ncid,  87, ALWTOA      ; Read in variable 'ALWTOA'
  
  NCDF_VARGET, ncid,  88, RSWIN      ; Read in variable 'RSWIN'
  
  NCDF_VARGET, ncid,  89, RSWINC      ; Read in variable 'RSWINC'
  
  NCDF_VARGET, ncid,  90, RSWOUT      ; Read in variable 'RSWOUT'
  
  NCDF_VARGET, ncid,  91, ASWIN      ; Read in variable 'ASWIN'
  
  NCDF_VARGET, ncid,  92, ASWOUT      ; Read in variable 'ASWOUT'
  
  NCDF_VARGET, ncid,  93, ASWTOA      ; Read in variable 'ASWTOA'
  
  NCDF_VARGET, ncid,  94, SFCSHX      ; Read in variable 'SFCSHX'
  
  NCDF_VARGET, ncid,  95, SFCLHX      ; Read in variable 'SFCLHX'
  
  NCDF_VARGET, ncid,  96, SUBSHX      ; Read in variable 'SUBSHX'
  
  NCDF_VARGET, ncid,  97, SNOPCX      ; Read in variable 'SNOPCX'
  
  NCDF_VARGET, ncid,  98, SFCUVX      ; Read in variable 'SFCUVX'
  
  NCDF_VARGET, ncid,  99, POTEVP      ; Read in variable 'POTEVP'
  
  NCDF_VARGET, ncid,  100, POTFLX      ; Read in variable 'POTFLX'
  
  NCDF_VARGET, ncid,  101, TLMIN      ; Read in variable 'TLMIN'
  
  NCDF_VARGET, ncid,  102, TLMAX      ; Read in variable 'TLMAX'
  
  NCDF_VARGET, ncid,  103, T02_MIN      ; Read in variable 'T02_MIN'
  
  NCDF_VARGET, ncid,  104, T02_MAX      ; Read in variable 'T02_MAX'
  
  NCDF_VARGET, ncid,  105, RH02_MIN      ; Read in variable 'RH02_MIN'
  
  NCDF_VARGET, ncid,  106, RH02_MAX      ; Read in variable 'RH02_MAX'
  
  NCDF_VARGET, ncid,  107, NCFRCV      ; Read in variable 'NCFRCV'
  
  NCDF_VARGET, ncid,  108, NCFRST      ; Read in variable 'NCFRST'
  
  NCDF_VARGET, ncid,  109, NPHS0      ; Read in variable 'NPHS0'
  
  NCDF_VARGET, ncid,  110, NPREC      ; Read in variable 'NPREC'
  
  NCDF_VARGET, ncid,  111, NCLOD      ; Read in variable 'NCLOD'
  
  NCDF_VARGET, ncid,  112, NHEAT      ; Read in variable 'NHEAT'
  
  NCDF_VARGET, ncid,  113, NRDLW      ; Read in variable 'NRDLW'
  
  NCDF_VARGET, ncid,  114, NRDSW      ; Read in variable 'NRDSW'
  
  NCDF_VARGET, ncid,  115, NSRFC      ; Read in variable 'NSRFC'
  
  NCDF_VARGET, ncid,  116, AVRAIN      ; Read in variable 'AVRAIN'
  
  NCDF_VARGET, ncid,  117, AVCNVC      ; Read in variable 'AVCNVC'
  
  NCDF_VARGET, ncid,  118, ACUTIM      ; Read in variable 'ACUTIM'
  
  NCDF_VARGET, ncid,  119, ARDLW      ; Read in variable 'ARDLW'
  
  NCDF_VARGET, ncid,  120, ARDSW      ; Read in variable 'ARDSW'
  
  NCDF_VARGET, ncid,  121, ASRFC      ; Read in variable 'ASRFC'
  
  NCDF_VARGET, ncid,  122, APHTIM      ; Read in variable 'APHTIM'
  
  NCDF_VARGET, ncid,  123, LANDMASK      ; Read in variable 'LANDMASK'
  
  NCDF_VARGET, ncid,  124, SMOIS      ; Read in variable 'SMOIS'
  
  NCDF_VARGET, ncid,  125, PSFC      ; Read in variable 'PSFC'
  
  NCDF_VARGET, ncid,  126, TH2      ; Read in variable 'TH2'
  
  NCDF_VARGET, ncid,  127, U10      ; Read in variable 'U10'
  
  NCDF_VARGET, ncid,  128, V10      ; Read in variable 'V10'
  
  NCDF_VARGET, ncid,  129, SMSTAV      ; Read in variable 'SMSTAV'
  
  NCDF_VARGET, ncid,  130, SMSTOT      ; Read in variable 'SMSTOT'
  
  NCDF_VARGET, ncid,  131, SFROFF      ; Read in variable 'SFROFF'
  
  NCDF_VARGET, ncid,  132, UDROFF      ; Read in variable 'UDROFF'
  
  NCDF_VARGET, ncid,  133, IVGTYP      ; Read in variable 'IVGTYP'
  
  NCDF_VARGET, ncid,  134, ISLTYP      ; Read in variable 'ISLTYP'
  
  NCDF_VARGET, ncid,  135, VEGFRA      ; Read in variable 'VEGFRA'
  
  NCDF_VARGET, ncid,  136, SFCEVP      ; Read in variable 'SFCEVP'
  
  NCDF_VARGET, ncid,  137, GRDFLX      ; Read in variable 'GRDFLX'
  
  NCDF_VARGET, ncid,  138, SFCEXC      ; Read in variable 'SFCEXC'
  
  NCDF_VARGET, ncid,  139, ACSNOW      ; Read in variable 'ACSNOW'
  
  NCDF_VARGET, ncid,  140, ACSNOM      ; Read in variable 'ACSNOM'
  
  NCDF_VARGET, ncid,  141, SNOW      ; Read in variable 'SNOW'
  
  NCDF_VARGET, ncid,  142, CANWAT      ; Read in variable 'CANWAT'
  
  NCDF_VARGET, ncid,  143, SST      ; Read in variable 'SST'
  
  NCDF_VARGET, ncid,  144, WEASD      ; Read in variable 'WEASD'
  
  NCDF_VARGET, ncid,  145, THZ0      ; Read in variable 'THZ0'
  
  NCDF_VARGET, ncid,  146, QZ0      ; Read in variable 'QZ0'
  
  NCDF_VARGET, ncid,  147, UZ0      ; Read in variable 'UZ0'
  
  NCDF_VARGET, ncid,  148, VZ0      ; Read in variable 'VZ0'
  
  NCDF_VARGET, ncid,  149, QSFC      ; Read in variable 'QSFC'
  
  NCDF_VARGET, ncid,  150, HTOP      ; Read in variable 'HTOP'
  
  NCDF_VARGET, ncid,  151, HBOT      ; Read in variable 'HBOT'
  
  NCDF_VARGET, ncid,  152, HTOPD      ; Read in variable 'HTOPD'
  
  NCDF_VARGET, ncid,  153, HBOTD      ; Read in variable 'HBOTD'
  
  NCDF_VARGET, ncid,  154, HTOPS      ; Read in variable 'HTOPS'
  
  NCDF_VARGET, ncid,  155, HBOTS      ; Read in variable 'HBOTS'
  
  NCDF_VARGET, ncid,  156, CUPPT      ; Read in variable 'CUPPT'
  
  NCDF_VARGET, ncid,  157, CPRATE      ; Read in variable 'CPRATE'
  
  NCDF_VARGET, ncid,  158, SNOWH      ; Read in variable 'SNOWH'
  
  NCDF_VARGET, ncid,  159, SMFR3D      ; Read in variable 'SMFR3D'
  
  NCDF_VARGET, ncid,  160, ITIMESTEP      ; Read in variable 'ITIMESTEP'
  
  NCDF_VARGET, ncid,  161, XTIME      ; Read in variable 'XTIME'
  
  NCDF_CLOSE, ncid      ; Close the NetCDF file
  t ={  Times     : Times      ,$; Read in variable 'Times'
    TOYVAR     : TOYVAR      ,$; Read in variable 'TOYVAR'
    LU_INDEX     : LU_INDEX      ,$; Read in variable 'LU_INDEX'
    HBM2     : HBM2      ,$; Read in variable 'HBM2'
    HBM3     : HBM3      ,$; Read in variable 'HBM3'
    VBM2     : VBM2      ,$; Read in variable 'VBM2'
    VBM3     : VBM3      ,$; Read in variable 'VBM3'
    SM     : SM      ,$; Read in variable 'SM'
    SICE     : SICE      ,$; Read in variable 'SICE'
    PD     : PD      ,$; Read in variable 'PD'
    FIS     : FIS      ,$; Read in variable 'FIS'
    RES     : RES      ,$; Read in variable 'RES'
    T     : T      ,$; Read in variable 'T'
    Q     : Q      ,$; Read in variable 'Q'
    U     : U      ,$; Read in variable 'U'
    V     : V      ,$; Read in variable 'V'
    DX_NMM     : DX_NMM      ,$; Read in variable 'DX_NMM'
    ETA1     : ETA1      ,$; Read in variable 'ETA1'
    ETA2     : ETA2      ,$; Read in variable 'ETA2'
    PDTOP     : PDTOP      ,$; Read in variable 'PDTOP'
    PT     : PT      ,$; Read in variable 'PT'
    PBLH     : PBLH      ,$; Read in variable 'PBLH'
    USTAR     : USTAR      ,$; Read in variable 'USTAR'
    Z0     : Z0      ,$; Read in variable 'Z0'
    THS     : THS      ,$; Read in variable 'THS'
    QS     : QS      ,$; Read in variable 'QS'
    TWBS     : TWBS      ,$; Read in variable 'TWBS'
    QWBS     : QWBS      ,$; Read in variable 'QWBS'
    PREC     : PREC      ,$; Read in variable 'PREC'
    APREC     : APREC      ,$; Read in variable 'APREC'
    ACPREC     : ACPREC      ,$; Read in variable 'ACPREC'
    CUPREC     : CUPREC      ,$; Read in variable 'CUPREC'
    LSPA     : LSPA      ,$; Read in variable 'LSPA'
    SNO     : SNO      ,$; Read in variable 'SNO'
    SI     : SI      ,$; Read in variable 'SI'
    CLDEFI     : CLDEFI      ,$; Read in variable 'CLDEFI'
    TH10     : TH10      ,$; Read in variable 'TH10'
    Q10     : Q10      ,$; Read in variable 'Q10'
    PSHLTR     : PSHLTR      ,$; Read in variable 'PSHLTR'
    TSHLTR     : TSHLTR      ,$; Read in variable 'TSHLTR'
    QSHLTR     : QSHLTR      ,$; Read in variable 'QSHLTR'
    Q2     : Q2      ,$; Read in variable 'Q2'
    AKHS_OUT     : AKHS_OUT      ,$; Read in variable 'AKHS_OUT'
    AKMS_OUT     : AKMS_OUT      ,$; Read in variable 'AKMS_OUT'
    ALBASE     : ALBASE      ,$; Read in variable 'ALBASE'
    ALBEDO     : ALBEDO      ,$; Read in variable 'ALBEDO'
    CNVBOT     : CNVBOT      ,$; Read in variable 'CNVBOT'
    CNVTOP     : CNVTOP      ,$; Read in variable 'CNVTOP'
    CZEN     : CZEN      ,$; Read in variable 'CZEN'
    CZMEAN     : CZMEAN      ,$; Read in variable 'CZMEAN'
    EPSR     : EPSR      ,$; Read in variable 'EPSR'
    GLAT     : GLAT      ,$; Read in variable 'GLAT'
    GLON     : GLON      ,$; Read in variable 'GLON'
    MXSNAL     : MXSNAL      ,$; Read in variable 'MXSNAL'
    RADOT     : RADOT      ,$; Read in variable 'RADOT'
    SIGT4     : SIGT4      ,$; Read in variable 'SIGT4'
    TGROUND     : TGROUND      ,$; Read in variable 'TGROUND'
    CWM     : CWM      ,$; Read in variable 'CWM'
    F_ICE     : F_ICE      ,$; Read in variable 'F_ICE'
    F_RAIN     : F_RAIN      ,$; Read in variable 'F_RAIN'
    F_RIMEF     : F_RIMEF      ,$; Read in variable 'F_RIMEF'
    CLDFRA     : CLDFRA      ,$; Read in variable 'CLDFRA'
    SR     : SR      ,$; Read in variable 'SR'
    CFRACH     : CFRACH      ,$; Read in variable 'CFRACH'
    CFRACL     : CFRACL      ,$; Read in variable 'CFRACL'
    CFRACM     : CFRACM      ,$; Read in variable 'CFRACM'
    ISLOPE     : ISLOPE      ,$; Read in variable 'ISLOPE'
    DZSOIL     : DZSOIL      ,$; Read in variable 'DZSOIL'
    SLDPTH     : SLDPTH      ,$; Read in variable 'SLDPTH'
    CMC     : CMC      ,$; Read in variable 'CMC'
    GRNFLX     : GRNFLX      ,$; Read in variable 'GRNFLX'
    PCTSNO     : PCTSNO      ,$; Read in variable 'PCTSNO'
    SOILTB     : SOILTB      ,$; Read in variable 'SOILTB'
    VEGFRC     : VEGFRC      ,$; Read in variable 'VEGFRC'
    SH2O     : SH2O      ,$; Read in variable 'SH2O'
    SMC     : SMC      ,$; Read in variable 'SMC'
    STC     : STC      ,$; Read in variable 'STC'
    PINT     : PINT      ,$; Read in variable 'PINT'
    W     : W      ,$; Read in variable 'W'
    ACFRCV     : ACFRCV      ,$; Read in variable 'ACFRCV'
    ACFRST     : ACFRST      ,$; Read in variable 'ACFRST'
    SSROFF     : SSROFF      ,$; Read in variable 'SSROFF'
    BGROFF     : BGROFF      ,$; Read in variable 'BGROFF'
    RLWIN     : RLWIN      ,$; Read in variable 'RLWIN'
    RLWTOA     : RLWTOA      ,$; Read in variable 'RLWTOA'
    ALWIN     : ALWIN      ,$; Read in variable 'ALWIN'
    ALWOUT     : ALWOUT      ,$; Read in variable 'ALWOUT'
    ALWTOA     : ALWTOA      ,$; Read in variable 'ALWTOA'
    RSWIN     : RSWIN      ,$; Read in variable 'RSWIN'
    RSWINC     : RSWINC      ,$; Read in variable 'RSWINC'
    RSWOUT     : RSWOUT      ,$; Read in variable 'RSWOUT'
    ASWIN     : ASWIN      ,$; Read in variable 'ASWIN'
    ASWOUT     : ASWOUT      ,$; Read in variable 'ASWOUT'
    ASWTOA     : ASWTOA      ,$; Read in variable 'ASWTOA'
    SFCSHX     : SFCSHX      ,$; Read in variable 'SFCSHX'
    SFCLHX     : SFCLHX      ,$; Read in variable 'SFCLHX'
    SUBSHX     : SUBSHX      ,$; Read in variable 'SUBSHX'
    SNOPCX     : SNOPCX      ,$; Read in variable 'SNOPCX'
    SFCUVX     : SFCUVX      ,$; Read in variable 'SFCUVX'
    POTEVP     : POTEVP      ,$; Read in variable 'POTEVP'
    POTFLX     : POTFLX      ,$; Read in variable 'POTFLX'
    TLMIN     : TLMIN      ,$; Read in variable 'TLMIN'
    TLMAX     : TLMAX      ,$; Read in variable 'TLMAX'
    T02_MIN     : T02_MIN      ,$; Read in variable 'T02_MIN'
    T02_MAX     : T02_MAX      ,$; Read in variable 'T02_MAX'
    RH02_MIN     : RH02_MIN      ,$; Read in variable 'RH02_MIN'
    RH02_MAX     : RH02_MAX      ,$; Read in variable 'RH02_MAX'
    NCFRCV     : NCFRCV      ,$; Read in variable 'NCFRCV'
    NCFRST     : NCFRST      ,$; Read in variable 'NCFRST'
    NPHS0     : NPHS0      ,$; Read in variable 'NPHS0'
    NPREC     : NPREC      ,$; Read in variable 'NPREC'
    NCLOD     : NCLOD      ,$; Read in variable 'NCLOD'
    NHEAT     : NHEAT      ,$; Read in variable 'NHEAT'
    NRDLW     : NRDLW      ,$; Read in variable 'NRDLW'
    NRDSW     : NRDSW      ,$; Read in variable 'NRDSW'
    NSRFC     : NSRFC      ,$; Read in variable 'NSRFC'
    AVRAIN     : AVRAIN      ,$; Read in variable 'AVRAIN'
    AVCNVC     : AVCNVC      ,$; Read in variable 'AVCNVC'
    ACUTIM     : ACUTIM      ,$; Read in variable 'ACUTIM'
    ARDLW     : ARDLW      ,$; Read in variable 'ARDLW'
    ARDSW     : ARDSW      ,$; Read in variable 'ARDSW'
    ASRFC     : ASRFC      ,$; Read in variable 'ASRFC'
    APHTIM     : APHTIM      ,$; Read in variable 'APHTIM'
    LANDMASK     : LANDMASK      ,$; Read in variable 'LANDMASK'
    SMOIS     : SMOIS      ,$; Read in variable 'SMOIS'
    PSFC     : PSFC      ,$; Read in variable 'PSFC'
    TH2     : TH2      ,$; Read in variable 'TH2'
    U10     : U10      ,$; Read in variable 'U10'
    V10     : V10      ,$; Read in variable 'V10'
    SMSTAV     : SMSTAV      ,$; Read in variable 'SMSTAV'
    SMSTOT     : SMSTOT      ,$; Read in variable 'SMSTOT'
    SFROFF     : SFROFF      ,$; Read in variable 'SFROFF'
    UDROFF     : UDROFF      ,$; Read in variable 'UDROFF'
    IVGTYP     : IVGTYP      ,$; Read in variable 'IVGTYP'
    ISLTYP     : ISLTYP      ,$; Read in variable 'ISLTYP'
    VEGFRA     : VEGFRA      ,$; Read in variable 'VEGFRA'
    SFCEVP     : SFCEVP      ,$; Read in variable 'SFCEVP'
    GRDFLX     : GRDFLX      ,$; Read in variable 'GRDFLX'
    SFCEXC     : SFCEXC      ,$; Read in variable 'SFCEXC'
    ACSNOW     : ACSNOW      ,$; Read in variable 'ACSNOW'
    ACSNOM     : ACSNOM      ,$; Read in variable 'ACSNOM'
    SNOW     : SNOW      ,$; Read in variable 'SNOW'
    CANWAT     : CANWAT      ,$; Read in variable 'CANWAT'
    SST     : SST      ,$; Read in variable 'SST'
    WEASD     : WEASD      ,$; Read in variable 'WEASD'
    THZ0     : THZ0      ,$; Read in variable 'THZ0'
    QZ0     : QZ0      ,$; Read in variable 'QZ0'
    UZ0     : UZ0      ,$; Read in variable 'UZ0'
    VZ0     : VZ0      ,$; Read in variable 'VZ0'
    QSFC     : QSFC      ,$; Read in variable 'QSFC'
    HTOP     : HTOP      ,$; Read in variable 'HTOP'
    HBOT     : HBOT      ,$; Read in variable 'HBOT'
    HTOPD     : HTOPD      ,$; Read in variable 'HTOPD'
    HBOTD     : HBOTD      ,$; Read in variable 'HBOTD'
    HTOPS     : HTOPS      ,$; Read in variable 'HTOPS'
    HBOTS     : HBOTS      ,$; Read in variable 'HBOTS'
    CUPPT     : CUPPT      ,$; Read in variable 'CUPPT'
    CPRATE     : CPRATE      ,$; Read in variable 'CPRATE'
    SNOWH     : SNOWH      ,$; Read in variable 'SNOWH'
    SMFR3D     : SMFR3D      ,$; Read in variable 'SMFR3D'
    ITIMESTEP     : ITIMESTEP      ,$; Read in variable 'ITIMESTEP'
    XTIME     : XTIME      }; Read in variable 'XTIME'
    
  return, t
  
end

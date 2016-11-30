PRO CAPE_SOUND_fast,P_in,T_in,R_in,CAPEP=CAPEP,TVPDIF=TVPDIF,$
  CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
  parcel_layer_mb=parcel_layer_mb, kpar=kpar, $
  entrainment_rate=entrainment_rate, ice=ice, $
  bminr=bminr,bminp=bminp,plcl=plcl_out,parcel_params=parcel_params, $
  TLVP=TLVP, TLVR=TLVR, $
  bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout
  
  p = double(p_in)
  t = double(t_in)
  r = double(r_in)
  
  
  N=n_elements(p)	; number of vertical levels
  
  IF N EQ 0 THEN return
  
  if ~keyword_set(entrainment_rate) then entrainment_rate = 0
  if ~keyword_set(ice) then ice=0
  debug = 0
  
  
  if ~keyword_set(fractional_fallout) then fractional_fallout = 0.d
  
  ; in Kerry Emanuel's code these were initialized to 0.0 - Ahijevych
  TLP=replicate(!VALUES.D_NAN,n,n)
  TLVP=replicate(!VALUES.D_NAN,n,n)
  TVPDIF=replicate(!VALUES.D_NAN,n,n)
  CAPEP=replicate(!VALUES.D_NAN,n)
  CINP=replicate(!VALUES.D_NAN,n)
  TLR=replicate(!VALUES.D_NAN,n,n)
  TLVR=replicate(!VALUES.D_NAN,n,n)
  TVRDIF=replicate(!VALUES.D_NAN,n,n)
  CAPER=replicate(!VALUES.D_NAN,n)
  CINR=replicate(!VALUES.D_NAN,n)
  LW= replicate(0d,n,n)
  IW= replicate(0d,n,n)
  BMINR = replicate(!VALUES.D_NAN,n)
  BMINP = replicate(!VALUES.D_NAN,n)
  BMAXR = replicate(!VALUES.D_NAN,n)
  BMAXP = replicate(!VALUES.D_NAN,n)
  PLCL_out  = replicate(!VALUES.D_NAN,n)
  parcel_params = replicate({p:!VALUES.D_NAN,t:!VALUES.D_NAN,r:!VALUES.D_NAN},n)
  
  
  ;
  ;   ***   ASSIGN VALUES OF THERMODYNAMIC CONSTANTS     ***
  ;
  atmos_const ; define a system variable !atmos with physical constants - Ahijevych
  
  EPS=!ATMOS.Rd/!ATMOS.Rv
  
  
  ;
  ;  *** Water vapor pressure EV and saturation vapor pressure ES ***
  ;
  TC=T-!CONST.T0	; Celsius
  TvEnv = T * (1. + R/eps) / (1. + R)
  EV=R*P/(EPS+R)	; vapor pressure
  ES=6.112 * exp((17.67*((TC)))/((TC) +243.5))	; saturation vapor pressure
  
  if ~keyword_set(parcel_layer_mb) then parcel_layer_mb=0
  ;
  ;   ***  Begin outer loop, which cycles through parcel origin levels I ***
  ;
  zlow = 0
  zhi  = n/3-1
  
  if n_elements(kpar) gt 0 then begin
    zlow = kpar
    zhi  = kpar
  endif
  
  
  FOR I=zlow,zhi DO BEGIN	; do calculation only for lowest N/3 levels
  
    if parcel_layer_mb eq 0 then begin
      parcel_p = p[i]
      parcel_t = t[i]
      parcel_r = r[i]
    endif else begin
      ; If parcel parameters (T, P, R) are given use those instead of environmental
      ; parameters. If parcel_layer_mb is given use average of top and bottom of pressure range.
      ; If bottom of layer is below sounding, don't extrapolate below sounding;
      ;  instead use the first pressure p[0] as the bottom of pressure range and shift top pressure up
      ; so full parcel_layer_mb is used. That's how tlift.f does it.
      pbot = p[0] < (p[i]+parcel_layer_mb/2.)
      ptop = pbot - parcel_layer_mb
      pss = [pbot,ptop]
      in_between = where(p le pbot and p ge ptop, nin)
      if nin gt 0 then pss = [pbot,p[in_between],ptop]
      
      parcel_p = (pbot+ptop)/2.
      temps = interpol(t, p, pss)
      thetas = temps*(1000./pss)^(!ATMOS.Rd/!ATMOS.Cpd)
      ; pressure thicknesses
      dps = pss-shift(pss,-1) & dps = dps[0:-2]
      thetameans = (thetas+shift(thetas,-1))/2. & thetameans=thetameans[0:-2]
      thetamean = total(thetameans*dps)/total(dps); pressure thickness weighted
      rss = interpol(R, p, pss) 
      rmeans = (rss+shift(rss,-1))/2. & rmeans = rmeans[0:-2]
      parcel_r = total(rmeans*dps)/total(dps); pressure thickness weighted
      parcel_t = thetamean*(parcel_p/1000.)^(!ATMOS.Rd/!ATMOS.Cpd)
      
      ; if parcel_p isn't found in pressure array, lift parcel dry
      ; adiabatically to next highest pressure level in pressure array.
      if where(p eq parcel_p, /null) eq !NULL then begin
        raise_to = value_locate(p,parcel_p)
        parcel_p = p[raise_to+1]
        parcel_t = thetamean*(parcel_p/1000.)^(!ATMOS.Rd/!ATMOS.Cpd)
      endif
    endelse
    parcel_params[i] = {p:parcel_p,t:parcel_t,r:parcel_r}
    ;
    ;   ***  Define the pseudo-adiabatic entropy SP (conserved variable) ***
    ;
    parcel_TC=parcel_T-!CONST.T0 ; Celsius
    parcel_EV=parcel_R*parcel_P/(EPS+parcel_R)  ; vapor pressure
    parcel_ES=6.112 * exp((17.67*((parcel_TC)))/((parcel_TC) +243.5)) ; saturation vapor pressure
    
    RS=EPS*parcel_ES/(parcel_P-parcel_ES)
    EM=(parcel_EV GT 1.e-6)*parcel_EV+(parcel_EV LE 1e-6)*1e-6
    
    
    S=(!ATMOS.Cpd+parcel_R*!ATMOS.CL)*ALOG(parcel_T)-!ATMOS.Rd*ALOG(parcel_P-parcel_EV)+$ ; P[I]-parcel_EV)+$
      (!ATMOS.LV0-2320.*(parcel_TC))*parcel_R/parcel_T-parcel_R*!ATMOS.Rv*ALOG(EM/parcel_ES)
      
    SP=!ATMOS.Cpd*ALOG(parcel_T)-!ATMOS.Rd*ALOG(parcel_P-parcel_EV)+$  ; P[I]-parcel_EV)+$
      (!ATMOS.LV0-2320.*(parcel_TC))*parcel_R/parcel_T-parcel_R*!ATMOS.Rv*ALOG(EM/parcel_ES)
      
    ;
    ;   ***   Find lifted condensation pressure PLCL    ***
    ;
    RH=parcel_R/RS	; relative humidity
    CHI=parcel_T/(1669.0-122.0*RH-parcel_T)
    IF RH eq 0 then pLCL = 0. else pLCL = parcel_P*RH^CHI ; P[I]*RH^CHI
    PLCL_out[i] = PLCL
    
    cloud = RH ge 1. ? 1 : 0
    
    ;
    ;   ***  Begin updraft loop   ***
    ;
    SUM=0.0
    RG0=parcel_R
    TG0=parcel_T
    Rrev = parcel_R ; for entrainment calculations below
    RG   = parcel_R
    icb = n-1
    ice_fraction = -TG0/40d > 0 < 1
    first_point = 1
    FOR J=I,N-1 DO BEGIN
      if p[j] gt parcel_p then continue
      ;
      ;   ***  Calculate estimates of the rates of change of the entropies  ***
      ;   ***           with temperature at constant pressure               ***
      ;
      RS=EPS*ES[J]/(P[J]-ES[J])	; saturation mixing ratio
      SL =(!ATMOS.Cpd+parcel_R*!ATMOS.CL+(!ATMOS.LV0-2320.*(TC[J]))*(!ATMOS.LV0-2320.*(TC[J]))*RS/(!ATMOS.Rv*T[J]*T[J]))/T[J]
      SLP=(!ATMOS.Cpd+RS*!ATMOS.CL+(!ATMOS.LV0-2320.*(TC[J]))*(!ATMOS.LV0-2320.*(TC[J]))*RS/(!ATMOS.Rv*T[J]*T[J]))/T[J]
      ;
      ;   ***  Calculate lifted parcel temperature below its LCL   ***
      ;
      IF P[J] GE pLCL && cloud ne 1 THEN BEGIN
        cloud = 0
        TLR[I,J] = first_point eq 1 ? parcel_T : TLR[I,J-1]*(P[J]/P[J-1])^(!ATMOS.Rd/!ATMOS.Cpd) ; used to base on original parcel pressure p[i], but need step-wise calculation
        first_point = 0
        TLP[I,J]=TLR[I,J]
        TLVR[I,J]=TLR[I,J] * (1. + Rrev/eps) / (1. + Rrev) ; note RG and Rrev are equal while unsaturated
        TLVP[I,J]=TLVR[I,J]
        TVRDIF[I,J]=TLVR[I,J]-TvEnv[J]
        TVPDIF[I,J]=TVRDIF[I,J]
      ENDIF ELSE BEGIN
        cloud = 1
        if j lt icb then begin
          icb = j
        endif
        ;
        ;   ***  Iteratively calculate lifted parcel temperature and mixing   ***
        ;   ***    ratios for both reversible and pseudo-adiabatic ascent     ***
        ;
        TG=T[J]
        RG=RS
        LWlast = j gt 0 ? LW[i,j-1] : 0.0
        IWlast = j gt 0 ? IW[i,j-1] : 0.0
        FOR K=1,20 DO BEGIN
          EM=RG*P[J]/(EPS+RG)
          SG=(!ATMOS.Cpd+(Rrev + LWlast + IWlast)*!ATMOS.CL)*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+(!ATMOS.LV0-2320.*(TG-!CONST.T0))*RG/TG
          TG=TG+(S-SG)/SL
          ENEW=6.112 * exp((17.67*((TG-!CONST.T0)))/((TG-!CONST.T0) +243.5))
          RG=EPS*ENEW/(P[J]-ENEW)
          if abs(S-SG) lt 0.01 then break
        ENDFOR ; K
        ice_fraction = ice_fraction > ice*(-TG/40d > 0 < 1); ice is 0 or 1 and can't decrease
        delta_vapor = RG - Rrev
        new_condensate = -delta_vapor
        if fractional_fallout then begin
          ; account for some fallout by multiplying by fraction 0-1.
          new_condensate = new_condensate * (1-fractional_fallout) ; fractional_fallout falls out. (1-fractional_fallout) stays in.
        endif
        condensate = LWlast + IWlast + new_condensate
        IW[I,J] = condensate * ice_fraction
        LW[I,J] = condensate * (1-ice_fraction)
        if fractional_fallout then S = (!ATMOS.Cpd+(RG+LW[I,J]+IW[I,J])*!ATMOS.CL)*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+(!ATMOS.LV0-2320.*(TG-!CONST.T0))*RG/TG
        
        ;amount of ice formed since the last incremental lift
        delta_IW = IW[I,J] - IWlast
        ;latent heat that corresponds to the amount of ice formed
        extra_dQ = delta_IW * (0.3337e6 + (!ATMOS.CL - (2106+7.3*(TG-!CONST.T0)))*(TG-!CONST.T0))
        S = S + extra_dQ/TG
        weighted_heat_capacity = !ATMOS.Cpd*(1.-RG-LW[I,J]-IW[I,J]) + !ATMOS.CL*LW[I,J] + (2106+7.3*(TG-!CONST.T0))*IW[I,J]
        TG = TG + 0.5*(extra_dQ/weighted_heat_capacity) ; not important to be exact here; what's important is the added entropy. 0.5 factor is to account for some ice will be sublimated in subssaturated parcel, cooling it.
        TLR[I,J]=TG
        Rrev = RG ; for entrainment calculation below
        LW[I,J] = LW[i,j] > 0. ; > means take greater of two values.  This removes negatives.
        IW[I,J] = IW[i,j] > 0. 
        TLVR[I,J] = TG * (1. + Rrev/eps) / (1. + Rrev + LW[i,j] + IW[i,j])
        TVRDIF[I,J] = TLVR[I,J] - TvEnv[J]
        ;
        ;   ***  Now do pseudo-adiabatic ascent   ***
        ;
        TG=T[J]
        RG=RS
        FOR K=1,20 DO BEGIN
          CPW = 0.0
          IF J gt 1 then CPW=SUM+!ATMOS.CL*0.5*(RG0+RG)*(ALOG(TG)-ALOG(TG0))
          EM=RG*P[J]/(EPS+RG)
          SPG=!ATMOS.Cpd*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+CPW+(!ATMOS.LV0-2320.*(TG-!CONST.T0))*RG/TG
          TG=TG+(SP-SPG)/SLP
          ENEW=6.112 * exp((17.67*((TG-!CONST.T0)))/((TG-!CONST.T0) +243.5))
          RG=EPS*ENEW/(P[J]-ENEW)
          if abs(SP-SPG) lt 0.01 then break
        ENDFOR ; K
        if TG lt !CONST.T0 && ice then begin
          delta_vapor = RG - RG0
          extra_dQ = -delta_vapor * (0.3337e6 + (!ATMOS.CL - (2106+7.3*(TG-!CONST.T0)))*(TG-!CONST.T0)) * (-TG/40d > 0 < 1)
          SP = SP + extra_dQ/TG
          TG = TG + extra_dQ/!ATMOS.Cpd
        endif
        TLP[I,J]=TG
        TLVP[I,J]= TG * (1. + RG/eps) / (1. + RG)
        TVPDIF[I,J] = TLVP[I,J] - TvEnv[J]
        RG0=RG
        TG0=TG
        SUM=CPW
      ENDELSE
      if entrainment_rate gt 0 && j gt 0 then begin
        dp = p[j] - p[j-1]
        env_r = 0.5*(r[j]+r[j-1])
        env_t = 0.5*(T[j]+t[j-1])
        env_Tv = env_t * (1. + env_r/eps) / (1. + env_r)
        env_p = 0.5*(p[j]+p[j-1])
        dz = -!ATMOS.Rd/!atmos.g * env_Tv * dp / env_p ; if we use Tv, we can use !ATMOS.Rd.
        fract = entrainment_rate/100.d * dz/1000.
        ; linearly mix air temperature, water vapor mixing ratio, liquid water mixing ratio, and ice mixing ratio.
        ; ignore heat capacity of condensed water when mixing temperature.
        TLR[i,j]  = (TLR[i,j] + env_t*fract) / (1.+fract)
        TLP[i,j]  = (TLP[i,j] + env_t*fract) / (1.+fract)
        Rrev      = (Rrev + env_r*fract) / (1.+fract)
        LW[i,j]   = LW[i,j] / (1.+fract)
        IW[i,j]   = IW[i,j] / (1.+fract)
        RG        = (RG   + env_r*fract) / (1.+fract)
        TLVR[i,j] = TLR[i,j] * (1. + Rrev/eps) / (1. + Rrev + LW[i,j] + IW[i,j])
        TLVP[i,j] = TLP[i,j] * (1. + RG/eps)   / (1. + RG)
        TVRDIF[I,J] = TLVR[i,j] - TvEnv[J]
        TVPDIF[I,J] = TLVP[i,j] - TvEnv[J]
        RG0 = RG ; RG0 and TG0 only used in pseudo-adiabatic calculation
        TG0 = TLP[i,j]
        
        
        EM = Rrev*P[j]/(EPS+Rrev)
        RH = EM/(6.112 * exp((17.67*((TLR[i,j]-!CONST.T0)))/((TLR[i,j]-!CONST.T0) +243.5)))
        ; redo reversible entropy
        S = (!ATMOS.Cpd+(Rrev+LW[I,J]+IW[I,J])*!ATMOS.CL)*ALOG(TLR[I,J])-!ATMOS.Rd*ALOG(P[J]-EM)+(!ATMOS.LV0-2320.*(TLR[I,J]-!CONST.T0))*Rrev/TLR[I,J]- Rrev*!ATMOS.Rv*ALOG(RH)
        
        
        EM = RG*P[j]/(EPS+RG)
        RH = EM/(6.112 * exp((17.67*((TLP[i,j]-!CONST.T0)))/((TLP[i,j]-!CONST.T0) +243.5)))
        if n_elements(CPW) eq 0 then CPW = 0.0
        SP = !ATMOS.Cpd*ALOG(TLP[I,J])      -     !ATMOS.Rd*ALOG(P[J]-EM)+CPW+(!ATMOS.LV0-2320.*(TLP[I,J]-!CONST.T0))*RG/TLP[I,J] - RG*!ATMOS.Rv*ALOG(RH)
        ;
        ;   ***   Find new lifted condensation pressure PLCL    ***
        ;
        CHI=TLR[I,J]/(1669.0-122.0*RH-TLR[I,J])
        pLCL = P[J]*RH^CHI
        
      endif
      
    ENDFOR 	; J
    
    
    
    ; Integrate area between parcel Tv and environment.
    ;
    ierr = integrate_area(i, tvpdif, TLVP, p, icb, capep, cinp, bmax=bmaxp, bmin=bminp, plot_points=0,debug=debug)
    ierr = integrate_area(i, tvrdif, TLVR, p, icb, caper, cinr, bmax=bmaxr, bmin=bminr, plot_points=0,debug=debug)
    
    
    
  ENDFOR	; loop over air parcel origins
  
  return
END



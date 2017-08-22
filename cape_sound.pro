function icef, T
  TC = T-!CONST.T0
  icef = -tc/40d
  ; between 0 and 1 (greater of icef and 0; lesser of icef and 1)
  icef = icef > 0 < 1
  ;    if TC le -10 then return, 1 else return, 0.
  return, icef

end


function Ci, T
  TC = T-!CONST.T0
  ; heat capacity of ice - (Apx. 2, Emanuel, 1994)
  ; Ci = 2106 + 7.3*TC ; (TC in deg C)  1959 is specific heat of ice (-20C) J/K/kg
  Ci = 2106 + 7.3*TC
  return, Ci
end


function Tvrt, TKelvin, wkg_per_kg, LW, IW
  T = TKelvin
  w = wkg_per_kg
  if n_elements(LW) eq 0 then LW=0.
  if n_elements(IW) eq 0 then IW=0.
  eps = !ATMOS.Rd/!ATMOS.Rv
  ; same as wallace and hobbs page 72, but divided numerator and denominator by EPS
  return, T * (1. + w/eps) / (1. + w + LW + IW)

end

function Lf, T
  TC = T-!CONST.T0
  ; !ATMOS.CL   ; J/K/kg  heat capacity of liquid water for T >= 0degC
  ; latent heat of fusion
  ; d(Lf)/dT = !ATMOS.CL - Ci
  ; Lf = Lf0 + (!ATMOS.CL - Ci) * T(degC)
  Lf0 = 0.3337e6 ; Lf at 0degC
  ; for future use below, latent heat of fusion.
  ; Used to have Ci(TC), but Ci needs Kelvin not Celsius. Fixed Feb 2 2015
  Lf = Lf0 + (!ATMOS.CL - Ci(T)) * TC

  return, Lf
end

function ALV, TC
  ; ALV = latent heat of vaporization (temperature dependent)
  ; d(ALV)/dT = !ATMOS.Cpv - !ATMOS.CL (negative quantity) Kirchhoff's equation
  ; ALV = ALV0 + (!ATMOS.Cpv - !ATMOS.CL) * T(degC)

  CPVMCL=2320.  ;negative rate of change of latent heat of vaporization w.r.t. temperature
  if CPVMCL ne (!ATMOS.CL - !ATMOS.Cpv) then stop
  ALV=!ATMOS.LV0-CPVMCL*TC

  return, ALV
end

;
; NAME:
; CAPE_SOUND
; PURPOSE:
; Calculate maximum convective available potential energy (CAPE)
;       for a vertical sounding of pressure, temperature and mixing
;       ratio.
;
; INPUT PARAMETERS:
;   P: pressures (hPa) at the N levels
;   T: Temperatures (K) at the N levels
;   R: Water vapor mixing ratio R (kg H2O per kg of dry air) at the N levels
;   Note: Relation between R and specific humidity S: R=S/(1-S)
;
; INPUT KEYWORDS:
;  entrainment_rate : entrainment rate in % per km (default 0.0)
;
; OUTPUT KEYWORDS:
;  Set these keywords to named variables in which the following will be returned:
;  CAPEP: CAPE of each air parcel based on pseudoadiabatic ascent (instantaneous fallout)
;  CAPER: CAPE of each air parcel based on reversible ascent (no fallout)
;  CINP: convective inhibition based on pseudoadiabatic ascent
;  CINR: convective inhibition based on reversible ascent
;  TvPDIF : virtual temperature of parcel minus virtual temperature of environment (pseudoadiabatic)
;  TvRDIF : virtual temperature of parcel minus virtual temperature of environment (reversible)
;  IW: ice water content of pacel at each level
;  LW: liquid water content of parcel at each level
;  Pout: output pressure array with bad values removed
;  Tout: output temperature array without bad values
;  Rout: output mixing ratio array without bad values
;
;
; HISTORY:
;   Based on Fortran program calcsound of Kerry Emanuel
;   Dominik Brunner, KNMI, Feb 2000: converted to IDL
;   and speeded up considerably
;   Ahijevych, NCAR, Aug 2012: went back to Kerry Emanuel's BOOK/sound.f program to add reversible ascent
;    in addition to pseudoadiabatic.
;   Changed from function to procedure
;   Added parcel_origin_p keyword to specify original parcel pressure
;   added entrainment_rate keyword (% per km)
;   added ice keyword ice=1 means include ice effects like latent heat of fusion from freezing or deposition of water
;   Dec 2013 replaced internal esa() function with external esat() function
;   Removed parcel_origin_p keyword in favor of kpar.
;   Jul 2017 added p, r, t keywords to return altered arrays without bad NaN lines.
;   Gave up trying to define parcel quantities as parcel_params input. parcel_params is strictly output.
;


PRO CAPE_SOUND,P_in,T_in,R_in,CAPEP=CAPEP,TVPDIF=TVPDIF,$
  CAPER=CAPER,TVRDIF=TVRDIF,cinr=cinr,cinp=cinp,iw=iw,lw=lw,$
  parcel_layer_mb=parcel_layer_mb, kpar=kpar, $
  entrainment_rate=entrainment_rate, ice=ice, $
  bminr=bminr,bminp=bminp,plcl=plcl_out,parcel_params=parcel_params, $
  TLVP=TLVP, TLVR=TLVR, TLP=TLP, TLR=TLR, $
  bmaxr=bmaxr,bmaxp=bmaxp,debug=debug, fractional_fallout=fractional_fallout, $
  pout=p, tout=t, rout=r

  ; check for input
  IF n_params() LT 3 THEN BEGIN
    message,'Usage: Cape_sound,p,T,R'
    stop
  ENDIF


  igood = where(finite(p_in) and finite(t_in) and finite(r_in), /NULL)
  if igood eq !NULL then return
  p = double(p_in[igood])
  t = double(t_in[igood])
  r = double(r_in[igood])

  N=n_elements(p)	; number of vertical levels
  IF (n_elements(T) NE N) OR (n_elements(R) NE N) THEN BEGIN
    print,'Error: arrays p,T,R must have same size'
    stop
  ENDIF

  if ~keyword_set(entrainment_rate) then entrainment_rate = 0
  if entrainment_rate lt 0 then stop
  if ~keyword_set(ice) then ice=0
  if ~keyword_set(debug) then debug = 0
  show_skewt = debug
  ; show_skewT = 1 just show pseudoadiabatic ascent
  ; show_skewT = 2 show reversible ascent too.



  if ~keyword_set(fractional_fallout) then fractional_fallout = 0.d

  ; make sure levels go from bottom up (high to low pressure)
  if n gt 1 && p[1] gt p[0] then stop

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

  ;parcel_params keyword is an output array of structures, not an input.
  parcel_params = {p:!VALUES.D_NAN,t:!VALUES.D_NAN,r:!VALUES.D_NAN}
  ; Make a parcel_params structure for each level.
  parcel_params = replicate(parcel_params,n)


  ;
  ;   ***   ASSIGN VALUES OF THERMODYNAMIC CONSTANTS     ***
  ;
  atmos_const ; define a system variable !atmos with physical constants - Ahijevych
  ; !ATMOS.CL  = 4190.   ; J/K/kg  heat capacity of liquid water for T >= 0degC
  ; !ATMOS.Cpd ; 1005.7  ; J/K/kg  heat capacity at constant pressure of dry air
  ; !ATMOS.Cpv ; 1870.   ; J/K/kg  heat capacity at constant pressure of water vapor (emanuel 1994)
  ; !ATMOS.Rv  ; 461.5d elsewhere  ; gas constant water vapor = R*/M_H2O
  ; !ATMOS.Rd  ; 287.04d ; gas constant dry air = R*/M_dry
  ; !ATMOS.LV0=2.501E6  ; ALV at 0degC (J/kg)

  EPS=!ATMOS.Rd/!ATMOS.Rv


  ;
  ;  *** Water vapor pressure EV and saturation vapor pressure ES ***
  ;
  TC=T-!CONST.T0	; Celsius
  TvEnv = Tvrt(T,R)
  EV=R*P/(EPS+R)	; vapor pressure
  ES=ESAT(TC)	; saturation vapor pressure

  if show_skewt gt 0 then begin
    prange=[1020,100]
    skewt, [-35.,45.], everyT=10, everyDA=10, prange=prange, /notitle
    DwptCs = rh2tdew(TC, 100*EV/ES )
    plot_skewt, TC, DwptCs, P, col_t=[254,0,17], col_dewpt=[44,253,43], thick=3.5
    plot_skewt, TvEnv - !CONST.T0, !NULL, P, thick=1, col_t=[254,0,17], linestyle=2
  endif

  if n_elements(parcel_layer_mb) eq 0 then parcel_layer_mb=0
  ;
  ;   ***  Begin outer loop, which cycles through parcel origin levels I ***
  ;
  zlow = 0
  zhi  = n/3-1

  ; Limit parcel origin levels to a single level, kpar, if set.
  if n_elements(kpar) gt 0 then begin
    zlow = kpar
    zhi  = kpar
  endif

  ; Range may be a single level if kpar is set, or lowest N/3 levels
  FOR I=zlow,zhi DO BEGIN	; cycle through parcel origin levels I.

    if parcel_layer_mb eq 0 then begin
      parcel_p = p[i]
      parcel_t = t[i]
      parcel_r = r[i]
    endif else begin
      ; If parcel_layer_mb is given use average of top and bottom of pressure range.
      ; If bottom of layer is below sounding, don't extrapolate below sounding;
      ;  instead use the first pressure p[0] as the bottom of pressure range and shift top pressure up
      ; so full parcel_layer_mb is used. That's how tlift.f does it.
      pbot = p[0] < (p[i]+parcel_layer_mb/2.)
      ptop = pbot - parcel_layer_mb
      pss = [pbot,ptop]
      in_between = where(p lt pbot and p gt ptop, nin) ; changed le and ge to lt and gt - July 9 2017
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
      if show_skewt gt 0 then begin
        tx = temps[0]- !CONST.T0
        dtx = 0.3
        plots, [tx,tx], [ptop,pbot]
        plots, [tx-dtx,tx+dtx], ptop
        plots, [tx-dtx,tx+dtx], pbot
      endif
    endelse


    if show_skewt gt 0 then begin
      plots, tnew(Tvrt(parcel_t,parcel_r)-!CONST.T0, parcel_p), parcel_p, psym=2
      plots, tnew(tmr(parcel_r*1000., parcel_p) - !CONST.T0, parcel_p), parcel_p, psym=2
    endif
    parcel_params[i] = {p:parcel_p,t:parcel_t,r:parcel_r}
    ;
    ;   ***  Define the pseudo-adiabatic entropy SP (conserved variable) ***
    ;
    parcel_TC=parcel_T-!CONST.T0 ; Celsius
    ;parcel_EV=parcel_R*P[i]/(EPS+parcel_R)  ; vapor pressure
    parcel_EV=parcel_R*parcel_P/(EPS+parcel_R)  ; vapor pressure
    parcel_ES=ESAT(parcel_TC) ; saturation vapor pressure

    ;RS=EPS*parcel_ES/(P[I]-parcel_ES)
    RS=EPS*parcel_ES/(parcel_P-parcel_ES)
    EM=(parcel_EV GT 1.e-6)*parcel_EV+(parcel_EV LE 1e-6)*1e-6


    S=(!ATMOS.Cpd+parcel_R*!ATMOS.CL)*ALOG(parcel_T)-!ATMOS.Rd*ALOG(parcel_P-parcel_EV)+$ ; P[I]-parcel_EV)+$
      ALV(parcel_TC)*parcel_R/parcel_T-parcel_R*!ATMOS.Rv*ALOG(EM/parcel_ES)

    SP=           !ATMOS.Cpd*ALOG(parcel_T)         -!ATMOS.Rd*ALOG(parcel_P-parcel_EV)+$ ; P[I]-parcel_EV)+$
      ALV(parcel_TC)*parcel_R/parcel_T-parcel_R*!ATMOS.Rv*ALOG(EM/parcel_ES)

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
    ice_fraction = icef(TG0)
    first_point = 1
    FOR J=I,N-1 DO BEGIN
      if p[j] gt parcel_p then continue
      ;
      ;   ***  Calculate estimates of the rates of change of the entropies  ***
      ;   ***           with temperature at constant pressure               ***
      ;
      RS=EPS*ES[J]/(P[J]-ES[J])	; saturation mixing ratio
      SL =(!ATMOS.Cpd+parcel_R*!ATMOS.CL+ALV(TC[J])*ALV(TC[J])*RS/(!ATMOS.Rv*T[J]*T[J]))/T[J]
      SLP=(!ATMOS.Cpd+RS*!ATMOS.CL+ALV(TC[J])*ALV(TC[J])*RS/(!ATMOS.Rv*T[J]*T[J]))/T[J]
      ;
      ;   ***  Calculate lifted parcel temperature below its LCL   ***
      ;
      IF P[J] GE pLCL && cloud ne 1 THEN BEGIN
        cloud = 0
        TLR[I,J] = first_point eq 1 ? parcel_T : TLR[I,J-1]*(P[J]/P[J-1])^(!ATMOS.Rd/!ATMOS.Cpd) ; used to base on original parcel pressure p[i], but need step-wise calculation
        first_point = 0
        TLP[I,J]=TLR[I,J]
        ;  TLVR[I,J]=TLR[I,J]*(1.+parcel_R/EPS)/(1.+parcel_R) ; account for entrainment so use Rrev instead of original parcel_R
        TLVR[I,J]=Tvrt(TLR[I,J],Rrev) ; note RG and Rrev are equal while unsaturated
        TLVP[I,J]=TLVR[I,J]
        TVRDIF[I,J]=TLVR[I,J]-TvEnv[J]
        TVPDIF[I,J]=TVRDIF[I,J]
      ENDIF ELSE BEGIN
        cloud = 1
        if j lt icb then begin
          icb = j
          if show_skewt gt 0 then begin
            ; Label 'LCL' and add plus sign
            xyouts, tnew(TLP[i,j-1]-!CONST.T0,pLCL), pLCL, ' LCL', charsize=0.8
            plots, tnew(TLP[i,j-1]-!CONST.T0,pLCL), pLCL, psym=1
          endif
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
          ; SG=(!ATMOS.Cpd+parcel_R*!ATMOS.CL)*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+ALV(TG-!CONST.T0)*RG/TG
          SG=(!ATMOS.Cpd+(Rrev + LWlast + IWlast)*!ATMOS.CL)*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+ALV(TG-!CONST.T0)*RG/TG
          TG=TG+(S-SG)/SL
          ENEW=ESAT(TG-!CONST.T0)
          RG=EPS*ENEW/(P[J]-ENEW)
          if abs(S-SG) lt 0.01 then break
        ENDFOR ; K
        if abs(S-SG) gt 0.01 && p[j] gt 10. then begin
          print, 's and sg did not converge at level',p[j]
          print, s, sg, s-sg
          stop
        endif
        ice_fraction = max([ice_fraction,ice*icef(TG)]); ice is 0 or 1 and can't decrease
        delta_vapor = RG - Rrev
        new_condensate = -delta_vapor
        if fractional_fallout then begin
          ; account for some fallout by multiplying by fraction 0-1.
          new_condensate = new_condensate * (1-fractional_fallout) ; fractional_fallout falls out. (1-fractional_fallout) stays in.
        endif
        condensate = LWlast + IWlast + new_condensate
        IW[I,J] = condensate * ice_fraction
        LW[I,J] = condensate * (1-ice_fraction)
        if fractional_fallout then S = (!ATMOS.Cpd+(RG+LW[I,J]+IW[I,J])*!ATMOS.CL)*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+ALV(TG-!CONST.T0)*RG/TG

        ;amount of ice formed since the last incremental lift
        delta_IW = IW[I,J] - IWlast
        ;latent heat that corresponds to the amount of ice formed
        extra_dQ = delta_IW * Lf(TG)
        S = S + extra_dQ/TG
        weighted_heat_capacity = !ATMOS.Cpd*(1.-RG-LW[I,J]-IW[I,J]) + !ATMOS.CL*LW[I,J] + Ci(TG)*IW[I,J]
        TG = TG + 0.5*(extra_dQ/weighted_heat_capacity) ; not important to be exact here; what's important is the added entropy. 0.5 factor is to account for some ice will be sublimated in subssaturated parcel, cooling it.
        TLR[I,J]=TG
        ;        TLVR[I,J]=TG*(1.+RG/EPS)/(1.+parcel_R)
        ;        LW[I,J] = parcel_R - RG
        Rrev = RG ; for entrainment calculation below
        LW[I,J] = LW[i,j] > 0. ; > means take greater of two values.  This removes negatives.
        IW[I,J] = IW[i,j] > 0.
        ;TLVR[I,J]=TG*(1.+Rrev/EPS)/(1.+(Rrev+LW[i,j]+IW[i,j]))
        TLVR[I,J] = Tvrt(TG,Rrev,LW[i,j],IW[i,j])
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
          SPG=!ATMOS.Cpd*ALOG(TG)-!ATMOS.Rd*ALOG(P[J]-EM)+CPW+ALV(TG-!CONST.T0)*RG/TG
          TG=TG+(SP-SPG)/SLP
          ENEW=ESAT(TG-!CONST.T0)
          RG=EPS*ENEW/(P[J]-ENEW)
          if abs(SP-SPG) lt 0.01 then break
        ENDFOR ; K
        if abs(SP-SPG) gt 0.01 && p[j] gt 60 then stop ; changed from 10mb to 60
        if TG lt !CONST.T0 && ice then begin
          delta_vapor = RG - RG0
          extra_dQ = -delta_vapor * Lf(TG) * icef(TG)
          SP = SP + extra_dQ/TG
          TG = TG + extra_dQ/!ATMOS.Cpd
        endif
        TLP[I,J]=TG
        TLVP[I,J]= Tvrt(TG,RG)
        TVPDIF[I,J] = TLVP[I,J] - TvEnv[J]
        RG0=RG
        TG0=TG
        SUM=CPW
      ENDELSE
      if entrainment_rate gt 0 && j gt 0 then begin
        dp = p[j] - p[j-1]
        env_r = 0.5*(r[j]+r[j-1])
        ; technically shouldn't we average potential temperature, not temperature?
        env_t = 0.5*(T[j]+t[j-1])
        env_Tv = Tvrt(env_t,env_r)
        env_p = 0.5*(p[j]+p[j-1])
        dz = -!ATMOS.Rd/!atmos.g * env_Tv * dp / env_p ; if we use Tv, we can use !ATMOS.Rd.
        fract = entrainment_rate/100.d * dz/1000.
        if ~finite(fract) || ~finite(dz) then stop
        ; linearly mix air temperature, water vapor mixing ratio, liquid water mixing ratio, and ice mixing ratio.
        ; ignore heat capacity of condensed water when mixing temperature.
        ; technically shouldn't we mix potential temperature, not temperature?
        TLR[i,j]  = (TLR[i,j] + env_t*fract) / (1.+fract)
        TLP[i,j]  = (TLP[i,j] + env_t*fract) / (1.+fract)
        Rrev      = (Rrev + env_r*fract) / (1.+fract)
        LW[i,j]   = LW[i,j] / (1.+fract)
        IW[i,j]   = IW[i,j] / (1.+fract)
        RG        = (RG   + env_r*fract) / (1.+fract)
        ;TLVR[i,j] = TLR[i,j] *(1.+Rrev/EPS)/(1.+(Rrev+LW[i,j]+IW[I,J]))
        TLVR[i,j] = Tvrt(TLR[i,j], Rrev, LW[i,j], IW[i,j])
        TLVP[i,j] = Tvrt(TLP[i,j], RG)
        TVRDIF[I,J] = TLVR[i,j] - TvEnv[J]
        TVPDIF[I,J] = TLVP[i,j] - TvEnv[J]
        RG0 = RG ; RG0 and TG0 only used in pseudo-adiabatic calculation
        TG0 = TLP[i,j]


        EM = Rrev*P[j]/(EPS+Rrev)
        RH = EM/ESAT(TLR[i,j]-!CONST.T0)
        ; redo reversible entropy
        S = (!ATMOS.Cpd+(Rrev+LW[I,J]+IW[I,J])*!ATMOS.CL)*ALOG(TLR[I,J])-!ATMOS.Rd*ALOG(P[J]-EM)+ALV(TLR[I,J]-!CONST.T0)*Rrev/TLR[I,J]- Rrev*!ATMOS.Rv*ALOG(RH)


        EM = RG*P[j]/(EPS+RG)
        RH = EM/ESAT(TLP[i,j]-!CONST.T0)
        if n_elements(CPW) eq 0 then CPW = 0.0
        SP = !ATMOS.Cpd*ALOG(TLP[I,J])      -     !ATMOS.Rd*ALOG(P[J]-EM)+CPW+ALV(TLP[I,J]-!CONST.T0)*RG/TLP[I,J] - RG*!ATMOS.Rv*ALOG(RH)
        ;
        ;   ***   Find new lifted condensation pressure PLCL    ***
        ;
        CHI=TLR[I,J]/(1669.0-122.0*RH-TLR[I,J])
        pLCL = P[J]*RH^CHI

      endif

      if show_skewt gt 1 && j mod 30 eq 0 then xyouts, 8+!X.CRANGE[1]-entrainment_rate*0,p[j], $
        string(Rrev*1000, LW[i,j]*1000, Rrev*P[j]/(EPS+Rrev)/ESAT(TLR[I,J]-!CONST.T0)*100,S,format='(3F6.1,F9.1)'), align=1,charsize=0.83, charthick=0.3
      if show_skewt gt 0 then begin
        ;forward_function tnew
        if show_skewt gt 1 then plots, tnew(TLVR[i,j]-!CONST.T0,P[j]), P[j], psym=3, color=183
        plots, tnew(TLVP[i,j]-!CONST.T0,P[j]), P[j], psym=3, color=208
      endif
    ENDFOR 	; J
    ; remember this will not follow the skew-T pseudoadiabats exactly because of virtual temperature effects
    if show_skewt gt 0 then plot_skewt, TLVP[i,*] - !CONST.T0, !NULL, P, thick=1, col_t=(!D.NAME eq 'X')*255, col_dewpt=222



    ; Integrate area between parcel Tv and environment.
    ;
    ierr = integrate_area(i, tvpdif, TLVP, p, icb, capep, cinp, bmax=bmaxp, bmin=bminp, plot_points=show_skewt gt 0,debug=debug)
    ierr = integrate_area(i, tvrdif, TLVR, p, icb, caper, cinr, bmax=bmaxr, bmin=bminr, plot_points=show_skewt gt 1,debug=debug)



  ENDFOR	; loop over air parcel origins

  return
END

pro some_predict_stuff_using_avg_sndg
  ; is this composite soundings, mean sounding, average sounding?
  ; no, for George Bryan, I think I used a shell script that
  ; used ~/bin/stats
  ; it is in basedir+subdir+'do_all.csh'
  ; and it uses avg_sndng.pl (in the same directory).
  ; Don't use this version. Use the one below (average_sounding_not_using_avg_sndg)
  basedir = '/pecan2/ahijevyc/PREDICT/analysis/'
  subdir  = 'entrainment10_ice/0.5rev_fallout'
  cd, basedir+subdir
  loadct, 39, /silent
  if !D.NAME eq 'PS' then device, /close
  if !D.NAME eq 'PS' then !P.CHARTHICK=2.
  radii = ['lt200km', 'ge200km']
  cloud_types = ['ge-20CIR_']

  skewt, [-20.,36.9], everyT=10, everyDA=10

  for irad = 0, n_elements(radii) - 1 do begin
    radius = radii[irad]
    for i = 0, n_elements(cloud_types)-1 do begin
      cloud_type = cloud_types[i]
      suff = cloud_type+radius+'.txt'
      dev = read_ascii('developing_le2days_prior_'+suff)
      all = read_ascii('all_'+suff)
      non = read_ascii('non-developing_'+suff)

      ;      skewt, [-20.,36.9], title=cloud_type+" "+radius, everyT=10, everyDA=10
      plot_skewt, non.field1[1,*], non.field1[2,*], non.field1[0,*], col_t=222, col_dewpt=222, thick=4, linestyle=irad
      plot_skewt, dev.field1[1,*], dev.field1[2,*], dev.field1[0,*], col_t=111, col_dewpt=111, thick=4, linestyle=irad
    endfor

  endfor

  cape_sound,reverse(non.field1[0,*],2),reverse(non.field1[1,*],2),reverse(mixr_sat(non.field1[2,*],non.field1[0,*])/1000.,2), caper=caper, capep=capep
  if !D.NAME eq 'PS' then device, /close
  !P.CHARSIZE = 1.
  !P.CHARTHICK= 1.
end


pro average_sounding_not_using_avg_sndg
  ; duplicates shell script do_all.csh
  ; but average dewpoint is done via mixing ratio - the correct way.
  ; do_all.csh does not average dewpoint the correct way. There's a slight
  ; difference between the 2 methods - the incorrect way looks a little drier above 700mb.
  atmos_const ; define a system variable !atmos with physical constants - Ahijevych

  basedir = '/pecan2/ahijevyc/PREDICT/analysis/'
  subdir  = 'entrainment10_ice/0.5rev_fallout/'
  cd, basedir+subdir
  loadct, 39, /silent
  skewt, [-20.,36.9], everyT=10, everyDA=10
  radii = [[0,200],[200,700]]
  delta_radius = 50
  max_radius = 700
  nradius = max_radius/delta_radius
  radii = transpose(delta_radius*[lindgen(nradius),1+lindgen(nradius)])
  cloud_type =  'all';'ge-20CIR_'

  types = ['developing_le2days_prior', 'non-developing']

  for irad = 0, n_elements(radii)/2 - 1 do begin
    min_radius = radii[0,irad]
    max_radius = radii[1,irad]
    radius = string(min_radius, max_radius, format='(I4.4,"-",I4.4,"km")')
    for itype = 0, n_elements(types)-1 do begin
      type = types[itype]
      infiles = file_search(basedir+subdir+type+'_0????mb*r.txt', count=ninfiles)
      Presss = replicate(!VALUES.F_NAN, ninfiles)
      TempCs = replicate(!VALUES.F_NAN, ninfiles)
      DwptCs = replicate(!VALUES.F_NAN, ninfiles)
      mixing_ratios = replicate(!VALUES.F_NAN, ninfiles)
      Zs     = replicate(!VALUES.F_NAN, ninfiles)
      for iinfile = 0, ninfiles-1 do begin
        file = infiles[iinfile]
        Press = strmid(file_basename(file),strlen(type)+1,5)
        get_scatter, file, TempC, DwptC, TvC, distance_km, IR, P, Z, U, V, GFS_mean, other=other
        ibad = where(P ne Press, nbad)
        if nbad gt 0 then stop
        correct_dist = distance_km ge min_radius and distance_km lt max_radius
        if cloud_type eq 'ge-20CIR_' then correct_cloud = IR ge -20
        if cloud_type eq 'all' then correct_cloud = finite(IR)
        i = where(finite(TempC) and finite(DwptC) and correct_cloud and correct_dist)
        mixing_ratio = mixr_sat(DwptC[i], P[i])
        presss[iinfile] = mean(P[i])
        TempCs[iinfile] = mean(TempC[i])
        Zs[iinfile] = mean(Z[i],/nan)
        mixing_ratios[iinfile] = mean(mixing_ratio) ; no [i] here. mixing_ratio already defined by it above.
        DwptCs[iinfile] = rh2tdew(mean(TempC[i]), 100*mean(mixing_ratio)/mixr_sat(mean(TempC[i]),press) )
      endfor
      openw, lun, type+"_"+radius+'.txt',/get_lun
      printf, lun, type, " ", radius
      printf, lun, "height(m) theta(K) mixing_ratio(g/kg)"
      for i = 0, ninfiles-1 do  printf, lun, string(Zs[i], $
        (TempCs[i]+!CONST.T0)*(1000./Presss[i])^(!ATMOS.RD/!ATMOS.CPD), $
        mixing_ratios[i], format='(F8.2,F9.3,F8.4)')
      free_lun, lun
      if type eq 'non-developing' then plot_skewt, TempCs, DwptCs, Presss, col_t=222, col_dewpt=222, thick=4, linestyle=irad
      if type eq 'developing_le2days_prior' then plot_skewt, TempCs, DwptCs, Presss, col_t=111, col_dewpt=111, thick=4, linestyle=irad
    endfor
  endfor

end



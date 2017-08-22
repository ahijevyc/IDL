function integrate_area, i_in, TvDIF_in, TLV_in, p_in, icb_in, cape, cin, bmin=bmin, bmax=bmax, plot_points=plot_points, debug=debug
  if ~keyword_set(plot_points) then plot_points=0
  if ~keyword_set(debug) then debug=0
  if ~keyword_set(bmin) then stop ; bmin and bmax are mandatory
  if ~keyword_set(bmax) then stop
  
  
  ; not the best way, but TvDIF_in, TLV_in, cape, cin, and bmax all have a dimension that corresponds
  ; to the origin level of the parcel.
  ; This function only operates on one of the origin levels, and that is level i_in.
  ;
  i=i_in
  TvDIF=TvDIF_in
  TLV=TLV_in
  p=p_in
  icb=icb_in
  
  
  ;I know there are a lot of [i] subscripts here; I copied from inside the i loop.
  ;
  ;
  ;       adapted from Emanuel's convect.f
  ;  ***  FIND THE PARCEL'S LOWEST and HIGHEST LEVEL OF POSITIVE BUOYANCY IN CLOUD ***
  ;
  INB=i
  iposbuoyancy = where(TvDIF[I,*]+shift(TvDIF[I,*],[0,1]) gt 0.0 and p le p[icb], nposbuoyancy, $
    complement=inegbuoyancy)
  if nposbuoyancy gt 0 then begin
    ; extract Bmax - if there is positive buoyancy within cloud.
    bmax[i] = max(TvDIF[I,iposbuoyancy],/nan)
    INB = max(iposbuoyancy) ; index of level of neutral buoyancy. highest level with positive buoyancy
    ; find first negatively buoyant parcel level below iNB. This is iLFC-1.
    ; Found strange case where it happens near the tropopause. 
    for iLFC = inb,i+1,-1 do if ~finite(TvDIF[i,iLFC-2]) || TvDIF[i,iLFC-1] le 0.0 && p[iLFC] gt 300 then break
    plfcp_exact = (TvDIF[i,iLFC]*p[iLFC-1]-TvDIF[i,iLFC-1]*p[iLFC])/(TvDIF[I,iLFC]-TvDIF[I,iLFC-1])
    tlfcp_exact = TLV[i,iLFC-1] + (TLV[i,iLFC]-TLV[i,iLFC-1])/(P[iLFC]-P[iLFC-1])*(plfcp_exact-p[iLFC-1])
    if plot_points then begin
      xyouts, tnew(tlfcp_exact-!CONST.T0,plfcp_exact), plfcp_exact, ' LFC', charsize=0.7, charthick=!P.CHARTHICK*0.5, color=208
      plots, tnew(tlfcp_exact-!CONST.T0,plfcp_exact), plfcp_exact, psym=1, color=208
    endif
    ;
    ;  ***  Redefine INB if there is an intervening stable layer   ***
    ;  ***  that the parcel can't punch through using cape below  ***
    ;       inspired by Emanuel's convect.f
    ;
    if (0) then begin
      INBOLD=INB
      up_momentum=0.0
      old = 0.0
      still_buoyant=iLFC
      for J= iLFC,INBOLD do begin
        TVM=0.5*(TvDIF[I,J]+TvDIF[I,J-1])
        PM=0.5*(P[J]+P[J-1])
        BY= !ATMOS.Rd*TVM*(P[J-1]-P[J])/PM
        up_momentum = up_momentum +BY
        if TVM gt 0 then still_buoyant = j
        IF up_momentum GT 0 THEN INB=still_buoyant else break
        if debug then if TVM * old lt 0 then print, 'pseudo parcel T deviation changed sign ', p[j]
        old = TVM
      endfor
      if debug && INB ne inbold then print, 'pseudo parcel neutral buoy level changed from ',p[inbold],' to', p[INB]
    endif
    if plot_points then begin
      xyouts, tnew(TLV[i,INB]-!CONST.T0,P[INB]), P[INB], ' NB', charsize=0.7, charthick=!P.CHARTHICK*0.5, color=208
      plots, tnew(TLV[i,INB]-!CONST.T0,P[INB]), P[INB], psym=1, color=208
    endif
    ;
    ;   ***  Do updraft loops        ***
    
    NegArea = 0.
    PosArea = 0.
    IF INB GT I THEN BEGIN
      FOR J=(I+1),INB DO BEGIN
        ; what do we do about TVM NaNs in the lowest levels?
        if ~finite(TvDIF[I,J-1]) then continue
        TVM=0.5*(TvDIF[I,J]+TvDIF[I,J-1])
        PM=0.5*(P[J]+P[J-1])
        IF TVM LE 0.0 THEN $
          NegArea=NegArea-!ATMOS.Rd*TVM*(P[J-1]-P[J])/PM $
        ELSE $
          PosArea=PosArea+!ATMOS.Rd*TVM*(P[J-1]-P[J])/PM
        if j eq iLFC then begin
          cin[i] = NegArea-PosArea
          NegArea = 0.0
          PosArea = 0.0
        endif
      ENDFOR
      cape[I]=PosArea-NegArea
    ENDIF
  endif Else begin
    cape[i]=0.0
    bmax[i]=0.0
  endelse

  ; extract Bmin - used to do this before getting inb, but one needs to be below inb.
  BminTop = 500.
  iBminTop = value_locate(p, BminTop) ; get the index below BminTop to bound bmin
  if nposBuoyancy gt 0 then iBminTop = iNB < iBminTop ; bound iBminTop below level of neutral buoyancy if there's positive area
  
  bmin[i] = min(TvDIF[I,0:iBminTop],kbmin,/nan)
  if plot_points then begin
    xs =  [tnew(TLV[i,kbmin]-TvDIF[i,kbmin],p[kbmin]), tnew(TLV[i,kbmin],p[kbmin])]-!CONST.T0
    bmin_c = !D.NAME eq 'X' ? 186 : 28
    plots, xs, replicate(p[kbmin],2), color=bmin_c
    xyouts, mean(xs), p[kbmin], 'Bmin', align=0.5, color=bmin_c, charsize=0.8
  endif
  

  return, 0
end



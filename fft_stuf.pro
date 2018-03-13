function fft_stuf, x, times, thresh_sec, dt=dt, _extra=_extra

  ; Before removing wave periods below a threshold via FFT filtering
  ;  interpolate MTP vertical profiles to regular sampling interval.
  ;
  ; x is the input array with dimensions (nsamples, nalts)
  
  ; Sample times come in irregularly spaced and return regularly spaced every 20s.
  ; A new array with times regularly spaced is returned.
  
  if n_elements(dt) eq 0 then dt = 20. ; regular interval in seconds
  
  ; thresh_sec is the upper limit for wave period in seconds
  if n_elements(thresh_sec) eq 0 then thresh_sec = 0.
  
  ; remember old times so you can interpolate from them to new times
  old_times = times
  ; make sure new times array doesn't go past old one. (by substracting a small finite number)
  times = timegen(start=min(times), final=max(times)-1e-10, step_size=dt, units='seconds')
  
  ; number of vertical levels does not change
  nalt = n_elements(x)/n_elements(old_times)
  
  ; new x will have new time-dimension size
  if nalt eq 1 then new_x = interpol_NaN(x,old_times,times) else begin
    new_x = dblarr(n_elements(times),nalt)
    for ialt=0,nalt-1 do new_x[*,ialt] = interpol_NaN(x[*,ialt], old_times, times)
  endelse
  x = new_x
  n = (size(x,/dim))[0]
  
  ; Transform the image into the frequency domain
  ffTransform = FFT(x, dimension=1)
  
  
  ; calculate frequency
  f = (FINDGEN((N - 1)/2) + 1)
  is_N_even = (N MOD 2) EQ 0
  if (is_N_even) then $
    freq = [0.0, f, N/2, -N/2 + f]/(N*dT) $
  else freq = [0.0, f, -(N/2 + 1) + f]/(N*dT)
  
  freq2D = freq#replicate(1,nalt)
  ; only allow 1/period (frequency) below threshold to pass.
  pass = abs(freq2D) lt 1./thresh_sec
  ; Apply the mask to the transform to exclude the noise.
  maskedTransform = ffTransform*pass
  
  title = thresh_sec eq 0 ? '' : string(thresh_sec,format='("thresh",x,I0," sec")')
  props = {xlog:1,ylog:1,xticklen:1.,xstyle:1,yticklen:0.5,xsubticklen:0.02,ysubticklen:0.05,title:title}
  if n_elements(_extra) then props = create_struct(props, _extra)
  
  y = abs(ffTransform)^2
  y2 = abs(maskedTransform)^2
  
  ; First plot wave period
  ;  x = 1/freq
  ;  pfft = plot(x,y, name='raw', xtitle='wave period (sec)',ytitle = 'power density ($K^2$/s)', _extra = props, /current)
  ;  pmfft = plot(x,y2, 'r', overplot=pfft, name='filtered')
  ;  l = legend()
  
  ; According to Chris,  Normally, we would plot Power vs. wavenumber.
  ; Wavenumber can be computed as k = 2*pi/(c*T), where c = GV air speed and T is the period, so cT = wavelength.
  c = 205.778 / 1000.; km/s ; 400.knots
  x = 2.*!DPI*freq/c
  pfft = plot(x,y, name='raw', xtitle='wavenumber k (km$^{-1}$)',ytitle = 'Power ($units^2$ km)', _extra = props, /overplot, /current)
  ax = pfft.axes
  ax[2].tickvalues = ax[0].tickvalues * 2. * !DPI / 10.
  ax[2].tickname = string(round(2.*!DPI/ax[2].tickvalues), format='(i0)')
  ax[2].title = 'wavelength (km)'
  ax[2].showtext=1
  ax[2].tickdir=0
  ax[2].ticklen=0.02
  ax[2].subticklen=0
  
  if thresh_sec ne 0 then pmfft = plot(x,y2, 'r', overplot=pfft, name='filtered')
  
  ; Find the legend if it exists and delete it.
  tool = pfft.Window.gettool()
  id = igetid('legend',tool=tool)
  if id ne '' then begin
    obj = tool->GetByIdentifier(id)
    obj->idlitcomponent::getproperty, _proxy=p1
    p1.Delete
  endif

  id = igetid('minus five-thirds', tool=tool)
  if id eq '' then fivethirds = plot(pfft.xrange,pfft.XRANGE^(-5./3), 'b:2', overplot=pfft, name='minus five-thirds')
  ; inertial subrange between energy input and dissipation

  l = legend(/relative, position=[0.39,0.3],font_size=10,vertical_spacing=0.01)
  
  
  
  ; Then plot wave frequency
  ;  x = freq
  ;  pfft = plot(x,y, name='raw', xtitle='frequency (Hz)', ytitle='power density ($K^2$/Hz)', _extra=props)
  ;  pmfft = plot(x,y2, 'r', overplot=pfft, name='filtered')
  ;  l = legend()
  
  
  
  ; Perform an inverse FFT to the masked transform, to
  ; transform it back to the spatial domain.
  inverseTransform = REAL_PART(FFT(maskedTransform, /INVERSE, dimension=1))
  
  ; Display the result of the inverse transformation.
  return, inverseTransform
  
end
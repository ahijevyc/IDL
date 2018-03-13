pro read_mode_analysis

  ; Run /pecan/ahijevyc/Vx/subj_eval/do_mode_analysis first - 20080703
  
  if !D.NAME eq 'PS' then device, /close, /color, bits_per_pixel=8
  loadct, 39 
  tvlct, 0, 0, 0, 0
  pos = [0.12,0.12,0.88,0.9]

  area_weight = 1
  
  
t = { $
   VERSION:       1.00000,$
   DATASTART:     0L,$
   DELIMITER:     32B,$
   MISSINGVALUE:  !VALUES.F_NAN,$
   COMMENTSYMBOL: '',$
   FIELDCOUNT:    15,$
   FIELDTYPES:    [7, 3, 7, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4], $
   FIELDNAMES:    ['obs_thr', 'obs_rad', 'model', 'field', 'n', 'min', 'max', 'mean', 'stddev', 'p10', 'p25', 'p50', 'p75', 'p90', 'sum'],$
   FIELDLOCATIONS:2+7*lindgen(15),$
   FIELDGROUPS:   lindgen(15) }
   infile = '/pecan/ahijevyc/Vx/subj_eval/simpleObsMatched.txt'

   
  infile = '/pecan/ahijevyc/Vx/subj_eval/simpleObsMatched.txt'
  infile2 = '/pecan/ahijevyc/Vx/subj_eval/simpleObs.txt'
  a = read_ascii(infile, template=t)
  b = read_ascii(infile2, template=t)
   
   models = a.model[uniq(a.model, sort(a.model))]
   obs_thrs = a.obs_thr[uniq(a.obs_thr, sort(a.obs_thr))]
   obs_rads = a.obs_rad[uniq(a.obs_rad, sort(a.obs_rad))]

 
  format = '(A12, 9F12.4)'
  area = fltarr(n_elements(obs_rads), n_elements(obs_thrs), n_elements(models))
  nobj = fltarr(n_elements(obs_rads), n_elements(obs_thrs), n_elements(models))
  for im = 0, n_elements(models)-1 do begin

    model = models[im]

    print, model, obs_rads, format = format 
    for i= n_elements(obs_thrs)-1, 0, -1 do begin
      bindex = where(b.obs_thr eq obs_thrs[i] and b.model eq model)
      if area_weight then begin
        totalsums = b.sum[bindex]
        row = a.sum[where(a.model eq model and a.obs_thr eq obs_thrs[i])]/totalsums
      endif else begin
        totalsums = b.n[bindex]
        row = a.n[where(a.model eq model and a.obs_thr eq obs_thrs[i])]/totalsums
      endelse
      print, obs_thrs[i], row, format = format
      area[*,i,im] = row
      nobj[*,i,im] = b.n[bindex]
    endfor
  endfor
  areacolor = bytscl(area, top=254, /nan)
  for im = 0, n_elements(models)-1 do begin
    tst = area_weight ? ' area' : 's'
    erase
    tv, areacolor[*,*,im], pos[0], pos[1], /normal, xsize=pos[2]-pos[0], ysize=pos[3]-pos[1]
    plot, [0], [0], xrange = [0, n_elements(obs_rads)], yrange =  [0, n_elements(obs_thrs)], /nodata, xstyle=1, ystyle=1, charsize=0.7, $
    title = models[im]+' % observed object'+tst+' matched to forecast object', $
    xminor=1, xtitle='convolution radius', xtickinterval=1, xtickname=strtrim(obs_rads*4,2)+'km', xticklayout=2, xticklen=1, yticklen=1, $
    ytickname=strmid(obs_thrs,0,5), ytickinterval=1, yticklayout=2, yminor=1, ytitle='precipitation threshold [mm]', /noerase, position=pos
    for i= n_elements(obs_thrs)-1, 0, -1 do begin
      ; only write boxes with finite numbers. 
      ifinite = where(finite(area[*,i,im]), nfinite)
      if nfinite gt 0 then xyouts, (indgen(n_elements(obs_rads))+0.5)[ifinite], i+0.36, string(100.*(area[*,i,im])[ifinite], format='(I3, "%")'), charthick=2, charsize=0.8, align=0.5
      ifinite = where(finite(nobj[*,i,im]), nfinite)
      if nfinite gt 0 then xyouts, (indgen(n_elements(obs_rads))+0.2)[ifinite], i+0.05, string((nobj[*,i,im])[ifinite], format='("n=",I3)'), charthick=1, charsize=0.5
    end
    levs = findgen(11)/10.*(max(area, /nan)-min(area, /nan)) + min(area, /nan)
    contourbar, levs, mycolors(levs), position=[pos[2]+0.06, pos[1], pos[2]+0.07, pos[3]], /vertical, format='(F4.2)'
  endfor


 
  if !D.NAME eq 'PS' then device, /close

end
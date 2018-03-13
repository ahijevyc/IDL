pro r

; reads the ascii file fort.22 produced by ~tuttle/uswrp/hovm/linux/plot_hov
; 

filename = 't.22'

ascii_temp = { $
   VERSION:       1.00000,$
   DATASTART:     0L,$
   DELIMITER:     0B,$
   MISSINGVALUE:  !VALUES.F_NAN,$
   COMMENTSYMBOL: '',$
   FIELDCOUNT:    10L,$
   FIELDTYPES:    [3,7,3,4,7,3,4,4,4,4], $
   FIELDNAMES:    ['ipts','idate1','time1','xbeg','idate2','time2','xend','dist','tlen','spd'],$
   FIELDLOCATIONS:  [16,22,32,40,47,57,65,72,79,86],$
   FIELDGROUPS:  indgen(10) }

d = read_ascii(filename,template=ascii_temp)

month1 = strmid(d.idate1,4,2)
day1   = strmid(d.idate1,6,2)
year1  = strmid(d.idate1,0,4)
hour1  = d.time1/10000L
min1   = (d.time1-hour1*10000)/100
sec1   = d.time1-hour1*10000-min1*100
dhour1 = hour1 + min1/60. + sec1/3600.

month2 = strmid(d.idate2,4,2)
day2   = strmid(d.idate2,6,2)
year2  = strmid(d.idate2,0,4)
hour2  = d.time2/10000L
min2   = (d.time2-hour2*10000)/100
sec2   = d.time2-hour2*10000-min2*100
dhour2 = hour2 + min2/60. + sec2/3600.

juldate1=julday(month1,day1,year1,hour1,min1,sec1)
juldate2=julday(month2,day2,year2,hour2,min2,sec2)

streak_age_now = 6.0
very_old_streak = 12.0
; tvar is the current time (starting time plus streak age [h])
tvar = (dhour1 + streak_age_now) mod 24.

; dvar : current longitude
;	starting longitude + streak_age_now * streak_speed [deg/h]
; Don't use d.tlen for delta time. It is not precise. It must have been saved with 1 decimal place when it needed 2 in order to accomodate quarter-hours.

total_streak_age = (juldate2-juldate1)*24.
streak_speed = (d.xend - d.xbeg)/total_streak_age
dvar = d.xbeg + streak_age_now * streak_speed

;plot,[d.xbeg,d.xend],[juldate1,juldate2],ystyle=1,/xstyle,ytickunits=['Day','Month'],/ynozero,/nodata

lon0 = -105.0 & nlon = 6 & dt = 4 & dlon = 5.
yes = indgen(nlon,24/dt) & all = yes

print, format = '("   ",$)' & print, abs(lon0+indgen(nlon)*dlon), format = '('+strtrim(nlon,2)+'i3)' 
for t = 0,24-dt,dt do begin
print, t, format='(i2," ", $)'
for x = lon0,lon0+(nlon-1)*dlon,dlon do begin
	subset = where(t eq dt*((round(tvar/dt)+24/dt)mod (24/dt)) and dvar ge x-(dlon/2.) and dvar lt x+(dlon/2.) and total_streak_age ge very_old_streak)
	xind = long((x-lon0)/dlon) & tind = long(t/dt)
	if subset[0] eq -1 then yes[xind,tind] = 0. else yes[xind,tind] = n_elements(subset)
	print, yes[xind,tind], format='(i3,$)'
endfor
print
endfor
print

for t = 0,24-dt,dt do begin
print, t, format='(i2," ", $)'
for x = lon0,lon0+(nlon-1)*dlon,dlon do begin
	subset = where(t eq dt*((round(tvar/dt)+24/dt)mod (24/dt)) and $
		dvar ge x-(dlon/2.) and dvar lt x+(dlon/2.) and $
		((total_streak_age ge very_old_streak and d.xend ge max(d.xend)) or $
		(total_streak_age ge streak_age_now and d.xend lt max(d.xend)) ))
	xind = long((x-lon0)/dlon) & tind = long(t/dt)
	if subset[0] eq -1 then all[xind,tind] = 0. else all[xind,tind] = n_elements(subset)
	print, all[xind,tind], format='(i3,$)'
endfor
print
endfor
print




c=round(100.*yes/all)
if (min(c) lt 0) then c[where(c lt 0)] = 0
all = [[all],[all]]
yes = [[yes],[yes]]
c = [[c],[c]]
print, c
tvar = [tvar,tvar+24.]
dvar = [dvar,dvar]


pos = [0.1,0.2,0.9,0.9]

dscale = round(!D.X_SIZE * (pos[2]-pos[0])/nlon)
tscale = round(!D.Y_SIZE * (pos[3]-pos[1])/48*dt)
; If you don't care about x-window rendering, and for quicker postscript plots, keep dscale and tscale small.
dscale = 3 & tscale = 3
tv, rebin(1.8*c+90,nlon*dscale,48/dt*tscale,/sample),pos[0],pos[1],xsize=pos[2]-pos[0],ysize=pos[3]-pos[1],/order,/normal,/nan
plot,[lon0-dlon/2.,lon0+(nlon-1)*dlon+dlon/2.],-[0.-dt/2.,48.-dt/2.],xtickformat='ticks',/xstyle,/ystyle,ytickformat='mod24',/nodata, position=pos,/noerase,title="probability a "+strtrim(streak_age_now,2)+"h-old streak will last "+strtrim(very_old_streak,2)+"h",xtitle="current longitude", ytitle="current time UTC", ytickinterval=dt/2
xyouts,rebin(lon0+findgen(nlon)*dlon,nlon,48/dt),rebin(transpose(-findgen(48/dt)*dt),nlon,48/dt),strtrim(yes,2)+"/"+strtrim(all,2)+"="+strtrim(c,2)+"%",alignment=0.5

tvar = -tvar 
ind = where(tvar le -48.+dt/2.)
tvar[ind] = tvar[ind] + 48.



; psym = 3 period	less than streak_age_now
; psym = 5 triangle	very_old_streak
; psym = 7 asterisk	at least streak_age_now hours old, but ends before very_old_streak
double_tlen = [total_streak_age,total_streak_age]
double_dxend = [d.xend,d.xend]
none_ind = where(double_tlen lt streak_age_now or (double_tlen lt very_old_streak and double_dxend ge max(d.xend)))
no_ind = where(double_tlen ge streak_age_now and double_tlen lt very_old_streak and double_dxend lt max(d.xend)) 
yes_ind = where(double_tlen ge very_old_streak) 
;if none_ind[0] ne -1 then plots, dvar[none_ind],tvar[none_ind],psym=3,noclip=0
;plots, dvar[no_ind],tvar[no_ind],psym=7,symsize=0.25,noclip=0
;plots, dvar[yes_ind],tvar[yes_ind],psym=5,symsize=0.3,noclip=0

; Draw an arrow from the start coordinate to the end coordinate of each streak.
;arrow, dvar[yes_ind],tvar[yes_ind],double_dxend[yes_ind],tvar[yes_ind]-double_tlen[yes_ind],/data


;mycolorbar,position=[0.25,0.05,0.75,0.1],range=[0,100],format='(i3)',title='%'
; print the parameters of the program used to create the streak stats.
t = read_ascii('plot_hov_cor.inp20031115',data_start=20,header=header)
;xyouts, 0.02,0.18,strjoin(header,'!C', /SINGLE), /normal,charsize=0.3



z = double_tlen
z[*] = 0
z[yes_ind] = 1
all_ind = where(double_tlen ge streak_age_now and (double_tlen ge very_old_streak or double_dxend lt max(d.xend)))
dvar = dvar[all_ind]
tvar = tvar[all_ind]
z    = z[all_ind]

end ;program r

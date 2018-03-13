!P.MULTI=[0,2,5]

t = { VERSION   : 1.00000,   $
      DATASTART : 0L,   $
      DELIMITER : 32B,       $
      MISSINGVALUE: !VALUES.F_NAN,$
      COMMENTSYMBOL:  '',   $
      FIELDCOUNT: 13L,  $
      FIELDTYPES:    [7,3,3,3,4,4,3,4,4,4,4,4,4] ,  $
      FIELDNAMES:   ['field01','nlines','ipts','d1','h1','x1','d2','h2','x2','span','dur','spd1','spd2'],$
      FIELDLOCATIONS: long([1,14,19,23,33,40,48,58,65,73,82,89,96]),$
      FIELDGROUPS:  indgen(13) }

d1996=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.1996',template=t)
d1997=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.1997',template=t)
d1998=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.1998',template=t)
d1999=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.1999',template=t)
d2000=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2000',template=t)
d2001=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2001',template=t)
d2002=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2002',template=t)
d2003=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2003',template=t)
d2004=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2004',template=t)
d2002skew=read_ascii('/data/data1/pd/ahijevyc/plot_hov_dir/streak_stats/0620-0831/fort.22.2002skew',template=t)


b=0.2*10
x1=-121 & x2=-75
bins = x1 + b*indgen((x2-x1)/b) + b/2

ge_thresh = 6  

data=d1996
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='1996'
print,bins,h

data=d1997
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='1997'
print,bins,h

data=d1998
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='1998'
print,bins,h

data=d1999
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='1999'
print,bins,h

data=d2000
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='2000'
print,bins,h

data=d2001
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='2001'
print,bins,h

data=d2002
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='2002'
print,bins,h

data=d2003
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='2003'
print,bins,h

all_x1  = [d1996.x1,d1997.x1,d1998.x1,d1999.x1,d2000.x1,d2001.x1,d2003.x1]
all_dur = [d1996.dur,d1997.dur,d1998.dur,d1999.dur,d2000.dur,d2001.dur,d2003.dur]
data = all_x1[where(all_dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='1996-2001,2003'
print,bins,h

data=d2002skew
data=data.x1[where(data.dur ge ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
plot,bins,h,xtickformat='ticks',xgridstyle=1,xticklen=1,xtickinterval=5,xstyle=1,psym=10,subtitle='>= '+strtrim(ge_thresh,2),ytitle='count',title='2002skew'
print,bins,h

;Now plot a cumulative histogram of duration.
b=6.
x1=ge_thresh & x2=60.
bins = x1 + b*indgen((x2-x1)/b) + b/2
data = all_dur[where(all_dur gt ge_thresh)]
h = histogram(data,min=x1,max=x2,binsize=b)
p = h & for i=0,n_elements(p)-1 do p[i]=total(h[i:*]) & p=100.*p/N_ELEMENTS(data)
plot,bins,p,xgridstyle=1,xticklen=1,xtickinterval=b,xstyle=1,psym=10,subtitle='',ytitle='count',title='',/ylog,yrange=[0.1,100]

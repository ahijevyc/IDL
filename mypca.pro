; I copied this from read_meanraw.pro on 20030226
; ahijevyc
pro mymap, limits, pos
	MAP_SET, limit=limits,/usa,/continents,/noerase,position=pos,/noborder
	MAP_GRID,/box
end ; pro mymap


pro mypca, ifile


	; reads the raw unformatted ouput of ~tuttle/uswrp/hovm/linux/plot_hov
	; 

	filename = ifile

	junk = 0L & grid = 'grid' & nx=0L & ny = 0L

	openr, filename,filename,/get_lun

	readu,filename,junk
	readu,filename,grid
	if grid ne 'LOLA' then print, 'not a LOLA grid'
	readu,filename,nx
	readu,filename,ny
	readu,filename,junk & readu,filename,junk
	x_c = findgen(nx)
	y_c = findgen(ny)
	readu,filename,x_c
	readu,filename,junk & readu,filename,junk
	readu,filename,y_c
	readu,filename,junk & readu,filename,junk
	data = findgen(nx,ny)
	readu,filename,data
	readu,filename,junk
	print,grid,nx,ny
	close,filename


	if !D.NAME EQ 'PS' then DEVICE, /COLOR
	;loadct,20; nice, light rainbow
	LOADCT,42,file="/users/ahijevyc/IDL/resource/colors/colors1.tbl" ; better rainbow

	thresh = '35'
	thresh_units = 'dBZ'
	field_desc = "% time with >= x native pts within 0.2deg grid box >= " + thresh + thresh_units
	date_range = ' '
	dataset = "WSI NOWrad (2km,15min,5dBZ) remapped to .2x.2deg "+grid+" grid "
	subtitle = dataset + date_range 

	pos=[0.05,0.2,0.8,0.75]
	barpos = [pos[0],0.03,pos[2],0.04]
	print, pos



	latmin = y_c[0] & lonmin = x_c[0] & latmax = y_c[ny-1] & lonmax = x_c[nx-1]
	print, latmin,lonmin,latmax,lonmax
	limits = [latmin,lonmin,latmax,lonmax]
	if grid eq 'LOTI' then BEGIN
		data = [[rotate(data,7)],[rotate(data,7)]]
		ny = ny*2
	ENDIF

	;levels = 0.5+findgen(23)/2
	levels = min(data)+(max(data)-min(data))*findgen(23)/23 
	colors = mycolors(levels)
	contour, data,xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill,subtitle=subtitle,position=pos
	mymap,limits,pos
	contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc

	; Make mean profiles.
	mean1 = rebin(data,nx)
	mean2 = rebin(data,[1,ny])
	plot, mean1,position=[pos[0],pos[3]+0.02,!X.WINDOW[1],0.9],xstyle=5,ystyle=1,/noerase,charsize=0.5,ytitle=''
	plot, mean2,findgen(ny),position=[!X.WINDOW[1]+0.05,pos[1],0.95,pos[3]],xstyle=1,ystyle=5,/noerase,charsize=0.5,xtitle=''


	old_data = data & data = mean1#mean2/mean(data)
	contour, data,xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill,subtitle=subtitle,position=pos
	mymap,limits,pos
	contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc
	mean1 = rebin(data,nx)
	mean2 = rebin(data,[1,ny])
	plot, mean1,position=[pos[0],pos[3]+0.02,!X.WINDOW[1],0.9],xstyle=5,ystyle=1,/noerase,charsize=0.5,ytitle=''
	plot, mean2,findgen(ny),position=[!X.WINDOW[1]+0.05,pos[1],0.95,pos[3]],xstyle=1,ystyle=5,/noerase,charsize=0.5,xtitle=''


	data = old_data - data
	levels = min(data)+(max(data)-min(data))*findgen(23)/23 
	colors = mycolors(levels)
	contour, data,xstyle=5,ystyle=5,c_colors=colors,levels=levels,/fill,subtitle=subtitle,position=pos
	mymap,limits,pos
	contourbar,levels,colors,position=barpos,format='(F4.1)',title=field_desc
	mean1 = rebin(data,nx)
	mean2 = rebin(data,[1,ny])
	plot, mean1,position=[pos[0],pos[3]+0.02,!X.WINDOW[1],0.9],xstyle=5,ystyle=1,/noerase,charsize=0.5,ytitle=''
	plot, mean2,findgen(ny),position=[!X.WINDOW[1]+0.05,pos[1],0.95,pos[3]],xstyle=1,ystyle=5,/noerase,charsize=0.5,xtitle=''


	if !D.NAME EQ 'PS' then DEVICE, /CLOSE
	!P.MULTI = 0
	message, 'stopping to preserve variables'
end


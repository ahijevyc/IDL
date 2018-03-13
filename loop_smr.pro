pro loop
window, xsize = 600, ysize = 400
for jday = 200, 221 do begin

	for h = 0,23 do begin

		for m = 0, 45, 15 do begin
			ifile= string('U',h,m,'Z98.',jday,'_refl_2km.hdf',format='(A,I2.2,I2.2,A,I3,A)')
			spawn, 'jday '+string(jday)+ ' 1998', date
			if (file_test(ifile)) then begin
				;ofile= string(date,h,m,'a.png',format='(A8,I2.2,I2.2,A)')
				;smearhdf, ifile, ofile, 0, 0, 0, 0
				;ofile= string(date,h,m,'b.png',format='(A8,I2.2,I2.2,A)')
				;smearhdf, ifile, ofile, 240, 480, 300., 1.4
				;ofile= string(date,h,m,'c.png',format='(A8,I2.2,I2.2,A)')
				;smearhdf, ifile, ofile, 240, 480, 0, 0 
				ofile= string(date,h,m,'d.png',format='(A8,I2.2,I2.2,A)')
				smearhdf, ifile, ofile, 300, 300, 0, 0 
				print, ifile
			endif else print, 'No ', ifile  
		endfor
	endfor
endfor

end

PRO REASONABLE, s, year, warnings

   indices = WHERE (FLOAT(s) GT 4.2,count) ; above 4.2, no legitimate rainfall data is cut out
   IF (count GT 0) THEN BEGIN
      PRINT
      PRINT, 'WARNING: very high rain rate average(s) ', s[indices]
      FOR i = 0,count-1 DO BEGIN
         warnings=warnings+1
         format='(A,$)'
         PRINTF, -1, DIGIT2MONTH(INDEX2MONTH(indices[i])), FORMAT=format
         PRINTF, -1, ' ' + INDEX2DAY(indices[i]) + ', ', FORMAT=format
      ENDFOR
      PRINT, STRTRIM(STRING(year),1)
      PRINT, 'Changing to -999. (i.e. missing data)'
      s[indices] = -999.
         
   ENDIF

   RETURN
END


PRO HV1, input, output, most_recent_year, warnings

   warnings  = 0
   n_years   = 0
   n_years   = SIZE(input,/N_ELEMENTS)/153
   sum_s     = FLTARR(153)
   n         = BYTARR(153)
   most_recent_year = 1996

   FOR year_index = 0,n_years-1 DO BEGIN

      s = input[153*year_index:153*year_index+152]
      year = year_index + 1996
      REASONABLE, s, year, warnings


      ; Use the IDL maximum operator > to make all the negative
      ; bad values equal to zero.

      sum_s = TEMPORARY(sum_s) + (s > 0)

      ; Use the threshold operator GE to keep track of the 
      ; number of non-negative elements.

      n = TEMPORARY(n) + (s GE 0)
      IF TOTAL(s GE 0) GT 0 THEN most_recent_year = year
   ENDFOR


   present = WHERE(n GT 0B, count)
   float_n = FLOAT(n) ; Need to be floating point array
                      ; since we need to take its inverse.

   IF (count NE 0) THEN float_n[present] = 1. / float_n[present]
   output = sum_s*float_n


   RETURN
END


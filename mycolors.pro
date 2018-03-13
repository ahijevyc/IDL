FUNCTION mycolors, levels
	; If more levels than colors, !D.N_COLORS/N_ELEMENTS(levels) is zero in LONG division.
;  RETURN, 1L+LONG(FLOAT(!D.N_COLORS)/N_ELEMENTS(levels)*INDGEN(N_ELEMENTS(levels)))
; switched from n_colors to table_size - daa 20111103
	RETURN, 1L+LONG(FLOAT(!D.TABLE_SIZE)/N_ELEMENTS(levels)*INDGEN(N_ELEMENTS(levels)))
	; I added 1L on 2007-02-05.  I didn't like how it was always the same color as the foreground (black on PostScript)
END; FUNCTION mycolors

FUNCTION TICKS, axis, index, value
	label = axis EQ 0 ? value GT 0 ? 'E' : 'W' $
			  : value GT 0 ? 'N' : 'S'
	RETURN, STRTRIM(ROUND(ABS(value)),2) + ' ' + label
END ; FUNCTION TICKS



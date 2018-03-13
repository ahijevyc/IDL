FUNCTION mod24, axis, index, value
	pvalue = ABS(value) MOD 24.0d
        return, STRING(pvalue, FORMAT='(I02)')
;I commented this out and replaced it on 20040103 - Dave Ahijevych
;	RETURN, STRTRIM(ROUND(ABS(value)) mod 24)
END ; FUNCTION mod24



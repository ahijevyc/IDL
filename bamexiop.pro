function BamexIOP, stid

	; For given 8-letter station ids (as used in GEMPAK), return the IOP strings.

	n = n_elements(stid)
	IOPs = strarr(n)
	for i = 0L, n-1 do begin
		; For dropsondes & rawinsondes, stid is a capital letter followed by julian day and hhmm.
		ijday = 1
		; For wind profiles, stid is first 3 capital letters of original station id followed by jday and hh.
		if strmatch(stid[i],'[A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9][0-9]') then ijday = 3
		jday = strmid(stid[i],ijday,3)
		hh = strmid(stid[i],ijday+3,2)
		case jday of
			'144': IOP = 'IOP01'
			'145': IOP = 'IOP01'
			'148': IOP = 'IOP02'
			'149': IOP = 'IOP02'
			'153': IOP = 'IOP04a'
			'154': IOP = 'IOP04b'
			'156': IOP = 'IOP05b'
			'157': IOP = 'IOP05b'
			'159': IOP = 'IOP06'
			'161': IOP = hh lt 15 ? 'IOP07A' : 'IOP07B'
			'162': IOP = 'IOP08'
			'172': IOP = hh lt 20 ? 'IOP09' : 'IOP10'
			'173': IOP = 'IOP10'
			'175': IOP = 'IOP12'
			'177': IOP = 'IOP13'
			'180': IOP = 'IOP15'
			'181': IOP = 'IOP15'
			'185': IOP = 'IOP17b'
			'186': IOP = 'IOP17b'
			'187': IOP = 'IOP18'
		else: message, 'IOP not defined for ' + stid[i]
		endcase
		IOPs[i] = IOP
	endfor
	return, IOPs
end

function mtp_bad_cases, date, xrange

  MMdd = strmid(date,4,4)
  ; a nan means it is unbounded. like for '0817', the bad stuff is after 40.0 till the end of the flight. For '0821' the whole flight is bad.
  case MMdd of
    '0817': ks = [[40.0, xrange[1]]]
    '0821': ks = [xrange]
    '0823': ks = [xrange]
    '0901': ks = [[54.7,57.3]]
    '0913': ks = [[41.9,46.8]]
    '0914': ks = [[54.8,56.7],[59.3,61.5],[63.2,64.8]]
    '0924': ks = [[54.3,55.9]]
    '0927': ks = [[46.2,47.4],[51.5,52.3],[55.7,56.8]]
    '0928': ks = [[46.9,47.3],[49.9,50.5],[50.9,51.5],[57.4,58.5],[62.3,62.9]]
    '0930': ks = [[52.1,53.0]]
  else  : ks = -1
endcase

; these are all in ks . put into julian days
hour = ks * 1000. / 3600.
bad_intervals = julday(strmid(date,4,2),strmid(date,6,2),strmid(date,0,4),hour, 0, 0)
return, bad_intervals

end
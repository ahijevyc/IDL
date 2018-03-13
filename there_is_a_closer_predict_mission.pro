function there_is_a_closer_PREDICT_mission, dropJulian, get_julday
  ; get_julday is the mission time/hour
  current_separation = abs(dropJulian-get_julday)
  t = get_PREDICT_missions(count=nmissions)
  other_mission_julday = julday(strmid(t.yyyymmdd,4,2), strmid(t.yyyymmdd,6,2), strmid(t.yyyymmdd,0,4), strmid(t.hhmm,0,2), strmid(t.hhmm,2,2), 0)
  if min(abs(dropJulian-other_mission_julday)) lt current_separation then return, 1 else return, 0

end
function tropical_reference_time, pgi, type
  ; type=1 means tropical depression
  if type eq 1 then begin
    case pgi of
      'PGI07L'   : td_time = julday(06,25,2010,22,0,0) ; TROPICAL DEPRESSION
      'ALEX'     : td_time = julday(06,25,2010,22,0,0) ; TROPICAL DEPRESSION
      'TWO'      : td_time = julday(07,08,2010,03,0,0) ; TROPICAL DEPRESSION
      'PGI17L'   : td_time = julday(07,22,2010,15,0,0) ; TROPICAL DEPRESSION
      'BONNIE'   : td_time = julday(07,22,2010,15,0,0) ; TROPICAL DEPRESSION
      'PGI22L'   : td_time = julday(08,02,2010,15,0,0) ; TROPICAL DEPRESSION
      'COLIN'    : td_time = julday(08,02,2010,15,0,0) ; TROPICAL DEPRESSION
      'PGI29L'   : td_time = julday(08,10,2010,23,0,0) ; TROPICAL DEPRESSION
      'FIVE'     : td_time = julday(08,10,2010,23,0,0) ; TROPICAL DEPRESSION
      'PGI31L'   : td_time = julday(08,21,2010,21,0,0) ; TROPICAL DEPRESSION
      'DANIELLE' : td_time = julday(08,21,2010,21,0,0) ; TROPICAL DEPRESSION
      'PGI34L'   : td_time = julday(08,25,2010,06,0,0) ; TD see README in best track directory.
      'EARL'     : td_time = julday(08,25,2010,06,0,0) ; TD see README in best track directory.
      'PGI36L'   : td_time = julday(08,30,2010,12,0,0) ; TD from nhc pdf downloaded 20111206
      'FIONA'    : td_time = julday(08,30,2010,12,0,0) ; TD from nhc pdf downloaded 20111206
      'PGI38L'   : td_time = julday(09,01,2010,15,0,0) ; TROPICAL DEPRESSION
      'GASTON'   : td_time = julday(09,01,2010,15,0,0) ; TROPICAL DEPRESSION
      'HERMINE'  : td_time = julday(09,06,2010,03,0,0) ; TROPICAL DEPRESSION
      'PGI41L'   : td_time = julday(09,08,2010,06,0,0) ; TROPICAL DEPRESSION
      'IGOR'     : td_time = julday(09,08,2010,06,0,0) ; TROPICAL DEPRESSION
      'PGI43L'   : td_time = julday(09,12,2010,15,0,0) ; TROPICAL DEPRESSION
      'JULIA'    : td_time = julday(09,12,2010,15,0,0) ; TROPICAL DEPRESSION
      'PGI44L'   : td_time = julday(09,14,2010,12,0,0) ; TD - http://www.nhc.noaa.gov/pdf/TCR-AL132010_Karl.pdf downloaded 20111206
      'KARL'     : td_time = julday(09,14,2010,12,0,0) ; TD - http://www.nhc.noaa.gov/pdf/TCR-AL132010_Karl.pdf downloaded 20111206
      'PGI45L'   : td_time = julday(09,21,2010,03,0,0) ; TROPICAL DEPRESSION
      'LISA'     : td_time = julday(09,21,2010,03,0,0) ; TROPICAL DEPRESSION
      'PGI46L'   : td_time = julday(09,23,2010,18,0,0) ; TROPICAL DEPRESSION
      'MATTHEW'  : td_time = julday(09,23,2010,18,0,0) ; TROPICAL DEPRESSION
      'PGI50L'   : td_time = julday(09,28,2010,15,0,0) ; TROPICAL DEPRESSION
      'NICOLE'   : td_time = julday(09,28,2010,15,0,0) ; TROPICAL DEPRESSION
      'PGI48L'   : td_time = julday(10,06,2010,09,0,0) ; SUBTROPICAL DEPRESSION - PGI48 and PGI49 merge before Otto
      'PGI49L'   : td_time = julday(10,06,2010,09,0,0) ; SUBTROPICAL DEPRESSION - PGI48 and PGI49 merge before Otto
      'OTTO'     : td_time = julday(10,06,2010,09,0,0) ; SUBTROPICAL DEPRESSION
      'PGI55L'   : td_time = julday(10,11,2010,00,0,0) ; TD http://www.nhc.noaa.gov/pdf/TCR-AL182010_Paula.pdf downloaded 20111206
      'PAULA'    : td_time = julday(10,11,2010,00,0,0) ; TD http://www.nhc.noaa.gov/pdf/TCR-AL182010_Paula.pdf downloaded 20111206
      'PGI60L'   : td_time = julday(10,21,2010,03,0,0) ; TROPICAL DEPRESSION
      'RICHARD'  : td_time = julday(10,21,2010,03,0,0) ; TROPICAL DEPRESSION
      'PGI66L'   : td_time = julday(10,29,2010,03,0,0) ; TROPICAL STORM
      'SHARY'    : td_time = julday(10,29,2010,03,0,0) ; TROPICAL STORM
      'PGI65L'   : td_time = julday(10,29,2010,06,0,0) ; TD - http://www.nhc.noaa.gov/pdf/TCR-AL212010_Tomas.pdf downloaded 20111206
      'TOMAS'    : td_time = julday(10,29,2010,06,0,0) ; TD - http://www.nhc.noaa.gov/pdf/TCR-AL212010_Tomas.pdf downloaded 20111206
      else       : td_time = !VALUES.D_NAN
    endcase
    return, td_time
  endif
  ; type=2 means tropical storm
  if type eq 2 then begin
    case pgi of
      'PGI07L'   : td_time = julday(06,26,2010,09,0,0)
      'ALEX'     : td_time = julday(06,26,2010,09,0,0)
      'PGI17L'   : td_time = julday(07,23,2010,00,0,0)
      'BONNIE'   : td_time = julday(07,23,2010,00,0,0)
      'PGI22L'   : td_time = julday(08,03,2010,09,0,0)
      'COLIN'    : td_time = julday(08,03,2010,09,0,0)
      'PGI31L'   : td_time = julday(08,22,2010,21,0,0)
      'DANIELLE' : td_time = julday(08,22,2010,21,0,0)
      'PGI34L'   : td_time = julday(08,25,2010,12,0,0) ; see README in best track directory.
      'EARL'     : td_time = julday(08,25,2010,12,0,0) ; see README in best track directory.
      'PGI36L'   : td_time = julday(08,30,2010,18,0,0) ; TS from NHC pdf downloaded 20111206
      'FIONA'    : td_time = julday(08,30,2010,18,0,0)
      'PGI38L'   : td_time = julday(09,01,2010,21,0,0)
      'GASTON'   : td_time = julday(09,01,2010,21,0,0)
      'HERMINE'  : td_time = julday(09,06,2010,09,0,0)
      'PGI41L'   : td_time = julday(09,08,2010,12,0,0) ; TS from NHC pdf downloaded 20111206
      'IGOR'     : td_time = julday(09,08,2010,12,0,0) ; TS from NHC pdf downloaded 20111206
      'PGI43L'   : td_time = julday(09,13,2010,03,0,0)
      'JULIA'    : td_time = julday(09,13,2010,03,0,0)
      'PGI44L'   : td_time = julday(09,14,2010,18,0,0) ; TS from http://www.nhc.noaa.gov/pdf/TCR-AL132010_Karl.pdf downloaded 20111206
      'KARL'     : td_time = julday(09,14,2010,18,0,0) ; TS from http://www.nhc.noaa.gov/pdf/TCR-AL132010_Karl.pdf downloaded 20111206
      'PGI45L'   : td_time = julday(09,21,2010,09,0,0)
      'LISA'     : td_time = julday(09,21,2010,09,0,0)
      'PGI46L'   : td_time = julday(09,23,2010,21,0,0)
      'MATTHEW'  : td_time = julday(09,23,2010,21,0,0)
      'PGI50L'   : td_time = julday(09,29,2010,15,0,0)
      'NICOLE'   : td_time = julday(09,29,2010,15,0,0)
      'PGI49L'   : td_time = julday(10,06,2010,21,0,0) ; subtropical storm
      'OTTO'     : td_time = julday(10,06,2010,21,0,0) ; subtropical storm
      'PGI55L'   : td_time = julday(10,11,2010,06,0,0) ; TS http://www.nhc.noaa.gov/pdf/TCR-AL182010_Paula.pdf downloaded 20111206
      'PAULA'    : td_time = julday(10,11,2010,06,0,0) ; TS http://www.nhc.noaa.gov/pdf/TCR-AL182010_Paula.pdf downloaded 20111206
      'PGI60L'   : td_time = julday(10,21,2010,15,0,0)
      'RICHARD'  : td_time = julday(10,21,2010,15,0,0)
      'PGI66L'   : td_time = julday(10,29,2010,03,0,0) ; TROPICAL STORM
      'SHARY'    : td_time = julday(10,29,2010,03,0,0) ; TROPICAL STORM
      'PGI65L'   : td_time = julday(10,29,2010,12,0,0) ; TS - http://www.nhc.noaa.gov/pdf/TCR-AL212010_Tomas.pdf downloaded 20111206
      'TOMAS'    : td_time = julday(10,29,2010,12,0,0) ; TS - http://www.nhc.noaa.gov/pdf/TCR-AL212010_Tomas.pdf downloaded 20111206
      else       : td_time = !VALUES.D_NAN
    endcase
    return, td_time
  endif
end
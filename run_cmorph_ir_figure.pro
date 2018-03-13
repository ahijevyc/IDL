pro run_cmorph_ir_figure



  dates = ['0916','0917','0918','0919','0920','0921','0922','0923','0924','0925']
  for idate=0,n_elements(dates)-1 do for ihour = 0, 0 do cmorph_ir_figure, 'PGI46L', ihour*3, mtm00=1, date=dates[idate] 


end
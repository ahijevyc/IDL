pro run_atm_CR_analysis

; uncomment for debugging (a single day)
;  atm_CR_analysis, model='CoSPA', suffix = '08050.50', plot_p_threshold=1

;  atm_CR_analysis, model='AFWA', suffix = 'thru08260.50', plot_p_threshold=1
;  for imember = 0,11 do atm_CR_analysis, model='AFWA', suffix = 'thru0826_mem'+string(imember+1,format='(I0)')+'_0.50'

  members = '_MEM' + ['8','7.5','6.5','6','5.5','5','4.5','4','3.5','2.5','2', '1.5','1','0.5']+'_'

  atm_CR_analysis, model='CoSPA', suffix = 'thru08260.50', plot_p_threshold = 1
;  for imember = 0,n_elements(members)-1 do atm_CR_analysis, model='CoSPA', suffix = 'thru0826'+members[imember]+'0.50'
end
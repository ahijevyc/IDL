pro run_mtm_hovm
  pgis = 'PGI' + ['38','44','46'] + 'L'
  
    for ipgi=0,n_elements(pgis)-1 do begin
;    mtm_hovm, pgis[ipgi], 'p75', /mtm00
    mtm_hovm, pgis[ipgi], '-60', /mtm00, /disk

    ; CMORPH
    percentiles = ['p50', 'p95']
    for iper = 0, n_elements(percentiles)-1 do begin
;      mtm_hovm, pgis[ipgi], percentiles[iper], /mtm00, /disk, type='CMORPH'
    endfor
    ; SST
    percentiles = ['p25','p50','p75','p90','pmax','avg', '28']
    for iper = 0, n_elements(percentiles)-1 do begin
;          mtm_hovm, pgis[ipgi], percentiles[iper], /mtm00, /disk, type='SST'
    endfor
    
  endfor
end
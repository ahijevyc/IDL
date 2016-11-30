function GFS_equiv, name, model=model
  if strmatch(model.name, 'mp*') then return, name
  if strmatch(model.name, 'al') then return, name
  if strmatch(model.name, 'wp') then return, name
  if strmatch(model.name, 'ep') then return, name
  if strmid(name,0,11) eq 'temperature' then return, 'TMP_P0_L100_GLL0'
  if strmid(name,0,6) eq 'height' then return, 'HGT_P0_L100_GLL0'
  if name eq 'precipw' then return, 'PWAT_P0_L200_GLL0'
  if name eq 'q2' then return, 'MIXR_P0_L103_GLL0' ; used to be 'SPFH_P0_L103_GLL0' but incorrect (create MIXR by hand with run_fcst-init.sh)
  if name eq 'skintemp' then return, 'TMP_P0_L1_GLL0'
  if strmid(name, 0, 2) eq 't2' then return, 'TMP_P0_L103_GLL0'
  if name eq 'u10' then return, 'UGRD_P0_L103_GLL0'
  if name eq 'v10' then return, 'VGRD_P0_L103_GLL0'
  if strmid(name,0, 6) eq 'relhum' then return, 'RH_P0_L100_GLL0'
  if strmid(name,0,11) eq 'umeridional' then return, 'VGRD_P0_L100_GLL0'
  if strmid(name,0, 6) eq 'uzonal' then return, 'UGRD_P0_L100_GLL0'
  return, name
end


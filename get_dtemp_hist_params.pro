function get_dtemp_hist_params, dtemp_alt, units=units
  if ~keyword_set(units) then units = 'm'
  tbin = 0.5
  if units eq 'm' then begin
    case dtemp_alt of
      20000. : tloc = findgen(45)*tbin-107
      19500. : tloc = findgen(45)*tbin-103
      19000. : tloc = findgen(50)*tbin-105
      18500. : tloc = findgen(45)*tbin-106
      18000. : tloc = findgen(45)*tbin-112
      17500. : tloc = findgen(50)*tbin-116
      17000. : tloc = findgen(50)*tbin-115
      16500. : tloc = findgen(50)*tbin-117
      16000. : tloc = findgen(50)*tbin-118
      15500. : tloc = findgen(55)*tbin-114
      15000. : tloc = findgen(55)*tbin-110
      14500. : tloc = findgen(60)*tbin-107
      14000. : tloc = findgen(60)*tbin-104
      13500. : tloc = findgen(60)*tbin-102
      13000. : tloc = findgen(60)*tbin-98
      12750. : tloc = findgen(60)*tbin-96
      12500. : tloc = findgen(62)*tbin-93
      12250. : tloc = findgen(62)*tbin-93
      12000. : tloc = findgen(60)*tbin-92
      11750. : tloc = findgen(60)*tbin-90
      11500. : tloc = findgen(60)*tbin-85
      11250. : tloc = findgen(60)*tbin-84
      11000. : tloc = findgen(60)*tbin-82
      10750. : tloc = findgen(60)*tbin-80
      10500. : tloc = findgen(60)*tbin-78      
      10250. : tloc = findgen(60)*tbin-76
      10000. : tloc = findgen(55)*tbin-75
      9750.  : tloc = findgen(55)*tbin-74
      9500.  : tloc = findgen(55)*tbin-72
      9250.  : tloc = findgen(55)*tbin-70
      9000.  : tloc = findgen(55)*tbin-68
      8750.  : tloc = findgen(55)*tbin-66
      8500.  : tloc = findgen(55)*tbin-64
      8250.  : tloc = findgen(55)*tbin-62
      8000.  : tloc = findgen(55)*tbin-60
      7750.  : tloc = findgen(55)*tbin-56
      7500.  : tloc = findgen(42)*tbin-54
      7250.  : tloc = findgen(42)*tbin-51
      7000.  : tloc = findgen(42)*tbin-48
      6750.  : tloc = findgen(42)*tbin-47
      6500   : tloc = findgen(38)*tbin-45
      6250   : tloc = findgen(38)*tbin-44
      6000   : tloc = findgen(38)*tbin-42
      5750   : tloc = findgen(38)*tbin-41
      5500   : tloc = findgen(38)*tbin-39
      5250   : tloc = findgen(38)*tbin-38
      5000   : tloc = findgen(38)*tbin-36.5
      4750   : tloc = findgen(38)*tbin-36.
      4500   : tloc = findgen(39)*tbin-34.
      4250   : tloc = findgen(39)*tbin-33.
      4000   : tloc = findgen(39)*tbin-32.
      3750   : tloc = findgen(40)*tbin-32.
      3500   : tloc = findgen(42)*tbin-30
      3250   : tloc = findgen(42)*tbin-29
      3000   : tloc = findgen(42)*tbin-28
      2750   : tloc = findgen(40)*tbin-27
      2500   : tloc = findgen(35)*tbin-25
      2250   : tloc = findgen(35)*tbin-24
      2000   : tloc = findgen(35)*tbin-22
      1500   : tloc = findgen(30)*tbin-19
      1750   : tloc = findgen(30)*tbin-19
      1250   : tloc = findgen(30)*tbin-18
      1000   : tloc = findgen(30)*tbin-17
       750   : tloc = findgen(30)*tbin-16
       500   : tloc = findgen(30)*tbin-14.5
       250   : tloc = findgen(45)*tbin-18
        10   : tloc = findgen(45)*tbin-15
         0   : tloc = findgen(22)*tbin-7
      else: stop
    endcase
    ; I used to subtract the avg SST but I don't anymore, so the old tloc values need to be increased by the usual amount of the SST.
    tloc = tloc+29.
  endif
  
  ; if we are in Pressure coordinates just center around zero because we're basing off GFS reference sounding.
  ; not around zero anymore because I don't subtract the GFS anymore. - 20120201
  if units eq 'Pa' or units eq 'mb' then begin
    case dtemp_alt of
      200.  : tloc = findgen(40)*tbin-58
      250.  : tloc = findgen(60)*tbin-45
      300.  : tloc = findgen(70)*tbin-51
      400.  : tloc = findgen(66)*tbin-33
      500   : tloc = findgen(60)*tbin-20
      600   : tloc = findgen(60)*tbin-12.
      700   : tloc = findgen(52)*tbin
      800   : tloc = findgen(52)*tbin+7
      850   : tloc = findgen(55)*tbin+11
      925   : tloc = findgen(55)*tbin+16
      975   : tloc = findgen(50)*tbin+20
      1000  : tloc = findgen(45)*tbin+22
      else: tloc = findgen(200)*tbin - 60
    endcase
    
  endif
  
  
  return_structure = {tloc : tloc, tbin : tbin, low_cloud_color : 78, mid_cloud_color : 201, high_cloud_color : 253}
  
  return, return_structure
  
  
end
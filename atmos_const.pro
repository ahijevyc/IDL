; $Id: atmos_const.pro,v 1.10 2001/08/09 Dominik Brunner $

pro atmos_const
   
    ; create a system variable with physical constants for atmospheric science

   defsysv,'!ATMOS',exists=i

   if (i eq 1) then return    ; already defined
   
    ; define sample structure 

   defsysv,'!ATMOS', $
      { R: 8.3145D ,R_desc  : 'Universal gas constant (J deg-1 kmol-1)',$
        k: 1.381D-23,k_desc : 'Botzmann constant (J deg-1 molecule-1)',$
        sigma: 5.6696D-8,sigma_desc:'Stefan-Boltzmann constant (W m-2 deg-4)',$
        NA: 6.022D26 ,NA_desc: "Avogadro's number",$
        g: 9.80665D ,g_desc : 'Acceleration due to gravity (m s-2)',$
        RE: 6.375D6 ,RE_desc: "Average earth's radius (m)",$
        Omega: 7.292D-5,Omega_desc:'Anglular velocity of earth rotation (rad s-1)',$
        Md: 28.966,Md_desc  : 'Molecular weight of dry air',$
        Rd: 287.04,Rd_desc  : 'Gas constant of dry air (=R*1000/Md)',$
        rho: 1.275,rho_desc : 'Air density at 0degC and 1000 mb (kg m-3, varies as p/T)',$  
        cpd: 1004.67,cpd_desc : 'Specific heat of dry air at const. p (J deg-1 kg-1)',$
        cvd: 717.63,cvd_desc: 'Specific heat of dry air at const. volume (J deg-1 kg-1)',$
        Mw: 18.016,Mw_desc  : 'Molecular weight of water',$
        Rv: 461.40, Rv_desc : 'Gas constant for water vapor (J deg-1 kg-1)', $
        cpv: 1865.1,cpv_desc: 'Specific heat of water vapor at const. p (J deg-1 kg-1)',$
        cvv: 1403.2,cvv_desc: 'Specific heat of water vapor at const. volume (J deg-1 kg-1)',$
; added these after here from Emanuel 1994 - Ahijevych
        C2K: !CONST.T0,c2k_desc: 'add this to Celsius to get Kelvin',$
        LV0:2.501e+06,LV0_desc: 'latent heat of vaporization at 0 deg C',$
        CL:4190., CL_desc: 'Heat capacity of liquid water',$
        kts2mps:0.514444444, kts2mps_desc: 'meters per second per knot'$
        }
    
;  2014/07/02 David Ahijevych 
; modified 3 constants to match Emanuel 1994: Atmospheric Convection
	!ATMOS.Cpd = 1005.7 ; +/-2.5 J/kg/K
	!ATMOS.Cpv = 1870. ;  +/-25 J/kg/K
	!ATMOS.Rv  = 461.50d 

    print,'system variable !atmos created'
end

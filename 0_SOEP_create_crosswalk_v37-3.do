* Code adapted from: Disappointed Expectations: Downward Mobility and Electoral Change
* Adapted by: Jonne Kamphorst

* Create simplified crosswalk from ISCO-08 to ISEI-88 based on pooled SOEP data
* SOEPlongv37 (up to 2020). The data in the paper uses the SOEP wave V37. 


global soeplong "C:\large data\SOEP\SOEPv37"
global savedata "C:\large data\SOEP\SOEPv37\savedata"

clear

use "$soeplong/pl.dta" 


* add pgen data (time varying)

merge 1:1 syear pid using "$soeplong/pgen.dta"
tab _merge
drop if _merge==2
drop _merge


keep syear p_isco08 p_isco88 *isei*
drop if p_isco08<0
drop if p_isco88<0
drop pgisei08

gen one=1

bysort p_isco08 p_isco88: egen distinct=total(one)

drop syear

duplicates drop

sort p_isco08

bysort p_isco08 (distinct): keep if _n==_N

rename p_isco88 isco88_cw
rename pgisei88 isei88_cw

keep *isco* *isei*

save "$savedata/cw_isco08_isei88.dta", replace

rename p_isco08 fisco08
rename isco88_cw fisco88_cw
rename isei88_cw fisei88_cw

save "$savedata/cw_fisco08_fisei88.dta", replace

rename fisco08 misco08
rename fisco88_cw misco88_cw
rename fisei88_cw misei88_cw

save "$savedata/cw_misco08_misei88.dta", replace

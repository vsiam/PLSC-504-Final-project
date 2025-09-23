* Code adapted from: Disappointed Expectations: Downward Mobility and Electoral Change
* Adapted by: Jonne Kamphorst


* Create Auxiliary Long Data Set
* SOEPcore v35 (2019)
* auxiliary SOEP files: pgen, bioparen, hbrutto, biosoc, ppfad



global soeplong "C:\large data\SOEP\SOEPv37"
global output "C:\large data\SOEP\SOEPv37\output"
global savedata "C:\large data\SOEP\SOEPv37\savedata"






************************************************************************************
* soep long
************************************************************************************

clear

use "$soeplong/pl.dta" 

sort pid syear
tab syear

* add pgen data (time varying)

merge 1:1 syear pid using "$soeplong/pgen.dta"
tab _merge
*drop if _merge==2
drop _merge

* add bioparen data (time invariant)
merge m:1 pid using "$soeplong/bioparen.dta"
tab _merge
*drop if _merge!=3
drop _merge

* Merge with Bundesland from HBRUTTO
merge m:m syear hid using "$soeplong/hbrutto.dta", keepusing(bula_h)
*drop if _merge != 3 /* have household nr but no personal file */
*drop if pid==. /* have household nr but no personal file */
drop _merge
rename bula_h bula

* Merge with school experience in childhood from biosoc
merge m:m pid using "$soeplong/biosoc.dta", keepusing(bsschla bselkuem bsntdeut bsntmath bsntfmd1)
*drop if _merge == 2 
drop _merge

merge m:m pid using "$soeplong/biol.dta", keepusing(lb0190 lb0143_h lb0144_h lb0145_h) 
*drop if _merge == 2 
drop _merge

* Merge with birthregion and location in 1989 from ppath
merge m:m pid using "$soeplong/ppath.dta", keepusing(birthregion loc1989 locinfo migback)
*drop if _merge == 2 
drop _merge

* Generate Variables

gen ybirth = ple0010_h
replace ybirth=. if ple0010_h<0

gen age = syear-ybirth


gen bulaschool1 = lb0190
replace bulaschool1=. if lb0190<0
replace bulaschool1=. if lb0190==98 /* germany before 1949, no region declared */
* harmonize laender between variables

replace bulaschool1=1 if lb0190==16
replace bulaschool1=2 if lb0190==7
replace bulaschool1=3 if lb0190==10
replace bulaschool1=4 if lb0190==6
replace bulaschool1=5 if lb0190==11
replace bulaschool1=6 if lb0190==8
replace bulaschool1=7 if lb0190==12
replace bulaschool1=8 if lb0190==1
replace bulaschool1=9 if lb0190==2
replace bulaschool1=10 if lb0190==13
replace bulaschool1=11 if lb0190==3 | lb0190==4 | lb0190==18
replace bulaschool1=12 if lb0190==5
replace bulaschool1=13 if lb0190==9
replace bulaschool1=14 if lb0190==14
replace bulaschool1=15 if lb0190==15
replace bulaschool1=16 if lb0190==17

gen bulaschool2 = bsschla
replace bulaschool2 = 11 if bsschla==0 | bsschla==18
replace bulaschool2=. if bsschla<0 
replace bulaschool2=. if bsschla==98 /* germany before 1949, no region declared */
* harmonize laender between variables
replace bulaschool2 = 12 if bsschla==13
replace bulaschool2 = 13 if bsschla==12
replace bulaschool2 = 15 if bsschla==14
replace bulaschool2 = 16 if bsschla==15
replace bulaschool2 = 14 if bsschla==16

* time-constant, impute
bysort pid: egen bula_school1 = mean(bulaschool1)
bysort pid: egen bula_school2 = mean(bulaschool2)


* complement with bula when younger than 21
bysort pid (syear): gen bulafirst = bula if _n==1
replace bulafirst=. if age>21
* time-constant, impute
bysort pid: egen bula_first = mean(bulafirst)

tab bula_school1 bula_school2
tab bula_school1 bula_first

gen bula_child = bula_school1
replace bula_child = bula_school2 if bula_child==.
replace bula_child = bula_first if bula_child==.

replace birthregion=. if birthregion<0

gen bula_youth = bula_child
replace bula_youth = birthregion if bula_youth==.


* lm variables
gen empstat = pgemplst
replace empstat=. if pgemplst<0

gen nilf = empstat==5
replace nilf=. if empstat==.

gen isco88 = pgisco88
replace isco88=. if pgisco88<0

*mdesc isco88 if nilf!=1


gen isei88 = pgisei88
replace isei88=. if pgisei88<0

*mdesc isei88
*mdesc isei88 if nilf!=1

* 2018 largely moved to isco08. crosswalk to isei88 if missing.

*merge m:1 p_isco08 using "$savedata/cw_isco08_isei88.dta"
*tab syear if _merge==3

*tab _merge
*drop if _merge==2
*drop _merge

*gen isei88_full = isei88
*replace isei88_full = isei88_cw if isei88==.

* family background

* father

replace fisei88=. if fisei88<0

*mdesc fisei88
*mdesc fisei88 if nilf!=1

replace fisco88=. if fisco88<0

*merge m:1 fisco08 using "$savedata/cw_fisco08_fisei88.dta"
*tab syear if _merge==3

*tab _merge
*drop if _merge==2
*drop _merge

*gen fisei88_full = fisei88
*replace fisei88_full = fisei88_cw if fisei88==.

* mother
replace misei88=. if misei88<0

*mdesc misei88
*mdesc misei88 if nilf!=1

*merge m:1 misco08 using "$savedata/cw_misco08_misei88.dta"
*tab syear if _merge==3

*tab _merge
*drop if _merge==2
*drop _merge


gen misei88_full = misei88
*replace misei88_full = misei88_cw if misei88==.

* year of birth

replace fybirth=. if fybirth<0
replace mybirth=. if mybirth<0

gen fedu = fsedu
replace fedu=. if fsedu<0
replace fedu=. if fsedu==0 /* weiss nicht */
replace fedu=0 if fsedu==6 /* no degree */
replace fedu=. if fsedu==5 /* other, not defined */
replace fedu=. if fsedu>6 /* migrant subsample, degrees in other countries */
/*
1 = hauptschule
2 = realschule
3= fachhochschulreife
4= abitur
*/
replace fedu=5 if inrange(fprofedu, 27,30)
replace fedu=6 if inlist(fprofedu, 31,32)

* n in 3 is too small. add fachhochschulreife to abitur
replace fedu = 3 if fedu==4
replace fedu = 4 if fedu==5
replace fedu = 5 if fedu==6

gen medu = msedu
replace medu=. if msedu<0
replace medu=. if msedu==0 /* weiss nicht */
replace medu=0 if msedu==6 /* no degree */
replace medu=. if msedu==5 /* other, not defined */
replace medu=. if msedu>6 /* migrant subsample, degrees in other countries */
/*
1 = hauptschule
2 = realschule
3= fachhochschulreife
4= abitur
*/
replace medu=5 if inrange(mprofedu, 27,30)
replace medu=6 if inlist(mprofedu, 31,32)

* n in 3 is too small. add fachhochschulreife to abitur
replace medu = 3 if medu==4
replace medu = 4 if medu==5
replace medu = 5 if medu==6

gen german = pgnation==1
replace german=. if pgnation<0

gen fgerman = fnat == 1
replace fgerman=. if fnat<0
gen mgerman = mnat == 1
replace mgerman=. if mnat<0

gen childloc = locchildh
replace childloc=. if locchildh<1

gen sameloc = locchild1
replace sameloc=. if locchild1<1

gen region1989 = .
replace region1989=1 if loc1989==1
replace region1989=2 if loc1989==2
replace region1989=3 if loc1989==3
replace region1989=4 if locinfo==0

label variable region1989   "residence in 1989" 
label define region1989 1 "east" 2 "west" 3 "abroad" 4 "born after 1989"
label value region1989 region1989

gen grade_dt = lb0143_h
replace grade_dt = . if lb0143_h<0
replace grade_dt = . if lb0143_h==7 /* did not have this subject */

replace bsntdeut = . if bsntdeut<0
replace bsntdeut = . if bsntdeut==7
replace grade_dt = bsntdeut if grade_dt==.

gen grade_math = lb0144_h
replace grade_math = . if lb0144_h<0
replace grade_math = . if lb0144_h==7 /* did not have this subject */

replace bsntmath = . if bsntmath<0
replace bsntmath = . if bsntmath==7
replace grade_math = bsntmath if grade_math==.

gen grade_flan = lb0145_h
replace grade_flan = . if lb0145_h<0
replace grade_flan = . if lb0145_h==7 /* did not have this subject */

replace bsntfmd1 = . if bsntfmd1<0
replace bsntfmd1 = . if bsntfmd1==7
replace grade_flan = bsntfmd1 if grade_flan==.

egen grade = rmean(grade_dt grade_math grade_flan)
bysort pid: egen grades = mean(grade)
drop grade

* gender

gen female_temp =  pla0009_v2==2
replace female_temp = . if  pla0009_v2<0
bysort pid: egen female = mean(female_temp)
replace female = round(female)
tab female


* save
save "$savedata/soeplong_prediction.dta", replace


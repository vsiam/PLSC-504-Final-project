/* THIS IS THE DO FILE FOR REPLICATING THE CECT SCHEMA IN A SURVEY CONDUCTED IN THE UNITED STATES (Spring 2023)
 ***************************

INPUT: use "US survey APSR.dta"
OUTPUT: US survey APSR.dta
AUTHOR: LH (March 2024)*/

**NOTE TO USERS: please use the output dataset US survey APSR.dta which already has all estimates for CECT incorporated as well as key independent variables. To replicate Table A.7 and Figure A.1, please skip to the code from line 189 onwards to line 2018. 

*******************************************************************************************************************
** Online APPENDIX E: Fig A.1 and Table A.7 and Note 10, main )*
*******************************************************************************************************************
use "USpilot_april2023.dta"

gen ORIGINALDATA=.
order ORIGINALDATA, before (code)
tab edu_degree
label define edu_degree 1 "<high school" 2 "high school degree" 3 "two-year college" 4 "four-year college" 5 "advanced degree"
label values edu_degree edu_degree
label variable edu_degree "level of education"

************************************************
*1. generate field variables*
************************************************
gen FIELDEDUCATION=.

*A) : collapse 21 US field of education info into 14 ESS categorie
tab edutypext
label variable edutypext "type of education extended list"
gen edutype=1 if edutypext==21 // general
replace edutype=2 if edutypext==2 // arts
replace edutype=3 if edutypext==8 // humanities
replace edutype=4 if edutypext==3| edutypext==5| edutypext==12| edutypext==19 // engineering & technical (also computing)
... // agriculture
replace edutype=6 if edutypext==18 // teacher training, education
replace edutype=7 if edutypext==15| edutypext==1 // science, math, agricultural science included here
replace edutype=8 if edutypext==10 // medical, health, nursing
replace edutype=9 if edutypext==4 // economics, business, accounting
replace edutype=10 if edutypext==13| edutypext==16| edutypext==17| edutypext==6 // social studies etc.
replace edutype=11 if edutypext==9 // law & legal
replace edutype=12 if edutypext==7| edutypext==11 // personal care
replace edutype=13 if edutypext==14 // public order and safety
replace edutype=14 if edutypext==20 // transport, telecom
label variable edutype "field of education -- ESS categories"
tab edutypext edutype
label define edutype 1 "general" 2 "arts" 3 "humanities" 4 "technical/engineering" 5 "agriculture" 6 "teaching/education" 7 "science/math/computing" 8 "medical/health/nursing" 9 "economics/business" 10 "social studies/admin/media" 11 "law & legal" 12 "personal care" 13 "public order & safety" 14 "transport/telecomm"
label values edutype edutype

*B): use new skill base to inform educational field 

*create four-resources 
egen usfieldculture=rowmean(edu_skills_1 edu_skills_2 edu_skills_3 edu_skills_4) // cultural skills
egen usfieldecon=rowmean(edu_skills_5 edu_skills_6 edu_skills_7 edu_skills_8) // economic skills
egen usfieldcomm=rowmean(edu_skills_9 edu_skills_10 edu_skills_11 edu_skills_12) // communicative skills
egen usfieldtech=rowmean(edu_skills_13 edu_skills_14 edu_skills_15 edu_skills_16) // technical skills
gen usfieldratio=(usfieldculture+usfieldcomm)/(usfieldculture+usfieldcomm+usfieldecon+usfieldtech)

*investigate the reliability
alpha (edu_skills_1 edu_skills_2 edu_skills_3 edu_skills_4)
alpha ( edu_skills_5 edu_skills_6 edu_skills_7 edu_skills_8)
alpha ( edu_skills_9 edu_skills_10 edu_skills_11 edu_skills_12)
alpha ( edu_skills_13 edu_skills_14 edu_skills_15 edu_skills_16 )	  

pca edu_skills_1 edu_skills_2 edu_skills_3 edu_skills_4 edu_skills_5 edu_skills_6 edu_skills_7 edu_skills_8 edu_skills_9 edu_skills_10 edu_skills_11 edu_skills_12 edu_skills_13 edu_skills_14 edu_skills_15 edu_skills_16, components(4)
rotate

gen us_n=1

save "USpilot_april2023.dta"
	   
/*C) use now these skills as field characteristic
a. calculate means (not including lowest educated)
b. collapse
c. re-read back in the US dataset 

collapse (mean) edu_skills_1 edu_skills_2 edu_skills_3 edu_skills_4 edu_skills_5 edu_skills_6 edu_skills_7 edu_skills_8 edu_skills_9 edu_skills_10 edu_skills_11 edu_skills_12 edu_skills_13 edu_skills_14 edu_skills_15 edu_skills_16 usfieldculture usfieldecon usfieldcomm usfieldtech usfieldratio (count) us_n, by (edutype) 

rename usfieldculture usfieldcultureav
rename usfieldecon usfieldeconav
rename usfieldcomm usfieldcommav
rename usfieldtech usfieldtechav
rename usfieldratio usfieldratioav


save "collapse_usdata.dta"
clear

******************
use "USpilot_april2023.dta"

merge m:1 edutype using "collapse_usdata.dta", keepusing(usfieldcultureav usfieldeconav usfieldcommav usfieldtechav usfieldratioav)
replace usfieldratioav=. if edu_degree!=. & edutype==.
replace usfieldratioav=0.5 if edu_degree==1
tabstat usfieldratioav, stats (mean N)

order usfieldcultureav usfieldeconav usfieldcommav usfieldtechav usfieldratioav, after (us_n)
label variable usfieldcultureav "mean cultural CECT score by edutype"
tab usfieldratioav
label variable usfieldratioav "educational CECT (av)"*/

*********************************************
*2. prepare data for analysis
********************************************
gen MODELING=.
order MODELING, after (usfieldratioav)


*******************************
*independent variables
********************************
*Key independent variable: fieldratio
nscale usfieldratioav, pre(n_)
label variable n_usfieldratioav "educational CECT (av) rescaled"

*gender
tab female
label define gender 0 "man" 1 "woman/other"
label values gender female
label variable female "female or other"

tab age_cat 
label define age_cat 1 "<35years" 2 "35-54y" 3 "55y or older", replace
label values age_cat age_cat
label variable age_cat "age category"

tab rural
label define rural 1 "urban" 2 "suburban" 3 "rural town" 4 "rural country"
label values rural rural
label variable rural "from urban to rural"

gen higher=1 if edu_degree == 4| edu_degree == 5
replace higher=0 if edu_degree<4
label define higher 0 "<four-year college" 1 "four-year college or more"
label values higher higher
label variable higher "higher education"

tab income 
label define income 1 "very difficult on present income" 2 "difficult on present income" 3 "coping on present income" 4 "living comfortably on present income"
label values income income
label variable income "income"

tab secularism
label define secularism 1 "at least once a week" 2 "once or twice a month" 3 "several times a year" 4 "never/almost never"
label values secularism secularism
label variable secularism "secular"

tab black
label variable black "black/african-american"
label define black 0 "other" 1 "black", replace
label values black black
tab white
label variable white "white/caucasian"
label define white 0 "other" 1 "white", replace
label values white white

/*rebalance gender because sample is unbalanced*/
tab gender
gen weightgender=3 if gender==1
tab gender [aweight=weightgender]
replace weightgender=1 if gender==0
tab gender [aweight=weightgender] if edutype!=.
label variable weightgender "weighting to rebalance gender"

nscale (income secularism age_cat rural), pre(n_)  // rescaled to 0-1

*DEPENDENT VARIABLES: USE tight definition of Dem or Republican

gen democrattight=1 if party_pid==1| party_pid==2
replace democrattight=0 if party_pid>2 & party_pid!=.
label define democrattight 1 "strong or weak democrat" 0 "other"
label values democrattight democrattight
gen republicantight=1 if party_pid==6| party_pid==7
replace republicantight=0 if party_pid<6
label define republicantight 1 "strong or weak republican" 0 "other", replace
label values republicantight republicantight
gen independenttight=1 if party_pid==4
replace independenttight=0 if party_pid!=4 & party_pid!=.
label define independenttight 1 "leaning D/none/ leaning R" 0 "other", replace
label values independenttight independenttight
label variable democrattight "democrat (strong or weak)"
label variable republicantight "republican (strong or weak)"
label variable independenttight "independent (leaning D/none/leaning R)"

********************************************  
*3. ANALYSIS : TABLE A.7 (online appendix)
********************************************
*To produce Table A.7, please run the code from line 189 to 212.

eststo USA1tight:  logit democrattight n_usfieldratioav higher female black n_income n_secularism n_age_cat n_rural [weight=weightgender]
outreg2 using USAtight, dec(3) word label replace 
estat ic

margins, at (n_usfieldratioav=(0 (.2) 1))
margins, at (higher =(0 1))
margins, at (n_rural =(0 1))
margins, at (black =(0 1))
margins, at (n_secularism =(0 1))

eststo USA2tight: logit republicantight n_usfieldratioav higher female black n_income n_secularism n_age_cat n_rural [weight=weightgender]
estat ic
outreg2 using USAtight, dec(3) word label

margins, at (n_usfieldratioav=(0 (.2) 1))
margins, at (higher =(0 1))
margins, at (n_rural =(0 1))
margins, at (black =(0 1))
margins, at (n_secularism =(0 1))
margins, at (female=(0 1))

eststo USA3tight: logit independenttight n_usfieldratioav higher female black n_income n_secularism n_age_cat n_rural [weight=weightgender]
estat ic
outreg2 using USAtight, dec(3) word label 
/*as theoretically expected, this category is not influenced by field (or level)*/

*To produce Figure A.1 (online appendix), please run the code from line 2014 to 216 -- AFTER having run the code from line 189 to 218.

coefplot USA1tight || USA2tight || USA3tight, base drop(_cons) xline(0, lpattern(-)) scheme(plotplain) order(n_usfieldratioav higher . black female n_age_cat n_income n_secularism n_rural) 
/*polish manually*/

*************************************************************************************************
**This do file provides a protocol to create the individual CECT field variable using ESS data for waves 2-3-4. 
*The input on the relative preponderance of cultural, economic, communicative, and technical skills is derived from vd Werfhorst & Kraaikamp. This information is then imported into the ESS, which is feasible because the fourteen field of study categories in the ESS are nearly identical to the 11 field of study categories in the vdW-K study. 

*We first calculate average skills scores (1=very little to 5=extensive) for each of the four skills types. We next create the ratio-as-part-of-whole measure for each of the fourteen types of study. 

*Next we rescale this ratio 0-1

*Users may simply use the average scores in the summary table below. Note that these are rescaled 0-1. 

*Reference: APSR article:
*Liesbet Hooghe, Gary Marks, Jonne Kamphorst. 2024. "Why it makes sense to pay attention to a person's field alongside their level of education: Voting on the socio-cultural divide." American Political Science Review.
****************************************************************************************************

use "ESSsubset"

*************************************************
**FIELD VARIABLES
*************************************************
***************************************
** EDUCATIONAL FIELD CHARACTERISTICS **
***************************************
*source: van de Werfhorst & Kraaikamp (2001) || 14 edutypes in ESS, and 11 fields in vdW&K: assign transport to technical || assign arts, humanities to humanities/arts || assign scientists to technical for economic & technical, and to teachers for culture & communicative. Following W&K, allocate a value of 1 to all respondents with primary education only, i.e. if educ5==1 

gen FIELD_CHAR=.

*A) INDIVIDUAL CECT
/*field variable in ESS 2, 3, 4 wave*/
gen edutype=edufld
label variable edutype "field of education"

*create the four skill resources using means calculated by vdW&K (Table 1) 

gen fieldculture=2.94 if edutype==1 // general
replace fieldculture=3.49 if (edutype==2| edutype==3) // arts,humanities
replace fieldculture=2.01 if (edutype==4| edutype==14) //technical
replace fieldculture=2.1 if edutype==5 // agriculture
replace fieldculture=3.6 if edutype==6| edutype==7  // teacher educ
replace fieldculture=1.87 if edutype==8 // medical
replace fieldculture=2.03 if edutype==9 // economics/admin
replace fieldculture=2.66 if edutype==10 //social-cultural
replace fieldculture=2.19 if edutype==11 // law
replace fieldculture=2.4 if edutype==12 // personal care
replace fieldculture=2.41 if edutype==13 // police
tab edutype fieldculture 
label variable fieldculture "cultural skills"

gen fieldecon=2.11 if edutype==1
replace fieldecon=1.71 if (edutype==2| edutype==3)
replace fieldecon=2.14 if (edutype==4| edutype==14| edutype==7)
replace fieldecon=2.88 if edutype==5
replace fieldecon=1.87 if edutype==6
replace fieldecon=1.82 if edutype==8
replace fieldecon=3.41 if edutype==9
replace fieldecon=2.24 if edutype==10
replace fieldecon=3.31 if edutype==11
replace fieldecon=1.82 if edutype==12
replace fieldecon=2.84 if edutype==13
tab edutype fieldecon
label variable fieldecon "economic skills"

gen fieldcomm=2.01 if edutype==1
replace fieldcomm=2.76 if (edutype==2| edutype==3)
replace fieldcomm=1.95 if (edutype==4| edutype==14)
replace fieldcomm=1.97 if edutype==5
replace fieldcomm=3.64 if edutype==6| edutype==7
replace fieldcomm=2.97 if edutype==8
replace fieldcomm=2.54 if edutype==9
replace fieldcomm=3.76 if edutype==10
replace fieldcomm=2.61 if edutype==11
replace fieldcomm=2.26 if edutype==12
replace fieldcomm=3.17 if edutype==13
tab edutype fieldcomm
label variable fieldcomm "communicative skills"

gen fieldtech=2.04 if edutype==1 // general
replace fieldtech=1.61 if (edutype==2| edutype==3) // arts,humanities
replace fieldtech=3.42 if (edutype==4| edutype==7| edutype==14) //technical
replace fieldtech=3.06 if edutype==5 // agriculture
replace fieldtech=1.77 if edutype==6 // teacher educ
replace fieldtech=2.14 if edutype==8 // medical
replace fieldtech=2.06 if edutype==9 // economics/admin
replace fieldtech=1.54 if edutype==10 //social-cultural
replace fieldtech=1.74 if edutype==11 // law
replace fieldtech=1.52 if edutype==12 // personal care
replace fieldtech=2.02 if edutype==13 // police
tab edutype fieldtech 
label variable fieldtech "technical skills"

gen educ5=edulvla if edulvla!=.
label define educ5 1 "<lower secondary" 2 "lower secondary completed" 3 "upper secondary completed" 4 "post-secondary non-tertiary completed" 5 "tertiary completed"
label values educ5 educ5
label variable educ5 "level of education"
tab educ5

replace fieldculture=1 if educ5==1 & edutype!=. // this allocates lowest rating to those with only primary education
replace fieldecon=1 if educ5==1 & edutype!=. 
replace fieldcomm=1 if educ5==1 & edutype!=. 
replace fieldtech=1 if educ5==1  & edutype!=. 

order fieldculture fieldecon fieldcomm fieldtech, after ( edutype)

/*CHIEF INDEPENDENT VARIABLE*/
gen fieldratio=(fieldculture+fieldcomm)/(fieldculture+fieldecon+fieldcomm+fieldtech) 
label variable fieldratio "RATIO of cultcomm to cultcomm+econtech"

tab fieldratio

nscale fieldratio, pre(n_) // this puts fieldratio on 0-1 scale

********************************************************************************************************
/**SUMMARY OF FIELD SCORES ONCE RESCALED 0-1. Note that there is no information on field specialization for respondents with only primary education; they receive 1 out of 5 for each skill, which translates into a score of 0.361 when rescaled to 0-1.
nscale fieldratio, pre (n)
tabstat tabstat n_fieldratio if select==1 & educ5!=1, stats (mean) by (edufld) 
tabstat n_fieldratio if educ5==1, stats (mean)

CECT	edufld						Mean
		
highest	Teacher training/ education						1.000
		Art, fine/applied								0.952
		Humanities										0.952
		Social studies/admininstration/media/culture	0.861
		Personal care services							0.680
		Science/mathematics/computing etc.				0.614
		Medical/health services/nursing etc.			0.554
		General/no specific field						0.531
		Public order and safety							0.494
		Those with primary education only				0.361
		Law and legal services							0.312
		Economics/commerce/business admin				0.188
		Technical and engineering						0.036
		Transport and telecommunications				0.036
lowest	Agriculture/forestry							0.000
*/

/* THIS IS THE DO FILE FOR REPLICATING TABLES, FIGURES IN MAIN TEXT AND ONLINE APPENDIX. 
FOR THE COMMANDS IN THE SUPPLEMENTARY DOCUMENTATION, SEE ESS Additional.do ***************************
APSR article:
Liesbet Hooghe, Gary Marks, Jonne Kamphorst. 2024. "Why it makes sense to pay attention to a person's field alongside their level of education: Voting on the socio-cultural divide." American Political Science Review.


This do file succeeds ESS prep_march 2024.do, which prepares the dataset
INPUT: ESS2to4.dta
OUTPUT: ESSmain.dta
AUTHOR: LH (April 2024)

Ancillary datasets: USpilot_appril2023.dta
Ancillary do file: Appendix_E.do

NOTE TO USERS: this do file specifies which lines of the code to run to produce each figure in the main text and each table or figure in the appendix. Please remember to run the entire do file in sequence so that you generate the additional variables needed in later steps of the analysis. The line code for tables and figures in the appendix starts on line 335. 

******************************************************************
******************************************************************/

use "ESS2to4.dta"
save "ESSmain.dta"

ssc install heatplot, replace
ssc install palettes, replace
ssc install colrspace, replace

*******************************************
tab select, missing
drop if select==0  // this selects cases that meet the central inclusion criteria: older than 20y old, having voted in past election, having a known party vote, respondent from one of 15 western countries: gen select=1 if age>20 & family!=. & (essround==2|essround==3|essround==4) & (cntry=="AT"| cntry=="BE"| cntry=="DK"| cntry=="DE"|cntry=="GR"| cntry=="ES"| cntry=="FR"| cntry=="IE"| cntry=="PT"| cntry=="FI"|cntry=="NL"| cntry=="NO"| cntry=="SE"| cntry=="CH"| cntry=="GB")

gen sample=1 if (GAL!=.| TAN!=.) & fieldratio!=. & occfield!=. & fieldincome!=. & educ5!=. & female!=. & rural!=. & incomeobj!=. & age!=. & secular!=. & class8!=. & select==1
label variable sample "shared N for all models if GAL or TAN party" 

save "ESSmain.dta", replace

*********************************************
**rescale key independent variables from 0-1:
nscale fieldratio occfield occfieldtan occfieldgal fieldincome fieldincometan fieldincomegal, pre(n_)
tab n_fieldratio
tab n_occfield
tab n_fieldincome
order  n_fieldratio n_occfield n_occfieldtan n_occfieldgal n_fieldincome n_fieldincometan n_fieldincomegal, after (fieldincometan)

**************************************
**FIGURE 1 (main): Distribution of CECT in the ESS
************************************** 
*To produce Figure 1, please run this do file from line 51 to 57. This will first create two subgraphs --one for educational CECT and one for occupational CECT-- and next combine them. Polish the figures manually. Calculate stats for the figures from line 60 and 70. 

tab n_fieldratio if educ5!=1

*FIGURE 1A
histogram n_fieldratio if educ5!=1, discrete percent fcolor(gs8%80) fintensity(80) lcolor(none%30) lwidth(none) normal ytitle(Percent) ylabel(0(5)20, angle(horizontal)) ymtick(##5) xtitle(Educational CECT) xlabel(0(.1)1) title(Distribution of educational CECT) graphregion(fcolor(white)) plotregion(fcolor(white)) saving (CECTdist.gph, replace)

*FIGURE 1B
histogram n_occfield, percent fcolor(gs8%80) fintensity(80) lcolor(gs8%80) normal ytitle(Percent) ylabel(0(2)12, angle(horizontal)) ymtick(##5) xtitle(Occupational CECT) xlabel(0(.1)1) title(Distribution of occupational CECT) graphregion(fcolor(white)) plotregion(fcolor(white)) saving(OCCdist.gph, replace)

graph combine CECTdist.gph OCCdist.gph, saving(dist.gph, replace)
*polish manually*

**add p25 p50 p75 manually to graphs
tabstat n_fieldratio if select==1 & educ5!=1, stats (sd p25 p50 p75 N)
/*. tabstat n_fieldratio if select==1 & educ5!=1, stats (sd p25 p50 p75 N)

    Variable |        SD       p25       p50       p75         N
-------------+--------------------------------------------------
n_fieldratio |   .322159  .1877005  .5306696  .6143828     41877
----------------------------------------------------------------
*/
			

tabstat n_fieldratio n_occfield if select==1, stats (sd p25 p50 p75 N)

/*. tabstat n_occfield if select==1, stats (sd p25 p50 p75 N)

    Variable |        SD       p25       p50       p75         N
-------------+--------------------------------------------------
  n_occfield |  .1850731  .3368919  .4150676  .5249513     49264
----------------------------------------------------------------

*/


********************************************
***FIGURE 2 (main) & Table A.3 (Online App) : Baseline model
********************************************
*To produce Figure 2, please run this do file from line 88 to 112. Polish the figure manually.  

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f
eststo CECT1: melogit GAL n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using cect, dec(3) word label replace

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1

set pformat %5.4f
eststo CECT2: melogit TAN n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using cect, dec(3) word label 

drop n_occfield

/*FIGURE 2: BASELINE MODEL: Field of education and voting GAL or TAN*
*figure 2: gal and tan voting with CECT*/
coefplot CECT1 CECT2, base drop(_cons *.country *.essround) xline(0, lpattern(-)) scheme(plotplain) order(n_fieldratio n_occfield n_fieldincome . *.educ5 . female incomeobj rural age secularism) // multilevel mixed-effects logistic regression
*polish manually*

/*discussion of substantive effects -- use logit to run this*/
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

logit GAL n_fieldratio n_occfield n_fieldincome i.educ5 female rural age incomeobj secular i.essround i.country if selectgal==1, vce(robust)

tabstat n_fieldratio n_occfieldgal n_fieldincome if selectgal==1, stats (mean sd)

margins, at (n_fieldratio= (0 (.2) 1)) // use this
margins, at (n_occfield=(0(.2) 1))
margins, at (educ5=(1(1)5))
margins, at (n_fieldincome=(0(.2)1))

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1

logit TAN n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround i.country if selecttan==1, vce(robust)

tabstat n_fieldratio n_occfieldtan n_fieldincome if selecttan==1, stats (mean sd)

margins, at (n_fieldratio= (0 (.2) 1)) 
margins, at (n_occfield=(0(.2)1))
margins, at (educ5=(1(1)5))
margins, at (n_fieldincome=(0(.2)1))

**********************************************************************
**FIGURE 3 (main) & Table A.4 (Online App): Testing a field logic vs. work logic of occupation
************************************************************************
*To produce Figure 3, please run this do file from line 151 to 176. Polish the figure manually.  
**reference = production workers (class8=4)

**********melogit Table A.4 in Online Appendix : Field of education and occupation

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo oesch1: melogit GAL n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular ib4.class8 i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using oesch, dec(3) word label replace

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1

drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo oesch2: melogit TAN n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular ib4.class8 i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using oesch, dec(3) word label 

drop n_occfield

/*Figure 3: Field, occupation and voting GAL or TAN*/
coefplot oesch1 oesch2, base drop(_cons *.country *.essround rural age secular incomeobj n_fieldincome *.educ5 female) xline(0, lpattern(-)) scheme(plotplain) order(n_fieldratio n_occfield . class8) 
*polish manually*

/*discussion*/
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

logit GAL n_fieldratio n_occfield n_fieldincome ib4.class8 i.educ5 female rural age secular incomeobj i.essround i.country if selectgal==1, vce(robust)
margins, at (n_fieldratio=(0 1))
margins i.class8
margins, at (n_occfield=(0 1))
margins, at (n_fieldincome=(0 (.1) 1))

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1

drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

logit TAN n_fieldratio n_occfieldtan n_fieldincome ib4.class8 i.educ5 female rural age secular incomeobj i.essround i.country if selecttan==1, vce(robust)
margins, at (n_fieldratio=(0 1))
margins i.class8
margins, at (n_occfieldtan=(0 1))
margins, at (n_fieldincome=(0(.2)1))

***************************************************
**FIGURE 4 (main) and TABLE A.5 (Online App): The effect of field of education among higher and lower educated

*****************************************************
*To produce Figure 4, please run this do file from line 211 to 234. This will first create separate figures for GAL and TAN, and then combine into one figure. Polish the figure manually.  

*This introduces an interaction between higher educ and educational CECT for GAL voting and tan voting. The full model is in Table A.11 in the appendix (using melogit)

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

logit GAL i.higher##c.n_fieldratio n_occfield n_fieldincome female incomeobj rural age secular i.essround i.country if selectgal==1, vce(robust)
margins, at (n_fieldratio=(0(.1)1) higher=(0 1))
marginsplot, recastci(rline) ciopts(lpattern(longdash) ytitle(Propensity to vote GAL) xtitle(Educational CECT) legend(position(6))) scheme(plotplain) saving(higher1pp.gph, replace)
margins, at (n_fieldratio=(0.0362103 0.861) higher=(0 1))

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1

logit TAN i.higher##c.n_fieldratio n_occfield n_fieldincome female incomesubj rural age secular i.essround i.country if selecttan==1, vce(robust)
margins, at (n_fieldratio=(0(.1)1) higher=(0 1))
marginsplot, recastci(rline) ciopts(lpattern(longdash) ytitle(Propensity to vote TAN) xtitle(Educational CECT) legend(position(6))) scheme(plotplain) saving(higher2pp.gph, replace)
margins, at (n_fieldratio=(0.0362103 0.861) higher=(0 1))

*Figure 4: The effect of field of education among higher and lower educated
graph combine higher1pp.gph higher2pp.gph, xcom scheme(plotplain) altshrink imargin(zero) saving(higherppx.gph, replace) // double figure
*polish manually*

/*use these to provide p-values*/
drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f
melogit TAN n_fieldratio n_occfield n_fieldincome female incomeobj rural age secular i.essround if selecttan==1 & higher==0, || cntry: || isco3tr:, vce(oim)
matrix list r(table) // grab p-value
melogit TAN n_fieldratio n_occfield n_fieldincome female rural age secular incomeobj i.essround if selecttan==1 & higher==1, || cntry: || isco3tr:, vce(oim)
matrix list r(table) // grab p-value
**# Bookmark #2
drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f
melogit GAL n_fieldratio n_occfield n_fieldincome female incomeobj rural age secular i.essround if selectgal==1 & higher==0, || cntry: || isco3tr:, vce(oim)
matrix list r(table) // grab p-value
melogit GAL n_fieldratio n_occfield n_fieldincome female incomeobj rural age secular i.essround if selectgal==1 & higher==1, || cntry: || isco3tr:, vce(oim)
matrix list r(table) // grab p-value

/*USE THIS FOR TABLE A.5 - Field of education for higher and lower educated (APPENDIX) WITH INTERACTION TERM*/
drop n_occfield
gen n_occfield=n_occfieldgal
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo CECT3: melogit GAL i.higher##c.n_fieldratio n_occfield n_fieldincome female incomeobj rural age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using cecthigh, dec(3) word label replace

drop n_occfield
drop n_fieldincome

gen n_occfield=n_occfieldtan if selecttan==1
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo CECT4: melogit TAN i.higher##c.n_fieldratio n_occfield n_fieldincome female incomeobj rural  age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using cecthigh, dec(3) word label 


*******************************************************************************
***FIGURE 5 (main): The effect of field on green and TAN voting while controlling for gender (or not) and Table A.6 (online app) **
********************************************************************************
*To produce Figure 5, please run this do file from line 286 to line 330. Then polish manually.

*a) *Table A.6 (online app) on gender **
drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo gender1: melogit GAL n_fieldratio n_occfield n_fieldincome i.educ5 incomeobj rural age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
outreg2 using gender, dec(3) word label replace 
estat ic // provides BIC and log linear

eststo gender2: melogit GAL n_fieldratio n_occfield n_fieldincome female i.educ5 incomeobj rural age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using gender, dec(3) word label 

eststo gender3: melogit GAL female i.educ5 n_fieldincome incomeobj rural age secular i.essround if selectgal==1 & n_fieldratio!=.|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using gender, dec(3) word label 

drop n_occfield
drop n_fieldincome

gen n_occfield=n_occfieldtan if selecttan==1
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo gender4: melogit TAN n_fieldratio n_occfield n_fieldincome i.educ5 incomeobj rural age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic 
outreg2 using gender, dec(3) word label 

eststo gender5: melogit TAN n_fieldratio n_occfield n_fieldincome female i.educ5 incomeobj rural age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using gender, dec(3) word label 

eststo gender6: melogit TAN female i.educ5 n_fieldincome incomeobj rural age secular i.essround if selecttan==1 & n_fieldratio!=.|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using gender, dec(3) word label 


*b)*FIGURE 5 (main): the effect of field on green and TAN voting with or without controlling for gender**

set scheme plotplain
**# Bookmark #1
coefplot gender1 gender2 || gender4 gender5, base drop(_cons *.country *.essround rural age *.educ5 n_fieldincome incomeobj secular) order(n_fieldratio n_occfield female) xline(0, lpattern(-)) nolabel scheme(plotplain) 
graph save genderfield.gph, replace
*polish manually*


*********************************************************************************

**TABLES AND FIGURES IN APPENDIX 

****************************************************************************
**APPENDIX A: DESCRIPTIVE SUPPORT FOR ESS ANALYSIS
***************************************************************
*TABLE A.1: COUNTRY COVERAGE
by essround, sort: tab cntry GAL if select==1
by essround, sort: tab cntry TAN if select==1

*TABLE A.2a, A.2b, A.2c: DESCRIBE INDEPENDENT AND CONTROL VARIABLES--no code

**********************************************************
***Table A.3: the baseline model of field of education
**********************************************************
*To produce Table A.3 (basis for generating Figure 2 in main text), please run this code from line 351 to line 368.

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f
eststo CECT1: melogit GAL n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using cect, dec(3) word label replace

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1

set pformat %5.4f
eststo CECT2: melogit TAN n_fieldratio n_occfield n_fieldincome i.educ5 female incomeobj rural age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using cect, dec(3) word label 


********************************************************************
**Table A.4: field, occupation, and voting
*********************************************************************
*To produce Table A.4 (basis for generating Figure 3 in main text), please run this code from line 376 to line 394.

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo oesch1: melogit GAL n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular ib4.class8 i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using oesch, dec(3) word label replace

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo oesch2: melogit TAN n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular ib4.class8 i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using oesch, dec(3) word label 


***********************************************************************
**Table A.5 TESTING THE EFFECT OF FIELD AMONG HIGHER AND LOWER EDUCATED
***********************************************************************
*To produce Table A.5 (basis for generating Figure 4 in main text), please run this code from line 402 to line 419.

drop n_occfield
gen n_occfield=n_occfieldgal
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f
eststo CECT3: melogit GAL i.higher##c.n_fieldratio n_occfield n_fieldincome female rural age incomeobj secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic

drop n_occfield
gen n_occfield=n_occfieldtan
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f
eststo CECT4: melogit TAN i.higher##c.n_fieldratio n_occfield n_fieldincome female rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic

outreg2 using cecthigh, dec(3) word label replace
outreg2 using cecthigh, dec(3) word label 

*******************************************************************************
**Table A.6: TESTING HOW GENDER AND FIELD OF EDUCATION RELATE
*******************************************************************************
*To produce Table A.6 (basis for generating Figure 5 in main text), please run this code from line 426 to line 460.

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo gender1: melogit GAL n_fieldratio n_occfield i.educ5 n_fieldincome rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
outreg2 using gender, dec(3) word label replace 
estat ic // provides BIC and log linear

eststo gender2: melogit GAL n_fieldratio n_occfield female i.educ5 n_fieldincome rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using gender, dec(3) word label 

eststo gender3: melogit GAL female i.educ5 n_fieldincome rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using gender, dec(3) word label 

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo gender4: melogit TAN n_fieldratio n_occfield i.educ5 n_fieldincome rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic 
outreg2 using gender, dec(3) word label 

eststo gender5: melogit TAN n_fieldratio n_occfield female i.educ5 n_fieldincome rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using gender, dec(3) word label 

eststo gender6: melogit TAN female i.educ5 n_fieldincome rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim) 
estat ic
outreg2 using gender, dec(3) word label 


save "ESSmain.dta"

***THIS IS THE END OF COVERAGE FOR ESS APSR main and appendix.do

*******************************************************************************************************************
** APPENDIX E: REPLICATION OF THE CECT SCHEMA IN A SURVEY IN THE UNITED STATES (Fig A.1 and Table A.7)*
*******************************************************************************************************************

/*use "USpilot_april2023.dta" 

*please go to separate do file: APPENDIX_E.do"*/


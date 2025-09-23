/* THIS IS THE DO FILE FOR REPLICATING TABLES, FIGURES IN SUPPLEMENTARY DOCUMENTATION, SEE DO FIELD ESS APSR ADDITIONAL.do ***************************

This do file succeeds ESS prep_march 2024.do, which prepares the dataset for analysis.
INPUT: ESS2to4.dta
OUTPUT: ESS_Add.dta
AUTHOR: LH (March 2024)

Ancillary datasets: none
Ancillary do file: none


STRUCTURE
1. APPENDIX A: DESCRIPTIVE SUPPORT FOR ESS ANALYSIS
2. APPENDIX B: INFORMATION ON FOUR-RESOURCE SCHEMA (no commands)
3. APPENDIX C: FIELD OF EDUCATION -- DECOMPOSING CECT
4. APPENDIX D: TESTING ALTERNATIVE OPERATIONALIZATION: STEM
5. APPENDIX E: LEVEL OF EDUCATION: DICHOTOMOUS OR FIVE CATEGORIES
6. APPENDIX F: FIELD AND LEVEL OF EDUCATION IN MULTIPARTY SYSTEMS
7. APPENDIX G: REPLICATION OF EDUCATIONAL FIELD MODEL BY COUNTRY
8. APPENDIX H: QUESTION WORDING US SURVEY(no commands)

******************************************************************
******************************************************************/

use "ESS2to4.dta"

ssc install heatplot, replace
ssc install palettes, replace
ssc install colrspace, replace

*******************************************
tab select, missing
drop if select==0  // this selects cases that meet the central inclusion criteria: older than 20y old, having voted in past election, having a known party vote, respondent from one of 15 western countries: gen select=1 if age>20 & family!=. & (essround==2|essround==3|essround==4) & (cntry=="AT"| cntry=="BE"| cntry=="DK"| cntry=="DE"|cntry=="GR"| cntry=="ES"| cntry=="FR"| cntry=="IE"| cntry=="PT"| cntry=="FI"|cntry=="NL"| cntry=="NO"| cntry=="SE"| cntry=="CH"| cntry=="GB")

save "ESS_Add.dta", replace

*********************************************
**rescale key independent variables from 0-1:
nscale fieldratio occfield occfieldgal occfieldtan fieldincome fieldincomegal fieldincometan, pre(n_)
tab n_fieldratio
tab n_occfield
tab n_fieldincome
order  n_fieldratio n_occfield n_occfieldtan n_occfieldgal n_fieldincome n_fieldincometan n_fieldincomegal, after (fieldincometan)


*********************************************************************************
**TABLES AND FIGURES IN SUPPLEMENTARY MATERIAL
****************************************************************************

******************************************************
**APPENDIX A: DESCRIPTIVE SUPPORT FOR THE ESS ANALYSIS
******************************************************

*TABLE A.1: PARTY FAMILIES*
tab family2 if selectgal==1 
tab family2 if selecttan==1

*TABLE A.2: LIST OF TAN AND GREEN PARTIES

tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==13
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==1
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==2
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==14
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==3
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==4
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==6
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==7
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==10
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==12
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==35
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==5
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==16
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==36
tab party essround if select==1 & (family2==7| family2==1| family2==12| family2==13) & country==11

*TABLE A.3: Descriptives
gen sample=1 if (GAL!=.| TAN!=.) & fieldratio!=. & occfield!=. & fieldincome!=. & educ5!=. & female!=. & rural!=. & incomeobj!=. & age!=. & secular!=. & class8!=. & select==1
tab sample

tabstat n_fieldratio n_occfield n_fieldincome educ5 higher female rural secular age incomeobj if sample==1, stats (mean min max sd N)

tabulate class8, gen(class8_)  // create dummies from categorical variable

tabstat  class8_1 class8_2 class8_3 class8_4 class8_5 class8_6 class8_7 class8_8 if sample==1, stats (mean min max sd N)

gen edu1=1 if edulvla==1 & sample==1
replace edu1=0 if edulvla!=1 & sample==1
gen edu2=1 if edulvla==2 & sample==1
replace edu2=0 if edulvla!=2 & sample==1
gen edu3=1 if edulvla==3 & sample==1
replace edu3=0 if edulvla!=3 & sample==1
gen edu4=1 if edulvla==4  & sample==1
replace edu4=0 if edulvla!=4 & sample==1
gen edu5=1 if edulvla==5  & sample==1
replace edu5=0 if edulvla!=5 & sample==1

tabstat edu1 edu2 edu3 edu4 edu5 if sample==1, stats (mean min max sd N)

*TABLE A.4 CORRELATION MATRIX HEATPLOT in appendix*
corr GAL TAN n_fieldratio n_occfield n_fieldincome educ5 higher female rural age incomeobj secular class8_1 class8_2 class8_3 class8_4 class8_5 class8_6 class8_7 class8_8 if sample==1
matrix C=r(C)
heatplot C, values(format(%9.3f)) color(hue, chroma(50) luminance(70)) aspectratio(1) legend(off) lower nodiagonal
*polish manually*


*TABLE A.5: individual CECT by field
tab edutype if select==1 & sample==1 & educ5!=1
tab edufld if select==1 & sample==1 & educ5!=1
tabstat n_fieldratio if select==1 & educ5!=1 & sample==1, stats (mean) by (edutype)
tab edutype country if select==1 & sample==1, column nofreq
tabstat fieldincome if sample==1, stats (mean N sd) by (edufld)

*******************************************************************************
***APPENDIX B: INFORMATION ON THE FOUR-RESOURCE SCHEMA AND APPLICATION
*******************************************************************************

*******************************************************************************
***APPENDIX C: DECOMPOSING CECT: Table A.6 and Figures A.1 and A.2
*******************************************************************************
*1. Estimate ratios for individual components *
gen fieldculturep=(fieldculture/(fieldculture+fieldecon+fieldcomm+fieldtech)) if select==1
gen fieldcommp=(fieldcomm/(fieldculture+fieldecon+fieldcomm+fieldtech)) if select==1
gen fieldeconp=(fieldecon/(fieldculture+fieldecon+fieldcomm+fieldtech)) if select==1
gen fieldtechp=(fieldtech/(fieldculture+fieldecon+fieldcomm+fieldtech)) if select==1

nscale fieldculturep fieldcommp fieldeconp fieldtechp, pre(n_)


*2. Table A.6: Field of education: decomposing CECT
*A) GAL

drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo DECOMGA: melogit GAL n_fieldratio i.educ5 n_fieldincome female rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim)
estat ic  
outreg2 using decomposeg, dec(3) word label replace

eststo DECOMGB: melogit GAL n_fieldculturep n_fieldcommp n_fieldeconp n_fieldtechp i.educ5 n_fieldincome female rural incomeobj age secular i.essround if selectgal==1|| cntry:|| isco3tr:, vce(oim)
estat ic  
outreg2 using decomposeg, dec(3) word label 

*B) TAN
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo DECOMTA: melogit TAN n_fieldratio i.educ5 n_fieldincome female rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic  
outreg2 using decomposeg, dec(3) word label

eststo DECOMTB: melogit TAN n_fieldcommp n_fieldeconp n_fieldtechp n_fieldculturep i.educ5 n_fieldincome female rural incomeobj age secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic  
outreg2 using decomposeg, dec(3) word label 

*Fig A.3 (GAL) and Fig A.4 (TAN)*
coefplot DECOMGA DECOMGB, base drop(_cons rural age secular incomeobj *.educ5 female n_fieldincome *.country *.essround) xline(0, lpattern(-)) scheme(plotplain) order(n_fieldratio . n_fieldculturep n_fieldcommp n_fieldeconp n_fieldtechp . higher female)
*polish manually*
coefplot DECOMTA DECOMTB, base drop(_cons rural age secular incomeobj *.educ5 female n_fieldincome  *.country *.essround) xline(0, lpattern(-)) scheme(plotplain) order(n_fieldratio . n_fieldculturep n_fieldcommp n_fieldeconp n_fieldtechp)
*polish manually*

*************************************************************************************
**APPENDIX D: TESTING AN ALTERNATIVE OPERATIONALIZATION OF FIELD: STEM: Table A.7 and Figure A.3 
*************************************************************************************

***********************************
*US-NSF DEFINITION (inclusive)

gen NSF=0 if edutype==4| edutype==5| edutype==7| edutype==8| edutype==9| edutype==9|edutype==10|edutype==14
replace NSF=1 if edutype==1| edutype==2|edutype==3|edutype==6|edutype==6| edutype==11|edutype==12| edutype==13
label define NSF 0 "STEM" 1 "OTHER", replace
label values NSF NSF
label variable NSF "NSF definition of STEM"

/*create a variable that summarizes NSF at the isco3 level*/
egen occNSF=mean(NSF) if select==1, by (isco3tr)
label variable occNSF "NSF by isco3tr 2-4 countries w gal/tan party"
tabstat occNSF n_occfield if select==1, stats (min max p50 mean sd N)


*(a) Table A.7: the effect of STEM or CECT on voting green and TAN
drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo NSFga: melogit GAL i.NSF occNSF i.educ5 n_fieldincome female rural age secular incomeobj i.essround if select==1, || cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using NSF, dec(3) word label replace
eststo NSFgb: melogit GAL n_fieldratio n_occfield i.educ5 n_fieldincome female rural age secular incomeobj i.essround if select==1 & NSF!=., || cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using NSF, dec(3) word label 

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo NSFta: melogit TAN i.NSF occNSF i.educ5 n_fieldincome female rural age secular incomeobj i.essround if select==1, || cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using NSF, dec(3) word label 
eststo NSFtb: melogit TAN n_fieldratio n_occfield i.educ5 n_fieldincome female rural age secular incomeobj i.essround if select==1 & NSF!=., || cntry: || isco3tr:, vce(oim)
estat ic
outreg2 using NSF, dec(3) word label


*(b) Figure A.3: the effect of field of education: STEM vs. CECT as measure

coefplot NSFga NSFgb || NSFta NSFtb, base keep(0.NSF 1.NSF occNSF n_fieldratio n_occfield) xline(0, lpattern(-)) order(0.NSF 1.NSF occNSF n_fieldratio n_occfield) scheme(plotplain)
*polish manually*

*********************************************************************
**APPENDIX E: TESTING ALTERNATIVE OPERATIONALIZATION FOR LEVEL OF EDUCATION (Table A.8) and Figure A.4.
*********************************************************************

drop n_occfield
gen n_occfield=n_occfieldgal if selectgal==1
drop n_fieldincome
gen n_fieldincome=n_fieldincomegal if selectgal==1
set pformat %5.4f

eststo LEVEL1A: melogit GAL n_fieldratio n_occfield higher n_fieldincome female rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using level, dec(3) word label replace

eststo LEVEL1B: melogit GAL n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround if selectgal==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using level, dec(3) word label 

drop n_occfield
gen n_occfield=n_occfieldtan if selecttan==1
drop n_fieldincome
gen n_fieldincome=n_fieldincometan if selecttan==1
set pformat %5.4f

eststo LEVEL2A: melogit TAN n_fieldratio n_occfield higher n_fieldincome female rural age incomeobj secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using level, dec(3) word label 

eststo LEVEL2B: melogit TAN n_fieldratio n_occfield i.educ5 n_fieldincome female rural age incomeobj secular i.essround if selecttan==1|| cntry: || isco3tr:, vce(oim)
estat ic 
outreg2 using level, dec(3) word label 

*Figure A.3
coefplot LEVEL1A LEVEL1H || LEVEL2A LEVEL2H, base drop(_cons *.country *.essround age rural secular female incomeobj) xline(0, lpattern(-)) scheme(plotplain) order(n_fieldratio n_occfield *.educ5 higher)
*polish manually*

****************************************************************************************
**APPENDIX F: FIELD AND LEVEL OF EDUCATION IN MULTIPARTY SYSTEMS (Figure A.4 and Table A.9)
****************************************************************************************

/*1. create ideological blocs for fifteen countries. following Abou-Chadi and Hix (Table A1 in Appendix) + 
Ireland (GAL| no TAN; Labour vs. FF and Fine Gail ), Greece (LAOS | Greens; PASOK vs. ND), Spain (GAL, no TAN*; PSOE vs. PP), Portugal (GAL, no TAN; PS vs. PSD) */

gen BTAN=1 if TAN==1 & select==1
replace BTAN= 0 if TAN!=1 & select==1
gen BGAL = 1 if GAL==1 & select==1
replace BGAL=0 if GAL!=1 & select==1

gen BRight=1 if (prtvtat==2| prtvtaat==2| prtvtabe==2| prtvtabe==8| prtvtabe==9| prtvtabe==12| prtvtbbe==2| prtvtbbe==8| prtvtbbe==9| prtvtbbe==12| prtvtdk==3| prtvtdk==8| prtvtadk==3| prtvtadk==8| prtvtbdk==3| prtvtbdk==7| prtvtfi==1| prtvtfi==4| prtvtafi==1| prtvtafi==4| prtvtfr==12| prtvtfr==13| prtvtafr==12| prtvtafr==13|prtvtbfr==10| prtvtbfr==11| prtvade2==2| prtvbde2==2| prtvtgb==1| prtvtagb==1| prtvtanl==1| prtvtanl==3| prtvtbnl==1| prtvtbnl==3|prtvtcnl==1| prtvtcnl==3| prtvtno==7| prtvtse==1| prtvtse==5| prtvtse==3| prtvtch==2| prtvtch==1 | prtvtach==2| prtvtach==1| prtvtbch==2| prtvtbch==1 | prtvtaes==1 | prtvtbes==1 | prtvtagr==2| prtvtbgr==2| prtvtie==1| prtvtie==2| prtvtpt==11| prtvtapt==11) & select==1
replace BRight=0 if BRight==. & select==1

gen BLeft=1 if (prtvtat==1| prtvtat==2| prtvtabe==5| prtvtabe==13| prtvtbbe==5| prtvtbbe==13| prtvtdk==1| prtvtadk==1| prtvtbdk==1| prtvtfi==9| prtvtafi==8| prtvtafi==8| prtvtfr==10| prtvtafr==10| prtvtbfr==8| prtvade2==1 | prtvbde2==1 |  prtvtgb==2| prtvtagb==2| prtvtanl==2 | prtvtbnl==2| prtvtcnl==2 | prtvtno==3 | prtvtse==6 | prtvtch==3 | prtvtach==3 | prtvtbch==3 | prtvtaes==2 | prtvtbes==2 | prtvtagr==1 | prtvtbgr==1| prtvtie==3| prtvtpt==10| prtvtapt==10) & select==1
replace BLeft=0 if BLeft==. & select==1

label variable BTAN "TAN bloc -- AC-H+"
label variable BRight "RIGHT bloc -- AC-H+"
label variable BLeft "LEFT bloc -- AC-H+"
label variable BGAL "GAL bloc -- AC-H+"

gen bloc4=1 if BTAN==1
replace bloc4=2 if BRight==1 
replace bloc4=3 if BLeft==1
replace bloc4=4 if BGAL==1
label define bloc4 1 "TAN" 2 "Mainstream Right" 3 "Mainstream Left" 4 "GAL", replace
label values bloc4 bloc4
label variable bloc4 "ideological blocs (Abou-Chadi/Hix +)"


/*2. run melogits for appendix -- table A.9 -- Party bloc analysis with field of education under controls*/
drop n_occfield n_fieldincome
nscale occfield fieldincome, pre(n_) // for all countries with either GAL or TAN party or both

eststo BLOC1: melogit BTAN n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround if select==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using bloc, dec(3) word label replace

eststo BLOC2: melogit BRight n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround if select==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using bloc, dec(3) word label 

eststo BLOC3: melogit BLeft n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround if select==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using bloc, dec(3) word label 

eststo BLOC4: melogit BGAL n_fieldratio n_occfield i.educ5 n_fieldincome female rural incomeobj age secular i.essround if select==1|| cntry: || isco3tr:, vce(oim) 
estat ic // provides BIC and log linear
outreg2 using bloc, dec(3) word label 

/*3: Figure A.4: The differential effect of field of education across party blocs*/
coefplot BLOC1 BLOC2 BLOC3 BLOC4, base keep (n_fieldratio n_occfield) xline(0) nolabel scheme(plotplain) 
*polish manually*

**************************************************************
**APPENDIX G: REPLICATION OF THE EDUCATIONAL FIELD MODEL BY COUNTRY (Table A.10a, A.10b, A.11, Table A.12) */ 
****************************************************************************************************
* MODELS FOR INDIVIDUAL COUNTRIES COUNTRY VARIATION */

*1. Table A.10a and b: GAL voting and field of education by country 
eststo COUNTRY1: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="AT", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label replace
eststo COUNTRY2: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="BE", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY3: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="DK", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY4: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="FI", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label
eststo COUNTRY5: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="FR", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label
eststo COUNTRY6: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="DE", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label
eststo COUNTRY7: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="GR", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY8: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="NL", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label replace
eststo COUNTRY9: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="NO", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY10: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="PT", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY11: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="ES", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 
eststo COUNTRY12: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="SE", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label
eststo COUNTRY13: logit GAL n_fieldratio n_occfieldgal i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if selectgal==1 & cntry=="CH", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using country, dec(3) word label 


*2. Table A.11: TAN voting and field of education by country 

eststo COUNTRY2b: logit TAN n_fieldratio n_occfieldtan i.educ5 n_fieldincometan female rural incomeobj age secular i.essround if selecttan==1 & cntry=="BE", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using countryb, dec(3) word label replace 
eststo COUNTRY3b: logit TAN n_fieldratio n_occfieldtan i.educ5 n_fieldincometan female rural incomeobj age secular i.essround if selecttan==1 & cntry=="DK", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using countryb, dec(3) word label 
eststo COUNTRY4b: logit TAN n_fieldratio n_occfieldtan i.educ5 n_fieldincometan female rural incomeobj age secular i.essround if selecttan==1 & cntry=="FR", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using countryb, dec(3) word label 
eststo COUNTRY5b: logit TAN n_fieldratio n_occfieldtan i.educ5 n_fieldincometan female rural incomeobj age secular i.essround if selecttan==1 & cntry=="NO", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using countryb, dec(3) word label 
eststo COUNTRY6b: logit TAN n_fieldratio n_occfieldtan i.educ5 n_fieldincometan female rural incomeobj age secular i.essround if selecttan==1 & cntry=="CH", vce(oim) 
estat ic // provides BIC and log linear
outreg2 using countryb, dec(3) word label 


*3.a. the French exception -- Table A.12 and Figure A.5

drop n_occfield
nscale occfield, pre(n_)

*construct a DV that includes all Left and all Right*
gen FLeft=1 if (family==5| family==6| family==7) & country==6 & sample==1 & edutype!=.
replace FLeft=0 if FLeft==. & country==6 & sample==1
gen FLeftmain=1 if (family==5) & country==6 & sample==1 & edutype!=.
replace FLeftmain=0 if FLeftmain==. & country==6 & sample==1 & edutype!=.
gen FRight=1 if (family==1| family==2| family==3| family==4) & country==6 & sample==1
replace FRight=0 if FRight==. & country==6 & sample==1 & edutype!=.
gen FRightmain=1 if (family==2| family==4) & country==6 & sample==1 & edutype!=.
replace FRightmain=0 if FRightmain==. & country==6 & sample==1 & edutype!=.

eststo FRANCEL2: logit FLeft n_fieldratio n_occfield i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if select==1 & cntry=="FR", vce(oim)
outreg2 using countryc, dec(3) word label replace
estat ic

eststo FRANCEL: logit FLeftmain n_fieldratio n_occfield i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if select==1 & cntry=="FR", vce(oim)
outreg2 using countryc, dec(3) word label
estat ic

eststo FRANCER2: logit FRight n_fieldratio n_occfield i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if select==1 & cntry=="FR", vce(oim)
outreg2 using countryc, dec(3) word label
estat ic

eststo FRANCER: logit FRightmain n_fieldratio n_occfield i.educ5 n_fieldincomegal female rural incomeobj age secular i.essround if select==1 & cntry=="FR", vce(oim)
outreg2 using countryc, dec(3) word label
estat ic

coefplot FRANCEL2 FRANCEL || FRANCER2 FRANCER, base drop (_cons *.essround  n_fieldincomegal female rural  incomeobj age secular) xline(0) nolabel scheme(plotplain) order(n_fieldratio n_occfield . *.educ5) /*use this -- Figure A.5*/


*****************************************************************
*** APPENDIX H. QUESTION WORDING IN 2023 US SURVEY REPLICATING THE SKILLS SCHEMA
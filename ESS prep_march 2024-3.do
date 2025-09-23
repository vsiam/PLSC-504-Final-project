/*THIS IS THE MASTER DO FILE FOR PREPARING THE DATASET for the APSR article:
Liesbet Hooghe, Gary Marks, Jonne Kamphorst. 2024. "Why it makes sense to pay attention to a person's field alongside their level of education: Voting on the socio-cultural divide." American Political Science Review.

****Prepares ESS data, merges with CHES, applies OESCH, creates field variables etc. 
INPUT: ESSsubset.dta
OUTPUT: ESS2to4.dta
AUTHOR: LH (March 2024)

ancillary dataset: CHES data set: 1999-2019_CHES_dataset_meansv3.dta

********************************************************************************************
********************************************************************************************
	
First step is to download ESS waves 2-4 for 15 western countries, and select variables: https://ess.sikt.no/en/?tab=builder*
Please note that the ESS cumulative file has errors for France - wave 2; this affects some variables that need to build the Oesch categories. The original France-wave 2 datafile is available on the ESS website and the relevant missing information has been read into this data file.*/
*this is saved as "ESSsubset.dta"

/* REFERENCES:
ESS Round 2: European Social Survey Round 2 Data (2004). Data file edition 3.6. Sikt - Norwegian Agency for Shared Services in Education and Research, Norway – Data Archive and distributor of ESS data for ESS ERIC. doi:10.21338/NSD-ESS2-2004.
ESS Round 3: European Social Survey Round 3 Data (2006). Data file edition 3.7. Sikt - Norwegian Agency for Shared Services in Education and Research, Norway – Data Archive and distributor of ESS data for ESS ERIC. doi:10.21338/NSD-ESS3-2006.
ESS Round 4: European Social Survey Round 4 Data (2008). Data file edition 4.5. Sikt - Norwegian Agency for Shared Services in Education and Research, Norway – Data Archive and distributor of ESS data for ESS ERIC. doi:10.21338/NSD-ESS4-2008.*/

use "ESSsubset.dta"

*now create working ESS dataset in steps.

save "ESS2to4.dta"  

/*this is the original dataset before manipulation*/



/* STRUCTURE
1. Clean Up (missing values reset)
2. Oesch commands (with links to oesch 1 and oesch 3.do)
3. CHES read-in [appr. 791] with link to party level data.dta 
4. create social demographics
5. create select gen select=1 if age>20 & family!=.
6. create field variables
7. create dependent variables
*/

****************************************************************************************************************************************************************************************************************************************************************************************************************	
* 1. Clean Up and Preparation 
********************************************************************** 

*************************
recode edulvla (55=.)  //| *education is now standardized between waves 55=other *
recode eisced (55=.) // large number of not-harmonized in 0 


********************************************************************
*****************************************************************
*2. OESCH class coding 

*******************************************************************
**OESCH script for 2002-2010 using (isco88)  is available here: 
https://people.unil.ch/danieloesch/files/2014/05/Oesch_class_schema_ESS2002_2006_Stata.txt
**please note that now there exists a module 'oesch' created by simon Kaiser that creates the OESCH class schema
*Reference: Oesch, Daniel. 2006. Coming to grips with a changing class structure. International Sociology 21(2): 263-288.

ssc install oesch 

gen ISCOCODING=.

iskooesch oesch16, isko( iscoco ) emplrel( emplrel ) emplno( emplno)
oesch oesch8, oesch(oesch16_oesch16)
tab oesch16_oesch16
tab oesch8_oesch8
rename oesch16_oesch16 class16
label variable class16 "Oesch 16 categories"
rename oesch8_oesch8 class8
label variable class8 "Oesch 8 categories"
oesch oesch5, oesch(class16) five
rename oesch5_oesch5 class5
label variable class5 "Oesch 5 caegories"
order class5, after (class8)

gen isco3tr=trunc(iscoco/10)



save "ESS2to4.dta", replace

**** References:
**** Oesch, D. (2006a) "Coming to grips with a changing class structure" International Sociology 21 (2): 263-288.
**** Oesch, D. (2006b) "Redrawing the Class Map. Stratification and Institutions in Britain, Germany, Sweden and Switzerland", Basingstoke: Palgrave Macmillan.


**** 16-Class schema constructed
  *1 Large employers
  *2 Self-employed professionals
  *3 Small business owners with employees
  *4 Small business owners without employees
  *5 Technical experts
  *6 Technicians
  *7 Skilled manual
  *8 Low-skilled manual
  *9 Higher-grade managers and administrators
  *10 Lower-grade managers and administrators
  *11 Skilled clerks
  *12 Unskilled clerks
  *13 Socio-cultural professionals
  *14 Socio-cultural semi-professionals
  *15 Skilled service
  *16 Low-skilled service

**** 8-Class schema constructed
  *1 Self-employed professionals and large employers
  *2 Small business owners
  *3 Technical (semi-)professionals
  *4 Production workers
  *5 (Associate) managers
  *6 Clerks
  *7 Socio-cultural (semi-)professionals
  *8 Service workers


***********************************************************
***********************************************************
***3. PREPARATION MERGING CHES
***********************************************************
***********************************************************

**************************************
**A) Code Connecting CHES party_id into ESS  
**************************************
*Reference for CHES trend data: Jolly, Seth, Ryan Bakker, Liesbet Hooghe, Gary Marks, Jonathan Polk, Jan Rovny, Marco Steenbergen, Milada Vachudova. 2022. Chapel Hill Expert Survey Trend File, 1999-2019. Electoral Studies 75 (Feb 2022), 102420 https://doi.org/10.1016/j.electstud.2021.102420.

*CHES data can be downloaded from https://www.chesdata.eu/ches-europe

gen CHESDATA=.
gen party_id=.
order party_id, after(CHESDATA)

*Austria	
replace party_id=	1301 if	prtvtat==1|	prtvtaat==1	 //	SPO	aus	
replace party_id=	1302 if	prtvtat==2	| prtvtaat==2 //	OVP	aus	
replace party_id=	1303 if	prtvtaat==3	| prtvtat==3 //	FPO	aus	
replace party_id=	1304 if	prtvtaat==5	| prtvtat==4 //	GRUNE	aus	
replace party_id=	1307 if	prtvtaat==4	 //BZO	aus	in CHES trend, not in 2019

*Belgium 
replace party_id=	102	if	prtvtbe==14	| prtvtabe==13	|	prtvtbbe==13  //	PS	be	
replace party_id=	103	if	prtvtbe==3	| prtvtabe==5	|	prtvtbbe==5	//	SP	be	
replace party_id=	104	if	prtvtbe==11	| prtvtabe==10	|	prtvtbbe==10  //	ECOLO	be	
replace party_id=	105	if	prtvtbe==1	| prtvtabe==1	|	prtvtbbe==1		//	AGALEV/Groen	be	
replace party_id=	106	if	prtvtabe==12 |	prtvtbbe==12  //	MR	be	
replace party_id=	107	if	prtvtbe==5	| prtvtabe==8	|	prtvtbbe==8	//	VLD	be	
replace party_id=	108	if	prtvtbe==12	| prtvtabe==9	|	prtvtbbe==9		//	CDH	be	
replace party_id=	109	if	prtvtabe==2	| prtvtbbe==2		//	CD&V	be	
replace party_id=	110	if	prtvtabe==3	| prtvtbbe==2	 //	NVA	be	
replace party_id=	111	if	prtvtbe==13  //	FDF/DeFI	be, in CHES trend, not in 2019	
replace party_id=	112	if	prtvtbe==8| prtvtabe==7	|	prtvtbbe==7		//	VB	be

*Denmark 2018 released
replace party_id=	201	if	prtvtdk==1	| prtvtadk==1	| prtvtbdk==1	 //	SD	dk	Not in 2016 ESS
replace party_id=	202	if	prtvtdk==2	| prtvtadk==2	| prtvtbdk==2	 //	RV	dk	Not in 2016 ESS
replace party_id=	203	if	prtvtdk==3	| prtvtadk==3	| prtvtbdk==3	 //	KF	dk	Not in 2016 ESS
replace party_id=	206	if	prtvtdk==5	| prtvtadk==5	| prtvtbdk==4	 //	SF	dk	Not in 2016 ESS
replace party_id=	211	if	prtvtdk==8	| prtvtadk==8	| prtvtbdk==7	 //	V	dk	Not in 2016 ESS
**#
replace party_id=	213	if	prtvtdk==10	| prtvtadk==10	| prtvtbdk==9	 //	EL	dk	Not in 2016 ESS
replace party_id=	215	if	prtvtdk==6	| prtvtadk==6	| prtvtbdk==5	 //	DF	dk	Not in 2016 ESS

*Spain
replace party_id=	501	if	prtvtaes==2	|	prtvtbes==2			//	PSOE	es	Y
replace party_id=	502	if	prtvtaes==1	|	prtvtbes==1	//	PP	es	Y
replace party_id=	504	if	prtvtaes==3	|	prtvtbes==3	 //	IU	es	
replace party_id=	505	if	prtvtaes==4	|	prtvtbes==4	//	CiU	es	Dissolved
replace party_id=	506	if	prtvtaes==7	|	prtvtbes==6	 //	PNV	es	Y
replace party_id=	511	if	prtvtaes==5	|	prtvtbes==5	 //	ERC	es	Y
replace party_id=	513	if	prtvtaes==9	|	prtvtbes==7	 //	BNG	es	Not in 2017 CHES/2016 ESS
replace party_id=	523	if	prtvtbes==10  //	UPyD	es	Not in 2017 CHES/2016 ESS

*Finland
replace party_id=	1401 if	prtvtfi==9	|	prtvtafi==8		//	SDP	fin	
replace party_id=	1402 if	prtvtfi==1	|	prtvtafi==1	 //	KOK	fin	
replace party_id=	1403 if	prtvtfi==4	|	prtvtafi==4		//	KESK	fin	
replace party_id=	1404 if	prtvtfi==10	|	prtvtafi==9 	//	VAS	fin	
replace party_id=	1405 if	prtvtfi==5	|	prtvtafi==5		//	PS	fin	
replace party_id=	1406 if	prtvtfi==2	|	prtvtafi==2		//	SFP	fin	
replace party_id=	1408 if	prtvtfi==8	|	prtvtafi==7		//	VIHR	fin	
replace party_id=	1409 if	prtvtfi==6	|	prtvtafi==6		//	KD	fin	

*France
replace party_id=	601	if prtvtfr==9	| prtvtafr==9	| prtvtbfr==7	//	PCF	fr	Not in 2016, but in 2018 ESS
replace party_id=	602	if	prtvtfr==10	| prtvtafr==10	| prtvtbfr==8	 //	PS	fr	Y
replace party_id=	603	if	prtvtbfr==9	 //	PRG	fr	Not in 2017 or 2019 CHES
replace party_id=	605	if	prtvtfr==14	| prtvtafr==14	| prtvtbfr==12	 //	VERTS, EELV	fr	Y
replace party_id=	609	if	prtvtfr==12	| prtvtafr==12	| prtvtbfr==11	 //	UMP/RPR	fr	Y
replace party_id=	610	if	prtvtfr==3	| prtvtafr==3	| prtvtbfr==2	  //	FN	fr	Y
replace party_id=	612	if	prtvtfr==8	| prtvtafr==8	| prtvtbfr==5	 //	MPF	fr	Not in 2017 CHES or 2019
replace party_id=	613	if	prtvtfr==13	| prtvtafr==13	| prtvtbfr==10	  //	UDF/ Modem	fr	Y
replace party_id=	614	if	prtvtfr==5	| prtvtafr==5	| prtvtbfr==4	 //	LO	fr	CHES trend, not in 2017 or 2019 CHES

*Britain
replace party_id=	1101	if	prtvtgb==1	|	prtvtagb==1		//	Conservatives	gb	Y
replace party_id=	1102	if	prtvtgb==2	|	prtvtagb==2		//	Labour	gb	Y
replace party_id=	1104	if	prtvtgb==3	|	prtvtagb==3		//	LibDem	gb	Y
replace party_id=	1105	if	prtvtgb==4	|	prtvtagb==4	 	//	SNP	gb	Y
replace party_id=	1106	if	prtvtgb==5	|	prtvtagb==5		//	Plaid	gb	Y
replace party_id=	1107	if	prtvtgb==6	|	prtvtagb==6		//	Greens	gb	Y
replace party_id=	1108	if	prtvtagb==7		//	UKIP	gb	Y

*Germany -- take the second vote which reflects the PR vote and is more sincere, note switch in party order from esround 7
replace party_id=	301	if	prtvade2==2	|	prtvbde2==2	 //	CDU	ge	Y
replace party_id=	302	if	prtvade2==1	|	prtvbde2==1	 //	SPD	ge	Y
replace party_id=	303	if	prtvade2==4	|	prtvbde2==4	 //	FDP	ge	Y
replace party_id=	304	if	prtvade2==3	|	prtvbde2==3	 //	Grunen	ge	Y
replace party_id=	305 if 	prtvade2==6	|	prtvbde2==6  // Republikaner
replace party_id=	306	if	prtvade2==5	|	prtvbde2==5	 //	LINKE	ge	Y
replace party_id=	309	if	prtvade2==7	|	prtvbde2==7  // NPD


*Greece*
replace party_id=	401	if	prtvtagr==1	| prtvtbgr==1	 //	PASOK	gre	Not in ESS 2016
replace party_id=	402	if	prtvtagr==2	| prtvtbgr==2	//	ND	gre	Not in ESS 2016
replace party_id=	403	if	prtvtagr==4	| prtvtbgr==4	//	SYN/Syriza	gre	Not in ESS 2016
replace party_id=	404	if	prtvtagr==3	| prtvtbgr==3	//	KKE	gre	Not in ESS 2016
replace party_id=	410	if	prtvtagr==6	|	prtvtbgr==5	//	LAOS	gre	Not in ESS 2016
replace party_id=	411	if	prtvtbgr==6	//	OP	gre	Not in ESS 2016


*Ireland
replace party_id=	701	if	prtvtie==1	//	FF	ie	
replace party_id=	702	if	prtvtie==2	//	FG	ie	
replace party_id=	703	if	prtvtie==3	//	Lab	ie	
replace party_id=	705	if	prtvtie==5		//	GP	ie	
replace party_id=	707	if	prtvtie==6		//	SF	ie	


*Netherlands
replace party_id=	1001	if	prtvtanl==1	|	prtvtbnl==1	| prtvtcnl==1	//	CDA	nl	Y
replace party_id=	1002	if	prtvtanl==2	|	prtvtbnl==2	| prtvtcnl==2	//	PvdA	nl	Y
replace party_id=	1003	if	prtvtanl==3	|	prtvtbnl==3	| prtvtcnl==3	//	VVD	nl	Y
replace party_id=	1004	if	prtvtanl==5	|	prtvtbnl==5	| prtvtcnl==5	//	D66	nl	Y
replace party_id=	1005	if	prtvtanl==6	|	prtvtbnl==6	| prtvtcnl==6		//	GL	nl	Y
replace party_id=	1006	if	prtvtanl==10	|	prtvtbnl==10 | prtvtcnl==10	 //	SGP	nl	Y
replace party_id=	1014	if	prtvtanl==7	|	prtvtbnl==7	| prtvtcnl==7	//	SP	nl	Y
replace party_id=	1015	if	prtvtanl==4	|	prtvtbnl==4	| prtvtcnl==4	//	LPF	nl	Y
replace party_id=	1016	if	prtvtanl==8	|	prtvtbnl==8	| prtvtcnl==8	//	CU	nl	Y
replace party_id=	1017	if	prtvtcnl==11  //	PVV	nl	Y
replace party_id=	1018	if	prtvtanl==11 | prtvtcnl==12	 //	PvdD	nl	Y


*norway
replace party_id=	3501	if   prtvtno==3		//	AP	
replace party_id=	3502	if	prtvtno==8	//	FrP	
replace party_id=	3503	if	prtvtno==7	//	Hoyre	
replace party_id=	3504	if	prtvtno==2	//	SV	
replace party_id=	3505	if	prtvtno==6	//	Sp	Centerpartiet 
replace party_id=	3506	if	prtvtno==5	//	KrF	
replace party_id=	3507	if	prtvtno==4	//	V	
replace party_id=	3509 	if 	prtvtno==1 // Red-Green Alliance

*Portugal
replace party_id=	1201	if	prtvtpt==5	| prtvtapt==3		//	CDU	por	Y
replace party_id=	1202	if	prtvtpt==2	|prtvtapt==2			//	CDS/PP	por	Alliance
replace party_id=	1205	if	prtvtpt==10	| prtvtapt==11		//	PS	por	Y
replace party_id=	1206	if	prtvtpt==11	| prtvtapt==10	//	PPD/PSD	por	Alliance
replace party_id=	1208	if	prtvtpt==1	| prtvtapt==1		//	BE	por	Y


*Sweden 2018 
replace party_id=	1601	if	prtvtse==7	//	V	sv	Y
replace party_id=	1602	if	prtvtse==6	 //	SAP	sv	Y
replace party_id=	1603	if	prtvtse==1	//	C	sv	Y
replace party_id=	1604	if	prtvtse==2	//	FP	sv	Y
replace party_id=	1605	if	prtvtse==5	//	M	sv	Y
replace party_id=	1606	if	prtvtse==3	 //	KD	sv	Y
replace party_id=	1607	if	prtvtse==4	 //	MP	sv	Y


*switzerland
replace party_id=	3601	if	prtvtch==4	|	prtvtach==4	|	prtvtbch==4		//	SVP/UDC	swi	Y
replace party_id=	3602	if	prtvtch==3	|	prtvtach==3	|	prtvtbch==3		//	SP/PS	swi	Y
replace party_id=	3603	if prtvtch==1	|	prtvtach==1	|	prtvtbch==1	//	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace party_id=	3604	if	prtvtch==2	|	prtvtach==2	|	prtvtbch==2	//	CVP/PVC	swi	Y
replace party_id=	3605	if	prtvtch==10	|	prtvtach==10	|	prtvtbch==8		//	GPS/PES	swi	Y
replace party_id=	3606	if	prtvtbch==9		//	GLP/PVL	swi	Y
replace party_id=	3607	if	prtvtch==7	|	prtvtach==7	|	prtvtbch==12		//	EVP/PEV	swi	Y
replace party_id=	3608 	if  prtvtch==12| prtvtach==12| prtvtbch==11 // Federal democratic Union
replace party_id=	3609 	if prtvtch==9 | prtvtach==9 | prtvtbch==7 // Swiss Labour Party
replace party_id=	3611	if prtvtch==8| prtvtach==8| prtvtbch==6    // CSU Christian-Social Union
replace party_id=   3650	if  prtvtch==5| prtvtach==5| prtvtbch==5 // Liberal party


save "ESS2to4.dta", replace


*************************************************************************
** b) Create year variable in ESS to match up with CHES year variable**
**************************************

gen year=2002 if essround==1
label variable year "ches year"  // 2002
replace year=2002 if essround==2 // 2004
replace year=2006 if essround==3 // 2006
replace year=2006 if essround==4 // 2008
replace year=2010 if essround==5 // 2010
replace year=2010 if essround==6 // 2012
replace year=2014 if essround==7 // 2014
replace year=2014 if essround==8 // 2016
replace year=2019 if essround==9 // 2018

*fine-grained matching for missing values in CHES dataset by using adjacent waves*
replace year=2010 if (essround==1| essround==2|essround==3| essround==4) & (cntry=="CH"| cntry=="NO") 


*party-specific additions matching parties in ESS with correct CHES wave that has information
replace year=2006 if (essround==2) & cntry=="DK" & party_id==213  // Enhedslisten DK
replace year=1999 if cntry=="FR" & party_id==614 // LO-LCR France
replace year=1999 if cntry=="DE" & party_id==305 // Republikaner
replace year=1999 if cntry=="DE" & party_id==309 & essround<5 // DVP
replace year=2006 if cntry=="FR" & party_id==612
replace year=2006 if cntry=="GR" & party_id==410 & essround==2 // LAOS
replace year=2010 if cntry=="GR" & party_id==411 & essround==4 // OP
replace year=2010 if cntry=="ES" & party_id==523 & essround==4 // UPyD
replace year=2002 if cntry=="NL" & party_id==1006 & essround==3 // SGP
replace year=2010 if cntry=="NL" & party_id==1006 & essround==4 // SGP
replace year=2010 if cntry=="NL" & party_id==1018 & essround==2 // PvdD
replace year=2002 if cntry=="NL" & party_id==1015 & (essround==3| essround==4) // LPF
replace year=2010 if cntry=="NL" & party_id==1018 & essround==4 // PvdD
replace year=2006 if cntry=="FI" & party_id==1405 & (essround==2) // True Finns
replace year=2006 if cntry=="PT" & party_id==1208 & essround==2 // Left Bloc portugal
replace year=2006 if cntry=="ES" & party_id==511 & essround==2 // ERC Spain
replace year=2006 if cntry=="GB" & party_id==1107 & essround==2 // Greens UKIP

/* [define year below so that it connects with CHES years]*/
gen essyear=.
label variable essyear "year in ess"
replace essyear=2002 if essround==1
replace essyear=2004 if essround==2
replace essyear=2006 if essround==3
replace essyear=2008 if essround==4
replace essyear=2010 if essround==5
replace essyear=2012 if essround==6
replace essyear=2014 if essround==7
replace essyear=2016 if essround==8
replace essyear=2018 if essround==9
*/


************************************************************************************************************************
** c) Merge with CHES data and party family data ** 
*************************************************************************************************************

merge m:1 party_id year using "1999_2019_dataset_meansv3.dta", keepusing(party family lrgen lrecon galtan)
tab _merge
drop if _merge==2
tab cntry essround if _merge==1
tab cntry essround if _merge==3

*fix some contested party codings in CHES 1999_2019_dataset_meansv3
replace family=2 if party_id==1402 // Finnish KOK is conservative, not liberal
replace family=3 if party_id==1403 // Finnish KESK is more liberal than agrarain by early 2000s
replace family=3 if party_id==523 // Spanish UPyD is liberal 
replace family=4 if party_id==110 // NVA is for the purposes here christian-democratic
replace family=4 if party_id==1409 // KD in Finland was sometimes confessional or CD -- make consistent  CD
replace family=7 if party_id==206 // Sosialisk F in DK (despite the name) is a green party -- miscoded in CHES
replace family=6 if party_id==707 // Sinn Fein is regionalist, but also rad left -- code here rad left

**now manually allocate values to Norwegian and Swiss parties using CHES data from on 1999 as basis**
**NORWAY
replace party="AP" if party_id== 3501		//	AP	
replace party="FrP" if party_id==3502	//	FrP	
replace party="Hoyre" if party_id==3503	//	Hoyre	
replace party="SV" if party_id==3504		//	SV	
replace party="Sp" if party_id==3505		//	Sp	Centerpartiet 
replace party="KrF" if party_id==3506	//	KrF	
replace party="V" if party_id==3507		//	V
replace party="RA" if party_id==3509  // Red-Green Electoral Alliance	 

replace lrgen=3.6666667 if party_id==3501		//	AP	
replace lrgen=8.2222223 if party_id==3502	//	FrP	
replace lrgen=7.4444447 if party_id==3503	//	Hoyre	
replace lrgen=1.7777778 if party_id==3504		//	SV	
replace lrgen=3.8888888 if party_id==3505		//	Sp	Centerpartiet 
replace lrgen=5.2222223 if party_id==3506	//	KrF	
replace lrgen=5.4444447 if party_id==3507	// V	
replace lrgen=.5 if party_id==3509  // Red-Green Electoral Alliance	 

replace galtan=4.5999999 if party_id==3501		//	AP	
replace galtan=7.3000002 if party_id==3502	//	FrP	
replace galtan=5.3000002 if party_id==3503	//	Hoyre	
replace galtan=1.7777778 if party_id==3504		//	SV	
replace galtan=6.0999999 if party_id==3505		//	Sp	Centerpartiet 
replace galtan=8.1999998 if party_id==3506	//	KrF	
replace galtan=3.7 if party_id==3507	// V	
replace galtan=1.4 if party_id==3509 // Red-Green

replace lrecon=3.7 if party_id==3501		//	AP	
replace lrecon=7.6999998 if party_id==3502	//	FrP	
replace lrecon=7.8000002 if party_id==3503	//	Hoyre	
replace lrecon=1.7 if party_id==3504		//	SV	
replace lrecon=3.5999999 if party_id==3505		//	Sp	Centerpartiet 
replace lrecon=5.1999998 if party_id==3506	//	KrF	
replace lrecon=5.8000002 if party_id==3507	// V	
replace lrecon=.4 if party_id==3509	// Red-Green

replace family=5 if party_id==3501		//	AP	
replace family=1 if party_id==3502	//	FrP	
replace family=2 if party_id==3503	//	Hoyre	
replace family=6 if party_id==3504		//	SV	
replace family=11 if party_id==3505		//	Sp	Centerpartiet 
replace family=4 if party_id==3506	//	KrF	
replace family=3 if party_id==3507	// V	
replace family=6 if party_id==3509

**SWITZERLAND
replace party="SVP/UDC" if party_id==3601   //	SVP/UDC	swi	Y
replace party="SP/PS" if party_id==3602 	//	SP/PS	swi	Y
replace party="FDP/PLR" if party_id==3603   //	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace party="CPV/PVC" if party_id==3604	//	CVP/PVC	swi	Y
replace party="GPS/PES" if party_id==3605	//	GPS/PES	swi	Y
replace party="GLP/PVL" if party_id==3606	//	GLP/PVL	swi	Y
replace party="EVP/PEV" if party_id==3607    //	EVP/PEV	swi	Y
replace party="EDU/UDF" if party_id==3608   // Federal democratic Union
replace party="PdA/PST-POP" if party_id==3609   // Swiss Labour Party
replace party="CSU Sw" if party_id==3611   // CSU Christian-Social Union
replace party="LPS/PLS" if party_id==3650  // LP Liberal party

replace family=1 if party_id==3601   //	SVP/UDC	swi	Y
replace family=5 if party_id==3602 	//	SP/PS	swi	Y
replace family=3 if party_id==3603   //	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace family=4 if party_id==3604	//	CVP/PVC	swi	Y
replace family=7 if party_id==3605	//	GPS/PES	swi	Y
replace family=7 if party_id==3606	//	GLP/PVL	swi	Y
replace family=4 if party_id==3607    //	EVP/PEV	swi	Y
replace family=10 if party_id==3608   // Federal democratic Union
replace family=6 if party_id==3609   // Swiss Labour Party
replace family=4 if party_id==3611   // CSU Christian-Social Union
replace family=3 if party_id==3650  // LPS/PLS Liberal part

replace lrgen=9.083333 if party_id==3601   //	SVP/UDC	swi	Y
replace lrgen=2.1666667 if party_id==3602 	//	SP/PS	swi	Y
replace lrgen=7.1666665 if party_id==3603   //	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace lrgen=6.1666665 if party_id==3604	//	CVP/PVC	swi	Y
replace lrgen=1.9166666 if party_id==3605	//	GPS/PES	swi	Y
replace lrgen=5.3333335 if party_id==3606	//	GLP/PVL	swi	Y
replace lrgen=5.8000002 if party_id==3607    //	EVP/PEV	swi	Y
replace lrgen=8.636364 if party_id==3608   // Federal democratic Union
replace lrgen=.58333331 if party_id==3609   // Swiss Labour Party
replace lrgen=4.909091 if party_id==3611   // CSU Christian-Social Union
replace lrgen=. if party_id==3650  // LPS Liberal party -- ideology scores not known

replace galtan=9.083333 if party_id==3601   //	SVP/UDC	swi	Y
replace galtan=1.6666666 if party_id==3602 	//	SP/PS	swi	Y
replace galtan=5.3333335 if party_id==3603   //	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace galtan=6.1666665 if party_id==3604	//	CVP/PVC	swi	Y
replace galtan=1.1666666 if party_id==3605	//	GPS/PES	swi	Y
replace galtan=2.7272727 if party_id==3606	//	GLP/PVL	swi	Y
replace galtan=6.4000001 if party_id==3607    //	EVP/PEV	swi	Y
replace galtan=8.3999996 if party_id==3608   // Federal democratic Union
replace galtan=2.1428571 if party_id==3609   // Swiss Labour Party
replace galtan=5.4000001 if party_id==3611   // CSU Christian-Social Union
replace galtan=. if party_id==3650  // LPS Liberal party

replace lrecon=7.75 if party_id==3601   //	SVP/UDC	swi	Y
replace lrecon=1.75 if party_id==3602 	//	SP/PS	swi	Y
replace lrecon=8.166667 if party_id==3603   //	FDP/PLR	Radicals and Liberal Party until 2008 swi		
replace lrecon=5.9166665 if party_id==3604	//	CVP/PVC	swi	Y
replace lrecon=1.5833334 if party_id==3605	//	GPS/PES	swi	Y
replace lrecon=6.181818 if party_id==3606	//	GLP/PVL	swi	Y
replace lrecon=5.625 if party_id==3607    //	EVP/PEV	swi	Y
replace lrecon=6.25 if party_id==3608   // Federal democratic Union
replace lrecon=.44444445 if party_id==3609   // Swiss Labour Party
replace lrecon=4.3636365 if party_id==3611   // CSU Christian-Social Union
replace lrecon=. if party_id==3650  // LPS Liberal party

**************
tab cntry family

********
***d) Now create a GAL bloc by hiving off social-liberals and new left from their traditional families (liberal and radical left) -- **GAL bloc**
*We expand our "Green" category to include social-liberal and new left parties. We use the criteria that we developed with Jan Rovny in Paris (2019), which employ CHES estimates on the galtan dimension. The notion is that these parties are radical-GAL alongside their specific economic location.

***************************** 

/*Decision rule: socialliberal parties = liberal party family (3) AND galtan<2.5 averaged over 2002, 2006, 2010 CHES estimates for party || new left parties = radical left party family (6) AND galtan<2.5 averaged over 2002, 2006, 2010

*for Norway and Switzerland use "ches2019v3.dta" for parties that were in existence in 2004-2008*	   
	   SV		 3504		2.1     2019   rad left // include
	   RV		 3509 		1.4		2019   rad left // include */
	   

bys party_id: egen galtan_p=mean(galtan) if year==2002| year==2006| year==2010
replace galtan_p=2.1 if party_id==3504
replace galtan_p=1.4 if party_id==3509
label variable galtan_p "average galtan position 2002-2010" 
 
tabstat galtan_p if (family==3|family==6) & essround>1 & essround<5, stats (mean N) by (party_id)	   

gen family2=family
*GAL parties from the lib families [based on Paris work]
replace family2=12 if family==3 & galtan_p<2.5 

***New left
replace family2=13 if family==6 & galtan_p<2.5 

label variable family2 "party family with social-libs and new left separate"

label define family2 1 "tan" 2"conservative" 3 "economic liberal" 4 "christ-dem" 5 "socialist" 6 "traditional left" 7 "green" 8 "regionalist/ethnic" 9 "no family" 10 "confessional" 11 "agrarian" 12 "social liberal" 13 "new left"
label values family2 family2
tab family2
tab family family2 
tab cntry family2 

tab family2, missing // adds social-liberals, new left, shifts some liberal CEE parties to ethnic category
tab family2 family, missing


**********************************************
*********************************************
**4. create key social characteristics variables 
***********************************
gen SOCIAL_CHARS=.


*gender
gen female=0 if gndr==2
replace female=0 if gndr==1
replace female=1 if gndr==2
label define female 0 "male" 1 "female", replace
label values female female
label variable female "female=1"
order female, after (SOCIAL_CHARS)

*country
gen country=13 if cntry=="AT"
replace country=1 if cntry=="BE"
replace country=20 if cntry=="BG"
replace country=36 if cntry=="CH"
replace country=40 if cntry=="CY"
replace country=3 if cntry=="DE"
replace country=2 if cntry=="DK"
replace country=22 if cntry=="EE"
replace country=5 if cntry=="ES"
replace country=14 if cntry=="FI"
replace country=6 if cntry=="FR"
replace country=11 if cntry=="GB"
replace country=4 if cntry=="GR"
replace country=31 if cntry=="HR"
replace country=23 if cntry=="HU"
replace country=7 if cntry=="IE"
replace country=8 if cntry=="IT"
replace country=25 if cntry=="LT"
replace country=24 if cntry=="LV"
replace country=10 if cntry=="NL"
replace country=35 if cntry=="NO"
replace country=26 if cntry=="PL"
replace country=12 if cntry=="PT"
replace country=16 if cntry=="SE"
replace country=29 if cntry=="SI"
replace country=28 if cntry=="SK"
tab cntry country
order country, after (SOCIAL_CHARS)
label variable country "CHES code for country"

label define country 1 "Belgium" 2 "Denmark" 3 "Germany" 4"Greece" 5 "Spain" 6 "France" 7 "Ireland" 8 "Italy" 10 "Netherlands" 11 "Great Britain" 12 "Portugal" 13 "Austria" 14 "Finland" 16 "Sweden" 20 "Bulgaria" 21 "Czech Republic" 22 "Estonia" 23 "Hungary" 24"Latvia" 25 "Lithuania" 26 "Poland" 28 "Slovakia" 29 "Slovenia" 31 "Croatia" 35 "Norway"36 "Switzerland"  38 "Luxembourg" 40 "Cyprus", replace
label values country country
tab country 

*rural
* creating urban - rural 
tab domicil
gen rural=domicil
label define rural 1 "big city" 2 "suburbs" 3 "town" 4 "country village" 5 "farm/countryside"
label value rural rural

*age
drop age
gen age=agea
recode age (.a = .)

*education
gen higher=1 if (edulvla==5| edulvla==4) & edulvla!=. // 1=university or other post-secondary, 0=other
replace higher=0 if edulvla<4 & edulvla!=.
label variable higher "1=post-secondary (university or higher)" 

gen educ5=edulvla if edulvla!=.
label define educ5 1 "<lower secondary" 2 "lower secondary completed" 3 "upper secondary completed" 4 "post-secondary non-tertiary completed" 5 "tertiary completed"
label values educ5 educ5
label variable educ5 "level of education"
tab educ5

*religion 
gen secular=rlgatnd
label variable secular "secularism (from high to low attendance of service)"
label define secular 1 "every day" 2 "more than once a week" 3 "once a week" 4 "at least once a month" 5 "special holy days" 6 "less often" 7 "never"
label value secular secular
tab secular

*income
gen incomesubj=5-hincfel if hincfel!=.
label define incomesubj 1 "very difficult on present income" 2 "difficult on present income" 3 "coping on present income" 4 "living comfortably in present income", replace
label values incomesubj incomesubj
label variable incomesubj "feeling about household income nowadays"
tab incomesubj 

**essround 2 and 3 use a non-decile measure -- align with essround4 making use of ESS documentation in round 4
tab hinctnta country
tab hinctnt country
gen incomeobj=hinctnt if hinctnt!=.
replace incomeobj=10 if hinctnt==11| hinctnt==12
replace incomeobj=hinctnta if essround==4  
tab incomeobj country
tab incomeobj essround
label variable incomeobj "objective income (deciles)" // use objective
recode incomeobj (.a=.) (.b=.) (.c=.)


**********************************************
**5.SELECT
****CREATE SELECT VARIABLE FOR ALL COUNTRIES WITH GAL PARTY/ TAN PARTY IN WAVES 2-4*/
********************************************************************************
gen SELECTVARS=.
*vote
gen vote2=vote
label variable vote2 "voted in last national election=1"
recode vote2 (.a .b .c =.) (2 3 = 0)
tab vote vote2, missing
order vote2, after (SELECTVARS)

gen select=1 if age>20 & family!=. & (essround==2|essround==3|essround==4) & (cntry=="AT"| cntry=="BE"| cntry=="DK"| cntry=="DE"|cntry=="GR"| cntry=="ES"| cntry=="FR"| cntry=="IE"| cntry=="PT"| cntry=="FI"|cntry=="NL"| cntry=="NO"| cntry=="SE"| cntry=="CH"| cntry=="GB")
replace select=0 if select==. & (essround==2|essround==3|essround==4)
label variable select "waves 2-4 -- western countries meeting criteria"

gen selectgal=1 if select==1 
tab cntry selectgal

gen selecttan=1 if select==1 & cntry!="IE" & cntry!="SE" & cntry!="IE" & cntry!="PT" & cntry!="ES" 
tab cntry selecttan if selecttan==1
label variable selectgal "waves 2-4 -- western countries with GAL party"
label variable selecttan "waves 2-4 -- western countries with TAN party"

*************************************************
***7. FIELD VARIABLES
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

replace fieldculture=1 if educ5==1 & edutype!=. 
replace fieldecon=1 if educ5==1 & edutype!=. 
replace fieldcomm=1 if educ5==1 & edutype!=. 
replace fieldtech=1 if educ5==1  & edutype!=. 

order fieldculture fieldecon fieldcomm fieldtech, after ( edutype)

/*CHIEF INDEPENDENT VARIABLE*/
gen fieldratio=(fieldculture+fieldcomm)/(fieldculture+fieldecon+fieldcomm+fieldtech) 
label variable fieldratio "RATIO of cultcomm to cultcomm+econtech"

tab fieldratio

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

***********************************************************************************************************


*B) OCCUPATIONAL CECT
/*create occfield: a variable that summarizes fieldratio at the isco3 level*/
egen occfield=mean(fieldratio) if select==1 & isco3tr!=., by (isco3tr)
label variable occfield "occupational field in all 15 countries"
egen occfieldtan=mean(fieldratio) if selecttan==1 & isco3tr!=., by (isco3tr)
label variable occfieldtan "occupational field 11 countries w TAN party"
gen occfieldgal=occfield
label variable occfieldgal "occupational field in all 15 countries w GAL party"
tab occfield
tab occfieldtan
tab occfieldgal

****other field variables ***

egen fieldincome=mean(incomeobj) if select==1 & edutype!=. & educ5<6, by (edutype educ5)
label variable fieldincome "field income -field x level all countries"
gen fieldincomegal=fieldincome
label variable fieldincomegal "field income - field x level with GAL party"
egen fieldincometan=mean(incomeobj) if selecttan==1 & edutype!=. & educ5<6 , by (edutype educ5)
label variable fieldincometan "field income -field x level with TAN party"
tab fieldincome
tab fieldincometan


*****************
**7. DEPENDENT VARIABLE: VOTING GAL OR TAN**
******************
gen DEPENDENTVAR=.
gen TAN=1 if family==1 & selecttan==1 
replace TAN=0 if family!=1 & family!=.& selecttan==1
label variable TAN "voted TAN"
tab TAN family, missing
tab family TAN, column

gen GAL=1 if family==7 & select==1
replace GAL=1 if (family2==12| family2==13) & family2!=. & select==1
replace GAL=0 if GAL==. 
label variable GAL "voted for GAL party (green+sociallib+newleft)"
tab GAL family2, missing
tab family2 GAL, column row

save "ESS2to4.dta", replace






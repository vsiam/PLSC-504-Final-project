
*************** THIS CODE ILLUSTRATES HOW THE CECT SCORES CAN BE ADDED TO THE SOEP DATASET. IT IS BASED ON SOEP VERSION V35. 
**note: a) For original scores for cultural, economic, communicative, technical skills by field of education, see: Van de Werfhorst, Herman G. and Gerbert Kraaykamp. 2001. Four field-related educational resources and their impact on labor, consumption, and sociopolitical orientation. Sociology of Education 74(4): 296-317;
*b) for an explanation of the four-resource schema and its application, see "Supplementary documentation" for this article on HARVARD DATAVERSE 


***************COUNTING INFORMATION ON FIELD****************************
gen traina=1 if pgtraina>0 & pgtraina<10000 & pgisced97>1 & pgisced97!=.
replace traina=0 if traina==. & pgisced97>2 & pgisced97!=.
gen trainb=1 if pgtrainb>0 & pgtrainb<10000 & pgisced97>2 & pgisced97!=.
replace trainb=0 if trainb==. & pgisced97>2 & pgisced97!=.
gen trainc=1 if pgtrainc>0 & pgtrainc<10000 & pgisced97>2 & pgisced97!=.
replace trainc=0 if trainc==. & pgisced97>2 & pgisced97!=.
gen traind=1 if pgtraind>0 & pgtraina<10000 & pgisced97>2 & pgisced97!=.
replace traind=0 if traind==. & pgisced97>2 & pgisced97!=.
gen train=1 if (pgtraina>0 & pgtraina<10000|pgtrainb>0 & pgtrainb<10000 | pgtrainc>0 & pgtrainc<10000| pgtraind>0 & pgtraina<10000) & pgisced97>1 & pgisced97!=.
replace train=0 if train==. & pgisced97>1 & pgisced97!=.
label variable traina "dummy training part-inschool/part-onjob"
label variable trainb "dummy training inschool"
label variable trainc "dummy training higher vocational"
label variable traind "dummy training civil service"
label variable train "dummy individual has received vocational training"






********************************************
**ASSIGNING EDUFIELD, AND THEN CECT, TO VOCATIONAL TRAINING
********************************************
/*(A) VOCATIONAL TRAINING*/
/*we create a variable that combines info from the four training variables, with order of priority -- civil service, then higher vocational training, then full school vocational, then part-time school vocational*/
gen pgtrain=pgtraind if pgisced97>1 & pgisced97!=. & pgtraind>0
replace pgtrain=pgtrainc if pgtrain==. & pgisced97>1 & pgisced97!=. & pgtrainc>0
replace pgtrain=pgtrainb if pgtrain==. & pgisced97>1 & pgisced97!=. & pgtrainb>0
replace pgtrain=pgtraina if pgtrain==. & pgisced97>1 & pgisced97!=. & pgtraina>0


*collapse (mean) green greenext (count) N=green if pgisced97>1 & pgisced97!=., by (pgtrain)
*tabstat green greenext if pgisced97>1 & pgisced97!=. & pgtrain>0 & pgtrain!=., stats (mean N)
*(a) 
gen edufield=1 if pgtrain==6860| pgtrain>=7800 & pgtrain<=7823| pgtrain>=7825 & pgtrain <=7840| pgtrain>=7880 & pgtrain<=7890| pgtrain==8564| pgtrain>=9310 & pgtrain<=9971
 replace edufield=2 if pgtrain>=3050 & pgtrain<=3054 |pgtrain>=8238 & pgtrain<=8391| pgtrain==8824| pgtrain==8826 
 replace edufield=3 if pgtrain==6740| pgtrain==6742| pgtrain==7824| pgtrain==7894| pgtrain>=8220 & pgtrain<=8234| pgtrain>=8591 & pgtrain<=8599 | pgtrain>=8910 & pgtrain<=8942
 replace edufield=4 if pgtrain>=1010 & pgtrain<=3022 | pgtrain>=3080 & pgtrain<=3107 | pgtrain==3114| pgtrain>=3130 & pgtrain<=3139| pgtrain==3153| pgtrain==3161| pgtrain>=3181 & pgtrain<=5315| pgtrain>=5498 & pgtrain<=6221| pgtrain==6224 | pgtrain>=6228 & pgtrain<=6278| pgtrain>=6291 & pgtrain<=6292|pgtrain>=6410 & pgtrain<=6519| pgtrain==8041| pgtrain==8042
 replace edufield=5 if pgtrain>=110 & pgtrain<=806| pgtrain==8431| pgtrain==8563| pgtrain==8573
 replace edufield=6 if pgtrain>=6520 & pgtrain<=6522| pgtrain>=8630 & pgtrain<=8639| pgtrain>=8701 & pgtrain<=8796
 replace edufield=7 if pgtrain>=3110 & pgtrain<=3112| pgtrain>=3120 & pgtrain<=3122 | pgtrain>=3124 & pgtrain<=3125| pgtrain==3160| pgtrain>=3162 & pgtrain<=3165| pgtrain>=3170 & pgtrain<=3172| pgtrain==6223| pgtrain>=6293 & pgtrain<=6345| pgtrain>=7741 & pgtrain<=7792| pgtrain>=8831 & pgtrain<=8835   
 replace edufield=8 if pgtrain>=3031 & pgtrain<=3041| pgtrain>=3071 & pgtrain<=3072| pgtrain==6751| pgtrain==6851| pgtrain==6893| pgtrain>=8050 & pgtrain<=8055| pgtrain>=8410 & pgtrain<=8420| pgtrain>=8441 & pgtrain<=8562| pgtrain>=8570 & pgtrain<=8572| pgtrain>=8576 & pgtrain<=8580| pgtrain>=8640 & pgtrain<=8670
 replace edufield=9 if pgtrain>=6281 & pgtrain<=6283| pgtrain>=6600 & pgtrain<=6739| pgtrain>=6762 & pgtrain<=6831| pgtrain==6871| pgtrain>=6910 & pgtrain<=6959| pgtrain>=7030 & pgtrain<=7059| pgtrain>=7502 & pgtrain<=7572| pgtrain>=7711 & pgtrain<=7730| pgtrain>=7851 & pgtrain<=7858| pgtrain>=8810 & pgtrain<=8815
 replace edufield=10 if pgtrain==8214| pgtrain==8215| pgtrain>=8610 & pgtrain<=8620| pgtrain>=8682 & pgtrain<=8697| pgtrain>=8839 & pgtrain<=8869
 replace edufield=11 if pgtrain>=7632 & pgtrain<=7659| pgtrain>=7861 & pgtrain<=7878| pgtrain>=8111 & pgtrain<=8135
 replace edufield=12 if pgtrain>=7940 & pgtrain<=7964| pgtrain==8838| pgtrain>=9010 & pgtrain<=9237
 replace edufield=13 if pgtrain==7844| pgtrain >=7910 & pgtrain<=7920| pgtrain>=8011 & pgtrain<=8039| pgtrain==8141| pgtrain==8142 
 replace edufield=14 if pgtrain==3123| pgtrain==3129| pgtrain==3151| pgtrain==3167| pgtrain>=5400 & pgtrain<=5495| pgtrain==6222| pgtrain==6225| pgtrain>=7010 & pgtrain<=7029| pgtrain>=7111 & pgtrain<=7448 
 
rename edufield edufielda 
label variable edufielda "edufield for vocational trainees" 

/* (B) POSTSECONDARY/TERTIARY EDUCATION*/

/*Take pgfield which gives completed specialization for those with tertiary education, and add info on teacher training from separate variable pgdegree*/




gen pgdegreeteach=0 if pgdegree>0 & pgdegree!=. 
replace pgdegreeteach=1 if pgdegree>30 & pgdegree<40 // dummy for tertiary trained with teacher degree [omit few people with isced<6]
label variable pgdegreeteach "dummy for postsecond with teacher degree"

gen pgfieldteach=pgfield 
replace pgfieldteach=600 if pgdegreeteach==1  // this takes teachers into account
label variable pgfieldteach "postsecondary field (cat for teaching"
label values pgfieldteach pgfield



gen edufieldb=1 if pgfieldteach==83
replace edufieldb=2 if pgfieldteach==66| pgfieldteach==67| pgfieldteach>=74 & pgfieldteach<=78
replace edufieldb=3 if pgfieldteach>=1 & pgfieldteach<=14 
replace edufieldb=4 if pgfieldteach==31| pgfieldteach==61| pgfieldteach==63| pgfieldteach==64| pgfieldteach==68| pgfieldteach==69
replace edufieldb=5 if pgfieldteach==51| pgfieldteach==58| pgfieldteach==62
replace edufieldb=6 if pgfieldteach==600
replace edufieldb=7 if pgfieldteach>=36 & pgfieldteach<=40| pgfieldteach>=42 & pgfieldteach<=44
replace edufieldb=8 if pgfieldteach==41| pgfieldteach>=48 & pgfieldteach<=50
replace edufieldb=9 if pgfieldteach==30
replace edufieldb=10 if pgfieldteach>=15 & pgfieldteach<=17| pgfieldteach>=22 & pgfieldteach<=27| pgfieldteach==57| pgfieldteach==59
replace edufieldb=11 if pgfieldteach==28| pgfieldteach==29
replace edufieldb=12 if pgfieldteach==60
replace edufieldb=14 if pgfieldteach==65

label variable edufieldb "edufield for postsec degree holders (teaching assigned)"

label define edufield 1 "gen education" 2 "arts" 3 "humanities" 4 "technical & engineering" 5 "agriculture, forestry" 6 "teacher training" 7 "science/math/computing" 8 "medical, health, nursing" 9 "economics, business" 10 "social studies, media, culture" 11 "law, legal services" 12 "personal care" 13 "public order and safety" 14 "transport and telecom" 15 "primary educ" , replace

label values edufielda edufield
label values edufieldb edufield

/* (C) PRIMARY EDUCATION*/


gen edufieldc=15 if pgisced97<3 & pgisced97>0 // this excludes those currently in school
label variable edufieldc "edufield for primary ed"

/*NOW MERGE INTO SINGLE VARIABLE EDUTYPE*/
gen edutype=edufieldb
replace edutype=edufielda if edutype==.
replace edutype=edufieldc if edutype==.
label values edutype edufield
label variable edutype "educational field all"

/*NOW CREATE FIELD COMPONENT VARIABLES WITH VD WERFHORST SCORES*/
gen FIELD_CHAR=.

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
replace fieldculture=1 if edutype==15 // primary educated

label variable fieldculture "cultural skills (Werfhorst)"

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
replace fieldecon=1 if edutype==15

label variable fieldecon "economic skills (Werfhorst)"

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
replace fieldcomm=1 if edutype==15 // primary educated

label variable fieldcomm "communicative skills (Werfhorst)"

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
replace fieldtech=1 if edutype==15 // primary educated

label variable fieldtech "technical skills (Werfhorst)"

gen fieldratio=(fieldcomm+fieldculture)/(fieldcomm+fieldculture+fieldtech+fieldecon)
label variable fieldratio "ratio of cultcomm to cultcomm+econtech (werfhorst)"
gen fieldratio1=(fieldratio-0.4065934)*3.863275586377627 // 3.86=1/(0.6654412-0.4065934)
replace fieldratio1=0 if fieldratio1<0.035
label variable fieldratio1 "individual CECT (0-1)"



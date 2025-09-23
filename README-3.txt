The reproduction files are organized according to the dataset: 'ESS and USA', or 'SOEP and LISS'. The latter two are featured in the section of the paper entitled 'Where Do Differences Between Fields Come From?'. The ESS dataset is utilized throughout the remainder of the paper; the USA dataset is used in a footnote in the paper with supportive analyses in the Online Appendix E. 

Additionally, we have provided separate code to integrate the CECT scores with the ESS and SOEP datasets, facilitating the use of CECT for other researchers. This code can be found in the 'CECT scores' folder. To incorporate the ESS into the LISS, please refer to the provided code that outlines the creation of the LISS datasets.

The rest of this readme file is organized by dataset. The sections of the readme file refer to the folders in the reproduction files folder. 




****************************
SOEP AND LISS
****************************
*To reproduce the results from the SOEP:*
- The file '04_SOEP_analysis' reproduces all the figures and regression results. However, it is not allowed to publically post the SOEP data online. In order to reproduce the SOEP results, one thus first needs to get access to the SOEP data and run the cleaning scripts (numbered 0 to 3). Data are available from the German Socio-economic Panel Study (SOEP) only due to third party restrictions (for requests, please contact soepmail@diw.de). The scientific use file of the SOEP with anonymous microdata is made available free of charge to universities and research institutes for research and teaching purposes. The direct use of SOEP data is subject to the strict provisions of German data protection law. Therefore, signing a data distribution contract is a precondition for working with SOEP data. The data distribution contract can be requested with a form, available at: http://www.diw.de/soepforms. For further information, contact the SOEPhotline at either soepmail@diw.de or +49-30-89789-292.


*To reproduce the results from the LISS:*
- Run the file '02_LISS_field_analysis'. The other LISS files are the cleaning files. The original data can not be posted online so those files can not be run. They are added for clarity reasons. 


*Other notes:*
-In both SOEP and LISS 'analysis' code, the table and figure numbers are added to the R-code. All tables and figures are named based on their number in the paper. Both R code files should be run from the R-project (so that the file directories do not have to be changed manually). For the SOEP this only works after the SOEP dataset has been created using the other code files. 


* What is created where for the SOEP and LISS *
The code files can be found in the 'SOEP and LISS' folder.
Tab/Figure	Code file 		  Line in code		

Table 1:	04_SOEP_analysis	  722
Table A10:	02_LISS_field_analysis    572
Table A11:	04_SOEP_analysis	  506
Table A12:	04_SOEP_analysis	  528
Table A13:	02_LISS_field_analysis    827
Table A14:	04_SOEP_analysis          908
Table A15:	04_SOEP_analysis          596
Table A16:	04_SOEP_analysis          641
Table A17:	04_SOEP_analysis          824
Table A18:	02_LISS_field_analysis    869

Figure 6:	02_LISS_field_analysis    612, 646
Figure 6 (top): 04_SOEP_analysis	  495
Figure 7      : 04_SOEP_analysis	  750
Figure 8:	02_LISS_field_analysis	  754, 817
Figure 8 (top): 04_SOEP_analysis	  903
Figure A2:      04_SOEP_analysis	  784
Figure A3:      04_SOEP_analysis	  691






****************************
ESS AND USA
****************************
This section contains the datasets and do files that use ESS data (or for Appendix E, a USA survey). It pertains to the 'ESS and USA' folder, which contains information for the ESS analysis in the first parts of the main paper, the appendix (A-E), and in the Supplementary (Additional) documentation available on Harvard Dataverse.

a) To create the dataset for the ESS analysis, use "ESS prep_march 2024.do" 
input: "ESSsubset.dta"
output: "ESS2to4.dta"
ancillary: "1999-2019_CHES_dataset_meansv3.dta"

b) To run the analyses in the main text and the online Appendix (A-E), use "ESS APSR main and appendix.do" 
input: "ESS2to4.dta"
output: "ESSmain.dta"

c) To run the replication of the skills measure using USA survey data, use "APPENDIX_E_APSR 2024.do" 
input & output: "US survey APSR.dta"

d) To run the analyses for the Additional documentation on Dataverse use "ESS Additional.do" 
input: "ESS2to4.dta"
output: "ESS_Add.dta"

e) To integrate the CECT scores with the ESS dataset, use "ESS_CECT.do" and "ESSsubset.dta" 



* What is created where for the ESS and USA *
The code files can be found in the 'ESS and USA' folder.

MAIN TEXT: 
Source do file: ESS APSR main and appendix.do
Source dataset: ESS2to4.dta
Figure 1: Distribution of CECT in the ESS - line 51 to 57
Figure 2: Field of education and voting GAL or TAN -- line 88 to 112
Figure 3: Field, occupation and voting GAL or TAN -- line 151 to 176
Figure 4: The effect of field of education among higher and lower educated -- line 211 to 234
Figure 5: The effect of field on GAL and TAN voting with or without controlling for gender -- line 286 to 330

APPENDIX:
Source do file: ESS APSR main and appendix.do
Source dataset: ESS2to4.dta 
Table A.1: Country coverage -- line 341-342
Table A.3: Baseline model -- line 351 to 368
Table A.4: Field of education and occupation -- line 376 to 394
Table A.5: Testing the effect of field among higher and lower educated -- line 402 to 419
Table A.6: Testing how gender and field of education relate -- line 426 to 460

Source do file: APPENDIX_E.do
Source dataset: US survey APSR.dta
Table A.7: Field of education and party affiliation using 2023 US CECT data -- line 189 to 212
Figure A.1: Field of education and party affiliation in the United States in 2023 -- line 189 to 218. 




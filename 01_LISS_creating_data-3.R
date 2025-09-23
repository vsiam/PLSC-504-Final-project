#################################3
###   Return to Center project ##3
###   Jonne Kamphorst          ##3
###   2020                     ##3
###   Database creation file   ##3
#################################3

########################################################################################3
###   This file creates the database for the panel analysis. It combines the diffeent  #3
###   waves per question set into one coherent dataframe  and then combines these      #3
###   dataframes into one big complete panel dataset in the standard long format.      #3
###   In total there are three different 'sets' of dataframes. The Income panel, used  #3
###   for the job questions, the voting panel, and the background data for income.     #3
###   The former two consist of a dataframe for each wave, the latter for each month   #3
###   throughout the whole survey period. All in all, a lot of frames thus need to be  #3
###   joined together. All the data that gets loaded here is freely downloadable from  #3
###   the LISS website, everyone can thus _completely_ reproduce the analysis, from    #3
###   the creation of the dataframes to the actual analysis (in a different file).     #3
########################################################################################3


###Libraries###
library(haven)
library(dplyr)
library(sjlabelled)
library(data.table)
library(labelled)




################### The economic-income data  ##################
#two more working directories to change at the bottom (to load the background variables)

####  Loading the data  ####3
inco_08 <- read_dta("data/LISS data/economic_income/ci08a_1.0p_EN.dta")
inco_08 <- remove_all_labels(inco_08)
inco_09 <- read_dta("data/LISS data/economic_income/ci09b_EN_1.1p.dta")
inco_09 <- remove_all_labels(inco_09)
inco_10 <- read_dta("data/LISS data/economic_income/ci10c_EN_1.0p.dta")
inco_10 <- remove_all_labels(inco_10)
inco_11 <- read_dta("data/LISS data/economic_income/ci11d_EN_1.0p.dta")
inco_11 <- remove_all_labels(inco_11)
inco_12 <- read_dta("data/LISS data/economic_income/ci12e_1.0p_EN.dta")
inco_12 <- remove_all_labels(inco_12)
inco_13 <- read_dta("data/LISS data/economic_income/ci13f_1.1p_EN.dta")
inco_13 <- remove_all_labels(inco_13)
inco_14 <- read_dta("data/LISS data/economic_income/ci14g_1.0p_EN.dta")
inco_14 <- remove_all_labels(inco_14)
inco_15 <- read_dta("data/LISS data/economic_income/ci15h_EN_2.0p.dta")
inco_15 <- remove_all_labels(inco_15)
inco_16 <- read_dta("data/LISS data/economic_income/ci16i_EN_2.0p.dta")
inco_16 <- remove_all_labels(inco_16)
inco_17 <- read_dta("data/LISS data/economic_income/ci17j_EN_2.0p.dta")
inco_17 <- remove_all_labels(inco_17)
inco_18 <- read_dta("data/LISS data/economic_income/ci18k_EN_1.0p.dta")
inco_18 <- remove_all_labels(inco_18)
inco_19 <- read_dta("data/LISS data/economic_income/ci19l_EN_1.0p.dta")
inco_19 <- remove_all_labels(inco_19)
inco_20 <- read_dta("data/LISS data/economic_income/ci20m_EN_1.0p.dta")
inco_20 <- remove_all_labels(inco_20)
inco_21 <- read_dta("data/LISS data/economic_income/ci21n_EN_1.0p.dta")
inco_21 <- remove_all_labels(inco_21)
inco_22 <- read_dta("data/LISS data/economic_income/ci22o_EN_1.0p.dta")
inco_22 <- remove_all_labels(inco_22)

#length(which(full_panel$year == 2008 & full_panel$fw_period > full_panel$wave_inc)) This one can be used on the final panel to check temporal order
#The income fieldwork is after the voting fieldwork

#the years are tricky, sometimes the collection is done in december and January. Double check. 
inco_08$year <- 1
inco_09$year <- 2
inco_10$year <- 3
inco_11$year <- 4
inco_12$year <- 5
inco_13$year <- 6
inco_14$year <- 7
inco_15$year <- 8
inco_16$year <- 9
inco_17$year <- 10
inco_18$year <- 11
inco_19$year <- 12
inco_20$year <- 13
inco_21$year <- 14
inco_22$year <- 15







####  Renaming variables by wave  ####3
#Variables:
#net change in income for a year (household or individual).
#losing job for those employed or new job for those who don't have a job, are not in the household and under 65. 
#ladder 10 step social position. 

## I rename all variables by wave. The variables have different names in different waves and these should obviously be the same
# you would need to do this for each variable you want to use. Note that you have to do this for each module, this is the income model
# and the variables in it that I used. Further down I do the same for the politics model. 



#2008
inco_08 <- inco_08 %>% rename(wave_inc = ci08a_m, #field work period
                              age = ci08a002,
                              pos_hous = ci08a001,
                              ladder = ci08a005,
                              employed = ci08a004,
                              losing_job = ci08a256, #chance someone loses their job in coming months
                              new_job = ci08a257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_08$new_job[inco_08$age > 64 | inco_08$pos_hous > 3 ] <- NA
inco_08$losing_job[inco_08$pos_hous > 3 ] <- NA



#2009
inco_09 <- inco_09 %>% rename(wave_inc = ci09b_m, #field work period
                              age = ci09b002,
                              pos_hous = ci09b001,
                              ladder = ci09b005,
                              employed = ci09b004,
                              losing_job = ci09b256, #chance someone loses their job in coming months
                              new_job = ci09b257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_09$new_job[inco_09$age > 64 | inco_09$pos_hous > 3 ] <- NA
inco_09$losing_job[inco_09$pos_hous > 3 ] <- NA



#2010
inco_10 <- inco_10 %>% rename(wave_inc = ci10c_m, #field work period
                              age = ci10c002,
                              pos_hous = ci10c001,
                              ladder = ci10c005,
                              employed = ci10c004,
                              losing_job = ci10c256, #chance someone loses their job in coming months
                              new_job = ci10c257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_10$new_job[inco_10$age > 64 | inco_10$pos_hous > 3 ] <- NA


#2011
inco_11 <- inco_11 %>% rename(wave_inc = ci11d_m, #field work period
                              age = ci11d002,
                              pos_hous = ci11d001,
                              ladder = ci11d005,
                              employed = ci11d004,
                              losing_job = ci11d256, #chance someone loses their job in coming months
                              new_job = ci11d257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 


#2012
inco_12 <- inco_12 %>% rename(wave_inc = ci12e_m, #field work period
                              age = ci12e002,
                              pos_hous = ci12e001,
                              ladder = ci12e005,
                              employed = ci12e004,
                              losing_job = ci12e256, #chance someone loses their job in coming months
                              new_job = ci12e257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 


#2013
inco_13 <- inco_13 %>% rename(wave_inc = ci13f_m, #field work period
                              age = ci13f002,
                              pos_hous = ci13f001,
                              ladder = ci13f005,
                              employed = ci13f004,
                              losing_job = ci13f256, #chance someone loses their job in coming months
                              new_job = ci13f257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_13$losing_job[inco_13$losing_job == 998] <- NA
inco_13$new_job[inco_13$new_job == 998 | inco_13$new_job == 999] <- NA



#2014
inco_14 <- inco_14 %>% rename(wave_inc = ci14g_m, #field work period
                              age = ci14g002,
                              pos_hous = ci14g001,
                              ladder = ci14g005,
                              employed = ci14g004,
                              losing_job = ci14g256, #chance someone loses their job in coming months
                              new_job = ci14g257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_14$losing_job[inco_14$losing_job == 998] <- NA
inco_14$new_job[inco_14$new_job == 998 | inco_14$new_job == 999] <- NA


#2015
inco_15 <- inco_15 %>% rename(wave_inc = ci15h_m, #field work period
                              age = ci15h002,
                              pos_hous = ci15h001,
                              ladder = ci15h005,
                              employed = ci15h004,
                              losing_job = ci15h256, #chance someone loses their job in coming months
                              new_job = ci15h257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_15$losing_job[inco_15$losing_job == 998] <- NA
inco_15$new_job[inco_15$new_job == 998 | inco_15$new_job == 999] <- NA



#2016
inco_16 <- inco_16 %>% rename(wave_inc = ci16i_m, #field work period
                              age = ci16i002,
                              pos_hous = ci16i001,
                              ladder = ci16i005,
                              employed = ci16i004,
                              losing_job = ci16i256, #chance someone loses their job in coming months
                              new_job = ci16i257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_16$losing_job[inco_16$losing_job == 998] <- NA
inco_16$new_job[inco_16$new_job == 998 | inco_16$new_job == 999] <- NA


#2017
inco_17 <- inco_17 %>% rename(wave_inc = ci17j_m, #field work period
                              age = ci17j002,
                              pos_hous = ci17j001,
                              ladder = ci17j005,
                              employed = ci17j004,
                              losing_job = ci17j256, #chance someone loses their job in coming months
                              new_job = ci17j257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_17$losing_job[inco_17$losing_job == 998] <- NA
inco_17$new_job[inco_17$new_job == 998 | inco_17$new_job == 999] <- NA




#2018
inco_18 <- inco_18 %>% rename(wave_inc = ci18k_m, #field work period
                              age = ci18k002,
                              pos_hous = ci18k001,
                              ladder = ci18k005,
                              employed = ci18k004,
                              losing_job = ci18k256, #chance someone loses their job in coming months
                              new_job = ci18k257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, employed, #select all the right variables
           losing_job, new_job)) 

inco_18$losing_job[inco_18$losing_job == 998] <- NA
inco_18$new_job[inco_18$new_job == 998 | inco_18$new_job == 999] <- NA



#2019
inco_19 <- inco_19 %>% rename(wave_inc = ci19l_m, #field work period
                              age = ci19l002,
                              pos_hous = ci19l001,
                              ladder = ci19l005,
                              new_job = ci19l257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, #select all the right variables
           new_job)) 

inco_19$losing_job <- NA
inco_19$employed <- NA
inco_19$new_job[inco_19$new_job == 998 | inco_19$new_job == 999] <- NA



#2020
inco_20 <- inco_20 %>% rename(wave_inc = ci20m_m, #field work period
                              age = ci20m002,
                              pos_hous = ci20m001,
                              ladder = ci20m005,
                              new_job = ci20m257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, #select all the right variables
           new_job)) 

inco_20$losing_job <- NA
inco_20$employed <- NA
inco_20$new_job[inco_20$new_job == 998 | inco_20$new_job == 999] <- NA



#2021
inco_21 <- inco_21 %>% rename(wave_inc = ci21n_m, #field work period
                              age = ci21n002,
                              pos_hous = ci21n001,
                              ladder = ci21n005,
                              new_job = ci21n257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, #select all the right variables
           new_job)) 

inco_21$losing_job <- NA
inco_21$employed <- NA
inco_21$new_job[inco_21$new_job == 998 | inco_21$new_job == 999] <- NA


#2022
inco_22 <- inco_22 %>% rename(wave_inc = ci22o_m, #field work period
                              age = ci22o002,
                              pos_hous = ci22o001,
                              ladder = ci22o005,
                              new_job = ci22o257) %>% #chances of finding a new job
  select(c(nomem_encr, age, pos_hous, wave_inc, year, ladder, #select all the right variables
           new_job)) 

inco_21$losing_job <- NA
inco_21$employed <- NA
inco_21$new_job[inco_21$new_job == 998 | inco_21$new_job == 999] <- NA





#merging all together
x <- full_join(inco_08, inco_09)
x <- full_join(x, inco_10)
x <- full_join(x, inco_11)
x <- full_join(x, inco_12)
x <- full_join(x, inco_13)
x <- full_join(x, inco_14)
x <- full_join(x, inco_15)
x <- full_join(x, inco_16)
x <- full_join(x, inco_17)
x <- full_join(x, inco_18)
x <- full_join(x, inco_19)
x <- full_join(x, inco_20)
x <- full_join(x, inco_21)
income_panel <- full_join(x, inco_22)
rm(x, inco_08, inco_09, inco_10, inco_11, inco_12, inco_13,
   inco_14, inco_15, inco_16, inco_17, inco_18, inco_19, inco_20, inco_21, inco_22)

income_panel <- income_panel %>% select(-c(age, pos_hous)) #drop variables used to set NAs in job variables. Better versions are in background panel




################### Adding the politics variables  ##################

####  Loading the data  ####3
pova_08 <- read_dta("data/LISS data/politics_values/cv08a_1.1p_EN.dta")
pova_08 <- remove_all_labels(pova_08)
pova_09 <- read_dta("data/LISS data/politics_values/cv09b_2.1p_EN.dta")
pova_09 <- remove_all_labels(pova_09)
pova_10 <- read_dta("data/LISS data/politics_values/cv10c_EN_1.0p.dta")
pova_10 <- remove_all_labels(pova_10)
pova_11 <- read_dta("data/LISS data/politics_values/cv11d_EN_1.0p.dta")
pova_11 <- remove_all_labels(pova_11)
pova_12 <- read_dta("data/LISS data/politics_values/cv12e_EN_1.0p.dta")
pova_12 <- remove_all_labels(pova_12)
pova_13 <- read_dta("data/LISS data/politics_values/cv13f_EN_1.0p.dta")
pova_13 <- remove_all_labels(pova_13)
pova_14 <- read_dta("data/LISS data/politics_values/cv14g_EN_1.0p.dta")
pova_14 <- remove_all_labels(pova_14)
pova_16 <- read_dta("data/LISS data/politics_values/cv16h_EN_1.0p.dta")
pova_16 <- remove_all_labels(pova_16)
pova_17 <- read_dta("data/LISS data/politics_values/cv17i_EN_1.0p.dta")
pova_17 <- remove_all_labels(pova_17)
pova_18 <- read_dta("data/LISS data/politics_values/cv18j_EN_1.0p.dta")
pova_18 <- remove_all_labels(pova_18)
pova_19 <- read_dta("data/LISS data/politics_values/cv19k_EN_1.0p.dta")
pova_19 <- remove_all_labels(pova_19)
pova_20 <- read_dta("data/LISS data/politics_values/cv20l_EN_1.0p.dta")
pova_20 <- remove_all_labels(pova_20)
pova_21 <- read_dta("data/LISS data/politics_values/cv21m_EN_1.0p.dta")
pova_21 <- remove_all_labels(pova_21)
pova_22 <- read_dta("data/LISS data/politics_values/cv22n_EN_1.0p.dta")
pova_22 <- remove_all_labels(pova_22)
pova_23 <- read_dta("data/LISS data/politics_values/cv23o_EN_1.0p.dta")
pova_23 <- remove_all_labels(pova_23)



pova_08$year <- 1
pova_09$year <- 2
pova_10$year <- 3
pova_11$year <- 4
pova_12$year <- 5
pova_13$year <- 6
pova_14$year <- 7
pova_16$year <- 8
pova_17$year <- 9
pova_18$year <- 10
pova_19$year <- 11
pova_20$year <- 12
pova_21$year <- 13
pova_22$year <- 14
pova_23$year <- 15



#2008
pova_08 <- pova_08 %>% rename(wave = cv08a_m, #field work period
                              elec_rec = cv08a054, #parliament elections 22 nov 2006
                              elec_today = cv08a058,
                              term_cda = cv08a076,
                              term_pvda = cv08a077,
                              term_vvd = cv08a078,
                              term_sp = cv08a079,
                              term_gl = cv08a080,
                              term_d66 = cv08a081,
                              term_cu = cv08a082,
                              term_sgp = cv08a083,
                              term_verdonk = cv08a084,
                              term_pvv = cv08a085,
                              term_pvdd = cv08a086,
                              party_mem = cv08a098, #are you a party member?
                              party_mem_p = cv08a099,
                              left_right = cv08a101,
                              redis = cv08a103,
                              immi = cv08a104,
                              eu_uni = cv08a105,
                              div_cultures = cv08a116,
                              asylum = cv08a118,
                              for_socials = cv08a119,
                              for_many = cv08a120,
                              for_neigh = cv08a123,
                              sat_gov = cv08a030,
                              sat_par = cv08a031,
                              sat_pol = cv08a034,
                              sat_part = cv08a035,
                              pol_int = cv08a012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_verdonk, term_pvv, term_pvdd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 

#the codes for parties are also different in each year. 
pova_08$elec_rec[pova_08$elec_rec == 998 | pova_08$elec_rec == 999] <- NA
pova_08$elec_rec[pova_08$elec_rec == 1] <- "CDA"
pova_08$elec_rec[pova_08$elec_rec == 2] <- "PvdA"
pova_08$elec_rec[pova_08$elec_rec == 3] <- "VVD"
pova_08$elec_rec[pova_08$elec_rec == 4] <- "SP"
pova_08$elec_rec[pova_08$elec_rec == 5] <- "GL"
pova_08$elec_rec[pova_08$elec_rec == 6] <- "LPF"
pova_08$elec_rec[pova_08$elec_rec == 7] <- "D66"
pova_08$elec_rec[pova_08$elec_rec == 8] <- "CU"
pova_08$elec_rec[pova_08$elec_rec == 9] <- "SGP"
pova_08$elec_rec[pova_08$elec_rec == 10] <- "1NL"
pova_08$elec_rec[pova_08$elec_rec == 11] <- "PVV"
pova_08$elec_rec[pova_08$elec_rec == 12] <- "PvdD"
pova_08$elec_rec[pova_08$elec_rec == 13] <- "Other"
pova_08$elec_rec[pova_08$elec_rec == 14] <- "Blanco"

pova_08$elec_today[pova_08$elec_today == 998 | pova_08$elec_today == 999] <- NA
pova_08$elec_today[pova_08$elec_today == 1] <- "Not"
pova_08$elec_today[pova_08$elec_today == 2] <- "Not"
pova_08$elec_today[pova_08$elec_today == 3] <- "CDA"
pova_08$elec_today[pova_08$elec_today == 4] <- "PvdA"
pova_08$elec_today[pova_08$elec_today == 5] <- "VVD"
pova_08$elec_today[pova_08$elec_today == 6] <- "SP"
pova_08$elec_today[pova_08$elec_today == 7] <- "GL"
pova_08$elec_today[pova_08$elec_today == 8] <- "D66"
pova_08$elec_today[pova_08$elec_today == 9] <- "CU"
pova_08$elec_today[pova_08$elec_today == 10] <- "SGP"
pova_08$elec_today[pova_08$elec_today == 11] <- "Verdonk"
pova_08$elec_today[pova_08$elec_today == 12] <- "PVV"
pova_08$elec_today[pova_08$elec_today == 13] <- "PvdD"
pova_08$elec_today[pova_08$elec_today == 14] <- "Other"
pova_08$elec_today[pova_08$elec_today == 15] <- "Blanco"

pova_08$party_mem_p[pova_08$party_mem_p == 1] <- "CDA"
pova_08$party_mem_p[pova_08$party_mem_p == 2] <- "PvdA"
pova_08$party_mem_p[pova_08$party_mem_p == 3] <- "VVD"
pova_08$party_mem_p[pova_08$party_mem_p == 4] <- "SP"
pova_08$party_mem_p[pova_08$party_mem_p == 5] <- "GL"
pova_08$party_mem_p[pova_08$party_mem_p == 6] <- "LPF"
pova_08$party_mem_p[pova_08$party_mem_p == 7] <- "D66"
pova_08$party_mem_p[pova_08$party_mem_p == 8] <- "CU"
pova_08$party_mem_p[pova_08$party_mem_p == 9] <- "SGP"
pova_08$party_mem_p[pova_08$party_mem_p == 10] <- "Verdonk"
pova_08$party_mem_p[pova_08$party_mem_p == 11] <- "PVV"
pova_08$party_mem_p[pova_08$party_mem_p == 12] <- "PvdD"
pova_08$party_mem_p[pova_08$party_mem_p == 13] <- "Other"




#2009
pova_09 <- pova_09 %>% rename(wave = cv09b_m, #field work period
                              elec_rec = cv09b054, #parliament elections 22 nov 2006
                              elec_today = cv09b058,
                              term_cda = cv09b076,
                              term_pvda = cv09b077,
                              term_vvd = cv09b078,
                              term_sp = cv09b079,
                              term_gl = cv09b080,
                              term_d66 = cv09b081,
                              term_cu = cv09b082,
                              term_sgp = cv09b083,
                              term_verdonk = cv09b084,
                              term_pvv = cv09b085,
                              term_pvdd = cv09b086,
                              party_mem = cv09b098, #are you a party member?
                              party_mem_p = cv09b099,
                              left_right = cv09b101,
                              redis = cv09b103,
                              immi = cv09b104,
                              eu_uni = cv09b105,
                              div_cultures = cv09b116,
                              asylum = cv09b118,
                              for_socials = cv09b119,
                              for_many = cv09b120,
                              for_neigh = cv09b123,
                              sat_gov = cv09b030,
                              sat_par = cv09b031,
                              sat_pol = cv09b034,
                              sat_part = cv09b035,
                              pol_int = cv09b012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_verdonk, term_pvv, term_pvdd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 

pova_09$elec_rec[pova_09$elec_rec == 998 | pova_09$elec_rec == 999] <- NA
pova_09$elec_rec[pova_09$elec_rec == 1] <- "CDA"
pova_09$elec_rec[pova_09$elec_rec == 2] <- "PvdA"
pova_09$elec_rec[pova_09$elec_rec == 3] <- "VVD"
pova_09$elec_rec[pova_09$elec_rec == 4] <- "SP"
pova_09$elec_rec[pova_09$elec_rec == 5] <- "GL"
pova_09$elec_rec[pova_09$elec_rec == 6] <- "LPF"
pova_09$elec_rec[pova_09$elec_rec == 7] <- "D66"
pova_09$elec_rec[pova_09$elec_rec == 8] <- "CU"
pova_09$elec_rec[pova_09$elec_rec == 9] <- "SGP"
pova_09$elec_rec[pova_09$elec_rec == 10] <- "1NL"
pova_09$elec_rec[pova_09$elec_rec == 11] <- "PVV"
pova_09$elec_rec[pova_09$elec_rec == 12] <- "PvdD"
pova_09$elec_rec[pova_09$elec_rec == 13] <- "Other"
pova_09$elec_rec[pova_09$elec_rec == 14] <- "Blanco"

pova_09$elec_today[pova_09$elec_today == 998 | pova_09$elec_today == 999] <- NA
pova_09$elec_today[pova_09$elec_today == 1] <- "Not"
pova_09$elec_today[pova_09$elec_today == 2] <- "Not"
pova_09$elec_today[pova_09$elec_today == 3] <- "CDA"
pova_09$elec_today[pova_09$elec_today == 4] <- "PvdA"
pova_09$elec_today[pova_09$elec_today == 5] <- "VVD"
pova_09$elec_today[pova_09$elec_today == 6] <- "SP"
pova_09$elec_today[pova_09$elec_today == 7] <- "GL"
pova_09$elec_today[pova_09$elec_today == 8] <- "D66"
pova_09$elec_today[pova_09$elec_today == 9] <- "CU"
pova_09$elec_today[pova_09$elec_today == 10] <- "SGP"
pova_09$elec_today[pova_09$elec_today == 11] <- "Verdonk"
pova_09$elec_today[pova_09$elec_today == 12] <- "PVV"
pova_09$elec_today[pova_09$elec_today == 13] <- "PvdD"
pova_09$elec_today[pova_09$elec_today == 14] <- "Other"
pova_09$elec_today[pova_09$elec_today == 15] <- "Blanco"

pova_09$party_mem_p[pova_09$party_mem_p == 1] <- "CDA"
pova_09$party_mem_p[pova_09$party_mem_p == 2] <- "PvdA"
pova_09$party_mem_p[pova_09$party_mem_p == 3] <- "VVD"
pova_09$party_mem_p[pova_09$party_mem_p == 4] <- "SP"
pova_09$party_mem_p[pova_09$party_mem_p == 5] <- "GL"
pova_09$party_mem_p[pova_09$party_mem_p == 6] <- "LPF"
pova_09$party_mem_p[pova_09$party_mem_p == 7] <- "D66"
pova_09$party_mem_p[pova_09$party_mem_p == 8] <- "CU"
pova_09$party_mem_p[pova_09$party_mem_p == 9] <- "SGP"
pova_09$party_mem_p[pova_09$party_mem_p == 10] <- "Verdonk"
pova_09$party_mem_p[pova_09$party_mem_p == 11] <- "PVV"
pova_09$party_mem_p[pova_09$party_mem_p == 12] <- "PvdD"
pova_09$party_mem_p[pova_09$party_mem_p == 13] <- "Other"









#2010
pova_10 <- pova_10 %>% rename(wave = cv10c_m, #field work period
                              elec_rec = cv10c054, #parliament elections 22 nov 2006
                              elec_today = cv10c058,
                              term_cda = cv10c076,
                              term_pvda = cv10c077,
                              term_vvd = cv10c078,
                              term_sp = cv10c079,
                              term_gl = cv10c080,
                              term_d66 = cv10c081,
                              term_cu = cv10c082,
                              term_sgp = cv10c083,
                              term_verdonk = cv10c084,
                              term_pvv = cv10c085,
                              term_pvdd = cv10c086,
                              party_mem = cv10c098, #are you a party member?
                              party_mem_p = cv10c099,
                              left_right = cv10c101,
                              redis = cv10c103,
                              immi = cv10c104,
                              eu_uni = cv10c105,
                              div_cultures = cv10c116,
                              asylum = cv10c118,
                              for_socials = cv10c119,
                              for_many = cv10c120,
                              for_neigh = cv10c123,
                              sat_gov = cv10c030,
                              sat_par = cv10c031,
                              sat_pol = cv10c034,
                              sat_part = cv10c035,
                              pol_int = cv10c012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_verdonk, term_pvv, term_pvdd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 

pova_10$elec_rec[pova_10$elec_rec == 998 | pova_10$elec_rec == 999] <- NA
pova_10$elec_rec[pova_10$elec_rec == 1] <- "CDA"
pova_10$elec_rec[pova_10$elec_rec == 2] <- "PvdA"
pova_10$elec_rec[pova_10$elec_rec == 3] <- "VVD"
pova_10$elec_rec[pova_10$elec_rec == 4] <- "SP"
pova_10$elec_rec[pova_10$elec_rec == 5] <- "GL"
pova_10$elec_rec[pova_10$elec_rec == 6] <- "LPF"
pova_10$elec_rec[pova_10$elec_rec == 7] <- "D66"
pova_10$elec_rec[pova_10$elec_rec == 8] <- "CU"
pova_10$elec_rec[pova_10$elec_rec == 9] <- "SGP"
pova_10$elec_rec[pova_10$elec_rec == 10] <- "1NL"
pova_10$elec_rec[pova_10$elec_rec == 11] <- "PVV"
pova_10$elec_rec[pova_10$elec_rec == 12] <- "PvdD"
pova_10$elec_rec[pova_10$elec_rec == 13] <- "Other"
pova_10$elec_rec[pova_10$elec_rec == 14] <- "Blanco"

pova_10$elec_today[pova_10$elec_today == 998 | pova_10$elec_today == 999] <- NA
pova_10$elec_today[pova_10$elec_today == 1] <- "Not"
pova_10$elec_today[pova_10$elec_today == 2] <- "Not"
pova_10$elec_today[pova_10$elec_today == 3] <- "CDA"
pova_10$elec_today[pova_10$elec_today == 4] <- "PvdA"
pova_10$elec_today[pova_10$elec_today == 5] <- "VVD"
pova_10$elec_today[pova_10$elec_today == 6] <- "SP"
pova_10$elec_today[pova_10$elec_today == 7] <- "GL"
pova_10$elec_today[pova_10$elec_today == 8] <- "D66"
pova_10$elec_today[pova_10$elec_today == 9] <- "CU"
pova_10$elec_today[pova_10$elec_today == 10] <- "SGP"
pova_10$elec_today[pova_10$elec_today == 11] <- "Verdonk"
pova_10$elec_today[pova_10$elec_today == 12] <- "PVV"
pova_10$elec_today[pova_10$elec_today == 13] <- "PvdD"
pova_10$elec_today[pova_10$elec_today == 14] <- "Other"
pova_10$elec_today[pova_10$elec_today == 15] <- "Blanco"

pova_10$party_mem_p[pova_10$party_mem_p == 1] <- "CDA"
pova_10$party_mem_p[pova_10$party_mem_p == 2] <- "PvdA"
pova_10$party_mem_p[pova_10$party_mem_p == 3] <- "VVD"
pova_10$party_mem_p[pova_10$party_mem_p == 4] <- "SP"
pova_10$party_mem_p[pova_10$party_mem_p == 5] <- "GL"
pova_10$party_mem_p[pova_10$party_mem_p == 6] <- "LPF"
pova_10$party_mem_p[pova_10$party_mem_p == 7] <- "D66"
pova_10$party_mem_p[pova_10$party_mem_p == 8] <- "CU"
pova_10$party_mem_p[pova_10$party_mem_p == 9] <- "SGP"
pova_10$party_mem_p[pova_10$party_mem_p == 10] <- "Verdonk"
pova_10$party_mem_p[pova_10$party_mem_p == 11] <- "PVV"
pova_10$party_mem_p[pova_10$party_mem_p == 12] <- "PvdD"
pova_10$party_mem_p[pova_10$party_mem_p == 13] <- "Other"




#2011
pova_11 <- pova_11 %>% rename(wave = cv11d_m, #field work period
                              elec_rec = cv11d169, #parliament elections 9 June 2010
                              elec_today = cv11d171,
                              term_cda = cv11d176,
                              term_pvda = cv11d174,
                              term_vvd = cv11d173,
                              term_sp = cv11d177,
                              term_gl = cv11d179,
                              term_d66 = cv11d178,
                              term_cu = cv11d180,
                              term_sgp = cv11d181,
                              term_pvv = cv11d175,
                              term_pvdd = cv11d182,
                              party_mem = cv11d098, #are you a party member?
                              party_mem_p = cv11d193,
                              left_right = cv11d101,
                              redis = cv11d103,
                              immi = cv11d104,
                              eu_uni = cv11d105,
                              div_cultures = cv11d116,
                              asylum = cv11d118,
                              for_socials = cv11d119,
                              for_many = cv11d120,
                              for_neigh = cv11d123,
                              sat_gov = cv11d030,
                              sat_par = cv11d031,
                              sat_pol = cv11d034,
                              sat_part = cv11d035,
                              pol_int = cv11d012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 

pova_11$elec_rec[pova_11$elec_rec == 998 | pova_11$elec_rec == 999] <- NA
pova_11$elec_rec[pova_11$elec_rec == 1] <- "VVD"
pova_11$elec_rec[pova_11$elec_rec == 2] <- "PvdA"
pova_11$elec_rec[pova_11$elec_rec == 3] <- "PVV"
pova_11$elec_rec[pova_11$elec_rec == 4] <- "CDA"
pova_11$elec_rec[pova_11$elec_rec == 5] <- "SP"
pova_11$elec_rec[pova_11$elec_rec == 6] <- "D66"
pova_11$elec_rec[pova_11$elec_rec == 7] <- "GL"
pova_11$elec_rec[pova_11$elec_rec == 8] <- "CU"
pova_11$elec_rec[pova_11$elec_rec == 9] <- "SGP"
pova_11$elec_rec[pova_11$elec_rec == 10] <- "PvdD"
pova_11$elec_rec[pova_11$elec_rec == 11] <- "Other"
pova_11$elec_rec[pova_11$elec_rec == 12] <- "Blanco"


pova_11$elec_today[pova_11$elec_today == 998 | pova_11$elec_today == 999] <- NA
pova_11$elec_today[pova_11$elec_today == 1] <- "Not"
pova_11$elec_today[pova_11$elec_today == 2] <- "Not"
pova_11$elec_today[pova_11$elec_today == 3] <- "VVD"
pova_11$elec_today[pova_11$elec_today == 4] <- "PvdA"
pova_11$elec_today[pova_11$elec_today == 5] <- "PVV"
pova_11$elec_today[pova_11$elec_today == 6] <- "CDA"
pova_11$elec_today[pova_11$elec_today == 7] <- "SP"
pova_11$elec_today[pova_11$elec_today == 8] <- "D66"
pova_11$elec_today[pova_11$elec_today == 9] <- "GL"
pova_11$elec_today[pova_11$elec_today == 10] <- "CU"
pova_11$elec_today[pova_11$elec_today == 11] <- "SGP"
pova_11$elec_today[pova_11$elec_today == 12] <- "PvdD"
pova_11$elec_today[pova_11$elec_today == 13] <- "Other"
pova_11$elec_today[pova_11$elec_today == 14] <- "Blanco"


pova_11$party_mem_p[pova_11$party_mem_p == 1] <- "VVD"
pova_11$party_mem_p[pova_11$party_mem_p == 2] <- "PvdA"
pova_11$party_mem_p[pova_11$party_mem_p == 3] <- "PVV"
pova_11$party_mem_p[pova_11$party_mem_p == 4] <- "CDA"
pova_11$party_mem_p[pova_11$party_mem_p == 5] <- "SP"
pova_11$party_mem_p[pova_11$party_mem_p == 6] <- "D66"
pova_11$party_mem_p[pova_11$party_mem_p == 7] <- "GL"
pova_11$party_mem_p[pova_11$party_mem_p == 8] <- "CU"
pova_11$party_mem_p[pova_11$party_mem_p == 9] <- "SGP"
pova_11$party_mem_p[pova_11$party_mem_p == 10] <- "PvdD"
pova_11$party_mem_p[pova_11$party_mem_p == 11] <- "Other"





#2012
pova_12 <- pova_12 %>% rename(wave = cv12e_m, #field work period
                              elec_rec = cv12e169, #parliament elections 9 June 2010
                              elec_today = cv12e171,
                              term_cda = cv12e176,
                              term_pvda = cv12e174,
                              term_vvd = cv12e173,
                              term_sp = cv12e177,
                              term_gl = cv12e179,
                              term_d66 = cv12e178,
                              term_cu = cv12e180,
                              term_sgp = cv12e181,
                              term_pvv = cv12e175,
                              term_pvdd = cv12e182,
                              party_mem = cv12e098, #are you a party member?
                              party_mem_p = cv12e193,
                              left_right = cv12e101,
                              redis = cv12e103,
                              immi = cv12e104,
                              eu_uni = cv12e105,
                              party_adh = cv12e199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv12e201,
                              party_att = cv12e200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv12e203,
                              div_cultures = cv12e116,
                              asylum = cv12e118,
                              for_socials = cv12e119,
                              for_many = cv12e120,
                              for_neigh = cv12e123,
                              sat_gov = cv12e030,
                              sat_par = cv12e031,
                              sat_pol = cv12e034,
                              sat_part = cv12e035,
                              pol_int = cv12e012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p,
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_12$elec_rec[pova_12$elec_rec == 998 | pova_12$elec_rec == 999] <- NA
pova_12$elec_rec[pova_12$elec_rec == 1] <- "VVD"
pova_12$elec_rec[pova_12$elec_rec == 2] <- "PvdA"
pova_12$elec_rec[pova_12$elec_rec == 3] <- "PVV"
pova_12$elec_rec[pova_12$elec_rec == 4] <- "CDA"
pova_12$elec_rec[pova_12$elec_rec == 5] <- "SP"
pova_12$elec_rec[pova_12$elec_rec == 6] <- "D66"
pova_12$elec_rec[pova_12$elec_rec == 7] <- "GL"
pova_12$elec_rec[pova_12$elec_rec == 8] <- "CU"
pova_12$elec_rec[pova_12$elec_rec == 9] <- "SGP"
pova_12$elec_rec[pova_12$elec_rec == 10] <- "PvdD"
pova_12$elec_rec[pova_12$elec_rec == 11] <- "Other"
pova_12$elec_rec[pova_12$elec_rec == 12] <- "Blanco"


pova_12$elec_today[pova_12$elec_today == 998 | pova_12$elec_today == 999] <- NA
pova_12$elec_today[pova_12$elec_today == 1] <- "Not"
pova_12$elec_today[pova_12$elec_today == 2] <- "Not"
pova_12$elec_today[pova_12$elec_today == 3] <- "VVD"
pova_12$elec_today[pova_12$elec_today == 4] <- "PvdA"
pova_12$elec_today[pova_12$elec_today == 5] <- "PVV"
pova_12$elec_today[pova_12$elec_today == 6] <- "CDA"
pova_12$elec_today[pova_12$elec_today == 7] <- "SP"
pova_12$elec_today[pova_12$elec_today == 8] <- "D66"
pova_12$elec_today[pova_12$elec_today == 9] <- "GL"
pova_12$elec_today[pova_12$elec_today == 10] <- "CU"
pova_12$elec_today[pova_12$elec_today == 11] <- "SGP"
pova_12$elec_today[pova_12$elec_today == 12] <- "PvdD"
pova_12$elec_today[pova_12$elec_today == 13] <- "Other"
pova_12$elec_today[pova_12$elec_today == 14] <- "Blanco"


pova_12$party_mem_p[pova_12$party_mem_p == 1] <- "VVD"
pova_12$party_mem_p[pova_12$party_mem_p == 2] <- "PvdA"
pova_12$party_mem_p[pova_12$party_mem_p == 3] <- "PVV"
pova_12$party_mem_p[pova_12$party_mem_p == 4] <- "CDA"
pova_12$party_mem_p[pova_12$party_mem_p == 5] <- "SP"
pova_12$party_mem_p[pova_12$party_mem_p == 6] <- "D66"
pova_12$party_mem_p[pova_12$party_mem_p == 7] <- "GL"
pova_12$party_mem_p[pova_12$party_mem_p == 8] <- "CU"
pova_12$party_mem_p[pova_12$party_mem_p == 9] <- "SGP"
pova_12$party_mem_p[pova_12$party_mem_p == 10] <- "PvdD"
pova_12$party_mem_p[pova_12$party_mem_p == 11] <- "Other"

pova_12$party_adh_p[pova_12$party_adh_p == 1] <- "VVD"
pova_12$party_adh_p[pova_12$party_adh_p == 2] <- "PvdA"
pova_12$party_adh_p[pova_12$party_adh_p == 3] <- "PVV"
pova_12$party_adh_p[pova_12$party_adh_p == 4] <- "CDA"
pova_12$party_adh_p[pova_12$party_adh_p == 5] <- "SP"
pova_12$party_adh_p[pova_12$party_adh_p == 6] <- "D66"
pova_12$party_adh_p[pova_12$party_adh_p == 7] <- "GL"
pova_12$party_adh_p[pova_12$party_adh_p == 8] <- "CU"
pova_12$party_adh_p[pova_12$party_adh_p == 9] <- "SGP"
pova_12$party_adh_p[pova_12$party_adh_p == 10] <- "PvdD"
pova_12$party_adh_p[pova_12$party_adh_p == 11] <- "Other"

pova_12$party_att_p[pova_12$party_att_p == 1] <- "VVD"
pova_12$party_att_p[pova_12$party_att_p == 2] <- "PvdA"
pova_12$party_att_p[pova_12$party_att_p == 3] <- "PVV"
pova_12$party_att_p[pova_12$party_att_p == 4] <- "CDA"
pova_12$party_att_p[pova_12$party_att_p == 5] <- "SP"
pova_12$party_att_p[pova_12$party_att_p == 6] <- "D66"
pova_12$party_att_p[pova_12$party_att_p == 7] <- "GL"
pova_12$party_att_p[pova_12$party_att_p == 8] <- "CU"
pova_12$party_att_p[pova_12$party_att_p == 9] <- "SGP"
pova_12$party_att_p[pova_12$party_att_p == 10] <- "PvdD"
pova_12$party_att_p[pova_12$party_att_p == 11] <- "Other"




#2013
pova_13 <- pova_13 %>% rename(wave = cv13f_m, #field work period
                              elec_rec = cv13f207, #parliament elections 12 Sep 2012
                              elec_today = cv13f209,
                              term_cda = cv13f215,
                              term_pvda = cv13f212,
                              term_vvd = cv13f211,
                              term_sp = cv13f214,
                              term_gl = cv13f218,
                              term_d66 = cv13f216,
                              term_cu = cv13f217,
                              term_sgp = cv13f219,
                              term_pvv = cv13f213,
                              term_pvdd = cv13f220,
                              term_50 = cv13f221,
                              party_mem = cv13f098, #are you a party member?
                              party_mem_p = cv13f237,
                              left_right = cv13f101,
                              redis = cv13f103,
                              immi = cv13f104,
                              eu_uni = cv13f105,
                              party_adh = cv13f199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv13f233,
                              party_att = cv13f200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv13f235,
                              div_cultures = cv13f116,
                              asylum = cv13f118,
                              for_socials = cv13f119,
                              for_many = cv13f120,
                              for_neigh = cv13f123,
                              sat_gov = cv13f030,
                              sat_par = cv13f031,
                              sat_pol = cv13f034,
                              sat_part = cv13f035,
                              pol_int = cv13f012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_13$elec_rec[pova_13$elec_rec == 998 | pova_13$elec_rec == 999] <- NA
pova_13$elec_rec[pova_13$elec_rec == 1] <- "VVD"
pova_13$elec_rec[pova_13$elec_rec == 2] <- "PvdA"
pova_13$elec_rec[pova_13$elec_rec == 3] <- "PVV"
pova_13$elec_rec[pova_13$elec_rec == 4] <- "SP"
pova_13$elec_rec[pova_13$elec_rec == 5] <- "CDA"
pova_13$elec_rec[pova_13$elec_rec == 6] <- "D66"
pova_13$elec_rec[pova_13$elec_rec == 7] <- "CU"
pova_13$elec_rec[pova_13$elec_rec == 8] <- "GL"
pova_13$elec_rec[pova_13$elec_rec == 9] <- "SGP"
pova_13$elec_rec[pova_13$elec_rec == 10] <- "PvdD"
pova_13$elec_rec[pova_13$elec_rec == 11] <- "50+"
pova_13$elec_rec[pova_13$elec_rec == 12] <- "Other"
pova_13$elec_rec[pova_13$elec_rec == 13] <- "Blanco"


pova_13$elec_today[pova_13$elec_today == 998 | pova_13$elec_today == 999] <- NA
pova_13$elec_today[pova_13$elec_today == 1] <- "Not"
pova_13$elec_today[pova_13$elec_today == 2] <- "Not"
pova_13$elec_today[pova_13$elec_today == 3] <- "VVD"
pova_13$elec_today[pova_13$elec_today == 4] <- "PvdA"
pova_13$elec_today[pova_13$elec_today == 5] <- "PVV"
pova_13$elec_today[pova_13$elec_today == 6] <- "SP"
pova_13$elec_today[pova_13$elec_today == 7] <- "CDA"
pova_13$elec_today[pova_13$elec_today == 8] <- "D66"
pova_13$elec_today[pova_13$elec_today == 9] <- "CU"
pova_13$elec_today[pova_13$elec_today == 10] <- "GL"
pova_13$elec_today[pova_13$elec_today == 11] <- "SGP"
pova_13$elec_today[pova_13$elec_today == 12] <- "PvdD"
pova_13$elec_today[pova_13$elec_today == 13] <- "50+"
pova_13$elec_today[pova_13$elec_today == 14] <- "Other"
pova_13$elec_today[pova_13$elec_today == 15] <- "Blanco"


pova_13$party_mem_p[pova_13$party_mem_p == 1] <- "VVD"
pova_13$party_mem_p[pova_13$party_mem_p == 2] <- "PvdA"
pova_13$party_mem_p[pova_13$party_mem_p == 3] <- "PVV"
pova_13$party_mem_p[pova_13$party_mem_p == 4] <- "SP"
pova_13$party_mem_p[pova_13$party_mem_p == 5] <- "CDA"
pova_13$party_mem_p[pova_13$party_mem_p == 6] <- "D66"
pova_13$party_mem_p[pova_13$party_mem_p == 7] <- "CU"
pova_13$party_mem_p[pova_13$party_mem_p == 8] <- "GL"
pova_13$party_mem_p[pova_13$party_mem_p == 9] <- "SGP"
pova_13$party_mem_p[pova_13$party_mem_p == 10] <- "PvdD"
pova_13$party_mem_p[pova_13$party_mem_p == 11] <- "50+"
pova_13$party_mem_p[pova_13$party_mem_p == 12] <- "Other"

pova_13$party_adh_p[pova_13$party_adh_p == 1] <- "VVD"
pova_13$party_adh_p[pova_13$party_adh_p == 2] <- "PvdA"
pova_13$party_adh_p[pova_13$party_adh_p == 3] <- "PVV"
pova_13$party_adh_p[pova_13$party_adh_p == 4] <- "SP"
pova_13$party_adh_p[pova_13$party_adh_p == 5] <- "CDA"
pova_13$party_adh_p[pova_13$party_adh_p == 6] <- "D66"
pova_13$party_adh_p[pova_13$party_adh_p == 7] <- "CU"
pova_13$party_adh_p[pova_13$party_adh_p == 8] <- "GL"
pova_13$party_adh_p[pova_13$party_adh_p == 9] <- "SGP"
pova_13$party_adh_p[pova_13$party_adh_p == 10] <- "PvdD"
pova_13$party_adh_p[pova_13$party_adh_p == 11] <- "50+"
pova_13$party_adh_p[pova_13$party_adh_p == 12] <- "Other"

pova_13$party_att_p[pova_13$party_att_p == 1] <- "VVD"
pova_13$party_att_p[pova_13$party_att_p == 2] <- "PvdA"
pova_13$party_att_p[pova_13$party_att_p == 3] <- "PVV"
pova_13$party_att_p[pova_13$party_att_p == 4] <- "SP"
pova_13$party_att_p[pova_13$party_att_p == 5] <- "CDA"
pova_13$party_att_p[pova_13$party_att_p == 6] <- "D66"
pova_13$party_att_p[pova_13$party_att_p == 7] <- "CU"
pova_13$party_att_p[pova_13$party_att_p == 8] <- "GL"
pova_13$party_att_p[pova_13$party_att_p == 9] <- "SGP"
pova_13$party_att_p[pova_13$party_att_p == 10] <- "PvdD"
pova_13$party_att_p[pova_13$party_att_p == 11] <- "50+"
pova_13$party_att_p[pova_13$party_att_p == 12] <- "Other"




#2014
pova_14 <- pova_14 %>% rename(wave = cv14g_m, #field work period
                              elec_rec = cv14g207, #parliament elections 12 Sep 2012
                              elec_today = cv14g209,
                              term_cda = cv14g215,
                              term_pvda = cv14g212,
                              term_vvd = cv14g211,
                              term_sp = cv14g214,
                              term_gl = cv14g218,
                              term_d66 = cv14g216,
                              term_cu = cv14g217,
                              term_sgp = cv14g219,
                              term_pvv = cv14g213,
                              term_pvdd = cv14g220,
                              term_50 = cv14g221,
                              party_mem = cv14g098, #are you a party member?
                              party_mem_p = cv14g237,
                              left_right = cv14g101,
                              redis = cv14g103,
                              immi = cv14g104,
                              eu_uni = cv14g105,
                              party_adh = cv14g199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv14g233,
                              party_att = cv14g200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv14g235,
                              div_cultures = cv14g116,
                              asylum = cv14g118,
                              for_socials = cv14g119,
                              for_many = cv14g120,
                              for_neigh = cv14g123,
                              sat_gov = cv14g030,
                              sat_par = cv14g031,
                              sat_pol = cv14g034,
                              sat_part = cv14g035,
                              pol_int = cv14g012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_14$elec_rec[pova_14$elec_rec == 998 | pova_14$elec_rec == 999] <- NA
pova_14$elec_rec[pova_14$elec_rec == 1] <- "VVD"
pova_14$elec_rec[pova_14$elec_rec == 2] <- "PvdA"
pova_14$elec_rec[pova_14$elec_rec == 3] <- "PVV"
pova_14$elec_rec[pova_14$elec_rec == 4] <- "SP"
pova_14$elec_rec[pova_14$elec_rec == 5] <- "CDA"
pova_14$elec_rec[pova_14$elec_rec == 6] <- "D66"
pova_14$elec_rec[pova_14$elec_rec == 7] <- "CU"
pova_14$elec_rec[pova_14$elec_rec == 8] <- "GL"
pova_14$elec_rec[pova_14$elec_rec == 9] <- "SGP"
pova_14$elec_rec[pova_14$elec_rec == 10] <- "PvdD"
pova_14$elec_rec[pova_14$elec_rec == 11] <- "50+"
pova_14$elec_rec[pova_14$elec_rec == 12] <- "Other"
pova_14$elec_rec[pova_14$elec_rec == 13] <- "Blanco"


pova_14$elec_today[pova_14$elec_today == 998 | pova_14$elec_today == 999] <- NA
pova_14$elec_today[pova_14$elec_today == 1] <- "Not"
pova_14$elec_today[pova_14$elec_today == 2] <- "Not"
pova_14$elec_today[pova_14$elec_today == 3] <- "VVD"
pova_14$elec_today[pova_14$elec_today == 4] <- "PvdA"
pova_14$elec_today[pova_14$elec_today == 5] <- "PVV"
pova_14$elec_today[pova_14$elec_today == 6] <- "SP"
pova_14$elec_today[pova_14$elec_today == 7] <- "CDA"
pova_14$elec_today[pova_14$elec_today == 8] <- "D66"
pova_14$elec_today[pova_14$elec_today == 9] <- "CU"
pova_14$elec_today[pova_14$elec_today == 10] <- "GL"
pova_14$elec_today[pova_14$elec_today == 11] <- "SGP"
pova_14$elec_today[pova_14$elec_today == 12] <- "PvdD"
pova_14$elec_today[pova_14$elec_today == 13] <- "50+"
pova_14$elec_today[pova_14$elec_today == 14] <- "Other"
pova_14$elec_today[pova_14$elec_today == 15] <- "Blanco"


pova_14$party_mem_p[pova_14$party_mem_p == 1] <- "VVD"
pova_14$party_mem_p[pova_14$party_mem_p == 2] <- "PvdA"
pova_14$party_mem_p[pova_14$party_mem_p == 3] <- "PVV"
pova_14$party_mem_p[pova_14$party_mem_p == 4] <- "SP"
pova_14$party_mem_p[pova_14$party_mem_p == 5] <- "CDA"
pova_14$party_mem_p[pova_14$party_mem_p == 6] <- "D66"
pova_14$party_mem_p[pova_14$party_mem_p == 7] <- "CU"
pova_14$party_mem_p[pova_14$party_mem_p == 8] <- "GL"
pova_14$party_mem_p[pova_14$party_mem_p == 9] <- "SGP"
pova_14$party_mem_p[pova_14$party_mem_p == 10] <- "PvdD"
pova_14$party_mem_p[pova_14$party_mem_p == 11] <- "50+"
pova_14$party_mem_p[pova_14$party_mem_p == 12] <- "Other"

pova_14$party_adh_p[pova_14$party_adh_p == 1] <- "VVD"
pova_14$party_adh_p[pova_14$party_adh_p == 2] <- "PvdA"
pova_14$party_adh_p[pova_14$party_adh_p == 3] <- "PVV"
pova_14$party_adh_p[pova_14$party_adh_p == 4] <- "SP"
pova_14$party_adh_p[pova_14$party_adh_p == 5] <- "CDA"
pova_14$party_adh_p[pova_14$party_adh_p == 6] <- "D66"
pova_14$party_adh_p[pova_14$party_adh_p == 7] <- "CU"
pova_14$party_adh_p[pova_14$party_adh_p == 8] <- "GL"
pova_14$party_adh_p[pova_14$party_adh_p == 9] <- "SGP"
pova_14$party_adh_p[pova_14$party_adh_p == 10] <- "PvdD"
pova_14$party_adh_p[pova_14$party_adh_p == 11] <- "50+"
pova_14$party_adh_p[pova_14$party_adh_p == 12] <- "Other"

pova_14$party_att_p[pova_14$party_att_p == 1] <- "VVD"
pova_14$party_att_p[pova_14$party_att_p == 2] <- "PvdA"
pova_14$party_att_p[pova_14$party_att_p == 3] <- "PVV"
pova_14$party_att_p[pova_14$party_att_p == 4] <- "SP"
pova_14$party_att_p[pova_14$party_att_p == 5] <- "CDA"
pova_14$party_att_p[pova_14$party_att_p == 6] <- "D66"
pova_14$party_att_p[pova_14$party_att_p == 7] <- "CU"
pova_14$party_att_p[pova_14$party_att_p == 8] <- "GL"
pova_14$party_att_p[pova_14$party_att_p == 9] <- "SGP"
pova_14$party_att_p[pova_14$party_att_p == 10] <- "PvdD"
pova_14$party_att_p[pova_14$party_att_p == 11] <- "50+"
pova_14$party_att_p[pova_14$party_att_p == 12] <- "Other"






#2016
#the wave number is different here due to a redistribution experiment in the survey. Thus copy in the right waves here. 
pova_16$wave <- pova_16$maandnr
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_lang[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_deel1[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_deel2[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_deel3[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_a[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_b[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_c[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_d[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_e[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_f[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_g[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_h[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_i[is.na(pova_16$wave)]
pova_16$wave[is.na(pova_16$wave)] <- pova_16$maandnr_j[is.na(pova_16$wave)]


pova_16 <- pova_16 %>% rename(elec_rec = cv16h207, #parliament elections 12 Sep 2012
                              elec_today = cv16h209,
                              term_cda = cv16h215,
                              term_pvda = cv16h212,
                              term_vvd = cv16h211,
                              term_sp = cv16h214,
                              term_gl = cv16h218,
                              term_d66 = cv16h216,
                              term_cu = cv16h217,
                              term_sgp = cv16h219,
                              term_pvv = cv16h213,
                              term_pvdd = cv16h220,
                              term_50 = cv16h221,
                              party_mem = cv16h098, #are you a party member?
                              party_mem_p = cv16h237,
                              left_right = cv16h101,
                              redis = cv16h103,
                              immi = cv16h104,
                              eu_uni = cv16h105,
                              party_adh = cv16h199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv16h233,
                              party_att = cv16h200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv16h235,
                              div_cultures = cv16h116,
                              asylum = cv16h118,
                              for_socials = cv16h119,
                              for_many = cv16h120,
                              for_neigh = cv16h123,
                              sat_gov = cv16h030,
                              sat_par = cv16h031,
                              sat_pol = cv16h034,
                              sat_part = cv16h035,
                              pol_int = cv16h012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_16$elec_rec[pova_16$elec_rec == 998 | pova_16$elec_rec == 999] <- NA
pova_16$elec_rec[pova_16$elec_rec == 1] <- "VVD"
pova_16$elec_rec[pova_16$elec_rec == 2] <- "PvdA"
pova_16$elec_rec[pova_16$elec_rec == 3] <- "PVV"
pova_16$elec_rec[pova_16$elec_rec == 4] <- "SP"
pova_16$elec_rec[pova_16$elec_rec == 5] <- "CDA"
pova_16$elec_rec[pova_16$elec_rec == 6] <- "D66"
pova_16$elec_rec[pova_16$elec_rec == 7] <- "CU"
pova_16$elec_rec[pova_16$elec_rec == 8] <- "GL"
pova_16$elec_rec[pova_16$elec_rec == 9] <- "SGP"
pova_16$elec_rec[pova_16$elec_rec == 10] <- "PvdD"
pova_16$elec_rec[pova_16$elec_rec == 11] <- "50+"
pova_16$elec_rec[pova_16$elec_rec == 12] <- "Other"
pova_16$elec_rec[pova_16$elec_rec == 13] <- "Blanco"


pova_16$elec_today[pova_16$elec_today == 998 | pova_16$elec_today == 999] <- NA
pova_16$elec_today[pova_16$elec_today == 1] <- "Not"
pova_16$elec_today[pova_16$elec_today == 2] <- "Not"
pova_16$elec_today[pova_16$elec_today == 3] <- "VVD"
pova_16$elec_today[pova_16$elec_today == 4] <- "PvdA"
pova_16$elec_today[pova_16$elec_today == 5] <- "PVV"
pova_16$elec_today[pova_16$elec_today == 6] <- "SP"
pova_16$elec_today[pova_16$elec_today == 7] <- "CDA"
pova_16$elec_today[pova_16$elec_today == 8] <- "D66"
pova_16$elec_today[pova_16$elec_today == 9] <- "CU"
pova_16$elec_today[pova_16$elec_today == 10] <- "GL"
pova_16$elec_today[pova_16$elec_today == 11] <- "SGP"
pova_16$elec_today[pova_16$elec_today == 12] <- "PvdD"
pova_16$elec_today[pova_16$elec_today == 13] <- "50+"
pova_16$elec_today[pova_16$elec_today == 14] <- "Other"
pova_16$elec_today[pova_16$elec_today == 15] <- "Blanco"


pova_16$party_mem_p[pova_16$party_mem_p == 1] <- "VVD"
pova_16$party_mem_p[pova_16$party_mem_p == 2] <- "PvdA"
pova_16$party_mem_p[pova_16$party_mem_p == 3] <- "PVV"
pova_16$party_mem_p[pova_16$party_mem_p == 4] <- "SP"
pova_16$party_mem_p[pova_16$party_mem_p == 5] <- "CDA"
pova_16$party_mem_p[pova_16$party_mem_p == 6] <- "D66"
pova_16$party_mem_p[pova_16$party_mem_p == 7] <- "CU"
pova_16$party_mem_p[pova_16$party_mem_p == 8] <- "GL"
pova_16$party_mem_p[pova_16$party_mem_p == 9] <- "SGP"
pova_16$party_mem_p[pova_16$party_mem_p == 10] <- "PvdD"
pova_16$party_mem_p[pova_16$party_mem_p == 11] <- "50+"
pova_16$party_mem_p[pova_16$party_mem_p == 12] <- "Other"

pova_16$party_adh_p[pova_16$party_adh_p == 1] <- "VVD"
pova_16$party_adh_p[pova_16$party_adh_p == 2] <- "PvdA"
pova_16$party_adh_p[pova_16$party_adh_p == 3] <- "PVV"
pova_16$party_adh_p[pova_16$party_adh_p == 4] <- "SP"
pova_16$party_adh_p[pova_16$party_adh_p == 5] <- "CDA"
pova_16$party_adh_p[pova_16$party_adh_p == 6] <- "D66"
pova_16$party_adh_p[pova_16$party_adh_p == 7] <- "CU"
pova_16$party_adh_p[pova_16$party_adh_p == 8] <- "GL"
pova_16$party_adh_p[pova_16$party_adh_p == 9] <- "SGP"
pova_16$party_adh_p[pova_16$party_adh_p == 10] <- "PvdD"
pova_16$party_adh_p[pova_16$party_adh_p == 11] <- "50+"
pova_16$party_adh_p[pova_16$party_adh_p == 12] <- "Other"

pova_16$party_att_p[pova_16$party_att_p == 1] <- "VVD"
pova_16$party_att_p[pova_16$party_att_p == 2] <- "PvdA"
pova_16$party_att_p[pova_16$party_att_p == 3] <- "PVV"
pova_16$party_att_p[pova_16$party_att_p == 4] <- "SP"
pova_16$party_att_p[pova_16$party_att_p == 5] <- "CDA"
pova_16$party_att_p[pova_16$party_att_p == 6] <- "D66"
pova_16$party_att_p[pova_16$party_att_p == 7] <- "CU"
pova_16$party_att_p[pova_16$party_att_p == 8] <- "GL"
pova_16$party_att_p[pova_16$party_att_p == 9] <- "SGP"
pova_16$party_att_p[pova_16$party_att_p == 10] <- "PvdD"
pova_16$party_att_p[pova_16$party_att_p == 11] <- "50+"
pova_16$party_att_p[pova_16$party_att_p == 12] <- "Other"




#2017
pova_17 <- pova_17 %>% rename(wave = cv17i_m, #field work period
                              elec_rec = cv17i207, #parliament elections 12 Sep 2012
                              elec_today = cv17i244,
                              term_cda = cv17i215,
                              term_pvda = cv17i212,
                              term_vvd = cv17i211,
                              term_sp = cv17i214,
                              term_gl = cv17i218,
                              term_d66 = cv17i216,
                              term_cu = cv17i217,
                              term_sgp = cv17i219,
                              term_pvv = cv17i213,
                              term_pvdd = cv17i220,
                              term_50 = cv17i221,
                              term_vnl = cv17i265,
                              term_denk = cv17i264,
                              party_mem = cv17i098, #are you a party member?
                              party_mem_p = cv17i237,
                              left_right = cv17i101,
                              redis = cv17i103,
                              immi = cv17i104,
                              eu_uni = cv17i105,
                              party_adh = cv17i199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv17i233,
                              party_att = cv17i200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv17i235,
                              div_cultures = cv17i116,
                              asylum = cv17i118,
                              for_socials = cv17i119,
                              for_many = cv17i120,
                              for_neigh = cv17i123,
                              sat_gov = cv17i030,
                              sat_par = cv17i031,
                              sat_pol = cv17i034,
                              sat_part = cv17i035,
                              pol_int = cv17i012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_vnl, term_denk, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_17$elec_rec[pova_17$elec_rec == 998 | pova_17$elec_rec == 999] <- NA
pova_17$elec_rec[pova_17$elec_rec == 1] <- "VVD"
pova_17$elec_rec[pova_17$elec_rec == 2] <- "PvdA"
pova_17$elec_rec[pova_17$elec_rec == 3] <- "PVV"
pova_17$elec_rec[pova_17$elec_rec == 4] <- "SP"
pova_17$elec_rec[pova_17$elec_rec == 5] <- "CDA"
pova_17$elec_rec[pova_17$elec_rec == 6] <- "D66"
pova_17$elec_rec[pova_17$elec_rec == 7] <- "CU"
pova_17$elec_rec[pova_17$elec_rec == 8] <- "GL"
pova_17$elec_rec[pova_17$elec_rec == 9] <- "SGP"
pova_17$elec_rec[pova_17$elec_rec == 10] <- "PvdD"
pova_17$elec_rec[pova_17$elec_rec == 11] <- "50+"
pova_17$elec_rec[pova_17$elec_rec == 12] <- "Other"
pova_17$elec_rec[pova_17$elec_rec == 13] <- "Blanco"


pova_17$elec_today[pova_17$elec_today == 998 | pova_17$elec_today == 999] <- NA
pova_17$elec_today[pova_17$elec_today == 1] <- "Not"
pova_17$elec_today[pova_17$elec_today == 2] <- "VVD"
pova_17$elec_today[pova_17$elec_today == 3] <- "PvdA"
pova_17$elec_today[pova_17$elec_today == 4] <- "PVV"
pova_17$elec_today[pova_17$elec_today == 5] <- "SP"
pova_17$elec_today[pova_17$elec_today == 6] <- "CDA"
pova_17$elec_today[pova_17$elec_today == 7] <- "D66"
pova_17$elec_today[pova_17$elec_today == 8] <- "CU"
pova_17$elec_today[pova_17$elec_today == 9] <- "GL"
pova_17$elec_today[pova_17$elec_today == 10] <- "SGP"
pova_17$elec_today[pova_17$elec_today == 11] <- "PvdD"
pova_17$elec_today[pova_17$elec_today == 12] <- "50+"
pova_17$elec_today[pova_17$elec_today == 13] <- "DENK"
pova_17$elec_today[pova_17$elec_today == 14] <- "VNL"
pova_17$elec_today[pova_17$elec_today == 15] <- "Other"
pova_17$elec_today[pova_17$elec_today == 16] <- "Blanco"


pova_17$party_mem_p[pova_17$party_mem_p == 1] <- "VVD"
pova_17$party_mem_p[pova_17$party_mem_p == 2] <- "PvdA"
pova_17$party_mem_p[pova_17$party_mem_p == 3] <- "PVV"
pova_17$party_mem_p[pova_17$party_mem_p == 4] <- "SP"
pova_17$party_mem_p[pova_17$party_mem_p == 5] <- "CDA"
pova_17$party_mem_p[pova_17$party_mem_p == 6] <- "D66"
pova_17$party_mem_p[pova_17$party_mem_p == 7] <- "CU"
pova_17$party_mem_p[pova_17$party_mem_p == 8] <- "GL"
pova_17$party_mem_p[pova_17$party_mem_p == 9] <- "SGP"
pova_17$party_mem_p[pova_17$party_mem_p == 10] <- "PvdD"
pova_17$party_mem_p[pova_17$party_mem_p == 11] <- "50+"
pova_17$party_mem_p[pova_17$party_mem_p == 12] <- "Other"

pova_17$party_adh_p[pova_17$party_adh_p == 1] <- "VVD"
pova_17$party_adh_p[pova_17$party_adh_p == 2] <- "PvdA"
pova_17$party_adh_p[pova_17$party_adh_p == 3] <- "PVV"
pova_17$party_adh_p[pova_17$party_adh_p == 4] <- "SP"
pova_17$party_adh_p[pova_17$party_adh_p == 5] <- "CDA"
pova_17$party_adh_p[pova_17$party_adh_p == 6] <- "D66"
pova_17$party_adh_p[pova_17$party_adh_p == 7] <- "CU"
pova_17$party_adh_p[pova_17$party_adh_p == 8] <- "GL"
pova_17$party_adh_p[pova_17$party_adh_p == 9] <- "SGP"
pova_17$party_adh_p[pova_17$party_adh_p == 10] <- "PvdD"
pova_17$party_adh_p[pova_17$party_adh_p == 11] <- "50+"
pova_17$party_adh_p[pova_17$party_adh_p == 12] <- "Other"

pova_17$party_att_p[pova_17$party_att_p == 1] <- "VVD"
pova_17$party_att_p[pova_17$party_att_p == 2] <- "PvdA"
pova_17$party_att_p[pova_17$party_att_p == 3] <- "PVV"
pova_17$party_att_p[pova_17$party_att_p == 4] <- "SP"
pova_17$party_att_p[pova_17$party_att_p == 5] <- "CDA"
pova_17$party_att_p[pova_17$party_att_p == 6] <- "D66"
pova_17$party_att_p[pova_17$party_att_p == 7] <- "CU"
pova_17$party_att_p[pova_17$party_att_p == 8] <- "GL"
pova_17$party_att_p[pova_17$party_att_p == 9] <- "SGP"
pova_17$party_att_p[pova_17$party_att_p == 10] <- "PvdD"
pova_17$party_att_p[pova_17$party_att_p == 11] <- "50+"
pova_17$party_att_p[pova_17$party_att_p == 12] <- "Other"






#2018
pova_18 <- pova_18 %>% rename(wave = cv18j_m1, #field work period
                              elec_rec = cv18j307, #parliament elections 15 March 2017
                              elec_today = cv18j308,
                              term_cda = cv18j215,
                              term_pvda = cv18j212,
                              term_vvd = cv18j211,
                              term_sp = cv18j214,
                              term_gl = cv18j218,
                              term_d66 = cv18j216,
                              term_cu = cv18j217,
                              term_sgp = cv18j219,
                              term_pvv = cv18j213,
                              term_pvdd = cv18j220,
                              term_50 = cv18j221,
                              term_denk = cv18j264,
                              term_fvd = cv18j305,
                              party_mem = cv18j098, #are you a party member?
                              party_mem_p = cv18j311,
                              left_right = cv18j101,
                              redis = cv18j103,
                              immi = cv18j104,
                              eu_uni = cv18j105,
                              party_adh = cv18j199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv18j309,
                              party_att = cv18j200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv18j310,
                              div_cultures = cv18j116,
                              asylum = cv18j118,
                              for_socials = cv18j119,
                              for_many = cv18j120,
                              for_neigh = cv18j123,
                              sat_gov = cv18j030,
                              sat_par = cv18j031,
                              sat_pol = cv18j034,
                              sat_part = cv18j035,
                              pol_int = cv18j012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_18$elec_rec[pova_18$elec_rec == 998 | pova_18$elec_rec == 999] <- NA
pova_18$elec_rec[pova_18$elec_rec == 1] <- "VVD"
pova_18$elec_rec[pova_18$elec_rec == 2] <- "PVV"
pova_18$elec_rec[pova_18$elec_rec == 3] <- "CDA"
pova_18$elec_rec[pova_18$elec_rec == 4] <- "D66"
pova_18$elec_rec[pova_18$elec_rec == 5] <- "GL"
pova_18$elec_rec[pova_18$elec_rec == 6] <- "SP"
pova_18$elec_rec[pova_18$elec_rec == 7] <- "PvdA"
pova_18$elec_rec[pova_18$elec_rec == 8] <- "CU"
pova_18$elec_rec[pova_18$elec_rec == 9] <- "PvdD"
pova_18$elec_rec[pova_18$elec_rec == 10] <- "50+"
pova_18$elec_rec[pova_18$elec_rec == 11] <- "SGP"
pova_18$elec_rec[pova_18$elec_rec == 12] <- "Denk"
pova_18$elec_rec[pova_18$elec_rec == 13] <- "FvD"
pova_18$elec_rec[pova_18$elec_rec == 14] <- "Blanco"
pova_18$elec_rec[pova_18$elec_rec == 15] <- "Other"

pova_18$elec_today[pova_18$elec_today == 998 | pova_18$elec_today == 999] <- NA
pova_18$elec_today[pova_18$elec_today == 1] <- "Not"
pova_18$elec_today[pova_18$elec_today == 2] <- "VVD"
pova_18$elec_today[pova_18$elec_today == 3] <- "PVV"
pova_18$elec_today[pova_18$elec_today == 4] <- "CDA"
pova_18$elec_today[pova_18$elec_today == 5] <- "D66"
pova_18$elec_today[pova_18$elec_today == 6] <- "GL"
pova_18$elec_today[pova_18$elec_today == 7] <- "SP"
pova_18$elec_today[pova_18$elec_today == 8] <- "PvdA"
pova_18$elec_today[pova_18$elec_today == 9] <- "CU"
pova_18$elec_today[pova_18$elec_today == 10] <- "PvdD"
pova_18$elec_today[pova_18$elec_today == 11] <- "50+"
pova_18$elec_today[pova_18$elec_today == 12] <- "SGP"
pova_18$elec_today[pova_18$elec_today == 13] <- "DENK"
pova_18$elec_today[pova_18$elec_today == 14] <- "FvD"
pova_18$elec_today[pova_18$elec_today == 15] <- "Blanco"
pova_18$elec_today[pova_18$elec_today == 16] <- "Other"


pova_18$party_mem_p[pova_18$party_mem_p == 1] <- "VVD"
pova_18$party_mem_p[pova_18$party_mem_p == 2] <- "PVV"
pova_18$party_mem_p[pova_18$party_mem_p == 3] <- "CDA"
pova_18$party_mem_p[pova_18$party_mem_p == 4] <- "D66"
pova_18$party_mem_p[pova_18$party_mem_p == 5] <- "GL"
pova_18$party_mem_p[pova_18$party_mem_p == 6] <- "SP"
pova_18$party_mem_p[pova_18$party_mem_p == 7] <- "PvdA"
pova_18$party_mem_p[pova_18$party_mem_p == 8] <- "CU"
pova_18$party_mem_p[pova_18$party_mem_p == 9] <- "PvdD"
pova_18$party_mem_p[pova_18$party_mem_p == 10] <- "50+"
pova_18$party_mem_p[pova_18$party_mem_p == 11] <- "SGP"
pova_18$party_mem_p[pova_18$party_mem_p == 12] <- "DENK"
pova_18$party_mem_p[pova_18$party_mem_p == 13] <- "FvD"
pova_18$party_mem_p[pova_18$party_mem_p == 14] <- "Other"

pova_18$party_adh_p[pova_18$party_adh_p == 1] <- "VVD"
pova_18$party_adh_p[pova_18$party_adh_p == 2] <- "PVV"
pova_18$party_adh_p[pova_18$party_adh_p == 3] <- "CDA"
pova_18$party_adh_p[pova_18$party_adh_p == 4] <- "D66"
pova_18$party_adh_p[pova_18$party_adh_p == 5] <- "GL"
pova_18$party_adh_p[pova_18$party_adh_p == 6] <- "SP"
pova_18$party_adh_p[pova_18$party_adh_p == 7] <- "PvdA"
pova_18$party_adh_p[pova_18$party_adh_p == 8] <- "CU"
pova_18$party_adh_p[pova_18$party_adh_p == 9] <- "PvdD"
pova_18$party_adh_p[pova_18$party_adh_p == 10] <- "50+"
pova_18$party_adh_p[pova_18$party_adh_p == 11] <- "SGP"
pova_18$party_adh_p[pova_18$party_adh_p == 12] <- "DENK"
pova_18$party_adh_p[pova_18$party_adh_p == 13] <- "FvD"
pova_18$party_adh_p[pova_18$party_adh_p == 14] <- "Other"

pova_18$party_att_p[pova_18$party_att_p == 1] <- "VVD"
pova_18$party_att_p[pova_18$party_att_p == 2] <- "PVV"
pova_18$party_att_p[pova_18$party_att_p == 3] <- "CDA"
pova_18$party_att_p[pova_18$party_att_p == 4] <- "D66"
pova_18$party_att_p[pova_18$party_att_p == 5] <- "GL"
pova_18$party_att_p[pova_18$party_att_p == 6] <- "SP"
pova_18$party_att_p[pova_18$party_att_p == 7] <- "PvdA"
pova_18$party_att_p[pova_18$party_att_p == 8] <- "CU"
pova_18$party_att_p[pova_18$party_att_p == 9] <- "PvdD"
pova_18$party_att_p[pova_18$party_att_p == 10] <- "50+"
pova_18$party_att_p[pova_18$party_att_p == 11] <- "SGP"
pova_18$party_att_p[pova_18$party_att_p == 12] <- "DENK"
pova_18$party_att_p[pova_18$party_att_p == 13] <- "FvD"
pova_18$party_att_p[pova_18$party_att_p == 14] <- "Other"



#2019
pova_19 <- pova_19 %>% rename(wave = cv19k_m1, #field work period
                              elec_rec = cv19k307, #parliament elections 15 March 2017
                              elec_today = cv19k308,
                              term_cda = cv19k215,
                              term_pvda = cv19k212,
                              term_vvd = cv19k211,
                              term_sp = cv19k214,
                              term_gl = cv19k218,
                              term_d66 = cv19k216,
                              term_cu = cv19k217,
                              term_sgp = cv19k219,
                              term_pvv = cv19k213,
                              term_pvdd = cv19k220,
                              term_50 = cv19k221,
                              term_denk = cv19k264,
                              term_fvd = cv19k305,
                              party_mem = cv19k098, #are you a party member?
                              party_mem_p = cv19k311,
                              left_right = cv19k101,
                              redis = cv19k103,
                              immi = cv19k104,
                              eu_uni = cv19k105,
                              party_adh = cv19k199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv19k309,
                              party_att = cv19k200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv19k310,
                              div_cultures = cv19k116,
                              asylum = cv19k118,
                              for_socials = cv19k119,
                              for_many = cv19k120,
                              for_neigh = cv19k123,
                              sat_gov = cv19k030,
                              sat_par = cv19k031,
                              sat_pol = cv19k034,
                              sat_part = cv19k035,
                              pol_int = cv19k012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_19$elec_rec[pova_19$elec_rec == 998 | pova_19$elec_rec == 999] <- NA
pova_19$elec_rec[pova_19$elec_rec == 1] <- "VVD"
pova_19$elec_rec[pova_19$elec_rec == 2] <- "PVV"
pova_19$elec_rec[pova_19$elec_rec == 3] <- "CDA"
pova_19$elec_rec[pova_19$elec_rec == 4] <- "D66"
pova_19$elec_rec[pova_19$elec_rec == 5] <- "GL"
pova_19$elec_rec[pova_19$elec_rec == 6] <- "SP"
pova_19$elec_rec[pova_19$elec_rec == 7] <- "PvdA"
pova_19$elec_rec[pova_19$elec_rec == 8] <- "CU"
pova_19$elec_rec[pova_19$elec_rec == 9] <- "PvdD"
pova_19$elec_rec[pova_19$elec_rec == 10] <- "50+"
pova_19$elec_rec[pova_19$elec_rec == 11] <- "SGP"
pova_19$elec_rec[pova_19$elec_rec == 12] <- "Denk"
pova_19$elec_rec[pova_19$elec_rec == 13] <- "FvD"
pova_19$elec_rec[pova_19$elec_rec == 14] <- "Blanco"
pova_19$elec_rec[pova_19$elec_rec == 15] <- "Other"

pova_19$elec_today[pova_19$elec_today == 998 | pova_19$elec_today == 999] <- NA
pova_19$elec_today[pova_19$elec_today == 1] <- "Not"
pova_19$elec_today[pova_19$elec_today == 2] <- "VVD"
pova_19$elec_today[pova_19$elec_today == 3] <- "PVV"
pova_19$elec_today[pova_19$elec_today == 4] <- "CDA"
pova_19$elec_today[pova_19$elec_today == 5] <- "D66"
pova_19$elec_today[pova_19$elec_today == 6] <- "GL"
pova_19$elec_today[pova_19$elec_today == 7] <- "SP"
pova_19$elec_today[pova_19$elec_today == 8] <- "PvdA"
pova_19$elec_today[pova_19$elec_today == 9] <- "CU"
pova_19$elec_today[pova_19$elec_today == 10] <- "PvdD"
pova_19$elec_today[pova_19$elec_today == 11] <- "50+"
pova_19$elec_today[pova_19$elec_today == 12] <- "SGP"
pova_19$elec_today[pova_19$elec_today == 13] <- "DENK"
pova_19$elec_today[pova_19$elec_today == 14] <- "FvD"
pova_19$elec_today[pova_19$elec_today == 15] <- "Blanco"
pova_19$elec_today[pova_19$elec_today == 16] <- "Other"


pova_19$party_mem_p[pova_19$party_mem_p == 1] <- "VVD"
pova_19$party_mem_p[pova_19$party_mem_p == 2] <- "PVV"
pova_19$party_mem_p[pova_19$party_mem_p == 3] <- "CDA"
pova_19$party_mem_p[pova_19$party_mem_p == 4] <- "D66"
pova_19$party_mem_p[pova_19$party_mem_p == 5] <- "GL"
pova_19$party_mem_p[pova_19$party_mem_p == 6] <- "SP"
pova_19$party_mem_p[pova_19$party_mem_p == 7] <- "PvdA"
pova_19$party_mem_p[pova_19$party_mem_p == 8] <- "CU"
pova_19$party_mem_p[pova_19$party_mem_p == 9] <- "PvdD"
pova_19$party_mem_p[pova_19$party_mem_p == 10] <- "50+"
pova_19$party_mem_p[pova_19$party_mem_p == 11] <- "SGP"
pova_19$party_mem_p[pova_19$party_mem_p == 12] <- "DENK"
pova_19$party_mem_p[pova_19$party_mem_p == 13] <- "FvD"
pova_19$party_mem_p[pova_19$party_mem_p == 14] <- "Other"

pova_19$party_adh_p[pova_19$party_adh_p == 1] <- "VVD"
pova_19$party_adh_p[pova_19$party_adh_p == 2] <- "PVV"
pova_19$party_adh_p[pova_19$party_adh_p == 3] <- "CDA"
pova_19$party_adh_p[pova_19$party_adh_p == 4] <- "D66"
pova_19$party_adh_p[pova_19$party_adh_p == 5] <- "GL"
pova_19$party_adh_p[pova_19$party_adh_p == 6] <- "SP"
pova_19$party_adh_p[pova_19$party_adh_p == 7] <- "PvdA"
pova_19$party_adh_p[pova_19$party_adh_p == 8] <- "CU"
pova_19$party_adh_p[pova_19$party_adh_p == 9] <- "PvdD"
pova_19$party_adh_p[pova_19$party_adh_p == 10] <- "50+"
pova_19$party_adh_p[pova_19$party_adh_p == 11] <- "SGP"
pova_19$party_adh_p[pova_19$party_adh_p == 12] <- "DENK"
pova_19$party_adh_p[pova_19$party_adh_p == 13] <- "FvD"
pova_19$party_adh_p[pova_19$party_adh_p == 14] <- "Other"

pova_19$party_att_p[pova_19$party_att_p == 1] <- "VVD"
pova_19$party_att_p[pova_19$party_att_p == 2] <- "PVV"
pova_19$party_att_p[pova_19$party_att_p == 3] <- "CDA"
pova_19$party_att_p[pova_19$party_att_p == 4] <- "D66"
pova_19$party_att_p[pova_19$party_att_p == 5] <- "GL"
pova_19$party_att_p[pova_19$party_att_p == 6] <- "SP"
pova_19$party_att_p[pova_19$party_att_p == 7] <- "PvdA"
pova_19$party_att_p[pova_19$party_att_p == 8] <- "CU"
pova_19$party_att_p[pova_19$party_att_p == 9] <- "PvdD"
pova_19$party_att_p[pova_19$party_att_p == 10] <- "50+"
pova_19$party_att_p[pova_19$party_att_p == 11] <- "SGP"
pova_19$party_att_p[pova_19$party_att_p == 12] <- "DENK"
pova_19$party_att_p[pova_19$party_att_p == 13] <- "FvD"
pova_19$party_att_p[pova_19$party_att_p == 14] <- "Other"





#2020
pova_20 <- pova_20 %>% rename(wave = cv20l_m1, #field work period
                              elec_rec = cv20l307, #parliament elections 15 March 2017
                              elec_today = cv20l308,
                              term_cda = cv20l215,
                              term_pvda = cv20l212,
                              term_vvd = cv20l211,
                              term_sp = cv20l214,
                              term_gl = cv20l218,
                              term_d66 = cv20l216,
                              term_cu = cv20l217,
                              term_sgp = cv20l219,
                              term_pvv = cv20l213,
                              term_pvdd = cv20l220,
                              term_50 = cv20l221,
                              term_denk = cv20l264,
                              term_fvd = cv20l305,
                              party_mem = cv20l098, #are you a party member?
                              party_mem_p = cv20l311,
                              left_right = cv20l101,
                              redis = cv20l103,
                              immi = cv20l104,
                              eu_uni = cv20l105,
                              party_adh = cv20l199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv20l309,
                              party_att = cv20l200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv20l310,
                              div_cultures = cv20l116,
                              asylum = cv20l118,
                              for_socials = cv20l119,
                              for_many = cv20l120,
                              for_neigh = cv20l123,
                              sat_gov = cv20l030,
                              sat_par = cv20l031,
                              sat_pol = cv20l034,
                              sat_part = cv20l035,
                              pol_int = cv20l012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_20$elec_rec[pova_20$elec_rec == -9 | pova_20$elec_rec == -8] <- NA
pova_20$elec_rec[pova_20$elec_rec == 1] <- "VVD"
pova_20$elec_rec[pova_20$elec_rec == 2] <- "PVV"
pova_20$elec_rec[pova_20$elec_rec == 3] <- "CDA"
pova_20$elec_rec[pova_20$elec_rec == 4] <- "D66"
pova_20$elec_rec[pova_20$elec_rec == 5] <- "GL"
pova_20$elec_rec[pova_20$elec_rec == 6] <- "SP"
pova_20$elec_rec[pova_20$elec_rec == 7] <- "PvdA"
pova_20$elec_rec[pova_20$elec_rec == 8] <- "CU"
pova_20$elec_rec[pova_20$elec_rec == 9] <- "PvdD"
pova_20$elec_rec[pova_20$elec_rec == 10] <- "50+"
pova_20$elec_rec[pova_20$elec_rec == 11] <- "SGP"
pova_20$elec_rec[pova_20$elec_rec == 12] <- "Denk"
pova_20$elec_rec[pova_20$elec_rec == 13] <- "FvD"
pova_20$elec_rec[pova_20$elec_rec == 14] <- "Blanco"
pova_20$elec_rec[pova_20$elec_rec == 15] <- "Other"

pova_20$elec_today[pova_20$elec_today == -8 | pova_20$elec_today == -9] <- NA
pova_20$elec_today[pova_20$elec_today == 1] <- "Not"
pova_20$elec_today[pova_20$elec_today == 2] <- "VVD"
pova_20$elec_today[pova_20$elec_today == 3] <- "PVV"
pova_20$elec_today[pova_20$elec_today == 4] <- "CDA"
pova_20$elec_today[pova_20$elec_today == 5] <- "D66"
pova_20$elec_today[pova_20$elec_today == 6] <- "GL"
pova_20$elec_today[pova_20$elec_today == 7] <- "SP"
pova_20$elec_today[pova_20$elec_today == 8] <- "PvdA"
pova_20$elec_today[pova_20$elec_today == 9] <- "CU"
pova_20$elec_today[pova_20$elec_today == 10] <- "PvdD"
pova_20$elec_today[pova_20$elec_today == 11] <- "50+"
pova_20$elec_today[pova_20$elec_today == 12] <- "SGP"
pova_20$elec_today[pova_20$elec_today == 13] <- "DENK"
pova_20$elec_today[pova_20$elec_today == 14] <- "FvD"
pova_20$elec_today[pova_20$elec_today == 15] <- "Blanco"
pova_20$elec_today[pova_20$elec_today == 16] <- "Other"


pova_20$party_mem_p[pova_20$party_mem_p == 1] <- "VVD"
pova_20$party_mem_p[pova_20$party_mem_p == 2] <- "PVV"
pova_20$party_mem_p[pova_20$party_mem_p == 3] <- "CDA"
pova_20$party_mem_p[pova_20$party_mem_p == 4] <- "D66"
pova_20$party_mem_p[pova_20$party_mem_p == 5] <- "GL"
pova_20$party_mem_p[pova_20$party_mem_p == 6] <- "SP"
pova_20$party_mem_p[pova_20$party_mem_p == 7] <- "PvdA"
pova_20$party_mem_p[pova_20$party_mem_p == 8] <- "CU"
pova_20$party_mem_p[pova_20$party_mem_p == 9] <- "PvdD"
pova_20$party_mem_p[pova_20$party_mem_p == 10] <- "50+"
pova_20$party_mem_p[pova_20$party_mem_p == 11] <- "SGP"
pova_20$party_mem_p[pova_20$party_mem_p == 12] <- "DENK"
pova_20$party_mem_p[pova_20$party_mem_p == 13] <- "FvD"
pova_20$party_mem_p[pova_20$party_mem_p == 14] <- "Other"

pova_20$party_adh_p[pova_20$party_adh_p == 1] <- "VVD"
pova_20$party_adh_p[pova_20$party_adh_p == 2] <- "PVV"
pova_20$party_adh_p[pova_20$party_adh_p == 3] <- "CDA"
pova_20$party_adh_p[pova_20$party_adh_p == 4] <- "D66"
pova_20$party_adh_p[pova_20$party_adh_p == 5] <- "GL"
pova_20$party_adh_p[pova_20$party_adh_p == 6] <- "SP"
pova_20$party_adh_p[pova_20$party_adh_p == 7] <- "PvdA"
pova_20$party_adh_p[pova_20$party_adh_p == 8] <- "CU"
pova_20$party_adh_p[pova_20$party_adh_p == 9] <- "PvdD"
pova_20$party_adh_p[pova_20$party_adh_p == 10] <- "50+"
pova_20$party_adh_p[pova_20$party_adh_p == 11] <- "SGP"
pova_20$party_adh_p[pova_20$party_adh_p == 12] <- "DENK"
pova_20$party_adh_p[pova_20$party_adh_p == 13] <- "FvD"
pova_20$party_adh_p[pova_20$party_adh_p == 14] <- "Other"

pova_20$party_att_p[pova_20$party_att_p == 1] <- "VVD"
pova_20$party_att_p[pova_20$party_att_p == 2] <- "PVV"
pova_20$party_att_p[pova_20$party_att_p == 3] <- "CDA"
pova_20$party_att_p[pova_20$party_att_p == 4] <- "D66"
pova_20$party_att_p[pova_20$party_att_p == 5] <- "GL"
pova_20$party_att_p[pova_20$party_att_p == 6] <- "SP"
pova_20$party_att_p[pova_20$party_att_p == 7] <- "PvdA"
pova_20$party_att_p[pova_20$party_att_p == 8] <- "CU"
pova_20$party_att_p[pova_20$party_att_p == 9] <- "PvdD"
pova_20$party_att_p[pova_20$party_att_p == 10] <- "50+"
pova_20$party_att_p[pova_20$party_att_p == 11] <- "SGP"
pova_20$party_att_p[pova_20$party_att_p == 12] <- "DENK"
pova_20$party_att_p[pova_20$party_att_p == 13] <- "FvD"
pova_20$party_att_p[pova_20$party_att_p == 14] <- "Other"







#2021
pova_21 <- pova_21 %>% rename(wave = cv21m_m1, #field work period
                              elec_rec = cv21m307, #parliament elections 15 March 2017
                              elec_today = cv21m308,
                              term_cda = cv21m215,
                              term_pvda = cv21m212,
                              term_vvd = cv21m211,
                              term_sp = cv21m214,
                              term_gl = cv21m218,
                              term_d66 = cv21m216,
                              term_cu = cv21m217,
                              term_sgp = cv21m219,
                              term_pvv = cv21m213,
                              term_pvdd = cv21m220,
                              term_50 = cv21m221,
                              term_denk = cv21m264,
                              term_fvd = cv21m305,
                              party_mem = cv21m098, #are you a party member?
                              party_mem_p = cv21m311,
                              left_right = cv21m101,
                              redis = cv21m103,
                              immi = cv21m104,
                              eu_uni = cv21m105,
                              party_adh = cv21m199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv21m309,
                              party_att = cv21m200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv21m310,
                              div_cultures = cv21m116,
                              asylum = cv21m118,
                              for_socials = cv21m119,
                              for_many = cv21m120,
                              for_neigh = cv21m123,
                              sat_gov = cv21m030,
                              sat_par = cv21m031,
                              sat_pol = cv21m034,
                              sat_part = cv21m035,
                              pol_int = cv21m012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_21$elec_rec[pova_21$elec_rec == -9 | pova_21$elec_rec == -8] <- NA
pova_21$elec_rec[pova_21$elec_rec == 1] <- "VVD"
pova_21$elec_rec[pova_21$elec_rec == 2] <- "PVV"
pova_21$elec_rec[pova_21$elec_rec == 3] <- "CDA"
pova_21$elec_rec[pova_21$elec_rec == 4] <- "D66"
pova_21$elec_rec[pova_21$elec_rec == 5] <- "GL"
pova_21$elec_rec[pova_21$elec_rec == 6] <- "SP"
pova_21$elec_rec[pova_21$elec_rec == 7] <- "PvdA"
pova_21$elec_rec[pova_21$elec_rec == 8] <- "CU"
pova_21$elec_rec[pova_21$elec_rec == 9] <- "PvdD"
pova_21$elec_rec[pova_21$elec_rec == 10] <- "50+"
pova_21$elec_rec[pova_21$elec_rec == 11] <- "SGP"
pova_21$elec_rec[pova_21$elec_rec == 12] <- "Denk"
pova_21$elec_rec[pova_21$elec_rec == 13] <- "FvD"
pova_21$elec_rec[pova_21$elec_rec == 14] <- "Blanco"
pova_21$elec_rec[pova_21$elec_rec == 15] <- "Other"

pova_21$elec_today[pova_21$elec_today == -8 | pova_21$elec_today == -9] <- NA
pova_21$elec_today[pova_21$elec_today == 1] <- "Not"
pova_21$elec_today[pova_21$elec_today == 2] <- "VVD"
pova_21$elec_today[pova_21$elec_today == 3] <- "PVV"
pova_21$elec_today[pova_21$elec_today == 4] <- "CDA"
pova_21$elec_today[pova_21$elec_today == 5] <- "D66"
pova_21$elec_today[pova_21$elec_today == 6] <- "GL"
pova_21$elec_today[pova_21$elec_today == 7] <- "SP"
pova_21$elec_today[pova_21$elec_today == 8] <- "PvdA"
pova_21$elec_today[pova_21$elec_today == 9] <- "CU"
pova_21$elec_today[pova_21$elec_today == 10] <- "PvdD"
pova_21$elec_today[pova_21$elec_today == 11] <- "50+"
pova_21$elec_today[pova_21$elec_today == 12] <- "SGP"
pova_21$elec_today[pova_21$elec_today == 13] <- "DENK"
pova_21$elec_today[pova_21$elec_today == 14] <- "FvD"
pova_21$elec_today[pova_21$elec_today == 15] <- "Blanco"
pova_21$elec_today[pova_21$elec_today == 16] <- "Other"


pova_21$party_mem_p[pova_21$party_mem_p == 1] <- "VVD"
pova_21$party_mem_p[pova_21$party_mem_p == 2] <- "PVV"
pova_21$party_mem_p[pova_21$party_mem_p == 3] <- "CDA"
pova_21$party_mem_p[pova_21$party_mem_p == 4] <- "D66"
pova_21$party_mem_p[pova_21$party_mem_p == 5] <- "GL"
pova_21$party_mem_p[pova_21$party_mem_p == 6] <- "SP"
pova_21$party_mem_p[pova_21$party_mem_p == 7] <- "PvdA"
pova_21$party_mem_p[pova_21$party_mem_p == 8] <- "CU"
pova_21$party_mem_p[pova_21$party_mem_p == 9] <- "PvdD"
pova_21$party_mem_p[pova_21$party_mem_p == 10] <- "50+"
pova_21$party_mem_p[pova_21$party_mem_p == 11] <- "SGP"
pova_21$party_mem_p[pova_21$party_mem_p == 12] <- "DENK"
pova_21$party_mem_p[pova_21$party_mem_p == 13] <- "FvD"
pova_21$party_mem_p[pova_21$party_mem_p == 14] <- "Other"

pova_21$party_adh_p[pova_21$party_adh_p == 1] <- "VVD"
pova_21$party_adh_p[pova_21$party_adh_p == 2] <- "PVV"
pova_21$party_adh_p[pova_21$party_adh_p == 3] <- "CDA"
pova_21$party_adh_p[pova_21$party_adh_p == 4] <- "D66"
pova_21$party_adh_p[pova_21$party_adh_p == 5] <- "GL"
pova_21$party_adh_p[pova_21$party_adh_p == 6] <- "SP"
pova_21$party_adh_p[pova_21$party_adh_p == 7] <- "PvdA"
pova_21$party_adh_p[pova_21$party_adh_p == 8] <- "CU"
pova_21$party_adh_p[pova_21$party_adh_p == 9] <- "PvdD"
pova_21$party_adh_p[pova_21$party_adh_p == 10] <- "50+"
pova_21$party_adh_p[pova_21$party_adh_p == 11] <- "SGP"
pova_21$party_adh_p[pova_21$party_adh_p == 12] <- "DENK"
pova_21$party_adh_p[pova_21$party_adh_p == 13] <- "FvD"
pova_21$party_adh_p[pova_21$party_adh_p == 14] <- "Other"

pova_21$party_att_p[pova_21$party_att_p == 1] <- "VVD"
pova_21$party_att_p[pova_21$party_att_p == 2] <- "PVV"
pova_21$party_att_p[pova_21$party_att_p == 3] <- "CDA"
pova_21$party_att_p[pova_21$party_att_p == 4] <- "D66"
pova_21$party_att_p[pova_21$party_att_p == 5] <- "GL"
pova_21$party_att_p[pova_21$party_att_p == 6] <- "SP"
pova_21$party_att_p[pova_21$party_att_p == 7] <- "PvdA"
pova_21$party_att_p[pova_21$party_att_p == 8] <- "CU"
pova_21$party_att_p[pova_21$party_att_p == 9] <- "PvdD"
pova_21$party_att_p[pova_21$party_att_p == 10] <- "50+"
pova_21$party_att_p[pova_21$party_att_p == 11] <- "SGP"
pova_21$party_att_p[pova_21$party_att_p == 12] <- "DENK"
pova_21$party_att_p[pova_21$party_att_p == 13] <- "FvD"
pova_21$party_att_p[pova_21$party_att_p == 14] <- "Other"






#2022
pova_22 <- pova_22 %>% rename(wave = cv22n_m1, #field work period
                              elec_rec = cv22n307, #parliament elections 15 March 2017
                              elec_today = cv22n308,
                              term_cda = cv22n215,
                              term_pvda = cv22n212,
                              term_vvd = cv22n211,
                              term_sp = cv22n214,
                              term_gl = cv22n218,
                              term_d66 = cv22n216,
                              term_cu = cv22n217,
                              term_sgp = cv22n219,
                              term_pvv = cv22n213,
                              term_pvdd = cv22n220,
                              term_50 = cv22n221,
                              term_denk = cv22n264,
                              term_fvd = cv22n305,
                              party_mem = cv22n098, #are you a party member?
                              party_mem_p = cv22n311,
                              left_right = cv22n101,
                              redis = cv22n103,
                              immi = cv22n104,
                              eu_uni = cv22n105,
                              party_adh = cv22n199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv22n309,
                              party_att = cv22n200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv22n310,
                              div_cultures = cv22n116,
                              asylum = cv22n118,
                              for_socials = cv22n119,
                              for_many = cv22n120,
                              for_neigh = cv22n123,
                              sat_gov = cv22n030,
                              sat_par = cv22n031,
                              sat_pol = cv22n034,
                              sat_part = cv22n035,
                              pol_int = cv22n012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_22$elec_rec[pova_22$elec_rec == -9 | pova_22$elec_rec == -8] <- NA
pova_22$elec_rec[pova_22$elec_rec == 1] <- "VVD"
pova_22$elec_rec[pova_22$elec_rec == 2] <- "PVV"
pova_22$elec_rec[pova_22$elec_rec == 3] <- "CDA"
pova_22$elec_rec[pova_22$elec_rec == 4] <- "D66"
pova_22$elec_rec[pova_22$elec_rec == 5] <- "GL"
pova_22$elec_rec[pova_22$elec_rec == 6] <- "SP"
pova_22$elec_rec[pova_22$elec_rec == 7] <- "PvdA"
pova_22$elec_rec[pova_22$elec_rec == 8] <- "CU"
pova_22$elec_rec[pova_22$elec_rec == 9] <- "PvdD"
pova_22$elec_rec[pova_22$elec_rec == 10] <- "50+"
pova_22$elec_rec[pova_22$elec_rec == 11] <- "SGP"
pova_22$elec_rec[pova_22$elec_rec == 12] <- "Denk"
pova_22$elec_rec[pova_22$elec_rec == 13] <- "FvD"
pova_22$elec_rec[pova_22$elec_rec == 14] <- "Blanco"
pova_22$elec_rec[pova_22$elec_rec == 15] <- "Other"
pova_22$elec_rec[pova_22$elec_rec == 16] <- "Volt"
pova_22$elec_rec[pova_22$elec_rec == 17] <- "JA21"
pova_22$elec_rec[pova_22$elec_rec == 18] <- "BBB"
pova_22$elec_rec[pova_22$elec_rec == 19] <- "Bij1"


pova_22$elec_today[pova_22$elec_today == -8 | pova_22$elec_today == -9] <- NA
pova_22$elec_today[pova_22$elec_today == 1] <- "Not"
pova_22$elec_today[pova_22$elec_today == 2] <- "VVD"
pova_22$elec_today[pova_22$elec_today == 3] <- "PVV"
pova_22$elec_today[pova_22$elec_today == 4] <- "CDA"
pova_22$elec_today[pova_22$elec_today == 5] <- "D66"
pova_22$elec_today[pova_22$elec_today == 6] <- "GL"
pova_22$elec_today[pova_22$elec_today == 7] <- "SP"
pova_22$elec_today[pova_22$elec_today == 8] <- "PvdA"
pova_22$elec_today[pova_22$elec_today == 9] <- "CU"
pova_22$elec_today[pova_22$elec_today == 10] <- "PvdD"
pova_22$elec_today[pova_22$elec_today == 11] <- "50+"
pova_22$elec_today[pova_22$elec_today == 12] <- "SGP"
pova_22$elec_today[pova_22$elec_today == 13] <- "DENK"
pova_22$elec_today[pova_22$elec_today == 14] <- "FvD"
pova_22$elec_today[pova_22$elec_today == 15] <- "Blanco"
pova_22$elec_today[pova_22$elec_today == 16] <- "Other"
pova_22$elec_today[pova_22$elec_today == 17] <- "Volt"
pova_22$elec_today[pova_22$elec_today == 18] <- "JA21"
pova_22$elec_today[pova_22$elec_today == 19] <- "BBB"
pova_22$elec_today[pova_22$elec_today == 20] <- "Bij1"

pova_22$party_mem_p[pova_22$party_mem_p == 1] <- "VVD"
pova_22$party_mem_p[pova_22$party_mem_p == 2] <- "PVV"
pova_22$party_mem_p[pova_22$party_mem_p == 3] <- "CDA"
pova_22$party_mem_p[pova_22$party_mem_p == 4] <- "D66"
pova_22$party_mem_p[pova_22$party_mem_p == 5] <- "GL"
pova_22$party_mem_p[pova_22$party_mem_p == 6] <- "SP"
pova_22$party_mem_p[pova_22$party_mem_p == 7] <- "PvdA"
pova_22$party_mem_p[pova_22$party_mem_p == 8] <- "CU"
pova_22$party_mem_p[pova_22$party_mem_p == 9] <- "PvdD"
pova_22$party_mem_p[pova_22$party_mem_p == 10] <- "50+"
pova_22$party_mem_p[pova_22$party_mem_p == 11] <- "SGP"
pova_22$party_mem_p[pova_22$party_mem_p == 12] <- "DENK"
pova_22$party_mem_p[pova_22$party_mem_p == 13] <- "FvD"
pova_22$party_mem_p[pova_22$party_mem_p == 14] <- "Other"
pova_22$party_mem_p[pova_22$party_mem_p == 16] <- "JA21"
pova_22$party_mem_p[pova_22$party_mem_p == 17] <- "BBB"
pova_22$party_mem_p[pova_22$party_mem_p == 18] <- "Bij1"

pova_22$party_adh_p[pova_22$party_adh_p == 1] <- "VVD"
pova_22$party_adh_p[pova_22$party_adh_p == 2] <- "PVV"
pova_22$party_adh_p[pova_22$party_adh_p == 3] <- "CDA"
pova_22$party_adh_p[pova_22$party_adh_p == 4] <- "D66"
pova_22$party_adh_p[pova_22$party_adh_p == 5] <- "GL"
pova_22$party_adh_p[pova_22$party_adh_p == 6] <- "SP"
pova_22$party_adh_p[pova_22$party_adh_p == 7] <- "PvdA"
pova_22$party_adh_p[pova_22$party_adh_p == 8] <- "CU"
pova_22$party_adh_p[pova_22$party_adh_p == 9] <- "PvdD"
pova_22$party_adh_p[pova_22$party_adh_p == 10] <- "50+"
pova_22$party_adh_p[pova_22$party_adh_p == 11] <- "SGP"
pova_22$party_adh_p[pova_22$party_adh_p == 12] <- "DENK"
pova_22$party_adh_p[pova_22$party_adh_p == 13] <- "FvD"
pova_22$party_adh_p[pova_22$party_adh_p == 14] <- "Other"
pova_22$party_adh_p[pova_22$party_adh_p == 15] <- "Volt"
pova_22$party_adh_p[pova_22$party_adh_p == 16] <- "JA21"
pova_22$party_adh_p[pova_22$party_adh_p == 17] <- "BBB"
pova_22$party_adh_p[pova_22$party_adh_p == 18] <- "Bij1"


pova_22$party_att_p[pova_22$party_att_p == 1] <- "VVD"
pova_22$party_att_p[pova_22$party_att_p == 2] <- "PVV"
pova_22$party_att_p[pova_22$party_att_p == 3] <- "CDA"
pova_22$party_att_p[pova_22$party_att_p == 4] <- "D66"
pova_22$party_att_p[pova_22$party_att_p == 5] <- "GL"
pova_22$party_att_p[pova_22$party_att_p == 6] <- "SP"
pova_22$party_att_p[pova_22$party_att_p == 7] <- "PvdA"
pova_22$party_att_p[pova_22$party_att_p == 8] <- "CU"
pova_22$party_att_p[pova_22$party_att_p == 9] <- "PvdD"
pova_22$party_att_p[pova_22$party_att_p == 10] <- "50+"
pova_22$party_att_p[pova_22$party_att_p == 11] <- "SGP"
pova_22$party_att_p[pova_22$party_att_p == 12] <- "DENK"
pova_22$party_att_p[pova_22$party_att_p == 13] <- "FvD"
pova_22$party_att_p[pova_22$party_att_p == 14] <- "Other"
pova_22$party_att_p[pova_22$party_att_p == 15] <- "Volt"
pova_22$party_att_p[pova_22$party_att_p == 16] <- "JA21"
pova_22$party_att_p[pova_22$party_att_p == 17] <- "BBB"
pova_22$party_att_p[pova_22$party_att_p == 18] <- "Bij1"









#2023
pova_23 <- pova_23 %>% rename(wave = cv23o_m1, 
                              elec_rec = cv23o307, 
                              elec_today = cv23o308,
                              term_cda = cv23o215,
                              term_pvda = cv23o212,
                              term_vvd = cv23o211,
                              term_sp = cv23o214,
                              term_gl = cv23o218,
                              term_d66 = cv23o216,
                              term_cu = cv23o217,
                              term_sgp = cv23o219,
                              term_pvv = cv23o213,
                              term_pvdd = cv23o220,
                              term_50 = cv23o221,
                              term_denk = cv23o264,
                              term_fvd = cv23o305,
                              party_mem = cv23o098, #are you a party member?
                              party_mem_p = cv23o311,
                              left_right = cv23o101,
                              redis = cv23o103,
                              immi = cv23o104,
                              eu_uni = cv23o105,
                              party_adh = cv23o199, #do you feel more adherent about a party? 1 is yes
                              party_adh_p = cv23o309,
                              party_att = cv23o200, #do you feel more attracted to a party? 1 is yes
                              party_att_p = cv23o310,
                              div_cultures = cv23o116,
                              asylum = cv23o118,
                              for_socials = cv23o119,
                              for_many = cv23o120,
                              for_neigh = cv23o123,
                              sat_gov = cv23o030,
                              sat_par = cv23o031,
                              sat_pol = cv23o034,
                              sat_part = cv23o035,
                              pol_int = cv23o012) %>% 
  select(c(nomem_encr, year, wave, elec_rec, elec_today, term_cda, term_pvda,
           term_vvd, term_sp, term_gl, term_d66, term_cu, term_sgp, 
           term_pvv, term_pvdd, term_50, term_denk, term_fvd, party_mem, party_mem_p, left_right,
           redis, immi, eu_uni, party_adh, party_adh_p, party_att, party_att_p, 
           div_cultures, asylum, for_socials, for_many, for_neigh,
           sat_gov, sat_par, sat_pol, sat_part, pol_int)) 


pova_23$elec_rec[pova_23$elec_rec == -9 | pova_23$elec_rec == -8] <- NA
pova_23$elec_rec[pova_23$elec_rec == 1] <- "VVD"
pova_23$elec_rec[pova_23$elec_rec == 2] <- "PVV"
pova_23$elec_rec[pova_23$elec_rec == 3] <- "CDA"
pova_23$elec_rec[pova_23$elec_rec == 4] <- "D66"
pova_23$elec_rec[pova_23$elec_rec == 5] <- "GL"
pova_23$elec_rec[pova_23$elec_rec == 6] <- "SP"
pova_23$elec_rec[pova_23$elec_rec == 7] <- "PvdA"
pova_23$elec_rec[pova_23$elec_rec == 8] <- "CU"
pova_23$elec_rec[pova_23$elec_rec == 9] <- "PvdD"
pova_23$elec_rec[pova_23$elec_rec == 10] <- "50+"
pova_23$elec_rec[pova_23$elec_rec == 11] <- "SGP"
pova_23$elec_rec[pova_23$elec_rec == 12] <- "Denk"
pova_23$elec_rec[pova_23$elec_rec == 13] <- "FvD"
pova_23$elec_rec[pova_23$elec_rec == 14] <- "Blanco"
pova_23$elec_rec[pova_23$elec_rec == 15] <- "Other"
pova_23$elec_rec[pova_23$elec_rec == 16] <- "Volt"
pova_23$elec_rec[pova_23$elec_rec == 17] <- "JA21"
pova_23$elec_rec[pova_23$elec_rec == 18] <- "BBB"
pova_23$elec_rec[pova_23$elec_rec == 19] <- "Bij1"


pova_23$elec_today[pova_23$elec_today == -8 | pova_23$elec_today == -9] <- NA
pova_23$elec_today[pova_23$elec_today == 1] <- "Not"
pova_23$elec_today[pova_23$elec_today == 2] <- "VVD"
pova_23$elec_today[pova_23$elec_today == 3] <- "PVV"
pova_23$elec_today[pova_23$elec_today == 4] <- "CDA"
pova_23$elec_today[pova_23$elec_today == 5] <- "D66"
pova_23$elec_today[pova_23$elec_today == 6] <- "GL"
pova_23$elec_today[pova_23$elec_today == 7] <- "SP"
pova_23$elec_today[pova_23$elec_today == 8] <- "PvdA"
pova_23$elec_today[pova_23$elec_today == 9] <- "CU"
pova_23$elec_today[pova_23$elec_today == 10] <- "PvdD"
pova_23$elec_today[pova_23$elec_today == 11] <- "50+"
pova_23$elec_today[pova_23$elec_today == 12] <- "SGP"
pova_23$elec_today[pova_23$elec_today == 13] <- "DENK"
pova_23$elec_today[pova_23$elec_today == 14] <- "FvD"
pova_23$elec_today[pova_23$elec_today == 15] <- "Blanco"
pova_23$elec_today[pova_23$elec_today == 16] <- "Other"
pova_23$elec_today[pova_23$elec_today == 17] <- "Volt"
pova_23$elec_today[pova_23$elec_today == 18] <- "JA21"
pova_23$elec_today[pova_23$elec_today == 19] <- "BBB"
pova_23$elec_today[pova_23$elec_today == 20] <- "Bij1"

pova_23$party_mem_p[pova_23$party_mem_p == 1] <- "VVD"
pova_23$party_mem_p[pova_23$party_mem_p == 2] <- "PVV"
pova_23$party_mem_p[pova_23$party_mem_p == 3] <- "CDA"
pova_23$party_mem_p[pova_23$party_mem_p == 4] <- "D66"
pova_23$party_mem_p[pova_23$party_mem_p == 5] <- "GL"
pova_23$party_mem_p[pova_23$party_mem_p == 6] <- "SP"
pova_23$party_mem_p[pova_23$party_mem_p == 7] <- "PvdA"
pova_23$party_mem_p[pova_23$party_mem_p == 8] <- "CU"
pova_23$party_mem_p[pova_23$party_mem_p == 9] <- "PvdD"
pova_23$party_mem_p[pova_23$party_mem_p == 10] <- "50+"
pova_23$party_mem_p[pova_23$party_mem_p == 11] <- "SGP"
pova_23$party_mem_p[pova_23$party_mem_p == 12] <- "DENK"
pova_23$party_mem_p[pova_23$party_mem_p == 13] <- "FvD"
pova_23$party_mem_p[pova_23$party_mem_p == 14] <- "Other"
pova_23$party_mem_p[pova_23$party_mem_p == 16] <- "JA21"
pova_23$party_mem_p[pova_23$party_mem_p == 17] <- "BBB"
pova_23$party_mem_p[pova_23$party_mem_p == 18] <- "Bij1"

pova_23$party_adh_p[pova_23$party_adh_p == 1] <- "VVD"
pova_23$party_adh_p[pova_23$party_adh_p == 2] <- "PVV"
pova_23$party_adh_p[pova_23$party_adh_p == 3] <- "CDA"
pova_23$party_adh_p[pova_23$party_adh_p == 4] <- "D66"
pova_23$party_adh_p[pova_23$party_adh_p == 5] <- "GL"
pova_23$party_adh_p[pova_23$party_adh_p == 6] <- "SP"
pova_23$party_adh_p[pova_23$party_adh_p == 7] <- "PvdA"
pova_23$party_adh_p[pova_23$party_adh_p == 8] <- "CU"
pova_23$party_adh_p[pova_23$party_adh_p == 9] <- "PvdD"
pova_23$party_adh_p[pova_23$party_adh_p == 10] <- "50+"
pova_23$party_adh_p[pova_23$party_adh_p == 11] <- "SGP"
pova_23$party_adh_p[pova_23$party_adh_p == 12] <- "DENK"
pova_23$party_adh_p[pova_23$party_adh_p == 13] <- "FvD"
pova_23$party_adh_p[pova_23$party_adh_p == 14] <- "Other"
pova_23$party_adh_p[pova_23$party_adh_p == 15] <- "Volt"
pova_23$party_adh_p[pova_23$party_adh_p == 16] <- "JA21"
pova_23$party_adh_p[pova_23$party_adh_p == 17] <- "BBB"
pova_23$party_adh_p[pova_23$party_adh_p == 18] <- "Bij1"


pova_23$party_att_p[pova_23$party_att_p == 1] <- "VVD"
pova_23$party_att_p[pova_23$party_att_p == 2] <- "PVV"
pova_23$party_att_p[pova_23$party_att_p == 3] <- "CDA"
pova_23$party_att_p[pova_23$party_att_p == 4] <- "D66"
pova_23$party_att_p[pova_23$party_att_p == 5] <- "GL"
pova_23$party_att_p[pova_23$party_att_p == 6] <- "SP"
pova_23$party_att_p[pova_23$party_att_p == 7] <- "PvdA"
pova_23$party_att_p[pova_23$party_att_p == 8] <- "CU"
pova_23$party_att_p[pova_23$party_att_p == 9] <- "PvdD"
pova_23$party_att_p[pova_23$party_att_p == 10] <- "50+"
pova_23$party_att_p[pova_23$party_att_p == 11] <- "SGP"
pova_23$party_att_p[pova_23$party_att_p == 12] <- "DENK"
pova_23$party_att_p[pova_23$party_att_p == 13] <- "FvD"
pova_23$party_att_p[pova_23$party_att_p == 14] <- "Other"
pova_23$party_att_p[pova_23$party_att_p == 15] <- "Volt"
pova_23$party_att_p[pova_23$party_att_p == 16] <- "JA21"
pova_23$party_att_p[pova_23$party_att_p == 17] <- "BBB"
pova_23$party_att_p[pova_23$party_att_p == 18] <- "Bij1"





















#merging all together
x <- full_join(pova_08, pova_09)
x <- full_join(x, pova_10)
x <- full_join(x, pova_11)
x <- full_join(x, pova_12)
x <- full_join(x, pova_13)
x <- full_join(x, pova_14)
x <- full_join(x, pova_16)
x <- full_join(x, pova_17)
x <- full_join(x, pova_18)
x <- full_join(x, pova_19)
x <- full_join(x, pova_20)
x <- full_join(x, pova_21)
x <- full_join(x, pova_22)
pova_panel <- full_join(x, pova_23)
rm(x, pova_08, pova_09, pova_10, pova_11, pova_12, pova_13,
   pova_14, pova_16, pova_17, pova_18, pova_19, pova_20, pova_21, pova_22, pova_23)



##Change variables
#add all NAs
pova_panel$term_cda[pova_panel$term_cda == 999 | pova_panel$term_cda == -9] <- NA
pova_panel$term_pvda[pova_panel$term_pvda == 999 | pova_panel$term_pvda == -9] <- NA
pova_panel$term_vvd[pova_panel$term_vvd == 999 | pova_panel$term_vvd == -9] <- NA
pova_panel$term_sp[pova_panel$term_sp == 999 | pova_panel$term_sp == -9] <- NA
pova_panel$term_gl[pova_panel$term_gl == 999 | pova_panel$term_gl == -9] <- NA
pova_panel$term_d66[pova_panel$term_d66 == 999 | pova_panel$term_d66 == -9] <- NA
pova_panel$term_cu[pova_panel$term_cu == 999 | pova_panel$term_cu == -9] <- NA
pova_panel$term_sgp[pova_panel$term_sgp == 999 | pova_panel$term_sgp == -9] <- NA
pova_panel$term_verdonk[pova_panel$term_verdonk == 999 | pova_panel$term_verdonk == -9] <- NA
pova_panel$term_pvv[pova_panel$term_pvv == 999 | pova_panel$term_pvv == -9] <- NA
pova_panel$term_pvdd[pova_panel$term_pvdd == 999 | pova_panel$term_pvdd == -9] <- NA
pova_panel$term_50[pova_panel$term_50 == 999 | pova_panel$term_50 == -9] <- NA
pova_panel$term_vnl[pova_panel$term_vnl == 999 | pova_panel$term_vnl == -9] <- NA
pova_panel$term_denk[pova_panel$term_denk == 999 | pova_panel$term_denk == -9] <- NA
pova_panel$term_fvd[pova_panel$term_fvd == 999 | pova_panel$term_fvd == -9] <- NA

pova_panel$left_right[pova_panel$left_right == 999 | pova_panel$left_right == -9] <- NA
pova_panel$redis[pova_panel$redis == 99 | pova_panel$redis == -9] <- NA
pova_panel$immi[pova_panel$immi == 99 | pova_panel$immi == -9] <- NA
pova_panel$eu_uni[pova_panel$eu_uni == 99 | pova_panel$eu_uni == -9] <- NA

pova_panel$sat_gov[pova_panel$sat_gov == 999 | pova_panel$sat_gov == -9] <- NA
pova_panel$sat_par[pova_panel$sat_par == 999 | pova_panel$sat_par == -9] <- NA
pova_panel$sat_pol[pova_panel$sat_pol == 999 | pova_panel$sat_pol == -9] <- NA
pova_panel$sat_part[pova_panel$sat_part ==999 | pova_panel$sat_part == -9] <- NA



#change order of immigration variables 
pova_panel$diver_cultures <- 5 - pova_panel$div_cultures
pova_panel$asylum_eas <- 5 - pova_panel$asylum
pova_panel$forei_socials <- 5 - pova_panel$for_socials
pova_panel <- subset(pova_panel, select = -c(div_cultures, asylum, for_socials))


#merge satisfaction variables together
pova_panel$satisf_all <- pova_panel$sat_gov + pova_panel$sat_par + pova_panel$sat_pol + pova_panel$sat_part













################### The social and network data  ##################


####  Loading the data  ####3

socio_08 <- read_dta("data/LISS data/soc int leasure/cs08a_2p_EN.dta")
socio_08 <- remove_all_labels(socio_08)
socio_09 <- read_dta("data/LISS data/soc int leasure/cs09b_1p_EN.dta")
socio_09 <- remove_all_labels(socio_09)
socio_10 <- read_dta("data/LISS data/soc int leasure/cs10c_1p_EN.dta")
socio_10 <- remove_all_labels(socio_10)
socio_11 <- read_dta("data/LISS data/soc int leasure/cs11d_EN_3.0p.dta")
socio_11 <- remove_all_labels(socio_11)
socio_12 <- read_dta("data/LISS data/soc int leasure/cs12e_1.0p_EN.dta")
socio_12 <- remove_all_labels(socio_12)
socio_13 <- read_dta("data/LISS data/soc int leasure/cs13f_2.0p_EN.dta")
socio_13 <- remove_all_labels(socio_13)
socio_14 <- read_dta("data/LISS data/soc int leasure/cs14g_EN_2.0.dta")
socio_14 <- remove_all_labels(socio_14)
socio_15 <- read_dta("data/LISS data/soc int leasure/cs15h_EN_1.0p.dta")
socio_15 <- remove_all_labels(socio_15)
socio_16 <- read_dta("data/LISS data/soc int leasure/cs16i_EN_1.0p.dta")
socio_16 <- remove_all_labels(socio_16)
socio_17 <- read_dta("data/LISS data/soc int leasure/cs17j_EN_1.0p.dta")
socio_17 <- remove_all_labels(socio_17)
socio_18 <- read_dta("data/LISS data/soc int leasure/cs18k_EN_1.0p.dta")
socio_18 <- remove_all_labels(socio_18)
socio_19 <- read_dta("data/LISS data/soc int leasure/cs19l_EN_1.0p.dta")
socio_19 <- remove_all_labels(socio_19)
socio_20 <- read_dta("data/LISS data/soc int leasure/cs20m_EN_1.0p.dta")
socio_20 <- remove_all_labels(socio_20)
socio_21 <- read_dta("data/LISS data/soc int leasure/cs21n_EN_1.0p.dta")
socio_21 <- remove_all_labels(socio_21)
socio_22 <- read_dta("data/LISS data/soc int leasure/cs22o_EN_1.0p.dta")
socio_22 <- remove_all_labels(socio_22)

socio_08$year <- 1
socio_09$year <- 2
socio_10$year <- 3
socio_11$year <- 4
socio_12$year <- 5
socio_13$year <- 6
socio_14$year <- 7
socio_15$year <- 8
socio_16$year <- 9
socio_17$year <- 10
socio_18$year <- 11
socio_19$year <- 12
socio_20$year <- 13
socio_21$year <- 14
socio_22$year <- 15





### create function to clean the data

clean_panel_wave <- function(data){
  
  names(data)[grep("_m", names(data))] <- "wave_socio" #fieldwork period
  
  names(data)[grep("294", names(data))] <- "pers1" #the five people
  names(data)[grep("295", names(data))] <- "pers2"
  names(data)[grep("296", names(data))] <- "pers3"
  names(data)[grep("297", names(data))] <- "pers4"
  names(data)[grep("298", names(data))] <- "pers5"
  
  names(data)[grep("299", names(data))] <- "equallydear"
  names(data)[grep("300", names(data))] <- "pers1_dear"
  names(data)[grep("301", names(data))] <- "pers2_dear"
  names(data)[grep("302", names(data))] <- "pers3_dear"
  names(data)[grep("303", names(data))] <- "pers4_dear"
  names(data)[grep("304", names(data))] <- "pers5_dear"
  
  names(data)[grep("321", names(data))] <- "pers1_know"
  names(data)[grep("332", names(data))] <- "pers2_know"
  names(data)[grep("343", names(data))] <- "pers3_know"
  names(data)[grep("354", names(data))] <- "pers4_know"
  names(data)[grep("365", names(data))] <- "pers5_know"
  names(data)[grep("323", names(data))] <- "pers1_talk"
  names(data)[grep("334", names(data))] <- "pers2_talk"
  names(data)[grep("345", names(data))] <- "pers3_talk"
  names(data)[grep("356", names(data))] <- "pers4_talk"
  names(data)[grep("367", names(data))] <- "pers5_talk"
  names(data)[grep("324", names(data))] <- "pers1_pol"
  names(data)[grep("335", names(data))] <- "pers2_pol"
  names(data)[grep("346", names(data))] <- "pers3_pol"
  names(data)[grep("357", names(data))] <- "pers4_pol"
  names(data)[grep("368", names(data))] <- "pers5_pol"
  names(data)[grep("326", names(data))] <- "pers1_edu"
  names(data)[grep("337", names(data))] <- "pers2_edu"
  names(data)[grep("348", names(data))] <- "pers3_edu"
  names(data)[grep("359", names(data))] <- "pers4_edu"
  names(data)[grep("370", names(data))] <- "pers5_edu"
  
  
  data <- data %>% select(nomem_encr, year, wave_socio,
                          pers1, pers2, pers3, pers4, pers5,
                          equallydear, pers1_dear, pers2_dear, pers3_dear, pers4_dear, pers5_dear,
                          pers1_know, pers2_know, pers3_know, pers4_know, pers5_know,
                          pers1_talk, pers2_talk, pers3_talk, pers4_talk, pers5_talk,
                          pers1_pol, pers2_pol, pers3_pol, pers4_pol, pers5_pol,
                          pers1_edu, pers2_edu, pers3_edu, pers4_edu, pers5_edu
  )
  
  
  return(data)
  
  
  
}




clean_panel_wave_2021 <- function(data){
  
  names(data)[grep("_m", names(data))] <- "wave_socio" #fieldwork period
  
  names(data)[grep("294", names(data))] <- "pers1" #the five people
  names(data)[grep("295", names(data))] <- "pers2"
  names(data)[grep("296", names(data))] <- "pers3"
  names(data)[grep("297", names(data))] <- "pers4"
  names(data)[grep("298", names(data))] <- "pers5"
  
  names(data)[grep("300", names(data))] <- "pers1_dear"
  names(data)[grep("301", names(data))] <- "pers2_dear"
  names(data)[grep("302", names(data))] <- "pers3_dear"
  names(data)[grep("303", names(data))] <- "pers4_dear"
  names(data)[grep("304", names(data))] <- "pers5_dear"
  
  names(data)[grep("321", names(data))] <- "pers1_know"
  names(data)[grep("332", names(data))] <- "pers2_know"
  names(data)[grep("343", names(data))] <- "pers3_know"
  names(data)[grep("354", names(data))] <- "pers4_know"
  names(data)[grep("365", names(data))] <- "pers5_know"
  names(data)[grep("323", names(data))] <- "pers1_talk"
  names(data)[grep("334", names(data))] <- "pers2_talk"
  names(data)[grep("345", names(data))] <- "pers3_talk"
  names(data)[grep("356", names(data))] <- "pers4_talk"
  names(data)[grep("367", names(data))] <- "pers5_talk"
  names(data)[grep("324", names(data))] <- "pers1_pol"
  names(data)[grep("335", names(data))] <- "pers2_pol"
  names(data)[grep("346", names(data))] <- "pers3_pol"
  names(data)[grep("357", names(data))] <- "pers4_pol"
  names(data)[grep("368", names(data))] <- "pers5_pol"
  names(data)[grep("326", names(data))] <- "pers1_edu"
  names(data)[grep("337", names(data))] <- "pers2_edu"
  names(data)[grep("348", names(data))] <- "pers3_edu"
  names(data)[grep("359", names(data))] <- "pers4_edu"
  names(data)[grep("370", names(data))] <- "pers5_edu"
  
  
  data <- data %>% select(nomem_encr, year, wave_socio,
                          pers1, pers2, pers3, pers4, pers5,
                          pers1_dear, pers2_dear, pers3_dear, pers4_dear, pers5_dear,
                          pers1_know, pers2_know, pers3_know, pers4_know, pers5_know,
                          pers1_talk, pers2_talk, pers3_talk, pers4_talk, pers5_talk,
                          pers1_pol, pers2_pol, pers3_pol, pers4_pol, pers5_pol,
                          pers1_edu, pers2_edu, pers3_edu, pers4_edu, pers5_edu
  )
  
  
  return(data)
  
  
  
}



socio_08 <- clean_panel_wave(socio_08)
socio_09 <- clean_panel_wave(socio_09)
socio_10 <- clean_panel_wave(socio_10)
socio_11 <- clean_panel_wave(socio_11)
socio_12 <- clean_panel_wave(socio_12)
socio_13 <- clean_panel_wave(socio_13)
socio_14 <- clean_panel_wave(socio_14)
socio_15 <- clean_panel_wave(socio_15)
socio_16 <- clean_panel_wave(socio_16)
socio_17 <- clean_panel_wave(socio_17)
socio_18 <- clean_panel_wave(socio_18)
socio_19 <- clean_panel_wave(socio_19)
socio_20 <- clean_panel_wave_2021(socio_20)
socio_21 <- clean_panel_wave_2021(socio_21)
socio_22 <- clean_panel_wave_2021(socio_22)


#merging all together
x <- full_join(socio_08, socio_09)
x <- full_join(x, socio_10)
x <- full_join(x, socio_11)
x <- full_join(x, socio_12)
x <- full_join(x, socio_13)
x <- full_join(x, socio_14)
x <- full_join(x, socio_15)
x <- full_join(x, socio_16)
x <- full_join(x, socio_17)
x <- full_join(x, socio_18)
x <- full_join(x, socio_19)
x <- full_join(x, socio_20)
x <- full_join(x, socio_21)
socio_panel <- full_join(x, socio_22)
rm(x, socio_08, socio_09, socio_10, socio_11, socio_12, socio_13,
   socio_14, socio_15, socio_16, socio_17, socio_18, socio_19, socio_20, socio_21, socio_22)










################### The work and schooling data  ##################
# In 2020 and 2021 they removed the question on fields. 


####  Loading the data  ####3

wosch_08 <- read_dta("data/LISS data/work schooling/cw08a_EN_1.1p.dta")
wosch_08 <- remove_all_labels(wosch_08)
wosch_09 <- read_dta("data/LISS data/work schooling/cw09b_EN_3.0p.dta")
wosch_09 <- remove_all_labels(wosch_09)
wosch_10 <- read_dta("data/LISS data/work schooling/cw10c_EN_1.0p.dta")
wosch_10 <- remove_all_labels(wosch_10)
wosch_11 <- read_dta("data/LISS data/work schooling/cw11d_EN_1.0p.dta")
wosch_11 <- remove_all_labels(wosch_11)
wosch_12 <- read_dta("data/LISS data/work schooling/cw12e_EN_1.0p.dta")
wosch_12 <- remove_all_labels(wosch_12)
wosch_13 <- read_dta("data/LISS data/work schooling/cw13f_EN_1.0p.dta")
wosch_13 <- remove_all_labels(wosch_13)
wosch_14 <- read_dta("data/LISS data/work schooling/cw14g_EN_1.0p.dta")
wosch_14 <- remove_all_labels(wosch_14)
wosch_15 <- read_dta("data/LISS data/work schooling/cw15h_EN_2.0p.dta")
wosch_15 <- remove_all_labels(wosch_15)
wosch_16 <- read_dta("data/LISS data/work schooling/cw16i_EN_2.0p.dta")
wosch_16 <- remove_all_labels(wosch_16)
wosch_17 <- read_dta("data/LISS data/work schooling/cw17j_EN_2.0p.dta")
wosch_17 <- remove_all_labels(wosch_17)
wosch_18 <- read_dta("data/LISS data/work schooling/cw18k_EN_2.0p.dta")
wosch_18 <- remove_all_labels(wosch_18)
wosch_19 <- read_dta("data/LISS data/work schooling/cw19l_EN_3.0p.dta")
wosch_19 <- remove_all_labels(wosch_19)
wosch_20 <- read_dta("data/LISS data/work schooling/cw20m_EN_1.0p.dta")
wosch_20 <- remove_all_labels(wosch_20)
wosch_21 <- read_dta("data/LISS data/work schooling/cw21n_EN_1.0p.dta")
wosch_21 <- remove_all_labels(wosch_21)
wosch_22 <- read_dta("data/LISS data/work schooling/cw22o_EN_1.0p.dta")
wosch_22 <- remove_all_labels(wosch_22)

wosch_08$year <- 1
wosch_09$year <- 2
wosch_10$year <- 3
wosch_11$year <- 4
wosch_12$year <- 5
wosch_13$year <- 6
wosch_14$year <- 7
wosch_15$year <- 8
wosch_16$year <- 9
wosch_17$year <- 10
wosch_18$year <- 11
wosch_19$year <- 12
wosch_20$year <- 13
wosch_21$year <- 14
wosch_22$year <- 15




### create function to clean the data
# In 2019 question 005 was not included due to an error in the question format


clean_panel_wave <- function(data){
  
  names(data)[grep("_m", names(data))] <- "wave_wosch" #fieldwork period
  
  names(data)[grep("011", names(data))] <- "field_gen" # The questions for different fields
  names(data)[grep("012", names(data))] <- "field_teach"
  names(data)[grep("013", names(data))] <- "field_art"
  names(data)[grep("014", names(data))] <- "field_hum"
  names(data)[grep("015", names(data))] <- "field_soc"
  names(data)[grep("016", names(data))] <- "field_eco"
  names(data)[grep("017", names(data))] <- "field_law"
  names(data)[grep("018", names(data))] <- "field_math"
  names(data)[grep("019", names(data))] <- "field_tech"
  names(data)[grep("020", names(data))] <- "field_agri"
  names(data)[grep("021", names(data))] <- "field_medi"
  names(data)[grep("022", names(data))] <- "field_pers"
  names(data)[grep("023", names(data))] <- "field_cat"
  names(data)[grep("024", names(data))] <- "field_trans"
  names(data)[grep("025", names(data))] <- "field_telec"
  names(data)[grep("026", names(data))] <- "field_pubo"
  names(data)[grep("027", names(data))] <- "field_other"
  names(data)[grep("028", names(data))] <- "field_dk"
  
  names(data)[grep("005", names(data))] <- "education_fromworkschool"
  names(data)[grep("006", names(data))] <- "education_fromworkschool_short"
  names(data)[grep("008", names(data))] <- "education_fromworkschool_atten"
  names(data)[grep("009", names(data))] <- "education_fromworkschool_atten_short"
  
  names(data)[grep("402", names(data))] <- "sector_402"
  names(data)[grep("404", names(data))] <- "profession_404"
  names(data)[grep("409", names(data))] <- "supervisor_409"
  
  
  
  
  data <- data %>% select(nomem_encr, year, wave_wosch,
                          field_gen, field_teach, field_art, field_hum,
                          field_soc, field_eco, field_law, field_math,
                          field_tech, field_agri, field_medi, field_pers,
                          field_cat, field_trans, field_telec, field_pubo,
                          field_other, field_dk,
                          education_fromworkschool, education_fromworkschool_short, 
                          education_fromworkschool_atten, education_fromworkschool_atten_short,
                          sector_402, profession_404, supervisor_409
                          
  )
  return(data)
}



clean_panel_wave_19 <- function(data){
  
  names(data)[grep("_m", names(data))] <- "wave_wosch" #fieldwork period
  
  names(data)[grep("011", names(data))] <- "field_gen" # The questions for different fields
  names(data)[grep("012", names(data))] <- "field_teach"
  names(data)[grep("013", names(data))] <- "field_art"
  names(data)[grep("014", names(data))] <- "field_hum"
  names(data)[grep("015", names(data))] <- "field_soc"
  names(data)[grep("016", names(data))] <- "field_eco"
  names(data)[grep("017", names(data))] <- "field_law"
  names(data)[grep("018", names(data))] <- "field_math"
  names(data)[grep("019", names(data))] <- "field_tech"
  names(data)[grep("020", names(data))] <- "field_agri"
  names(data)[grep("021", names(data))] <- "field_medi"
  names(data)[grep("022", names(data))] <- "field_pers"
  names(data)[grep("023", names(data))] <- "field_cat"
  names(data)[grep("024", names(data))] <- "field_trans"
  names(data)[grep("025", names(data))] <- "field_telec"
  names(data)[grep("026", names(data))] <- "field_pubo"
  names(data)[grep("027", names(data))] <- "field_other"
  names(data)[grep("028", names(data))] <- "field_dk"
  
  names(data)[grep("006", names(data))] <- "education_fromworkschool_short"
  names(data)[grep("008", names(data))] <- "education_fromworkschool_atten"
  names(data)[grep("009", names(data))] <- "education_fromworkschool_atten_short"
  
  names(data)[grep("402", names(data))] <- "sector_402"
  names(data)[grep("404", names(data))] <- "profession_404"
  names(data)[grep("409", names(data))] <- "supervisor_409"
  
  
  data <- data %>% select(nomem_encr, year, wave_wosch,
                          field_gen, field_teach, field_art, field_hum,
                          field_soc, field_eco, field_law, field_math,
                          field_tech, field_agri, field_medi, field_pers,
                          field_cat, field_trans, field_telec, field_pubo,
                          field_other, field_dk,
                          education_fromworkschool_short, 
                          education_fromworkschool_atten, education_fromworkschool_atten_short,
                          sector_402, profession_404, supervisor_409
                          
  )
  
  return(data)
  
}





clean_panel_wave_20 <- function(data){
  
  names(data)[grep("_m", names(data))] <- "wave_wosch" #fieldwork period
  
  names(data)[grep("011", names(data))] <- "field_gen" # The questions for different fields
  names(data)[grep("012", names(data))] <- "field_teach"
  names(data)[grep("013", names(data))] <- "field_art"
  names(data)[grep("014", names(data))] <- "field_hum"
  names(data)[grep("015", names(data))] <- "field_soc"
  names(data)[grep("016", names(data))] <- "field_eco"
  names(data)[grep("017", names(data))] <- "field_law"
  names(data)[grep("018", names(data))] <- "field_math"
  names(data)[grep("019", names(data))] <- "field_tech"
  names(data)[grep("020", names(data))] <- "field_agri"
  names(data)[grep("021", names(data))] <- "field_medi"
  names(data)[grep("022", names(data))] <- "field_pers"
  names(data)[grep("023", names(data))] <- "field_cat"
  names(data)[grep("024", names(data))] <- "field_trans"
  names(data)[grep("025", names(data))] <- "field_telec"
  names(data)[grep("026", names(data))] <- "field_pubo"
  names(data)[grep("027", names(data))] <- "field_other"
  
  names(data)[grep("006", names(data))] <- "education_fromworkschool_short"
  names(data)[grep("008", names(data))] <- "education_fromworkschool_atten"
  names(data)[grep("009", names(data))] <- "education_fromworkschool_atten_short"
  
  names(data)[grep("402", names(data))] <- "sector_402"
  names(data)[grep("404", names(data))] <- "profession_404"
  names(data)[grep("409", names(data))] <- "supervisor_409"
  
  
  data <- data %>% select(nomem_encr, year, wave_wosch,
                          field_gen, field_teach, field_art, field_hum,
                          field_soc, field_eco, field_law, field_math,
                          field_tech, field_agri, field_medi, field_pers,
                          field_cat, field_trans, field_telec, field_pubo,
                          field_other,
                          education_fromworkschool_short, 
                          education_fromworkschool_atten, education_fromworkschool_atten_short,
                          sector_402, profession_404, supervisor_409
                          
  )
  
  return(data)
  
}




wosch_08 <- clean_panel_wave(wosch_08)
wosch_09 <- clean_panel_wave(wosch_09)
wosch_10 <- clean_panel_wave(wosch_10)
wosch_11 <- clean_panel_wave(wosch_11)
wosch_12 <- clean_panel_wave(wosch_12)
wosch_13 <- clean_panel_wave(wosch_13)
wosch_14 <- clean_panel_wave(wosch_14)
wosch_15 <- clean_panel_wave(wosch_15)
wosch_16 <- clean_panel_wave(wosch_16)
wosch_17 <- clean_panel_wave(wosch_17)
wosch_18 <- clean_panel_wave(wosch_18)
wosch_19 <- clean_panel_wave_19(wosch_19)
wosch_20 <- clean_panel_wave_20(wosch_20)
wosch_21 <- clean_panel_wave_20(wosch_21)
wosch_22 <- clean_panel_wave_20(wosch_22)





#merging all together
x <- full_join(wosch_08, wosch_09)
x <- full_join(x, wosch_10)
x <- full_join(x, wosch_11)
x <- full_join(x, wosch_12)
x <- full_join(x, wosch_13)
x <- full_join(x, wosch_14)
x <- full_join(x, wosch_15)
x <- full_join(x, wosch_16)
x <- full_join(x, wosch_17)
x <- full_join(x, wosch_18)
x <- full_join(x, wosch_19)
x <- full_join(x, wosch_20)
x <- full_join(x, wosch_21)
wosch_panel <- full_join(x, wosch_22)

rm(x, wosch_08, wosch_09, wosch_10, wosch_11, wosch_12, wosch_13,
   wosch_14, wosch_15, wosch_16, wosch_17, wosch_18, wosch_19, wosch_20, wosch_21, wosch_22)









################### Merging all panels together  ##################
#Variable to use in merging: nomem_encr and wave (you need to merge with both because this data is by month). 
full_panel <- left_join(pova_panel, income_panel, by=c("year", "nomem_encr")) #this is a left join, only people in politics and value are useful anyway

full_panel <- full_join(full_panel, socio_panel, by=c("year", "nomem_encr"))

full_panel <- full_join(full_panel, wosch_panel, by=c("year", "nomem_encr"))


rm(income_panel, pova_panel, socio_panel, wosch_panel)



# If there is an NA on any of the waves, take it from another wave. Baseline is wave from the politics as this is the main outcome variable wave
full_panel$wave[is.na(full_panel$wave)] <- full_panel$wave_inc[is.na(full_panel$wave)]
full_panel$wave[is.na(full_panel$wave)] <- full_panel$wave_wosch[is.na(full_panel$wave)]
full_panel$wave[is.na(full_panel$wave)] <- full_panel$wave_socio[is.na(full_panel$wave)]




################### Adding the demographic background variables  ##################
##Add all the background data together into one big dataframe. Some key variables -- mainly income -- are in here! 
#links used to build the function below (Python would be better here)
#https://jozef.io/r006-merge/#merging-multiple-data-frames
#https://www.musgraveanalytics.com/blog/2018/2/12/how-to-merge-multiple-data-frames-using-base-r
#https://blog.zhaw.ch/datascience/r-reduce-applys-lesser-known-brother/
#Alternative: https://www.rdocumentation.org/packages/plyr/versions/1.8.5/topics/join_all

#######load all data frames in one go
#setwd("data/LISS data/background")
#
###get the names of all variables using regular expression * in the working directory (set above)
#files = list.files(pattern="/*.dta")
#
###load all the files in the list into a big object with multiple data frames (a really big list)
#my.data <- lapply(files, 
#                  read_dta)
#
###full join them all using reduce, which is a kind of for loop. 
#background_panel <- Reduce(
#  function(x, y) full_join(x, y),
#  my.data
#)
#
#
###save so you don't have to rerun the whole code since it takes about half an hour
#save(background_panel, file="background_panel.Rda")
#rm(files, my.data)




##change the names and kick out the variables you don't want. 

#this is a dataframe with all background data that I created with the code above. This is needed because you donwload the background
# data per month (thus months X number of years is the number of dataframes)

load("data/LISS data/background/background_panel.Rda")

background_panel <- background_panel %>% select(nomem_encr, wave, geslacht, leeftijd, sted, belbezig,
                                                nettoink, nettoink_f, nettohh_f, oplcat, herkomstgroep, positie, partner) %>%
  rename(gender = geslacht,
         age = leeftijd,
         urban = sted,
         occupation = belbezig,
         education = oplcat,
         hhposition = positie)


##add it to the full_panel
full_panel <- left_join(full_panel, background_panel)
rm(background_panel)




#Add labels for variables that are perhaps not as easy to interpret based on just the name
var_label(full_panel$nomem_encr) <- "Unique respondent ID"
var_label(full_panel$year) <- "Year of survey"
var_label(full_panel$wave) <- "Year and month of fieldwork (background variables and political)"
var_label(full_panel$elec_rec) <- "Recall of vote at last election"
var_label(full_panel$elec_today) <- "Would vote if election is today"
var_label(full_panel$elec_today) <- "Would vote if election is today"
var_label(full_panel$party_mem) <- "Party member 1 = yes, 2 = no"
var_label(full_panel$party_mem_p) <- "Party respondent is member of"
var_label(full_panel$party_adh) <- "Party respondent is supporter of particular party. 2=n, 1=y"
var_label(full_panel$party_att) <- "Party respondent is attracted to particular party. 2=n, 1=y"
var_label(full_panel$wave_inc) <- "Year and month of fieldwork of the income wave"
var_label(full_panel$nettoink) <- "After tax (net) income"
var_label(full_panel$nettohh_f) <- "Household income"
var_label(full_panel$losing_job) <- "how afraid to lose job, 0 to 100"
var_label(full_panel$new_job) <- "how likely to find a new job if unemployed, 0 to 100"
var_label(full_panel$diver_cultures) <- "It is good society has different cultures (reverse)"
var_label(full_panel$asylum_eas) <- "it should be easier to get asylum (reverse)"
var_label(full_panel$forei_socials) <- "foreigners should get social security (reverse)"
var_label(full_panel$for_neigh) <- "it does not help if a neighborhood has foreigners"
var_label(full_panel$for_many) <- "there are too many foreigners"
var_label(full_panel$sat_gov) <- "how satisfied are you with the government"
var_label(full_panel$sat_par) <- "how satisfied are you with the parliament"
var_label(full_panel$sat_pol) <- "how satisfied are you with politicians"
var_label(full_panel$sat_part) <- "how satisfied are you with parties"
var_label(full_panel$satisf_all) <- "additive index of satisfaction variables"







#save
save(full_panel, file="data/LISS data/full_panel.Rda")


full_panel_forstata <- full_panel %>% rename(edu_fromworkschool_atten_short = education_fromworkschool_atten_short)
write_dta(full_panel_forstata, "data/LISS data/full_panel.dta")

#Done :)

######### SOEP Analysis. Jonne Kamphorst 2023. 


library(haven)
library(tidyverse)
library(extrafont)
library(texreg)
library(ggthemes)
library(estimatr)
library(margins)
library(lme4)
library(plm)
library(lmtest)
library(ggeffects)


#font_import()
loadfonts(quiet = T, device = "pdf")
windowsFonts(Georgia = windowsFont("Georgia")) #load fonts for windows machines


## Set ggplot theme with the Georgia font
theme_set(theme_light(base_family = "Georgia", base_size = 12))








################### -------------------- Prepare the data ---------------------------------------------------------------------- 
pl <- read_dta("data/SOEP_long_small.dta") # The version of the 'pl' soep data that has been cleaned in the data code
pl <- pl %>% distinct(pid, syear,.keep_all=T)


# Leans towards a party
#pl$plh0011_h   #https://paneldata.org/soep-core/data/pl/plh0011_h

#Which party
#https://paneldata.org/soep-core/data/pl/plh0012_h
pl$plh0012_h_cleaned <- pl$plh0012_h
pl$plh0012_h_cleaned[pl$plh0012_h_cleaned==-5] <- NA #keep trifft nicht zu in (they are -2, those who do not lean) and those who do not know (-1)
pl$plh0012_h_cleaned[pl$plh0012_h_cleaned==-4] <- NA
pl$plh0012_h_cleaned <- as.numeric(pl$plh0012_h_cleaned)

# Leans towards AfD
pl$lean_afd <- ifelse(pl$plh0012_h_cleaned == 27 | 
                        pl$plh0012_h_cleaned == 7 |
                        pl$plh0012_h_cleaned == 30 |
                        pl$plh0012_h_cleaned == 31, 1, 0)

table(pl$lean_afd, pl$syear)

pl$lean_green <- ifelse(pl$plh0012_h_cleaned == 18 | 
                          pl$plh0012_h_cleaned == 5 | 
                          pl$plh0012_h_cleaned == 9 | 
                          pl$plh0012_h_cleaned == 15 | 
                          pl$plh0012_h_cleaned == 16 | 
                          pl$plh0012_h_cleaned == 18 | 
                          pl$plh0012_h_cleaned == 23, 1, 0)

table(pl$lean_green, pl$syear)





## Create a variable for someone's field score throughout their whole live
pl <- pl %>% group_by(pid) %>%
  fill(fieldratio1, .direction="downup") %>% #filling in the NAs. 
  ungroup()


# Add the latest observation of this filled forward score ratio (i.e. someone's latest field score). It is our main right hand side variable
pl <- pl %>% group_by(pid) %>%
  mutate(max_wave = max(syear)) %>%
  ungroup()

fields <- pl %>% group_by(pid) %>% filter(syear==max_wave) %>%
  select(pid, fieldratio1) %>% 
  rename(max_fieldratio1 = fieldratio1) %>% distinct(pid, .keep_all = T)

pl <- left_join(pl, fields)






############ CREATE KEY EDUCATION VARIABLE 
## Add variable for whether someone completed university
# Whether someone stopped with a degree and the degree was university
pl$respondent_completed_uni_dummy <- ifelse((pl$plg0079_v4 > 0 | pl$plg0079_v1 >0 | pl$plg0079_v3 > 0) & pl$plg0072 == 1, 1, NA) #v1 is from 1984 till 2009 v3 and v4 are after 2009. pl$plg0072 is from 2000 only

# Now fill the 1 forward for years when someone didn't graduate university and for missigness
pl <- pl %>% group_by(pid) %>% 
  fill(respondent_completed_uni_dummy, .direction = c("down")) %>% ungroup()

# Fill in the NAs with 0: people who completed a degree and it is not university. I.e. highschool or vocational training
pl$respondent_completed_uni_dummy <- ifelse(pl$plg0072 == 1 & is.na(pl$respondent_completed_uni_dummy), 0, pl$respondent_completed_uni_dummy) #NOTE pl$plg0076 is from 2000 only

# Fill in the NAs for the people who graduated a degree and it wasn't university
pl <- pl %>% group_by(pid) %>% 
  fill(respondent_completed_uni_dummy, .direction = c("down")) %>% ungroup()



## Create a current education variable. Using questions:  #https://paneldata.org/soep-core/inst/soep-core-2019-pe2/12
# Categories we use that question for and the corresponding variables: 
#Not in current education
# In current education, larger categories https://paneldata.org/soep-core/data/pl/plg0267
# In current education, High school categories (learning 'Allgemeinbildende Schule') https://paneldata.org/soep-core/data/pl/plg0013_v3
# In current education, High school categories (learning 'Berufliche Ausbildung')  https://paneldata.org/soep-core/data/pl/plg0293_h
# In current education, After high school, graduate degree categories (`Hochschule`) https://paneldata.org/soep-core/data/pl/plg0014_v7

pl$current_education <- NA
pl$current_education[pl$plg0012 == 2 & pl$respondent_completed_uni_dummy == 0] <- "Not in edu and has practical edu or no edu" #high practical education
pl$current_education[pl$plg0012 == 2 & pl$respondent_completed_uni_dummy == 1] <- "Not in edu and has university"
pl$current_education[pl$plg0012 == 2 & pl$pgisced97 < 5] <- "Not in edu and has practical edu or no edu" #these two variables directly ask about someone's education level
pl$current_education[pl$plg0012 == 2 & pl$pgisced97 >=5] <- "Not in edu and has university"

# These are different variables that capture current education levels
pl$current_education[pl$plg0267 == 2] <- "In edu: University" #includes HBO (Hochschule), 2014, 2018
pl$current_education[pl$plg0014_v7 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni  2013, 2018
pl$current_education[pl$plg0014_v6 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni  2009, 2012
pl$current_education[pl$plg0014_v5 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni 1999, 2008
pl$current_education[pl$plg0014_v4 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni 1996, 1998
pl$current_education[pl$plg0014_v3 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni  1996, 1998
pl$current_education[pl$plg0014_v2 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni, 1990, 1991
pl$current_education[pl$plg0014_v1 > 0] <- "In edu: University" #includes HBO (Hochschule) IS uni, 1984 till 1995

#the same for particucal education
pl$current_education[pl$plg0267 == 3] <- "In edu: Practical edu" #once again the more general question, only from 2000
pl$current_education[pl$plg0293_h >0] <- "In edu: Practical edu" # this one is in all waves. Type of vocational education. If non-missing then thus in a type of vocational edu


## In highschool
# Cleaning the variables
# Version v2 of this variable is not usable.
pl$plg0013_v1_cleaned <- pl$plg0013_v1 #v1 is from before 2016
pl$plg0013_v1_cleaned[pl$plg0013_v1_cleaned < 1] <- NA
pl$plg0013_v3_cleaned <- pl$plg0013_v3 #v3 is from after 2016
pl$plg0013_v3_cleaned[pl$plg0013_v3_cleaned < 1] <- NA

#which type of highschool
pl$current_education[pl$plg0267 == 1] <- "In edu: Highschool Gym" #'algemein bildende schule'. This will be overwritten below because algemein includes other schools too
pl$current_education[pl$plg0013_v1_cleaned == 3] <- "In edu: Highschool Gym" 
pl$current_education[pl$plg0013_v3_cleaned == 3] <- "In edu: Highschool Gym" 
pl$current_education[pl$plg0013_v1_cleaned != 3] <- "In edu: Highschool No Gym" 
pl$current_education[pl$plg0013_v3_cleaned != 3] <- "In edu: Highschool No Gym" 


#few missing cases (less than 10% of the sample)
table(pl$current_education)
sum(is.na(pl$current_education))


#Clean version of in edu variable
pl$in_education <- NA
pl$in_education[pl$plg0072 == 1] <- 1
pl$in_education[pl$plg0072 == 2] <- 0

pl$in_highschool <-NA
pl$in_highschool[!is.na(pl$current_education)] <- 0 # Anyone who has data on the current education variable
pl$in_highschool[pl$current_education == "In edu: Highschool Gym" | 
                   pl$current_education == "In edu: Highschool No Gym"] <- 1 #overwriting if that variable is indicating that you're in highschool


# mean per person which we use to measure if we have people in different life phases
pl <- pl %>% group_by(pid) %>% mutate(mean_in_education = mean(in_education, na.rm=T),
                                      mean_in_highschool = mean(in_highschool, na.rm=T)) %>% ungroup()

# Add a dummy for if you both are and are not in (high)school in the data. These people are useful for our life-phase model
pl$are_andnot_inhs <- ifelse(pl$mean_in_highschool != 0 & pl$mean_in_highschool != 1, 1, 0)
pl$are_andnot_inedu <- ifelse(pl$mean_in_education != 0 & pl$mean_in_education != 1, 1, 0)




# Add the life phases (in highschool, In post-secondary education, not in education) based on the current_education variable
pl$life_phase <- NA
pl$life_phase[pl$current_education=="In edu: Highschool Gym" | 
                pl$current_education=="In edu: Highschool No Gym"] <- "In highschool"
pl$life_phase[pl$current_education=="In edu: Practical edu" | 
                pl$current_education=="In edu: University"] <- "In post-secondary education"
pl$life_phase[pl$current_education=="Not in edu and has practical edu or no edu" | 
                pl$current_education=="Not in edu and has university"] <- "Working"

pl$life_phase_highornon <- NA
pl$life_phase_highornon[pl$current_education=="In edu: Highschool Gym" | 
                pl$current_education=="In edu: Highschool No Gym"] <- "In highschool"
pl$life_phase_highornon[pl$current_education=="Not in edu and has practical edu or no edu" | 
                pl$current_education=="Not in edu and has university"] <- "Working"


pl$life_phase <- factor(pl$life_phase)


# Add if someone ever gets university
pl$current_edu_uni <- ifelse(pl$current_education == "In edu: University" | #these are only people for whom we have data on their current education
                                              pl$current_education == "Not in edu and has university", 1, 0) #these are only people for whom we have data when they *finished* a degree




# Set variable so that we can subset for people for whom we have at least one graduation
# First set if someone has a graduation
pl <- pl %>% group_by(pid) %>% mutate(graduation = ifelse(plg0072==1, 1, 0), #variable captures if you 'finished a degree last year'
                                      mean_graduation = mean(graduation, na.rm=T),
                                      has_one_graduation = ifelse(mean_graduation>0, 1, 0),
                                      mean_uni = mean(current_edu_uni, na.rm=T),
                                      ever_uni = ifelse(mean_uni>0,1,0)) %>% 
  ungroup()



## Add max and min age. SLOWER ONE
pl <- pl %>% group_by(pid) %>%
  mutate(max_age=max(age, na.rm=T),
         min_age=min(age, na.rm=T)) %>%
  ungroup()



# add a dummy for being under 18
pl$under19 <- ifelse(pl$age <19, 1, 0)





##### Add occupational field
## three-digit ISCO variable
pl$iscothree <- substr(pl$isco88, 1, 3)


pl <- pl %>% group_by(iscothree) %>% mutate(n_peroccup=n(),
                                            isco3_fieldscore=mean(fieldratio1,na.rm=T)) %>%
  ungroup()

pl$isco3_fieldscore[pl$n_peroccup<30] <- NA






#### Get someone's mean occupational field score when they are not in education anymore
# Calculated as the mean of all their occupational field scores
xx <- pl %>% filter(in_education == 0) %>% group_by(pid) %>% 
  mutate(mean_isco3_fieldscore = mean(isco3_fieldscore, na.rm=T)) %>% 
  select(pid, mean_isco3_fieldscore) %>% distinct(pid, .keep_all = T)

pl <- left_join(pl, xx)

# fill all the NAs so that the job score is there when someone is in highschool
pl <- pl %>% group_by(pid) %>% 
  fill(mean_isco3_fieldscore, .direction=c("downup")) %>% ungroup()

rm(xx)



## Calculate the number of years someone has been in a job
## Add a variable that measures how long someone is in the same job
# first add the lags of someone's work score
pl <- pl %>% ungroup() %>% group_by(pid) %>%
  dplyr::mutate(isco3_fieldscore_lag1 =  dplyr::lag(isco3_fieldscore, n=1, order_by=syear),
                isco3_fieldscore_lag2 = dplyr::lag(isco3_fieldscore, n=2, order_by=syear),
                isco3_fieldscore_lag3 = dplyr::lag(isco3_fieldscore, n=3, order_by=syear),
                isco3_fieldscore_lag4 = dplyr::lag(isco3_fieldscore, n=4, order_by=syear),
                isco3_fieldscore_lag5 = dplyr::lag(isco3_fieldscore, n=5, order_by=syear),
                isco3_fieldscore_lag6 = dplyr::lag(isco3_fieldscore, n=6, order_by=syear),
                isco3_fieldscore_lag7 = dplyr::lag(isco3_fieldscore, n=7, order_by=syear),
                isco3_fieldscore_lag8 = dplyr::lag(isco3_fieldscore, n=8, order_by=syear),
                isco3_fieldscore_lag9 = dplyr::lag(isco3_fieldscore, n=9, order_by=syear),
                isco3_fieldscore_lag10 = dplyr::lag(isco3_fieldscore, n=10), order_by=syear,
                isco3_fieldscore_lag11 = dplyr::lag(isco3_fieldscore, n=11), order_by=syear,
                isco3_fieldscore_lag12 = dplyr::lag(isco3_fieldscore, n=12), order_by=syear,
                isco3_fieldscore_lag13 = dplyr::lag(isco3_fieldscore, n=13), order_by=syear,
                isco3_fieldscore_lag14 = dplyr::lag(isco3_fieldscore, n=14), order_by=syear,
                isco3_fieldscore_lag15 = dplyr::lag(isco3_fieldscore, n=15), order_by=syear,
                isco3_fieldscore_lag16 = dplyr::lag(isco3_fieldscore, n=16), order_by=syear,
                isco3_fieldscore_lag17 = dplyr::lag(isco3_fieldscore, n=17), order_by=syear,
                isco3_fieldscore_lag18 = dplyr::lag(isco3_fieldscore, n=18), order_by=syear,
                isco3_fieldscore_lag19 = dplyr::lag(isco3_fieldscore, n=19), order_by=syear,
                isco3_fieldscore_lag20 = dplyr::lag(isco3_fieldscore, n=20), order_by=syear,
                isco3_fieldscore_lag21 = dplyr::lag(isco3_fieldscore, n=21), order_by=syear,
                isco3_fieldscore_lag22 = dplyr::lag(isco3_fieldscore, n=22), order_by=syear,
                isco3_fieldscore_lag23 = dplyr::lag(isco3_fieldscore, n=23), order_by=syear,
                isco3_fieldscore_lag24 = dplyr::lag(isco3_fieldscore, n=24), order_by=syear,
                isco3_fieldscore_lag25 = dplyr::lag(isco3_fieldscore, n=25), order_by=syear,
                isco3_fieldscore_lag26 = dplyr::lag(isco3_fieldscore, n=26), order_by=syear,
                isco3_fieldscore_lag27 = dplyr::lag(isco3_fieldscore, n=27), order_by=syear,
                isco3_fieldscore_lag28 = dplyr::lag(isco3_fieldscore, n=28), order_by=syear,
                isco3_fieldscore_lag29 = dplyr::lag(isco3_fieldscore, n=29), order_by=syear,
                isco3_fieldscore_lag30 = dplyr::lag(isco3_fieldscore, n=30), order_by=syear) %>% ungroup()



#add a variable for years in job
pl$years_in_job <- NA
pl$years_in_job[!is.na(pl$isco3_fieldscore)] <- 0
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag1] <- 1
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag2] <- 2
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag3] <- 3
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag4] <- 4
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag5] <- 5
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag6] <- 6
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag7] <- 7
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag8] <- 8
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag9] <- 9
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag10] <- 10
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag11] <- 11
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag12] <- 12
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag13] <- 13
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag14] <- 14
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag15] <- 15
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag16] <- 16
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag17] <- 17
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag18] <- 18
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag19] <- 19
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag20] <- 20
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag21] <- 21
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag22] <- 22
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag23] <- 23
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag24] <- 24
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag25] <- 25
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag26] <- 26
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag27] <- 27
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag28] <- 28
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag29] <- 29
pl$years_in_job[pl$isco3_fieldscore == pl$isco3_fieldscore_lag30] <- 30








##### Set treatment dummies using current education. This will later be used to set graduation by field
pl$current_education_unitreatment <- ifelse(pl$current_education == "In edu: University" | #these are only people for whom we have data on their current education
                                              pl$current_education == "Not in edu and has university", 1, NA) #these are only people for whom we have data when they *finished* a degree
# All NAs in this variable are thus respondents for whom we have no data or respondents who don't finish a degree or do a degree. 


#Fill forward in case there is missingness or someone drops out of uni
pl <- pl %>% group_by(pid) %>% 
  fill(current_education_unitreatment, .direction = c("down")) %>% ungroup()


#Fill in the NAs, doing it this way makes sure people don't fall out of treatment because it only fills in NAs and doesn't overwrite
pl$current_education_unitreatment <- ifelse(is.na(pl$current_education_unitreatment) & 
                                              pl$current_education != "In edu: University" & 
                                              pl$current_education != "Not in edu and has university", 0, pl$current_education_unitreatment)


#### And a version for graduating
pl$graduating_unitreatment <- ifelse(pl$current_education == "Not in edu and has university", 1, NA) #these are only people for whom we have data when they *finished* a degree
# All NAs in this variable are thus respondents for whom we have no data or respondents who don't finish a degree or do a degree. 


#Fill forward in case there is missingness or someone drops out of uni
pl <- pl %>% group_by(pid) %>% 
  fill(graduating_unitreatment, .direction = c("down")) %>% ungroup()


#Fill in the NAs, doing it this way makes sure people don't fall out of treatment because it only fills in NAs and doesn't overwrite
pl$graduating_unitreatment <- ifelse(is.na(pl$graduating_unitreatment) & pl$current_education != "Not in edu and has university", 0, pl$current_education_unitreatment)





## Add variables for within-effect of education analysis with CECT split up in above and below median
# Graduating with a particular CECT score (disregarding whether it's uni or not)
pl$education_cect_treatment <- NA
pl$education_cect_treatment[pl$in_education==1] <- "In education"
pl$education_cect_treatment[pl$in_education==0 & pl$max_fieldratio1 <= median(pl$max_fieldratio1,na.rm=T) ] <-  "Graduated, <= median CECT"
pl$education_cect_treatment[pl$in_education==0 & pl$max_fieldratio1 > median(pl$max_fieldratio1,na.rm=T) ] <-  "Graduated, > median CECT"

pl$education_cect_treatment <- factor(pl$education_cect_treatment)
pl$education_cect_treatment <- relevel(pl$education_cect_treatment, ref="In education")


# Same variable with with graduation for people both in and not in uni (thus separating out uni and no uni)
pl$education_cect_treatment_wuni <- NA
pl$education_cect_treatment_wuni[pl$in_education==1] <- "In education"
# non-uni grads
pl$education_cect_treatment_wuni[pl$ever_uni == 0 & pl$in_education==0 & pl$max_fieldratio1 <= median(pl$max_fieldratio1,na.rm=T) ] <-  "Graduated, <= median CECT (University)"
pl$education_cect_treatment_wuni[pl$ever_uni == 0 & pl$in_education==0 & pl$max_fieldratio1 > median(pl$max_fieldratio1,na.rm=T) & pl$max_fieldratio1 <= 0.50 ] <-  "Graduated, > median CECT (University)"

#uni grads
pl$education_cect_treatment_wuni[pl$ever_uni == 1 & pl$in_education==0 & pl$max_fieldratio1 <= median(pl$max_fieldratio1,na.rm=T) ] <-  "Graduated, <= median CECT (Not University)"
pl$education_cect_treatment_wuni[pl$ever_uni == 1 & pl$in_education==0 & pl$max_fieldratio1 > median(pl$max_fieldratio1,na.rm=T) & pl$max_fieldratio1 <= 0.50 ] <-  "Graduated, > median CECT (Not University)"

pl$education_cect_treatment_wuni <- factor(pl$education_cect_treatment_wuni)
pl$education_cect_treatment_wuni <- relevel(pl$education_cect_treatment_wuni, ref="In education")

# Create a variable where you look at graduating university by cect score
pl$education_cect_treatment_onlyuni <- as.character(pl$education_cect_treatment_wuni)
pl$education_cect_treatment_onlyuni[grepl("Not University", pl$education_cect_treatment_onlyuni)] <- "control"
pl$education_cect_treatment_onlyuni[pl$education_cect_treatment_onlyuni=="Not University"] <- "control"
pl$education_cect_treatment_onlyuni[pl$education_cect_treatment_onlyuni=="In education"] <- "control"
pl$education_cect_treatment_onlyuni <- factor(pl$education_cect_treatment_onlyuni)
pl$education_cect_treatment_onlyuni <- relevel(pl$education_cect_treatment_onlyuni, ref="control")









# number of years since X
pl$number_of_years_since_21 <- pl$age - 21
pl$number_of_years_since_25 <- pl$age - 25



# Add birth cohorts
pl$year_born <- pl$syear - pl$age

pl$generation <- NA
pl$generation[pl$year_born < 1946] <- "Silent"
pl$generation[pl$year_born > 1945 & pl$year_born < 1956] <- "Boomers I"
pl$generation[pl$year_born > 1955 & pl$year_born < 1966] <- "Boomers II"
pl$generation[pl$year_born > 1965 & pl$year_born < 1976] <- "Gen X"
pl$generation[pl$year_born > 1975 & pl$year_born < 1986] <- "Gen X II"
pl$generation[pl$year_born > 1985 & pl$year_born < 1996] <- "Millennials"


rm(fields)



## Save the data for use in STATA if needed
#write_dta(pl, "SOEP data/SOEP_long_small_wvars.dta")










################### -------------------- Educational analysis: The effect of field when someone is still in education #############
pl$pid <- as.character(pl$pid)


#### Run the models
## With any education and highschool
green_mod <- lm_robust(lean_green~ max_fieldratio1 * life_phase + ever_uni + female, 
                       data=subset(pl,  
                                   are_andnot_inhs == 1 &
                                   are_andnot_inedu == 1),   #people who we have in education and not in education
                       clusters=pid)
summary(green_mod)





#plot
# Get the marginal effects in each highschool group of all the variables
green_mod_mod_margins <- margins_summary(green_mod, at=list(life_phase=c("Working", "In post-secondary education" ,"In highschool")))
green_mod_mod_margins$outcome="DV: Vote intention for the Greens"
mod_margins <- green_mod_mod_margins %>% filter(grepl('max_fieldratio1', factor))
mod_margins$factor[mod_margins$factor=="max_fieldratio1"] <- "Later CECT"
mod_margins$factor[mod_margins$factor=="ever_uni"] <- "Getting a post-sec degree"
mod_margins$factor <- factor(mod_margins$factor, levels=c("Getting a post-sec degree",
                                                          "Later CECT"))
mod_margins <- mod_margins %>% filter(factor=='Later CECT')


# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=life_phase, shape=life_phase)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black')) 


x # FIGURE 6, TOP PANEL
ggsave(plot=x, "plots/figure_6top.png", dpi = 300, width=4.6, height=2.5)



# Count the number of obs
pl %>% drop_na(life_phase, lean_green, max_fieldratio1, ever_uni) %>% filter(are_andnot_inhs == 1 &
                                        are_andnot_inedu == 1) %>%
  group_by(life_phase) %>% summarize(n())


# TABLE A11 (MAIN APPENDIX)
wordreg(l=list(green_mod), stars=c(0.05, 0.01, 0.001), 
        digits=3, include.ci = FALSE,
        file="tables/A11.doc",
        custom.model.names = c("DV: vote intention for the Greens"),
        custom.coef.names = c("Intercept", "CECT" , "In Post-secondary education", "Working", "Later higher edu", 
                              "Female", "CECT * In Post-secondary education", "CECT * Working"),
        reorder.coef = c(2,3,4,5,6,7,8,1),
        caption.above=T,
        caption="\\item %stars. SOEP - Effect of someone's later CECT score on attitudes")






## Model for the appendix where we use the lowest minimum age as a tracking test
green_mod_minage <- lm_robust(lean_green~ max_fieldratio1 * life_phase + ever_uni + female, 
                              data=subset(pl,  
                                          are_andnot_inhs == 1 &
                                            are_andnot_inedu == 1 & min_age < 18),   #people who we have in education and not in education
                              clusters=pid)
# TABLE A12 (MAIN APPENDIX)
wordreg(l=list(green_mod_minage), stars=c(0.05, 0.01, 0.001), 
        digits=3, include.ci = FALSE,
        file="tables/A12.doc",
        custom.model.names = c("DV: vote intention for the Greens"),
        custom.coef.names = c("Intercept", "CECT" , "In Post-secondary education", "Working", "Later higher edu", 
                              "Female", "CECT * In Post-secondary education", "CECT * Working"),
        reorder.coef = c(2,3,4,5,6,7,8,1),
        caption.above=T,
        caption="\\item %stars. SOEP - Effect of someone's later CECT score on attitudes (minimum wage is 17)")






rm(green_mod_minage, green_mod, x, mod_margins, green_mod_mod_margins)








################### -------------------- Educational analysis: Does it matter with which field someone attends education ####################


#### TWFE models. 
# model that captures general effect of graduating higher education
pl_panel <- pl %>% drop_na(pid, syear)
holdem <- plm(formula=lean_green~current_education_unitreatment,
              data=subset(pl_panel, age < 31),
              index = c("pid", "syear"),
              model="within", effect="twoway")
summary(holdem) # check no of respondents and observations
holdem <- coeftest(holdem, vcov=function(x) 
  vcovHC(x, cluster="group", type="HC1"))
holdem


# Subsetting for people who graduate with field higher than median
holdem0 <- plm(formula=lean_green~current_education_unitreatment,
              data=subset(pl_panel, max_fieldratio1 >  median(pl_panel$max_fieldratio1 ,na.rm=T) &  age < 31),
              index = c("pid", "syear"),
              model="within", effect="twoway")
summary(holdem0)
holdem0 <- coeftest(holdem0, vcov=function(x) 
  vcovHC(x, cluster="group", type="HC1"))
holdem0



# Subsetting for people who graduate with field higher lower than median
holdem1 <- plm(formula=lean_green~current_education_unitreatment,
               data=subset(pl_panel, max_fieldratio1 <=  median(pl_panel$max_fieldratio1 ,na.rm=T) &  age < 31),
               index = c("pid", "syear"),
               model="within", effect="twoway")
summary(holdem1)
holdem1 <- coeftest(holdem1, vcov=function(x) 
  vcovHC(x, cluster="group", type="HC1"))
holdem1





# TABLE A15. 
wordreg(l=list(holdem, holdem0, holdem1), use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        file="tables/A15.doc",         
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        custom.header = list("Leaning Green"=1:3),
        custom.model.names = c("Effect of attending post-secondary education", "Attending post sec with > median CECT", "Attending post sec with <= median CECT"),
        custom.coef.names = c("Attending post-secondary"),
        caption.above=T,
        caption="SOEP - Effect of attending education on attitudes by field using TWFE",
        custom.gof.rows=list("Reference group"= c("People in education or working without a post-secondary degree",
                                                  "People in education or working without a post-secondary degree and > median CECT",
                                                  "People in education or working without a post-secondary degree and <= median CECT"),
                             "Individual and time FE"= c("Yes", "Yes", "Yes"),
                             "Num. obs" =c(142558,	33120,	66866),
                             "Num. Respondents" =c(32240,	5243,	14179)),
        custom.note="\\item %stars.  Standard Errors are clustered at the respondent level")







#### Implement method from Scott (REWB) as robustness check
## Add time-specific demeaned treatment status. This estimates the change in the outcome within the individual attributable to a change
#in treatment condition, accounting for the effects of time.
pl_panel <- pl_panel %>% group_by(pid) %>% 
  mutate(mean_current_education_unitreatment = mean(current_education_unitreatment,na.rm=T)) %>% ungroup() 
pl_panel$current_education_unitreatment_dem <- pl_panel$current_education_unitreatment - pl_panel$mean_current_education_unitreatment


rewb_green <- lmer(lean_green ~ current_education_unitreatment_dem + mean_current_education_unitreatment + as.factor(syear) +
                  (1 + current_education_unitreatment_dem | pid ),
                data = subset(pl_panel,  age < 31))

rewb_green_am <- lmer(lean_green ~ current_education_unitreatment_dem + mean_current_education_unitreatment + as.factor(syear) +
                     (1 + current_education_unitreatment_dem | pid ),
                   data = subset(pl_panel, max_fieldratio1 >  median(pl_panel$max_fieldratio1 ,na.rm=T) &  age < 31))

rewb_green_bm <- lmer(lean_green ~ current_education_unitreatment_dem + mean_current_education_unitreatment + as.factor(syear) +
                        (1 + current_education_unitreatment_dem | pid ),
                      data = subset(pl_panel, max_fieldratio1 <=  median(pl_panel$max_fieldratio1 ,na.rm=T) &  age < 31))


# TABLE A16
wordreg(l=list(rewb_green, rewb_green_am, rewb_green_bm), use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        file="tables/A16.doc",         
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        custom.header = list("Leaning Green"=1:3),
        omit.coef = c("(syear)"),
        custom.model.names = c("Effect of attending post-secondary education", "Attending post sec with > median CECT", "Attending post sec with <= median CECT"),
        custom.coef.names = c("Intercept", "Attending post-secondary (within effect)", "Attending post-secondary (between effect)"),
        reorder.coef = c(2,3,1),
        caption.above=T,
        caption="SOEP - Effect of graduating education on attitudes by field using REWB",
        custom.gof.rows=list("Reference group"= c("People in education or working without a post-secondary degree",
                                                  "People in education or working without a post-secondary degree and > median CECT",
                                                  "People in education or working without a post-secondary degree and <= median CECT"),
                             "Individual and time FE"= c("Yes", "Yes", "Yes"),
                             "Num. obs" =c(142558, 33120, 66866),
                             "Num. Respondents" =c(32240,5243, 14179)),
        custom.note="\\item %stars.")




### Add a plot with both results
coefs <- c(holdem[1], holdem0[1], holdem1[1], 
           summary(rewb_green)$coefficients[[2]], summary(rewb_green_am)$coefficients[[2]], summary(rewb_green_bm)$coefficients[[2]])
SEs <- c(holdem[2], holdem0[2], holdem1[2],
         summary(rewb_green)$coefficients[,2][2], summary(rewb_green_am)$coefficients[,2][2], summary(rewb_green_bm)$coefficients[,2][2])
group <- c(rep(c("Full sample", "> median CECT", "<= median CECT"), 2))
Model <- c(rep("DV: Green intention.  Model: TWFE", 3), rep("DV: Green intention.  Model: REWB", 3))
outcome <- c(rep("Green vote intention", 6))


resdf <- data.frame(coefs=coefs, SEs=SEs, group=group, Model=Model, outcome=outcome)
resdf$conf.high <- resdf$coefs + 1.96 * resdf$SEs
resdf$conf.low <- resdf$coefs - 1.96 * resdf$SEs
resdf$group <- factor(resdf$group, 
                      levels=c("Full sample", "> median CECT", "<= median CECT"))


x <- ggplot(resdf, aes(x=coefs, y=outcome, color=group, shape=group, xmin=conf.low, xmax=conf.high)) +
  geom_pointrange(position=position_dodge2(width=0.4), size=1, linewidth=1) + 
  facet_wrap(~Model) + labs(x="Within effect of attending higher education", y="", shape="", color="") +
  scale_color_grey(end=0.6) +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 12)) 


x # FIGURE A3
ggsave(plot=x, "plots/figure_A3.png", dpi = 300, width=7.5, height=2.5)














######## More Modern methods from Xu : fect
library(fect)
pl_panel_fect <- pl %>% drop_na(pid, syear, lean_green, max_fieldratio1) %>% filter(age<31)
pl_panel_fect$max_fieldratio1_group <- NA
pl_panel_fect$max_fieldratio1_group[pl_panel_fect$max_fieldratio1 > median(pl_panel_fect$max_fieldratio1,na.rm=T)] <- "> median CECT"
pl_panel_fect$max_fieldratio1_group[pl_panel_fect$max_fieldratio1 <= median(pl_panel_fect$max_fieldratio1,na.rm=T)] <- "<= median CECT"


out.fe <- fect(lean_green ~ current_education_unitreatment, data = pl_panel_fect, index = c("pid","syear"), 
               group="max_fieldratio1_group",
               force = "two-way", method = "ife", na.rm=T,
               se = TRUE, nboots = 200, parallel = TRUE)


# TABLE 1 
print(out.fe)
print(out.fe$est.group.att)

# Find total number of observations and number of unique units
nrow(pl_panel_fect)
length(unique(pl_panel_fect$pid))

pl_panel_fect %>%
  filter(max_fieldratio1 > median(max_fieldratio1, na.rm = TRUE)) %>%
  nrow()
pl_panel_fect %>%
  filter(max_fieldratio1 > median(max_fieldratio1, na.rm = TRUE)) %>%
  distinct(pid) %>%
  nrow()

pl_panel_fect %>%
  filter(max_fieldratio1 <= median(max_fieldratio1, na.rm = TRUE)) %>%
  nrow()
pl_panel_fect %>%
  filter(max_fieldratio1 <= median(max_fieldratio1, na.rm = TRUE)) %>%
  distinct(pid) %>%
  nrow()





# Create the plots (FIGURE 7)
plot_all <- plot(out.fe, main = "Estimated ATT (IFEct)")
plot_abom <- plot(out.fe, main = "Estimated ATT (IFEct)", show.group = "> median CECT")
plot_belm <- plot(out.fe, main = "Estimated ATT (IFEct)", show.group = "<= median CECT")


plot <- plot_all + coord_cartesian(ylim=c(-0.11, 0.12), xlim=c(-6,6)) + 
  labs(y="Effect on Leaning Green", subtitle="Group: Everyone", x="Time since the Treatment (attending higher education) began") +
  theme_light(base_family = "Georgia", base_size = 12)
ggsave(plot=plot, filename="plots/figure_7top.png", dpi = 600, width=7, height=3.5)

plot <- plot_abom + coord_cartesian(ylim=c(-0.11, 0.12), xlim=c(-6,6)) + # add coord cartesian to make sure plot fits
  labs(y="Effect on Leaning Green", subtitle="Group: > Median CECT", x="Time since the Treatment (attending higher education) began") +
  theme_light(base_family = "Georgia", base_size = 12)
ggsave(plot=plot, filename="plots/figure_7middle.png", dpi = 600, width=7, height=3.5)


plot <- plot_belm + coord_cartesian(ylim=c(-0.11, 0.12), xlim=c(-6,6)) + 
  labs(y="Effect on Leaning Green", subtitle="Group: <= Median CECT", x="Time since the Treatment (attending higher education) began") +
  theme_light(base_family = "Georgia", base_size = 12)
ggsave(plot=plot, filename="plots/figure_7bottom.png", dpi = 600, width=7, height=3.5)





## Get an overview of the treatment and control conditions using panelview
library(panelView)
library(patchwork)

# Create the panelview plot (with 500 randomly selected units)
plot <- panelview(lean_green ~ current_education_unitreatment, data = pl_panel_fect, index = c("pid","syear"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", main = "SOEP: Treatment Status")

# FIGURE A.2
ggsave(plot=plot, "plots/figure_A2.png", dpi = 600, width=11, height=7)

















################### -------------------- Educational analysis: Does the effect of field become smaller when people grow older #############
## For people who are in the data since 18 and in there for at least 10 years, does the effect become smaller
# Model does not include occupational CECT because that is a post-treatment variable
pl$number_of_years_since_25 <- pl$age - 25

### Exclude people with multiple generations in their responses. 
# This is due to the few people who's birtday falls right before or after a given wave is fielded. 
age_data <- pl %>% group_by(pid) %>% mutate(number_gens = length(unique(generation))) %>% ungroup() %>%
  filter(number_gens == 1) %>% ungroup()

# multilevel model that has random effects for survey year, generation, and individual. 
age_g <- lmer(lean_green ~ max_fieldratio1 * number_of_years_since_25 +
                (1 | syear ) + (1 | generation) + (1 | pid ),
              data = age_data)
summary(age_g)





# TABLE A17. 
wordreg(l=list(age_g), use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        file="tables/A17.doc",         
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        custom.model.names = c("Leaning Green"),
        custom.coef.names = c("Intercept", "Individual CECT", "Number of years since 25", "Individual CECT X Number of years since 25"),
        reorder.coef = c(2,3,4,1),
        caption.above=T,
        caption="SOEP - Effect of attending higher education on attitudes by field using REWB",
        custom.gof.rows=list("Individual, year, generation intercepts (random effects)"= c("Yes"),
                             "Num. obs" =c(387626),
                             "Num. Respondents" =c(49071)),
        custom.note="\\item %stars.")
















################### -------------------- Occupational analysis: Effect of occupation while in education --------------------------------
# focus on the effect of someone's job score whilst they are in other life phases
mod1 <- (lm_robust(lean_green~mean_isco3_fieldscore * life_phase, 
                   data=subset(pl, are_andnot_inedu == 1),
                   cluster=pid))
summary(mod1)


mod2 <- (lm_robust(lean_green~mean_isco3_fieldscore * life_phase + max_fieldratio1, 
                   data=subset(pl, are_andnot_inedu == 1),
                   cluster=pid))

summary(mod2)

# Create the plots
mod1_margins <- margins_summary(mod1, at=list(life_phase=c("Working", "In post-secondary education", "In highschool")))
mod2_margins <- margins_summary(mod2, at=list(life_phase=c("Working", "In post-secondary education", "In highschool")))

mod1_margins$factor[mod1_margins$factor=="mean_isco3_fieldscore"] <- "Later occupational CECT"
mod2_margins$factor[mod2_margins$factor=="mean_isco3_fieldscore"] <- "Later occupational CECT"

mod1_margins <- mod1_margins %>% filter(factor=='Later occupational CECT')
mod2_margins <- mod2_margins %>% filter(factor=='Later occupational CECT')

mod1_margins$controls <- "DV: Vote intention for the Greens. Without individual CECT"
mod2_margins$controls <- "DV: Vote intention for the Greens. With individual CECT"

mod_margins <- rbind(mod1_margins, mod2_margins)
mod_margins$controls <- factor(mod_margins$controls, levels = c("DV: Vote intention for the Greens. Without individual CECT", 
                                                                "DV: Vote intention for the Greens. With individual CECT"))



# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=life_phase, shape=life_phase)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later occupational CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black'),
        strip.text.x = element_text(size = 9)) +
   facet_wrap(~controls)


x
ggsave(plot=x, "plots/figure_8top.png", dpi = 300, width=7.5, height=2.5)



# TABLE A14
wordreg(l=list(mod1, mod2), use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        file="tables/A14.doc",         
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        custom.header = list("Leaning Green"=1:2),
        custom.model.names = c("Without control for individual CECT", "With control for individual CECT"),
        custom.coef.names = c("Intercept", "Later Occupational CECT", "In Post-secondary education", "Working",
                              "In Post-secondary education X Occu CECT", "Working X Occu CECT", "Individual CECT"),
        reorder.coef =c(2,3,4,5,6,7,1),
        caption.above=T,
        caption="SOEP - Effect of occupational CECT score while someone is still studying",
        custom.note="\\item %stars.  Standard Errors are clustered at the respondent level")







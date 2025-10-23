




## Load the libraries and data
library(tidyverse)
library(ggeffects)
library(margins)
library(estimatr)
library(extrafont)
library(haven)
library(modelsummary)
library(magrittr)
library(plm)
library(lmtest)
library(texreg)
library(ggthemes)
library(lme4)

load("full_panel-3.Rda")

#font_import()
loadfonts(quiet = T, device = "pdf")
windowsFonts(Georgia = windowsFont("Georgia")) #load fonts for windows machines


## Set ggplot theme with the Georgia font
theme_set(theme_light(base_family = "Georgia", base_size = 12))




##### ------------------------ Prepare the data ---------------------------------------------------------------------- 
###    Attitudes of people whilst they are still in highschool whilst knowing their later SECT score.


##### Prepare the control variables
#Gender
full_panel$women <- ifelse(full_panel$gender==2, 1, 0)

#Rural urban and ethnicity
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  fill(urban, .direction = "downup") %>%
  fill(herkomstgroep, .direction = "downup") %>%
  ungroup()

full_panel$urban_dummy <- ifelse(full_panel$urban < 3, 1, 0)
full_panel$urban <- abs(5 - full_panel$urban)


full_panel$migration_background <- ifelse(full_panel$herkomstgroep!=0,1,0)


### Income
full_panel$nettoink[full_panel$nettoink<0] <- NA

#Fill in the NA
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  fill(nettoink, .direction = "downup") %>%
  ungroup()

full_panel$nettoink_by1000 <- full_panel$nettoink + 1
full_panel$nettoink_by1000 <- full_panel$nettoink / 1000

# Add the latest income
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  mutate(max_wave = max(wave)) %>%
  ungroup()

income <- full_panel %>% group_by(nomem_encr) %>% filter(wave==max_wave) %>%
  select(nomem_encr, nettoink_by1000) %>% rename(max_nettoinkby1000 = nettoink_by1000) %>% drop_na(max_nettoinkby1000) %>%
  distinct(nomem_encr, .keep_all=T)

full_panel <- left_join(full_panel, income)
rm(income)


## Add voting behavior outcomes
full_panel$elec_today
full_panel$vote_tan <- ifelse(full_panel$elec_today=="PVV" | full_panel$elec_today=="FvD" | full_panel$elec_today=="Verdonk", 1, 0)
full_panel$vote_green <- ifelse(full_panel$elec_today=="GL" | full_panel$elec_today=="PvdD" | full_panel$elec_today=="D66", 1, 0)


## Add the difference between GAL and TAN
full_panel$term_tan <- rowMeans(subset(full_panel, select = c(term_pvv, term_fvd, term_verdonk)), na.rm = TRUE)
full_panel$term_gal <- rowMeans(subset(full_panel, select = c(term_gl, term_pvdd, term_d66)), na.rm = TRUE)
full_panel$term_galn0d66 <- rowMeans(subset(full_panel, select = c(term_gl, term_pvdd)), na.rm = TRUE)

full_panel$dif_tangal <- full_panel$term_tan - full_panel$term_gal
full_panel$dif_tangl <- full_panel$term_tan - full_panel$term_gl
full_panel$dif_pvvgl <- full_panel$term_pvv - full_panel$term_gl
full_panel$dif_glpvv <- full_panel$term_gl - full_panel$term_pvv







######## Prepare the variables that require the panel set-up and education, such as treatment and some controls

# Respondent education dummy. This is for completing education!
full_panel$higher_edu <- NA
full_panel$higher_edu[full_panel$education >=5] <- 1 #"University #HBO is 5
full_panel$higher_edu[full_panel$education < 5] <- 0#"Non-univeristy"


#### Add a variable that captures whether people are still in highschool
# Create a dummy for in education
full_panel$ineducatio <- ifelse(full_panel$occupation==7 | full_panel$occupation==14, 1, 0)

# Create a dummy for the highest education someone attended or finished is high school
full_panel$highest_attended_highschool <- NA
full_panel$highest_attended_highschool[full_panel$education_fromworkschool_atten < 15] <- 1
full_panel$highest_attended_highschool[full_panel$education_fromworkschool_atten >= 15 & full_panel$education_fromworkschool_atten != 27] <- 0
full_panel$highest_attended_highschool[full_panel$education_fromworkschool_atten_short < 5] <- 1
full_panel$highest_attended_highschool[full_panel$education_fromworkschool_atten_short >= 5 & full_panel$education_fromworkschool_atten_short < 8] <- 0


# Create a dummy for someone being in high school
full_panel$in_highschool <- ifelse(full_panel$ineducatio == 1 & #you're in education
                                     full_panel$highest_attended_highschool==1 & #the highest degree you have attended or finished is highschool
                                     full_panel$age <20, # Also drop on age because otherwise there is a very small amount of people who are in highschool post 19
                                   1,0)
table(full_panel$in_highschool)
table(full_panel$in_highschool, full_panel$age)
table(full_panel$ineducatio, full_panel$age)

## Add a variable that starts counting the number of years someone has left education
# Add year someone leaves education with the lead of education.
full_panel <- full_panel %>% group_by(nomem_encr) %>% mutate(lead_ineducatio = dplyr::lead(ineducatio)) %>% ungroup()
full_panel$leaves_education_thisyear <- ifelse(full_panel$ineducatio != full_panel$lead_ineducatio, 1,0)
table(full_panel$age, full_panel$leaves_education_thisyear)

# For each person, get the years which are the last years in their education. If there are multiple cases, we take the last time someone is in education
xxx <- full_panel %>% group_by(nomem_encr) %>% 
  filter(leaves_education_thisyear == 1) %>%
  select(nomem_encr, year) %>% 
  rename(last_year_in_edu = year) %>% 
  filter(last_year_in_edu == max(last_year_in_edu)) %>% 
  drop_na(last_year_in_edu)

full_panel <- left_join(full_panel, xxx)

full_panel$years_since_last_year_in_edu <- full_panel$year - full_panel$last_year_in_edu
table(full_panel$years_since_last_year_in_edu)




# Add a variable for different life phases
full_panel$life_phase <- NA
full_panel$life_phase[full_panel$in_highschool==1] <- "In highschool"
full_panel$life_phase[full_panel$in_highschool==0 & full_panel$ineducatio == 1] <-  "In post-secondary education"
full_panel$life_phase[full_panel$in_highschool==0 & full_panel$ineducatio == 0] <-  "Working"
full_panel$life_phase <- as.factor(full_panel$life_phase)


full_panel$life_phase_highornon <- NA
full_panel$life_phase_highornon[full_panel$in_highschool==1] <- "In highschool"
full_panel$life_phase_highornon[full_panel$in_highschool==0 & full_panel$ineducatio == 0] <-  "Working"
full_panel$life_phase_highornon <- as.factor(full_panel$life_phase_highornon)

full_panel$life_phase_highorelse <- NA
full_panel$life_phase_highorelse[full_panel$in_highschool==1] <- "In highschool"
full_panel$life_phase_highorelse[full_panel$in_highschool==0] <-  "In post-sec or working"
full_panel$life_phase_highorelse <- as.factor(full_panel$life_phase_highorelse)



## Add a dummy that captures that we also have people post highschool. We run the analysis only on these people
# add minimum and maximum age and the mean of the in-highschool variable
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  mutate(max_age = max(age, na.rm=T), #there are quite some NAs in age, I don't get where those come from
         min_age = min(age, na.rm=T),
         years_in_panel = max_age - min_age,
         observations_peri = n(),
         mean_in_highschool = mean(in_highschool, na.rm=T),
         mean_ineducation = mean(ineducatio, na.rm=T)) %>%
  ungroup()

# Add a dummy for if you both are and are not in (high)school in the data. These people are useful
full_panel$are_andnot_inhs <- ifelse(full_panel$mean_in_highschool != 0 & full_panel$mean_in_highschool != 1, 1, 0)

full_panel$are_andnot_inedu <- ifelse(full_panel$mean_ineducation != 0 & full_panel$mean_ineducation != 1, 1, 0)




####### Get the CECT in the latest wave someone is in the survey

full_panel$field_agri[full_panel$field_agri<0] <- NA
full_panel$field_art[full_panel$field_art<0] <- NA
full_panel$field_cat[full_panel$field_cat<0] <- NA
full_panel$field_dk[full_panel$field_dk<0] <-NA
full_panel$field_eco[full_panel$field_eco<0] <- NA
full_panel$field_gen[full_panel$field_gen<0] <- NA
full_panel$field_hum[full_panel$field_hum<0] <- NA
full_panel$field_law[full_panel$field_law<0] <- NA
full_panel$field_math[full_panel$field_math<0] <- NA
full_panel$field_medi[full_panel$field_medi<0] <- NA
full_panel$field_other[full_panel$field_other<0] <- NA
full_panel$field_pers[full_panel$field_pers<0] <- NA
full_panel$field_pubo[full_panel$field_pubo<0] <- NA
full_panel$field_soc[full_panel$field_soc<0] <- NA
full_panel$field_teach[full_panel$field_teach<0] <- NA
full_panel$field_tech[full_panel$field_tech<0] <- NA
full_panel$field_telec[full_panel$field_telec<0] <- NA
full_panel$field_trans[full_panel$field_trans<0] <- NA


## Add how many fields a given person chooses in a given wave
full_panel$no_fields_chosen <- NA #note that this doesn't include general, other, and don't know
full_panel$no_fields_chosen <-     full_panel$field_agri + full_panel$field_art + full_panel$field_cat + 
                                   full_panel$field_eco + full_panel$field_hum + full_panel$field_law +
                                   full_panel$field_math + full_panel$field_medi + full_panel$field_pers +
                                   full_panel$field_pubo + full_panel$field_soc + full_panel$field_teach +
                                   full_panel$field_tech + full_panel$field_telec + full_panel$field_trans


## Add the culture score for a given person. Note that we take the average of multiple scores reported. If no scores reported but general or other reported we take that value
full_panel$score_cult <- 
  full_panel$field_agri * 2.1 + full_panel$field_art * 3.49 + full_panel$field_cat * 2.66 + 
  full_panel$field_eco * 2.03 + full_panel$field_hum * 3.49 + full_panel$field_law * 2.19 +
  full_panel$field_math * 2.01 + full_panel$field_medi * 1.87 + full_panel$field_pers * 2.4 +
  full_panel$field_pubo * 2.41 + full_panel$field_soc * 2.66 + full_panel$field_teach * 3.6 +
  full_panel$field_tech * 2.01 + full_panel$field_telec * 2.01 + full_panel$field_trans * 2.01

full_panel$score_cult <- full_panel$score_cult / full_panel$no_fields_chosen
full_panel$score_cult[full_panel$no_fields_chosen == 0 & (full_panel$field_gen == 1 | full_panel$field_other==1)] <- 2.94


# Add the econ score
full_panel$score_econ <- 
  full_panel$field_agri * 2.88 + full_panel$field_art * 1.71 + full_panel$field_cat * 2.24 + 
  full_panel$field_eco * 3.41 + full_panel$field_hum * 1.71 + full_panel$field_law * 3.31 +
  full_panel$field_math * 2.14 + full_panel$field_medi * 1.82 + full_panel$field_pers * 1.82 +
  full_panel$field_pubo * 2.84 + full_panel$field_soc * 2.24 + full_panel$field_teach * 1.87 +
  full_panel$field_tech * 2.14 + full_panel$field_telec * 2.14 + full_panel$field_trans * 2.14

full_panel$score_econ <- full_panel$score_econ / full_panel$no_fields_chosen
full_panel$score_econ[full_panel$no_fields_chosen == 0 & (full_panel$field_gen == 1 | full_panel$field_other==1)] <- 2.11


# Add the com score
full_panel$score_com <- 
  full_panel$field_agri * 1.97 + full_panel$field_art * 2.76 + full_panel$field_cat * 3.76 + 
  full_panel$field_eco * 2.54 + full_panel$field_hum * 2.76 + full_panel$field_law * 2.61 +
  full_panel$field_math * 1.95 + full_panel$field_medi * 2.97 + full_panel$field_pers * 2.26 +
  full_panel$field_pubo * 3.17 + full_panel$field_soc * 3.76 + full_panel$field_teach * 3.64 +
  full_panel$field_tech * 1.95 + full_panel$field_telec * 1.95 + full_panel$field_trans * 1.95

full_panel$score_com <- full_panel$score_com / full_panel$no_fields_chosen
full_panel$score_com[full_panel$no_fields_chosen == 0 & (full_panel$field_gen == 1 | full_panel$field_other==1)] <- 2.01

  
# Add the tech score
full_panel$score_tech <- 
  full_panel$field_agri * 3.06 + full_panel$field_art * 1.61 + full_panel$field_cat * 1.54 + 
  full_panel$field_eco * 2.06 + full_panel$field_hum * 1.61 + full_panel$field_law * 1.74 +
  full_panel$field_math * 3.42 + full_panel$field_medi * 2.14 + full_panel$field_pers * 1.52 +
  full_panel$field_pubo * 2.02 + full_panel$field_soc * 1.54 + full_panel$field_teach * 1.77 +
  full_panel$field_tech * 3.42 + full_panel$field_telec * 3.42 + full_panel$field_trans * 3.42

full_panel$score_tech <- full_panel$score_tech / full_panel$no_fields_chosen
full_panel$score_tech[full_panel$no_fields_chosen == 0 & (full_panel$field_gen == 1 | full_panel$field_other==1)] <- 2.04


# When someone has only done primary school the different field scores should be set to 1 according to vd werfhorst 
full_panel$score_cult[full_panel$education==1] <- 1
full_panel$score_econ[full_panel$education==1] <- 1
full_panel$score_com[full_panel$education==1] <- 1
full_panel$score_tech[full_panel$education==1] <- 1

# Add the added up scores
full_panel$cultcomm <- full_panel$score_cult + full_panel$score_com
full_panel$econtech <- full_panel$score_tech + full_panel$score_econ
full_panel$score_diff <- full_panel$cultcomm - full_panel$econtech
full_panel$score_ratio <- full_panel$cultcomm / (full_panel$cultcomm + full_panel$econtech)
full_panel$score_ratio_01 <- (full_panel$score_ratio - min(full_panel$score_ratio,na.rm=T)) / (max(full_panel$score_ratio,na.rm=T) - min(full_panel$score_ratio, na.rm=T))


# Change the NaNs to NAs. These are people who selected no fields and also didn't select other or general. Thus 0/0
full_panel$score_ratio_01[is.nan(full_panel$score_ratio_01)] <- NA


# Fill forward the score_ratio_01 variable. Missingess in fields seems to be caused by people not filling in the variable and they drop it in the last wave
full_panel$score_ratio_01_filfor <- full_panel$score_ratio_01
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  fill(score_ratio_01_filfor, .direction="down") %>%
  ungroup()


# Add the latest observation of this filled forward score ratio. It is our main right hand side variable
fields <- full_panel %>% group_by(nomem_encr) %>% filter(wave==max_wave) %>%
  select(nomem_encr, score_ratio_01_filfor, score_ratio_01) %>% 
  rename(max_score_ratio_01_filfor = score_ratio_01_filfor,
         max_score_ratio_01 = score_ratio_01) %>% distinct(nomem_encr, .keep_all = T)

full_panel <- left_join(full_panel, fields)
rm(fields)


## Add education in different fields. 
full_panel$max_field_three <- NA
full_panel$max_field_three[full_panel$field_agri==1] <- "STEM"
full_panel$max_field_three[full_panel$field_cat==1] <- "CTSS"
full_panel$max_field_three[full_panel$field_eco==1] <- "BLS"
full_panel$max_field_three[full_panel$field_art==1 | full_panel$field_hum==1] <- "CTSS"
full_panel$max_field_three[full_panel$field_law==1] <- "BLS"
full_panel$max_field_three[full_panel$field_math==1] <- "STEM"
full_panel$max_field_three[full_panel$field_medi==1] <- "CTSS"
full_panel$max_field_three[full_panel$field_pers==1] <- "BLS"
full_panel$max_field_three[full_panel$field_pubo==1] <- "BLS"
full_panel$max_field_three[full_panel$field_soc==1] <- "CTSS"
full_panel$max_field_three[full_panel$field_teach==1] <- "CTSS"
full_panel$max_field_three[full_panel$field_tech==1] <- "STEM"
full_panel$max_field_three[full_panel$field_telec==1] <- "STEM"
full_panel$max_field_three[full_panel$field_trans==1] <- "BLS"





## Get the later education level of someone. We use the normal education variable here as it's common in research to use this one.
#     However, we're also interested in what people study so we also use the attending variable

edu <- full_panel %>% group_by(nomem_encr) %>% filter(wave==max_wave) %>%
  select(nomem_encr, education) %>% rename(max_education = education) %>% drop_na(max_education) 
full_panel <- left_join(full_panel, edu)

# Fill forward the other educational variables and merge those in too
full_panel$education_fromworkschool_atten_short[full_panel$education_fromworkschool_atten_short==99] <- NA

full_panel$education_fromworkschool_atten_filfor <- full_panel$education_fromworkschool_atten

full_panel <- full_panel %>% group_by(nomem_encr) %>%
  fill(education_fromworkschool_atten_filfor, .direction="down") %>%
  ungroup()

full_panel <- full_panel %>% group_by(nomem_encr) %>% 
  mutate(max_education_fromworkschool_atten_filfor = max(education_fromworkschool_atten_filfor, na.rm=T),
         max_education_fromworkschool_atten_short = max(education_fromworkschool_atten_short, na.rm=T)) %>% ungroup()


rm(xxx, edu)
gc()




# Dummy for later attending higher education. 
full_panel$later_higher_educated <- NA
full_panel$later_higher_educated[full_panel$max_education_fromworkschool_atten_filfor > 18 & # post secondary education only
                                   full_panel$max_education_fromworkschool_atten_filfor != 27] <- 1

full_panel$later_higher_educated[is.na(full_panel$later_higher_educated) & #is NA because we trust the info from the longer variable more. 
                                   full_panel$max_education_fromworkschool_atten_short > 4 &
                                   full_panel$max_education_fromworkschool_atten_short != 99] <- 1

full_panel$later_higher_educated[!is.na(full_panel$max_education_fromworkschool_atten_filfor) &
                                          full_panel$max_education_fromworkschool_atten_filfor != 27 &
                                   is.na(full_panel$later_higher_educated)] <- 0

full_panel$later_higher_educated[is.na(full_panel$later_higher_educated) & 
                                   !is.na(full_panel$max_education_fromworkschool_atten_short) &
                                   full_panel$max_education_fromworkschool_atten_short != 99 &
                                   is.na(full_panel$later_higher_educated)] <- 0


# Dummy for later getting a higher education degree
full_panel$later_higher_educated_deg <-ifelse(full_panel$max_education > 4, 1, 0) #includes HBO





# Fill the occupation variables forward
full_panel <- full_panel %>% group_by(nomem_encr) %>%
  fill(sector_402, .direction="down") %>%
  fill(profession_404, .direction="down") %>%
  fill(supervisor_409, .direction="down") %>%
  ungroup()






#### Add CECT scores per occupation

#self employed variable
full_panel$selfemployed <- NA
full_panel$selfemployed[full_panel$occupation==3] <- 1
full_panel$selfemployed[full_panel$occupation!=3] <- 0

#simplify sector variable slightly by collapsing the small mining category into agriculture
full_panel$sector1 <- full_panel$sector_402
full_panel$sector1[full_panel$sector_402==2] <- 1

## First way to get the CECT scores.
#crosstab profession and sector and calculate mean CECT (using individual's latest CECT) || exclude those for whom we have neither info on profession and on sector,
# but include if we have info on one of these, note that encompasses quite a lot of retired, in school, homemakers, unemployed*/

## Calculate the scores
# the non-lean version (meanscore) takes everyone
scores <- full_panel %>% filter(ineducatio==0) %>%
  group_by(sector1, profession_404) %>% 
  summarize(meanscore=mean(max_score_ratio_01_filfor, na.rm=T),
         medscore=median(max_score_ratio_01_filfor, na.rm=T),
         sdcore=sd(max_score_ratio_01_filfor, na.rm=T),
         countscore=n()) %>% ungroup() %>% drop_na(sector1, profession_404)

# Merge them back in
full_panel <- left_join(full_panel, scores)


## Second way to get the CECT scores. The version that grabs it only from people who are in a job
# post-education respondents who are actively in a job (occupation<4)
scores2 <- full_panel %>% filter(ineducatio==0 & !is.na(sector1) & !is.na(profession_404) & occupation < 4) %>%
  group_by(sector1, profession_404) %>% 
  summarize(meanscore_lean=mean(max_score_ratio_01_filfor, na.rm=T),
            medscore_lean=median(max_score_ratio_01_filfor, na.rm=T),
            sdcore_lean=sd(max_score_ratio_01_filfor, na.rm=T),
            countscore_lean=n()) %>% ungroup()
full_panel <- left_join(full_panel, scores2)
rm(scores, scores2)

sum(table(full_panel$meanscore))
sum(table(full_panel$meanscore_lean))

full_panel$meanscore_lean[full_panel$occupation > 3] <- NA
full_panel$medscore_lean[full_panel$occupation > 3] <- NA
full_panel$sdcore_lean[full_panel$occupation > 3] <- NA
full_panel$countscore_lean[full_panel$occupation > 3] <- NA





## Add variables for within-effect of education analysis
# Graduating with a particular CECT score
full_panel$education_cect_treatment <- NA
full_panel$education_cect_treatment[full_panel$ineducatio==1] <- "In education"
full_panel$education_cect_treatment[full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor <= 0.25 ] <-  "Graduated - 0 to 0.25 CECT"
full_panel$education_cect_treatment[full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.25 & full_panel$max_score_ratio_01_filfor <= 0.50 ] <-  "Graduated - 0.25 to 0.5 CECT"
full_panel$education_cect_treatment[full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.5 & full_panel$max_score_ratio_01_filfor <= 0.75 ] <-  "Graduated - 0.51 to 0.75 CECT"
full_panel$education_cect_treatment[full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.75 ] <-  "Graduated - 0.75 to 1 CECT"

full_panel$education_cect_treatment <- factor(full_panel$education_cect_treatment)
full_panel$education_cect_treatment <- relevel(full_panel$education_cect_treatment, ref="In education")
table(full_panel$education_cect_treatment)


# Same variable with university
full_panel$education_cect_treatment_wuni <- NA
full_panel$education_cect_treatment_wuni[full_panel$ineducatio==1] <- "In education"
# non-uni grads
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 0 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor <= 0.25 ] <-  "Graduated - 0 to 0.25 CECT (University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 0 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.25 & full_panel$max_score_ratio_01_filfor <= 0.50 ] <-  "Graduated - 0.25 to 0.5 CECT (University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 0 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.5 & full_panel$max_score_ratio_01_filfor <= 0.75 ] <-  "Graduated - 0.51 to 0.75 CECT (University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 0 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.75 ] <-  "Graduated - 0.75 to 1 CECT (University)"

#uni grads
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 1 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor <= 0.25 ] <-  "Graduated - 0 to 0.25 CECT (Not University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 1 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.25 & full_panel$max_score_ratio_01_filfor <= 0.50 ] <-  "Graduated - 0.25 to 0.5 CECT (Not University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 1 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.5 & full_panel$max_score_ratio_01_filfor <= 0.75 ] <-  "Graduated - 0.51 to 0.75 CECT (Not University)"
full_panel$education_cect_treatment_wuni[full_panel$later_higher_educated == 1 & full_panel$ineducatio==0 & full_panel$max_score_ratio_01_filfor > 0.75 ] <-  "Graduated - 0.75 to 1 CECT (Not University)"

full_panel$education_cect_treatment_wuni <- factor(full_panel$education_cect_treatment_wuni)
full_panel$education_cect_treatment_wuni <- relevel(full_panel$education_cect_treatment_wuni, ref="In education")


# Create a variable where you look at graduating higher education by CECT
full_panel$education_cect_treatment_onlyuni <- as.character(full_panel$education_cect_treatment_wuni)
full_panel$education_cect_treatment_onlyuni[grepl("Not University", full_panel$education_cect_treatment_onlyuni)] <- "control"
full_panel$education_cect_treatment_onlyuni[full_panel$education_cect_treatment_onlyuni=="Not University"] <- "control"
full_panel$education_cect_treatment_onlyuni[full_panel$education_cect_treatment_onlyuni=="In education"] <- "control"
full_panel$education_cect_treatment_onlyuni <- factor(full_panel$education_cect_treatment_onlyuni)
full_panel$education_cect_treatment_onlyuni <- relevel(full_panel$education_cect_treatment_onlyuni, ref="control")









######### ----- add survey weights 
### For the model that uses highschool students
final_sample <- full_panel %>% drop_na(dif_glpvv, max_score_ratio_01_filfor, 
                                       life_phase_highornon, later_higher_educated_deg,
                                       are_andnot_inhs, are_andnot_inedu) %>% filter(are_andnot_inhs ==1  & are_andnot_inedu == 1)

observations <- sum(table(final_sample$gender))
full_panel$weight_mainedumodel <- NA
full_panel$weight_mainedumodel[full_panel$gender==2] <- 1/ (table(final_sample$gender)[2] / observations)
full_panel$weight_mainedumodel[full_panel$gender==1] <- 1/ (table(final_sample$gender)[1] / observations)



#### For the model with years since in education
# with controls
final_sample <- full_panel %>% drop_na(dif_glpvv, max_score_ratio_01_filfor, 
                                       life_phase_highornon, later_higher_educated_deg, years_since_last_year_in_edu, 
                                       women, nettoink_by1000, urban, age, migration_background,
                                       are_andnot_inedu) %>% filter(are_andnot_inedu == 1)

observations <- sum(table(final_sample$gender))
full_panel$weight_yearsedumodel <- NA
full_panel$weight_yearsedumodel[full_panel$gender==2] <- 1/ (table(final_sample$gender)[2] / observations)
full_panel$weight_yearsedumodel[full_panel$gender==1] <- 1/ (table(final_sample$gender)[1] / observations)

# without controls
final_sample <- full_panel %>% drop_na(dif_glpvv, max_score_ratio_01_filfor, 
                                       later_higher_educated_deg, years_since_last_year_in_edu, 
                                        are_andnot_inedu) %>% filter(are_andnot_inedu == 1)

observations <- sum(table(final_sample$gender))
full_panel$weight_yearsedumodelnocont <- NA
full_panel$weight_yearsedumodelnocont[full_panel$gender==2] <- 1/ (table(final_sample$gender)[2] / observations)
full_panel$weight_yearsedumodelnocont[full_panel$gender==1] <- 1/ (table(final_sample$gender)[1] / observations)




# Save the DF in stata format for Marks and Hooghe
full_panel_forstata <- full_panel %>%
  rename(edu_fws_atten_short=education_fromworkschool_atten_short,
         edu_fws_atten_filfor=education_fromworkschool_atten_filfor,
         max_edu_fws_atten_filfor=max_education_fromworkschool_atten_filfor) %>% select(-max_education_fromworkschool_atten_short)



## Save the data for stata  
#write_dta(full_panel_forstata, "LISS data/full_panel.dta") 







########### ----------------  The analysis with high school vs no education #############

# people who we have when they're not in education when they're in post secondary and beyond
tan_mod <- lm_robust(term_tan~(max_score_ratio_01_filfor) * life_phase_highorelse + later_higher_educated_deg, 
                     data=subset(full_panel,  
                                 are_andnot_inhs ==1  & #people who we have in high school and not in high school
                                   are_andnot_inedu == 1),   #people who we have in education and not in education
                     clusters=nomem_encr)
summary(tan_mod)


gal_mod <- lm_robust(term_gal~(max_score_ratio_01_filfor) * life_phase_highorelse + later_higher_educated_deg, 
                     data=subset(full_panel,  
                                 are_andnot_inhs ==1  & #people who we have in high school and not in high school
                                   are_andnot_inedu == 1),   #people who we have in education and not in education
                     clusters=nomem_encr)
summary(gal_mod)




# TABLE A10
wordreg(l=list(tan_mod, gal_mod), stars=c(0.05, 0.01, 0.001), 
        digits=3, include.ci = FALSE,
        file="tables/A10.doc",
        custom.model.names = c("TAN parties", "GAL parties"),
        custom.coef.names = c("Intercept", "CECT" , "In Post-sec or working", "Later higher edu", 
                              "CECT * In Post-sec or working"),
        reorder.coef = c(2,3,4,5,1),
        caption.above=T,
        caption="\\item %stars. LISS - Effect of someone's later CECT score on attitudes")
###Viktar's update




# Get the marginal effects in each highschool group of all the variables
pvv_mod_margins <- margins_summary(tan_mod, at=list(life_phase_highorelse=c("In post-sec or working", "In highschool")))
pvv_mod_margins$outcome="DV: TAN parties thermostat"
mod_margins <- pvv_mod_margins %>% filter(!grepl('life_phase_highorelse', factor))
mod_margins$factor[mod_margins$factor=="max_score_ratio_01_filfor"] <- "Later CECT"
mod_margins$factor[mod_margins$factor=="later_higher_educated_deg"] <- "Getting a post-sec degree"
mod_margins$factor <- factor(mod_margins$factor, levels=c("Woman",
                                                          "Getting a post-sec degree",
                                                          "Later CECT"))
mod_margins <- mod_margins %>% filter(factor=='Later CECT')


# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=life_phase_highorelse, shape=life_phase_highorelse)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black')) 

 
x #FIGURE 6 BOTTOM
ggsave(plot=x, "plots/figure6_bottom.png", dpi = 600,  width=4.6, height=2.5)




#### Same plot for GAL parties
# Get the marginal effects in each highschool group of all the variables
gal_mod_margins <- margins_summary(gal_mod, at=list(life_phase_highorelse=c("In post-sec or working", "In highschool")))
gal_mod_margins$outcome="DV: GAL parties thermostat"
mod_margins <- gal_mod_margins %>% filter(!grepl('life_phase_highorelse', factor))
mod_margins$factor[mod_margins$factor=="max_score_ratio_01_filfor"] <- "Later CECT"
mod_margins$factor[mod_margins$factor=="later_higher_educated_deg"] <- "Getting a post-sec degree"
mod_margins$factor <- factor(mod_margins$factor, levels=c("Woman",
                                                          "Getting a post-sec degree",
                                                          "Later CECT"))
mod_margins <- mod_margins %>% filter(factor=='Later CECT')


# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=life_phase_highorelse, shape=life_phase_highorelse)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black')) 


x # FIGURE 6 MIDDLE
ggsave(plot=x, "plots/figure_6middle.png", dpi = 600,  width=4.6, height=2.5)


















########### ---------------- Occupational analysis: predicting effect of later CECT #############
## Add a variable that captures the first year someone leaves education 
full_panel_panel <- pdata.frame(full_panel, index = c("nomem_encr", "year"))
full_panel_panel$lag_ineducatio <- plm::lag(full_panel_panel$ineducatio, k=1L)


## Get the mean occupational score for people who are not in education anymore
xx <- full_panel_panel %>% filter(are_andnot_inedu==1 & ineducatio == 0) %>% # people we have in and not in education when they're working
  group_by(nomem_encr) %>% #per individual
  mutate(mean_meanscore = mean(meanscore,na.rm=T)) %>% #what is the mean score for their jobs Working
 select(nomem_encr, mean_meanscore) %>% distinct(nomem_encr, .keep_all=T)

full_panel_panel <- full_join(full_panel_panel, xx)

# fill all the NAs so that the job score is there when someone is in highschool
full_panel_panel <- full_panel_panel %>% group_by(nomem_encr) %>% 
  fill(mean_meanscore, .direction=c("downup")) %>% ungroup()



#Calculate the weights
final_sample <- full_panel_panel %>% drop_na(dif_glpvv, mean_meanscore, ineducatio, max_score_ratio_01_filfor,
                                       ineducatio, are_andnot_inedu) %>%  filter(are_andnot_inedu == 1)

observations <- sum(table(final_sample$gender))
full_panel_panel$weight_occupmodel <- NA
full_panel_panel$weight_occupmodel[full_panel_panel$gender==2] <- 1/ (table(final_sample$gender)[2] / observations)
full_panel_panel$weight_occupmodel[full_panel_panel$gender==1] <- 1/ (table(final_sample$gender)[1] / observations)






##### focus on the effect of someone's job score whilst they are still in their education
### TAN parties
mod1 <- (lm_robust(term_tan~mean_meanscore * ineducatio, 
                  data=subset(full_panel_panel, are_andnot_inedu == 1),
                  cluster=nomem_encr, weights=weight_occupmodel))
summary(mod1)
# and in the same setup as the education models with individual cect included
mod2 <- (lm_robust(term_tan~mean_meanscore * ineducatio + max_score_ratio_01_filfor, 
                  data=subset(full_panel_panel, are_andnot_inedu == 1),
                  cluster=nomem_encr, weights=weight_occupmodel))
summary(mod2)




# Create the plots
mod1_margins <- margins_summary(mod1, at=list(ineducatio=c(0,1)))
mod2_margins <- margins_summary(mod2, at=list(ineducatio=c(0,1)))

mod1_margins$factor[mod1_margins$factor=="mean_meanscore"] <- "Later occupational CECT"
mod2_margins$factor[mod2_margins$factor=="mean_meanscore"] <- "Later occupational CECT"

mod1_margins$ineducatio[mod1_margins$ineducatio==0] <- "In education"
mod1_margins$ineducatio[mod1_margins$ineducatio=="1"] <- "Working"
mod2_margins$ineducatio[mod2_margins$ineducatio==0] <- "In education"
mod2_margins$ineducatio[mod2_margins$ineducatio=="1"] <- "Working"

mod1_margins <- mod1_margins %>% filter(factor=='Later occupational CECT')
mod2_margins <- mod2_margins %>% filter(factor=='Later occupational CECT')

mod1_margins$controls <- "DV: TAN parties. Without individual CECT"
mod2_margins$controls <- "DV: TAN parties. With individual CECT"

mod_margins <- rbind(mod1_margins, mod2_margins)
mod_margins$ineducatio <- factor(mod_margins$ineducatio, levels=c("Working", "In education"))
mod_margins$controls <- factor(mod_margins$controls, levels = c("DV: TAN parties. Without individual CECT", "DV: TAN parties. With individual CECT"))




# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=ineducatio, shape=ineducatio)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later occupational CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black')) +
   facet_wrap(~controls)


x # FIGURE 8 BOTTOM
ggsave(plot=x, "plots/figure8_bottom.png", dpi = 600, width=7.5, height=2.5)






### GAL parties
mod3 <- (lm_robust(term_gal~mean_meanscore * ineducatio, 
                   data=subset(full_panel_panel, are_andnot_inedu == 1),
                   cluster=nomem_encr, weights=weight_occupmodel))
summary(mod3)
# and in the same setup as the education models with individual cect included
mod4 <- (lm_robust(term_gal~mean_meanscore * ineducatio + max_score_ratio_01_filfor, 
                   data=subset(full_panel_panel, are_andnot_inedu == 1),
                   cluster=nomem_encr, weights=weight_occupmodel))
summary(mod4)




# Create the plots
mod1_margins <- margins_summary(mod3, at=list(ineducatio=c(0,1)))
mod2_margins <- margins_summary(mod4, at=list(ineducatio=c(0,1)))

mod1_margins$factor[mod1_margins$factor=="mean_meanscore"] <- "Later occupational CECT"
mod2_margins$factor[mod2_margins$factor=="mean_meanscore"] <- "Later occupational CECT"

mod1_margins$ineducatio[mod1_margins$ineducatio==0] <- "In education"
mod1_margins$ineducatio[mod1_margins$ineducatio=="1"] <- "Working"
mod2_margins$ineducatio[mod2_margins$ineducatio==0] <- "In education"
mod2_margins$ineducatio[mod2_margins$ineducatio=="1"] <- "Working"

mod1_margins <- mod1_margins %>% filter(factor=='Later occupational CECT')
mod2_margins <- mod2_margins %>% filter(factor=='Later occupational CECT')

mod1_margins$controls <- "DV: GAL parties. Without individual CECT"
mod2_margins$controls <- "DV: GAL parties. With individual CECT"

mod_margins <- rbind(mod1_margins, mod2_margins)
mod_margins$ineducatio <- factor(mod_margins$ineducatio, levels=c("Working", "In education"))
mod_margins$controls <- factor(mod_margins$controls, levels = c("DV: GAL parties. Without individual CECT", "DV: GAL parties. With individual CECT"))




# Create the forest plots
x <- ggplot(mod_margins, aes(x=AME, y=factor, xmin=lower, xmax=upper, 
                             color=ineducatio, shape=ineducatio)) +
  geom_pointrange(position = position_dodge(width = 0.4), size=0.8) + 
  geom_vline(xintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Effect of a one-unit increase in later occupational CECT") + ylab("") +
  facet_grid(cols = vars(outcome)) + 
  scale_color_grey(end=0.6) +
  labs(color="", shape="")+
  theme(legend.position = "bottom", axis.text.y = element_blank(), 
        strip.background =element_rect(fill="lightgrey"),
        strip.text = element_text(colour = 'black')) +
  facet_wrap(~controls)


x  # FIGURE 8 middle
ggsave(plot=x, "plots/figure_8middle.png", dpi = 600, width=7.5, height=2.5)








# TABLE A13
wordreg(l=list(mod1, mod2, mod3, mod4), use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        file="tables/A13.doc",         
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        custom.header = list("TAN parties thermostat"=1:2, "GAL parties thermostat"=3:4),
        custom.model.names = c("Without control for individual CECT", "With control for individual CECT", "Without control for individual CECT", "With control for individual CECT"),
        custom.coef.names = c("Intercept", "Later Occupational CECT", "In education", "Later Occupational CECT * In education", "Later individual CECT"),
        reorder.coef =c(2,3,4,5,1),
        caption.above=T,
        caption="LISS - Effect of occupational CECT score while someone is still studying",
        custom.note="\\item %stars.  Standard Errors are clustered at the respondent level")










########### ---------------- Robustness check using latest LISS wave #############


ESS_robus <- full_panel %>% filter(year > 13)

ESS_robus$nettoink_1000 <- ESS_robus$nettoink + 1
ESS_robus$nettoink_1000 <- ESS_robus$nettoink / 1000


x1 <- lm_robust(term_tan ~ score_ratio_01 +
                  higher_edu + gender + nettoink_1000 + age + migration_background + urban + 
                  profession_404 + sector_402 + supervisor_409,
                data=ESS_robus)

x2 <- lm_robust(term_gal ~ score_ratio_01 +
                  higher_edu + gender + nettoink_1000 + age + migration_background + urban + 
                  profession_404 + sector_402 + supervisor_409,
                data=ESS_robus)


# TABLE A18
wordreg(l=list(x1, x2),use.packages=F,
        stars=c(0.05, 0.01, 0.001),
        digits=3, include.ci = FALSE, booktabs=T, threeparttable=T,
        file="tables/A18.doc",      
       omit.coef = c("(profession)|(supervisor)|(sector)"),
       custom.model.names = c("DV: TAN Thermostat", "GAL Thermostat"),
       custom.coef.names = c("Intercept", "CECT", "Higher Education", "Female", "Income in 1000", "Age", "Migration Background", "Urban"),
       reorder.coef =c(2,3,4,5,6,7,8,1),
       custom.gof.rows=list("FE for occupation"= c("Yes", "Yes"),
                            "FE for sector"= c("Yes", "Yes"),
                            "FE for supervising"= c("Yes", "Yes")))




#504 Final 
#12/12/2025
#processing ESS data and merging with CHES data 
###
library(haven)
library(tidyverse)
library(ggplot2)
library(psych)
library(lme4)
library(numDeriv)
library(patchwork)
library(broom.mixed)
library(modelsummary)
library(sandwich)
library(lmtest)
######
#reading in raw ess data 
ess2 <- read.csv("ESS2e03_6.csv")
ess3 <- read.csv("ESS3e03_7.csv")
ess4 <- read.csv("ESS4e04_6.csv")
ess4LT <- read.csv("ESS4LT.csv")
ess3RO <- read.csv("ESS3RO.csv")
ess3LV <- read.csv("ESS3LV.csv")
####
#Subset for communist countries 
ess2_c <- 
  ess2 %>%
  filter(cntry %in% c("CZ", "EE", "HU", "PL", "SI", "SK"));
rm(ess2)

ess3_c <-
  ess3 %>%
  filter(cntry %in% c("BG", "EE", "HU", "PL", "SI", "SK"));
rm(ess3)

ess4_c <- 
  ess4 %>%
  filter(cntry %in% c(
    "BG", "CZ", "EE", "HR", "HU", "LV", "PL", "RO", "SI", "SK"));
rm(ess4)
##Merging
harmonize_ess <- function(df) {
  if ("ctzshipa" %in% names(df)) {
    df %>% mutate(ctzshipa = as.character(ctzshipa))
  } else {
    df
  }
}

ess_all <- bind_rows(
  harmonize_ess(ess2_c),
  harmonize_ess(ess3_c),
  harmonize_ess(ess3LV),
  harmonize_ess(ess3RO),
  harmonize_ess(ess4_c),
  harmonize_ess(ess4LT)
)
write.csv(ess_all, "ESS_all_waves_merged.csv")
##########
#CECT scores coding 
ess_all <-
  ess_all %>%
  mutate(
    edutype = edufld   
  )

ess_all <- 
  ess_all %>%
  mutate(
    fieldculture = case_when(
      edutype == 1                     ~ 2.94,   # general
      edutype %in% c(2, 3)             ~ 3.49,   # arts, humanities
      edutype %in% c(4, 14)            ~ 2.01,   # technical
      edutype == 5                     ~ 2.10,   # agriculture
      edutype %in% c(6, 7)             ~ 3.60,   # teacher educ
      edutype == 8                     ~ 1.87,   # medical
      edutype == 9                     ~ 2.03,   # economics/admin
      edutype == 10                    ~ 2.66,   # social-cultural
      edutype == 11                    ~ 2.19,   # law
      edutype == 12                    ~ 2.40,   # personal care
      edutype == 13                    ~ 2.41,   # police
      TRUE                             ~ NA_real_
    )
  )

ess_all <-
  ess_all %>%
  mutate(
    fieldecon = case_when(
      edutype == 1                     ~ 2.11,
      edutype %in% c(2, 3)             ~ 1.71,
      edutype %in% c(4, 7, 14)         ~ 2.14,
      edutype == 5                     ~ 2.88,
      edutype == 6                     ~ 1.87,
      edutype == 8                     ~ 1.82,
      edutype == 9                     ~ 3.41,
      edutype == 10                    ~ 2.24,
      edutype == 11                    ~ 3.31,
      edutype == 12                    ~ 1.82,
      edutype == 13                    ~ 2.84,
      TRUE                             ~ NA_real_
    )
  )

ess_all <- 
  ess_all %>%
  mutate(
    fieldcomm = case_when(
      edutype == 1                     ~ 2.01,
      edutype %in% c(2, 3)             ~ 2.76,
      edutype %in% c(4, 14)            ~ 1.95,
      edutype == 5                     ~ 1.97,
      edutype %in% c(6, 7)             ~ 3.64,
      edutype == 8                     ~ 2.97,
      edutype == 9                     ~ 2.54,
      edutype == 10                    ~ 3.76,
      edutype == 11                    ~ 2.61,
      edutype == 12                    ~ 2.26,
      edutype == 13                    ~ 3.17,
      TRUE                             ~ NA_real_
    )
  )

ess_all <-
  ess_all %>%
  mutate(
    fieldtech = case_when(
      edutype == 1                     ~ 2.04,
      edutype %in% c(2, 3)             ~ 1.61,
      edutype %in% c(4, 7, 14)         ~ 3.42,
      edutype == 5                     ~ 3.06,
      edutype == 6                     ~ 1.77,
      edutype == 8                     ~ 2.14,
      edutype == 9                     ~ 2.06,
      edutype == 10                    ~ 1.54,
      edutype == 11                    ~ 1.74,
      edutype == 12                    ~ 1.52,
      edutype == 13                    ~ 2.02,
      TRUE                             ~ NA_real_
    )
  )

ess_all <- 
  ess_all %>%
  mutate(
    educ5 = ifelse(!is.na(edulvla), edulvla, NA)
  )

ess_all <- 
  ess_all %>%
  mutate(
    fieldculture = ifelse(educ5 == 1 & !is.na(edutype), 1, fieldculture),
    fieldecon    = ifelse(educ5 == 1 & !is.na(edutype), 1, fieldecon),
    fieldcomm    = ifelse(educ5 == 1 & !is.na(edutype), 1, fieldcomm),
    fieldtech    = ifelse(educ5 == 1 & !is.na(edutype), 1, fieldtech)
  )

ess_all <- 
  ess_all %>%
  mutate(
    fieldratio =
      (fieldculture + fieldcomm) /
      (fieldculture + fieldecon + fieldcomm + fieldtech)
  )

ess_all <- 
  ess_all %>%
  mutate(
    n_fieldratio = (fieldratio - min(fieldratio, na.rm = TRUE)) /
      (max(fieldratio, na.rm = TRUE) - min(fieldratio, na.rm = TRUE))
  )

write.csv(ess_all, "final_1_1.csv")
##########
#Party coding
#Delete empty columns
prtvt_cols <- grep("^prtvt", names(ess_all), value = TRUE)
empty_prtvt <- prtvt_cols[sapply(ess_all[prtvt_cols], function(x) all(is.na(x)))]
ess_all <- ess_all %>% select(-all_of(empty_prtvt))
###
#reading in ches data 
ches_data <- as.data.frame(read.csv("1999-2024_CHES_dataset_means.csv"))
###merging party id 
ess_all_mapped <- ess_all %>%
  mutate(
    ches_party_id = case_when(
      # --- CZECH REPUBLIC (CZ) ---
      prtvtcz == 1 ~ 2101, prtvtcz == 2 ~ 2102, prtvtcz == 3 ~ 2103, prtvtcz == 4 ~ 2104, 
      prtvtcz == 5 ~ 2105, prtvtcz == 6 ~ 2108,
      prtvtacz == 1 ~ 2101, prtvtacz == 2 ~ 2102, prtvtacz == 3 ~ 2103, prtvtacz == 4 ~ 2104, 
      prtvtacz == 5 ~ 2107, prtvtacz == 6 ~ 2106, prtvtacz == 7 ~ 2105,
      # --- ESTONIA (EE) ---
      prtvtee == 1 ~ 2203, prtvtee == 2 ~ 2202, prtvtee == 3 ~ 2201, prtvtee == 4 ~ 2206, 
      prtvtee == 6 ~ 2204, prtvtee == 8 ~ 2207,
      prtvtaee == 1 ~ 2203, prtvtaee == 2 ~ 2202, prtvtaee == 3 ~ 2201, prtvtaee == 4 ~ 2204, 
      prtvtaee == 5 ~ 2206, prtvtaee == 6 ~ 2207,
      # --- HUNGARY (HU) ---
      prtvthu == 1 ~ 2302, prtvthu == 2 ~ 2301, prtvthu == 3 ~ 2304, prtvthu == 4 ~ 2303, 
      prtvthu == 5 ~ 2306,
      prtvtahu == 1 ~ 2301, prtvtahu == 2 ~ 2302, prtvtahu == 3 ~ 2304, prtvtahu == 4 ~ 2303,
      prtvtbhu == 1 ~ 2302, prtvtbhu == 2 ~ 2301, prtvtbhu == 3 ~ 2304, prtvtbhu == 4 ~ 2303,
      # --- POLAND (PL) ---
      prtvtpl == 1 ~ 2605, prtvtpl == 2 ~ 2603, prtvtpl == 3 ~ 2607, prtvtpl == 4 ~ 2604, 
      prtvtpl == 5 ~ 2606, prtvtpl == 6 ~ 2601, prtvtpl == 7 ~ 2609,
      prtvtapl == 1 ~ 2603, prtvtapl == 2 ~ 2605, prtvtapl == 3 ~ 2601, prtvtapl == 4 ~ 2606, 
      prtvtapl == 5 ~ 2604,
      prtvtbpl == 1 ~ 2603, prtvtbpl == 2 ~ 2605, prtvtbpl == 3 ~ 2606, prtvtbpl == 4 ~ 2601, 
      prtvtbpl == 5 ~ 2604,
      # --- SLOVENIA (SI) ---
      prtvtasi == 1 ~ 2902, prtvtasi == 2 ~ 2901, prtvtasi == 3 ~ 2903, prtvtasi == 4 ~ 2905, 
      prtvtasi == 5 ~ 2906, prtvtasi == 6 ~ 2904, prtvtasi == 7 ~ 2908,
      prtvtbsi == 1 ~ 2902, prtvtbsi == 2 ~ 2901, prtvtbsi == 3 ~ 2903, prtvtbsi == 4 ~ 2904, 
      prtvtbsi == 5 ~ 2906, prtvtbsi == 6 ~ 2907, prtvtbsi == 7 ~ 2905, prtvtbsi == 8 ~ 2910,
      prtvtcsi == 1 ~ 2903, prtvtcsi == 2 ~ 2902, prtvtcsi == 3 ~ 2910, prtvtcsi == 4 ~ 2906, 
      prtvtcsi == 5 ~ 2901, prtvtcsi == 6 ~ 2904, prtvtcsi == 7 ~ 2907, prtvtcsi == 8 ~ 2905,
      # --- SLOVAKIA (SK) ---
      prtvtsk == 1 ~ 2803, prtvtsk == 2 ~ 2802, prtvtsk == 3 ~ 2801, prtvtsk == 4 ~ 2804, 
      prtvtsk == 5 ~ 2805, prtvtsk == 6 ~ 2806, prtvtsk == 7 ~ 2809,
      prtvtask == 1 ~ 2803, prtvtask == 2 ~ 2802, prtvtask == 3 ~ 2804, prtvtask == 4 ~ 2805, 
      prtvtask == 5 ~ 2809, prtvtask == 6 ~ 2801,
      # --- BULGARIA (BG) ---
      prtvtbg == 1 ~ 2003, prtvtbg == 2 ~ 2001, prtvtbg == 3 ~ 2004, prtvtbg == 4 ~ 2002, 
      prtvtbg == 6 ~ 2007, prtvtbg == 7 ~ 2012,
      prtvtabg == 1 ~ 2001, prtvtabg == 2 ~ 2003, prtvtabg == 3 ~ 2004, prtvtabg == 4 ~ 2007, 
      prtvtabg == 5 ~ 2008, prtvtabg == 8 ~ 2003,
      # --- LATVIA (LV) (W4) ---
      prtvtlv == 1 ~ 2403, prtvtlv == 2 ~ 2402, prtvtlv == 3 ~ 2404, prtvtlv == 4 ~ 2401, 
      prtvtlv == 5 ~ 2405, prtvtlv == 6 ~ 2406,
      # --- ROMANIA (RO) (W4) ---
      prtvtro == 1 ~ 2701, prtvtro == 2 ~ 2704, prtvtro == 3 ~ 2705, prtvtro == 4 ~ 2706, 
      prtvtro == 5 ~ 2703, prtvtro == 6 ~ 2702,
      prtvtaro == 1 ~ 2701, prtvtaro == 2 ~ 2705, prtvtaro == 3 ~ 2704, prtvtaro == 4 ~ 2706, 
      prtvtaro == 5 ~ 2702,
      # --- BELGIUM (BE) (W4) ---
      prtvtbee == 1 ~ 109, prtvtbee == 2 ~ 107, prtvtbee == 3 ~ 103, prtvtbee == 4 ~ 112, 
      prtvtbee == 5 ~ 105, prtvtbee == 6 ~ 110, prtvtbee == 7 ~ 102, prtvtbee == 8 ~ 106, 
      prtvtbee == 9 ~ 108, prtvtbee == 10 ~ 104,
      # --- CROATIA (HR) (W4) ---
      prtvthr == 1 ~ 3101, prtvthr == 2 ~ 3102, prtvthr == 3 ~ 3105, prtvthr == 4 ~ 3103, 
      prtvthr == 5 ~ 3109, prtvthr == 6 ~ 3104, prtvthr == 8 ~ 3106, prtvthr == 10 ~ 3108,
      
      # --- ALL others to NA ---
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    party_voted_family = case_when(
      # Social Democratic / Socialist
      ches_party_id %in% c(102, 103, 2101, 2204, 2301, 2601, 2701, 2803, 2903, 3102, 3701, 3804, 4005, 2003, 2008) ~ "Social Democratic",
      # Conservative / Christian Democratic
      ches_party_id %in% c(108, 109, 2102, 2104, 2201, 2302, 2303, 2401, 2403, 2406, 2605, 2702, 2704, 2802, 2805, 2902, 3101, 3119, 3702, 3801, 4001) ~ "Conservative/CD",
      # Liberal / Centrist
      ches_party_id %in% c(106, 107, 2001, 2105, 2106, 2203, 2304, 2404, 2705, 2806, 2901, 3104, 3105, 3118, 3121, 3803, 4002, 4004) ~ "Liberal/Centrist",
      # Green / Ecologist
      ches_party_id %in% c(104, 105, 2107, 2207, 2405, 3114, 3120, 3802, 4006) ~ "Green",
      # Far Right / Nationalist
      ches_party_id %in% c(112, 2007, 2703, 2809, 2907, 3109, 3122, 3805, 4009) ~ "Far Right/Nationalist",
      # Agrarian / Regionalist / Ethnic Minority
      ches_party_id %in% c(110, 2004, 2402, 2706, 2804, 3103, 3106) ~ "Agrarian/Regional",
      # Communist / Hard Left
      ches_party_id %in% c(2103, 3806, 3808, 4003) ~ "Communist/Left",
      # Other / Populist / Single-Issue
      ches_party_id %in% c(2012, 2306, 2801, 2905, 2906, 2908, 2910, 3108, 3115, 3116, 3117, 3807, 4007, 4008) ~ "Other/Populist",
      
      TRUE ~ NA_character_
    )
  )

ess_all_merged <- ess_all_mapped %>%
  left_join(
    ches_data %>% 
      mutate(party = as.double(party)) %>%
      mutate(country = as.character(country)),
    by = c("ches_party_id" = "party",
           "cntry"         = "country",
           "essround"      = "year")      
  ) %>%
  rename(
    party_voted_galtan = galtan, 
    LRGEN = lrgen               
  )

ess_all_recoded <- ess_all_merged %>%
  mutate(
    votedtan = case_when(
      LRGEN >= 7.5 ~ 1,
      party_voted_galtan >= 7.5 ~ 1,
      party_voted_family %in% c("Far Right/Nationalist") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    votedgal = case_when(
      LRGEN <= 2.5 ~ 1,
      party_voted_galtan <= 2.5 ~ 1,
      party_voted_family %in% c("Social Democratic", "Liberal/Centrist", "Green", "Communist/Left") ~ 1,
      TRUE ~ 0
    )
  )
###save csv
write.csv(ess_all_recoded, "final_ready_for_analysis2.csv")

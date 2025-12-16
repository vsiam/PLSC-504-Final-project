#504 Final 
#12/13/2025
#Running the analysis 
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
library(fixest)
library(forcats)
library(lme4)
library(lmtest)
library(stringr)
library(rdrobust)
library(rddensity)
library(stargazer)
library(texreg)
###
ess <- #READ THE DATA 
  as.data.frame(read_csv("final_ready_for_analysis2.csv"))
###
#Some recoding
###
ess <- 
  ess %>%
  mutate(
    education = case_match(
      educ5,
      1:5 ~ educ5,
      c(55, 77, 88,  99) ~ NA_real_,
      .default = educ5
    )
  )
ess <- #Create female var
  ess %>% 
  mutate(
    female = case_when(
      gndr == 1 ~ 0, 
      gndr == 2 ~ 1, 
      TRUE ~ NA_real_
    )
  )

ess <- ##Rural
  ess %>%
  mutate(
    rural = case_match(
      domicil,
      1:5 ~ domicil,
      c(7, 8, 9) ~ NA_real_,
      .default = domicil
    )
  )

ess <- ##Secularity
  ess %>%
  mutate(
    secularity = case_match(
      rlgatnd,
      1:7 ~ rlgatnd,
      c(77, 88, 99) ~ NA_real_, 
      .default = rlgatnd
    )
  )

ess <-
  ess %>% ##Birth year 
  mutate(
    birth_year = case_match(
      yrbrn,
      1907:2000 ~ yrbrn, 
      c(7777, 8888, 9999) ~ NA_real_,
      .default = NA_real_ 
    )
  )

ess <- #income feeling
  ess %>% 
  mutate(
    income  = case_match(
      hincfel, 
      1:5 ~ hincfel,  
      c(7, 8,  9) ~ NA_real_,
      .default = hincfel
    )
  )

ess <- #Democracy satisfaction
  ess %>% 
  mutate(
    dem_satisfied = case_match(
      stfdem, 
      1:10 ~ stfdem, 
      c(77,88,99) ~ NA_real_,
      .default = stfdem
    )
  )

ess <- #Authoritarian tendency
  ess %>%
  mutate(auth_tend = case_match(
    prtyban, 
    1:5 ~ prtyban, 
    c(7,8,9) ~ NA_real_, 
    .default = prtyban
  ))
#RDD vars 
cutoff_year <- 1973
ess <- 
  ess %>%
  mutate(
    treated = if_else(
      birth_year >= cutoff_year,
      1,
      0,
      missing = NULL
    ),
    birth_year_c = birth_year - cutoff_year,
    birth_year_c_treat = birth_year_c * treated
  )
#####
#####
#drop HR  and SI
ess <- 
  ess %>%
    filter(cntry != "HR"  & cntry !="SI")

ess_main <- #FILTER CECT >= 0.75 UNIVERSITY GRADS ONLY FOR MAIN ANALYSIS
  ess %>% 
  filter(n_fieldratio >= 0.75 &  education >=4)
#
ess_rob <- #FILTER CECT >= 0.60 FOR ADDITIONAL CHECK
  ess %>% 
  filter(n_fieldratio >=0.60  &  education >=4)


##########
##########
#Descriptive stat
covariates <- c( "female", "rural", "secularity",
                "dem_satisfied", "auth_tend",  "income", "n_fieldratio")
###
ess_clean <- ess_main %>%
  filter(!is.na(birth_year))

ess_clean <- ess_clean %>%
  filter(across(all_of(covariates), ~ !is.na(.)))

mccrary_result_rdrobust <- #Density test
  rddensity(
  X = ess_clean$birth_year, 
  c = cutoff_year              
)
summary(mccrary_result_rdrobust)
###
#balance tests
year_below <- cutoff_year - 1
year_above <- cutoff_year
ess_discrete <-
  ess_clean %>%
  filter(birth_year %in% c(year_below, year_above))
ess_discrete <- 
  ess_discrete %>%
  mutate(
    treated_dummy = if_else(birth_year >= cutoff_year, 1, 0)
  )
balance_results_discrete <- list()

for (cov in covariates) {
  current_formula <- as.formula(paste(cov, "~ treated_dummy"))
  model <- lm(current_formula, data = ess_discrete)
  result_tidy <- tidy(model) %>%
    filter(term == "treated_dummy")
  balance_results_discrete[[cov]] <- c(
    Estimate = result_tidy$estimate,
    `P-value` = result_tidy$p.value
  )
}
balance_df_discrete <- do.call(rbind, balance_results_discrete)
balance_df_discrete <- as.data.frame(balance_df_discrete)
colnames(balance_df_discrete) <- c("Difference-in-Means Estimate", "P-value")
print(balance_df_discrete)
#####
#visualization 
bin_width <- 1
ess_binned <- ess_clean %>%
  mutate(bin = floor(birth_year_c / bin_width) * bin_width) %>%
  group_by(bin) %>%
  summarise(
    mean_votedtan = mean(votedtan, na.rm = TRUE),
    mean_votedgal = mean(votedgal, na.rm = TRUE),
    n = n()
  )
ggplot(ess_binned, aes(x = bin, y = mean_votedtan)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Non-Parametric Global Trend: TAN Vote",
    x = "Centered Birth Year (Bin Midpoint)",
    y = "Mean Voted TAN",
    size = "N in Bin"
  ) +
  theme_minimal()
# 
ggplot(ess_binned, aes(x = bin, y = mean_votedgal)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Non-Parametric Global Trend: GAL Vote",
    x = "Centered Birth Year (Bin Midpoint)",
    y = "Mean Voted GAL",
    size = "N in Bin"
  ) +
  theme_minimal()
#####
#MAIN MODELS 
#Specify covariate-fixed effects matrix
ess_clean$cntry <- as.factor(ess_clean$cntry)
ess_clean$essround <- as.factor(ess_clean$essround)
all_fe_vars <- c("female", "rural", "secularity", 
                 "dem_satisfied", "auth_tend", "income", 
                 "cntry", "essround")
X_fe_temp <- ess_clean[, all_fe_vars]
X_fe_matrix <- model.matrix(~ . - 1, data = X_fe_temp)
#model 1 - tan
model1_tan_fe <- rdrobust::rdrobust(
  y = ess_clean$votedtan,               
  x = ess_clean$birth_year_c,           
  c = 0,                                
  p = 1,
  covs = X_fe_matrix,                 
  kernel = "triangular"                 
)
summary(model1_tan_fe)
#model 1 - gal 
model1_gal_fe <- rdrobust::rdrobust(
  y = ess_clean$votedgal,               
  x = ess_clean$birth_year_c,           
  c = 0,                                
  p = 1,                               
  covs = X_fe_matrix,              
  kernel = "triangular"                 
)
summary(model1_gal_fe)
#####
#####
#Robustness check params  
p_tests <- c(1, 2, 3)
c_tests <- c(0, -5, 5)

#Robustness check - local shape 
rdd_p_tests <- list() 
for (p_val in p_tests) {
  model_result <- rdrobust::rdrobust(
    y = ess_clean$votedtan, x = ess_clean$birth_year_c, c = 0, p = p_val,
    covs = X_fe_matrix, kernel = "triangular"
  )
  model_name <- paste0("tan_p", p_val)
  rdd_p_tests[[model_name]] <- model_result
}

for (p_val in p_tests) {
  model_result <- rdrobust::rdrobust(
    y = ess_clean$votedgal, x = ess_clean$birth_year_c, c = 0, p = p_val,
    covs = X_fe_matrix, kernel = "triangular"
  )
  model_name <- paste0("gal_p", p_val)
  rdd_p_tests[[model_name]] <- model_result
}
#Robustness check - placebo cut-offs
rdd_c_test = list()
for (c_val in c_tests) {
  model <- rdrobust(
    y = ess_clean$votedtan,
    x = ess_clean$birth_year_c,
    c = c_val,
    covs = X_fe_matrix,
    kernel = "triangular"
  )
  model_name <- paste0("TAN c=", c_val)
  rdd_c_test[[model_name]] <- model
}

for (c_val in c_tests) {
  model <- rdrobust(
    y = ess_clean$votedgal,
    x = ess_clean$birth_year_c,
    c = c_val,
    covs = X_fe_matrix,
    kernel = "triangular"
  )
  model_name <- paste0("GAL c=", c_val)
  rdd_c_test[[model_name]] <- model
}
#################
#################
###########
########### 
#Interaction effects
covariates <- c( "female", "rural", "secularity",
                 "dem_satisfied", "auth_tend",  "income", "n_fieldratio")
ess_int <-  ess %>%
  filter(!is.na(birth_year))  %>%
  filter(across(all_of(covariates), ~ !is.na(.))) %>%
  filter(education >=4)
#####
ess_int$cntry <- as.factor(ess_int$cntry)
ess_int$essround <- as.factor(ess_int$essround)
all_fe_vars <- c("female", "rural", "secularity", 
                 "dem_satisfied", "auth_tend", "income", 
                 "cntry", "essround",  "n_fieldratio")
X_fe_temp <- ess_int[, all_fe_vars]
X_fe_matrix <- model.matrix(~ . - 1, data = X_fe_temp)
####
#Estimate the base model
#TAN
rdd_tan_h <- rdrobust::rdrobust(
  y = ess_int$votedtan, 
  x = ess_int$birth_year_c, 
  c = 0, 
  p = 1, 
  covs = X_fe_matrix, 
  kernel = "triangular"
)
h_optimal <- rdd_tan_h$bws[1, 1] 
ess_local <- subset(ess_int, abs(ess_int$birth_year_c) <= h_optimal) 
local_indices <- which(abs(ess_int$birth_year_c) <= h_optimal) 
ess_local$D <- as.numeric(ess_local$birth_year_c >= 0)
ess_local$weight_triangular <- 1 - abs(ess_local$birth_year_c / h_optimal)
X_fe_matrix_local <- X_fe_matrix[local_indices, ]
n_fieldratio_cols <- grep("n_fieldratio", colnames(X_fe_matrix_local))
X_fe_matrix_clean <- X_fe_matrix_local[, -n_fieldratio_cols]
model2_tan_fe_int <- lm(
  votedtan ~ D * n_fieldratio + birth_year_c + D:birth_year_c + X_fe_matrix_clean, 
  data = ess_local, 
  weights = ess_local$weight_triangular
)
summary(model2_tan_fe_int)
##GAL 
rdd_gal_h <- rdrobust::rdrobust(
  y = ess_int$votedgal,
  x = ess_int$birth_year_c,
  c = 0,
  p = 1,
  covs = X_fe_matrix, 
  kernel = "triangular"
)
h_optimal_gal <- rdd_gal_h$bws[1, 1]
ess_local_gal <- subset(ess_int, abs(birth_year_c) <= h_optimal_gal)
local_indices_gal <- which(abs(ess_int$birth_year_c) <= h_optimal_gal)
ess_local_gal$D <- as.numeric(ess_local_gal$birth_year_c >= 0)
ess_local_gal$weight_triangular <- 1 - abs(ess_local_gal$birth_year_c / h_optimal_gal)
X_fe_matrix_local_gal <- X_fe_matrix[local_indices_gal, ]
n_fieldratio_cols_gal <- grep("n_fieldratio", colnames(X_fe_matrix_local_gal))
X_fe_matrix_clean_gal <- X_fe_matrix_local_gal[, -n_fieldratio_cols_gal]
model2_gal_fe_int <- lm(
  votedgal ~ D * n_fieldratio + birth_year_c + D:birth_year_c + X_fe_matrix_clean_gal, 
  data = ess_local_gal, 
  weights = ess_local_gal$weight_triangular
)
summary(model2_gal_fe_int)
h_tan_rounded <- round(h_optimal, 3) 
h_gal_rounded <- round(h_optimal_gal, 3) 



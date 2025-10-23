##Replication of main models using ESS data from Hooghe et al. (2024)

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
ess <- as.data.frame(read_dta("ESSmain.dta"))

##histograms 
educ_data <- ess %>% filter(educ5!=1)

p1 <- ggplot(ess, aes(x=n_fieldratio)) + 
  geom_histogram(aes(y=..count../sum(..count..)*100),
                 binwidth = 0.05, fill = "gray40", alpha=0.8, color="black") +
  stat_function(fun=function(x) {
    dnorm(x, mean = mean(educ_data$n_fieldratio, na.rm=T),
          sd = sd(educ_data$n_fieldratio, na.rm=T)) *100 *0.05
  }, color = "black", linetype="dashed") + 
  scale_x_continuous(name = "Educational CECT", breaks = seq(0,1,0.1)) + 
  scale_y_continuous(name = "Percent", breaks = seq(0,20,5)) +
  ggtitle("Distribution of educational CECT") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill="white"), 
        plot.background = element_rect(fill="white")
  )
p2 <- ggplot(ess, aes(x=n_occfield)) + 
  geom_histogram(aes(y=..count../sum(..count..)*100),
                 binwidth = 0.05, fill = "gray40", alpha=0.8, color="black") +
  stat_function(fun=function(x) {
    dnorm(x, mean = mean(ess$n_occfield, na.rm=T),
          sd = sd(ess$n_occfield, na.rm=T)) *100 *0.05
  }, color = "black", linetype="dashed") + 
  scale_x_continuous(name = "Educational CECT", breaks = seq(0,1,0.1)) + 
  scale_y_continuous(name = "Percent", breaks = seq(0,12,2)) +
  ggtitle("Distribution of occupational CECT") + 
  theme_minimal() +
  theme(panel.background = element_rect(fill="white"), 
        plot.background = element_rect(fill="white")
  )
p1
p2
### baseline models 
#GAL model 
ess_sub <- ess %>%
  filter(selectgal == 1) %>%
  mutate(
    n_occfield = ifelse(selectgal == 1, n_occfieldgal, NA),
    n_fieldincome = ifelse(selectgal == 1, n_fieldincomegal, NA)
  ) %>%
  
  drop_na(
    GAL,
    n_fieldratio, n_occfield, n_fieldincome,
    educ5, female, incomeobj, rural, age, secular,
    essround, cntry, isco3tr
  ) %>%
  droplevels() %>%
  mutate(
    educ5_f = factor(educ5, levels = c(1,2,3,4,5))
  )

model_CECT1 <- glmer(
  GAL ~ n_fieldratio + n_occfield + n_fieldincome +
    educ5 + female + incomeobj + rural + age + secular +
    essround + (1 | cntry) + (1 | isco3tr),
  data = ess_sub,
  family = binomial(link = "logit"),
  nAGQ = 1 
)
#tan model
ess_sub_tan <- ess %>%
  filter(selecttan == 1) %>%
  # Recreate derived variables
  mutate(
    n_occfield = ifelse(selecttan == 1, n_occfieldtan, NA),
    n_fieldincome = ifelse(selecttan == 1, n_fieldincometan, NA)
  ) %>%
  drop_na(
    TAN,
    n_fieldratio, n_occfield, n_fieldincome,
    educ5, female, incomeobj, rural, age, secular,
    essround, cntry, isco3tr
  ) %>%
  droplevels() %>%
  mutate(
    educ5_f = factor(educ5, levels = c(1,2,3,4,5))
  )
model_CECT2 <- glmer(
  TAN ~ n_fieldratio + n_occfield + n_fieldincome +
    educ5 + female + incomeobj + rural + age + secular +
    essround + (1 | cntry) + (1 | isco3tr),
  data = ess_sub_tan,
  family = binomial(link = "logit"),
  nAGQ = 1  
)
modelsummary(list(
  "GAL parties" = model_CECT1,
  "TAN parties" = model_CECT2),
             coef_map = c(
               "n_fieldratio" = "Educational CECT",
               "n_occfield" = "Occupational CECT"),
               stars=T, 
               gof_map = c("nobs")
             )
####plot 
df1 <- broom.mixed::tidy(model_CECT1, effects = "fixed", conf.int = TRUE) %>%
  filter(grepl("n_fieldratio|n_occfield", term)) %>%
  mutate(model = "GAL parties")

df2 <- broom.mixed::tidy(model_CECT2, effects = "fixed", conf.int = TRUE) %>%
  filter(grepl("n_fieldratio|n_occfield", term)) %>%
  mutate(model = "TAN parties")

coef_df <- bind_rows(df1, df2) %>%
  mutate(term = recode(term,
                       "n_fieldratio" = "Educational CECT",
                       "n_occfield" = "Occupational CECT"))

ggplot(coef_df, aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  labs(x = "Coefficient Estimate",
       y = "",
       color = "Model") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

####
####
#models with specified occupation 
###GAL model
ess_sub_gal <- ess %>%
  filter(selectgal == 1) %>%
  mutate(
    n_occfield = ifelse(selectgal == 1, n_occfieldgal, NA),
    n_fieldincome = ifelse(selectgal == 1, n_fieldincomegal, NA)
  ) %>%
  drop_na(
    GAL,
    n_fieldratio, n_occfield, n_fieldincome,
    class8, female, incomeobj, rural, age, secular, essround,
    cntry, isco3tr
  ) %>%
  droplevels() %>%
  mutate(
    class8_f = relevel(factor(class8), ref = "4")
  )

model_oesch1 <- glmer(
  GAL ~ n_fieldratio + n_occfield + n_fieldincome +
    class8_f + female + incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_gal,
  family = binomial(link = "logit"),
  nAGQ = 1
)
##TAN model 
ess_sub_tan <- ess %>%
  filter(selecttan == 1) %>%
  mutate(
    n_occfield = ifelse(selecttan == 1, n_occfieldtan, NA),
    n_fieldincome = ifelse(selecttan == 1, n_fieldincometan, NA)
  ) %>%
  drop_na(
    TAN,
    n_fieldratio, n_occfield, n_fieldincome,
    class8, female, incomeobj, rural, age, secular, essround,
    cntry, isco3tr
  ) %>%
  droplevels() %>%
  mutate(
    class8_f = relevel(factor(class8), ref = "4")
  )
model_oesch2 <- glmer(
  TAN ~ n_fieldratio + n_occfield + n_fieldincome +
    class8_f + female + incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_tan,
  family = binomial(link = "logit"),
  nAGQ = 1
)
modelsummary(
  list(
    "GAL parties" = model_oesch1,
    "TAN parties" = model_oesch2
  ),
  coef_map = c(
    "n_fieldratio" = "Educational CECT",
    "n_occfield" = "Occupational CECT",
    "class8_f1" = "Self-employed profs",
    "class8_f2" = "Small business owners",
    "class8_f3" = "Tech profs",
    "class8_f5" = "Managers",
    "class8_f6" = "Clerks",
    "class8_f7" = "Socio-cultural profs", 
    "class8_f8" = "Service workers"
  ),
  stars = TRUE,            
  gof_map = c("nobs"),
  output = "occmodel.html"
)
##coef plot 
tidy_coefs <- function(model, model_name) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    filter(term %in% c("n_fieldratio", "n_occfield") |
             grepl("^class8_f", term)) %>%
    mutate(
      model = model_name,
      term = recode(term,
                    "n_fieldratio" = "Educational CECT",
                    "n_occfield" = "Occupational CECT",
                    "class8_f1" = "Self-employed profs",
                    "class8_f2" = "Small business owners",
                    "class8_f3" = "Tech profs",
                    "class8_f4" = "Production workers",  
                    "class8_f5" = "Managers",
                    "class8_f6" = "Clerks",
                    "class8_f7" = "Socio-cultural profs",
                    "class8_f8" = "Service workers")
    )
}
coefs_gal <- tidy_coefs(model_oesch1, "GAL parties")
coefs_tan <- tidy_coefs(model_oesch2, "TAN parties")
coef_df <- bind_rows(coefs_gal, coefs_tan)
ref_rows <- data.frame(
  term = "Production workers (ref)",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  model = rep(c("GAL parties", "TAN parties"), each = 1)
)
coef_df <- bind_rows(coef_df, ref_rows)
term_levels <- c("Educational CECT", "Occupational CECT",
                 "Self-employed profs", "Small business owners",
                 "Tech profs", "Managers", "Clerks",
                 "Socio-cultural profs", "Service workers",
                 "Production workers (ref)")

coef_df$term <- factor(coef_df$term, levels = term_levels)

ggplot(coef_df, aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                position = position_dodge(width = 0.5), width = 0.2) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  labs(x = "Coefficient Estimate",
       y = "",
       color = "Model") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())
####
####models with high/no high education 
#GAL
model_gal <- glm(
  GAL ~ higher * n_fieldratio + n_occfield + n_fieldincome +
    female + incomeobj + rural + age + secular + factor(essround) + factor(country),
  data = ess_sub_gal,
  family = binomial(link = "logit")
)

# Robust SEs
robust_gal <- coeftest(model_gal, vcov = sandwich)

#TAN
model_tan <- glm(
  TAN ~ higher * n_fieldratio + n_occfield + n_fieldincome +
    female + incomesubj + rural + age + secular + factor(essround) + factor(country),
  data = ess_sub_tan,
  family = binomial(link = "logit")
)

# Robust SEs
robust_tan <- coeftest(model_tan, vcov = sandwich)
##plot 
library(marginaleffects)
#GAL
marg_gal <- predictions(
  model_gal,
  newdata = expand.grid(
    n_fieldratio = seq(0, 1, by = 0.1),
    higher = c(0, 1),
    n_occfield = mean(ess_sub_gal$n_occfield, na.rm = TRUE),
    n_fieldincome = mean(ess_sub_gal$n_fieldincome, na.rm = TRUE),
    female = mean(ess_sub_gal$female, na.rm = TRUE),
    incomeobj = mean(ess_sub_gal$incomeobj, na.rm = TRUE),
    rural = mean(ess_sub_gal$rural, na.rm = TRUE),
    age = mean(ess_sub_gal$age, na.rm = TRUE),
    secular = mean(ess_sub_gal$secular, na.rm = TRUE),
    essround = factor(ess_sub_gal$essround[1]),
    country = factor(ess_sub_gal$country[1])
  ),
  type = "response"
)
#TAN
marg_tan <- predictions(
  model_tan,
  newdata = expand.grid(
    n_fieldratio = seq(0, 1, by = 0.1),
    higher = c(0, 1),
    n_occfield = mean(ess_sub_tan$n_occfield, na.rm = TRUE),
    n_fieldincome = mean(ess_sub_tan$n_fieldincome, na.rm = TRUE),
    female = mean(ess_sub_tan$female, na.rm = TRUE),
    incomesubj = mean(ess_sub_tan$incomesubj, na.rm = TRUE),
    rural = mean(ess_sub_tan$rural, na.rm = TRUE),
    age = mean(ess_sub_tan$age, na.rm = TRUE),
    secular = mean(ess_sub_tan$secular, na.rm = TRUE),
    essround = factor(ess_sub_tan$essround[1]),
    country = factor(ess_sub_tan$country[1])
  ),
  type = "response"
)
plot_marg_effects <- function(pred_data, ytitle = "Propensity to vote", xtitle = "Educational CECT") {
  df <- as.data.frame(pred_data)
  
  ggplot(df, aes(x = n_fieldratio, y = estimate, color = factor(higher))) +  # <-- use 'estimate'
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(higher)),
                alpha = 0.2, color = NA) +
    labs(x = xtitle, y = ytitle, color = "Higher education", fill = "Higher education") +
    theme_minimal(base_size = 14)
}
plot_gal <- plot_marg_effects(marg_gal, ytitle = "Propensity to vote GAL")
plot_tan <- plot_marg_effects(marg_tan, ytitle = "Propensity to vote TAN")
combined_plot <- plot_gal + plot_tan
combined_plot
####
####gender models 
ess_sub_gal <- ess %>%
  filter(selectgal == 1) %>%
  mutate(
    n_occfield = n_occfieldgal,
    n_fieldincome = n_fieldincomegal,
    educ5_f = factor(educ5, levels = c(1,2,3,4,5))  
  ) %>%
  drop_na(GAL, female, n_fieldratio, n_occfield, n_fieldincome, educ5_f,
          incomeobj, rural, age, secular, essround, cntry, isco3tr)
ess_sub_tan <- ess %>%
  filter(selecttan == 1) %>%
  mutate(
    n_occfield = n_occfieldtan,
    n_fieldincome = n_fieldincometan,
    educ5_f = factor(educ5, levels = c(1,2,3,4,5))
  ) %>%
  drop_na(TAN, female, n_fieldratio, n_occfield, n_fieldincome, educ5_f,
          incomeobj, rural, age, secular, essround, cntry, isco3tr)
#model 1: no female
gender1 <- glmer(
  GAL ~ n_fieldratio + n_occfield + n_fieldincome + educ5_f +
    incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_gal,
  family = binomial(link = "logit")
)

# Model 2: add female
gender2 <- glmer(
  GAL ~ n_fieldratio + n_occfield + n_fieldincome + female + educ5_f +
    incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_gal,
  family = binomial(link = "logit")
)

# Model 3: only female and n_fieldincome
gender3 <- glmer(
  GAL ~ female + educ5_f + n_fieldincome + incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_gal %>% filter(!is.na(n_fieldratio)),
  family = binomial(link = "logit")
)


# Model 4: no female
gender4 <- glmer(
  TAN ~ n_fieldratio + n_occfield + n_fieldincome + educ5_f +
    incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_tan,
  family = binomial(link = "logit")
)

# Model 5: add female
gender5 <- glmer(
  TAN ~ n_fieldratio + n_occfield + n_fieldincome + female + educ5_f +
    incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_tan,
  family = binomial(link = "logit")
)

# Model 6: only female and n_fieldincome
gender6 <- glmer(
  TAN ~ female + educ5_f + n_fieldincome + incomeobj + rural + age + secular + essround +
    (1 | cntry) + (1 | isco3tr),
  data = ess_sub_tan %>% filter(!is.na(n_fieldratio)),
  family = binomial(link = "logit")
)
modelsummary(
  list("CECT only (GAL)" = gender1,
       "CECT + gender (GAL)" = gender2,
       "Gender only (GAL)" = gender3,
       "CECT only (TAN)" = gender4,
       "CECT + gender (TAN)" = gender5,
       "Gender only (TAN)" = gender6
  ),
  coef_map = c(
    "female" = "Female",
    "n_fieldratio" = "Educational CECT",
    "n_occfield" = "Occupational CECT"
  ),
  gof_map = c("nobs"),
  stars = TRUE)
###
##plot 
cect_models <- bind_rows(
  broom.mixed::tidy(gender1, effects = "fixed") %>% mutate(Model = "CECT only (GAL)"),
  broom.mixed::tidy(gender2, effects = "fixed") %>% mutate(Model = "CECT + gender (GAL)"),
  broom.mixed::tidy(gender4, effects = "fixed") %>% mutate(Model = "CECT only (TAN)"),
  broom.mixed::tidy(gender5, effects = "fixed") %>% mutate(Model = "CECT + gender (TAN)")
) %>%
  filter(term %in% c("n_fieldratio", "n_occfield"))
female_models <- bind_rows(
  broom.mixed::tidy(gender3, effects = "fixed") %>% 
    filter(term == "female") %>% mutate(Model = "Gender only (GAL)"),
  broom.mixed::tidy(gender6, effects = "fixed") %>% 
    filter(term == "female") %>% mutate(Model = "Gender only (TAN)")
)
plot_df <- bind_rows(cect_models, female_models) %>%
  mutate(
    Variable = case_when(
      term == "n_fieldratio" ~ "Educational CECT",
      term == "n_occfield" ~ "Occupational CECT",
      term == "female" ~ "Female"
    )
  )
ggplot(plot_df, aes(x = Variable, y = estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = "", y = "Coefficient (log-odds)") +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

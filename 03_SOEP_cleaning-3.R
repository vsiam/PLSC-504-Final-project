############# SOEP coding
library(haven)
library(tidyverse)
SOEP <- read_dta("data/soeplong_prediction_wfield.dta")


### Select only variables that we need (also for reproduction purposes)


SOEP$plg0012 <- SOEP$plg0012_v1
SOEP$plg0012[is.na(SOEP$plg0012)] <- SOEP$plg0012_v2[is.na(SOEP$plg0012)]



SOEP_small <- SOEP %>% select(pid, respid, hid, cid, syear, pgfield, pgdegree, 
                              edufieldb, pgtraina, pgtrainb, pgtrainc, pgtraind,
                              pgisced97, fieldratio, fieldratio1, edutype, green,
                              greenext, TAN,pgnation, migback,
                              female, age, isco88, pglabnet, birthregion,
                              plh0012_h, plh0013_h, plg0072, plg0079_v1, plg0079_v3, plg0079_v4,
                              plg0012, plg0267, plg0293_h, plg0014_v1, plg0014_v2, plg0014_v3,
                              plg0014_v4, plg0014_v5, plg0014_v6, plg0014_v7,
                              plg0013_v1, plg0013_v3, pgisco88, pgisco08, pgemplst,
                              ) 



write_dta(SOEP_small, "data/SOEP_long_small.dta")

rm()
gc()

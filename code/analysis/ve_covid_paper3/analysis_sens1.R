# This code creates descriptive table for sensitivity analysis 
# on the Vaccine effectiveness against lung injury associated with COVID-19
# during delta and omicron variant surges

library(data.table)
library(lubridate)
library(stringr)
library(tidyverse)
library(tableone)
library(xtable)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper3/trc_dt_all.Rdata")

#Table 1
myVars <- c( "age",  
             "age_group", 
             "sex",  
             "vaccine_status",
             "voc",
             "source")

# Vector of categorical variables 
catVars <- c( "age_group", 
              "sex",  
              "vaccine_status",
              "voc",
              "source")

table_1 <- CreateTableOne(vars = myVars, factorVars = catVars, 
                               data = trc_dt_all, 
                               strata = "ct_simple",
                               test = FALSE, 
                               includeNA = TRUE,
                               addOverall=TRUE )

# print table 
# xtable(print(table_1,  showAllLevels = T))

mod_any <- glm(ct_simple ~ vaccine_status, 
               data = trc_dt_all,
               family = binomial(link = "logit"))

# summary(mod_any)
# exp(coef(mod_any)[-1])
test_any <- (1-exp(coef(mod_any)[-1]))*100

vcov_any <- sandwich(mod_any, type = "HC1")
# exp(coefci(mod_any, vcov = vcov))
# (1-exp(coef(mod_any, vcov = vcov)))*100
test_any_ci <- (1-exp(coefci(mod_any, vcov = vcov_any)))*100

# adj mod_anyel
mod_any2 <- glm(ct_simple ~ sex + age + source + vaccine_status, 
                data = trc_dt_all,
                family = binomial(link = "logit"))

# summary(mod_any2)
# exp(coef(mod_any2)[-1])
test_any2 <- (1-exp(coef(mod_any2)[-1]))*100

vcov_any2 <- sandwich(mod_any2, type = "HC1")
# exp(coefci(mod_any2, vcov = vcov2))
# (1-exp(coef(mod_any2, vcov = vcov2)))*100
test_any_ci2 <- (1-exp(coefci(mod_any2, vcov = vcov_any2)))*100

# crude mod_sev
mod_sev <- glm(ct_34 ~ vaccine_status, 
               data = trc_dt_all,
               family = binomial(link = "logit"))

# summary(mod_sev)
# exp(coef(mod_sev)[-1])
test_sev <- (1-exp(coef(mod_sev)[-1]))*100

vcov_sev <- sandwich(mod_sev, type = "HC1")
# exp(coefci(mod_sev, vcov = vcov))
# (1-exp(coef(mod_sev, vcov = vcov)))*100
test_sev_ci <- (1-exp(coefci(mod_sev, vcov = vcov_sev)))*100

# adj mod_sevel
mod_sev2 <- glm(ct_34 ~ sex + age + source + vaccine_status, 
                data = trc_dt_all,
                family = binomial(link = "logit"))

# summary(mod_sev2)
# exp(coef(mod_sev2)[-1])
test_sev2 <- (1-exp(coef(mod_sev2)[-1]))*100

vcov_sev2 <- sandwich(mod_sev2, type = "HC1")
# exp(coefci(mod_sev2, vcov = vcov_sev2))
# (1-exp(coef(mod_sev2, vcov = vcov_sev2)))*100
test_sev_ci2 <- (1-exp(coefci(mod_sev2, vcov = vcov_sev2)))*100
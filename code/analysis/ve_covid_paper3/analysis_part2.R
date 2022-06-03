# This code fits logit models of lung injury
# to the data and calculates OR and CI
# on the Vaccine effectiveness against lung injury associated with COVID-19
# during delta and omicron variant surges

library(data.table)
library(sandwich)
library(lmtest)

options(scipen = 999)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper3/trc_dt_all.Rdata")

trc_dt <- trc_dt_all %>% 
  filter(is.na(past_inf)) %>%
  filter(is.na(pcr_in_pr)) %>%
  filter(is.na(pcr_2)) %>%
  filter(pcr==1)

mod_any <- glm(ct_simple ~ vaccine_status, 
               data = trc_dt,
               family = binomial(link = "logit"))

# summary(mod_any)
# exp(coef(mod_any)[-1])
test_any <- (1-exp(coef(mod_any)[-1]))*100

vcov_any <- sandwich(mod_any, type = "HC1")
# exp(coefci(mod_any, vcov = vcov))
# (1-exp(coef(mod_any, vcov = vcov)))*100
test_any_ci <- (1-exp(coefci(mod_any, vcov = vcov_any)))*100

# adj mod_any 
mod_any2 <- glm(ct_simple ~ sex + age + source + vaccine_status, 
                data = trc_dt,
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
               data = trc_dt,
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
                data = trc_dt,
                family = binomial(link = "logit"))

# summary(mod_sev2)
# exp(coef(mod_sev2)[-1])
test_sev2 <- (1-exp(coef(mod_sev2)[-1]))*100

vcov_sev2 <- sandwich(mod_sev2, type = "HC1")
# exp(coefci(mod_sev2, vcov = vcov_sev2))
# (1-exp(coef(mod_sev2, vcov = vcov_sev2)))*100
test_sev_ci2 <- (1-exp(coefci(mod_sev2, vcov = vcov_sev2)))*100


# "delta" period October 1, 2021— January 9, 2022 
mod_per <- glm(ct_simple ~ sex + age + source + vaccine_status, 
               data = trc_dt[voc == "delta"],
               family = binomial(link = "logit"))

# summary(mod_per)
# exp(coef(mod_per)[-1])
test_per <- (1-exp(coef(mod_per)[-1]))*100

vcov_per <- sandwich(mod_per, type = "HC1")
# exp(coefci(mod_per, vcov = vcov))
# (1-exp(coef(mod_per, vcov = vcov)))*100
test_per_ci <- (1-exp(coefci(mod_per, vcov = vcov_per)))*100

# "omicron" period January 10, 2022 —- April 5, 2022
mod_per2 <- glm(ct_simple ~ sex + age + source + vaccine_status, 
                data = trc_dt[voc = "omicron"],
                family = binomial(link = "logit"))

# summary(mod_per2)
# exp(coef(mod_per2)[-1])
test_per2 <- (1-exp(coef(mod_per2)[-1]))*100

vcov_per2 <- sandwich(mod_per2, type = "HC1")
# exp(coefci(mod_per2, vcov = vcov2))
# (1-exp(coef(mod_per2, vcov = vcov2)))*100
test_per_ci2 <- (1-exp(coefci(mod_per2, vcov = vcov_per2)))*100

# age
vac_model_age_18_30 <- glm(ct_simple ~ sex + source + vaccine_status,
                           data = trc_dt[age_group == "18-30"] ,
                           family = binomial(link = "logit"))

# summary(vac_model_age_18_30)
# exp(coef(vac_model_age_18_30)[-1])
test_age_18_30 <- (1-exp(coef(vac_model_age_18_30)[-1]))*100
vcov_age_18_30 <- sandwich(vac_model_age_18_30, type = "HC1")
# exp(coefci(vac_model_age_18_30, vcov = vcov_age_18_30))
test_age_18_30_ci <- (1-exp(coefci(vac_model_age_18_30, vcov = vcov_age_18_30)))*100

vac_model_age_31_40 <- glm(ct_simple ~ sex + source + vaccine_status,
                           data = trc_dt[age_group == "31-40"],
                           family = binomial(link = "logit"))

# summary(vac_model_age_31_40)
# exp(coef(vac_model_age_31_40)[-1])
test_age_31_40 <- (1-exp(coef(vac_model_age_31_40)[-1]))*100

vcov_age_31_40 <- sandwich(vac_model_age_31_40, type = "HC1")
# exp(coefci(vac_model_age_31_40, vcov = vcov_age_31_40))
test_age_31_40_ci <- (1-exp(coefci(vac_model_age_31_40, vcov = vcov_age_31_40)))*100

vac_model_age_41_50 <- glm(ct_simple ~ sex + source + vaccine_status,
                           data = trc_dt[age_group == "41-50"],
                           family = binomial(link = "logit"))

# summary(vac_model_age_41_50)
# exp(coef(vac_model_age_41_50)[-1])
test_age_41_50 <- (1-exp(coef(vac_model_age_41_50)[-1]))*100

vcov_age_41_50 <- sandwich(vac_model_age_41_50, type = "HC1")
# exp(coefci(vac_model_age_41_50, vcov = vcov_age_41_50))
test_age_41_50_ci <- (1-exp(coefci(vac_model_age_41_50, vcov = vcov_age_41_50)))*100

vac_model_age_51_60 <- glm(ct_simple ~ sex + source + vaccine_status,
                           data = trc_dt[age_group == "51-60"],
                           family = binomial(link = "logit"))

# summary(vac_model_age_51_60)
# exp(coef(vac_model_age_51_60)[-1])
test_age_51_60 <- (1-exp(coef(vac_model_age_51_60)[-1]))*100

vcov_age_51_60 <- sandwich(vac_model_age_51_60, type = "HC1")
# exp(coefci(vac_model_age_51_60, vcov = vcov_age_51_60))
test_age_51_60_ci <- (1-exp(coefci(vac_model_age_51_60, vcov = vcov_age_51_60)))*100

vac_model_age_60pl <- glm(ct_simple ~ sex + source + vaccine_status,
                          data = trc_dt[age_group == "60+"],
                          family = binomial(link = "logit"))

# summary(vac_model_age_60pl)
# exp(coef(vac_model_age_60pl)[-1])
test_age_60pl <- (1-exp(coef(vac_model_age_60pl)[-1]))*100

vcov_age_60pl <- sandwich(vac_model_age_60pl, type = "HC1")
# exp(coefci(vac_model_age_60pl, vcov = vcov_age_60pl))
test_age_60pl_ci <- (1-exp(coefci(vac_model_age_60pl, vcov = vcov_age_60pl)))*100

# split by sex
# male
vac_model_male_pcr <- glm(ct_simple ~ age + source + vaccine_status, 
                          data = trc_dt[sex == "M"],
                          family = binomial(link = "logit"))

# summary(vac_model_male_pcr)
# exp(coef(vac_model_male_pcr)[-1])
test__male_pcr <- (1-exp(coef(vac_model_male_pcr)[-1]))*100

vcov_male_pcr <- sandwich(vac_model_male_pcr, type = "HC1")
# exp(coefci(vac_model_male_pcr, vcov = vcov_male_pcr))
test__male_pcr_ci <- (1-exp(coefci(vac_model_male_pcr, vcov = vcov_male_pcr)))*100


# female
vac_model_female_pcr <- glm(ct_simple ~ age + source + vaccine_status, 
                            data = trc_dt[sex == "F"],
                            family = binomial(link = "logit"))

# summary(vac_model_female_pcr)
# exp(coef(vac_model_female_pcr)[-1])
test_female_pcr <- (1-exp(coef(vac_model_female_pcr)[-1]))*100

vcov_female_pcr <- sandwich(vac_model_female_pcr, type = "HC1")
# exp(coefci(vac_model_female_pcr, vcov = vcov_female_pcr))
test_female_pcr_ci <- (1-exp(coefci(vac_model_female_pcr, vcov = vcov_female_pcr)))*100
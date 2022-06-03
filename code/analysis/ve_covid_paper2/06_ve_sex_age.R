# This code prepares ve models by sex and age
# for the COVID-19 vaccines effectiveness against 
# symptomatic SARS-CoV-2 Delta variant infection: 
# a population-based case-control study in St. Petersburg, Russia

library(data.table)
library(sandwich)
library(lmtest)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load dataset
load("data/ve_covid_paper2/case_control_final.Rdata")

# order vaccination status factor
case_control_final[, vac_status := factor(vac_status, levels = c("non_vac", "part_vac", "full_vac"))]

# by age
# age vars
case_control_final[, age_18_30 := fifelse(age_group == "18-30", 1, 0)]
case_control_final[, age_31_40 := fifelse(age_group == "31-40", 1, 0)]
case_control_final[, age_41_50 := fifelse(age_group == "41-50", 1, 0)]
case_control_final[, age_51_60 := fifelse(age_group == "51-60", 1, 0)]
case_control_final[, age_60pl := fifelse(age_group == "60+", 1, 0)]

vac_model_age_18_30_pcr <- glm(is_case ~ male + final_vac_status, 
                           data = case_control_final[age_18_30 == 1 & pcr_past == 0] ,
                           family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_age_18_30_pcr)[-1])
# 0.2829132582965
(1-exp(coef(vac_model_age_18_30_pcr)[-1]))*100
#    71.708674

vcov_age_18_30_pcr <- sandwich(vac_model_age_18_30_pcr, type = "HC1")
exp(coefci(vac_model_age_18_30_pcr, vcov = vcov_age_18_30_pcr))
#  0.15661349804859 0.5110664962935
(1-exp(coefci(vac_model_age_18_30_pcr, vcov = vcov_age_18_30_pcr)))*100
# 84.33865   48.893350

vac_model_age_31_40_pcr <- glm(is_case ~ male + final_vac_status, 
                           data = case_control_final[age_31_40 == 1 &  pcr_past == 0],
                           family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_age_31_40_pcr)[-1])
# 0.4074143843192
(1-exp(coef(vac_model_age_31_40_pcr)[-1]))*100
#    59.25856 

vcov_age_31_40_pcr <- sandwich(vac_model_age_31_40_pcr, type = "HC1")
exp(coefci(vac_model_age_31_40_pcr, vcov = vcov_age_31_40_pcr))
# 0.2768779628827  0.5994932887474
(1-exp(coefci(vac_model_age_31_40_pcr, vcov = vcov_age_31_40_pcr)))*100
# 72.31220    40.05067

vac_model_age_41_50_pcr <- glm(is_case ~ male + final_vac_status, 
                           data = case_control_final[age_41_50 == 1 &  pcr_past == 0],
                           family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_age_41_50_pcr)[-1])
# 0.4014936
(1-exp(coef(vac_model_age_41_50_pcr)[-1]))*100
#  59.850640

vcov_age_41_50_pcr <- sandwich(vac_model_age_41_50_pcr, type = "HC1")
exp(coefci(vac_model_age_41_50_pcr, vcov = vcov_age_41_50_pcr))
# 0.26754639  0.6025015
(1-exp(coefci(vac_model_age_41_50_pcr, vcov = vcov_age_41_50_pcr)))*100
# 73.24536    39.74985

vac_model_age_51_60_pcr <- glm(is_case ~ male + final_vac_status, 
                           data = case_control_final[age_51_60 == 1 &  pcr_past == 0],
                           family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_age_51_60_pcr)[-1])
# 0.480912795936
(1-exp(coef(vac_model_age_51_60_pcr)[-1]))*100
#    51.9087204 

vcov_age_51_60_pcr <- sandwich(vac_model_age_51_60_pcr, type = "HC1")
exp(coefci(vac_model_age_51_60_pcr, vcov = vcov_age_51_60_pcr))
#  0.3351898144946  0.68998849993
(1-exp(coefci(vac_model_age_51_60_pcr, vcov = vcov_age_51_60_pcr)))*100
# 66.48102    31.00115

vac_model_age_60pl_pcr <- glm(is_case ~ male + final_vac_status, 
                          data = case_control_final[age_60pl == 1 &  pcr_past == 0],
                          family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_age_60pl_pcr)[-1])
#  0.4540448
(1-exp(coef(vac_model_age_60pl_pcr)[-1]))*100
#   54.59552

vcov_age_60pl_pcr <- sandwich(vac_model_age_60pl_pcr, type = "HC1")
exp(coefci(vac_model_age_60pl_pcr, vcov = vcov_age_60pl_pcr))
# 0.33155625  0.6217850
(1-exp(coefci(vac_model_age_60pl_pcr, vcov = vcov_age_60pl_pcr)))*100
# 66.844375    37.82150

# split by sex
# male
vac_model_male_pcr <- glm(is_case ~ age + final_vac_status, 
                      data = case_control_final[male == 1 & pcr_past == 0],
                      family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_male_pcr)[-1])
# 0.3430951
(1-exp(coef(vac_model_male_pcr)[-1]))*100
#  65.690493

vcov_male_pcr <- sandwich(vac_model_male_pcr, type = "HC1")
exp(coefci(vac_model_male_pcr, vcov = vcov_male_pcr))
# 0.2589759 0.4545373
(1-exp(coefci(vac_model_male_pcr, vcov = vcov_male_pcr)))*100
# 74.102408   54.546266

# female
vac_model_female_pcr <- glm(is_case ~ age + final_vac_status, 
                        data = case_control_final[male == 0 & pcr_past == 0],
                        family = binomial(link = "logit"))

# final_vac_statusspuntik_full
exp(coef(vac_model_female_pcr)[-1])
# 0.4769771
(1-exp(coef(vac_model_female_pcr)[-1]))*100
#52.302291 

vcov_female_pcr <- sandwich(vac_model_female_pcr, type = "HC1")
exp(coefci(vac_model_female_pcr, vcov = vcov_female_pcr))
# 0.3846135 0.5915215
(1-exp(coefci(vac_model_female_pcr, vcov = vcov_female_pcr)))*100
# 61.5386528   40.847848
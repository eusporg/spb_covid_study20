# This code fits logit models of hospitalization 
# to the data and calculates OR and CI
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(sandwich)
library(lmtest)

options(scipen = 999)
# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper/trc_dt.Rdata")

# Model for effect after 56 days
# trc_dt[ , .N, by = .(is.na(days_since_second), (days_since_second < 56) ) ]

trc_dt[, full_vac_56 := fcase(is.na(days_since_second), "non_vac", 
															(vac_status3 != "full_vac"), NA_character_,  
															((days_since_second) < 56) & (vac_status3 == "full_vac"),  "before_56",
															(days_since_second) >= 56,  "after_56") ]
trc_dt[, full_vac_56 := factor(full_vac_56, levels = c("non_vac", "before_56", "after_56"))]

# trc_dt[ , .N, by = full_vac_56]
model_day_groups_1 <- glm(hospitalization ~ full_vac_56, data = trc_dt, family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.model_day_groups_1 <- sandwich(model_day_groups_1, type = "HC1")

exp(coef(model_day_groups_1)[-1]) 
# full_vac_56before_56 0.5474350
# full_vac_56after_56   0.2775572
exp(coefci(model_day_groups_1, vcov = vcov_sandwich.model_day_groups_1))
# full_vac_56before_56 0.24257043 1.23545576
# full_vac_56after_56  0.15214560 0.50634395

(1-exp(coef(model_day_groups_1)[-1]))*100
# full_vac_56before_56 45.25650 
# full_vac_56after_56   72.24428 
(1-exp(coefci(model_day_groups_1, vcov = vcov_sandwich.model_day_groups_1)))*100
# full_vac_56before_56 75.74296 -23.54558
# full_vac_56after_56  84.78544  49.36561

model_day_groups_2 <- glm(hospitalization ~ 
														sex +
														age +
														source + 
														# vac_status_simple + 
														full_vac_56,
													data = trc_dt, 
													family = binomial(link = "logit"))
# covariance matrix for ci
vcov_sandwich.model_day_groups_2 <- sandwich(model_day_groups_2, type = "HC1")

exp(coef(model_day_groups_2)[-1]) 
# full_vac_56before_56 0.4014528
# full_vac_56after_56   0.1495183
exp(coefci(model_day_groups_2, vcov = vcov_sandwich.model_day_groups_2))
# full_vac_56before_56 0.170338226 0.9461430466
# full_vac_56after_56  0.080786569 0.2767258874

(1-exp(coef(model_day_groups_2)[-1]))*100
# full_vac_56before_56 59.854723
# full_vac_56after_56   85.048166 
(1-exp(coefci(model_day_groups_2, vcov = vcov_sandwich.model_day_groups_2)))*100
# full_vac_56before_56  82.966177   5.385695
# full_vac_56after_56   91.921343  72.327411

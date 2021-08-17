# This code fits logit models for lung injury
# and calculates OR and CI 
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

# Lung injury models
# Any injury == 0 vs 1, 2, 3+4 
# ct_score_simple

lung_model_0vs1234_crude <- glm(ct_score_simple ~ vac_status_simple,
																data = trc_dt, 
																family = binomial(link = "logit"))

# summary(lung_model_0vs1234_crude)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs1234_crude <- sandwich(lung_model_0vs1234_crude, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs1234_crude)[-1])
# vac_status_simple 0.6387707 
exp(coefci(lung_model_0vs1234_crude, vcov = vcov_sandwich.lung_model_0vs1234_crude))
# vac_status_simple 0.5683935 0.7178618

(1-exp(coef(lung_model_0vs1234_crude)[-1]))*100
# vac_status_simple 36.12293
(1-exp(coefci(lung_model_0vs1234_crude, vcov = vcov_sandwich.lung_model_0vs1234_crude)))*100
# vac_status_simple 43.16065   28.21382

lung_model_0vs1234_adj <- glm(ct_score_simple ~ 
																age + 
																sex + 
																source + 
																vac_status_simple,
																data = trc_dt, 
																family = binomial(link = "logit"))

# summary(lung_model_0vs1234_adj)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs1234_adj <- sandwich(lung_model_0vs1234_adj, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs1234_adj)[-1])
# vac_status_simple 0.4572608 
exp(coefci(lung_model_0vs1234_adj, vcov = vcov_sandwich.lung_model_0vs1234_adj))
# vac_status_simple 0.4039675 0.5175848

(1-exp(coef(lung_model_0vs1234_adj)[-1]))*100
# vac_status_simple 54.273921 
(1-exp(coefci(lung_model_0vs1234_adj, vcov = vcov_sandwich.lung_model_0vs1234_adj)))*100
# vac_status_simple 59.603255  48.241516

# More than 25% == 0 vs 2, 3+4
# ct_score_0vs234
trc_dt[ct_score == 0,  ct_score_0vs234 := 0]
trc_dt[ct_score > 1, ct_score_0vs234 := 1]
trc_dt[, ct_score_0vs234 := as.factor(ct_score_0vs234)]

lung_model_0vs234_crude <- glm(ct_score_0vs234 ~ vac_status_simple,
																data = trc_dt, 
																family = binomial(link = "logit"))

# summary(lung_model_0vs234_crude)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs234_crude <- sandwich(lung_model_0vs234_crude, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs234_crude)[-1])
# vac_status_simple 0.5922682 
exp(coefci(lung_model_0vs234_crude, vcov = vcov_sandwich.lung_model_0vs234_crude))
# vac_status_simple 0.4846299 0.7238134

(1-exp(coef(lung_model_0vs234_crude)[-1]))*100
# vac_status_simple 40.77318
(1-exp(coefci(lung_model_0vs234_crude, vcov = vcov_sandwich.lung_model_0vs234_crude)))*100
# vac_status_simple 51.53701 27.61866

lung_model_0vs234_adj <- glm(ct_score_0vs234 ~ 
																age + 
																sex + 
																source + 
																vac_status_simple,
															data = trc_dt[!is.na(ct_score_0vs234)], 
															family = binomial(link = "logit"))

# summary(lung_model_0vs234_adj)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs234_adj <- sandwich(lung_model_0vs234_adj, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs234_adj)[-1])
# vac_status_simple 0.3579901 
exp(coefci(lung_model_0vs234_adj, vcov = vcov_sandwich.lung_model_0vs234_adj))
# vac_status_simple 0.28559282 0.4487399

(1-exp(coef(lung_model_0vs234_adj)[-1]))*100
# vac_status_simple 64.200993
(1-exp(coefci(lung_model_0vs234_adj, vcov = vcov_sandwich.lung_model_0vs234_adj)))*100
# vac_status_simple 71.440718  55.126011

# More than 50% == 0 vs 3+4
# ct_score_0vs34
trc_dt[ct_score == 0,  ct_score_0vs34 := 0]
trc_dt[ct_score > 2, ct_score_0vs34 := 1]
trc_dt[, ct_score_0vs34 := as.factor(ct_score_0vs34)]

lung_model_0vs34_crude <- glm(ct_score_0vs34 ~ vac_status_simple,
															data = trc_dt, 
															family = binomial(link = "logit"))

# summary(lung_model_0vs34_crude)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs34_crude <- sandwich(lung_model_0vs34_crude, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs34_crude)[-1])
# vac_status_simple 0.4193269 
exp(coefci(lung_model_0vs34_crude, vcov = vcov_sandwich.lung_model_0vs34_crude))
# vac_status_simple 0.25512398 0.68921406

(1-exp(coef(lung_model_0vs34_crude)[-1]))*100
# vac_status_simple 58.06731 
(1-exp(coefci(lung_model_0vs34_crude, vcov = vcov_sandwich.lung_model_0vs34_crude)))*100
# vac_status_simple 74.48760 31.07859

lung_model_0vs34_adj <- glm(ct_score_0vs34 ~ 
															age + 
															sex + 
															source + 
															vac_status_simple,
														data = trc_dt[!is.na(ct_score_0vs34)], 
														family = binomial(link = "logit"))

# summary(lung_model_0vs34_adj)

# covariance matrix for ci
vcov_sandwich.lung_model_0vs34_adj <- sandwich(lung_model_0vs34_adj, type = "HC1")

# coeffs and ci
exp(coef(lung_model_0vs34_adj)[-1])
# vac_status_simple  0.2420246 
exp(coefci(lung_model_0vs34_adj, vcov = vcov_sandwich.lung_model_0vs34_adj))
# vac_status_simple 0.143717309 0.407577392

(1-exp(coef(lung_model_0vs34_adj)[-1]))*100
# vac_status_simple 75.797536
(1-exp(coefci(lung_model_0vs34_adj, vcov = vcov_sandwich.lung_model_0vs34_adj)))*100
# vac_status_simple 85.628269  59.24226080

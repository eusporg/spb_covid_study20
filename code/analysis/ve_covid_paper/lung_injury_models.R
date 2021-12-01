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
# with the aid of usethis::edit_r_environ()
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
# vac_status_simple 0.6386104 
exp(coefci(lung_model_0vs1234_crude, vcov = vcov_sandwich.lung_model_0vs1234_crude))
# vac_status_simple 0.5682502 0.7176824

(1-exp(coef(lung_model_0vs1234_crude)[-1]))*100
# vac_status_simple 36.13896
(1-exp(coefci(lung_model_0vs1234_crude, vcov = vcov_sandwich.lung_model_0vs1234_crude)))*100
# vac_status_simple 43.17498   28.23176

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
# vac_status_simple 0.4569664 
exp(coefci(lung_model_0vs1234_adj, vcov = vcov_sandwich.lung_model_0vs1234_adj))
# vac_status_simple 0.4037003 0.5172606

(1-exp(coef(lung_model_0vs1234_adj)[-1]))*100
# vac_status_simple 54.303359
(1-exp(coefci(lung_model_0vs1234_adj, vcov = vcov_sandwich.lung_model_0vs1234_adj)))*100
# vac_status_simple 59.629966  48.273936

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
# vac_status_simple 0.5921195
exp(coefci(lung_model_0vs234_crude, vcov = vcov_sandwich.lung_model_0vs234_crude))
# vac_status_simple 0.4845080 0.7236322

(1-exp(coef(lung_model_0vs234_crude)[-1]))*100
# vac_status_simple 40.78805
(1-exp(coefci(lung_model_0vs234_crude, vcov = vcov_sandwich.lung_model_0vs234_crude)))*100
# vac_status_simple 51.54920 27.63678

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
# vac_status_simple 0.3576057
exp(coefci(lung_model_0vs234_adj, vcov = vcov_sandwich.lung_model_0vs234_adj))
# vac_status_simple 0.28526856 0.44828589

(1-exp(coef(lung_model_0vs234_adj)[-1]))*100
# vac_status_simple 64.239425
(1-exp(coefci(lung_model_0vs234_adj, vcov = vcov_sandwich.lung_model_0vs234_adj)))*100
# vac_status_simple 71.473144  55.171411

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
# vac_status_simple 0.4192216
exp(coefci(lung_model_0vs34_crude, vcov = vcov_sandwich.lung_model_0vs34_crude))
# vac_status_simple 0.25505988 0.68904123

(1-exp(coef(lung_model_0vs34_crude)[-1]))*100
# vac_status_simple 58.07784
(1-exp(coefci(lung_model_0vs34_crude, vcov = vcov_sandwich.lung_model_0vs34_crude)))*100
# vac_status_simple 74.49401 31.09588

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
# vac_status_simple   0.2417356
exp(coefci(lung_model_0vs34_adj, vcov = vcov_sandwich.lung_model_0vs34_adj))
# vac_status_simple 0.143538911 0.407109766

(1-exp(coef(lung_model_0vs34_adj)[-1]))*100
# vac_status_simple 75.826442
(1-exp(coefci(lung_model_0vs34_adj, vcov = vcov_sandwich.lung_model_0vs34_adj)))*100
# vac_status_simple 85.646109  59.289023

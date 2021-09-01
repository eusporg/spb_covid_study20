# This code fits logit models to the data 
# and calculates OR and CI
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
# library(gtsummary)
library(sandwich)
library(lmtest)
library(mgcv)
library(gratia)
# library(ggplot2)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper/trc_dt.Rdata")


# Saturation models
# 96% and more
# sat_model_96_vs_more
trc_dt[saturation >= 96,  sat_score_96_vs_more := 0]
trc_dt[saturation < 96, sat_score_96_vs_more := 1]
trc_dt[, sat_score_96_vs_more := as.factor(sat_score_96_vs_more)]

sat_model_96_vs_more_crude <- glm(as.factor(sat_score_96_vs_more) ~ vac_status_simple,
																	data = trc_dt, 
																	family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_96_vs_more_crude <- sandwich(sat_model_96_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_96_vs_more_crude)[-1])
# vac_status_simple 0.4678175
exp(coefci(sat_model_96_vs_more_crude, vcov = vcov_sandwich.sat_model_96_vs_more_crude))
# vac_status_simple 0.28230913 0.77522535

(1-exp(coef(sat_model_96_vs_more_crude)[-1]))*100
# vac_status_simple 53.21825
(1-exp(coefci(sat_model_96_vs_more_crude, vcov = vcov_sandwich.sat_model_96_vs_more_crude)))*100
# vac_status_simple 71.76909 22.47747

sat_model_96_vs_more_adj <- glm(sat_score_96_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_96_vs_more)], 
																family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_96_vs_more_adj <- sandwich(sat_model_96_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_96_vs_more_adj)[-1])
# vac_status_simple  0.3035670
exp(coefci(sat_model_96_vs_more_adj, vcov = vcov_sandwich.sat_model_96_vs_more_adj))
# vac_status_simple 0.1811762217 0.508637013

(1-exp(coef(sat_model_96_vs_more_adj)[-1]))*100
# vac_status_simple 69.643299
(1-exp(coefci(sat_model_96_vs_more_adj, vcov = vcov_sandwich.sat_model_96_vs_more_adj)))*100
# vac_status_simple 81.882378  49.136299

# Less than 94%
# sat_model_94_vs_more == 0 vs 2+3
trc_dt[sat_score == 0,  sat_score_94_vs_more := 0]
trc_dt[sat_score ==  2 | sat_score ==  3, sat_score_94_vs_more := 1]
trc_dt[, sat_score_94_vs_more := as.factor(sat_score_94_vs_more)]

sat_model_94_vs_more_crude <- glm(as.factor(sat_score_94_vs_more) ~ vac_status_simple,
																	data = trc_dt, 
																	family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_94_vs_more_crude <- sandwich(sat_model_94_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_94_vs_more_crude)[-1])
# vac_status_simple 0.5813982 
exp(coefci(sat_model_94_vs_more_crude, vcov = vcov_sandwich.sat_model_94_vs_more_crude))
# vac_status_simple 0.31549880 1.07139496

(1-exp(coef(sat_model_94_vs_more_crude)[-1]))*100
# vac_status_simple 41.86018
(1-exp(coefci(sat_model_94_vs_more_crude, vcov = vcov_sandwich.sat_model_94_vs_more_crude)))*100
# vac_status_simple 68.45012 -7.139496

sat_model_94_vs_more_adj <- glm(sat_score_94_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_94_vs_more)], 
																family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_94_vs_more_adj <- sandwich(sat_model_94_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_94_vs_more_adj)[-1])
# vac_status_simple  0.3878276 
exp(coefci(sat_model_94_vs_more_adj, vcov = vcov_sandwich.sat_model_94_vs_more_adj))
# vac_status_simple 0.2081766085 0.722512711

(1-exp(coef(sat_model_94_vs_more_adj)[-1]))*100
# vac_status_simple 61.217240 
(1-exp(coefci(sat_model_94_vs_more_adj, vcov = vcov_sandwich.sat_model_94_vs_more_adj)))*100
# vac_status_simple 79.182339  27.748729

# Less than 92%
# sat_model_92_vs_more == 0 vs 3
trc_dt[sat_score == 0,  sat_score_92_vs_more := 0]
trc_dt[sat_score ==  3, sat_score_92_vs_more := 1]
trc_dt[, sat_score_92_vs_more := as.factor(sat_score_92_vs_more)]

sat_model_92_vs_more_crude <- glm(as.factor(sat_score_92_vs_more) ~ vac_status_simple,
																	data = trc_dt, 
																	family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_92_vs_more_crude <- sandwich(sat_model_92_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_92_vs_more_crude)[-1])
# vac_status_simple 0.5771699 
exp(coefci(sat_model_92_vs_more_crude, vcov = vcov_sandwich.sat_model_92_vs_more_crude))
# vac_status_simple 0.179768145 1.853081659

(1-exp(coef(sat_model_92_vs_more_crude)[-1]))*100
# vac_status_simple 42.28301
(1-exp(coefci(sat_model_92_vs_more_crude, vcov = vcov_sandwich.sat_model_92_vs_more_crude)))*100
# vac_status_simple 82.02319 -85.30817

sat_model_92_vs_more_adj <- glm(sat_score_92_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_92_vs_more)], 
																family = binomial(link = "logit"))

# covariance matrix for ci
vcov_sandwich.sat_model_92_vs_more_adj <- sandwich(sat_model_92_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_92_vs_more_adj)[-1])
# vac_status_simple  0.338905
exp(coefci(sat_model_92_vs_more_adj, vcov = vcov_sandwich.sat_model_92_vs_more_adj))
# vac_status_simple 0.10255436515 1.1199583908

(1-exp(coef(sat_model_92_vs_more_adj)[-1]))*100
# vac_status_simple 66.10950 
(1-exp(coefci(sat_model_92_vs_more_adj, vcov = vcov_sandwich.sat_model_92_vs_more_adj)))*100
# vac_status_simple 89.744563  -11.995839
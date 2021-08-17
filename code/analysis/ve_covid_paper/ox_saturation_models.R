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

summary(sat_model_96_vs_more_crude)
tbl_regression(sat_model_96_vs_more_crude, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_96_vs_more_crude <- sandwich(sat_model_96_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_96_vs_more_crude)[-1])
# vac_status_simple 0.4678557 
exp(coefci(sat_model_96_vs_more_crude, vcov = vcov_sandwich.sat_model_96_vs_more_crude))
# vac_status_simple 0.28233221 0.77528868

(1-exp(coef(sat_model_96_vs_more_crude)[-1]))*100
# vac_status_simple 50.05862
(1-exp(coefci(sat_model_96_vs_more_crude, vcov = vcov_sandwich.sat_model_96_vs_more_crude)))*100
# vac_status_simple 68.62287 20.51086

sat_model_96_vs_more_adj <- glm(sat_score_96_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_96_vs_more)], 
																family = binomial(link = "logit"))

summary(sat_model_96_vs_more_adj)
tbl_regression(sat_model_96_vs_more_adj, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_96_vs_more_adj <- sandwich(sat_model_96_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_96_vs_more_adj)[-1])
# vac_status_simple  0.3036418 
exp(coefci(sat_model_96_vs_more_adj, vcov = vcov_sandwich.sat_model_96_vs_more_adj))
# vac_status_simple 0.1812215984 0.508760217

(1-exp(coef(sat_model_96_vs_more_adj)[-1]))*100
# vac_status_simple 69.635820 
(1-exp(coefci(sat_model_96_vs_more_adj, vcov = vcov_sandwich.sat_model_96_vs_more_adj)))*100
# vac_status_simple 81.877840  49.123978

# Less than 94%
# sat_model_94_vs_more == 0 vs 2+3
trc_dt[sat_score == 0,  sat_score_94_vs_more := 0]
trc_dt[sat_score ==  2 | sat_score ==  3, sat_score_94_vs_more := 1]
trc_dt[, sat_score_94_vs_more := as.factor(sat_score_94_vs_more)]

sat_model_94_vs_more_crude <- glm(as.factor(sat_score_94_vs_more) ~ vac_status_simple,
																	data = trc_dt, 
																	family = binomial(link = "logit"))

summary(sat_model_94_vs_more_crude)
tbl_regression(sat_model_94_vs_more_crude, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_94_vs_more_crude <- sandwich(sat_model_94_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_94_vs_more_crude)[-1])
# vac_status_simple 0.5818315
exp(coefci(sat_model_94_vs_more_crude, vcov = vcov_sandwich.sat_model_94_vs_more_crude))
# vac_status_simple 0.31573604 1.07218644

(1-exp(coef(sat_model_94_vs_more_crude)[-1]))*100
# vac_status_simple 41.81685
(1-exp(coefci(sat_model_94_vs_more_crude, vcov = vcov_sandwich.sat_model_94_vs_more_crude)))*100
# vac_status_simple 68.42640 -7.218644

sat_model_94_vs_more_adj <- glm(sat_score_94_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_94_vs_more)], 
																family = binomial(link = "logit"))

summary(sat_model_94_vs_more_adj)
tbl_regression(sat_model_94_vs_more_adj, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_94_vs_more_adj <- sandwich(sat_model_94_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_94_vs_more_adj)[-1])
# vac_status_simple  0.3873692 
exp(coefci(sat_model_94_vs_more_adj, vcov = vcov_sandwich.sat_model_94_vs_more_adj))
# vac_status_simple 0.2079149743 0.721712739

(1-exp(coef(sat_model_94_vs_more_adj)[-1]))*100
# vac_status_simple 61.263082
(1-exp(coefci(sat_model_94_vs_more_adj, vcov = vcov_sandwich.sat_model_94_vs_more_adj)))*100
# vac_status_simple 79.208503  27.82873

# Less than 92%
# sat_model_92_vs_more == 0 vs 3
trc_dt[sat_score == 0,  sat_score_92_vs_more := 0]
trc_dt[sat_score ==  3, sat_score_92_vs_more := 1]
trc_dt[, sat_score_92_vs_more := as.factor(sat_score_92_vs_more)]

sat_model_92_vs_more_crude <- glm(as.factor(sat_score_92_vs_more) ~ vac_status_simple,
																	data = trc_dt, 
																	family = binomial(link = "logit"))

summary(sat_model_92_vs_more_crude)
tbl_regression(sat_model_92_vs_more_crude, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_92_vs_more_crude <- sandwich(sat_model_92_vs_more_crude, type = "HC1")

# coeffs and ci
exp(coef(sat_model_92_vs_more_crude)[-1])
# vac_status_simple 0.5776001
exp(coefci(sat_model_92_vs_more_crude, vcov = vcov_sandwich.sat_model_92_vs_more_crude))
# vac_status_simple 0.179902758 1.854456416

(1-exp(coef(sat_model_92_vs_more_crude)[-1]))*100
# vac_status_simple 42.23999
(1-exp(coefci(sat_model_92_vs_more_crude, vcov = vcov_sandwich.sat_model_92_vs_more_crude)))*100
# vac_status_simple 82.00972 -85.44564

sat_model_92_vs_more_adj <- glm(sat_score_92_vs_more ~ 
																	age + 
																	sex + 
																	source + 
																	vac_status_simple,
																data = trc_dt[!is.na(sat_score_92_vs_more)], 
																family = binomial(link = "logit"))

summary(sat_model_92_vs_more_adj)
tbl_regression(sat_model_92_vs_more_adj, exponentiate = T) 
# covariance matrix for ci
vcov_sandwich.sat_model_92_vs_more_adj <- sandwich(sat_model_92_vs_more_adj, type = "HC1")

# coeffs and ci
exp(coef(sat_model_92_vs_more_adj)[-1])
# vac_status_simple  0.3388173
exp(coefci(sat_model_92_vs_more_adj, vcov = vcov_sandwich.sat_model_92_vs_more_adj))
# vac_status_simple 0.10251407520 1.1198187906

(1-exp(coef(sat_model_92_vs_more_adj)[-1]))*100
# vac_status_simple 66.11827 
(1-exp(coefci(sat_model_92_vs_more_adj, vcov = vcov_sandwich.sat_model_92_vs_more_adj)))*100
# vac_status_simple 89.748592  -11.981879
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

# OR against referral to hospital
hosp_model_1 <- glm(hospitalization ~ vac_status_simple,
										data = trc_dt, 
										family = binomial(link = "logit"))


# covariance matrix for ci
vcov_sandwich.hosp_model_1 <- sandwich(hosp_model_1, type = "HC1")

exp(coef(hosp_model_1)[-1])
# vac_status_simple 0.3384803 
exp(coefci(hosp_model_1, vcov = vcov_sandwich.hosp_model_1))
# vac_status_simple 0.20794914 0.55094666

(1-exp(coef(hosp_model_1)[-1]))*100
# vac_status_simple 66.15197
(1-exp(coefci(hosp_model_1, vcov = vcov_sandwich.hosp_model_1)))*100
# vac_status_simple 79.20509 44.90533

# adjOR against referral to hospital
hosp_model_2 <- glm(hospitalization ~ 
											age + 
											sex + 
											source + 
											vac_status_simple,
										data = trc_dt, 
										family = binomial(link = "logit"))

tbl_regression(hosp_model_2, exponentiate = T) 
# tbl <- tbl_regression(hosp_model_2, exponentiate = T) 
# as_kable(tbl, format = "latex")

# covariance matrix for ci
vcov_sandwich.hosp_model_2 <- sandwich(hosp_model_2, type = "HC1")

exp(coef(hosp_model_2)[-1]) 
# vac_status_simple 0.193970 
exp(coefci(hosp_model_2, vcov = vcov_sandwich.hosp_model_2))
# vac_status_simple 0.1171310855 0.3212159525

(1-exp(coef(hosp_model_2)[-1]))*100
# vac_status_simple 80.602997  
(1-exp(coefci(hosp_model_2, vcov = vcov_sandwich.hosp_model_2)))*100
# vac_status_simple 88.286891  67.878405
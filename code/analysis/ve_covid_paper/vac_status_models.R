# This code fits logit models to the data 
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


# Vaccination status models
# Change order of levels with non_vac as basis
trc_dt[, vac_status3 := factor(vac_status3, c("non_vac", "part_vac", "full_vac"))]
part_vac_model_crude <- glm(hospitalization ~
															vac_status3, 
														data = trc_dt,
														family = binomial(link = "logit"))

# summary(part_vac_model_crude)

vcov_sandwich.part_vac_model_crude <- sandwich(part_vac_model_crude, type = "HC1")

exp(coef(part_vac_model_crude)[-1])
# vvac_status3part_vac 0.6284992 
# vac_status3full_vac 0.3339379 
exp(coefci(part_vac_model_crude, vcov = vcov_sandwich.part_vac_model_crude))
# vac_status3part_vac 0.34305895 1.15143845
# vac_status3full_vac 0.20511612 0.54366518

(1-exp(coef(part_vac_model_crude)[-1]))*100
# vvac_status3part_vac 37.15008 
# vac_status3full_vac 66.60621
(1-exp(coefci(part_vac_model_crude, vcov = vcov_sandwich.part_vac_model_crude)))*100
# vac_status3part_vac 65.69411 -15.14384
# vac_status3full_vac 79.48839  45.63348

part_vac_model_adj <- glm(hospitalization ~
														age + 
														sex + 
														source + 
														vac_status3, 
													data = trc_dt,
													family = binomial(link = "logit"))

# summary(part_vac_model_adj)

vcov_sandwich.part_vac_model_adj <- sandwich(part_vac_model_adj, type = "HC1")

exp(coef(part_vac_model_adj)[-1])
# vvac_status3part_vac 0.6521487
# vac_status3full_vac 0.1915862
exp(coefci(part_vac_model_adj, vcov = vcov_sandwich.part_vac_model_adj))
# vac_status3part_vac 0.3524132651 1.2068159435
# vac_status3full_vac 0.1156631509 0.3173463149

(1-exp(coef(part_vac_model_adj)[-1]))*100
# vvac_status3part_vac 34.785128 
# vac_status3full_vac 80.841379 
(1-exp(coefci(part_vac_model_adj, vcov = vcov_sandwich.part_vac_model_adj)))*100
# vac_status3part_vac 64.758673 -20.681594
# vac_status3full_vac 88.433685  68.265369
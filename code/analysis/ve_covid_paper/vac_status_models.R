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
# with the aid of usethis::edit_r_environ()
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
# vvac_status3part_vac 0.6299374
# vac_status3full_vac 0.3339379 
exp(coefci(part_vac_model_crude, vcov = vcov_sandwich.part_vac_model_crude))
# vac_status3part_vac 0.34383828 1.15409245
# vac_status3full_vac 0.20511612 0.54366518

(1-exp(coef(part_vac_model_crude)[-1]))*100
# vvac_status3part_vac 37.00626
# vac_status3full_vac 66.60621
(1-exp(coefci(part_vac_model_crude, vcov = vcov_sandwich.part_vac_model_crude)))*100
# vac_status3part_vac 65.61617 -15.40924
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
# vvac_status3part_vac 0.6562901
# vac_status3full_vac 0.1915800 
exp(coefci(part_vac_model_adj, vcov = vcov_sandwich.part_vac_model_adj))
# vac_status3part_vac 0.3546156350 1.2146016549
# vac_status3full_vac 0.1156592894 0.3173364709

(1-exp(coef(part_vac_model_adj)[-1]))*100
# vvac_status3part_vac 34.785128 -> 34.370987
# vac_status3full_vac 80.841996
(1-exp(coefci(part_vac_model_adj, vcov = vcov_sandwich.part_vac_model_adj)))*100
# vac_status3part_vac 64.538437 -21.460165
# vac_status3full_vac 88.434071  68.266353
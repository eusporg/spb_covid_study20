# This code finds the relevant values for the
# flow chart figure for COVID-19 pandemic in Saint
# Petersburg, Russia paper
library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

#######
# Wave 1 data
load("data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
load("data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Samples not sent to ELISA lab
length(setdiff(test_B_results_matched_to_phone_survey_ids$ID, test_C_results_matched_to_phone_survey_ids$ID))

#######
# Wave 1.5 data
load("data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Tested wave 1.5
uniqueN(test_C_results_matched_to_phone_survey_ids$ID)

# By test status
test_C_results_matched_to_phone_survey_ids[, IgA_or_G_or_M_testC_qualitative := NA ]
test_C_results_matched_to_phone_survey_ids[!is.na(IgA_or_G_or_M_testC), IgA_or_G_or_M_testC_qualitative := 0 ]
test_C_results_matched_to_phone_survey_ids[ IgA_or_G_or_M_testC >= 1, IgA_or_G_or_M_testC_qualitative := 1 ]
test_C_results_matched_to_phone_survey_ids[, .N, by = "IgA_or_G_or_M_testC_qualitative"]

#######
# Wave 2 data

# Phone survey data
load("data/wave2/phone_survey/phone_survey_data.rdata")

# Agreed to participate in the survey
uniqueN(phone_survey_data$ID)

# Load wave 2 serosurvey data
source(paste0("code/analysis/wave2/create_serosurvey_data_for_model_fit.r"))

# Non-eligible or excluded individuals
nrow(phone_survey_data) - nrow(serosurvey_data)

# Asked to provide blood samples
nrow(serosurvey_data)

# Refused to provide blood sample
nrow(serosurvey_data[ agreed == 0])

# Failed to show up at the test site
nrow(serosurvey_data[ agreed == 1 & agreed_and_tested == 0])

# Load wave 2 samples
load("data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
load("data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")
load("data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata")

# Samples not sent to ELISA lab
length(setdiff(unique(c(test_B_results_matched_to_phone_survey_ids$ID, test_D_results_matched_to_phone_survey_ids$ID)), test_C_results_matched_to_phone_survey_ids$ID)) + nrow(test_C_results_matched_to_phone_survey_ids[is.na(IgA_or_G_or_M_testC_quantitative)])

# Positivity by test status
test_C_results_matched_to_phone_survey_ids[, IgA_or_G_or_M_testC_qualitative := NA ]
test_C_results_matched_to_phone_survey_ids[!is.na(IgA_or_G_or_M_testC_quantitative), IgA_or_G_or_M_testC_qualitative := 0 ]
test_C_results_matched_to_phone_survey_ids[ IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC_qualitative := 1 ]
test_C_results_matched_to_phone_survey_ids[, .N, by = "IgA_or_G_or_M_testC_qualitative"]

#######
# Wave 3 data
source(paste0("code/analysis/wave3/create_serosurvey_data_for_model_fit.r"))

# Load wave 3 samples
load("data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Positivity by test status 
#test_C_results_matched_to_phone_survey_ids[, IgA_or_G_or_M_testC_qualitative := NA ]
#test_C_results_matched_to_phone_survey_ids[!is.na(IgA_or_G_or_M_testC_quantitative), IgA_or_G_or_M_testC_qualitative := 0 ]
#test_C_results_matched_to_phone_survey_ids[ IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC_qualitative := 1 ]

serosurvey_data[wave == 3 & was_vaccinated %in% c(0, NA) & !is.na(IgA_or_G_or_M_testC) , .N, by = "IgA_or_G_or_M_testC"]

# Vaccinated
serosurvey_data[wave == 3 & !is.na(IgA_or_G_or_M_testC_quantitative), .N, by = "was_vaccinated"]
serosurvey_data[wave == 3 & !is.na(IgA_or_G_or_M_testC) & was_vaccinated == 1, .N, by = c("IgA_or_G_or_M_testC")]

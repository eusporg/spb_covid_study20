library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Load phone survey data
# created by code/prepare_phone_survey_data/prepare_phone_survey_data.r
load("data/wave2/phone_survey/phone_survey_data.rdata")

# Load Abbott results from the second wave, created by extraneous code
load("data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")

# Load Genetico results from the second wave
load("data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Load Vector-best results from the second wave
load("data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata")

# Load other test results from the second wave
load("data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata")

# Convert Abbott quantitative test to qualitative one
test_B_results_matched_to_phone_survey_ids[IgG_testB_quantitative >= 1.1, IgG_testB := 1]
test_B_results_matched_to_phone_survey_ids[IgG_testB_quantitative < 1.1, IgG_testB := 0]

# Convert Genetico quantitative test to qualitative one
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

# Convert Vector-best quantitative test to qualitative one
test_D_results_matched_to_phone_survey_ids[IgG_or_M_testD_quantitative >= 1.1, IgG_or_M_testD := 1]
test_D_results_matched_to_phone_survey_ids[IgG_or_M_testD_quantitative < 1.1, IgG_or_M_testD := 0]

# Load a modified prevalence computation function
# that is based on GJRM::prev
source("code/analysis/helper_functions/prev_modified.r")

# Load a function that estimates the
# bivariate probit model and outputs the results
source("code/analysis/helper_functions/estimate_bivariate_selection.r")

# Load a function that adjusts the prevalence
# estimates for sensitivity and specificity
# in classical way
source("code/analysis/helper_functions/adjust_prev_test_chars.r")

# Load raking weights fit for phone survey data
# produced by estimate_raking_weights_phone_survey.r
load("estimates/wave2/phone_survey_raking_fit.rdata")
raking_weights <- data.table(ID = names(phone_survey_raking_fit$weightvec), raking_weight = phone_survey_raking_fit$weightvec)

############################
# Prepare data for model fitting

# Test results data contains all individuals that were contacted as of this date
# max(test_D_results_matched_to_phone_survey_ids$draw_sample_date)
initial_phone_call_last_date <- "2020-12-04"

# Keep individuals that have been called up to that date
# in a separate object
serosurvey_data <- phone_survey_data[interview_date <= initial_phone_call_last_date]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district since they are outside Saint Petersburg
serosurvey_data <- serosurvey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Do not remove individuals from districts in St. Petersburg that we have omitted from the study
#serosurvey_data <- serosurvey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
serosurvey_data <- serosurvey_data[!is.na(district)]
serosurvey_data[, district := droplevels(district)]

# Add test results
## Test B
serosurvey_data <- merge(serosurvey_data, test_B_results_matched_to_phone_survey_ids[, c("ID", "IgG_testB_quantitative", "IgG_testB", "draw_sample_date")], by = "ID", all.x = T, all.y = F)

## Test C
serosurvey_data <- merge(serosurvey_data, test_C_results_matched_to_phone_survey_ids[, c("ID", "IgA_or_G_or_M_testC_quantitative", "IgA_or_G_or_M_testC")], by = "ID", all.x = T, all.y = F)

## Test D
serosurvey_data <- merge(serosurvey_data, test_D_results_matched_to_phone_survey_ids[, c("ID", "IgG_or_M_testD_quantitative", "IgG_or_M_testD")], by = "ID", all.x = T, all.y = F)

## Add other test results
serosurvey_data <- merge(serosurvey_data, other_test_results_matched_to_phone_survey_ids[, -c("draw_sample_date", "visited_clinic")], by = "ID", all.x = T, all.y = F)

# Devise agreed and tested variable
serosurvey_data[, agreed_and_tested := 0]
serosurvey_data[agreed == 1 & !is.na(IgG_or_M_testD) | !is.na(IgG_testB) | !is.na(IgA_or_G_or_M_testC), agreed_and_tested := 1]

# Define age groups
serosurvey_data[, agegroup := NA_character_]
serosurvey_data[age < 35, agegroup := "18-34"]
serosurvey_data[age >= 35 & age < 50, agegroup := "35-49"]
serosurvey_data[age >= 50 & age < 65, agegroup := "50-64"]
serosurvey_data[age >= 65, agegroup := "65+"]
serosurvey_data[, agegroup := as.factor(agegroup)]

# Higher education dummy
serosurvey_data[, highereduc := NA_real_]
serosurvey_data[!is.na(education_level), highereduc := 0]
serosurvey_data[education_level %in% c("Higher education"), highereduc := 1]

# Good health dummy
serosurvey_data[, goodhealth := NA_real_]
serosurvey_data[!is.na(health_level), goodhealth := 0]
serosurvey_data[health_level %in% c("Good", "Very good"), goodhealth := 1]

# Was sick dummy
serosurvey_data[, was_sick := NA_real_]
serosurvey_data[times_sick == 0 , was_sick := 0]
serosurvey_data[times_sick > 0 , was_sick := 1]

# Wears mask dummy
serosurvey_data[, wears_mask := NA_real_]
serosurvey_data[!is.na(street_used_mask), wears_mask := 0]
serosurvey_data[street_used_mask %in% c(0,1), wears_mask := 1]

# Higher income dummy
serosurvey_data[, higherincome := NA_real_]
serosurvey_data[!is.na(income_level), higherincome := 1]
serosurvey_data[income_level %in% c("Can't buy appliances", "Can't buy clothes", "Can't buy food"), higherincome := 0]

# Interview week of year
serosurvey_data[, interview_week := as.factor(isoweek(interview_date))]
serosurvey_data[interview_week == 41, interview_week := "October 5-11"]
serosurvey_data[interview_week == 42, interview_week := "October 12-18"]
serosurvey_data[interview_week == 43, interview_week := "October 19-25"]
serosurvey_data[interview_week == 44, interview_week := "October 26 - November 1"]
serosurvey_data[interview_week == 45, interview_week := "November 2-8"]
serosurvey_data[interview_week == 46, interview_week := "November 9-15"]
serosurvey_data[interview_week == 47, interview_week := "November 16-22"]
serosurvey_data[interview_week == 48, interview_week := "November 23-29"]
serosurvey_data[interview_week == 49, interview_week := "November 30 - December 6"]
serosurvey_data[, interview_week := factor(interview_week, levels = c("October 5-11", "October 12-18", "October 19-25", "October 26 - November 1", "November 2-8", "November 9-15", "November 16-22", "November 23-29", "November 30 - December 6"))]

# Sample draw week of year 
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]
serosurvey_data[draw_week == 41, draw_week := "October 5-11"]
serosurvey_data[draw_week == 42, draw_week := "October 12-18"]
serosurvey_data[draw_week == 43, draw_week := "October 19-25"]
serosurvey_data[draw_week == 44, draw_week := "October 26 - November 1"]
serosurvey_data[draw_week == 45, draw_week := "November 2-8"]
serosurvey_data[draw_week == 46, draw_week := "November 9-15"]
serosurvey_data[draw_week == 47, draw_week := "November 16-22"]
serosurvey_data[draw_week == 48, draw_week := "November 23-29"]
serosurvey_data[draw_week == 49, draw_week := "November 30 - December 6"]
serosurvey_data[, draw_week := factor(draw_week, levels = c("October 5-11", "October 12-18", "October 19-25", "October 26 - November 1", "November 2-8", "November 9-15", "November 16-22", "November 23-29", "November 30 - December 6"))]

## Create special definitions of seropositivity for sensitivity analysis
# Positive if both tests agree
serosurvey_data[, testC_and_testD := NA_real_]
serosurvey_data[IgA_or_G_or_M_testC == 0 & IgG_or_M_testD == 0, testC_and_testD := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 & IgG_or_M_testD == 1, testC_and_testD := 1]
serosurvey_data[IgA_or_G_or_M_testC == 0 & IgG_or_M_testD == 1, testC_and_testD := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 & IgG_or_M_testD == 0, testC_and_testD := 0]

# Positive if any test is positive
serosurvey_data[, testC_or_testD := NA_real_]
serosurvey_data[!is.na(IgA_or_G_or_M_testC) | !is.na(IgG_or_M_testD), testC_or_testD := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 | IgG_or_M_testD == 1, testC_or_testD := 1]

# Add raking weights
serosurvey_data <- merge(serosurvey_data, raking_weights, by = "ID", all.x = T, all.y = F)

# On November 10, 2020 there was an error in randomization that prompted card offers
# to individuals that were not offered the card before. Fix that
serosurvey_data[error_in_randomization == 1, lenta_card := 1]

# Keep only districts with enough tested individuals
districts_to_keep <- serosurvey_data[agreed_and_tested == 1, .N, by = "district"][ N >= 5]$district
serosurvey_data <- serosurvey_data[ district %in% districts_to_keep]
serosurvey_data[, district := droplevels(district)]

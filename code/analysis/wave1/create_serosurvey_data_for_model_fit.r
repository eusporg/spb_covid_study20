library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Load phone survey data
# created by code/prepare_phone_survey_data/prepare_phone_survey_data.r
load("data/wave1/phone_survey/phone_survey_data.rdata")

# Load Sugentech results, created by extraneous code
load("data/wave1/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata")

# Load Abbott results, created by extraneous code
load("data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")

# Load Genetico results, created by extraneous code
load("data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Convert Genetico quantitative test to qualitative one
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

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
load("estimates/wave1/phone_survey_raking_fit.rdata")
raking_weights <- data.table(ID = names(phone_survey_raking_fit$weightvec), raking_weight = phone_survey_raking_fit$weightvec)

############################
# Prepare data for model fitting

# Test results data contains all individuals that were contacted as of this date 
initial_phone_call_last_date <- "2020-06-24"

# Keep individuals that have been called up to that date
# in a separate object
serosurvey_data <- phone_survey_data[interview_date <= initial_phone_call_last_date]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district as they are outside Saint Petersburg
serosurvey_data <- serosurvey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Remove individuals from districts in St. Petersburg that we have omitted from the study
serosurvey_data <- serosurvey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
serosurvey_data <- serosurvey_data[!is.na(district)]
serosurvey_data[, district := droplevels(district)]

# Add test results
## Test A
serosurvey_data <- merge(serosurvey_data, test_A_results_matched_to_phone_survey_ids[, c("ID", "IgM_testA", "IgG_testA")], by = "ID", all.x = T, all.y = F)

## Test B
serosurvey_data <- merge(serosurvey_data, test_B_results_matched_to_phone_survey_ids[, c("ID", "IgG_testB_quantitative", "IgG_testB", "draw_sample_date")], by = "ID", all.x = T, all.y = F)

## Test C
serosurvey_data <- merge(serosurvey_data, test_C_results_matched_to_phone_survey_ids[, c("ID", "IgA_or_G_or_M_testC_quantitative", "IgA_or_G_or_M_testC")], by = "ID", all.x = T, all.y = F)

# Devise agreed and tested variable
serosurvey_data[, agreed_and_tested := 0]
serosurvey_data[agreed == 1 & !is.na(IgG_testB), agreed_and_tested := 1]

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
serosurvey_data[!is.na(street_used_mask) & !is.na(left_house), wears_mask := 0]
serosurvey_data[street_used_mask == 1 | left_house == 0, wears_mask := 1]

# Mark interviewers that conducted less than ten interviews
interviews_per_interviewee <- serosurvey_data[, list(interviews = .N), by = "interviewer"]
serosurvey_data[, rare_interviewer := 0]
serosurvey_data[ interviewer %in% interviews_per_interviewee[ interviews < 10]$interviewer, rare_interviewer := 1 ]

# Higher income dummy
serosurvey_data[, higherincome := NA_real_]
serosurvey_data[!is.na(income_level), higherincome := 1]
serosurvey_data[income_level %in% c("Can't buy appliances", "Can't buy clothes", "Can't buy food"), higherincome := 0]

# Policy proponent
serosurvey_data[, policy := NA_character_]
serosurvey_data[!is.na(stricter_policy_proponent) & !is.na(lenient_policy_proponent), policy := "no"]
serosurvey_data[lenient_policy_proponent==1, policy := "lenient"]
serosurvey_data[stricter_policy_proponent==1, policy := "stricter"]
serosurvey_data[, policy := as.factor(policy)]

# Visits of places
serosurvey_data[, visited := NA_real_]
serosurvey_data[!is.na(visited_work ) & !is.na(visited_transport) & !is.na(visited_pharmacy) & !is.na(visited_market_or_shop) & !is.na(visited_facilities) , visited := 0]
serosurvey_data[visited_work==1 | visited_transport==1 | visited_pharmacy==1 | visited_market_or_shop==1 | visited_facilities==1 , visited := 1]

# Interview week of year
serosurvey_data[, interview_week := as.factor(isoweek(interview_date))]

# Sample draw week of year 
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]

## Create special definitions of seropositivity for sensitivity analysis
# Positive if both tests agree
serosurvey_data[, testB_and_testC := NA_real_]
serosurvey_data[IgA_or_G_or_M_testC == 0 & IgG_testB==0, testB_and_testC := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 & IgG_testB==1, testB_and_testC := 1]
serosurvey_data[IgA_or_G_or_M_testC == 0 & IgG_testB==1, testB_and_testC := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 & IgG_testB==0, testB_and_testC := 0]

# Positive if any test is positive
serosurvey_data[, testB_or_testC := NA_real_]
serosurvey_data[!is.na(IgA_or_G_or_M_testC) | !is.na(IgG_testB), testB_or_testC := 0]
serosurvey_data[IgA_or_G_or_M_testC == 1 | IgG_testB==1, testB_or_testC := 1]

# Add raking weights 
serosurvey_data <- merge(serosurvey_data, raking_weights, by = "ID", all.x = T, all.y = F)

# Keep only districts with enough tested individuals
districts_to_keep <- serosurvey_data[agreed_and_tested == 1, .N, by = "district"][ N >= 5]$district
serosurvey_data <- serosurvey_data[ district %in% districts_to_keep]
serosurvey_data[, district := droplevels(district)]

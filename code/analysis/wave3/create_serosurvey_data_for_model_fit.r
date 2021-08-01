library(data.table)
library(lubridate)
library(stringi)
library(stringr)

# Load phone survey data for the two waves
# created by data_preparation/wave3/prepare_phone_survey_data/aggregate_phone_survey_data_from_all_waves.r
load("data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata")

# Load Abbott results from the first wave
load("data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
test_B_results_matched_to_phone_survey_ids_wave1 <- copy(test_B_results_matched_to_phone_survey_ids)
test_B_results_matched_to_phone_survey_ids_wave1[, wave := 1 ]

# Load Genetico results from the first wave
load("data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")
test_C_results_matched_to_phone_survey_ids_wave1 <- copy(test_C_results_matched_to_phone_survey_ids)
test_C_results_matched_to_phone_survey_ids_wave1[, wave := 1 ]

# Load Abbott results from the wave 1.5
load("data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
test_B_results_matched_to_phone_survey_ids_wave15 <- copy(test_B_results_matched_to_phone_survey_ids)
test_B_results_matched_to_phone_survey_ids_wave15[, wave := 1.5 ]

# Load Genetico results from the wave 1.5
load("data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")
test_C_results_matched_to_phone_survey_ids_wave15 <- copy(test_C_results_matched_to_phone_survey_ids)
test_C_results_matched_to_phone_survey_ids_wave15[, wave := 1.5 ]

# Load Abbott results from the second wave
load("data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
test_B_results_matched_to_phone_survey_ids_wave2 <- copy(test_B_results_matched_to_phone_survey_ids)
test_B_results_matched_to_phone_survey_ids_wave2[, c("visited_clinic") := NULL ]
test_B_results_matched_to_phone_survey_ids_wave2[, wave := 2 ]

# Load Genetico results from the second wave
load("data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")
test_C_results_matched_to_phone_survey_ids_wave2 <- copy(test_C_results_matched_to_phone_survey_ids)
test_C_results_matched_to_phone_survey_ids_wave2[, c("visited_clinic") := NULL ]
test_C_results_matched_to_phone_survey_ids_wave2[, draw_sample_date := ymd(draw_sample_date) ]
test_C_results_matched_to_phone_survey_ids_wave2[, wave := 2 ]

# Load Vector-best results from the second wave
load("data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata")
test_D_results_matched_to_phone_survey_ids_wave2 <- copy(test_D_results_matched_to_phone_survey_ids)
test_D_results_matched_to_phone_survey_ids_wave2[, c("visited_clinic") := NULL ]
test_D_results_matched_to_phone_survey_ids_wave2[, draw_sample_date := ymd(draw_sample_date) ]
test_D_results_matched_to_phone_survey_ids_wave2[, wave := 2 ]

# Load Genetico results from the third wave
load("data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")
test_C_results_matched_to_phone_survey_ids_wave3 <- copy(test_C_results_matched_to_phone_survey_ids)
test_C_results_matched_to_phone_survey_ids_wave3[, wave := 3 ]

# Load Vector-best results from the third wave
load("data/wave3/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata")
test_D_results_matched_to_phone_survey_ids_wave3 <- copy(test_D_results_matched_to_phone_survey_ids)
test_D_results_matched_to_phone_survey_ids_wave3[, wave := 3 ]

# Convert Abbott quantitative test to qualitative one
test_B_results_matched_to_phone_survey_ids_wave1[IgG_testB_quantitative >= 1.1, IgG_testB := 1]
test_B_results_matched_to_phone_survey_ids_wave1[IgG_testB_quantitative < 1.1, IgG_testB := 0]

test_B_results_matched_to_phone_survey_ids_wave15[IgG_testB_quantitative >= 1.1, IgG_testB := 1]
test_B_results_matched_to_phone_survey_ids_wave15[IgG_testB_quantitative < 1.1, IgG_testB := 0]

test_B_results_matched_to_phone_survey_ids_wave2[IgG_testB_quantitative >= 1.1, IgG_testB := 1]
test_B_results_matched_to_phone_survey_ids_wave2[IgG_testB_quantitative < 1.1, IgG_testB := 0]

# Convert Genetico quantitative test to qualitative one
test_C_results_matched_to_phone_survey_ids_wave1[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids_wave1[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

test_C_results_matched_to_phone_survey_ids_wave15[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids_wave15[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

test_C_results_matched_to_phone_survey_ids_wave2[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids_wave2[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

test_C_results_matched_to_phone_survey_ids_wave3[IgA_or_G_or_M_testC_quantitative >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids_wave3[IgA_or_G_or_M_testC_quantitative < 1, IgA_or_G_or_M_testC := 0]

# Convert Vector-best quantitative test to qualitative one
test_D_results_matched_to_phone_survey_ids_wave2[IgG_or_M_testD_quantitative >= 1.1, IgG_or_M_testD := 1]
test_D_results_matched_to_phone_survey_ids_wave2[IgG_or_M_testD_quantitative < 1.1, IgG_or_M_testD := 0]

test_D_results_matched_to_phone_survey_ids_wave3[IgG_or_M_testD_quantitative >= 1.1, IgG_or_M_testD := 1]
test_D_results_matched_to_phone_survey_ids_wave3[IgG_or_M_testD_quantitative < 1.1, IgG_or_M_testD := 0]

# Aggregate test results across waves
test_B_results_matched_to_phone_survey_ids_across_waves <- rbind(test_B_results_matched_to_phone_survey_ids_wave1, test_B_results_matched_to_phone_survey_ids_wave15, test_B_results_matched_to_phone_survey_ids_wave2, fill = T)

test_C_results_matched_to_phone_survey_ids_across_waves <- rbind(test_C_results_matched_to_phone_survey_ids_wave1, test_C_results_matched_to_phone_survey_ids_wave15, test_C_results_matched_to_phone_survey_ids_wave2, test_C_results_matched_to_phone_survey_ids_wave3, fill = T)

test_D_results_matched_to_phone_survey_ids_across_waves <- rbind(test_D_results_matched_to_phone_survey_ids_wave2, test_D_results_matched_to_phone_survey_ids_wave3, fill = T)

# Remove data on wave 1.5 for the moment
test_B_results_matched_to_phone_survey_ids_across_waves <- test_B_results_matched_to_phone_survey_ids_across_waves[ wave != 1.5 ]
test_C_results_matched_to_phone_survey_ids_across_waves <- test_C_results_matched_to_phone_survey_ids_across_waves[ wave != 1.5 ]
test_D_results_matched_to_phone_survey_ids_across_waves <- test_D_results_matched_to_phone_survey_ids_across_waves[ wave != 1.5 ]

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
load("estimates/wave3/phone_survey_raking_fit.rdata")
raking_weights <- data.table(ID = names(phone_survey_raking_fit$weightvec), raking_weight = phone_survey_raking_fit$weightvec)

############################
# Prepare data for model fitting

# Keep individuals that have been called up to that date
# in a separate object
serosurvey_data <- phone_survey_data_waves_1_2_3[called == 1]
serosurvey_data[, called := 1]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district since they are outside Saint Petersburg
serosurvey_data <- serosurvey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Do not remove individuals from districts in St. Petersburg that we have omitted from the first wave of the study
#serosurvey_data <- serosurvey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
serosurvey_data <- serosurvey_data[!is.na(district)]
serosurvey_data[, district := droplevels(district)]

# Add test results
## Test B
serosurvey_data <- merge(serosurvey_data, test_B_results_matched_to_phone_survey_ids_across_waves, by = c("ID", "wave"), all.x = T, all.y = F)
setnames(serosurvey_data, "draw_sample_date", "draw_sample_date_testB")

## Test C
serosurvey_data <- merge(serosurvey_data, test_C_results_matched_to_phone_survey_ids_across_waves, by = c("ID", "wave"), all.x = T, all.y = F)
setnames(serosurvey_data, "draw_sample_date", "draw_sample_date_testC")

# Debug (avoid repetitions): serosurvey_data[ ID == "00e9c8bf450c9bf7"]

## Test D
serosurvey_data <- merge(serosurvey_data, test_D_results_matched_to_phone_survey_ids_across_waves, by = c("ID", "wave"), all.x = T, all.y = F)
setnames(serosurvey_data, "draw_sample_date", "draw_sample_date_testD")

# Uniform test date
serosurvey_data[, draw_sample_date := as.Date(NA_character_)]
serosurvey_data[!is.na(draw_sample_date_testD), draw_sample_date := draw_sample_date_testD]
serosurvey_data[is.na(draw_sample_date_testD) & !is.na(draw_sample_date_testC), draw_sample_date := draw_sample_date_testC]
serosurvey_data[is.na(draw_sample_date_testD) & is.na(draw_sample_date_testC) & !is.na(draw_sample_date_testB) & wave != 3, draw_sample_date := draw_sample_date_testB]
serosurvey_data[, c("draw_sample_date_testB", "draw_sample_date_testC", "draw_sample_date_testD") := NULL ]

# Diagnose non-matching IDs from wave 3
# These are due to individuals from districts to remove
#test_D_results_matched_to_phone_survey_ids_across_waves[wave == 3][ !( ID %in% serosurvey_data[wave == 3]$ID )]

# Fix agreed variable for those who were actually tested in the third wave
# Examples: 
# serosurvey_data[wave == 3 & called == 1 & is.na(agreed) & !is.na(IgG_or_M_testD)]
serosurvey_data[ (agreed == 0 | is.na(agreed)) & (!is.na(IgG_or_M_testD) | !is.na(IgA_or_G_or_M_testC)), agreed := 1]

# Devise agreed and tested variable
serosurvey_data[, agreed_and_tested := 0]
serosurvey_data[agreed == 1 & (!is.na(IgG_or_M_testD) | !is.na(IgA_or_G_or_M_testC)), agreed_and_tested := 1]

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
# NB: we lose information on interview week for those who were called the second time
serosurvey_data[, interview_week := as.factor(isoweek(interview_date))]
serosurvey_data[interview_week == 21 & wave == 1, interview_week := "May 18-24, 2020"]
serosurvey_data[interview_week == 22 & wave == 1, interview_week := "May 25-31, 2020"]
serosurvey_data[interview_week == 23 & wave == 1, interview_week := "June 1-7, 2020"]
serosurvey_data[interview_week == 24 & wave == 1, interview_week := "June 8-14, 2020"]
serosurvey_data[interview_week == 25 & wave == 1, interview_week := "June 15-21, 2020"]
serosurvey_data[interview_week == 26 & wave == 1, interview_week := "June 22-28, 2020"]
serosurvey_data[interview_week == 41 & wave == 2, interview_week := "October 5-11, 2020"]
serosurvey_data[interview_week == 42 & wave == 2, interview_week := "October 12-18, 2020"]
serosurvey_data[interview_week == 43 & wave == 2, interview_week := "October 19-25, 2020"]
serosurvey_data[interview_week == 44 & wave == 2, interview_week := "October 26 - November 1, 2020"]
serosurvey_data[interview_week == 45 & wave == 2, interview_week := "November 2-8, 2020"]
serosurvey_data[interview_week == 46 & wave == 2, interview_week := "November 9-15, 2020"]
serosurvey_data[interview_week == 47 & wave == 2, interview_week := "November 16-22, 2020"]
serosurvey_data[interview_week == 48 & wave == 2, interview_week := "November 23-29, 2020"]
serosurvey_data[interview_week == 49 & wave == 2, interview_week := "November 30 - December 6, 2020"]
serosurvey_data[interview_week == 6 & wave == 3, interview_week := "February 8-14, 2021"]
serosurvey_data[interview_week == 7 & wave == 3, interview_week := "February 15-21, 2021"]
serosurvey_data[interview_week == 8 & wave == 3, interview_week := "February 22-28, 2021"]
serosurvey_data[interview_week == 9 & wave == 3, interview_week := "March 1-7, 2021"]
serosurvey_data[interview_week == 10 & wave == 3, interview_week := "March 8-14, 2021"]
serosurvey_data[interview_week == 11 & wave == 3, interview_week := "March 15-21, 2021"]
serosurvey_data[interview_week == 12 & wave == 3, interview_week := "March 22-28, 2021"]
serosurvey_data[interview_week == 13 & wave == 3, interview_week := "March 29 - April 4, 2021"]
serosurvey_data[interview_week == 14 & wave == 3, interview_week := "April 5-11, 2021"]

serosurvey_data[, interview_week := factor(interview_week, levels = c("May 18-24, 2020", "May 25-31, 2020", "June 1-7, 2020", "June 8-14, 2020", "June 15-21, 2020", "June 22-28, 2020", "October 5-11, 2020", "October 12-18, 2020", "October 19-25, 2020", "October 26 - November 1, 2020", "November 2-8, 2020", "November 9-15, 2020", "November 16-22, 2020", "November 23-29, 2020", "November 30 - December 6, 2020", "February 8-14, 2021", "February 15-21, 2021", "February 22-28, 2021", "March 1-7, 2021", "March 8-14, 2021", "March 15-21, 2021", "March 22-28, 2021", "March 29 - April 4, 2021", "April 5-11, 2021"))]

# Sample draw week of year
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]
serosurvey_data[draw_week == 21 & wave == 1, draw_week := "May 18-24, 2020"]
serosurvey_data[draw_week == 22 & wave == 1, draw_week := "May 25-31, 2020"]
serosurvey_data[draw_week == 23 & wave == 1, draw_week := "June 1-7, 2020"]
serosurvey_data[draw_week == 24 & wave == 1, draw_week := "June 8-14, 2020"]
serosurvey_data[draw_week == 25 & wave == 1, draw_week := "June 15-21, 2020"]
serosurvey_data[draw_week == 26 & wave == 1, draw_week := "June 22-28, 2020"]
serosurvey_data[draw_week == 29 & wave == 1.5, draw_week := "July 13-19, 2020"]
serosurvey_data[draw_week == 30 & wave == 1.5, draw_week := "July 20-26, 2020"]
serosurvey_data[draw_week == 31 & wave == 1.5, draw_week := "July 27 - August 2, 2020"]
serosurvey_data[draw_week == 32 & wave == 1.5, draw_week := "August 3-9, 2020"]
serosurvey_data[draw_week == 33 & wave == 1.5, draw_week := "August 10-16, 2020"]
serosurvey_data[draw_week == 41 & wave == 2, draw_week := "October 5-11, 2020"]
serosurvey_data[draw_week == 42 & wave == 2, draw_week := "October 12-18, 2020"]
serosurvey_data[draw_week == 43 & wave == 2, draw_week := "October 19-25, 2020"]
serosurvey_data[draw_week == 44 & wave == 2, draw_week := "October 26 - November 1, 2020"]
serosurvey_data[draw_week == 45 & wave == 2, draw_week := "November 2-8, 2020"]
serosurvey_data[draw_week == 46 & wave == 2, draw_week := "November 9-15, 2020"]
serosurvey_data[draw_week == 47 & wave == 2, draw_week := "November 16-22, 2020"]
serosurvey_data[draw_week == 48 & wave == 2, draw_week := "November 23-29, 2020"]
serosurvey_data[draw_week == 49 & wave == 2, draw_week := "November 30 - December 6, 2020"]
serosurvey_data[draw_week == 6 & wave == 3, draw_week := "February 8-14, 2021"]
serosurvey_data[draw_week == 7 & wave == 3, draw_week := "February 15-21, 2021"]
serosurvey_data[draw_week == 8 & wave == 3, draw_week := "February 22-28, 2021"]
serosurvey_data[draw_week == 9 & wave == 3, draw_week := "March 1-7, 2021"]
serosurvey_data[draw_week == 10 & wave == 3, draw_week := "March 8-14, 2021"]
serosurvey_data[draw_week == 11 & wave == 3, draw_week := "March 15-21, 2021"]
serosurvey_data[draw_week == 12 & wave == 3, draw_week := "March 22-28, 2021"]
serosurvey_data[draw_week == 13 & wave == 3, draw_week := "March 29 - April 4, 2021"]
serosurvey_data[draw_week == 14 & wave == 3, draw_week := "April 5-11, 2021"]

serosurvey_data[, draw_week := factor(draw_week, levels = c("May 18-24, 2020", "May 25-31, 2020", "June 1-7, 2020", "June 8-14, 2020", "June 15-21, 2020", "June 22-28, 2020", "July 13-19, 2020", "July 20-26, 2020", "July 27 - August 2, 2020", "August 3-9, 2020", "August 10-16, 2020", "October 5-11, 2020", "October 12-18, 2020", "October 19-25, 2020", "October 26 - November 1, 2020", "November 2-8, 2020", "November 9-15, 2020", "November 16-22, 2020", "November 23-29, 2020", "November 30 - December 6, 2020", "February 8-14, 2021", "February 15-21, 2021", "February 22-28, 2021", "March 1-7, 2021", "March 8-14, 2021", "March 15-21, 2021", "March 22-28, 2021", "March 29 - April 4, 2021", "April 5-11, 2021"))]

# Sample restrictions
# First and foremost, remove all data from the summer 2020 wave 1.5
serosurvey_data <- serosurvey_data[ wave != 1.5 ]

# Important assumption:
# Assume that vaccinated individuals were not tested
serosurvey_data[wave == 3 & was_vaccinated == 1, agreed_and_tested := 0 ]

# Another assumption (inactive):
# Individuals positive in previous waves are tested and positive in wave 3
#positive_previous_waves_testC <- unique(serosurvey_data[wave < 3 & IgA_or_G_or_M_testC == 1]$ID)
#positive_previous_waves_testD <- unique(serosurvey_data[wave < 3 & IgG_or_M_testD == 1]$ID)
#serosurvey_data[wave == 3 & ID %in% positive_previous_waves_testC, c("agreed_and_tested", "IgA_or_G_or_M_testC") := list(1, 1)] 
#serosurvey_data[wave == 3 & ID %in% positive_previous_waves_testD, c("agreed_and_tested", "IgG_or_M_testD") := list(1, 1)] 

# !! Important sample restriction: keep all individuals from wave 3 (i.e. agreed and tested in previous waves and called during wave 3)
# and all untested individuals from previous waves 1 and 2
serosurvey_data <- rbind(serosurvey_data[ wave == 3 ], serosurvey_data[ wave != 3 & agreed_and_tested == 0 ])

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

# Keep only districts with enough tested individuals
districts_to_keep <- serosurvey_data[agreed_and_tested == 1, .N, by = "district"][ N >= 5]$district
serosurvey_data <- serosurvey_data[ district %in% districts_to_keep]
serosurvey_data[, district := droplevels(district)]

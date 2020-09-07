library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(margins)
library(ggplot2)
library(ggthemes)
library(ggeffects)
library(ggrepel)

setwd("~")

# Load phone survey data
# created by code/prepare_phone_survey_data/prepare_phone_survey_data.r
load("data/phone_survey/phone_survey_data.rdata")

# Load Sugentech results, created by extraneous code
load("data/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata")

# Load Abbott results, created by extraneous code
load("data/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")

# Load Genetico results, created by extraneous code
load("data/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata")

# Convert Genetico quantitative test to qualitative one
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC < 1, IgA_or_G_or_M_testC := 0]

# Load a function that estimates the
# bivariate probit model and outputs the results
source("code/analysis/helper_functions/estimate_bivariate_selection.r")

# Load a function that adjusts the prevalence
# estimates for sensitivity and specificity
# in classical way
source("code/analysis/helper_functions/adjust_prev_test_chars.r")


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
serosurvey_data <- merge(serosurvey_data, test_B_results_matched_to_phone_survey_ids[, c("ID", "IgG_testB", "draw_sample_date")], by = "ID", all.x = T, all.y = F)

## Test C
serosurvey_data <- merge(serosurvey_data, test_C_results_matched_to_phone_survey_ids[, c("ID", "IgA_or_G_or_M_testC")], by = "ID", all.x = T, all.y = F)

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

serosurvey_data[interview_week == 21, interview_week := "May 18-24"]
serosurvey_data[interview_week == 22, interview_week := "May 25-31"]
serosurvey_data[interview_week == 23, interview_week := "June 1-7"]
serosurvey_data[interview_week == 24, interview_week := "June 8-14"]
serosurvey_data[interview_week == 25, interview_week := "June 15-21"]
serosurvey_data[interview_week == 26, interview_week := "June 22-28"]
serosurvey_data[, interview_week := factor(interview_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

# Sample draw week of year 
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]
serosurvey_data[draw_week == 21, draw_week := "May 18-24"]
serosurvey_data[draw_week == 22, draw_week := "May 25-31"]
serosurvey_data[draw_week == 23, draw_week := "June 1-7"]
serosurvey_data[draw_week == 24, draw_week := "June 8-14"]
serosurvey_data[draw_week == 25, draw_week := "June 15-21"]
serosurvey_data[draw_week == 26, draw_week := "June 22-28"]
serosurvey_data[, draw_week := factor(draw_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

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

###################
# Define regressors list
exhaustive_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + wears_mask + washing_hands_more + was_sick + selftested_covid + lives_alone + russian_citizen + goodhealth + self_medical_checkups + visited + travelled_abroad + follows_news +  + work_status + manages_people + offered_taxi + district + interview_week"

# Fit the models
agreed_fit <- glm(paste0("agreed ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))
agreed_and_tested_fit <- glm(paste0("agreed_and_tested ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))
IgG_testB_fit <- glm(paste0("IgG_testB ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))

# Adjusted predicted values
agreed_adjusted_predictions <- ggeffect(agreed_fit)
agreed_adjusted_predictions_dt <- rbindlist(agreed_adjusted_predictions, id = NULL)
agreed_adjusted_predictions_dt[, index := 1:.N]

agreed_and_tested_adjusted_predictions <- ggeffect(agreed_and_tested_fit)
agreed_and_tested_adjusted_predictions_dt <- rbindlist(agreed_and_tested_adjusted_predictions, id = NULL)
agreed_and_tested_adjusted_predictions_dt[, index := 1:.N]

IgG_testB_adjusted_predictions <- ggeffect(IgG_testB_fit)
IgG_testB_adjusted_predictions_dt <- rbindlist(IgG_testB_adjusted_predictions, id = NULL)
IgG_testB_adjusted_predictions_dt[, index := 1:.N]

# Organise labels
agreed_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
agreed_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
agreed_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
agreed_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
agreed_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
agreed_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
agreed_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
agreed_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
agreed_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
agreed_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
agreed_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
agreed_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
agreed_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
agreed_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
agreed_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
agreed_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
agreed_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
agreed_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
agreed_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
agreed_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
agreed_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
agreed_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
agreed_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
agreed_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
agreed_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
agreed_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
agreed_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
agreed_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
agreed_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
agreed_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
agreed_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
agreed_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
agreed_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
agreed_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
agreed_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
agreed_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
agreed_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
agreed_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
agreed_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]

agreed_and_tested_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
agreed_and_tested_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
agreed_and_tested_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
agreed_and_tested_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
agreed_and_tested_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
agreed_and_tested_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
agreed_and_tested_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
agreed_and_tested_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
agreed_and_tested_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
agreed_and_tested_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
agreed_and_tested_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
agreed_and_tested_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
agreed_and_tested_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
agreed_and_tested_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
agreed_and_tested_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
agreed_and_tested_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
agreed_and_tested_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
agreed_and_tested_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
agreed_and_tested_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
agreed_and_tested_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
agreed_and_tested_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
agreed_and_tested_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
agreed_and_tested_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
agreed_and_tested_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
agreed_and_tested_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
agreed_and_tested_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
agreed_and_tested_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
agreed_and_tested_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
agreed_and_tested_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
agreed_and_tested_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
agreed_and_tested_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
agreed_and_tested_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
agreed_and_tested_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
agreed_and_tested_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
agreed_and_tested_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
agreed_and_tested_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
agreed_and_tested_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
agreed_and_tested_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
agreed_and_tested_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]

IgG_testB_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
IgG_testB_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
IgG_testB_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
IgG_testB_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
IgG_testB_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
IgG_testB_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
IgG_testB_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
IgG_testB_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
IgG_testB_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
IgG_testB_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
IgG_testB_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
IgG_testB_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
IgG_testB_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
IgG_testB_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
IgG_testB_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
IgG_testB_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
IgG_testB_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
IgG_testB_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
IgG_testB_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
IgG_testB_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
IgG_testB_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
IgG_testB_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
IgG_testB_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
IgG_testB_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
IgG_testB_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
IgG_testB_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
IgG_testB_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
IgG_testB_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
IgG_testB_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
IgG_testB_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
IgG_testB_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
IgG_testB_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
IgG_testB_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
IgG_testB_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
IgG_testB_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
IgG_testB_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
IgG_testB_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
IgG_testB_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
IgG_testB_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]


# Remove week of call and district dummies
agreed_adjusted_predictions_dt <- agreed_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]
agreed_and_tested_adjusted_predictions_dt <- agreed_and_tested_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]
IgG_testB_adjusted_predictions_dt <- IgG_testB_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]

# Plot the results
## Agreed
agreed_adjusted_predictions_plot <- ggplot(agreed_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.04) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$agreed, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to agree to partipate in phone survey", limits = c(0.4, max(agreed_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

ggsave("media/agreed_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)


## Agreed and tested
agreed_and_tested_adjusted_predictions_plot <- ggplot(agreed_and_tested_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.03) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$agreed_and_tested, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to volunteer to give blood sample", limits = c(0, max(agreed_and_tested_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

ggsave("media/agreed_and_tested_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)



## IgG-positive (Abbott)
IgG_testB_adjusted_predictions_plot <- ggplot(IgG_testB_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.05) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$IgG_testB, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to be CMIA-positive", limits = c(0, max(IgG_testB_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

ggsave("media/IgG_testB_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)



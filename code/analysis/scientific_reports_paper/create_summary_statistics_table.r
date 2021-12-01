# This code creates the summary   
# statistics tables for
# Seroprevalence of SARS-CoV-2 antibodies in Saint
# Petersburg, Russia paper (https://doi.org/10.1038/s41598-021-92206-y)
library(data.table)
library(stringi)
library(stringr)
library(stargazer)
library(fastDummies)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

#######
# Load and prepare the serosurvey data for wave 1
source("code/analysis/wave1/create_serosurvey_data_for_model_fit.r")

# Labels for dates
serosurvey_data[interview_week == 21, interview_week := "May 18-24"]
serosurvey_data[interview_week == 22, interview_week := "May 25-31"]
serosurvey_data[interview_week == 23, interview_week := "June 1-7"]
serosurvey_data[interview_week == 24, interview_week := "June 8-14"]
serosurvey_data[interview_week == 25, interview_week := "June 15-21"]
serosurvey_data[interview_week == 26, interview_week := "June 22-28"]
serosurvey_data[, interview_week := factor(interview_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

serosurvey_data[draw_week == 21, draw_week := "May 18-24"]
serosurvey_data[draw_week == 22, draw_week := "May 25-31"]
serosurvey_data[draw_week == 23, draw_week := "June 1-7"]
serosurvey_data[draw_week == 24, draw_week := "June 8-14"]
serosurvey_data[draw_week == 25, draw_week := "June 15-21"]
serosurvey_data[draw_week == 26, draw_week := "June 22-28"]
serosurvey_data[, draw_week := factor(draw_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

# Add paper survey data
load("data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")
serosurvey_data <- merge(serosurvey_data, paper_survey_data_matched_to_phone_survey_ids, by = "ID", all.x = T, all.y = F)

# Consumes alcohol variable
serosurvey_data[, consumes_alcohol := NA_character_]
serosurvey_data[!is.na(alcohol_frequency), consumes_alcohol := "3Weekly or more often"]
serosurvey_data[alcohol_frequency %in% c("Once over last 30 days") | alcohol_frequency %in% c("2-3 times over last 30 days"), consumes_alcohol := '2Monthly']
serosurvey_data[alcohol_frequency %in% c("Never"), consumes_alcohol := "1Never"]

# Smoking now or earlier variable
serosurvey_data[, smoking := NA_character_]
serosurvey_data[smoking_now == 1, smoking := "3Smoking now"]
serosurvey_data[smoking_now == 0 & smoking_earlier==1, smoking := "2Smoking earlier"]
serosurvey_data[smoking_now == 0 & smoking_earlier==0, smoking := "1Never smoked"]
serosurvey_data[, smoking := factor(smoking, ordered = F) ]

# Chronic disease or medication variable
serosurvey_data[, chronic := NA_real_]
serosurvey_data[!is.na(chronic_diabetes) | !is.na(if_diabetes_insulin) | !is.na(chronic_another_lung_disease) | !is.na(chronic_asthma) | !is.na(asthma_treatment_nebulizer) | !is.na(asthma_treatment_oral_medication) | !is.na(asthma_treatment_hand_inhalers) | !is.na(asthma_treatment_nothing) | !is.na(chronic_renal_failure) | !is.na(chronic_liver_diseases) | !is.na(chronic_cardiovascular_diseases) | !is.na(chronic_hematologic_disorders) | !is.na(chronic_other_diseases) | !is.na(if_other_diseases_which) | !is.na(which_treatment) | !is.na(other_drugs) | !is.na(if_other_drugs_which) | !is.na(oncology) | !is.na(if_oncology_treatment), chronic := 0]
serosurvey_data[chronic_diabetes ==1| if_diabetes_insulin == 1| chronic_another_lung_disease == 1|  chronic_asthma ==1| asthma_treatment_nebulizer ==1| asthma_treatment_oral_medication ==1|  asthma_treatment_hand_inhalers ==1| asthma_treatment_nothing ==1|  chronic_renal_failure ==1| chronic_liver_diseases ==1| chronic_cardiovascular_diseases ==1| chronic_hematologic_disorders ==1| chronic_other_diseases ==1| oncology ==1, chronic := 1]

# BMI
serosurvey_data[, bmi := weight/(height/100)^2]

# BMI_levels
serosurvey_data[, bmi_level := NA_character_]
serosurvey_data[bmi < 18.5, bmi_level := "Underweight"]
serosurvey_data[bmi >= 18.5 & bmi < 25, bmi_level := "1Normal"]
serosurvey_data[bmi >= 25 & bmi < 30, bmi_level := "Overweight"]
serosurvey_data[bmi >= 30, bmi_level := "Obese"]
serosurvey_data[, bmi_level := as.factor(bmi_level)]

# Define regressors to include in the table
summary_stat_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + washing_hands_more + was_sick + travelled_abroad + selftested_covid + district + interview_week + draw_week + consumes_alcohol + smoking + chronic + allergy + bmi_level + offered_taxi + IgG_testB + IgA_or_G_or_M_testC"
summary_stat_regressors_list <- str_trim(unlist(strsplit(summary_stat_regressors, "+", fixed = T)))

# Create a new object with selected variabes
# and expand their dummies
data_for_summary_stat <- serosurvey_data[,summary_stat_regressors_list, with = F]
data_for_summary_stat <- dummy_cols(data_for_summary_stat, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

# Produce a table with summary statistics
#stargazer(data_for_summary_stat, type = "latex", summary = T, out = "estimates/wave1/summary_statistics.tex", summary.stat = c("n", "mean", "sd"), float = T, multicolumn = F)

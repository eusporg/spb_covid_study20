# This code estimates the individual-level  
# risk-ratios for seroconversion for
# Seroprevalence of SARS-CoV-2 antibodies in Saint
# Petersburg, Russia paper (https://doi.org/10.1038/s41598-021-92206-y)
library(data.table)
library(stringi)
library(stringr)
library(stargazer)
library(Greg)
library(xtable)
library(Hmisc)
library(tableone)
library(kableExtra)
library(labelled)
library(glm)
library(sandwich)
library(lmtest)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load and prepare the serosurvey data for model fit
source("code/analysis/wave1/create_serosurvey_data_for_model_fit.r")

# Load paper survey data from this study
load("data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")

# Add paper survey data
serosurvey_data <- merge(serosurvey_data, paper_survey_data_matched_to_phone_survey_ids, by = "ID", all.x = T, all.y = F)

# Alcohol variable
serosurvey_data[, consumes_alcohol := NA_character_]
serosurvey_data[!is.na(alcohol_frequency), consumes_alcohol := "3Weekly or more often"]
serosurvey_data[alcohol_frequency %in% c("Once over last 30 days") | alcohol_frequency %in% c("2-3 times over last 30 days"), 
                consumes_alcohol := '2Monthly']
serosurvey_data[alcohol_frequency %in% c("Never"), consumes_alcohol := "1Never"]
serosurvey_data[, consumes_alcohol := factor(consumes_alcohol, ordered = F) ]

# Smoking now or earlier variable
serosurvey_data[, smoking := NA_character_]
serosurvey_data[smoking_now == 1, smoking := "3Smoking now"]
serosurvey_data[smoking_now == 0 & smoking_earlier==1, smoking := "2Smoking earlier"]
serosurvey_data[smoking_now == 0 & smoking_earlier==0, smoking := "1Never smoked"]
serosurvey_data[, smoking := factor(smoking, ordered = F) ]

# Chronic disease or medication variable
serosurvey_data[, chronic := NA_real_]
serosurvey_data[!is.na(chronic_diabetes)|!is.na(if_diabetes_insulin)|!is.na(chronic_another_lung_disease)|!is.na( chronic_asthma)|
                  !is.na(asthma_treatment_nebulizer)|
                  !is.na(asthma_treatment_oral_medication)|
                  !is.na( asthma_treatment_hand_inhalers)|!is.na(asthma_treatment_nothing)|
                  !is.na(chronic_renal_failure)|!is.na(chronic_liver_diseases)|!is.na(chronic_cardiovascular_diseases)|
                  !is.na(chronic_hematologic_disorders)|!is.na(chronic_other_diseases)|!is.na(if_other_diseases_which)|
                  !is.na( which_treatment)|!is.na(other_drugs)|!is.na(if_other_drugs_which)|
                  !is.na(oncology)|!is.na(if_oncology_treatment), chronic := 0]
serosurvey_data[chronic_diabetes ==1| if_diabetes_insulin == 1| chronic_another_lung_disease == 1|  chronic_asthma ==1|
                  asthma_treatment_nebulizer ==1| asthma_treatment_oral_medication ==1|  asthma_treatment_hand_inhalers ==1| 
                  asthma_treatment_nothing ==1|  chronic_renal_failure ==1| chronic_liver_diseases ==1| chronic_cardiovascular_diseases ==1| 
                  chronic_hematologic_disorders ==1| chronic_other_diseases ==1| oncology ==1, chronic := 1]

# BMI
serosurvey_data$bmi <- as.numeric(serosurvey_data$weight/(serosurvey_data$height/100)^2)

# Define basic regressors
basic_regressors <- "+agegroup + male + highereduc + higherincome + lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"

# Regressors from paper-based survey
papersurvey_regressors <- "+smoking + consumes_alcohol + chronic + allergy + cold_symptoms"

## Log-linear models

# CMIA
out.basic.IgG_testB <- glm(formula = as.formula(paste0("IgG_testB ~ ", basic_regressors, papersurvey_regressors)), data = serosurvey_data, family = poisson(link="log"))

# ELISA
out.basic.IgA_or_G_or_M_testC <- glm(formula = as.formula(paste0("IgA_or_G_or_M_testC ~ ", basic_regressors,papersurvey_regressors)), data = serosurvey_data, family = poisson(link="log"))

# Huber-Eicker-White-adjust variance-covariance-estimator
vcov_sandwich_out.basic.IgG_testB <- sandwich(out.basic.IgG_testB, type = "HC1")
vcov_sandwich_out.basic.IgA_or_G_or_M_testC <- sandwich(out.basic.IgA_or_G_or_M_testC, type = "HC1")

# Compare the results with and without the Huber-Eicker-White adjusted variance-covariance-matrix
## IgG test B, without adjustment
coefci(out.basic.IgG_testB)

## IgG test B, with adjustment
coefci(out.basic.IgG_testB, vcov = vcov_sandwich_out.basic.IgG_testB)

## IgA/G/M test C, without adjustment
coefci(out.basic.IgA_or_G_or_M_testC)

## IgA/G/M test C, without adjustment
coefci(out.basic.IgA_or_G_or_M_testC, vcov = vcov_sandwich_out.basic.IgA_or_G_or_M_testC)

# Labelling variables for output table
Hmisc::label(serosurvey_data$allergy) <- "Past history of allergies"
Hmisc::label(serosurvey_data$chronic) <- "Chronic diseases or medication use"
Hmisc::label(serosurvey_data$smoking) <- "Smoking status"
Hmisc::label(serosurvey_data$highereduc) <- "Higher education"
Hmisc::label(serosurvey_data$higherincome) <- "Higher income"
Hmisc::label(serosurvey_data$consumes_alcohol) <- "Alcohol consumption frequency"
Hmisc::label(serosurvey_data$cold_symptoms) <- "Cold symptoms in the last 3 months"
Hmisc::label(serosurvey_data$lives_alone) <- "Respondent lives alone"
Hmisc::label(serosurvey_data$agegroup) <- "Age group"
Hmisc::label(serosurvey_data$male) <- "Male"
Hmisc::label(serosurvey_data$washing_hands_more) <- "Respondent started to wash hands more often"
Hmisc::label(serosurvey_data$selftested_covid) <-"History of COVID-19 testing"
Hmisc::label(serosurvey_data$travelled_abroad) <-"Respondent travelled abroad in the last 3 months"

# Printing and binding crude and adjusted prevalence ratio from loglinear models
print_CMIA_PR_table <- printCrudeAndAdjustedModel(out.basic.IgG_testB,digits = 2,add_references = TRUE, sprintf_ci_str = "(%s-%s)", reference_zero_effect=1)

print_ELISA_PR_table <- printCrudeAndAdjustedModel(out.basic.IgA_or_G_or_M_testC, digits = 2, add_references = TRUE, sprintf_ci_str = "(%s-%s)", reference_zero_effect=1)

# Print LaTex table for prevalence ratios
xtable(cbind("CMIA" = print_CMIA_PR_table, "ELISA" = print_ELISA_PR_table))

## Summary statistics table
# Selecting variables for summary statistics table
vars= c( "agegroup", "male", "highereduc", "higherincome", "lives_alone","travelled_abroad",
         "washing_hands_more", "was_sick", "selftested_covid","district","interview_week","offered_taxi",
         "IgG_testB","IgA_or_G_or_M_testC",
         "smoking","allergy","chronic","cold_symptoms","consumes_alcohol")

# Labelling variables for output tabbles
labelled::var_label(serosurvey_data$allergy) <- "Past history of allergies"
labelled::var_label(serosurvey_data$chronic) <- "Chronic diseases or medication use"
labelled::var_label(serosurvey_data$smoking) <- "Smoking status"
labelled::var_label(serosurvey_data$highereduc) <- "Higher education"
labelled::var_label(serosurvey_data$higherincome) <- "Higher income"
labelled::var_label(serosurvey_data$consumes_alcohol) <- "Alcohol consumption frequency"
labelled::var_label(serosurvey_data$cold_symptoms) <- "Cold symptoms in the last 3 months"
labelled::var_label(serosurvey_data$lives_alone) <- "Respondent lives alone"
labelled::var_label(serosurvey_data$agegroup) <- "Age group"
labelled::var_label(serosurvey_data$male) <- "Male"
labelled::var_label(serosurvey_data$washing_hands_more) <- "Respondent started to wash hands more often"
labelled::var_label(serosurvey_data$selftested_covid) <-"History of COVID-19 testing"
labelled::var_label(serosurvey_data$travelled_abroad) <-"Respondent travelled abroad in the last 3 months"

# Creating and printing summary statistics table 
summary_statistics_xtable<-xtable(print(CreateTableOne(data = serosurvey_data, vars=vars, strata = "agreed_and_tested",addOverall = TRUE, factorVars = vars,includeNA=TRUE),
      varLabels = TRUE,dropEqual = TRUE, test = FALSE,printToggle = FALSE,showAllLevels = TRUE))

print.xtable(summary_statistics_xtable, sanitize.colnames.function=function(x)gsub("\\."," ",x), sanitize.rownames.function=function(x)gsub("\\."," ",x))

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

setwd("~")

# Load phone survey data
# created by code/prepare_phone_survey_data/prepare_phone_survey_data.r
load("phone_survey_data.rdata")

# Load paper survey data from this study
load("paper_survey_data_matched_to_phone_survey_ids.rdata")

# Load Sugentech results, created by extraneous code
load("test_A_results_matched_to_phone_survey_ids.rdata")

# Load Abbott results, created by extraneous code
load("test_B_results_matched_to_phone_survey_ids.rdata")

# Load Genetico results, created by extraneous code
load("test_C_results_matched_to_phone_survey_ids.rdata")

# Convert Genetico quantitative test to qualitative one
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC >= 1, IgA_or_G_or_M_testC := 1]
test_C_results_matched_to_phone_survey_ids[IgA_or_G_or_M_testC < 1, IgA_or_G_or_M_testC := 0]

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
serosurvey_data <- serosurvey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", 
                                                     "Pushkinskiy District", "Kolpinskiy District",
                                                     "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
serosurvey_data <- serosurvey_data[!is.na(district)]
serosurvey_data[, district := droplevels(district)]

# Add test results
## Test A
serosurvey_data <- merge(serosurvey_data, 
                         test_A_results_matched_to_phone_survey_ids[, c("ID", "IgM_testA",
                                                                        "IgG_testA")], by = "ID", all.x = T, all.y = F)

## Test B
serosurvey_data <- merge(serosurvey_data, 
                         test_B_results_matched_to_phone_survey_ids[, c("ID", "IgG_testB", "draw_sample_date")], by = "ID", all.x = T, all.y = F)

## Test C
serosurvey_data <- merge(serosurvey_data, 
                         test_C_results_matched_to_phone_survey_ids[, c("ID","IgA_or_G_or_M_testC")], by = "ID", all.x = T, all.y = F)

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
serosurvey_data[, highereduc := as.factor(highereduc)]


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
serosurvey_data[!is.na(visited_work ) & !is.na(visited_transport) & !is.na(visited_pharmacy) & 
                  !is.na(visited_market_or_shop) & !is.na(visited_facilities) , visited := 0]
serosurvey_data[visited_work==1 | visited_transport==1 | visited_pharmacy==1 | visited_market_or_shop==1 | 
                  visited_facilities==1 , visited := 1]

# Interview week of year
serosurvey_data[, interview_week := as.factor(isoweek(interview_date))]

# Sample draw week of year 
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]

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

#CMIA
out.basic.IgG_testB <- glm(formula = as.formula(paste0("IgG_testB ~ ", basic_regressors, papersurvey_regressors)), data = serosurvey_data, amily=poisson(link="log"))

#ELISA
out.basic.IgA_or_G_or_M_testC <- glm(formula = as.formula(paste0("IgA_or_G_or_M_testC ~ ", basic_regressors,papersurvey_regressors)), data = serosurvey_data, amily=poisson(link="log"))

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

#Printing and binding crude and adjusted prevalence ratio from loglinear models

print_CMIA_PR_table<-printCrudeAndAdjustedModel(out.basic.IgG_testB,digits = 2,add_references = TRUE, sprintf_ci_str = "(%s-%s)", reference_zero_effect=1)

print_ELISA_PR_table<<-printCrudeAndAdjustedModel(out.basic.IgA_or_G_or_M_testC,digits = 2, add_references = TRUE,sprintf_ci_str = "(%s-%s)",reference_zero_effect=1)

#Print LaTex table for prevalence ratios
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


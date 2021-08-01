library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(anesrake)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load Сomprehensive monitoring of living
# conditions 2018 data for Saint Petersburg
load("data/kouzh_2018/kouzh_2018_data_spb.rdata")
kouzh_2018_data_spb[, male := NA_real_]
kouzh_2018_data_spb[sex == 1, male := 1]
kouzh_2018_data_spb[sex == 2, male := 0]

# Keep only adults in KOUZh data
kouzh_2018_data_spb <- kouzh_2018_data_spb[ age >= 18 ]

# Load phone survey data from this study
load("data/wave2/phone_survey/phone_survey_data.rdata")

# Load test results from the second wave
# to mark invididuals that were tested
load("data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata")

# Devise agreed and tested variable
phone_survey_data[, agreed_and_tested := 0]
phone_survey_data[agreed == 1 & ID %in% other_test_results_matched_to_phone_survey_ids$ID, agreed_and_tested := 1]

# Test results data contains all individuals that were contacted as of this date 
#initial_phone_call_last_date <- "2020-11-01"
#phone_survey_data <- phone_survey_data[interview_date <= initial_phone_call_last_date]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district even though they are outside Saint Petersburg
phone_survey_data <- phone_survey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Do not remove individuals from districts in St. Petersburg that we have omitted from the study
#phone_survey_data <- phone_survey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
phone_survey_data <- phone_survey_data[!is.na(district)]

# Define age groups
phone_survey_data[, agegroup := NA_character_]
phone_survey_data[age < 35, agegroup := "18-34"]
phone_survey_data[age >= 35 & age < 50, agegroup := "35-49"]
phone_survey_data[age >= 50 & age < 65, agegroup := "50-64"]
phone_survey_data[age >= 65, agegroup := "65+"]
phone_survey_data[, agegroup := as.factor(agegroup)]

kouzh_2018_data_spb[, agegroup := NA_character_]
kouzh_2018_data_spb[age < 35, agegroup := "18-34"]
kouzh_2018_data_spb[age >= 35 & age < 50, agegroup := "35-49"]
kouzh_2018_data_spb[age >= 50 & age < 65, agegroup := "50-64"]
kouzh_2018_data_spb[age >= 65, agegroup := "65+"]
kouzh_2018_data_spb[, agegroup := as.factor(agegroup)]

# Lives alone variable
kouzh_2018_data_spb[, lives_alone := 0]
kouzh_2018_data_spb[household_size == 1, lives_alone := 1]

# Recode education in KOUZh into 3 groups
kouzh_2018_data_spb[, education_level := as.character(education_level)]
kouzh_2018_data_spb[education_level %in% c("Primary education", "Complete secondary education"), education_level := "Primary / secondary education"]
kouzh_2018_data_spb[, education_level := factor(education_level, levels = c("Primary / secondary education", "Special secondary education", "Higher education"), ordered = T)]

# Rename smoking now variable
setnames(kouzh_2018_data_spb, "smoking_now", "smoking")

# Numeric variables to factors for compatibility with
# anesrake
to_factor <- c("male", "lives_alone", "smoking")
kouzh_2018_data_spb[, c(to_factor) := lapply(.SD, as.factor), .SDcols = to_factor]
phone_survey_data[, c(to_factor) := lapply(.SD, as.factor), .SDcols = to_factor]

# Define raking variables
raking_variables <- c("agegroup", "male", "lives_alone", "education_level", "health_level", "smoking")

# Generate target proportions
agegroup_proportions <- kouzh_2018_data_spb[!is.na(agegroup), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(agegroup)])), by = "agegroup"]
agegroup_proportions_list <- agegroup_proportions$prop
names(agegroup_proportions_list) <- agegroup_proportions$agegroup

male_proportions <- kouzh_2018_data_spb[!is.na(male), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(male)])), by = "male"]
male_proportions_list <- male_proportions$prop
names(male_proportions_list) <- male_proportions$male
# Zero comes first
male_proportions_list <- rev(male_proportions_list)

lives_alone_proportions <- kouzh_2018_data_spb[!is.na(lives_alone), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(lives_alone)])), by = "lives_alone"]
lives_alone_proportions_list <- lives_alone_proportions$prop
names(lives_alone_proportions_list) <- lives_alone_proportions$lives_alone
lives_alone_proportions_list <- rev(lives_alone_proportions_list)

education_level_proportions <- kouzh_2018_data_spb[!is.na(education_level), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(education_level)])), by = "education_level"]
education_level_proportions_list <- education_level_proportions$prop
names(education_level_proportions_list) <- education_level_proportions$education_level

health_level_proportions <- kouzh_2018_data_spb[!is.na(health_level), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(health_level)])), by = "health_level"]
health_level_proportions_list <- health_level_proportions$prop
names(health_level_proportions_list) <- health_level_proportions$health_level

smoking_proportions <- kouzh_2018_data_spb[!is.na(smoking), list(prop = .N/nrow(kouzh_2018_data_spb[!is.na(smoking)])), by = "smoking"]
smoking_proportions_list <- smoking_proportions$prop
names(smoking_proportions_list) <- smoking_proportions$smoking
smoking_proportions_list <- rev(smoking_proportions_list)

# Define a list with raking targets
raking_target <- list( "male" = male_proportions_list, "agegroup" = agegroup_proportions_list, "education_level" = education_level_proportions_list, "health_level" = health_level_proportions_list, "lives_alone" = lives_alone_proportions_list, "smoking" = smoking_proportions_list)

# Perform raking
## Entire phone survey
phone_survey_raking_fit <- anesrake(inputter = raking_target, dataframe = phone_survey_data[, names(raking_target), with = F], caseid = phone_survey_data$ID, cap = 5, choosemethod = "total", type = "pctlim", pctlim = 0.05)

## Only tested individuals
phone_survey_agreed_and_tested_raking_fit <- anesrake(inputter = raking_target, dataframe = phone_survey_data[ agreed_and_tested == 1], caseid = phone_survey_data[ agreed_and_tested == 1]$ID, cap = 5, choosemethod = "total", type = "pctlim", pctlim = 0.05)

# Save the object with raking
save(phone_survey_raking_fit, file = "estimates/wave2/phone_survey_raking_fit.rdata", compress = "gzip")
save(phone_survey_agreed_and_tested_raking_fit, file = "estimates/wave2/phone_survey_agreed_and_tested_raking_fit.rdata", compress = "gzip")

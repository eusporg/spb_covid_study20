library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(anesrake)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
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
load("data/wave1/phone_survey/phone_survey_data.rdata")

# Load paper survey data from this study
load("data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")

# Test results data contains all individuals that were contacted as of this date 
initial_phone_call_last_date <- "2020-06-24"
phone_survey_data <- phone_survey_data[interview_date <= initial_phone_call_last_date]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district as they are outside Saint Petersburg
phone_survey_data <- phone_survey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Remove individuals from districts in St. Petersburg that we have omitted from the study
phone_survey_data <- phone_survey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
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

# Define raking variables
raking_variables <- c("agegroup", "male", "lives_alone", "education_level")

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

# Define a list with raking targets
raking_target <- list( "male" = male_proportions_list, "agegroup" = agegroup_proportions_list, "education_level" = education_level_proportions_list, "lives_alone" = lives_alone_proportions_list)

# Perform raking
phone_survey_raking_fit <- anesrake(raking_target, phone_survey_data[, c(raking_variables), with = F], phone_survey_data$ID, cap = 5, choosemethod = "total", type = "pctlim", pctlim = 0.05 )

# Save the object with raking
save(phone_survey_raking_fit, file = "estimates/wave1/phone_survey_raking_fit.rdata", compress = "xz")

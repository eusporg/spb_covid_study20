# This code prepares table one 
# for the COVID-19 vaccines effectiveness against 
# symptomatic SARS-CoV-2 Delta variant infection: 
# a population-based case-control study in St. Petersburg, Russia


library(data.table)
library(tableone)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load dataset
load("data/ve_covid_paper2/case_control_final.Rdata")

myVars <- c( "age",  "age_group", 
             "male",  "date",
             "ct_score_simple", 
             "vaccine_name",
             "vac_status_simple", "vac_status", "pcr_past")

# Vector of categorical variables 
catVars <- c( "age_group", 
              "male", "date",
              "ct_score_simple", 
              "vac_status_simple", 
              "vaccine_name", "vac_status", "pcr_past")

# Split by case/control
# Create a TableOne object
table_1_0614 <- CreateTableOne(vars = myVars, factorVars = catVars, 
                               data = case_control_final, 
                               strata = "source",
                               # strata = "ct_score_34",
                               addOverall = TRUE, 
                               test = FALSE, 
                               includeNA = TRUE)

# print table 
# print(table_1_0614,  showAllLevels = T)
# write.csv(print(table_1_0614,  showAllLevels = T), file = "table_1_0614.csv")


# descriptive table for vaccine name2 и vac_status2

myVars1 <- c( "age",  "age_group", 
             "male",  "ct_score_simple", 
             "vaccine_name2",
             "vac_status_simple", "vac_status2", "pcr_past")

catVars1 <- c( "age_group", 
              "male", "ct_score_simple", 
              "vac_status_simple", 
              "vaccine_name2", "vac_status2", "pcr_past")

# Create a TableOne object
table_1s <- CreateTableOne(vars = myVars1, factorVars = catVars1, 
                               data = case_control_final, 
                               strata = "source",
                               addOverall = TRUE, 
                               test = FALSE, 
                               includeNA = TRUE)

# # print table 
# print(table_1s,  showAllLevels = T)
# write.csv(print(table_1s,  showAllLevels = T), file = "table_1s.csv")


# descriptive table for  vaccine name и vac_status3 - unknown dates moved to un-vaccinated

myVars2 <- c( "age",  "age_group", 
              "male",  "ct_score_simple", 
              "vaccine_name",
              "vac_status_simple", "vac_status3", "pcr_past")

catVars2 <- c( "age_group", 
               "male", "ct_score_simple", 
               "vac_status_simple", 
               "vaccine_name", "vac_status3", "pcr_past")

# Create a TableOne object
table_2s <- CreateTableOne(vars = myVars2, factorVars = catVars2, 
                           data = case_control_final, 
                           strata = "source",
                           addOverall = TRUE, 
                           test = FALSE, 
                           includeNA = TRUE)

# print table 
# print(table_2s,  showAllLevels = T)
# write.csv(print(table_2s,  showAllLevels = T), file = "table_2s.csv")
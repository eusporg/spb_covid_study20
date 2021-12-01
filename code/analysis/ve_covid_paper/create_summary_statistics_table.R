# This code creates the summary statistics table
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(tableone)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))


# Load data
load("data/ve_covid_paper/trc_dt.Rdata")

# Select variables for the table
# dput(names(trc_dt))
# Vector of variables to summarize
myVars <- c( "age",  "age_group", 
						 "sex",
						 "source",
						 "vac_doses",
						 "dose",
						 "vac_status3",
						 "saturation", 
						 "sat_score",
						 "ct_score")

# Vector of categorical variables 
catVars <- c( # "age",  
							"age_group", 
							"sex",
							"source",
							"vac_doses",
							"dose",
							"vac_status3",
							"sat_score",
							"ct_score")

# Create a TableOne object
table_1 <- CreateTableOne(vars = myVars, factorVars = catVars, 
											 data = trc_dt, 
											 strata = "hospitalization", 
											 addOverall = TRUE, 
											 test = FALSE, 
											 includeNA = TRUE)

# print table with IRQ for saturation and count NAs
print(table_1, nonnormal = "saturation", showAllLevels = TRUE)
# # Save table to file
# write.csv(print(table_1, nonnormal = "saturation", showAllLevels = TRUE), file = "table_1.csv")
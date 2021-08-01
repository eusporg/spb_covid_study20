# This code estimates seroprevalence by narrowly
# define age and sex groups for IR/IFR estimation
# for COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(stats)
library(w)
library(GJRM)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load and prepare the serosurvey data for model fit
source("code/analysis/wave3/create_serosurvey_data_for_model_fit.r")

# Update the agegroups
serosurvey_data[, agegroup := NULL]
serosurvey_data[, agegroup := NA_character_]
serosurvey_data[age <= 29, agegroup := "18-29"]
serosurvey_data[age >= 30 & age < 40, agegroup := "30-39"]
serosurvey_data[age >= 40 & age < 50, agegroup := "40-49"]
serosurvey_data[age >= 50 & age < 60, agegroup := "50-59"]
serosurvey_data[age >= 60 & age < 70, agegroup := "60-69"]
serosurvey_data[age >= 70, agegroup := "70+"]
serosurvey_data[, agegroup := as.factor(agegroup)]

############################
# Parametric sample selection bivariate probit fit

# Define regressors list
basic_regressors <- "agegroup*male + highereduc + lives_alone + was_sick + selftested_covid + district"

# Fit the models with given regressors
model <- estimate_bivariate_selection(data = serosurvey_data, subset_condition = NULL, selection_type = "2step", agreementvar = "agreed", clinicvisitvar = "agreed_and_tested", outcomevar = "IgA_or_G_or_M_testC", regressors = basic_regressors, semiparametric_age = F, exclusion_restriction_clinicvisit = NULL, error_distribution = "N", surveyweightsvar = "raking_weight")

############################
# Prevalence estimation

# Set test characteristics from own validation
## Genetico CoronaPass
sensitivity_iga_g_m_genetico <- 0.920
specificity_iga_g_m_genetico <- 1

# Initiate an object to store the results
prevalence_by_agegroup_sex <- CJ(agegroup = unique(serosurvey_data$agegroup), male = unique(serosurvey_data$male), prevalence_type = c("naive", "univariate"))

for(i in 1:nrow(prevalence_by_agegroup_sex) ) {
	
	agegroup_row <- prevalence_by_agegroup_sex$agegroup[i]
	male_row <- prevalence_by_agegroup_sex$male[i]
	prevalence_type_row <- prevalence_by_agegroup_sex$prevalence_type[i]

	# True/False indicators for variable levels
	## Outcome equation
	var_indicators_outcome <- model$fit$gam2$model[["agegroup"]] == agegroup_row & model$fit$gam2$model[["male"]] == male_row
	## Selection equation
	var_indicators_selection <- model$fit$gam1$model[["agegroup"]] == agegroup_row & model$fit$gam1$model[["male"]] == male_row

	# Count the number of entries
	n_var_indicators_outcome <- sum(var_indicators_outcome)
	n_var_indicators_selection <- sum(var_indicators_selection)

	# Blank survey weights
	group_survey_weights_outcome <- rep(1, sum(var_indicators_outcome))
	group_survey_weights_selection <- rep(1, sum(var_indicators_selection))

	if( prevalence_type_row == "naive" ) {

		var_prev <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_outcome, type = prevalence_type_row, delta = T, sw = group_survey_weights_outcome), sensitivity = sensitivity_iga_g_m_genetico, specificity = specificity_iga_g_m_genetico)

	}

	if( prevalence_type_row == "univariate" ) {

		var_prev <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_selection, type = prevalence_type_row, delta = T, sw = group_survey_weights_selection), sensitivity = sensitivity_iga_g_m_genetico, specificity = specificity_iga_g_m_genetico)


	}

	set(prevalence_by_agegroup_sex, i = i, j = "lowerbound", value = var_prev$res[1])
 	set(prevalence_by_agegroup_sex, i = i, j = "pointest", value = var_prev$res[2])
 	set(prevalence_by_agegroup_sex, i = i, j = "upperbound", value = var_prev$res[3])
 	set(prevalence_by_agegroup_sex, i = i, j = "n_selection", value = n_var_indicators_selection)
 	set(prevalence_by_agegroup_sex, i = i, j = "n_outcome", value = n_var_indicators_outcome)
					
}
# Round the results
prevalence_by_agegroup_sex[, c("lowerbound", "pointest", "upperbound") := lapply(.SD, function(x) { round(x*100, 1) }), .SDcols = c("lowerbound", "pointest", "upperbound") ]

# Ordering
setorderv(prevalence_by_agegroup_sex, c("prevalence_type", "agegroup", "male")) 

# Save the object with computed prevalences for future use
save(prevalence_by_agegroup_sex, file = "estimates/pandemic_course_paper/prevalence_by_agegroup_sex.rdata", compress = "gzip")

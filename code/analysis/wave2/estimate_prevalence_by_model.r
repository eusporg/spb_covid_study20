library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(stats)
library(matrixStats)
# We require a specific version of GJRM:
# remotes::install_version("GJRM", "0.2-2")
library(GJRM)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load and prepare the serosurvey data for model fit
source("code/analysis/wave2/create_serosurvey_data_for_model_fit.r")

############################
# Parametric sample selection bivariate probit fit

# Define regressors list
demographic_regressors <- "agegroup + male"
sociodemographic_regressors <- "agegroup + male + highereduc"
infection_regressors <- "smoking + lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"
basic_regressors <- "agegroup + male + highereduc + smoking + lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"
other_regressors <- paste0(basic_regressors, " + wears_glasses + no_pets + allergies")

# Fit the models with given scenarios
regression_scenarios <- c("demographic", "sociodemographic", "infection", "basic", "other")

# Define serological tests to consider
serological_tests <- c("IgG_or_M_testD", "IgA_or_G_or_M_testC", "IgG_testB", "testC_and_testD", "testC_or_testD")

# Fit the models with given regressors
for(scenario in regression_scenarios) {

	# Debug: scenario <- "demographic"; serotest <- "IgG_or_M_testD"

	regressors_used <- get(paste0(scenario, "_regressors"))

	for(serotest in serological_tests) {

		model_fit <- estimate_bivariate_selection(data = serosurvey_data, subset_condition = NULL, selection_type = "2step", agreementvar = "agreed", clinicvisitvar = "agreed_and_tested", outcomevar = serotest, regressors = regressors_used, semiparametric_age = F, exclusion_restriction_clinicvisit = "lenta_card", error_distribution = "N", surveyweightsvar = "raking_weight")

		# Save to an R object with a naming convention
		# Example: out.demographic.IgG_testB
		assign(paste("out", scenario, serotest, sep = "."), model_fit)

		print(paste0("Fit model ", scenario, ", test ", serotest))

	}
	
}

############################
# Prevalence estimation

# Set test characteristics
## Abbott (from own validation)
sensitivity_igg_abbott <- 0.677
specificity_igg_abbott <- 1

## Genetico CoronaPass (from own validation)
sensitivity_iga_g_m_genetico <- 0.920
specificity_iga_g_m_genetico <- 1

## Vector-best (from own validation)
sensitivity_igg_m_vector <- 0.939
specificity_igg_m_vector <- 1

######## Create an object with prevalence results from different regression models

# Specify which prevalences to estimate
prevalence_types <- c("naive", "univariate", "joint")

# Initiate an object to store the results
prevalence_by_model <- data.table()

for(scenario in regression_scenarios) {

	for(serotest in serological_tests) {

		for(prevalence_type in prevalence_types) {
			
			for( surveyweight in c("no", "raking") ) {

				# Debug: scenario <- "demographic"; serotest <- "IgG_or_M_testD"; prevalence_type <- "univariate"; surveyweight <- "no"
	
				# Retrieve the specified model
				model <- get(paste("out", scenario, serotest, sep = "."))
				
				# Variables in selection and outcome equations
				n_selection <- nrow(model$fit$gam1$model)
				n_outcome <- nrow(model$fit$gam2$model)
	
				# Set test characteristics
				test_sensitivity <- ifelse(grepl("testB", serotest), sensitivity_igg_abbott, ifelse(grepl("testC", serotest), sensitivity_iga_g_m_genetico, ifelse(grepl("testD", serotest), sensitivity_igg_m_vector, NA_real_)))
				test_specificity <- ifelse(grepl("testB", serotest), specificity_igg_abbott, ifelse(grepl("testC", serotest), specificity_iga_g_m_genetico, ifelse(grepl("testD", serotest), specificity_igg_m_vector, NA_real_)))
				
				# Consider a special case when we
				# perform logical OR or AND on tests
				## testC OR testD
				if( serotest == "testC_or_testD") {
	
					test_sensitivity <- 0.939
					test_specificity <- 1
	
				}
				## testC AND testD
				if( serotest == "testC_and_testD") {
	
					test_sensitivity <- 0.920
					test_specificity <- 1
	
				}
	
				# Specify survey weights for prevalence computation
				if( !is.null(model$fit$survey_weights_visit) & surveyweight != "no" ) {
					
					if (prevalence_type == "naive") {
	
						survey_weights <- model$fit$survey_weights_outcome
	
					} else {
	
						survey_weights <- model$fit$survey_weights_visit
	
					}
	
				} else {
	
					survey_weights <- NULL
				}
				
				# Adjust the prevalence with respect to test quality
				# and store the unadjusted version as well
				for( adjusted in c(T, F) ) {

					adjusted_prevalence <- adjust_prev_test_chars(prev_modified(model$fit, type = prevalence_type, sw = survey_weights, delta = T), sensitivity = ifelse(adjusted, test_sensitivity, 1), specificity = ifelse(adjusted, test_specificity, 1))
		
					# Append to an object
					prevalence_by_model <- rbind(prevalence_by_model,
							data.table(	scenario = scenario,
										serotest = serotest,
										prevalence_type = prevalence_type,
										surveyweight = surveyweight,
										sensitivity = adjusted_prevalence$sensitivity,
										specificity = adjusted_prevalence$specificity,
										lowerbound = adjusted_prevalence$res[1],
										pointest = adjusted_prevalence$res[2],
										upperbound = adjusted_prevalence$res[3],
										n_selection = n_selection,
										n_outcome = n_outcome,
										rho = as.numeric(summary(model$fit)$theta),
										rhoci = list(as.numeric(summary(model$fit)$CItheta)) 
									),
								fill = T)

				}

			}

		}
	}

}

# Round the results
prevalence_by_model[, c("lowerbound", "pointest", "upperbound", "rho") := lapply(.SD, function(x) { round(x*100, 1) }), .SDcols = c("lowerbound", "pointest", "upperbound", "rho") ]

# Save point
save(prevalence_by_model, file = "estimates/wave2/prevalence_by_model.rdata", compress = "xz")

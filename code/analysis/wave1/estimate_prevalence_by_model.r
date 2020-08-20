library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(stats)
library(matrixStats)
library(GJRM)

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

# Load a modified prevalence computation function
# that is based on GJRM::prev
source("code/analysis/helper_functions/prev_modified.r")

# Load a function that estimates the
# bivariate probit model and outputs the results
source("code/analysis/helper_functions/estimate_bivariate_selection.r")

# Load a function that adjusts the prevalence
# estimates for sensitivity and specificity
# in classical way
source("code/analysis/helper_functions/adjust_prev_test_chars.r")

# Load raking weights fit for phone survey data
# produced by estimate_raking_weights_phone_survey.r
load("estimates/phone_survey_raking_fit.rdata")
raking_weights <- data.table(ID = names(phone_survey_raking_fit$weightvec), raking_weight = phone_survey_raking_fit$weightvec)


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

# Sample draw week of year 
serosurvey_data[, draw_week := as.factor(isoweek(draw_sample_date))]

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

# Add raking weights
serosurvey_data <- merge(serosurvey_data, raking_weights, by = "ID", all.x = T, all.y = F)

############################
# Parametric sample selection bivariate probit fit

# Define regressors list
demographic_regressors <- "agegroup + male"
sociodemographic_regressors <- "agegroup + male + highereduc + higherincome"
infection_regressors <- "lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"
basic_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"

regression_scenarios <- c("demographic", "sociodemographic", "infection", "basic")

# Define serological tests to consider
serological_tests <- c("IgG_testB", "IgA_or_G_or_M_testC", "testB_and_testC", "testB_or_testC")

# Fit the models with given regressors
for(scenario in regression_scenarios) {

	# Debug: scenario <- "demographic"; serotest <- "IgG_testB"

	regressors_used <- get(paste0(scenario, "_regressors"))

	for(serotest in serological_tests) {

		model_fit <- estimate_bivariate_selection(data = serosurvey_data, subset_condition = NULL, selection_type = "2step", agreementvar = "agreed", clinicvisitvar = "agreed_and_tested", outcomevar = serotest, regressors = regressors_used, semiparametric_age = F, exclusion_restriction_clinicvisit = "offered_taxi", error_distribution = "N", surveyweightsvar = "raking_weight")

		# Save to an R object with a naming convention
		# Example: out.demographic.IgG_testB
		assign(paste("out", scenario, serotest, sep = "."), model_fit)

		print(paste0("Fit model ", scenario, ", test ", serotest))

	}
	
}

############################
# Prevalence estimation

# Set test characteristics
## Sugentech: http://sugentech.com/images/popup/Clinical%20Evaluation%20Study%20Report_SGTi-flex%20COVID-19%20IgM_testA%20&%20IgG_testA_200515.pdf
sensitivity_igg_m_sugentech <- 0.9448
specificity_igg_m_sugentech <- 0.9818

sensitivity_igg_sugentech <- 0.9018
specificity_igg_sugentech <- 1

sensitivity_igm_sugentech <- 0.9080
specificity_igm_sugentech <- 0.9818

## Abbott: https://www.fda.gov/medical-devices/emergency-situations-medical-devices/eua-authorized-serology-test-performance
sensitivity_igg_abbott <- 1
specificity_igg_abbott <- 0.996

## Genetico CoronaPass: https://pass.genetico.ru
sensitivity_iga_g_m_genetico <- 0.96
specificity_iga_g_m_genetico <- 1

######## Create an object with prevalence results from different regression models

# Specify which prevalences to estimate
prevalence_types <- c("naive", "univariate", "joint")

# Initiate an object to store the results
prevalence_by_model <- data.table()

for(scenario in regression_scenarios) {

	for(serotest in serological_tests) {

		for(prevalence_type in prevalence_types) {
			
			for( surveyweight in c("no", "raking") ) {

				# Debug: scenario <- "demographic"; serotest <- "IgG_testB"; prevalence_type <- "univariate"; surveyweight <- "no"
	
				# Retrieve the specified model
				model <- get(paste("out", scenario, serotest, sep = "."))
				
				# Variables in selection and outcome equations
				n_selection <- nrow(model$fit$gam1$model)
				n_outcome <- nrow(model$fit$gam2$model)
	
				# Set test characteristics
				test_sensitivity <- ifelse(grepl("testB", serotest), sensitivity_igg_abbott, ifelse(grepl("testC", serotest), sensitivity_iga_g_m_genetico, ifelse(grepl("testA", serotest), sensitivity_igg_m_sugentech, NA_real_)))
				test_specificity <- ifelse(grepl("testB", serotest), specificity_igg_abbott, ifelse(grepl("testC", serotest), specificity_iga_g_m_genetico, ifelse(grepl("testA", serotest), specificity_igg_m_sugentech, NA_real_)))
				
				# Consider a special case when we
				# perform logical OR or AND on tests
				## testB OR testC
				if( serotest == "testB_or_testC") {
	
					test_sensitivity <- 0.96
					test_specificity <- 1
	
				}
				## testB OR testC
				if( serotest == "testB_and_testC") {
	
					test_sensitivity <- 1
					test_specificity <- 0.996
	
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
				adjusted_prevalence <- adjust_prev_test_chars(prev_modified(model$fit, type = prevalence_type, sw = survey_weights, delta = T), sensitivity = test_sensitivity, specificity = test_specificity)
	
				# Append to an object
				prevalence_by_model <- rbind(prevalence_by_model, data.table(scenario = scenario, serotest = serotest, prevalence_type = prevalence_type, surveyweight = surveyweight, lowerbound = adjusted_prevalence$res[1], pointest = adjusted_prevalence$res[2], upperbound = adjusted_prevalence$res[3], n_selection = n_selection, n_outcome = n_outcome, rho = as.numeric(summary(model$fit)$theta), rhoci = list(as.numeric(summary(model$fit)$CItheta)) ), fill = T)

			}

		}
	}

}

# Round the results
prevalence_by_model[, c("lowerbound", "pointest", "upperbound") := lapply(.SD, function(x) { round(x*100, 1) }), .SDcols = c("lowerbound", "pointest", "upperbound") ]

# Save point
save(prevalence_by_model, file = "estimates/wave1/prevalence_by_model.rdata", compress = "xz")

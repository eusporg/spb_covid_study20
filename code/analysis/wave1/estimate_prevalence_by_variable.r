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

# Add raking weights
serosurvey_data <- merge(serosurvey_data, raking_weights, by = "ID", all.x = T, all.y = F)

############################
# Parametric sample selection bivariate probit fit

# Define regressors list
basic_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + washing_hands_more + was_sick + selftested_covid + district + interview_week"

# Fit the models with given scenarios
regression_scenarios <- c("basic")

# Define serological tests to consider
serological_tests <- c("IgG_testB", "IgA_or_G_or_M_testC")

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
# Prevalence estimation by variable

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
sensitivity_iga_g_m_genetico <- 0.987
specificity_iga_g_m_genetico <- 1

# Specify which prevalences to estimate
prevalence_types <- c("naive", "univariate", "joint")

# Which variables to group by
grouping_vars <- str_trim(unlist(strsplit(basic_regressors, "+", fixed = T)))

# Initiate an object to store the results
prevalence_by_variable_level <- data.table()

for(scenario in regression_scenarios) {

	for(serotest in serological_tests) {
			
		for(group_var in grouping_vars) {

			for( surveyweight in c("no", "raking") ) {

				# Debug: scenario <- "basic"; serotest <- "IgG_testB"; group_var <- "education_level"
	
				# Retrieve the specified model
				model <- get(paste("out", scenario, serotest, sep = "."))
	
				# Set test characteristics
				test_sensitivity <- ifelse(grepl("testB", serotest), sensitivity_igg_abbott, ifelse(grepl("testC", serotest), sensitivity_iga_g_m_genetico, ifelse(grepl("testA", serotest), sensitivity_igg_m_sugentech, NA_real_)))
				test_specificity <- ifelse(grepl("testB", serotest), specificity_igg_abbott, ifelse(grepl("testC", serotest), specificity_iga_g_m_genetico, ifelse(grepl("testA", serotest), specificity_igg_m_sugentech, NA_real_)))
	
				# Consider a special case when we
				# perform logical OR or AND on tests
				## testB OR testC
				if( serotest == "testB_or_testC") {
	
					test_sensitivity <- 0.987
					test_specificity <- 1
	
				}
				## testB OR testC
				if( serotest == "testB_and_testC") {
	
					test_sensitivity <- 1
					test_specificity <- 0.996
	
				}
		
				# Enumerate levels of grouping variable 
				# (respecting the order of appearance for ordered factors)
				group_var_levels <- model$fit$gam2$model[[group_var]]
	
				if( is.factor(group_var_levels) ) { 
	
					group_var_levels <- levels(group_var_levels)
	
				} else {
	
					group_var_levels <- unique(group_var_levels)
	
				}
				
				# For each level of grouping variable
				for( var in group_var_levels ) {
	
					# True/False indicators for variable levels
					# Debug: var <- "Primary education"
					## Outcome equation
					var_indicators_outcome <- model$fit$gam2$model[[group_var]] == var
					## Selection equation
					var_indicators_selection <- model$fit$gam1$model[[group_var]] == var
					
					# Take the subset of survey weights that corresponds to a group of interest
					if( surveyweight != "no" ) {
					
						group_survey_weights_outcome <- model$fit$survey_weights_outcome[var_indicators_outcome]
						group_survey_weights_selection <- model$fit$survey_weights_visit[var_indicators_selection]
					
					} else {

						group_survey_weights_outcome <- rep(1, sum(var_indicators_outcome))
						group_survey_weights_selection <- rep(1, sum(var_indicators_selection))


					}

					# Count number 
					n_var_indicators_outcome <- sum(var_indicators_outcome)
					n_var_indicators_selection <- sum(var_indicators_selection)
	
					# Naive prevalence by group
					var_prev_naive <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_outcome, type = "naive", sw = group_survey_weights_outcome, delta = T), sensitivity = test_sensitivity, specificity = test_specificity)
					# Univariate prevalence by group
					var_prev_univariate <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_selection, type = "univariate", sw = group_survey_weights_selection, delta = T), sensitivity = test_sensitivity, specificity = test_specificity)
					# Joint prevalence by group
					var_prev_joint <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_selection, type = "joint", sw = group_survey_weights_selection, delta = T), sensitivity = test_sensitivity, specificity = test_specificity)
	
					# Append to an object
					## Naive
					prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest, prevalence_type = "naive", surveyweight = surveyweight, variable = group_var, variable_level = var, lowerbound = var_prev_naive$res[1], pointest = var_prev_naive$res[2], upperbound = var_prev_naive$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)
					## Univariate
					prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest,  prevalence_type = "univariate",  surveyweight = surveyweight, variable = group_var, variable_level = var, lowerbound = var_prev_univariate$res[1], pointest = var_prev_univariate$res[2], upperbound = var_prev_univariate$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)
					## Joint
					prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest,  prevalence_type = "joint", surveyweight = surveyweight, variable = group_var, variable_level = var, lowerbound = var_prev_joint$res[1], pointest = var_prev_joint$res[2], upperbound = var_prev_joint$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)
	
				}

			}

		}
	}

}


#####
# Compute naive prevalence by blood draw week

## Make data conformable
test_results_basic_regressors_abbott <- na.omit(serosurvey_data[, c(grouping_vars, "draw_week", "IgG_testB"), with = F ])
test_results_basic_regressors_genetico <- na.omit(serosurvey_data[, c(grouping_vars, "draw_week", "IgA_or_G_or_M_testC"), with = F ])

# Test result means and their CIs by week
naive_prevalence_by_draw_sample_week_abbott <- test_results_basic_regressors_abbott[, list(pointest = mean(IgG_testB, na.rm = T), se = sd(IgG_testB, na.rm = T)/sqrt(.N), N = .N), by = "draw_week"]
naive_prevalence_by_draw_sample_week_genetico <- test_results_basic_regressors_genetico[, list(pointest = mean(IgA_or_G_or_M_testC, na.rm = T), se = sd(IgA_or_G_or_M_testC, na.rm = T)/sqrt(.N), N = .N), by = "draw_week"]

naive_prevalence_by_draw_sample_week_abbott[, upperbound := pointest+qnorm(0.975)*se]
naive_prevalence_by_draw_sample_week_abbott[, lowerbound := pointest-qnorm(0.975)*se]

naive_prevalence_by_draw_sample_week_genetico[, upperbound := pointest+qnorm(0.975)*se]
naive_prevalence_by_draw_sample_week_genetico[, lowerbound := pointest-qnorm(0.975)*se]

naive_prevalence_by_draw_sample_week_adjusted <- data.table()
 
# Apply test performance adjustments to a manually constructed object with naive prevalences
for(w in unique(naive_prevalence_by_draw_sample_week_abbott$draw_week)) {

	# Weekly adjusted prevalence
	abbot_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_week_abbott[draw_week == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA))$res
	names(abbot_prev) <- c("lowerbound", "pointest", "upperbound")

	out_abbott <- data.table(serotest = "IgG_testB", prevalence_type = "naive", surveyweight = "no", variable = "draw_week", variable_level = w, t(abbot_prev), n_outcome = naive_prevalence_by_draw_sample_week_abbott[draw_week == w]$N)

	genetico_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_week_genetico[draw_week == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA))$res
	names(genetico_prev) <- c("lowerbound", "pointest", "upperbound")

	out_genetico <- data.table(serotest = "IgA_or_G_or_M_testC", prevalence_type = "naive", surveyweight = "no", variable = "draw_week", variable_level = w, t(genetico_prev), n_outcome = naive_prevalence_by_draw_sample_week_genetico[draw_week == w]$N)

	naive_prevalence_by_draw_sample_week_adjusted <- rbind(naive_prevalence_by_draw_sample_week_adjusted, out_abbott, out_genetico)

}

# Append those results to the main data set with prevalence
prevalence_by_variable_level <- rbind(prevalence_by_variable_level, naive_prevalence_by_draw_sample_week_adjusted, fill = T)

# Round the results
prevalence_by_variable_level[, c("lowerbound", "pointest", "upperbound") := lapply(.SD, function(x) { round(x*100, 1) }), .SDcols = c("lowerbound", "pointest", "upperbound") ]

# Ordering
setorderv(prevalence_by_variable_level, c("serotest", "prevalence_type", "variable", "variable_level")) 

# Save the object with computed prevalences for future use
save(prevalence_by_variable_level, file = "estimates/wave1/prevalence_by_variable_level.rdata", compress = "xz")

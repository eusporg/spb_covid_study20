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
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load and prepare the serosurvey data for model fit
source("code/analysis/wave1.5/create_serosurvey_data_for_model_fit.r")

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
## Abbott from own validation
sensitivity_igg_abbott <- 0.677
specificity_igg_abbott <- 1

## Genetico CoronaPass from own validation
sensitivity_iga_g_m_genetico <- 0.920
specificity_iga_g_m_genetico <- 1

# Specify which prevalences to estimate
prevalence_types <- c("naive", "univariate")

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
	
					test_sensitivity <- 0.96
					test_specificity <- 1
	
				}
				## testB AND testC
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
	
# Store the adjusted and unadjusted versions alongside
					for( adjusted in c(T, F) ) {

						# Naive prevalence by group
						var_prev_naive <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_outcome, type = "naive", sw = group_survey_weights_outcome, delta = T), sensitivity = ifelse(adjusted, test_sensitivity, 1), specificity = ifelse(adjusted, test_specificity, 1))
						# Univariate prevalence by group
						var_prev_univariate <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_selection, type = "univariate", sw = group_survey_weights_selection, delta = T), sensitivity = ifelse(adjusted, test_sensitivity, 1), specificity = ifelse(adjusted, test_specificity, 1))
						# Joint prevalence by group
						#var_prev_joint <- adjust_prev_test_chars(prev_modified(model$fit, ind = var_indicators_selection, type = "joint", sw = group_survey_weights_selection, delta = T), sensitivity = ifelse(adjusted, test_sensitivity, 1), specificity = ifelse(adjusted, test_specificity, 1))
	
						# Append to an object
						## Naive
						prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest, prevalence_type = "naive", surveyweight = surveyweight, sensitivity = var_prev_naive$sensitivity, specificity = var_prev_naive$specificity, variable = group_var, variable_level = var, lowerbound = var_prev_naive$res[1], pointest = var_prev_naive$res[2], upperbound = var_prev_naive$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)
						## Univariate
						prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest,  prevalence_type = "univariate",  surveyweight = surveyweight, sensitivity = var_prev_univariate$sensitivity, specificity = var_prev_univariate$specificity, variable = group_var, variable_level = var, lowerbound = var_prev_univariate$res[1], pointest = var_prev_univariate$res[2], upperbound = var_prev_univariate$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)
						## Joint
						#prevalence_by_variable_level <- rbind(prevalence_by_variable_level, data.table(serotest = serotest,  prevalence_type = "joint",  surveyweight = surveyweight, sensitivity = var_prev_joint$sensitivity, specificity = var_prev_joint$specificity, variable = group_var, variable_level = var, lowerbound = var_prev_joint$res[1], pointest = var_prev_joint$res[2], upperbound = var_prev_joint$res[3], n_selection = n_var_indicators_selection, n_outcome = n_var_indicators_outcome), fill = T)

					}

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

	# Weekly adjusted and unadjusted prevalence
	for( adjusted in c(T, F) ) {

		abbot_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_week_abbott[draw_week == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA), sensitivity = ifelse(adjusted, sensitivity_igg_abbott, 1), specificity = ifelse(adjusted, specificity_igg_abbott, 1))$res
		names(abbot_prev) <- c("lowerbound", "pointest", "upperbound")
	
		out_abbott <- data.table(serotest = "IgG_testB", prevalence_type = "naive", surveyweight = "no", sensitivity = ifelse(adjusted, sensitivity_igg_abbott, 1), specificity = ifelse(adjusted, specificity_igg_abbott, 1), variable = "draw_week", variable_level = w, t(abbot_prev), n_outcome = naive_prevalence_by_draw_sample_week_abbott[draw_week == w]$N)
	
		genetico_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_week_genetico[draw_week == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA), sensitivity = ifelse(adjusted, sensitivity_iga_g_m_genetico, 1), specificity = ifelse(adjusted, specificity_iga_g_m_genetico, 1))$res
		names(genetico_prev) <- c("lowerbound", "pointest", "upperbound")
	
		out_genetico <- data.table(serotest = "IgA_or_G_or_M_testC", prevalence_type = "naive", surveyweight = "no", sensitivity = ifelse(adjusted, sensitivity_iga_g_m_genetico, 1), specificity = ifelse(adjusted, specificity_iga_g_m_genetico, 1), variable = "draw_week", variable_level = w, t(genetico_prev), n_outcome = naive_prevalence_by_draw_sample_week_genetico[draw_week == w]$N)
	
		naive_prevalence_by_draw_sample_week_adjusted <- rbind(naive_prevalence_by_draw_sample_week_adjusted, out_abbott, out_genetico)

	}

} 

# Append those results to the main data set with prevalence
prevalence_by_variable_level <- rbind(prevalence_by_variable_level, naive_prevalence_by_draw_sample_week_adjusted, fill = T)

# Same, but biweekly
test_results_basic_regressors_abbott[draw_week %in% c(30, 31), draw_biweek := 30.5]
test_results_basic_regressors_abbott[draw_week %in% c(32, 33), draw_biweek := 32.5]

test_results_basic_regressors_genetico[draw_week %in% c(30, 31), draw_biweek := 30.5]
test_results_basic_regressors_genetico[draw_week %in% c(32, 33), draw_biweek := 32.5]

naive_prevalence_by_draw_sample_biweek_abbott <- test_results_basic_regressors_abbott[!is.na(draw_biweek), list(pointest = mean(IgG_testB, na.rm = T), se = sd(IgG_testB, na.rm = T)/sqrt(.N), N = .N), by = "draw_biweek"]
naive_prevalence_by_draw_sample_biweek_genetico <- test_results_basic_regressors_genetico[!is.na(draw_biweek), list(pointest = mean(IgA_or_G_or_M_testC, na.rm = T), se = sd(IgA_or_G_or_M_testC, na.rm = T)/sqrt(.N), N = .N), by = "draw_biweek"]

naive_prevalence_by_draw_sample_biweek_abbott[, upperbound := pointest+qnorm(0.975)*se]
naive_prevalence_by_draw_sample_biweek_abbott[, lowerbound := pointest-qnorm(0.975)*se]

naive_prevalence_by_draw_sample_biweek_genetico[, upperbound := pointest+qnorm(0.975)*se]
naive_prevalence_by_draw_sample_biweek_genetico[, lowerbound := pointest-qnorm(0.975)*se]

naive_prevalence_by_draw_sample_biweek_adjusted <- data.table()
 
# Apply test performance adjustments to a manually constructed object with naive biweekly prevalences
for(w in unique(naive_prevalence_by_draw_sample_biweek_abbott$draw_biweek)) {

	# Biweekly adjusted and unadjusted prevalence
	for( adjusted in c(T, F) ) {

		abbot_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_biweek_abbott[draw_biweek == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA), sensitivity = ifelse(adjusted, sensitivity_igg_abbott, 1), specificity = ifelse(adjusted, specificity_igg_abbott, 1))$res
		names(abbot_prev) <- c("lowerbound", "pointest", "upperbound")
	
		out_abbott <- data.table(serotest = "IgG_testB", prevalence_type = "naive", surveyweight = "no", sensitivity = ifelse(adjusted, sensitivity_igg_abbott, 1), specificity = ifelse(adjusted, specificity_igg_abbott, 1), variable = "draw_biweek", variable_level = w, t(abbot_prev), n_outcome = naive_prevalence_by_draw_sample_biweek_abbott[draw_biweek == w]$N)
	
		genetico_prev <- adjust_prev_test_chars(list(res = unlist(unname(naive_prevalence_by_draw_sample_biweek_genetico[draw_biweek == w, c("lowerbound", "pointest", "upperbound")])), prob.lev = 0.05, sim.prev = NA), sensitivity = ifelse(adjusted, sensitivity_iga_g_m_genetico, 1), specificity = ifelse(adjusted, specificity_iga_g_m_genetico, 1))$res
		names(genetico_prev) <- c("lowerbound", "pointest", "upperbound")
	
		out_genetico <- data.table(serotest = "IgA_or_G_or_M_testC", prevalence_type = "naive", surveyweight = "no", sensitivity = ifelse(adjusted, sensitivity_iga_g_m_genetico, 1), specificity = ifelse(adjusted, specificity_iga_g_m_genetico, 1), variable = "draw_biweek", variable_level = w, t(genetico_prev), n_outcome = naive_prevalence_by_draw_sample_biweek_genetico[draw_biweek == w]$N)
	
		naive_prevalence_by_draw_sample_biweek_adjusted <- rbind(naive_prevalence_by_draw_sample_biweek_adjusted, out_abbott, out_genetico)

	}

}

# Append those results to the main data set with prevalence
prevalence_by_variable_level <- rbind(prevalence_by_variable_level, naive_prevalence_by_draw_sample_biweek_adjusted, fill = T)

# Round the results
prevalence_by_variable_level[, c("lowerbound", "pointest", "upperbound") := lapply(.SD, function(x) { round(x*100, 1) }), .SDcols = c("lowerbound", "pointest", "upperbound") ]

# Ordering
setorderv(prevalence_by_variable_level, c("serotest", "prevalence_type", "variable", "variable_level")) 

# Save the object with computed prevalences for future use
save(prevalence_by_variable_level, file = "estimates/wave1.5/prevalence_by_variable_level.rdata", compress = "gzip")

############################
# A function that estimates the
# bivariate probit model and outputs the results
# We require a specific version of GJRM:
# remotes::install_version("GJRM", "0.2-2")
library(GJRM)

estimate_bivariate_selection <- function(data = serosurvey_data, subset_condition = NULL, selection_type = "2step", agreementvar = "agreed", clinicvisitvar = "agreed_and_tested", outcomevar = "IgG_testB", regressors = "age + male", semiparametric_age = T, exclusion_restriction_agreement = NULL, exclusion_restriction_clinicvisit = NULL, error_distribution = "N", surveyweightsvar = NULL) {

	# Debug: data = serosurvey_data; subset_condition = NULL; regressors = "agegroup + male"; semiparametric_age = F; exclusion_restriction_agreement = NULL; exclusion_restriction_clinicvisit = "offered_taxi"; error_distribution = "N"; clinicvisitvar = "agreed_and_tested"; outcomevar = "IgA_or_G_or_M_testC"; agreementvar = "agreed"; selection_type = "2step"; surveyweightsvar = "raking_weight"

	# Define selection and outcome equations
	if( grepl("age +", regressors, fixed = T) & semiparametric_age ) {

		agreement_equation <- as.formula(paste0(agreementvar, "~", 
											gsub("age +", "s(age) +", regressors, fixed = T),
											ifelse(!is.null(exclusion_restriction_agreement), paste0(" + ", exclusion_restriction_agreement), ""))
										)

		visit_equation <- as.formula(paste0(clinicvisitvar, "~", 
											gsub("age +", "s(age) +", regressors, fixed = T),
											ifelse(!is.null(exclusion_restriction_clinicvisit), paste0(" + ", exclusion_restriction_clinicvisit), ""))
										)

		outcome_equation <- as.formula(paste0(outcomevar, "~", gsub("age +", "s(age) +", regressors, fixed = T)))

	} else {

		agreement_equation <- as.formula(paste0(agreementvar, "~", 
											regressors,
											ifelse(!is.null(exclusion_restriction_agreement), paste0(" + ", exclusion_restriction_agreement), ""))
										)

		visit_equation <- as.formula(paste0(clinicvisitvar, "~", 
											regressors,
											ifelse(!is.null(exclusion_restriction_clinicvisit), paste0(" + ", exclusion_restriction_clinicvisit), ""))
										)

		outcome_equation <- as.formula(paste0(outcomevar, "~", regressors ))


	}

	# Subset fit
	if( !is.null(subset_condition) ) {

		eval(parse(text = paste0("data <- data[ ", subset_condition, " ]"))) 

	}

	# Testing the hypothesis of absence of non-random sample selection
	pvalue_sample_select_agreed <- LM.bpm(list(agreement_equation, outcome_equation), data, Model = "BSS")
	pvalue_sample_select_visited <- LM.bpm(list(visit_equation, outcome_equation), data, Model = "BSS")

	# Fit the model
	if( selection_type == "2step") {

		model_fit <- gjrm(list(visit_equation, 
								outcome_equation), 
								data = data,
								Model = "BSS", BivD = error_distribution, gamlssfit = F,
								margins = c("probit", "probit"))

	} else {

		model_fit <- gjrm(list(agreement_equation,
								visit_equation, 
								outcome_equation), 
								data = data,
								Model = "TSS", BivD = error_distribution, gamlssfit = F,
								margins = c("probit", "probit", "probit"))

	}

	# Add survey weights to model fit
	if( !is.null(surveyweightsvar) ) {
	
		# Attach survey weights to the model object
		model_fit$survey_weights_visit <- unname(unlist(data[as.numeric(rownames(model_fit$gam1$model))][, surveyweightsvar, with = F]))
		model_fit$survey_weights_outcome <- unname(unlist(data[as.numeric(rownames(model_fit$gam2$model))][, surveyweightsvar, with = F]))

	}

	# Output object
	output <- list(fit = model_fit, agreed_selection_p_value = pvalue_sample_select_agreed, visited_selection_p_value = pvalue_sample_select_visited)

	return(output)

}

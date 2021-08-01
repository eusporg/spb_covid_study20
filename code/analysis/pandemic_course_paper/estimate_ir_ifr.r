# This code computes the IR/IFR rates 
# for COVID-19 pandemic in Saint Petersburg, Russia
# paper with Bayesian evidence synthesis
# 
# Code builds on https://github.com/harlanhappydog/BayesianSeroMetaAnalysis
# by Campbell and Gustafson (2021, https://www.medrxiv.org/content/10.1101/2021.05.12.21256975v2)
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(binom)
library(rjags)
library(hBayesDM)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

#######
# Load and prepare the estimated prevalence across waves
# (see the estimation code in analysis/wave*/estimate_prevalence_by_{model,variable}.r)
waves <- c(1, 1.5, 2, 3)

# Init an object to store the results
# of prevalence computations
perwave_prevalence_by_model_list <- list()

# Load the per-wave estimates and store them in one object
for(w in waves) {

	load(paste0("estimates/wave", w, "/prevalence_by_model.rdata"))

	perwave_prevalence_by_model_list[[ as.character(w) ]] <- prevalence_by_model

	rm(prevalence_by_model)

}

# Aggregate the results
perwave_prevalence_by_model <- rbindlist(perwave_prevalence_by_model_list, idcol = "wave")

# We are interested in per-wave 95% confidence intervals for ELISA Coronapass
# adjusted for non-response and test characteristics 
model_data <- perwave_prevalence_by_model[ serotest == "IgA_or_G_or_M_testC" & prevalence_type == "univariate" & surveyweight == "no" & sensitivity != 1 & scenario == "basic", c("wave", "lowerbound", "upperbound")]
setnames(model_data, "wave", "k")

# Manually add wave start and end dates
model_data[ k == 1, start_date := ymd("2020-05-25") ]
model_data[ k == 1, end_date := ymd("2020-06-28") ]
model_data[ k == 1.5, start_date := ymd("2020-07-20") ]
model_data[ k == 1.5, end_date := ymd("2020-08-16") ]
model_data[ k == 2, start_date := ymd("2020-10-12") ]
model_data[ k == 2, end_date := ymd("2020-12-06") ]
model_data[ k == 3, start_date := ymd("2021-02-15") ]
model_data[ k == 3, end_date := ymd("2021-04-04") ]

# Manually add populations
## Adult population
model_data[ k %in% c(1, 1.5), p_k_adult := 4451025 ]
model_data[ k %in% c(2, 3), p_k_adult := 4439897 ]
# 
model_data[ k %in% c(1, 1.5), p_k_all := 5398064 ]
model_data[ k %in% c(2, 3), p_k_all := 5384342 ]

#######
# Find the parameters of the binomial distribution that
# correspond to a given CI for seroprevalence (with given tolerance)

# Compute CIs space with all combinations
# of cc_k and t_k and their CI
ci_space <- CJ(cc_k = 1:1000, t_k = 2:1000)
ci_space <- ci_space[ cc_k < t_k]
# NB: the below takes time and memory
ci_space[, c("lower", "upper") := binom::binom.bayes(x = cc_k, n = t_k, type = "central", conf.level = 0.95, maxit = 200)[, c("lower", "upper")]]

search_binomial_parameters <- function(search_space, lowerbound, upperbound) {
	
	# Euclidean distance from the target
	search_space[, eucl_dist := sqrt((lower - lowerbound)^2 + (upper - upperbound)^2)]
	# Sort from smallest distance
	setorderv(search_space, "eucl_dist", 1)
	# Return the parameters producing the smallest distance
	smallest_distance_params <- unlist(search_space[1, c("cc_k", "t_k")])
	return(smallest_distance_params)

}

# Run and assign the corresponding values
model_data[, c("cc_k", "t_k") := NA_real_]

for(i in 1:nrow(model_data) ) {

	search_results <- search_binomial_parameters(search_space = ci_space,
												lowerbound = model_data[i]$lowerbound/100,
												upperbound = model_data[i]$upperbound/100)

	set(model_data, i = i, j = "cc_k", value = search_results["cc_k"])
	set(model_data, i = i, j = "t_k", value = search_results["t_k"])

}

# Clean up
rm(ci_space)
gc()

#model_data[, c("k", "lowerbound", "upperbound", "t_k", "cc_k")]
#     k lowerbound upperbound t_k cc_k
#1:   1        7.7       11.7 827   79
#2: 1.5        9.9       16.6 385   50
#3:   2       20.3       25.5 999  228
#4:   3       39.7       48.0 550  241

#######
# Get the data on COVID or excess deaths

# Daily covid deaths from stopcoronavirus.rf
stopcoronavirus_deaths <- fread("https://raw.githubusercontent.com/alexei-kouprianov/COVID-19.SPb/main/data/primary/covid.SPb.stopkoronavirus.rf.txt")
stopcoronavirus_deaths <- stopcoronavirus_deaths[, c("TIME", "d")]
setnames(stopcoronavirus_deaths, c("TIME", "d"), c("day", "covid_deaths"))
setorderv(stopcoronavirus_deaths, "day")

# Monthly excess deaths from https://doi.org/10.1111/1740-9713.01486
excess_deaths <- fread("https://raw.githubusercontent.com/dkobak/excess-mortality/main/russia_excess_deaths.csv", encoding = "UTF-8")[ V1 == "Санкт-Петербург"]
excess_deaths <- melt(excess_deaths, id.vars = "V1", variable.name = "month", value.name = "excess_deaths", variable.factor = F)
excess_deaths[, V1 := NULL]
excess_deaths[, month := gsub("Январь ", "01.01.", month)]
excess_deaths[, month := gsub("Февраль ", "01.02.", month)]
excess_deaths[, month := gsub("Март ", "01.03.", month)]
excess_deaths[, month := gsub("Апрель ", "01.04.", month)]
excess_deaths[, month := gsub("Май ", "01.05.", month)]
excess_deaths[, month := gsub("Июнь ", "01.06.", month)]
excess_deaths[, month := gsub("Июль ", "01.07.", month)]
excess_deaths[, month := gsub("Август ", "01.08.", month)]
excess_deaths[, month := gsub("Сентябрь ", "01.09.", month)]
excess_deaths[, month := gsub("Октябрь ", "01.10.", month)]
excess_deaths[, month := gsub("Ноябрь ", "01.11.", month)]
excess_deaths[, month := gsub("Декабрь ", "01.12.", month)]
excess_deaths[, month := dmy(month)]
setorderv(excess_deaths, "month")

# Compute the respective D^upper/lower_k for each wave
for(i in 1:nrow(model_data) ) {

	wave_start <- model_data$start_date[i]
	wave_end <- model_data$end_date[i]

	d_deaths_lower <- sum(stopcoronavirus_deaths[ day < wave_start + 14 ]$covid_deaths, na.rm = T)
	d_deaths_upper <- sum(stopcoronavirus_deaths[ day < wave_end + 14 ]$covid_deaths, na.rm = T)
	
	d_excess_lower <- sum(excess_deaths[ month <= floor_date(wave_start, "month") ]$excess_deaths, na.rm = T)
	d_excess_upper <- sum(excess_deaths[ month <= floor_date(wave_end, "month") ]$excess_deaths, na.rm = T)

	set(model_data, i = i, j = "d_deaths_lower", value = d_deaths_lower)
	set(model_data, i = i, j = "d_deaths_upper", value = d_deaths_upper)

	set(model_data, i = i, j = "d_excess_lower", value = d_excess_lower)
	set(model_data, i = i, j = "d_excess_upper", value = d_excess_upper)

}

#######
# Build and estimate the evidence synthesis model
# (borrows from https://github.com/harlanhappydog/BayesianSeroMetaAnalysis)

# Number of periods in the data
K <- nrow(model_data)

# JAGS model likelihood and summary 
model_likelihood <- "

# Likelihood:
	
for(k in 1:K){
	cc[k] ~ dbin(ir[k], tests[k]);
	censor.index[k] ~ dinterval(deaths[k], c(deaths_lower[k], deaths_upper[k]))
	deaths[k] ~ dbin(ifr[k]*ir[k], pop[k]);
	cloglog(ir[k]) <- cloglog_ir[k];
	cloglog(ifr[k]) <- cloglog_ifr[k];
	
	cloglog_ir[k] ~ dnorm(beta, inv.var_sig);
	cloglog_ifr[k] ~ dnorm(theta, inv.var_tau);
	
}

# Summary:

g_IFR = theta;
IFR <- 1 - exp(-exp(g_IFR))
g_IR = beta;
IR <- 1 - exp(-exp(g_IR))
"

# Two types of priors
model_weakly_informative_priors <- "

# Weakly informative priors:
	icloglog_theta ~ dbeta(0.3, 30); 
	icloglog_beta  ~ dbeta(1, 30);
	theta <- log(-log(1-icloglog_theta));
	beta  <- log(-log(1-icloglog_beta));
	inv.var_sig   <- (1/sigma)^2 ;
	inv.var_tau   <- (1/tau)^2 ;
	sigma     ~ dnorm(0, 1/10) T(0,);
	tau     ~ dnorm(0, 1/10) T(0,);

"

model_non_informative_priors <- "

# Non-informative priors:
	icloglog_theta ~ dunif(0, 1); 
	icloglog_beta  ~ dunif(0, 1);
	theta <- log(-log(1-icloglog_theta));
	beta  <- log(-log(1-icloglog_beta));
	inv.var_sig   <- (1/sigma)^2 ;
	inv.var_tau   <- (1/tau)^2 ;
	sigma     ~ dnorm(0, 1/100) T(0,);
	tau     ~ dnorm(0, 1/100) T(0,);

"

# Gather into the full JAGS model statements
model_statement_weakly_priors <- paste0("model {", model_weakly_informative_priors, model_likelihood, "\n}")
model_statement_noninformative_priors <- paste0("model {", model_non_informative_priors, model_likelihood, "\n}")

# Build the models
## Weakly informative priors, official deaths
jags.model_weakly_priors_official_deaths <- jags.model(textConnection(model_statement_weakly_priors), 
	data = list(
		K = K,
		tests = model_data$t_k,
		cc = model_data$cc_k,
		pop = model_data$p_k_adult,
		deaths_lower = model_data$d_deaths_lower,
		deaths_upper = model_data$d_deaths_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data$d_deaths_lower, model_data$d_deaths_upper), 1, mean))
	)
)

## Weakly informative priors, excess deaths
jags.model_weakly_priors_excess_deaths <- jags.model(textConnection(model_statement_weakly_priors), 
	data = list(
		K = K,
		tests = model_data$t_k,
		cc = model_data$cc_k,
		pop = model_data$p_k_adult,
		deaths_lower = model_data$d_excess_lower,
		deaths_upper = model_data$d_excess_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data$d_excess_lower, model_data$d_excess_upper), 1, mean))
	)
)

# Non-informative priors, official deaths
jags.model_noninformative_priors_official_deaths <- jags.model(textConnection(model_statement_noninformative_priors), 
	data = list(
		K = K,
		tests = model_data$t_k,
		cc = model_data$cc_k,
		pop = model_data$p_k_adult,
		deaths_lower = model_data$d_deaths_lower,
		deaths_upper = model_data$d_deaths_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data$d_deaths_lower, model_data$d_deaths_upper), 1, mean))
	)
)

# Non-informative priors, excess deaths
jags.model_noninformative_priors_excess_deaths <- jags.model(textConnection(model_statement_noninformative_priors), 
	data = list(
		K = K,
		tests = model_data$t_k,
		cc = model_data$cc_k,
		pop = model_data$p_k_adult,
		deaths_lower = model_data$d_excess_lower,
		deaths_upper = model_data$d_excess_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data$d_excess_lower, model_data$d_excess_upper), 1, mean))
	)
)

## Weakly informative priors, excess deaths, all population
jags.model_weakly_priors_excess_deaths_all_pop <- jags.model(textConnection(model_statement_weakly_priors), 
	data = list(
		K = K,
		tests = model_data$t_k,
		cc = model_data$cc_k,
		pop = model_data$p_k_all,
		deaths_lower = model_data$d_excess_lower,
		deaths_upper = model_data$d_excess_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data$d_excess_lower, model_data$d_excess_upper), 1, mean))
	)
)

params <- c("theta", "ir", "ifr", "tau", "sigma", "IFR", "IR")

# Set the number of iterations
MCMCn <- 2e6

# Sample from the models

sample_weakly_priors_official_deaths <- coda.samples(jags.model_weakly_priors_official_deaths, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

sample_weakly_priors_excess_deaths <- coda.samples(jags.model_weakly_priors_excess_deaths, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

sample_noninformative_priors_official_deaths <- coda.samples(jags.model_noninformative_priors_official_deaths, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

sample_noninformative_priors_excess_deaths <- coda.samples(jags.model_noninformative_priors_excess_deaths, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

sample_weakly_priors_excess_deaths_all_pop <- coda.samples(jags.model_weakly_priors_excess_deaths_all_pop, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

# Save the estimation results
save(sample_weakly_priors_official_deaths, file = "estimates/pandemic_course_paper/sample_weakly_priors_official_deaths.rdata", compress = "gzip")
save(sample_weakly_priors_excess_deaths, file = "estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths.rdata", compress = "gzip")
save(sample_noninformative_priors_official_deaths, file = "estimates/pandemic_course_paper/sample_noninformative_priors_official_deaths.rdata", compress = "gzip")
save(sample_noninformative_priors_excess_deaths, file = "estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths.rdata", compress = "gzip")
save(sample_weakly_priors_excess_deaths_all_pop, file = "estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_all_pop.rdata", compress = "gzip")

#######
# Report the results
load("estimates/pandemic_course_paper/sample_weakly_priors_official_deaths.rdata")
load("estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths.rdata")
load("estimates/pandemic_course_paper/sample_noninformative_priors_official_deaths.rdata")
load("estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths.rdata")
load("estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_all_pop.rdata")

# Model summary function
summary_md_ci <- function(xx, samps) {
	c(	md = summary(samps)$quantiles[xx, "50%"], 
		lower = HDIofMCMC(unlist(((samps[[1]])[,xx])), credMass = 0.95)[1], 
		higher = HDIofMCMC(unlist(((samps[[1]])[,xx])), credMass = 0.95)[2]
	)
}

# Create the confidence intervals
ir_irf_results_object <- CJ(k = 1:K, type = c("ir", "ifr"), deaths = c("official", "excess"), priors = c("weakly", "noninformative"), population = c("", "_all_pop"))
ir_irf_results_object[, c("pointest", "lower", "upper", "ir_overall", "ir_overall_lower", "ir_overall_upper", "ifr_overall", "ifr_overall_lower", "ifr_overall_upper") := NA_real_ ] 

for(i in 1:nrow(ir_irf_results_object)) {

	# Compute the confidence intervals programmatically
	results_object_name <- paste0("sample_", ir_irf_results_object$priors[i], "_priors_", ir_irf_results_object$deaths[i], "_deaths", ir_irf_results_object$population[i])

	if( exists(results_object_name) ) {

		# Example: summary_md_ci('ir[1]', sample_weakly_priors_official_deaths)
		# Per-wave IR/IFR
		eval(parse(text = paste0("res <- summary_md_ci('", ir_irf_results_object$type[i], "[", ir_irf_results_object$k[i], "]', ", results_object_name, ")")))
		set(ir_irf_results_object, i = i, j = "pointest", value = res["md"])
		set(ir_irf_results_object, i = i, j = "lower", value = res["lower"])
		set(ir_irf_results_object, i = i, j = "upper", value = res["higher"])
		# Overall IR
		eval(parse(text = paste0("ir_overall <- summary_md_ci('IR', ", results_object_name, ")")))
		set(ir_irf_results_object, i = i, j = "ir_overall", value = ir_overall["md"])
		set(ir_irf_results_object, i = i, j = "ir_overall_lower", value = ir_overall["lower"])
		set(ir_irf_results_object, i = i, j = "ir_overall_upper", value = ir_overall["higher"])
		# Overall IFR
		eval(parse(text = paste0("ifr_overall <- summary_md_ci('IFR', ", results_object_name, ")")))
		set(ir_irf_results_object, i = i, j = "ifr_overall", value = ifr_overall["md"])
		set(ir_irf_results_object, i = i, j = "ifr_overall_lower", value = ifr_overall["lower"])
		set(ir_irf_results_object, i = i, j = "ifr_overall_upper", value = ifr_overall["higher"])

	}
}

# Fix the waves
ir_irf_results_object[, k := as.numeric(k)]
ir_irf_results_object[ k == 2, k := 1.5 ]
ir_irf_results_object[ k == 3, k := 2 ]
ir_irf_results_object[ k == 4, k := 3 ]

# To percentages and round
ir_irf_results_object[, c("pointest", "lower", "upper", "ir_overall", "ir_overall_lower", "ir_overall_upper", "ifr_overall", "ifr_overall_lower", "ifr_overall_upper") := lapply(.SD, function(x) round(100*x, 2)), .SDcols = c("pointest", "lower", "upper", "ir_overall", "ir_overall_lower", "ir_overall_upper", "ifr_overall", "ifr_overall_lower", "ifr_overall_upper")]

# Remove empty
ir_irf_results_object <- ir_irf_results_object[!is.na(pointest)]

# Save point
save(ir_irf_results_object, file = "estimates/pandemic_course_paper/ir_irf_results_object.rdata", compress = "gzip")

############################################################################
# Per agegroup-sex analysis

# Load the per-age group/sex seroprevalence estimates
# created by estimate_prevalence_by_agegroup_sex.r
load("estimates/pandemic_course_paper/prevalence_by_agegroup_sex.rdata")
model_data_agesex <- prevalence_by_agegroup_sex[ prevalence_type == "univariate" ]

# Create new CI search space
# with all combinations
# of cc_k and t_k and their CI
ci_space <- CJ(cc_k = 1:1000, t_k = 2:1000)
ci_space <- ci_space[ cc_k < t_k]
# NB: the below takes time and memory
ci_space[, c("lower", "upper") := binom::binom.bayes(x = cc_k, n = t_k, type = "central", conf.level = 0.95, maxit = 200)[, c("lower", "upper")]]

# Run and assign the corresponding values
model_data_agesex[, c("cc_k", "t_k") := NA_real_]

for(i in 1:nrow(model_data_agesex) ) {

	search_results <- search_binomial_parameters(search_space = ci_space,
												lowerbound = model_data_agesex[i]$lowerbound/100,
												upperbound = model_data_agesex[i]$upperbound/100)

	set(model_data_agesex, i = i, j = "cc_k", value = search_results["cc_k"])
	set(model_data_agesex, i = i, j = "t_k", value = search_results["t_k"])

}

# Add population data
model_data_agesex[ agegroup == "18-29" & male == 0, p_k := 419516]
model_data_agesex[ agegroup == "30-39" & male == 0, p_k := 543892]
model_data_agesex[ agegroup == "40-49" & male == 0, p_k := 432308]
model_data_agesex[ agegroup == "50-59" & male == 0, p_k := 418371]
model_data_agesex[ agegroup == "60-69" & male == 0, p_k := 437588]
model_data_agesex[ agegroup == "70+" & male == 0, p_k := 482856]

model_data_agesex[ agegroup == "18-29" & male == 1, p_k := 413098]
model_data_agesex[ agegroup == "30-39" & male == 1, p_k := 527930]
model_data_agesex[ agegroup == "40-49" & male == 1, p_k := 404853]
model_data_agesex[ agegroup == "50-59" & male == 1, p_k := 342243]
model_data_agesex[ agegroup == "60-69" & male == 1, p_k := 281789]
model_data_agesex[ agegroup == "70+" & male == 1, p_k := 241605]

# Add excess deaths data
model_data_agesex[ agegroup == "18-29" & male == 0, d_excess_lower := 0]
model_data_agesex[ agegroup == "18-29" & male == 0, d_excess_upper := 36]
model_data_agesex[ agegroup == "30-39" & male == 0, d_excess_lower := 57]
model_data_agesex[ agegroup == "30-39" & male == 0, d_excess_upper := 182]
model_data_agesex[ agegroup == "40-49" & male == 0, d_excess_lower := 167]
model_data_agesex[ agegroup == "40-49" & male == 0, d_excess_upper := 341]
model_data_agesex[ agegroup == "50-59" & male == 0, d_excess_lower := 367]
model_data_agesex[ agegroup == "50-59" & male == 0, d_excess_upper := 596]
model_data_agesex[ agegroup == "60-69" & male == 0, d_excess_lower := 1412]
model_data_agesex[ agegroup == "60-69" & male == 0, d_excess_upper := 1755]
model_data_agesex[ agegroup == "70+" & male == 0, d_excess_lower := 7679]
model_data_agesex[ agegroup == "70+" & male == 0, d_excess_upper := 8509]

model_data_agesex[ agegroup == "18-29" & male == 1, d_excess_lower := 0]
model_data_agesex[ agegroup == "18-29" & male == 1, d_excess_upper := 105]
model_data_agesex[ agegroup == "30-39" & male == 1, d_excess_lower := 161]
model_data_agesex[ agegroup == "30-39" & male == 1, d_excess_upper := 352]
model_data_agesex[ agegroup == "40-49" & male == 1, d_excess_lower := 528]
model_data_agesex[ agegroup == "40-49" & male == 1, d_excess_upper := 791]
model_data_agesex[ agegroup == "50-59" & male == 1, d_excess_lower := 856]
model_data_agesex[ agegroup == "50-59" & male == 1, d_excess_upper := 1167]
model_data_agesex[ agegroup == "60-69" & male == 1, d_excess_lower := 2111]
model_data_agesex[ agegroup == "60-69" & male == 1, d_excess_upper := 2527]
model_data_agesex[ agegroup == "70+" & male == 1, d_excess_lower := 5330]
model_data_agesex[ agegroup == "70+" & male == 1, d_excess_upper := 5928]

# Add the row index
model_data_agesex[, k := 1:.N ]

# Define the models
K <- nrow(model_data_agesex)

# Set the number of iterations
MCMCn <- 2e6

# Model parameters
params <- c("theta", "ir", "ifr", "tau", "sigma", "IFR", "IR")

## Weakly informative priors, excess deaths
jags.model_weakly_priors_excess_deaths_agesex <- jags.model(textConnection(model_statement_weakly_priors), 
	data = list(
		K = K,
		tests = model_data_agesex$t_k,
		cc = model_data_agesex$cc_k,
		pop = model_data_agesex$p_k,
		deaths_lower = model_data_agesex$d_excess_lower,
		deaths_upper = model_data_agesex$d_excess_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data_agesex$d_excess_lower, model_data_agesex$d_excess_upper), 1, mean))
	)
)

# Non-informative priors, excess deaths
jags.model_noninformative_priors_excess_deaths_agesex <- jags.model(textConnection(model_statement_noninformative_priors), 
	data = list(
		K = K,
		tests = model_data_agesex$t_k,
		cc = model_data_agesex$cc_k,
		pop = model_data_agesex$p_k,
		deaths_lower = model_data_agesex$d_excess_lower,
		deaths_upper = model_data_agesex$d_excess_upper,
		deaths = rep(NA, K),
		censor.index = rep(1, K)
	),
	n.chains = 5,
	n.adapt = 5000,
	inits = list(
		deaths = round(apply(cbind(model_data_agesex$d_excess_lower, model_data_agesex$d_excess_upper), 1, mean))
	)
)

# Sample from the models
sample_weakly_priors_excess_deaths_agesex <- coda.samples(jags.model_weakly_priors_excess_deaths_agesex, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

sample_noninformative_priors_excess_deaths_agesex <- coda.samples(jags.model_noninformative_priors_excess_deaths_agesex, params, 
				n.iter = MCMCn, thin = 100, n.adapt = round(MCMCn*0.20))

# Save the estimation results
save(sample_weakly_priors_excess_deaths_agesex, file = "estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_agesex.rdata", compress = "gzip")
save(sample_noninformative_priors_excess_deaths_agesex, file = "estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths_agesex.rdata", compress = "gzip")

#######
# Report the results
load("estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_agesex.rdata")
load("estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths_agesex.rdata")

# Model summary function
summary_md_ci <- function(xx, samps) {
	c(	md = summary(samps)$quantiles[xx, "50%"], 
		lower = HDIofMCMC(unlist(((samps[[1]])[,xx])), credMass = 0.95)[1], 
		higher = HDIofMCMC(unlist(((samps[[1]])[,xx])), credMass = 0.95)[2]
	)
}

# Create the confidence intervals
ir_irf_agesex_results_object <- CJ(k = 1:K, type = c("ir", "ifr"), deaths = c("excess"), priors = c("weakly", "noninformative"))
ir_irf_agesex_results_object[, c("pointest", "lower", "upper") := NA_real_ ] 

# Add agegroup and sex
ir_irf_agesex_results_object <- merge(ir_irf_agesex_results_object, model_data_agesex[, c("k", "agegroup", "male"), with = F], by = "k", all.x = T, all.y = F)

for(i in 1:nrow(ir_irf_agesex_results_object)) {

	# Compute the confidence intervals programmatically
	results_object_name <- paste0("sample_", ir_irf_agesex_results_object$priors[i], "_priors_", ir_irf_agesex_results_object$deaths[i], "_deaths_agesex")

	if( exists(results_object_name) ) {

		# Example: summary_md_ci('ir[1]', sample_weakly_priors_official_deaths)
		# Per-wave IR/IFR
		eval(parse(text = paste0("res <- summary_md_ci('", ir_irf_agesex_results_object$type[i], "[", ir_irf_agesex_results_object$k[i], "]', ", results_object_name, ")")))
		set(ir_irf_agesex_results_object, i = i, j = "pointest", value = res["md"])
		set(ir_irf_agesex_results_object, i = i, j = "lower", value = res["lower"])
		set(ir_irf_agesex_results_object, i = i, j = "upper", value = res["higher"])

	}
}

# To percentages and round
ir_irf_agesex_results_object[, c("pointest", "lower", "upper") := lapply(.SD, function(x) round(100*x, 2)), .SDcols = c("pointest", "lower", "upper")]

# Remove empty
ir_irf_agesex_results_object <- ir_irf_agesex_results_object[!is.na(pointest)]

# Save point
save(ir_irf_agesex_results_object, file = "estimates/pandemic_course_paper/ir_irf_agesex_results_object.rdata", compress = "gzip")

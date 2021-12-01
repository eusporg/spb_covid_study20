# This code creates the summary statistics tables
# for COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(lubridate)
library(stringi)
library(stringr)
library(stargazer)
library(fastDummies)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

#######
# Load and prepare the serosurvey data
# (see the estimation code in analysis/wave*/create_serosurvey_data_for_model_fit.r)

waves <- c(1, 1.5, 2, 3)

# Init an object to store the seroprevalence data
serosurvey_data_bywave_list <- list()

# Load the per-wave data and store them in one object
for(w in waves) {

	source(paste0("code/analysis/wave", w , "/create_serosurvey_data_for_model_fit.r"))
	
	# Make variables conformable across the waves
	if( w %in% c(1, 1.5) ) {

		serosurvey_data[, education_level := factor(education_level, ordered = F)]
		serosurvey_data[ education_level %in% c("Complete secondary education", "Primary education"), education_level := "Primary / secondary education"]
		serosurvey_data[, education_level := droplevels(education_level)]

	}

	setnames(serosurvey_data, "offered_taxi", "encouragement", skip_absent = T)
	setnames(serosurvey_data, "lenta_card", "encouragement", skip_absent = T)
	serosurvey_data[ , wave := NULL ]

	serosurvey_data_bywave_list[[ as.character(w) ]] <- serosurvey_data

	rm(serosurvey_data)

}

# Aggregate the results
serosurvey_data_bywave <- rbindlist(serosurvey_data_bywave_list, idcol = "wave", fill = T)
serosurvey_data_bywave[, wave := as.numeric(wave)]

# Full encouragement in wave 1.5
serosurvey_data_bywave[ wave %in% c(1.5), encouragement := 1]
# No encouragement in wave 3
serosurvey_data_bywave[ wave %in% c(3), encouragement := 0]

# Load paper survey data from wave 1 to get data on smoking
load("data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")
serosurvey_data_bywave <- merge(serosurvey_data_bywave, unique(paper_survey_data_matched_to_phone_survey_ids[, c("ID", "smoking_now")]), by = "ID", all.x = T, all.y = F)
serosurvey_data_bywave[ is.na(smoking) & !is.na(smoking_now) & wave %in% c(1, 1.5, 3), smoking := smoking_now ]
serosurvey_data_bywave[, smoking_now := NULL]

# Define regressors to include in the table
summary_stat_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + washing_hands_more + was_sick + selftested_covid + smoking + encouragement + was_vaccinated + IgG_testB + IgA_or_G_or_M_testC + IgG_or_M_testD"
summary_stat_regressors_list <- str_trim(unlist(strsplit(summary_stat_regressors, "+", fixed = T)))

# Create a new object with selected variabes
# and expand their dummies for each wave
## Wave 1: overall
wave1_overall <- serosurvey_data_bywave[wave == 1, summary_stat_regressors_list, with = F]
wave1_overall <- dummy_cols(wave1_overall, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

## Wave 1: tested
wave1_tested <- serosurvey_data_bywave[wave == 1 & agreed_and_tested == 1, summary_stat_regressors_list, with = F]
wave1_tested <- dummy_cols(wave1_tested, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

## Wave 1.5: tested
wave1.5_tested <- serosurvey_data_bywave[wave == 1.5 & agreed_and_tested == 1, summary_stat_regressors_list, with = F]
wave1.5_tested <- dummy_cols(wave1.5_tested, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

## Wave 2: overall
wave2_overall <- serosurvey_data_bywave[wave == 2, summary_stat_regressors_list, with = F]
wave2_overall <- dummy_cols(wave2_overall, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

## Wave 2: tested
wave2_tested <- serosurvey_data_bywave[wave == 2 & agreed_and_tested == 1, summary_stat_regressors_list, with = F]
wave2_tested <- dummy_cols(wave2_tested, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

## Wave 3: tested
wave3_tested <- serosurvey_data_bywave[wave == 3 & agreed_and_tested == 1, summary_stat_regressors_list, with = F]
wave3_tested <- dummy_cols(wave3_tested, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)

# Produce a set of tables with summary statistics
waves <- c("wave1_overall", "wave1_tested", "wave1.5_tested", "wave2_overall", "wave2_tested", "wave3_tested")

# Export the results to designated .tex files
for( w in waves ) {

	#stargazer(get(w), type = "latex", summary = T, out = paste0("temp/summary_statistics_", w, ".tex"), summary.stat = c("n", "mean"), float = T, multicolumn = F)

}

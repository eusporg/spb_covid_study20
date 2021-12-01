# This code compares the seroprevalence surveys and
# representative survey KOUZh-18 for
# COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(Publish)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load Сomprehensive monitoring of living
# conditions 2018 data for Saint Petersburg
load("data/kouzh_2018/kouzh_2018_data_spb.rdata")
kouzh_2018_data_spb[, male := NA_real_]
kouzh_2018_data_spb[sex == 1, male := 1]
kouzh_2018_data_spb[sex == 2, male := 0]

# Keep only adults in KOUZh data
kouzh_2018_data_spb <- kouzh_2018_data_spb[ age >= 18 ]

# Define age groups
kouzh_2018_data_spb[, agegroup := NA_character_]
kouzh_2018_data_spb[age < 35, agegroup := "18-34"]
kouzh_2018_data_spb[age >= 35 & age < 50, agegroup := "35-49"]
kouzh_2018_data_spb[age >= 50 & age < 65, agegroup := "50-64"]
kouzh_2018_data_spb[age >= 65, agegroup := "65+"]
kouzh_2018_data_spb[, agegroup := as.factor(agegroup)]

# Make education levels conformable
kouzh_2018_data_spb[ education_level %in% c("Primary education", "Complete secondary education"), education_level := "Primary / secondary education" ]
kouzh_2018_data_spb[, education_level := droplevels(education_level)] 

# Lives alone variable
kouzh_2018_data_spb[, lives_alone := 0]
kouzh_2018_data_spb[household_size == 1, lives_alone := 1]

# Load the prepared per-wave survey data
waves <- c(1, 2)

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

# Load paper survey data from wave 1 to get data on smoking
load("data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")
serosurvey_data_bywave <- merge(serosurvey_data_bywave, unique(paper_survey_data_matched_to_phone_survey_ids[, c("ID", "smoking_now")]), by = "ID", all.x = T, all.y = F)
serosurvey_data_bywave[ is.na(smoking) & !is.na(smoking_now) & wave %in% c(1, 1.5, 3), smoking := smoking_now ]
serosurvey_data_bywave[, smoking_now := NULL]

# Assume that all surveyed by phone have a cell phone
serosurvey_data_bywave[, has_cellphone := 1]

# Smoking now variable
setnames(kouzh_2018_data_spb, "smoking_now", "smoking")

# We need to expand factors by their levels
# NB: this results in a complete-case comparison 
called_wave1 <- model.matrix(~ male + age + agegroup + education_level + work_status + health_level + lives_alone + has_cellphone - 1, data = serosurvey_data_bywave[wave == 1], contrasts.arg = lapply(serosurvey_data_bywave[ wave == 1, sapply(serosurvey_data_bywave, is.factor), with = F], contrasts, contrasts = F))

smoking_wave1 <- model.matrix(~ smoking - 1, data = serosurvey_data_bywave[wave == 1], contrasts.arg = lapply(serosurvey_data_bywave[ wave == 1, sapply(serosurvey_data_bywave, is.factor), with = F], contrasts, contrasts = F))

called_wave2 <- model.matrix(~ male + age + agegroup + education_level + work_status + smoking + health_level + lives_alone + has_cellphone - 1, data = serosurvey_data_bywave[wave == 2], contrasts.arg = lapply(serosurvey_data_bywave[ wave == 2, sapply(serosurvey_data_bywave, is.factor), with = F], contrasts, contrasts = F))

kouzh <- model.matrix(~ male + age + agegroup + education_level + work_status + smoking + health_level + lives_alone + has_cellphone - 1, data = kouzh_2018_data_spb, contrasts.arg = lapply(kouzh_2018_data_spb[, sapply(kouzh_2018_data_spb, is.factor), with = F], contrasts, contrasts = F))

# Function that computes per-column
# variable means and their CIs
compute_column_means_with_ci <- function(mat) {

	# Multiple dummy variables by 100 (i.e. all except age) to arrive at percentages
	age_ind <- which(grepl("age$", colnames(mat)))
	if( !length(age_ind) == 0 ) {
		mat[, -age_ind] <- mat[, -age_ind]*100
	} else {
		mat <- mat*100
	}

	out <- lapply(apply(mat, 2, ci.mean), function(x) { data.table(mean = x$mean, lower = x$lower, upper = x$upper) })
	out <- rbindlist(out, id = "variable")
	# Add number of observations
	out <- rbind(out, data.table(variable = "N", mean = nrow(mat)), fill = T)
	return(out)

}

# Perform the computation
mean_computation_results <- rbind(
									data.table(type = "called_wave_1", compute_column_means_with_ci(called_wave1)),
									data.table(type = "smoking_wave_1", compute_column_means_with_ci(smoking_wave1)),
									data.table(type = "called_wave_2", compute_column_means_with_ci(called_wave2)),
									data.table(type = "kouzh", compute_column_means_with_ci(kouzh))

)

# Round variables to two digits
mean_computation_results[, c("mean", "lower", "upper") := lapply(.SD, function(x) { round(x, 1) } ), .SDcols = c("mean", "lower", "upper")]

# Add smoking data for wave 1
mean_computation_results <- mean_computation_results[ !( type == "smoking_wave_1" & variable == "N") ]
mean_computation_results[ type == "smoking_wave_1" , type := "called_wave_1" ]

# To latex code
## CIs to one variable
mean_computation_results[, ci := paste0("\\begin{footnotesize}(", lower, "; ", upper, ")\\end{footnotesize}")]

# Transform to a table with means and CIs underneath
mean_computation_results_long <- melt(mean_computation_results, id.vars = c("type", "variable"), measure.vars = c("mean", "ci"), variable.name = "indicator", value.name = "value")
mean_computation_results_wide <- dcast(mean_computation_results_long, variable + indicator ~ type, value.var = "value", fill = NA)

# Order rows manually
row_order <- c("male", "age", "agegroup18-34", "agegroup35-49", "agegroup50-64", "agegroup65+", "education_levelPrimary / secondary education", "education_levelSpecial secondary education", "education_levelHigher education", "work_status", "smoking", "health_levelVery good", "health_levelGood", "health_levelSatisfactory", "health_levelBad", "health_levelVery bad", "lives_alone", "has_cellphone", "N")

current_order <- match(row_order, mean_computation_results_wide$variable)
# Add CIs
target_order <- as.numeric(c(stri_split_fixed(paste(current_order, current_order + 1, sep = ",", collapse = ","), ",", simplify = T)))

mean_computation_results_output <- mean_computation_results_wide[target_order]
mean_computation_results_output <- mean_computation_results_output[!(variable == "N" & indicator == "ci")]
mean_computation_results_output[, indicator := NULL]

# Order columns
setcolorder(mean_computation_results_output, c("variable", "called_wave_1", "called_wave_2", "kouzh"))

# Add line break to last column
mean_computation_results_output[, kouzh := paste0(kouzh, " \\\\")]

# Rename colums
mean_computation_results_output[variable == "male", variable := "Male, \\%"]
mean_computation_results_output[variable == "age", variable := "Age, years"]
mean_computation_results_output[grepl("agegroup", variable), variable := paste0(gsub("agegroup", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[grepl("education_level", variable), variable := paste0(gsub("education_level", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[variable == "work_status", variable := "Employed, \\%"]
mean_computation_results_output[variable == "smoking", variable := "Smokes, \\%"]
mean_computation_results_output[grepl("health_level", variable), variable := paste0(gsub("health_level", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[variable == "lives_alone", variable := "Lives alone, \\%"]
mean_computation_results_output[variable == "has_cellphone", variable := "Has cellphone, \\%"]
mean_computation_results_output[variable == "consumes_alcohol", variable := "Consumes alcohol, \\%"]

# Remove CI variable names
mean_computation_results_output[duplicated(variable), variable := ""]

## Write to special temp file that will
## form the respective table after manual work
#fwrite(mean_computation_results_output, file = "temp/temp.tex", sep = "&", quote = F)

# Manually add dividers to the table:
# Education & \\
# Self-reported health status & \\
# \multicolumn{4}{l}{Self-reported smoking status (from paper-based survey)} & \\

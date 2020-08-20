library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(Publish)

setwd("~")

# Load Сomprehensive monitoring of living
# conditions 2018 data for Saint Petersburg
load("data/kouzh_2018/kouzh_2018_data_spb.rdata")
kouzh_2018_data_spb[, male := NA_real_]
kouzh_2018_data_spb[sex == 1, male := 1]
kouzh_2018_data_spb[sex == 2, male := 0]

# Keep only adults in KOUZh data
kouzh_2018_data_spb <- kouzh_2018_data_spb[ age >= 18 ]

# Load phone survey data from this study
load("data/phone_survey/phone_survey_data.rdata")

# Load paper survey data from this study
load("data/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata")

# Test results data contains all individuals that were contacted as of this date 
initial_phone_call_last_date <- "2020-06-24"
phone_survey_data <- phone_survey_data[interview_date <= initial_phone_call_last_date]

# Remove individuals from Lomonosovskiy and Vsevolozhskiy district as they are outside Saint Petersburg
phone_survey_data <- phone_survey_data[!(district %in% c("Lomonosovskiy District", "Vsevolozhskiy District"))]
# Remove individuals from districts in St. Petersburg that we have omitted from the study
phone_survey_data <- phone_survey_data[!(district %in% c("Kurortniy District", "Petrodvortsoviy District", "Pushkinskiy District", "Kolpinskiy District", "Kronshtadtskiy District", "Krasnoselskiy District"))]
# Remove individuals with missing districts
phone_survey_data <- phone_survey_data[!is.na(district)]

# Add "tested in the clinic" from test
load("data/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata")
phone_survey_data[, tested := 0]
phone_survey_data[ID %in% test_B_results_matched_to_phone_survey_ids$ID, tested := 1]

# Add paper survey data
phone_survey_data <- merge(phone_survey_data, paper_survey_data_matched_to_phone_survey_ids[, c("ID", "smoking_now", "smoking_earlier", "alcohol_frequency")], by = "ID", all.x = T, all.y = F)
phone_survey_data[, consumes_alcohol := NA_real_]
phone_survey_data[!is.na(alcohol_frequency), consumes_alcohol := 1]
phone_survey_data[alcohol_frequency %in% c("Never"), consumes_alcohol := 0]

# Assume that all surveyed by phone have a cell phone
phone_survey_data[, has_cellphone := 1]

# Define age groups
phone_survey_data[, agegroup := NA_character_]
phone_survey_data[age < 35, agegroup := "18-34"]
phone_survey_data[age >= 35 & age < 50, agegroup := "35-49"]
phone_survey_data[age >= 50 & age < 65, agegroup := "50-64"]
phone_survey_data[age >= 65, agegroup := "65+"]
phone_survey_data[, agegroup := as.factor(agegroup)]

kouzh_2018_data_spb[, agegroup := NA_character_]
kouzh_2018_data_spb[age < 35, agegroup := "18-34"]
kouzh_2018_data_spb[age >= 35 & age < 50, agegroup := "35-49"]
kouzh_2018_data_spb[age >= 50 & age < 65, agegroup := "50-64"]
kouzh_2018_data_spb[age >= 65, agegroup := "65+"]
kouzh_2018_data_spb[, agegroup := as.factor(agegroup)]

# Lives alone variable
kouzh_2018_data_spb[, lives_alone := 0]
kouzh_2018_data_spb[household_size == 1, lives_alone := 1]

# Smoking now or earlier variable
phone_survey_data[, smoking := NA_character_]
phone_survey_data[smoking_now == 1, smoking := "Smoking now"]
phone_survey_data[smoking_now == 0 & smoking_earlier==1, smoking := "Smoking earlier"]
phone_survey_data[smoking_now == 0 & smoking_earlier==0, smoking := "Never smoked"]
phone_survey_data[, smoking := factor(smoking, levels = c("Never smoked", "Smoking earlier", "Smoking now"), ordered = T) ]

kouzh_2018_data_spb[, smoking := NA_character_]
kouzh_2018_data_spb[smoking_now == 1, smoking := "Smoking now"]
kouzh_2018_data_spb[smoking_now == 0 & smoking_earlier==1, smoking := "Smoking earlier"]
kouzh_2018_data_spb[smoking_now == 0 & smoking_earlier==0, smoking := "Never smoked"]
kouzh_2018_data_spb[, smoking := factor(smoking, levels = c("Never smoked", "Smoking earlier", "Smoking now"), ordered = T) ]

# We need to expand factors by their levels
# NB: this results in a complete-case comparison 
called <- model.matrix(~ male + age + agegroup + education_level + work_status + health_level + lives_alone + has_cellphone - 1, data = phone_survey_data, contrasts.arg = lapply(phone_survey_data[, sapply(phone_survey_data, is.factor), with = F], contrasts, contrasts = F))

agreed <- model.matrix(~ male + age + agegroup + education_level + work_status + health_level + lives_alone + has_cellphone - 1, data = phone_survey_data[agreed == 1], contrasts.arg = lapply(phone_survey_data[, sapply(phone_survey_data, is.factor), with = F], contrasts, contrasts = F))

tested <- model.matrix(~ male + age + agegroup + education_level + work_status + health_level + lives_alone + has_cellphone - 1, data = phone_survey_data[tested == 1], contrasts.arg = lapply(phone_survey_data[, sapply(phone_survey_data, is.factor), with = F], contrasts, contrasts = F))

kouzh <- model.matrix(~ male + age + agegroup + education_level + work_status + health_level + lives_alone + has_cellphone + smoking + consumes_alcohol - 1, data = kouzh_2018_data_spb, contrasts.arg = lapply(kouzh_2018_data_spb[, sapply(kouzh_2018_data_spb, is.factor), with = F], contrasts, contrasts = F))

tested_paper_survey <- model.matrix(~ smoking + consumes_alcohol - 1 , data = phone_survey_data[tested == 1], contrasts.arg = lapply(phone_survey_data[, sapply(phone_survey_data, is.factor), with = F], contrasts, contrasts = F))

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
									data.table(type = "called", compute_column_means_with_ci(called)),
									data.table(type = "agreed", compute_column_means_with_ci(agreed)),
									data.table(type = "tested", compute_column_means_with_ci(tested)),
									data.table(type = "tested_paper_survey", compute_column_means_with_ci(tested_paper_survey)),
									data.table(type = "kouzh", compute_column_means_with_ci(kouzh))

)

# Keep the information on number of entries in paper-based survey
paper_based_n <- mean_computation_results[type == "tested_paper_survey" & variable == "N"]$mean

# Plug in variables from paper-based survey into tested column
mean_computation_results <- mean_computation_results[ !(type == "tested_paper_survey" & variable == "N")]
mean_computation_results[ type == "tested_paper_survey", type := "tested"]

# Round variables to two digits
mean_computation_results[, c("mean", "lower", "upper") := lapply(.SD, function(x) { round(x, 1) } ), .SDcols = c("mean", "lower", "upper")]

# To latex code
## CIs to one variable
mean_computation_results[, ci := paste0("\\begin{footnotesize}(", lower, "; ", upper, ")\\end{footnotesize}")]

# Transform to a table with means and CIs underneath
mean_computation_results_long <- melt(mean_computation_results, id.vars = c("type", "variable"), measure.vars = c("mean", "ci"), variable.name = "indicator", value.name = "value")
mean_computation_results_wide <- dcast(mean_computation_results_long, variable + indicator ~ type, value.var = "value", fill = NA)

# Order rows manually
row_order <- c("male", "age", "agegroup18-34", "agegroup35-49", "agegroup50-64", "agegroup65+", "education_levelPrimary education", "education_levelComplete secondary education", "education_levelSpecial secondary education", "education_levelHigher education", "work_status", "health_levelVery good", "health_levelGood", "health_levelSatisfactory", "health_levelBad", "health_levelVery bad", "lives_alone", "has_cellphone", "smokingNever smoked", "smokingSmoking earlier", "smokingSmoking now", "consumes_alcohol", "N")

current_order <- match(row_order, mean_computation_results_wide$variable)
# Add CIs
target_order <- as.numeric(c(stri_split_fixed(paste(current_order, current_order + 1, sep = ",", collapse = ","), ",", simplify = T)))

mean_computation_results_output <- mean_computation_results_wide[target_order]
mean_computation_results_output <- mean_computation_results_output[!(variable == "N" & indicator == "ci")]
mean_computation_results_output[, indicator := NULL]

# Add data on number of observations from the paper-based survey
mean_computation_results_output <- rbind(mean_computation_results_output, data.table(variable = "N (paper-based survey)", tested = paste0(paper_based_n)), fill = T)

# Order columns
setcolorder(mean_computation_results_output, c("variable", "called", "agreed", "tested", "kouzh"))

# Add line break to last column
mean_computation_results_output[, kouzh := paste0(kouzh, " \\\\")]

# Rename colums
mean_computation_results_output[variable == "male", variable := "Male, \\%"]
mean_computation_results_output[variable == "age", variable := "Age, years"]
mean_computation_results_output[grepl("agegroup", variable), variable := paste0(gsub("agegroup", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[grepl("education_level", variable), variable := paste0(gsub("education_level", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[variable == "work_status", variable := "Employed, \\%"]
mean_computation_results_output[grepl("health_level", variable), variable := paste0(gsub("health_level", "\\indentintable ", variable, fixed = T), ", \\%") ]
mean_computation_results_output[variable == "lives_alone", variable := "Lives alone, \\%"]
mean_computation_results_output[variable == "has_cellphone", variable := "Has cellphone, \\%"]
mean_computation_results_output[variable == "consumes_alcohol", variable := "Consumes alcohol, \\%"]
mean_computation_results_output[variable == "smokingNever smoked", variable := "\\indentintable Never smoked, \\%"]
mean_computation_results_output[variable == "smokingSmoking earlier", variable := "\\indentintable Smoking earlier, \\%"]
mean_computation_results_output[variable == "smokingSmoking now", variable := "\\indentintable Smoking now, \\%"]

# Remove CI variable names
mean_computation_results_output[duplicated(variable), variable := ""]

## Write to special temp file that will
## form the respective table after manual work
fwrite(mean_computation_results_output, file = "temp.tex", sep = "&", quote = F)

# Manually add dividers to the table:
# Education & \\
# Self-reported health status & \\
# \multicolumn{4}{l}{Self-reported smoking status (from paper-based survey)} & \\

# This code creates the antibody kinetics chart
# for COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(lubridate)
library(stringi)
library(stringr)
library(ggplot2)
library(ggthemes)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
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

# Locate individuals that were present in multiple waves with available test results
manywave_individuals <- unique(serosurvey_data_bywave[!is.na(IgA_or_G_or_M_testC_quantitative), .N, by = "ID"][N > 1]$ID)

# Examples:
#serosurvey_data_bywave[ ID == "0031505b41cc9f11" ]
#serosurvey_data_bywave[ ID == "012f425f700c90e0" ]

# Get the data on those individuals
antibody_kinetics_data <- serosurvey_data_bywave[ ID %in% manywave_individuals & !is.na(draw_sample_date), c("wave", "ID", "male", "age", "selftested_covid", "selftested_covid_positive", "was_hospitalized_covid", "was_vaccinated", "vaccine_doses_received", "draw_sample_date", "IgA_or_G_or_M_testC", "IgA_or_G_or_M_testC_quantitative")]
# Remove duplicates (due to erroneous matches of test results to phone survey IDs in wave 3)
antibody_kinetics_data <- unique(antibody_kinetics_data, by =  c("ID", "wave"))
setorderv(antibody_kinetics_data, c("ID", "wave"))

# Days elapsed from first test
antibody_kinetics_data[, first_draw_sample_date := min(draw_sample_date), by = "ID"]
antibody_kinetics_data[, days_from_first_sample := as.numeric(draw_sample_date - first_draw_sample_date)]

# Make no COVID positive results for those who were not tested
antibody_kinetics_data[ selftested_covid == 0, selftested_covid_positive := 0]
antibody_kinetics_data[ selftested_covid == 0, was_hospitalized_covid := 0]

# Make everybody unvaccinated in the waves before vaccine availability
antibody_kinetics_data[ wave %in% c(1, 1.5, 2), was_vaccinated := 0]
antibody_kinetics_data[ wave %in% c(1, 1.5, 2), vaccine_doses_received := 0]

# Zero doses received for unvaccinated in wave 3
antibody_kinetics_data[ wave == 3 & was_vaccinated == 0, vaccine_doses_received := 0]

# Construct wave-invariate variables
# (e.g. if a person claims to have been selftested COVID-positive
# in any wave we mark him/her as positive in all the waves)
antibody_kinetics_data[, selftested_covid_positive_anywave := max(selftested_covid_positive, na.rm = T), by = "ID"]
antibody_kinetics_data[ !is.finite(selftested_covid_positive_anywave), selftested_covid_positive_anywave := NA]

antibody_kinetics_data[, was_hospitalized_covid_anywave := max(was_hospitalized_covid, na.rm = T), by = "ID"]
antibody_kinetics_data[ !is.finite(was_hospitalized_covid_anywave), was_hospitalized_covid_anywave := NA]

antibody_kinetics_data[, was_vaccinated_anywave := max(was_vaccinated, na.rm = T), by = "ID"]
antibody_kinetics_data[ !is.finite(was_vaccinated_anywave), was_vaccinated_anywave := NA]

antibody_kinetics_data[, vaccine_doses_received_anywave := max(vaccine_doses_received, na.rm = T), by = "ID"]
antibody_kinetics_data[ !is.finite(vaccine_doses_received_anywave), vaccine_doses_received_anywave := NA]

# Create a variable indicating that individial was positive in his/her first wave
antibody_kinetics_data[, IgA_or_G_or_M_testC_positive_firsttime := min(IgA_or_G_or_M_testC, na.rm = T), by = "ID"]
antibody_kinetics_data[ !is.finite(IgA_or_G_or_M_testC_positive_firsttime), IgA_or_G_or_M_testC_positive_firsttime := NA]

# Create group identifiers
#antibody_kinetics_data[, group := NA_character_]

# Plot the kinetics chart
antibody_kinetics_plot <- ggplot(antibody_kinetics_data[IgA_or_G_or_M_testC_positive_firsttime == 1 & !(wave %in% c(1.5))], aes( x = days_from_first_sample, y = IgA_or_G_or_M_testC_quantitative) ) +
								geom_line(lwd = 0.5, aes(group = ID), color = "black", alpha = 0.15) +
								geom_smooth(method = "loess", fill = "#00468BFF") +
								geom_hline(aes(yintercept = 1), color = "#ED0000FF", linetype = "dotted") +
								labs(x = "Days elapsed since first draw sample date for test subject", y = "ELISA Coronapass (log scale)") +
								scale_y_continuous(trans = 'log10') +
								theme_minimal() + scale_fill_lancet()
								theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 12), text = element_text(size = 12), legend.position = "none")

# Return the plot
#cairo_pdf("pandemic_course_paper/media/antibody_kinetics_excl_wave1.5.pdf", height = 7, width = 12)
#antibody_kinetics_plot
#dev.off()

# This code creates the seroprevalence tables and
# figures for COVID-19 pandemic in Saint Petersburg, Russia
# paper
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

#######
# Load and prepare the estimated prevalence across waves
# (see the estimation code in analysis/wave*/estimate_prevalence_by_{model,variable}.r)

waves <- c(1, 1.5, 2, 3)

# Init an object to store the results
# of prevalence computations
perwave_prevalence_by_model_list <- list()
perwave_prevalence_by_variable_level_list <- list()

# Load the per-wave estimates and store them in one object
for(w in waves) {

	load(paste0("estimates/wave", w, "/prevalence_by_model.rdata"))
	load(paste0("estimates/wave", w, "/prevalence_by_variable_level.rdata"))

	perwave_prevalence_by_model_list[[ as.character(w) ]] <- prevalence_by_model
	perwave_prevalence_by_variable_level_list[[ as.character(w) ]] <- prevalence_by_variable_level

	rm(prevalence_by_model)
	rm(prevalence_by_variable_level)

}

# Aggregate the results
perwave_prevalence_by_model <- rbindlist(perwave_prevalence_by_model_list, idcol = "wave")
perwave_prevalence_by_variable_level <- rbindlist(perwave_prevalence_by_variable_level_list, idcol = "wave")

# Define a variable for test adjustment
perwave_prevalence_by_model[ sensitivity == 1 & specificity == 1, test_adjustment := "unadjusted" ]
perwave_prevalence_by_model[ sensitivity != 1 & specificity == 1, test_adjustment := "adjusted" ]

perwave_prevalence_by_variable_level[ sensitivity == 1 & specificity == 1, test_adjustment := "unadjusted" ]
perwave_prevalence_by_variable_level[ sensitivity != 1 & specificity == 1, test_adjustment := "adjusted" ]

##############
# Table tab:seroprevalence_table

seroprevalence_table_data <- perwave_prevalence_by_model[ scenario == "basic" & serotest == "IgA_or_G_or_M_testC" & prevalence_type %in% c("naive", "univariate") & test_adjustment %in% c("adjusted", "unadjusted") & surveyweight == "no" ]

## Column 1
seroprevalence_table_data[ prevalence_type == "naive" & test_adjustment == "unadjusted", c("wave", "pointest", "lowerbound", "upperbound", "n_selection", "n_outcome")]  

## Column 2
seroprevalence_table_data[ prevalence_type == "univariate" & test_adjustment == "unadjusted", c("wave", "pointest", "lowerbound", "upperbound", "n_selection", "n_outcome")]  

## Column 3
seroprevalence_table_data[ prevalence_type == "univariate" & test_adjustment == "adjusted", c("wave", "pointest", "lowerbound", "upperbound", "n_selection", "n_outcome")]  

##############
# Table tab:seroprevalence_table_robustness

seroprevalence_table_robustness_data <- perwave_prevalence_by_model[ scenario == "basic" & serotest %in% c("IgG_testB", "IgA_or_G_or_M_testC", "IgG_or_M_testD") & prevalence_type %in% c("naive", "univariate") & test_adjustment %in% c("adjusted", "unadjusted") & surveyweight %in% c("no", "raking") ]

seroprevalence_table_robustness_pointest <- dcast(seroprevalence_table_robustness_data[ test_adjustment == "adjusted"], wave ~ serotest + prevalence_type + surveyweight, value.var = c("pointest"))
seroprevalence_table_robustness_lowerbound <- dcast(seroprevalence_table_robustness_data[ test_adjustment == "adjusted"], wave ~ serotest + prevalence_type + surveyweight, value.var = c("lowerbound"))
seroprevalence_table_robustness_upperbound <- dcast(seroprevalence_table_robustness_data[ test_adjustment == "adjusted"], wave ~ serotest + prevalence_type + surveyweight, value.var = c("upperbound"))

##############
# Table tab:seroprevalence_by_subgroup

seroprevalence_by_subgroup_data <- perwave_prevalence_by_variable_level[ serotest %in% c( "IgA_or_G_or_M_testC") & prevalence_type %in% c("naive", "univariate") & test_adjustment %in% c("adjusted") & surveyweight %in% c("no") & !(variable %in% c("district", "draw_week", "draw_biweek", "interview_week"))]
seroprevalence_by_subgroup_data[ lowerbound < 0, lowerbound := 0 ]
seroprevalence_by_subgroup_data[ upperbound < 0, upperbound := 0 ]
seroprevalence_by_subgroup_data[ pointest == 0, c("lowerbound", "upperbound", "pointest") := NA]
seroprevalence_by_subgroup_data[, display_seroprevalence := paste0(pointest, " (", lowerbound, "--", upperbound, ")")]
seroprevalence_by_subgroup_data[ is.na(pointest), display_seroprevalence := "---"]

# Convert to tables to display
seroprevalence_by_subgroup_table_naive <- dcast(seroprevalence_by_subgroup_data[ prevalence_type == "naive"], variable + variable_level ~ wave, value.var = c("n_outcome", "display_seroprevalence"))
seroprevalence_by_subgroup_table_univariate <- dcast(seroprevalence_by_subgroup_data[ prevalence_type == "univariate"], variable + variable_level ~ wave, value.var = c("n_outcome", "display_seroprevalence"))
setcolorder(seroprevalence_by_subgroup_table_naive, c("variable", "variable_level", "n_outcome_1", "display_seroprevalence_1", "n_outcome_1.5", "display_seroprevalence_1.5", "n_outcome_2", "display_seroprevalence_2", "n_outcome_3", "display_seroprevalence_3"))
setcolorder(seroprevalence_by_subgroup_table_univariate, c("variable", "variable_level", "n_outcome_1", "display_seroprevalence_1", "n_outcome_1.5", "display_seroprevalence_1.5", "n_outcome_2", "display_seroprevalence_2", "n_outcome_3", "display_seroprevalence_3"))
# NAs to em-dashes
seroprevalence_by_subgroup_table_naive <- seroprevalence_by_subgroup_table_naive[, lapply(.SD, function(x) { ifelse(is.na(x), "---", x) })]
seroprevalence_by_subgroup_table_univariate <- seroprevalence_by_subgroup_table_univariate[, lapply(.SD, function(x) { ifelse(is.na(x), "---", x) })]

# Write to csv tables for subsequent latex import
#fwrite(seroprevalence_by_subgroup_table_naive, file = "temp/seroprevalence_by_subgroup_table_naive.csv")
#fwrite(seroprevalence_by_subgroup_table_univariate, file = "temp/seroprevalence_by_subgroup_table_univariate.csv")

##############
# Figure fig:seroprevalence_time

# Define start and end dates for all charts
start_date <- ymd("2020-03-01")
end_date <- ymd("2021-05-31")
start_end_range <- c(start_date, end_date)

# Convert date range to week numbers
start_end_range_weeknumbers <- lapply(start_end_range, isoweek)
# Add 2020 weeks to 2021
start_end_range_weeknumbers[[2]] <- start_end_range_weeknumbers[[2]] + 53

# Data for the plot
## Naive prevalence by draw sample week 
prevalence_by_draw_sample_week <- perwave_prevalence_by_variable_level[ serotest %in% c("IgG_testB", "IgA_or_G_or_M_testC", "IgG_or_M_testD") & prevalence_type == "naive" & variable == "draw_week" & test_adjustment == "adjusted" ]
setnames(prevalence_by_draw_sample_week, "variable_level", "week")

# Proper week names
prevalence_by_draw_sample_week[ week == "October 5-11", week := 41 ]
prevalence_by_draw_sample_week[ week == "October 12-18", week := 42]
prevalence_by_draw_sample_week[ week == "October 19-25", week := 43 ]
prevalence_by_draw_sample_week[ week == "October 26 - November 1", week := 44]
prevalence_by_draw_sample_week[ week == "November 2-8", week := 45]
prevalence_by_draw_sample_week[ week == "November 9-15", week := 46]
prevalence_by_draw_sample_week[ week == "November 16-22", week := 47]
prevalence_by_draw_sample_week[ week == "November 23-29", week := 48]
prevalence_by_draw_sample_week[ week == "November 30 - December 6", week := 49]

prevalence_by_draw_sample_week[ week == "February 8-14, 2021", week := 53 + 6 ]
prevalence_by_draw_sample_week[ week == "February 15-21, 2021", week := 53 + 7]
prevalence_by_draw_sample_week[ week == "February 22-28, 2021", week := 53 + 8 ]
prevalence_by_draw_sample_week[ week == "March 1-7, 2021", week := 53 + 9]
prevalence_by_draw_sample_week[ week == "March 8-14, 2021", week := 53 + 10]
prevalence_by_draw_sample_week[ week == "March 15-21, 2021", week := 53 + 11]
prevalence_by_draw_sample_week[ week == "March 22-28, 2021", week := 53 + 12]
prevalence_by_draw_sample_week[ week == "March 29 - April 4, 2021", week := 53 + 13]
prevalence_by_draw_sample_week[ week == "April 5-11, 2021", week := 53 + 14]

# Add data on overall prevalence per wave
prevalence_by_draw_sample_week[, week := as.numeric(week)]
prevalence_by_draw_sample_week[week >= 22 & week <= 26, wave := 1 ]
prevalence_by_draw_sample_week[week >= 29 & week <= 33, wave := 1.5 ]
prevalence_by_draw_sample_week[week >= 41 & week <= 49, wave := 2 ]
prevalence_by_draw_sample_week[week >= 59 & week <= 67, wave := 3 ]

## Add the all-wave adjusted prevalence estimates
prevalence_waves_long <- perwave_prevalence_by_model[ scenario == "basic" & prevalence_type == "univariate" & serotest %in% c("IgG_testB", "IgA_or_G_or_M_testC", "IgG_or_M_testD") & surveyweight == "no" & test_adjustment == "adjusted" ]

prevalence_waves <- dcast(prevalence_waves_long, serotest + wave ~ ., value.var = c("lowerbound", "pointest", "upperbound"))
setnames(prevalence_waves, c("lowerbound", "pointest", "upperbound"), paste0(c("lowerbound", "pointest", "upperbound"), "_univariate"))

# Add the all-wave data
prevalence_by_draw_sample_week <- merge(prevalence_by_draw_sample_week, prevalence_waves, by = c("serotest", "wave"), all.x = T)

# Rename the tests
prevalence_by_draw_sample_week[serotest == "IgA_or_G_or_M_testC", serotest := "ELISA Coronapass"]
prevalence_by_draw_sample_week[serotest == "IgG_testB", serotest := "CMIA Abbott"]
prevalence_by_draw_sample_week[serotest == "IgG_or_M_testD", serotest := "ELISA Vector"]

# Remove rare weeks
prevalence_by_draw_sample_week <- prevalence_by_draw_sample_week[ n_outcome > 30 ]

# Add intervals for weeks where there was no serosurvey
#empty_weeks <- data.table(expand.grid(serotest = unique(prevalence_by_draw_sample_week$serotest), week = min(prevalence_by_draw_sample_week$week):max(prevalence_by_draw_sample_week$week)))
empty_weeks <- data.table(expand.grid(serotest = unique(prevalence_by_draw_sample_week$serotest), week = start_end_range_weeknumbers[[1]]:start_end_range_weeknumbers[[2]]))

empty_weeks <- empty_weeks[ !(week %in% prevalence_by_draw_sample_week$week) ]

prevalence_by_draw_sample_week <- rbind(prevalence_by_draw_sample_week, empty_weeks, fill = T)
setorderv(prevalence_by_draw_sample_week, c("week", "serotest"))

# Week labels
prevalence_by_draw_sample_week[ week == 22, week_label := "May 25-31"]
prevalence_by_draw_sample_week[ week == 23, week_label := "Jun 1-7"]
prevalence_by_draw_sample_week[ week == 24, week_label := "Jun 8-14"]
prevalence_by_draw_sample_week[ week == 25, week_label := "Jun 15-21"]
prevalence_by_draw_sample_week[ week == 26, week_label := "Jun 22-28"]
prevalence_by_draw_sample_week[ week == 30, week_label := "Jul 20-26"]
prevalence_by_draw_sample_week[ week == 31, week_label := "Jul 27\nAug 2"]
prevalence_by_draw_sample_week[ week == 32, week_label := "Aug 3-9"]
prevalence_by_draw_sample_week[ week == 33, week_label := "Aug 10-16"]
prevalence_by_draw_sample_week[ week == 41, week_label := "Oct 5-11"]
prevalence_by_draw_sample_week[ week == 42, week_label := "Oct 12-18"]
prevalence_by_draw_sample_week[ week == 43, week_label := "Oct 19-25"]
prevalence_by_draw_sample_week[ week == 44, week_label := "Oct 26\nNov 1"]
prevalence_by_draw_sample_week[ week == 45, week_label := "Nov 2-8"]
prevalence_by_draw_sample_week[ week == 46, week_label := "Nov 9-15"]
prevalence_by_draw_sample_week[ week == 47, week_label := "Nov 16-22"]
prevalence_by_draw_sample_week[ week == 48, week_label := "Nov 23-29"]
prevalence_by_draw_sample_week[ week == 49, week_label := "Nov 30\nDec 6"]
prevalence_by_draw_sample_week[ week == 53 + 6, week_label := "Feb 8-14"]
prevalence_by_draw_sample_week[ week == 53 + 7, week_label := "Feb 15-21"]
prevalence_by_draw_sample_week[ week == 53 + 8, week_label := "Feb 22-28"]
prevalence_by_draw_sample_week[ week == 53 + 9, week_label := "Mar 1-7"]
prevalence_by_draw_sample_week[ week == 53 + 10, week_label := "Mar 8-14"]
prevalence_by_draw_sample_week[ week == 53 + 11, week_label := "Mar 15-21"]
prevalence_by_draw_sample_week[ week == 53 + 12, week_label := "Mar 22-28"]
prevalence_by_draw_sample_week[ week == 53 + 13, week_label := "Mar 29\nApr 4"]
prevalence_by_draw_sample_week[ week == 53 + 14, week_label := "Apr 5-11"]

# Empty weeks
prevalence_by_draw_sample_week[ is.na(week_label), week_label := ""]

# Fix lowerbounds
prevalence_by_draw_sample_week[ lowerbound < 0, lowerbound := 0 ]

## Plot by draw sample week with univariate results
prevalence_plot_data <- prevalence_by_draw_sample_week[ serotest %in% c("ELISA Coronapass", NA) ]
perweek_labels <- unique(prevalence_plot_data[, c("week", "week_label")])

naive_vs_univariate_prevalence_plot <- ggplot(prevalence_plot_data[ week > 21 & week < 67 ], aes(x = week, y = pointest, color = serotest)) +
											geom_point(size = 2, position = position_dodge(width=0.5)) +
											geom_pointrange(aes(ymin = lowerbound, ymax = upperbound), lwd = 0.3, position = position_dodge(width=0.5)) +
											# Adjusted prevalence for the entire wave
											geom_line(aes(y = pointest_univariate, group = serotest), linetype = "solid", lwd = 2) +
											geom_ribbon(aes(ymin = lowerbound_univariate, ymax = upperbound_univariate, group = serotest), alpha = .3, colour = NA, fill = "grey70") +
											geom_text(aes(label = paste0(pointest, "%")), hjust = 1.2, vjust = 0.2, size = 3, position = position_dodge(width=0.5)) +
											scale_y_continuous(name = "Seroprevalence, % (dots: naïve, solid line: adj. for non-response + 95% CI)", limits = c(0, max(prevalence_by_draw_sample_week$upperbound))) +
											scale_x_continuous(	name = "Week of blood sample draw",
																breaks = perweek_labels$week, labels = perweek_labels$week_label, 
																expand = c(0.05, 0), guide = guide_axis(n.dodge = 4)) +
											labs(color = "test") +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 12), text = element_text(size = 12), legend.position = "none")

# Return the plot
#cairo_pdf("pandemic_course_paper/media/seroprevalence_time.pdf", height = 7, width = 12)
#naive_vs_univariate_prevalence_plot
#dev.off()

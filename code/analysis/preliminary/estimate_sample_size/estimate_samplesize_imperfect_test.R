# This code assumes binomial distribution of # of infected
# individuals and computes sampling errors under perfect
# and imperfect screening tests
library(data.table)
library(ggplot2)
library(scales)
library(ggthemes)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load function from 
# Reiczigel, Földi and Ózsvári (2010) Exact confidence limits for 
# prevalence of a disease with an imperfect diagnostic test, 
# Epidemiology and infection, 138: 1674-1678.
# http://www2.univet.hu/users/jreiczig/CI4prevSeSp/R_function_ci4prev.txt
source("code/analysis/preliminary/estimate_sample_size/R_function_ci4prev.R")

compute_sampling_error <- function(sample_size, prevalence, se, sp) {

	res <- ci4prev(n = sample_size, poz = ceil(sample_size*prevalence), se = se, sp = sp, method = "wi", dec = 9)
	sampling_error <- (max(res)-min(res))/2
	sampling_error

}

# Conservative case, perfect test
compute_sampling_error(sample_size = 2397, prevalence = 0.5, se = 1, sp = 1)

# 20% prevalence, perfect test
compute_sampling_error(sample_size = 1550, prevalence = 0.2, se = 1, sp = 1)

# 20% prevalence;, Abbott test
compute_sampling_error(sample_size = 1550, prevalence = 0.2, se = 1, sp = 0.996)

# 20% prevalence; Genetico test
compute_sampling_error(sample_size = 1550, prevalence = 0.2, se = 0.96, sp = 1)

# 10% prevalence, perfect test
compute_sampling_error(sample_size = 1550, prevalence = 0.1, se = 1, sp = 1)

# 10% prevalence;, Abbott test
compute_sampling_error(sample_size = 1550, prevalence = 0.1, se = 1, sp = 0.996)

# 10% prevalence; Genetico test
compute_sampling_error(sample_size = 1550, prevalence = 0.1, se = 0.96, sp = 1)

# Sample size yielding 2% sampling error, Abbott
compute_sampling_error(sample_size = 882, prevalence = 0.1, se = 1, sp = 0.996)

###########
# Compute sample-size - error margin gradient

sampling_error_estimation_list <- list()

for( n in seq(100, 3000, 50) ) {
	
	# Gather scenario outputs
	out1 <- data.table(type = "Ideal test, conservative case (hypothetical prevalence 50%)", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.5, se = 1, sp = 1))
	#out2 <- data.table(type = "Ideal test, hypothetical prevalence 20%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.2, se = 1, sp = 1))
	#out3 <- data.table(type = "Ideal test, hypothetical prevalence 10%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.1, se = 1, sp = 1))
	out4 <- data.table(type = "Abbott Sars-CoV-2 IgG Test, hypothetical prevalence 20%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.2, se = 1, sp = 0.995))
	out5 <- data.table(type = "Abbott Sars-CoV-2 IgG Test, hypothetical prevalence 10%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.1, se = 1, sp = 0.995))
	out6 <- data.table(type = "Genetico CoronaPass Total Ig Test, hypothetical prevalence 20%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.2, se = 0.96, sp = 1))
	out7 <- data.table(type = "Genetico CoronaPass Total Ig Test, hypothetical prevalence 10%", n = n, sampling_error = compute_sampling_error(sample_size = n, prevalence = 0.1, se = 0.96, sp = 1))

	sampling_error_estimation_list[[as.character(n)]] <- rbind(out1, out4, out5, out6, out7) 

}

# Gather into one object
sampling_error_estimation <- rbindlist(sampling_error_estimation_list, idcol = F)

# Create a plot with error margins
sampling_error_plot <- ggplot(data = sampling_error_estimation, aes(x=n, y=sampling_error, group = type, color = type)) +
								geom_line(size = 2, alpha = 1) +
								geom_point(size = 1.5) +
								scale_x_continuous(breaks = seq(100, 3000, 100) ) + 
								scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = breaks_pretty(n = 10) ) + 
								theme_minimal() +
								labs(x = "Sample size", y = "Sampling error", title = "", color = "", group = "") +
								theme(legend.direction ="vertical", legend.position="bottom", legend.margin = margin(t=-0.6, unit = 'cm')) +
								guides(color=guide_legend(ncol=2,))

#ggsave(sampling_error_plot, file = "media/sampling_error_plot.pdf", width = 10, height = 7)

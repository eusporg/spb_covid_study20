# This code creates the plot for probability of 
# referral to hospital, according to age and vaccination status
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))


# Load data
load("data/ve_ct_paper/trc_dt.Rdata")

# Create readable names for facet_wrap
trc_dt[, vaccination := as.factor(fifelse(vac_status_simple == 1, 
																					"Completely vaccinated", 
																					"Non-vaccinated and partially vaccinated"))]

# Semiparametric logistic regression for relationship 
# between the hospitalization and the thin plate regression spline 
# of the patient age by vaccination status
hosp_model_gam <- mgcv::gam(hospitalization ~ 
															sex + 
															s(age, by = vaccination) +
															vac_status_simple + 
															source, 
														data = trc_dt, 
														family = binomial(link = "logit"))
# Plot relationship 
age_status_ve_plot <- gratia::draw(hosp_model_gam, 
																	 parametric = F, 
																	 fun = plogis, 
																	 rug = F, 
																	 constant = coef(hosp_model_gam)[1],
																	 # scales = "fixed",)
																	 response_range = c(0, 1),
																	 show = "estimate")

# Extract data from plot object to plot it with theme, title and breaks
data_both_binded <- rbind(age_status_ve_plot[[1]]$data, 
													age_status_ve_plot[[2]]$data)

# Plot 
figure_1 <- ggplot() +  
	geom_line(data = data_both_binded, aes(x = age, y = est), size = 0.7)  +  
	geom_ribbon(data = data_both_binded,aes(x = age, ymin = lower, ymax = upper), alpha = 0.2) + 
	expand_limits(x = c(18,96), y = c(0, 0.5)) +
	facet_wrap(~vaccination, nrow = 1)+ 
	theme_bw() + 
	scale_x_continuous(name = "Age (years)", breaks = c(seq(20, 90, by = 10)), 1) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1,suffix = ""), 
										 name = "Probability of hospitalization (%)",
										 breaks = seq(0.0, 0.6, by = 0.1), 1)+ 
	theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
				legend.position = "none")+ 
	labs(color = "", linetype = "") 

# figure_1 

# Save plot in a file
# pdf("figure_1.pdf", width = 10, height = 5) 
# figure_1
# dev.off()

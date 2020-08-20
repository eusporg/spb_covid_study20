library(data.table)
library(ggplot2)
library(ggthemes)
library(ggrepel)

setwd("~")

# Load the per-group prevalence computed by estimate_prevalence_by_variable.r
load("estimates/wave1/prevalence_by_variable_level.rdata")

# Show univariate prevalence for Abbott test
abbott_per_variable_prevalence <- prevalence_by_variable_level[prevalence_type == "univariate" &  serotest == "IgG_testB"]
abbott_per_variable_prevalence[, index := 1:.N]
 

per_variable_prevalence <- ggplot(abbott_per_variable_prevalence, aes(x = pointest, y = rev(index), label = variable_level)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = lowerbound, xmax = upperbound), width = 0.2) +
											#geom_text_repel(aes(label = paste0(variable, ":", variable_level)), min.segment.length = 10e6, nudge_x = -0) +
											geom_text(aes(label = paste0(variable, ":", variable_level)), vjust=1) +

											geom_text(aes(label = paste0(pointest, "%")), hjust = 0.5, vjust=-0.5) +
											scale_x_continuous(name = "Adjusted prevalence") +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

ggsave("media/per_variable_prevalence.png", width = 10, height = 25)

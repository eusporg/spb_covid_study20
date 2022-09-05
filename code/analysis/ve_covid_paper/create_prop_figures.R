# This code creates the plot for probability of 
# referral to hospital, according to age and vaccination status
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)
library(tidymv)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load data
load("data/ve_covid_paper/trc_dt.Rdata")

# Create readable names for facet_wrap
trc_dt[, vaccination := as.factor(fifelse(vac_status_simple == 1, 
                                          "Completely vaccinated", 
                                          "Non-vaccinated and partially vaccinated"))]

# Semiparametric logistic regression for relationship 
# between the hospitalization and the thin plate regression spline 
# of the patient age by vaccination status
h_model_gam <- mgcv::gam(hospitalization ~ s(age, by = vaccination) + 
                            vaccination, 
                          data = trc_dt, 
                          family = binomial(link = "logit"))
# Plot relationship 
age_status_ve_plot <- plot_smooths(model = h_model_gam,
                                   series = age,
                                   comparison = vaccination, 
                                   transform = plogis) + 
  scale_x_continuous(name = "Age (years)", breaks = c(seq(20, 90, by = 10)), 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1,suffix = ""), 
                     name = "Probability of hospitalization (%)",
                     breaks = seq(0.0, 1, by = 0.1), 1) + 
  theme_bw()+
  labs(color = "", linetype = "", fill = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "top")

# Extract data from plot object to plot it with theme, title and breaks
age_status_ve_data <- age_status_ve_plot$data

# Overlay plots

# Add color palette 
cbPalette <- c("#009E73","#E69F00")

one_plot1 <- ggplot() + 
  geom_line(data = age_status_ve_data, aes(x=age, y=hospitalization, color = vaccination, linetype = vaccination), size = 1)  + 
  geom_ribbon(data = age_status_ve_data[age_status_ve_data$.idx == 1,], 
              aes(x= age, ymin = CI_lower, ymax = CI_upper), 
              alpha = 0.2, 
              fill= "#009E73") +
  geom_ribbon(data = age_status_ve_data[age_status_ve_data$.idx == 2,], 
              aes(x= age, ymin = CI_lower, ymax = CI_upper), 
              alpha = 0.2, 
              fill= "#E69F00") +
  expand_limits(x = c(18, 96), y = c(0, 0.5)) +
  theme_bw() +
  scale_x_continuous(name = "Age (years)", breaks = seq(20, 90, by = 10), 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     name = "Probability of hospitalization (%)", 
                     breaks = seq(0.0, 0.5, by = 0.1),1)+
  scale_colour_manual(values = cbPalette)+
  theme(legend.justification = c(0.2, 1), legend.position = c(0.2, 0.9),
        axis.text.x = element_text(angle = 0, hjust = 0.5))+
  labs(color = "", linetype = "")

one_plot1

# Save new plot in a file
# pdf("one_plot1.pdf", width = 6, height = 5) 
# one_plot1
# dev.off()


#Similar plot for any lung injury

# Semiparametric logistic regression for relationship 
# between any lung injury and the thin plate regression spline 
# of the patient age by vaccination status
li_model_gam <- mgcv::gam(ct_score_simple ~ s(age, by = vaccination) + 
                            vaccination, 
                          data = trc_dt, 
                          family = binomial(link = "logit"))
# Plot relationship 
age_status_ve_plot2 <- plot_smooths(model = li_model_gam,
                                   series = age,
                                   comparison = vaccination, 
                                   transform = plogis) + 
  scale_x_continuous(name = "Age (years)", breaks = c(seq(20, 90, by = 10)), 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1,suffix = ""), 
                     name = "Probability of hospitalization (%)",
                     breaks = seq(0.0, 1, by = 0.1), 1)+ 
  theme_bw() +
  labs(color = "", linetype = "", fill = "") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "top")

# Extract data from plot object to plot it with theme, title and breaks
age_status_ve_data2 <- age_status_ve_plot2$data

# Overlay plots

# Add color palette 
cbPalette <- c("#009E73","#E69F00")

one_plot2 <- ggplot() + 
  geom_line(data = age_status_ve_data2, aes(x=age, y=ct_score_simple, color = vaccination, linetype = vaccination), size = 1)  + 
  geom_ribbon(data = age_status_ve_data2[age_status_ve_data2$.idx == 1,], 
              aes(x= age, ymin = CI_lower, ymax = CI_upper), 
              alpha = 0.2, 
              fill= "#009E73") +
  geom_ribbon(data = age_status_ve_data2[age_status_ve_data2$.idx == 2,], 
              aes(x= age, ymin = CI_lower, ymax = CI_upper), 
              alpha = 0.2, 
              fill= "#E69F00") +
  expand_limits(x = c(18, 96), y = c(0, 0.5)) +
  theme_bw() +
  scale_x_continuous(name = "Age (years)", breaks = seq(20, 90, by = 10), 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     name = "Probability of any lung injury (%)", 
                     breaks = seq(0.0, 0.9, by = 0.1), 1)+
  scale_colour_manual(values = cbPalette)+
  theme(legend.justification = c(0.5, 0.7), legend.position = c(0.9, 0.3),
        axis.text.x = element_text(angle = 0, hjust = 0.5))+
  labs(color = "", linetype = "")

one_plot2

# Save new plot in a file
# pdf("one_plot2.pdf", width = 6, height = 5)
# one_plot2
# dev.off()
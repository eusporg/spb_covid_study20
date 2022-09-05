# This code creates the plot for referral to hospital and any lung injury,  
# according to age
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)
library(tidymv)

library(dplyr)
library(broom)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load data
load("data/ve_covid_paper/trc_dt.Rdata")


trc_dt$age_group <- cut(trc_dt$age,
                        breaks = c(0,25,35,45,55,65,75,85,100))
# trc_dt_li <- trc_dt[trc_dt$ct_score_simple == 1]
# trc_dt_hosp <- trc_dt[trc_dt$hospitalization == 1]

new <- trc_dt %>% group_by(age_group) %>%
  do(tidy(glm(ct_score_simple ~ 
                source + vac_status_simple,
              data = ., 
              family = binomial(link = "logit")))) %>%
  ungroup() %>% filter(term == "vac_status_simple") %>%
  mutate(OR = exp(estimate), upper = exp(estimate+1.96*std.error), lower = exp(estimate-1.96*std.error)) %>%
  mutate(VE = round(100*(1-OR), 0), lowerVE = round(100*(1-upper), 0), upperVE = round(100*(1-lower), 0))

ggplot(new, aes(age_group, VE)) +
  geom_point() +
  geom_linerange(aes(ymin = lowerVE, ymax = upperVE))+
  # facet_wrap(~sex)+
  theme_bw()
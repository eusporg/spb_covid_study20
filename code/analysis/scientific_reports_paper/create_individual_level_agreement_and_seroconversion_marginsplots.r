# This code computes the invididual-level variables
# associated with seroconversion for
# Seroprevalence of SARS-CoV-2 antibodies in Saint
# Petersburg, Russia paper (https://doi.org/10.1038/s41598-021-92206-y)
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(margins)
library(ggplot2)
library(ggthemes)
library(ggeffects)
library(ggrepel)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load and prepare the serosurvey data for model fit
source("code/analysis/wave1/create_serosurvey_data_for_model_fit.r")

# Add factor values to dates
serosurvey_data[interview_week == 21, interview_week := "May 18-24"]
serosurvey_data[interview_week == 22, interview_week := "May 25-31"]
serosurvey_data[interview_week == 23, interview_week := "June 1-7"]
serosurvey_data[interview_week == 24, interview_week := "June 8-14"]
serosurvey_data[interview_week == 25, interview_week := "June 15-21"]
serosurvey_data[interview_week == 26, interview_week := "June 22-28"]
serosurvey_data[, interview_week := factor(interview_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

serosurvey_data[draw_week == 21, draw_week := "May 18-24"]
serosurvey_data[draw_week == 22, draw_week := "May 25-31"]
serosurvey_data[draw_week == 23, draw_week := "June 1-7"]
serosurvey_data[draw_week == 24, draw_week := "June 8-14"]
serosurvey_data[draw_week == 25, draw_week := "June 15-21"]
serosurvey_data[draw_week == 26, draw_week := "June 22-28"]
serosurvey_data[, draw_week := factor(draw_week, levels = c("May 18-24", "May 25-31", "June 1-7", "June 8-14", "June 15-21", "June 22-28"))]

###################
# Define regressors list
exhaustive_regressors <- "agegroup + male + highereduc + higherincome + lives_alone + wears_mask + washing_hands_more + was_sick + selftested_covid + lives_alone + russian_citizen + goodhealth + self_medical_checkups + visited + travelled_abroad + follows_news +  + work_status + manages_people + offered_taxi + district + interview_week"

# Fit the models
agreed_fit <- glm(paste0("agreed ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))
agreed_and_tested_fit <- glm(paste0("agreed_and_tested ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))
IgG_testB_fit <- glm(paste0("IgG_testB ~ ", exhaustive_regressors), data = serosurvey_data, family = binomial(link = probit))

# Adjusted predicted values
agreed_adjusted_predictions <- ggeffect(agreed_fit)
agreed_adjusted_predictions_dt <- rbindlist(agreed_adjusted_predictions, id = NULL)
agreed_adjusted_predictions_dt[, index := 1:.N]

agreed_and_tested_adjusted_predictions <- ggeffect(agreed_and_tested_fit)
agreed_and_tested_adjusted_predictions_dt <- rbindlist(agreed_and_tested_adjusted_predictions, id = NULL)
agreed_and_tested_adjusted_predictions_dt[, index := 1:.N]

IgG_testB_adjusted_predictions <- ggeffect(IgG_testB_fit)
IgG_testB_adjusted_predictions_dt <- rbindlist(IgG_testB_adjusted_predictions, id = NULL)
IgG_testB_adjusted_predictions_dt[, index := 1:.N]

# Organise labels
agreed_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
agreed_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
agreed_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
agreed_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
agreed_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
agreed_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
agreed_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
agreed_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
agreed_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
agreed_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
agreed_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
agreed_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
agreed_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
agreed_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
agreed_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
agreed_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
agreed_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
agreed_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
agreed_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
agreed_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
agreed_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
agreed_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
agreed_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
agreed_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
agreed_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
agreed_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
agreed_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
agreed_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
agreed_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
agreed_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
agreed_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
agreed_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
agreed_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
agreed_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
agreed_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
agreed_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
agreed_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
agreed_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
agreed_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]

agreed_and_tested_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
agreed_and_tested_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
agreed_and_tested_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
agreed_and_tested_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
agreed_and_tested_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
agreed_and_tested_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
agreed_and_tested_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
agreed_and_tested_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
agreed_and_tested_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
agreed_and_tested_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
agreed_and_tested_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
agreed_and_tested_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
agreed_and_tested_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
agreed_and_tested_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
agreed_and_tested_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
agreed_and_tested_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
agreed_and_tested_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
agreed_and_tested_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
agreed_and_tested_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
agreed_and_tested_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
agreed_and_tested_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
agreed_and_tested_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
agreed_and_tested_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
agreed_and_tested_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
agreed_and_tested_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
agreed_and_tested_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
agreed_and_tested_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
agreed_and_tested_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
agreed_and_tested_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
agreed_and_tested_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
agreed_and_tested_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
agreed_and_tested_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
agreed_and_tested_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
agreed_and_tested_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
agreed_and_tested_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
agreed_and_tested_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
agreed_and_tested_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
agreed_and_tested_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
agreed_and_tested_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]

IgG_testB_adjusted_predictions_dt[group == "agegroup", label := paste0("age\n", x)]
IgG_testB_adjusted_predictions_dt[group == "male" & x == 0, label := "female"]
IgG_testB_adjusted_predictions_dt[group == "male" & x == 1, label := "male"]
IgG_testB_adjusted_predictions_dt[group == "highereduc" & x == 0, label := "w.o. higher\neducation"]
IgG_testB_adjusted_predictions_dt[group == "highereduc" & x == 1, label := "with higher\neducation"]
IgG_testB_adjusted_predictions_dt[group == "higherincome" & x == 0, label := "lower\nincome"]
IgG_testB_adjusted_predictions_dt[group == "higherincome" & x == 1, label := "higher\nincome"]
IgG_testB_adjusted_predictions_dt[group == "lives_alone" & x == 0, label := "doesn't\nlive alone"]
IgG_testB_adjusted_predictions_dt[group == "lives_alone" & x == 1, label := "lives\nalone"]
IgG_testB_adjusted_predictions_dt[group == "wears_mask" & x == 0, label := "doesn't wear\nmask"]
IgG_testB_adjusted_predictions_dt[group == "wears_mask" & x == 1, label := "wears\nmask"]
IgG_testB_adjusted_predictions_dt[group == "washing_hands_more" & x == 0, label := "not washing\nhands more"]
IgG_testB_adjusted_predictions_dt[group == "washing_hands_more" & x == 1, label := "washing\nhands more"]
IgG_testB_adjusted_predictions_dt[group == "was_sick" & x == 0, label := "have not had\nillnesses"]
IgG_testB_adjusted_predictions_dt[group == "was_sick" & x == 1, label := "have had\nillnesses"]
IgG_testB_adjusted_predictions_dt[group == "selftested_covid" & x == 0, label := "not tested for\nCOVID-19 before"]
IgG_testB_adjusted_predictions_dt[group == "selftested_covid" & x == 1, label := "tested for\nCOVID-19 before"]
IgG_testB_adjusted_predictions_dt[group == "russian_citizen" & x == 0, label := "not russian\ncitizen"]
IgG_testB_adjusted_predictions_dt[group == "russian_citizen" & x == 1, label := "russian\ncitizen"]
IgG_testB_adjusted_predictions_dt[group == "goodhealth" & x == 0, label := "not good self-\nreported health"]
IgG_testB_adjusted_predictions_dt[group == "goodhealth" & x == 1, label := "good self-\nreported health"]
IgG_testB_adjusted_predictions_dt[group == "self_medical_checkups" & x == 0, label := "not regular\nmedical check-ups"]
IgG_testB_adjusted_predictions_dt[group == "self_medical_checkups" & x == 1, label := "regular\nmedical check-ups"]
IgG_testB_adjusted_predictions_dt[group == "visited" & x == 0, label := "not visited\npublic places"]
IgG_testB_adjusted_predictions_dt[group == "visited" & x == 1, label := "visited\npublic places"]
IgG_testB_adjusted_predictions_dt[group == "travelled_abroad" & x == 0, label := "no travelling\nabroad history"]
IgG_testB_adjusted_predictions_dt[group == "travelled_abroad" & x == 1, label := "travelling\nabroad history"]
IgG_testB_adjusted_predictions_dt[group == "met_travelers" & x == 0, label := "didn't meet\ntravellers"]
IgG_testB_adjusted_predictions_dt[group == "met_travelers" & x == 1, label := "met\ntravellers"]
IgG_testB_adjusted_predictions_dt[group == "follows_news" & x == 0, label := "does not\nfollow news"]
IgG_testB_adjusted_predictions_dt[group == "follows_news" & x == 1, label := "follows\nnews"]
IgG_testB_adjusted_predictions_dt[group == "work_status" & x == 0, label := "not employed"]
IgG_testB_adjusted_predictions_dt[group == "work_status" & x == 1, label := "employed"]
IgG_testB_adjusted_predictions_dt[group == "manages_people" & x == 0, label := "doesn't manage\nother people"]
IgG_testB_adjusted_predictions_dt[group == "manages_people" & x == 1, label := "manages\nother people"]
IgG_testB_adjusted_predictions_dt[group == "offered_taxi" & x == 0, label := "not offered taxi\nto/from clinic"]
IgG_testB_adjusted_predictions_dt[group == "offered_taxi" & x == 1, label := "offered taxi\nto/from clinic"]
IgG_testB_adjusted_predictions_dt[group == "interview_week", label := paste0("call week:\n", x)]
IgG_testB_adjusted_predictions_dt[group == "district", label := paste0("district:\n", gsub(" District", "", x))]


# Remove week of call and district dummies
agreed_adjusted_predictions_dt <- agreed_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]
agreed_and_tested_adjusted_predictions_dt <- agreed_and_tested_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]
IgG_testB_adjusted_predictions_dt <- IgG_testB_adjusted_predictions_dt[ !(group %in% c("interview_week", "district"))]

# Plot the results
## Agreed
agreed_adjusted_predictions_plot <- ggplot(agreed_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.04) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$agreed, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to agree to partipate in phone survey", limits = c(0.4, max(agreed_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

#ggsave("media/wave1/agreed_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)


## Agreed and tested
agreed_and_tested_adjusted_predictions_plot <- ggplot(agreed_and_tested_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.03) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$agreed_and_tested, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to volunteer to give blood sample", limits = c(0, max(agreed_and_tested_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

#ggsave("media/wave1/agreed_and_tested_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)



## IgG-positive (Abbott)
IgG_testB_adjusted_predictions_plot <- ggplot(IgG_testB_adjusted_predictions_dt, aes(x = predicted, y = rev(index), label = label)) +
											geom_point(size = 2) +
											geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
											geom_text_repel(aes(label = label), min.segment.length = 10e6, nudge_x = -0.05) +
											geom_text(aes(label = paste0(round(100*predicted, 1), "%")), hjust = 0.5, vjust=-0.5) +
											geom_vline(xintercept = mean(serosurvey_data$IgG_testB, na.rm = T), linetype = 2, color = "firebrick2") +
											scale_x_continuous(name = "Adjusted predicted probability to be CMIA-positive", limits = c(0, max(IgG_testB_adjusted_predictions_dt$conf.high))) +
											scale_y_discrete(name = NULL) +
											theme_minimal() +
											theme(panel.grid.minor = element_blank(), axis.text.y = element_text(size = 8), text = element_text(size = 15))

#ggsave("media/wave1/IgG_testB_adjusted_predictions_plot.pdf", scale = 1.3, width = 7, height = 20)

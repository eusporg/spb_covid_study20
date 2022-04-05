# This code prepares ve models (crude, adjusted, pcr_past==0) 
# for the COVID-19 vaccines effectiveness against 
# symptomatic SARS-CoV-2 Delta variant infection: 
# a population-based case-control study in St. Petersburg, Russia

library(data.table)
library(sandwich)
library(lmtest)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load dataset
load("data/ve_covid_paper2/case_control_final.Rdata")

# crude ve
ve_model_crude <- glm(is_case  ~ final_vac_status, 
                      data = case_control_final,
                           family = binomial(link = "logit"))

# summary(ve_model_crude)
exp(coef(ve_model_crude)[-1])
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6835543                         1.8553616                         1.1132170                         0.7663450 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 0.5455744                         0.5175482 
(1-exp(coef(ve_model_crude)[-1]))*100
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart      final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 31.64457                         -85.53616                         -11.32170                          23.36550                          45.44256                          48.24518 

vcov_ve_model_crude <- sandwich(ve_model_crude, type = "HC1")
exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude))
# final_vac_statuscovivac_full      0.4394796 1.0631812
# final_vac_statusepivaccorona_full 0.8801386 3.9111644
# final_vac_statusother_full        0.5835648 2.1235893
# final_vac_statuspart              0.4459822 1.3168343
# final_vac_statusspuntik_full      0.4643442 0.6410146
# final_vac_statussputniklite_full  0.3771957 0.7101251

(1-exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude)))*100
# final_vac_statuscovivac_full      56.05204   -6.318121
# final_vac_statusepivaccorona_full 11.98614 -291.116442
# final_vac_statusother_full        41.64352 -112.358935
# final_vac_statuspart              55.40178  -31.683427
# final_vac_statusspuntik_full      53.56558   35.898542
# final_vac_statussputniklite_full  62.28043   28.987486

# adjusted ve
# is_cse ~ final_vac_status + male + age, data = case_control_final
ve_model_adj <- glm(is_case  ~ final_vac_status + male + age, 
                      data = case_control_final,
                      family = binomial(link = "logit"))

# summary(ve_model_adj)
exp(coef(ve_model_adj)[-1])
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6697113                         1.6359788                         1.1654850                         0.7505181 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   0.4957697                         0.4945756                         0.7211761                         1.0176478 
(1-exp(coef(ve_model_adj)[-1]))*100
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   33.028867                        -63.597878                        -16.548497                         24.948188 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   50.423029                         50.542438                         27.882389                         -1.764777 
vcov_ve_model_adj <- sandwich(ve_model_adj, type = "HC1")
exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj))
# final_vac_statuscovivac_full      0.4241934 1.0573321
# final_vac_statusepivaccorona_full 0.8110946 3.2997710
# final_vac_statusother_full        0.6368517 2.1329225
# final_vac_statuspart              0.4287839 1.3136629
# final_vac_statusspuntik_full      0.4210149 0.5837979
# final_vac_statussputniklite_full  0.3594837 0.6804344
(1-exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj)))*100
# final_vac_statuscovivac_full      57.580663   -5.733208
# final_vac_statusepivaccorona_full 18.890536 -229.977100
# final_vac_statusother_full        36.314834 -113.292247
# final_vac_statuspart              57.121613  -31.366288
# final_vac_statusspuntik_full      57.898513   41.620209
# final_vac_statussputniklite_full  64.051634   31.956562

# crude ve pcr_past == 0
ve_model_crude_pcr <- glm(is_case  ~ final_vac_status, 
                        data = case_control_final[pcr_past == 0],
                        family = binomial(link = "logit"))

# summary(ve_model_crude_pcr)
exp(coef(ve_model_crude_pcr)[-1])
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6414482                         1.6036205                         0.8964338                         0.7461744 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
#                   0.4741602                         0.5255563 

(1-exp(coef(ve_model_crude_pcr)[-1]))*100
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                     35.85518                         -60.36205                          10.35662                          25.38256 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
#                     52.58398                          47.44437 

vcov_ve_model_crude_pcr <- sandwich(ve_model_crude_pcr, type = "HC1")
exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr))
# final_vac_statuscovivac_full      0.4058616 1.0137835
# final_vac_statusepivaccorona_full 0.7377607 3.4856812
# final_vac_statusother_full        0.4648024 1.7288929
# final_vac_statuspart              0.4237217 1.3140139
# final_vac_statusspuntik_full      0.4015395 0.5599148
# final_vac_statussputniklite_full  0.3769772 0.7326953


(1-exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr)))*100
# final_vac_statuscovivac_full      59.41384   -1.378351
# final_vac_statusepivaccorona_full 26.22393 -248.568121
# final_vac_statusother_full        53.51976  -72.889287
# final_vac_statuspart              57.62783  -31.401392
# final_vac_statusspuntik_full      59.84605   44.008524
# final_vac_statussputniklite_full  62.30228   26.730467

# adjusted ve pcr_past == 0
ve_model_adj_pcr <- glm(is_case  ~ final_vac_status + male + age, 
                    data = case_control_final[pcr_past == 0],
                    family = binomial(link = "logit"))

# summary(ve_model_adj_pcr)
exp(coef(ve_model_adj_pcr)[-1])
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6195430                         1.4006943                         0.9331790                         0.7275077 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   0.4241375                         0.4980817                         0.7038157                         1.0174134 

(1-exp(coef(ve_model_adj_pcr)[-1]))*100
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   38.045697                        -40.069434                          6.682100                         27.249231 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   57.586255                         50.191830                         29.618425                         -1.741343 
vcov_ve_model_adj_pcr <- sandwich(ve_model_adj_pcr, type = "HC1")
exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr))
# final_vac_statuscovivac_full      0.3849091 0.9972058
# final_vac_statusepivaccorona_full 0.6745277 2.9086197
# final_vac_statusother_full        0.5046773 1.7255048
# final_vac_statuspart              0.4031883 1.3127052
# final_vac_statusspuntik_full      0.3578202 0.5027457
# final_vac_statussputniklite_full  0.3555827 0.6976869

(1-exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr)))*100
# final_vac_statuscovivac_full      61.509093    0.2794195
# final_vac_statusepivaccorona_full 32.547228 -190.8619738
# final_vac_statusother_full        49.532273  -72.5504798
# final_vac_statuspart              59.681165  -31.2705209
# final_vac_statusspuntik_full      64.217977   49.7254320
# final_vac_statussputniklite_full  64.441729   30.2313146
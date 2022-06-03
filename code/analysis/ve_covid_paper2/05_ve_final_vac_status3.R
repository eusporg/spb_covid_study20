# This code prepares supplementary ve models (with final_vac_status3) 
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
ve_model_crude <- glm(is_case  ~ final_vac_status3, 
                      data = case_control_final,
                      family = binomial(link = "logit"))

# summary(ve_model_crude)
exp(coef(ve_model_crude)[-1])
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                    0.7303169                          1.8779577                          1.2247550                          0.7756782 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full 
#                   0.5568971                          0.5322554 
(1-exp(coef(ve_model_crude)[-1]))*100
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     26.96831                          -87.79577                          -22.47550                           22.43218 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full 
#                   44.31029                           46.77446 

vcov_ve_model_crude <- sandwich(ve_model_crude, type = "HC1")
exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude))
# final_vac_status3covivac_full      0.4680811 1.1394664
# final_vac_status3epivaccorona_full 0.8908816 3.9586910
# final_vac_status3other_full        0.6354989 2.3603892
# final_vac_status3part              0.4514305 1.3328222
# final_vac_status3spuntik_full      0.4738845 0.6544515
# final_vac_status3sputniklite_full  0.3877404 0.7306326

(1-exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude)))*100
# final_vac_status3covivac_full      53.19189  -13.94664
# final_vac_status3epivaccorona_full 10.91184 -295.86910
# final_vac_status3other_full        36.45011 -136.03892
# final_vac_status3part              54.85695  -33.28222
# final_vac_status3spuntik_full      52.61155   34.55485
# final_vac_status3sputniklite_full  61.22596   26.93674

# adjusted ve
ve_model_adj <- glm(is_case  ~ final_vac_status3 + male + age, 
                    data = case_control_final,
                    family = binomial(link = "logit"))

# summary(ve_model_adj)
exp(coef(ve_model_adj)[-1])
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     0.7155668                          1.6578548                          1.2562583                          0.7598808 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full                               male                                age 
#                     0.5060415                          0.5096511                          0.7184947                          1.0174691 

(1-exp(coef(ve_model_adj)[-1]))*100
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     28.443322                         -65.785476                         -25.625834                          24.011921 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full                               male                                age 
#                     49.395847                          49.034888                          28.150526                          -1.746915 

vcov_ve_model_adj <- sandwich(ve_model_adj, type = "HC1")
exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj))
# final_vac_status3covivac_full      0.4516398 1.1337260
# final_vac_status3epivaccorona_full 0.8218365 3.3443176
# final_vac_status3other_full        0.6782920 2.3267044
# final_vac_status3part              0.4342321 1.3297469
# final_vac_status3spuntik_full      0.4297006 0.5959453
# final_vac_status3sputniklite_full  0.3703171 0.7014104

(1-exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj)))*100
# final_vac_status3covivac_full      54.836015  -234.372595
# final_vac_status3epivaccorona_full 17.816347 -234.431763
# final_vac_status3other_full        32.170800 -132.670445
# final_vac_status3part              56.576788  -32.974690
# final_vac_status3spuntik_full      57.029944   40.405469
# final_vac_status3sputniklite_full  62.968289   29.858962

# crude ve pcr_past == 0
ve_model_crude_pcr <- glm(is_case  ~ final_vac_status3, 
                          data = case_control_final[pcr_past == 0],
                          family = binomial(link = "logit"))

# summary(ve_model_crude_pcr)
exp(coef(ve_model_crude_pcr)[-1])
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     0.6976309                          1.6278055                          0.9966156                          0.7574279 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full 
#                     0.4851499                          0.5455159 

(1-exp(coef(ve_model_crude_pcr)[-1]))*100
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                   30.2369077                        -62.7805486                          0.3384396                         24.2572141 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full 
#                   51.4850130                         45.4484091 

vcov_ve_model_crude_pcr <- sandwich(ve_model_crude_pcr, type = "HC1")
exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr))
# final_vac_status3covivac_full      0.4392516 1.1079957
# final_vac_status3epivaccorona_full 0.7489172 3.5381090
# final_vac_status3other_full        0.5106070 1.9452196
# final_vac_status3part              0.4301357 1.3337580
# final_vac_status3spuntik_full      0.4107838 0.5729787
# final_vac_status3sputniklite_full  0.3909560 0.7611792


(1-exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr)))*100
# final_vac_status3covivac_full      56.07484  -10.79957
# final_vac_status3epivaccorona_full 25.10828 -253.81090
# final_vac_status3other_full        48.93930  -94.52196
# final_vac_status3part              56.98643  -33.37580
# final_vac_status3spuntik_full      58.92162   42.70213
# final_vac_status3sputniklite_full  60.90440   23.88208

# adjusted ve pcr_past == 0
ve_model_adj_pcr <- glm(is_case  ~ final_vac_status3 + male + age, 
                        data = case_control_final[pcr_past == 0],
                        family = binomial(link = "logit"))

# summary(ve_model_adj_pcr)
exp(coef(ve_model_adj_pcr)[-1])
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     0.6736946                          1.4241097                          1.0151737                          0.7384991 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full                               male                                age 
#                     0.4341880                          0.5183464                          0.7009234                          1.0171960 

(1-exp(coef(ve_model_adj_pcr)[-1]))*100
# final_vac_status3covivac_full final_vac_status3epivaccorona_full        final_vac_status3other_full              final_vac_status3part 
#                     32.630538                         -42.410972                          -1.517372                          26.150095 
# final_vac_status3spuntik_full  final_vac_status3sputniklite_full                               male                                age 
#                     56.581202                          48.165362                          29.907660                          -1.719604 

vcov_ve_model_adj_pcr <- sandwich(ve_model_adj_pcr, type = "HC1")
exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr))
# final_vac_status3covivac_full      0.4162371 1.0903989
# final_vac_status3epivaccorona_full 0.6856819 2.9577686
# final_vac_status3other_full        0.5413276 1.9037965
# final_vac_status3part              0.4093789 1.3322152
# final_vac_status3spuntik_full      0.3662967 0.5146626
# final_vac_status3sputniklite_full  0.3697985 0.7265659

(1-exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr)))*100
# final_vac_status3covivac_full      58.376294   -9.039894
# final_vac_status3epivaccorona_full 31.431807 -195.776864
# final_vac_status3other_full        45.867236  -90.379654
# final_vac_status3part              59.062107  -33.221523
# final_vac_status3spuntik_full      63.370331   48.533742
# final_vac_status3sputniklite_full  63.020154   27.343405
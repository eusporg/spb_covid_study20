# This code prepares supplementary ve models (with final_vac_status2) 
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

# Crude ve
ve_model_crude <- glm(is_case  ~ final_vac_status2, 
                      data = case_control_final,
                      family = binomial(link = "logit"))

# summary(ve_model_crude)
exp(coef(ve_model_crude)[-1])
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#               0.6835542722142                    1.8553615960099                    0.0000008759225                    0.5243413206115 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full 
#               0.5762246366463                    0.5761386008663 

(1-exp(coef(ve_model_crude)[-1]))*100
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#                     31.64457                          -85.53616                           99.99991                           47.56587 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full 
#                     42.37754                           42.38614 

vcov_ve_model_crude <- sandwich(ve_model_crude, type = "HC1")
exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude))
# final_vac_status2covivac_full      0.4394795916321 1.063181207863
# final_vac_status2epivaccorona_full 0.8801385687409 3.911164416841
# final_vac_status2other_full        0.0000005863942 0.000001308404
# final_vac_status2part              0.2816174266557 0.976267071841
# final_vac_status2spuntik_full      0.4916051345603 0.675409609330
# final_vac_status2sputniklite_full  0.4249123475842 0.781186259461

(1-exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude)))*100
# final_vac_status2covivac_full      56.05204   -6.318121
# final_vac_status2epivaccorona_full 11.98614 -291.116442
# final_vac_status2other_full        99.99994   99.999869
# final_vac_status2part              71.83826    2.373293
# final_vac_status2spuntik_full      50.83949   32.459039
# final_vac_status2sputniklite_full  57.50877   21.881374

# adjusted ve
ve_model_adj <- glm(is_case  ~ final_vac_status2 + male + age, 
                    data = case_control_final,
                    family = binomial(link = "logit"))

# summary(ve_model_adj)
exp(coef(ve_model_adj)[-1])
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#               0.670176433799                     1.639567971535                     0.000001088374                     0.509932612981 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full                               male                                age 
#               0.524325191691                     0.552769256745                     0.729158341952                     1.017280740307 

(1-exp(coef(ve_model_adj)[-1]))*100
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#                     32.982357                         -63.956797                          99.999891                          49.006739 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full                               male                                age 
#                     47.567481                          44.723074                          27.084166                          -1.728074 

vcov_ve_model_adj <- sandwich(ve_model_adj, type = "HC1")
exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj))
# final_vac_status2covivac_full      0.4247763311509 1.057348113542
# final_vac_status2epivaccorona_full 0.8126067924880 3.308098280909
# final_vac_status2other_full        0.0000007097172 0.000001669056
# final_vac_status2part              0.2689377111468 0.966882884044
# final_vac_status2spuntik_full      0.4464597333179 0.615770888448
# final_vac_status2sputniklite_full  0.4064061394609 0.751843590768

(1-exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj)))*100
# final_vac_status2covivac_full      57.522367   -5.734811
# final_vac_status2epivaccorona_full 18.739321 -230.809828
# final_vac_status2other_full        99.999929   99.999833
# final_vac_status2part              73.106229    3.311712
# final_vac_status2spuntik_full      55.354027   38.422911
# final_vac_status2sputniklite_full  59.359386   24.815641

# crude ve pcr_past == 0
ve_model_crude_pcr <- glm(is_case  ~ final_vac_status2, 
                          data = case_control_final[pcr_past == 0],
                          family = binomial(link = "logit"))

# summary(ve_model_crude_pcr)
exp(coef(ve_model_crude_pcr)[-1])
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#               0.6414481897628                    1.6036204744070                    0.0000006489212                    0.5105403959337 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full 
#               0.5007984205789                    0.5861974003084  

(1-exp(coef(ve_model_crude_pcr)[-1]))*100
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#                     35.85518                          -60.36205                           99.99994                           48.94596 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full 
#                     49.92016                           41.38026 

vcov_ve_model_crude_pcr <- sandwich(ve_model_crude_pcr, type = "HC1")
exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr))
# final_vac_status2covivac_full      0.4058615812357 1.0137835143135
# final_vac_status2epivaccorona_full 0.7377607052252 3.4856812076381
# final_vac_status2other_full        0.0000004269286 0.0000009863445
# final_vac_status2part              0.2683791721051 0.9712061254068
# final_vac_status2spuntik_full      0.4250820919787 0.5900014674505
# final_vac_status2sputniklite_full  0.4254255523995 0.8077262641846

(1-exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr)))*100
# final_vac_status2covivac_full      59.41384   -1.378351
# final_vac_status2epivaccorona_full 26.22393 -248.568121
# final_vac_status2other_full        99.99996   99.999901
# final_vac_status2part              73.16208    2.879387
# final_vac_status2spuntik_full      57.49179   40.999853
# final_vac_status2sputniklite_full  57.45744   19.227374

# adjusted ve pcr_past == 0
ve_model_adj_pcr <- glm(is_case  ~ final_vac_status2 + male + age, 
                        data = case_control_final[pcr_past == 0],
                        family = binomial(link = "logit"))

# summary(ve_model_adj_pcr)
exp(coef(ve_model_adj_pcr)[-1])
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#               0.620269584326                     1.404999557277                     0.000000812544                     0.492981966057 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full                               male                                age 
#               0.449032115945                     0.558492500493                     0.711342533462                     1.016972079957 

(1-exp(coef(ve_model_adj_pcr)[-1]))*100
# final_vac_status2covivac_full final_vac_status2epivaccorona_full        final_vac_status2other_full              final_vac_status2part 
#                     37.973042                         -40.499956                          99.999919                          50.701803 
# final_vac_status2spuntik_full  final_vac_status2sputniklite_full                               male                                age 
#                     55.096788                          44.150750                          28.865747                          -1.697208 

vcov_ve_model_adj_pcr <- sandwich(ve_model_adj_pcr, type = "HC1")
exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr))
# final_vac_status2covivac_full      0.3856940841617 0.997511688767
# final_vac_status2epivaccorona_full 0.6763602731648 2.918598022783
# final_vac_status2other_full        0.0000005188274 0.000001272538
# final_vac_status2part              0.2528955493334 0.960994448094
# final_vac_status2spuntik_full      0.3798430092514 0.530824146396
# final_vac_status2sputniklite_full  0.4032924277094 0.773418620525

(1-exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr)))*100
# final_vac_status2covivac_full      61.430592    0.2488311
# final_vac_status2epivaccorona_full 32.363973 -191.8598023
# final_vac_status2other_full        99.999948   99.9998727
# final_vac_status2part              74.710445    3.9005552
# final_vac_status2spuntik_full      62.015699   46.9175854
# final_vac_status2sputniklite_full  59.670757   22.6581379
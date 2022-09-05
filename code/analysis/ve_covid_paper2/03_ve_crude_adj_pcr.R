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
# is_case ~ final_vac_status, data = case_control_final

ve_model_crude <- glm(is_case  ~ final_vac_status, 
                      data = case_control_final,
                           family = binomial(link = "logit"))

# summary(ve_model_crude)
exp(coef(ve_model_crude)[-1])
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6835543                         1.8553616                         1.1132170                         0.7663450 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 0.5455744                         0.5175482 

# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6604946                         2.0488812                         1.2190843                         0.7794657 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 0.5666578                         0.5283957 

(1-exp(coef(ve_model_crude)[-1]))*100
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart      final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 31.64457                         -85.53616                         -11.32170                          23.36550                          45.44256                          48.24518 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 33.95054                        -104.88812                         -21.90843                          22.05343 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 43.33422                          47.16043 
vcov_ve_model_crude <- sandwich(ve_model_crude, type = "HC1")
exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude))
# OLD
# final_vac_statuscovivac_full      0.4394796 1.0631812
# final_vac_statusepivaccorona_full 0.8801386 3.9111644
# final_vac_statusother_full        0.5835648 2.1235893
# final_vac_statuspart              0.4459822 1.3168343
# final_vac_statusspuntik_full      0.4643442 0.6410146
# final_vac_statussputniklite_full  0.3771957 0.7101251
# NEW
# final_vac_statuscovivac_full      0.4247315 1.0271269
# final_vac_statusepivaccorona_full 0.9950245 4.2189053
# final_vac_statusother_full        0.6545144 2.2706401
# final_vac_statuspart              0.4579802 1.3266223
# final_vac_statusspuntik_full      0.4840531 0.6633592
# final_vac_statussputniklite_full  0.3875967 0.7203415

(1-exp(coefci(ve_model_crude, vcov = vcov_ve_model_crude)))*100
# OLD
# final_vac_statuscovivac_full      56.05204   -6.318121
# final_vac_statusepivaccorona_full 11.98614 -291.116442
# final_vac_statusother_full        41.64352 -112.358935
# final_vac_statuspart              55.40178  -31.683427
# final_vac_statusspuntik_full      53.56558   35.898542
# final_vac_statussputniklite_full  62.28043   28.987486
# NEW
# final_vac_statuscovivac_full      57.5268531   -2.712693
# final_vac_statusepivaccorona_full  0.4975462 -321.890534
# final_vac_statusother_full        34.5485610 -127.064010
# final_vac_statuspart              54.2019776  -32.662226
# final_vac_statusspuntik_full      51.5946854   33.664085
# final_vac_statussputniklite_full  61.2403256   27.965854

# adjusted ve
# is_cse ~ final_vac_status + male + age, data = case_control_final
ve_model_adj <- glm(is_case  ~ final_vac_status + male + age, 
                      data = case_control_final,
                      family = binomial(link = "logit"))

# summary(ve_model_adj)
exp(coef(ve_model_adj)[-1])
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6697113                         1.6359788                         1.1654850                         0.7505181 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   0.4957697                         0.4945756                         0.7211761                         1.0176478 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6499742                         1.8424107                         1.2622975                         0.7674116 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
# 0.5136578                         0.5069561                         0.7287715                         1.0179916 
(1-exp(coef(ve_model_adj)[-1]))*100
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   33.028867                        -63.597878                        -16.548497                         24.948188 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   50.423029                         50.542438                         27.882389                         -1.764777 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 35.002581                        -84.241074                        -26.229755                         23.258837 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
# 48.634216                         49.304387                         27.122854                         -1.799158 

vcov_ve_model_adj <- sandwich(ve_model_adj, type = "HC1")
exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj))
# OLD
# final_vac_statuscovivac_full      0.4241934 1.0573321
# final_vac_statusepivaccorona_full 0.8110946 3.2997710
# final_vac_statusother_full        0.6368517 2.1329225
# final_vac_statuspart              0.4287839 1.3136629
# final_vac_statusspuntik_full      0.4210149 0.5837979
# final_vac_statussputniklite_full  0.3594837 0.6804344
# NEW
# final_vac_statuscovivac_full      0.4112233 1.0273407
# final_vac_statusepivaccorona_full 0.9238768 3.6741665
# final_vac_statusother_full        0.7068525 2.2542115
# final_vac_statuspart              0.4418293 1.3329143
# final_vac_statusspuntik_full      0.4378621 0.6025741
# final_vac_statussputniklite_full  0.3706084 0.6934664

(1-exp(coefci(ve_model_adj, vcov = vcov_ve_model_adj)))*100
# OLD
# final_vac_statuscovivac_full      57.580663   -5.733208
# final_vac_statusepivaccorona_full 18.890536 -229.977100
# final_vac_statusother_full        36.314834 -113.292247
# final_vac_statuspart              57.121613  -31.366288
# final_vac_statusspuntik_full      57.898513   41.620209
# final_vac_statussputniklite_full  64.051634   31.956562

# NEW
# final_vac_statuscovivac_full      58.877668   -2.734068
# final_vac_statusepivaccorona_full  7.612316 -267.416649
# final_vac_statusother_full        29.314746 -125.421146
# final_vac_statuspart              55.817070  -33.291432
# final_vac_statusspuntik_full      56.213786   39.742592
# final_vac_statussputniklite_full  62.939155   30.653358

# crude ve pcr_past == 0
# is_cse ~ final_vac_status, data = case_control_final[pcr_past == 0]
ve_model_crude_pcr <- glm(is_case  ~ final_vac_status, 
                        data = case_control_final[pcr_past == 0],
                        family = binomial(link = "logit"))

# summary(ve_model_crude_pcr)
exp(coef(ve_model_crude_pcr)[-1])
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6414482                         1.6036205                         0.8964338                         0.7461744 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
#                   0.4741602                         0.5255563 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6197829                         1.7708082                         0.9816437                         0.7589178 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 0.4924631                         0.5371018 

(1-exp(coef(ve_model_crude_pcr)[-1]))*100
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                     35.85518                         -60.36205                          10.35662                          25.38256 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
#                     52.58398                          47.44437 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 38.021713                        -77.080820                          1.835632                         24.108220 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full 
# 50.753687                         46.289825 

vcov_ve_model_crude_pcr <- sandwich(ve_model_crude_pcr, type = "HC1")
exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr))
# OLD
# final_vac_statuscovivac_full      0.4058616 1.0137835
# final_vac_statusepivaccorona_full 0.7377607 3.4856812
# final_vac_statusother_full        0.4648024 1.7288929
# final_vac_statuspart              0.4237217 1.3140139
# final_vac_statusspuntik_full      0.4015395 0.5599148
# final_vac_statussputniklite_full  0.3769772 0.7326953
# NEW
# final_vac_statuscovivac_full      0.3922228 0.9793690
# final_vac_statusepivaccorona_full 0.8332310 3.7633761
# final_vac_statusother_full        0.5210756 1.8492985
# final_vac_statuspart              0.4349192 1.3242832
# final_vac_statusspuntik_full      0.4185184 0.5794726
# final_vac_statussputniklite_full  0.3877199 0.7440378

(1-exp(coefci(ve_model_crude_pcr, vcov = vcov_ve_model_crude_pcr)))*100
# OLD
# final_vac_statuscovivac_full      59.41384   -1.378351
# final_vac_statusepivaccorona_full 26.22393 -248.568121
# final_vac_statusother_full        53.51976  -72.889287
# final_vac_statuspart              57.62783  -31.401392
# final_vac_statusspuntik_full      59.84605   44.008524
# final_vac_statussputniklite_full  62.30228   26.730467
# NEW
# final_vac_statuscovivac_full      60.77772    2.063101
# final_vac_statusepivaccorona_full 16.67690 -276.337610
# final_vac_statusother_full        47.89244  -84.929849
# final_vac_statuspart              56.50808  -32.428324
# final_vac_statusspuntik_full      58.14816   42.052737
# final_vac_statussputniklite_full  61.22801   25.596216

# adjusted ve pcr_past == 0
# is_cse ~ final_vac_status + male + age, data = case_control_final[pcr_past == 0]
ve_model_adj_pcr <- glm(is_case  ~ final_vac_status + male + age, 
                    data = case_control_final[pcr_past == 0],
                    family = binomial(link = "logit"))

# summary(ve_model_adj_pcr)
exp(coef(ve_model_adj_pcr)[-1])
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   0.6195430                         1.4006943                         0.9331790                         0.7275077 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   0.4241375                         0.4980817                         0.7038157                         1.0174134 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
# 0.6017440                         1.5807030                         1.0104122                         0.7453162 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
# 0.4394333                         0.5116101                         0.7116301                         1.0177515 

(1-exp(coef(ve_model_adj_pcr)[-1]))*100
# OLD
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   38.045697                        -40.069434                          6.682100                         27.249231 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   57.586255                         50.191830                         29.618425                         -1.741343 
# NEW
# final_vac_statuscovivac_full final_vac_statusepivaccorona_full        final_vac_statusother_full              final_vac_statuspart 
#                   39.825604                        -58.070295                         -1.041224                         25.468384 
# final_vac_statusspuntik_full  final_vac_statussputniklite_full                              male                               age 
#                   56.056667                         48.838985                         28.836989                         -1.775152 

vcov_ve_model_adj_pcr <- sandwich(ve_model_adj_pcr, type = "HC1")
exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr))
# OLD
# final_vac_statuscovivac_full      0.3849091 0.9972058
# final_vac_statusepivaccorona_full 0.6745277 2.9086197
# final_vac_statusother_full        0.5046773 1.7255048
# final_vac_statuspart              0.4031883 1.3127052
# final_vac_statusspuntik_full      0.3578202 0.5027457
# final_vac_statussputniklite_full  0.3555827 0.6976869
# NEW
# final_vac_statuscovivac_full      0.3733494 0.9698576
# final_vac_statusepivaccorona_full 0.7688279 3.2499104
# final_vac_statusother_full        0.5598004 1.8237445
# final_vac_statuspart              0.4161676 1.3347895
# final_vac_statusspuntik_full      0.3721113 0.5189352
# final_vac_statussputniklite_full  0.3672921 0.7126342

(1-exp(coefci(ve_model_adj_pcr, vcov = vcov_ve_model_adj_pcr)))*100
# OLD
# final_vac_statuscovivac_full      61.509093    0.2794195
# final_vac_statusepivaccorona_full 32.547228 -190.8619738
# final_vac_statusother_full        49.532273  -72.5504798
# final_vac_statuspart              59.681165  -31.2705209
# final_vac_statusspuntik_full      64.217977   49.7254320
# final_vac_statussputniklite_full  64.441729   30.2313146
# NEW
# final_vac_statuscovivac_full      62.665058    3.014236
# final_vac_statusepivaccorona_full 23.117208 -224.991035
# final_vac_statusother_full        44.019960  -82.374449
# final_vac_statuspart              58.383238  -33.478950
# final_vac_statusspuntik_full      62.788873   48.106477
# final_vac_statussputniklite_full  63.270785   28.736581
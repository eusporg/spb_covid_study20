# This code fits logit models of hospitalization 
# by age, sex and LDCT triage centre and calculates OR and CI
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study

library(data.table)
library(sandwich)
library(lmtest)

options(scipen = 999)
# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/ve_ct_paper/trc_dt.Rdata")


# Vaccine effectiveness against referral to hospital by age, sex and LDCT triage centre
# OR by sex
model_sex_F_1 <- glm(hospitalization ~  
										 	vac_status_simple,
										 data = trc_dt[sex == "F"], 
										 family = binomial(link = "logit"))
# summary(model_sex_F_1)
# covariance matrix for ci
vcov_sandwich.model_sex_F_1 <- sandwich(model_sex_F_1, type = "HC1")

exp(coef(model_sex_F_1)[-1]) 
# vac_status_simple 0.260517
exp(coefci(model_sex_F_1, vcov = vcov_sandwich.model_sex_F_1))
# vac_status_simple 0.1286061 0.52772861

(1-exp(coef(model_sex_F_1)[-1]))*100
# vac_status_simple  73.9483
(1-exp(coefci(model_sex_F_1, vcov = vcov_sandwich.model_sex_F_1)))*100
# vac_status_simple 87.13939 47.22714


model_sex_M_1 <- glm(hospitalization ~  
										 	vac_status_simple ,
										 data = trc_dt[sex == "M"], 
										 family = binomial(link = "logit"))
summary(model_sex_M_1)
# covariance matrix for ci
vcov_sandwich.model_sex_M_1 <- sandwich(model_sex_M_1, type = "HC1")

exp(coef(model_sex_M_1)[-1]) 
# vac_status_simple 0.464509
exp(coefci(model_sex_M_1, vcov = vcov_sandwich.model_sex_M_1))
# vac_status_simple 0.23618221 0.91356844

(1-exp(coef(model_sex_M_1)[-1]))*100
# vac_status_simple  53.5491
(1-exp(coefci(model_sex_M_1, vcov = vcov_sandwich.model_sex_M_1)))*100
# vac_status_simple 76.38178  8.643156

# adj OR by sex
model_sex_F_2 <- glm(hospitalization ~  
										 	age +
										 	source +
										 	vac_status_simple,
										 data = trc_dt[sex == "F"], 
										 family = binomial(link = "logit"))
summary(model_sex_F_2)

# covariance matrix for ci
vcov_sandwich.model_sex_F_2 <- sandwich(model_sex_F_2, type = "HC1")

exp(coef(model_sex_F_2)[-1]) 
# vac_status_simple 0.1637325
exp(coefci(model_sex_F_2, vcov = vcov_sandwich.model_sex_F_2))
# vac_status_simple 0.0792605522 0.3382306234

(1-exp(coef(model_sex_F_2)[-1]))*100
# vac_status_simple   83.626746 
(1-exp(coefci(model_sex_F_2, vcov = vcov_sandwich.model_sex_F_2)))*100
# vac_status_simple 92.073945  66.17694


model_sex_M_2 <- glm(hospitalization ~  
										 	age +
										 	source +
										 	vac_status_simple,
										 data = trc_dt[sex == "M"], 
										 family = binomial(link = "logit"))
summary(model_sex_M_2)
# covariance matrix for ci
vcov_sandwich.model_sex_M_2 <- sandwich(model_sex_M_2, type = "HC1")

exp(coef(model_sex_M_2)[-1]) 
# vac_status_simple 0.2402415
exp(coefci(model_sex_M_2, vcov = vcov_sandwich.model_sex_M_2))
# vac_status_simple 0.1182337089 0.488151489

(1-exp(coef(model_sex_M_2)[-1]))*100
# vac_status_simple   75.975854 
(1-exp(coefci(model_sex_M_2, vcov = vcov_sandwich.model_sex_M_2)))*100
# vac_status_simple 88.176629 51.184851


# OR by age
trc_dt[age <50, age_group2 := "18-49"]
trc_dt[age >=50, age_group2 := "50+"]
trc_dt[, age_group2 := as.factor(age_group2)]

model_age_less50_1 <- glm(hospitalization ~  
														vac_status_simple,
													data = trc_dt[age_group2 == "18-49"], 
													family = binomial(link = "logit"))

summary(model_age_less50_1)
# covariance matrix for ci
vcov_sandwich.model_age_less50_1 <- sandwich(model_age_less50_1, type = "HC1")

exp(coef(model_age_less50_1)[-1]) 
# vac_status_simple 0.4266203
exp(coefci(model_age_less50_1, vcov = vcov_sandwich.model_age_less50_1))
# vac_status_simple 0.104449530 1.74251530

(1-exp(coef(model_age_less50_1)[-1]))*100
# vac_status_simple   57.33797  
(1-exp(coefci(model_age_less50_1, vcov = vcov_sandwich.model_age_less50_1)))*100
# vac_status_simple 89.55505 -74.25153

model_age_more50_1 <- glm(hospitalization ~  
														vac_status_simple,
													data = trc_dt[age_group2 =="50+"], 
													family = binomial(link = "logit"))

summary(model_age_more50_1)
# covariance matrix for ci
vcov_sandwich.model_age_more50_1 <- sandwich(model_age_more50_1, type = "HC1")

exp(coef(model_age_more50_1)[-1]) 
# vac_status_simple 0.2262034
exp(coefci(model_age_more50_1, vcov = vcov_sandwich.model_age_more50_1))
# vac_status_simple 0.1344107 0.38068371

(1-exp(coef(model_age_more50_1)[-1]))*100
# vac_status_simple   77.37966
(1-exp(coefci(model_age_more50_1, vcov = vcov_sandwich.model_age_more50_1)))*100
# vac_status_simple 86.55893 61.93163

# adj OR by age
model_age_less50_2 <- glm(hospitalization ~  
														sex +
														age +
														source +
														vac_status_simple,
													data = trc_dt[age_group2 == "18-49"], 
													family = binomial(link = "logit"))

summary(model_age_less50_2)
# covariance matrix for ci
vcov_sandwich.model_age_less50_2 <- sandwich(model_age_less50_2, type = "HC1")

exp(coef(model_age_less50_2)[-1]) 
# vac_status_simple 0.3704155 
exp(coefci(model_age_less50_2, vcov = vcov_sandwich.model_age_less50_2))
# vac_status_simple 0.0911771609 1.504846234

(1-exp(coef(model_age_less50_2)[-1]))*100
# vac_status_simple   62.958455
(1-exp(coefci(model_age_less50_2, vcov = vcov_sandwich.model_age_less50_2)))*100
# vac_status_simple 90.882284  -50.484623

model_age_more50_2 <- glm(hospitalization ~  
														sex +
														age +
														source +
														vac_status_simple,
													data = trc_dt[age_group2 == "50+"], 
													family = binomial(link = "logit"))

summary(model_age_more50_2)
# covariance matrix for ci
vcov_sandwich.model_age_more50_2 <- sandwich(model_age_more50_2, type = "HC1")

exp(coef(model_age_more50_2)[-1]) 
# vac_status_simple 0.1818274
exp(coefci(model_age_more50_2, vcov = vcov_sandwich.model_age_more50_2))
# vac_status_simple 0.1060958773 0.3116162789

(1-exp(coef(model_age_more50_2)[-1]))*100
# vac_status_simple   81.817260 
(1-exp(coefci(model_age_more50_2, vcov = vcov_sandwich.model_age_more50_2)))*100
# vac_status_simple 89.390412  68.838372

# OR by LDCT triage centre
model_LDCT1_1 <- glm(hospitalization ~  
										 	vac_status_simple,
										 data = trc_dt[source == "facility_1"], 
										 family = binomial(link = "logit"))

summary(model_LDCT1_1)
# covariance matrix for ci
vcov_sandwich.model_LDCT1_1 <- sandwich(model_LDCT1_1, type = "HC1")

exp(coef(model_LDCT1_1)[-1]) 
# vac_status_simple 0.3389475
exp(coefci(model_LDCT1_1, vcov = vcov_sandwich.model_LDCT1_1))
# vac_status_simple 0.15878078 0.72354717

(1-exp(coef(model_LDCT1_1)[-1]))*100
# vac_status_simple   66.10525
(1-exp(coefci(model_LDCT1_1, vcov = vcov_sandwich.model_LDCT1_1)))*100
# vac_status_simple 84.12192 27.64528

model_LDCT2_1 <- glm(hospitalization ~  
										 	vac_status_simple,
										 data = trc_dt[source == "facility_2"], 
										 family = binomial(link = "logit"))

summary(model_LDCT2_1)
# covariance matrix for ci
vcov_sandwich.model_LDCT2_1 <- sandwich(model_LDCT2_1, type = "HC1")

exp(coef(model_LDCT2_1)[-1]) 
# vac_status_simple 0.3458256
exp(coefci(model_LDCT2_1, vcov = vcov_sandwich.model_LDCT2_1))
# vac_status_simple 0.18307255 0.65326766

(1-exp(coef(model_LDCT2_1)[-1]))*100
# vac_status_simple  65.41744 
(1-exp(coefci(model_LDCT2_1, vcov = vcov_sandwich.model_LDCT2_1)))*100
# vac_status_simple 81.69275 34.67323

# adj OR by LDCT triage centre
model_LDCT1_2 <- glm(hospitalization ~  
										 	age +
										 	sex +
										 	vac_status_simple,
										 data = trc_dt[source == "facility_1"], 
										 family = binomial(link = "logit"))
# tbl_regression(model_LDCT_1, exponentiate = TRUE)
summary(model_LDCT1_2)
# covariance matrix for ci
vcov_sandwich.model_LDCT1_2 <- sandwich(model_LDCT1_2, type = "HC1")

exp(coef(model_LDCT1_2)[-1]) 
# vac_status_simple 0.2100014 
exp(coefci(model_LDCT1_2, vcov = vcov_sandwich.model_LDCT1_2))
# vac_status_simple 0.0966493786 0.45629475

(1-exp(coef(model_LDCT1_2)[-1]))*100
# vac_status_simple   78.999856 
(1-exp(coefci(model_LDCT1_2, vcov = vcov_sandwich.model_LDCT1_2)))*100
# vac_status_simple 90.335062   54.370525

model_LDCT2_2 <- glm(hospitalization ~  
										 	age +
										 	sex +
										 	vac_status_simple,
										 data = trc_dt[source == "facility_2"], 
										 family = binomial(link = "logit"))
# tbl_regression(model_LDCT_1, exponentiate = TRUE)
summary(model_LDCT2_2)

# covariance matrix for ci
vcov_sandwich.model_LDCT2_2 <- sandwich(model_LDCT2_2, type = "HC1")

exp(coef(model_LDCT2_2)[-1]) 
# vac_status_simple 0.1842171 
exp(coefci(model_LDCT2_2, vcov = vcov_sandwich.model_LDCT2_2))
# vac_status_simple 0.094857492 0.3577572625

(1-exp(coef(model_LDCT2_2)[-1]))*100
# vac_status_simple  81.578285 
(1-exp(coefci(model_LDCT2_2, vcov = vcov_sandwich.model_LDCT2_2)))*100
# vac_status_simple 90.514251  64.224274
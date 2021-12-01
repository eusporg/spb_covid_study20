# This code computes minimum detectable odds ratio
# for the Vaccine effectiveness against referral
# to hospital and severe lung injury associated with COVID-19
# study
library(epiR)

# Load data to extract characteristics
load("data/ve_ct_paper/trc_dt.Rdata")

# Study characteristics
total_patients <- trc_dt[, .N] # 13893
completely_vaccinated_patients <- trc_dt[vac_status3 == "full_vac", .N] # 1291
partially_vaccinated_patients <- trc_dt[vac_status3 == "part_vac", .N] # 448

# Outcomes
hospitalized_patients <- trc_dt[hospitalization == 1, .N] # 495
any_lung_injury_patients <- trc_dt[ct_score != 0, .N] # 9368
saturation_less_than_96 <- trc_dt[saturation < 96 | is.na(saturation), .N] # 385

# MDE for hospitalization, complete vaccination
epi.sscc(OR = NA, 
				 p0 = completely_vaccinated_patients/total_patients, 
				 n = total_patients, 
				 conf.level = 0.95, 
				 power = 0.8, 
				 method = "unmatched", 
				 r = (total_patients-hospitalized_patients)/hospitalized_patients)

# MDE for any lung injury, complete vaccination
epi.sscc(OR = NA, 
				 p0 = completely_vaccinated_patients/total_patients, 
				 n = total_patients, 
				 conf.level = 0.95, 
				 power = 0.8, 
				 method = "unmatched", 
				 r = (total_patients-any_lung_injury_patients)/any_lung_injury_patients)

# MDE for oxygen saturation, complete vaccination
epi.sscc(OR = NA, 
				 p0 = completely_vaccinated_patients/total_patients, 
				 n = total_patients, 
				 conf.level = 0.95, 
				 power = 0.8, 
				 method = "unmatched", 
				 r = (total_patients-saturation_less_than_96)/saturation_less_than_96)

# MDE for hospitalization, partial vaccination
epi.sscc(OR = NA, 
				 p0 = partially_vaccinated_patients/total_patients, 
				 n = total_patients, 
				 conf.level = 0.95, 
				 power = 0.8, 
				 method = "unmatched", 
				 r = (total_patients-hospitalized_patients)/hospitalized_patients)

# This code computes minimum detectable odds ratio
# for the COVID-19 vaccines effectiveness against 
# symptomatic SARS-CoV-2 Delta variant infection: 
# a population-based case-control study in St. Petersburg, Russia

library(epiR)
library(data.table)

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

# Load data to extract characteristics
load("data/ve_covid_paper2/case_control_final.Rdata")

# Study characteristics
total_patients <- case_control_final[, .N] # 3945
completely_vaccinated_patients <- case_control_final[vac_status3 == "full_vac", .N] # 1569
# case_control_final[vac_status3 == "full_vac", .N]/total_patients # 0.3977186
partially_vaccinated_patients <- case_control_final[vac_status3 == "part_vac", .N] # 65

completely_vaccinated_spuntik <- case_control_final[final_vac_status == "spuntik_full", .N] # 1175
# completely_vaccinated_spuntik/total_patients # 0.2978454
# partially_vaccinated_patients <- case_control_final[vac_status3 == "part_vac", .N] # 65
completely_vaccinated_epivac <- case_control_final[final_vac_status == "epivaccorona_full", .N] # 28
# completely_vaccinated_epivac/total_patients # 0.007097592


# Outcomes 
case_patients <- case_control_final[is_case == 1, .N] # 1198
any_lung_injury_patients <- case_control_final[ct_score_simple != 0, .N] # 833
# saturation_less_than_96 <- case_control_final[saturation < 96 | is.na(saturation), .N] # 385

# MDE for case, complete vaccination sputnik
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_spuntik/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)

# MDE for case, complete vaccination EpiVacCorona
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_epivac/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)


# MDE for case, complete vaccination CoviVac
completely_vaccinated_covivac <- case_control_final[final_vac_status == "covivac_full", .N] # 104
# completely_vaccinated_covivac/total_patients # 0.02636248
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_covivac/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)

# MDE for case, complete vaccination  Sputnik Light
completely_vaccinated_sputniklite <- case_control_final[final_vac_status == "sputniklite_full", .N] # 243
# completely_vaccinated_sputniklite/total_patients # 0.06159696
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_sputniklite/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)


# MDE for case, complete vaccination
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_patients/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)

# MDE for any lung injury, complete vaccination
epi.sscc(OR = NA, 
         p0 = completely_vaccinated_patients/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-any_lung_injury_patients)/any_lung_injury_patients)

# MDE for case, partial vaccination
epi.sscc(OR = NA, 
         p0 = partially_vaccinated_patients/total_patients, 
         n = total_patients, 
         conf.level = 0.95, 
         power = 0.8, 
         method = "unmatched", 
         r = (total_patients-case_patients)/case_patients)
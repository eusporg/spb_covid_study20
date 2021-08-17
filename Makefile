# List of R dependencies for the project
DEPENDENCIES:
	# Do nothing, the file is created outside the repo
	noop

# Preliminary analysis. Helper function to estimate confidence intervals for prevalence estimates
code/analysis/preliminary/estimate_sample_size/R_function_ci4prev.R:
	# Do nothing, the file is created outside the repo
	noop

# Preliminary analysis. Computes sampling errors of seroprevalence estimates
# under perfect and imperfect tests
estimate_sample_size: code/analysis/preliminary/estimate_sample_size/R_function_ci4prev.R
	Rscript code/analysis/preliminary/estimate_sample_size/estimate_samplesize_imperfect_test.R

# Wave 1. Data from phone survey of SPb residents
data/wave1/phone_survey/phone_survey_data.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data from paper-based survey of clinic visitors
data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Sugentech test results
data/wave1/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Abbott test results
data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Genetico test results
data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1.5. Data on Abbott test results
data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1.5. Data on Genetico test results
data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Data from phone survey of SPb residents
data/wave2/phone_survey/phone_survey_data.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Data on Abbott test results
data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Data on Genetico test results
data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Data on Vector test results
data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Data on other blood test results
data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 3. Data from phone survey of SPb residents
data/wave3/phone_survey/phone_survey_data_wave3.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 3. Merged data from phone surveys of SPb residents across all waves
data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 3. Data on Genetico test results
data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 3. Data on Vector test results
data/wave3/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Data on monthly distribution of variants of concern in the city (see https://doi.org/10.1038/s41467-020-20880-z)
data/variants/variants_count_by_months.xlsx:
	# Do nothing, the file is created outside the repo
	noop

# Data from 2018 Rosstat survey of SPb residents
data/kouzh_2018/kouzh_2018_data_spb.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Data on SPb boundaries from Open Street Map
data/spb_map/spb_district_boundaries.cpg:
	# Do nothing, the file is created outside the repo
	noop

data/spb_map/spb_district_boundaries.dbf:
	# Do nothing, the file is created outside the repo
	noop

data/spb_map/spb_district_boundaries.shp:
	# Do nothing, the file is created outside the repo
	noop

data/spb_map/spb_district_boundaries.shx:
	# Do nothing, the file is created outside the repo
	noop

data/spb_map/spb_district_boundaries.prj:
	# Do nothing, the file is created outside the repo
	noop

# Helper function to estimate the bivariate selection model
code/analysis/helper_functions/estimate_bivariate_selection.r
	# Do nothing, the file is created outside the repo
	noop

# Helper function to compute prevalence from the estimated bivariate selection model
code/analysis/helper_functions/prev_modified.r
	# Do nothing, the file is created outside the repo
	noop

# Helper function to adjust the computed prevalence for test characteristics
code/analysis/helper_functions/adjust_prev_test_chars.r
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Procedure to prepare serosurvey data for estimation
code/analysis/wave1/create_serosurvey_data_for_model_fit.r:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Create raking weights for phone survey using the 2018 survey
estimates/wave1/phone_survey_raking_fit.rdata: data/wave1/phone_survey/phone_survey_data.rdata data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata code/analysis/wave1/create_serosurvey_data_for_model_fit.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave1/estimate_raking_weights_phone_survey.r

# Wave 1. Estimate seroprevalence by model
estimates/wave1/prevalence_by_model.rdata: data/wave1/phone_survey/phone_survey_data.rdata estimates/wave1/phone_survey_raking_fit.rdata data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata code/analysis/wave1/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave1/estimate_prevalence_by_model.r

# Wave 1. Estimate seroprevalence by variable level
estimates/wave1/prevalence_by_variable_level.rdata: data/wave1/phone_survey/phone_survey_data.rdata estimates/wave1/phone_survey_raking_fit.rdata data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata code/analysis/wave1/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave1/estimate_prevalence_by_variable.r

# Wave 1.5. Procedure to prepare serosurvey data for estimation
code/analysis/wave1.5/create_serosurvey_data_for_model_fit.r:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1.5. Estimate seroprevalence by model
estimates/wave1.5/prevalence_by_model.rdata: data/wave1/phone_survey/phone_survey_data.rdata estimates/wave1/phone_survey_raking_fit.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata code/analysis/wave1.5/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave1.5/estimate_prevalence_by_model.r

# Wave 1.5. Estimate seroprevalence by variable level
estimates/wave1.5/prevalence_by_variable_level.rdata: data/wave1/phone_survey/phone_survey_data.rdata estimates/wave1/phone_survey_raking_fit.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata code/analysis/wave1.5/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave1.5/estimate_prevalence_by_variable.r

# Wave 2. Procedure to prepare serosurvey data for estimation
code/analysis/wave2/create_serosurvey_data_for_model_fit.r:
	# Do nothing, the file is created outside the repo
	noop

# Wave 2. Create raking weights for phone survey using the 2018 survey
estimates/wave2/phone_survey_raking_fit.rdata estimates/wave2/phone_survey_agreed_and_tested_raking_fit.rdata: data/wave2/phone_survey/phone_survey_data.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata code/analysis/wave2/create_serosurvey_data_for_model_fit.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave2/estimate_raking_weights_phone_survey.r

# Wave 2. Estimate seroprevalence by model
estimates/wave2/prevalence_by_model.rdata: data/wave2/phone_survey/phone_survey_data.rdata estimates/wave2/phone_survey_raking_fit.rdata data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata code/analysis/wave2/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave2/estimate_prevalence_by_model.r

# Wave 2. Estimate seroprevalence by variable level
estimates/wave2/prevalence_by_variable_level.rdata: data/wave2/phone_survey/phone_survey_data.rdata estimates/wave2/phone_survey_raking_fit.rdata data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/other_tests/other_test_results_matched_to_phone_survey_ids.rdata code/analysis/wave2/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave2/estimate_prevalence_by_variable.r

# Wave 3. Procedure to prepare serosurvey data for estimation
code/analysis/wave3/create_serosurvey_data_for_model_fit.r:
	# Do nothing, the file is created outside the repo
	noop

# Wave 3. Create raking weights for phone survey using the 2018 survey
estimates/wave3/phone_survey_raking_fit.rdata estimates/wave3/phone_survey_agreed_and_tested_raking_fit.rdata: data/wave3/phone_survey/phone_survey_data_wave3.rdata data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata code/analysis/wave3/create_serosurvey_data_for_model_fit.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave3/estimate_raking_weights_phone_survey.r

# Wave 3. Estimate seroprevalence by model
estimates/wave3/prevalence_by_model.rdata: data/wave3/phone_survey/phone_survey_data_wave3.rdata data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata estimates/wave3/phone_survey_raking_fit.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata code/analysis/wave3/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave3/estimate_prevalence_by_model.r

# Wave 3. Estimate seroprevalence by variable level
estimates/wave3/prevalence_by_variable_level.rdata: data/wave3/phone_survey/phone_survey_data_wave3.rdata data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata estimates/wave3/phone_survey_raking_fit.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata code/analysis/wave3/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/wave3/estimate_prevalence_by_variable.r

# Scientific Reports paper detailing the progress of the pandemic during the first wave (https://doi.org/10.1038/s41598-021-92206-y)
scientific_reports_paper: estimates/wave1/prevalence_by_model.rdata estimates/wave1/prevalence_by_variable_level.rdata data/wave1/phone_survey/phone_survey_data.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata code/analysis/wave1/create_serosurvey_data_for_model_fit.r DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/scientific_reports_paper/compare_phone_and_paper_survey_with_kouzh.r
	Rscript code/analysis/scientific_reports_paper/create_individual_level_agreement_and_seroconversion_marginsplots.r
	Rscript code/analysis/scientific_reports_paper/create_prevalence_by_variable_plot.r
	Rscript code/analysis/scientific_reports_paper/create_spb_district_prevalence_map.r
	Rscript code/analysis/scientific_reports_paper/create_summary_statistics_table.r
	Rscript code/analysis/scientific_reports_paper/create_weekly_prevalence_chart.r
	Rscript code/analysis/scientific_reports_paper/estimate_risk_ratio_create_summary_stat_paper_survey_data.r

# Wave 3. Estimate the fine-grained seroprevalence by age/sex groups (for the pandemic course paper)
estimates/pandemic_course_paper/prevalence_by_agegroup_sex.rdata: data/wave3/phone_survey/phone_survey_data_wave3.rdata data/wave3/phone_survey/phone_survey_data_waves_1_2_3.rdata estimates/wave3/phone_survey_raking_fit.rdata data/wave1/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave1.5/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave2/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata data/wave3/test_results/test_D/test_D_results_matched_to_phone_survey_ids.rdata code/analysis/wave3/create_serosurvey_data_for_model_fit.r code/analysis/helper_functions/estimate_bivariate_selection.r code/analysis/helper_functions/prev_modified.r code/analysis/helper_functions/adjust_prev_test_chars.r DEPENDENCIES
	Rscript code/analysis/pandemic_course_paper/estimate_prevalence_by_agegroup_sex.r

# Waves 1-3. Estimate IR/IFR across the study waves (for the pandemic course paper)
estimates/pandemic_course_paper/sample_weakly_priors_official_deaths.rdata estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths.rdata estimates/pandemic_course_paper/sample_noninformative_priors_official_deaths.rdata estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths.rdata estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_all_pop.rdata estimates/pandemic_course_paper/ir_irf_results_object.rdata estimates/pandemic_course_paper/sample_weakly_priors_excess_deaths_agesex.rdata estimates/pandemic_course_paper/sample_noninformative_priors_excess_deaths_agesex.rdata estimates/pandemic_course_paper/ir_irf_agesex_results_object.rdata: estimates/wave1/prevalence_by_model.rdata estimates/wave1.5/prevalence_by_model.rdata estimates/wave2/prevalence_by_model.rdata estimates/wave3/prevalence_by_model.rdata DEPENDENCIES
	Rscript code/analysis/pandemic_course_paper/estimate_ir_ifr.r

# Pandemic course paper detailing the progress of the pandemic
pandemic_course_paper: estimates/wave1/prevalence_by_model.rdata estimates/wave1/prevalence_by_variable_level.rdata estimates/wave1.5/prevalence_by_model.rdata estimates/wave1.5/prevalence_by_variable_level.rdata estimates/wave2/prevalence_by_model.rdata estimates/wave2/prevalence_by_variable_level.rdata estimates/wave3/prevalence_by_model.rdata estimates/wave3/prevalence_by_variable_level.rdata estimates/pandemic_course_paper/ir_irf_agesex_results_object.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata data/wave1/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/variants/variants_count_by_months.xlsx DEPENDENCIES
	Rscript code/analysis/helper_functions/install_dependencies.r
	Rscript code/analysis/pandemic_course_paper/compare_phone_and_paper_survey_with_kouzh.r
	Rscript code/analysis/pandemic_course_paper/create_antibody_kinetics_chart.r
	Rscript code/analysis/pandemic_course_paper/create_combined_surveillance_figure.r
	Rscript code/analysis/pandemic_course_paper/create_seroprevalence_tables_and_figures.r
	Rscript code/analysis/pandemic_course_paper/create_summary_statistics_table.r

# Evaluation of the performance of SARS-­CoV­-2 antibody assays paper (https://doi.org/10.1002/jmv.27126) evaluation data
data/validation_of_covid_tests/full_abbott.rda data/validation_of_covid_tests/full_dataset.rda data/validation_of_covid_tests/full_genetico.rda data/validation_of_covid_tests/full_roche.rda data/validation_of_covid_tests/full_vector.rda data/validation_of_covid_tests/test_nab.rda:
	# Do nothing, the files are created outside the repo
	noop

# Evaluation of the performance of SARS-­CoV­-2 antibody assays paper (https://doi.org/10.1002/jmv.27126) helper functions
code/analysis/validation_of_covid_tests_paper/functions_for_se_sp.R:
	# Do nothing, the file is created outside the repo
	noop

# Evaluation of the performance of SARS-­CoV­-2 antibody assays paper (https://doi.org/10.1002/jmv.27126) estimation
validation_of_covid_tests_paper: data/validation_of_covid_tests/full_abbott.rda data/validation_of_covid_tests/full_dataset.rda data/validation_of_covid_tests/full_genetico.rda data/validation_of_covid_tests/full_roche.rda data/validation_of_covid_tests/full_vector.rda data/validation_of_covid_tests/test_nab.rda code/analysis/validation_of_covid_tests_paper/functions_for_se_sp.R DEPENDENCIES
	Rscript code/analysis/validation_of_covid_tests_paper/se_sp_roc_calculation.R
	Rscript code/analysis/validation_of_covid_tests_paper/nab_analysis.R

# Dataset for paper on vaccine effectiveness against referral to hospital and severe lung injury
data/ve_covid_paper/trc_dt.Rdata:
# Do nothing, the file is created outside the repo
	noop

# Paper on vaccine effectiveness against referral to hospital and severe lung injury
ve_against_covid_paper: data/ve_covid_paper/trc_dt.Rdata
	Rscript code/analysis/ve_covid_paper/minimum_detectable_ve.r 
	Rscript code/analysis/ve_covid_paper/create_summary_statistics_table.R
	Rscript code/analysis/ve_covid_paper/hospitalization_models.R 
	Rscript code/analysis/ve_covid_paper/lung_injury_models.R 
	Rscript code/analysis/ve_covid_paper/ox_saturation_models.R 
	Rscript code/analysis/ve_covid_paper/sex_age_trc_models.R
	Rscript code/analysis/ve_covid_paper/vac_status_models.R
	Rscript code/analysis/ve_covid_paper/56days_models.R
	Rscript code/analysis/ve_covid_paper/create_ve_figure_for_age_vac_status.R
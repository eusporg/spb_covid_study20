REPOSITORYPATH="~"

# Wave 1. Data from phone survey of SPb residents
data/phone_survey/phone_survey_data.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data from paper-based survey of clinic visitors
data/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Sugentech test results
data/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Abbott test results
data/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Wave 1. Data on Genetico test results
data/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata:
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

# Wave 1. Create raking weights for phone survey using the 2018 survey
estimates/phone_survey_raking_fit.rdata: data/phone_survey/phone_survey_data.rdata data/kouzh_2018/kouzh_2018_data_spb.rdata
	Rscript code/analysis/wave1/estimate_raking_weights_phone_survey.r

# Wave 1. Estimate seroprevalence by model
estimates/wave1/prevalence_by_model.rdata: data/phone_survey/phone_survey_data.rdata estimates/phone_survey_raking_fit.rdata data/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata data/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata
	Rscript code/analysis/wave1/estimate_prevalence_by_model.r

# Wave 1. Estimate seroprevalence by variable
estimates/wave1/prevalence_by_variable.rdata: data/phone_survey/phone_survey_data.rdata estimates/phone_survey_raking_fit.rdata data/paper_survey/paper_survey_data_matched_to_phone_survey_ids.rdata data/test_results/test_A/test_A_results_matched_to_phone_survey_ids.rdata data/test_results/test_B/test_B_results_matched_to_phone_survey_ids.rdata data/test_results/test_C/test_C_results_matched_to_phone_survey_ids.rdata
	Rscript code/analysis/wave1/estimate_prevalence_by_variable.r

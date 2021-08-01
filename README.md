# Study of the Spread of COVID-19 in Saint Petersburg, Russia

Data and replication files for Study of the Spread of COVID-19 in Saint Petersburg, Russia ([NCT04406038](https://clinicaltrials.gov/ct2/show/NCT04406038), [ISRCTN11060415](https://www.isrctn.com/ISRCTN11060415)).

This repository features the data and replication files for the analysis of the seroprevalence and spread of COVID-19 in St. Petersburg, Russia.

## Short study summary

Seroprevalence study of COVID-19 in Saint Petersburg, Russia is a regional longitudinal cohort study aiming to evaluate the spread dynamics of the COVID-19 disease in the population of Saint Petersburg. Clinically asymptomatic adults are sampled from the population using random digit dialing and tested for the presence of SARS-CoV-2-specific antibodies in the blood serum. Data collection and serial sampling of the same individuals spans four weeks and is conducted every two weeks in order to understand both the spread of the virus in the population.

## Installation

To replicate our analysis you need to clone this repository to your local machine. Then you need to install the required versions of R dependencies listed in `DEPENDENCIES`. `code/analysis/helper_functions/install_dependencies.r` automates this step, but you may still need to install the underlying libraries such as JAGS or gfortran manually with [Homebrew](https://brew.sh) or `apt-get`, depending on your platform. Finally, you need to declare the environment variable `SPB_COVID_STUDY_PATH` in bash pointing to the repository. Or, better yet, you can add it in your `.Renviron` with
```console
user:~$ echo 'SPB_COVID_STUDY_PATH="path_to_cloned_repository"' >> ~/.Renviron
```

## Structure

We provide an annotated `Makefile` that documents the data analysis in our papers.

To build the ‘[Seroprevalence of SARS-CoV-2 antibodies in Saint Petersburg, Russia: a population-based study](https://doi.org/10.1038/s41598-021-92206-y)’ paper run `make scientific_reports_paper` when in the repository folder.

To build the ‘[Evaluation of the performance of SARS-CoV-2 antibody assays](https://doi.org/10.1002/jmv.27126)’ run `make validation_of_covid_tests_paper`.

To build the ‘COVID-19 pandemic in Saint Petersburg, Russia’ paper run `make pandemic_course_paper`.

Please note that those commands will not produce any publication-ready output files (e.g. tables or figures): the export statements are commented out in the code. Our intention is to make the analysis pipeline transparent to the readers with the aid of `make`.

For convenience, we also briefly describe the repository structure below:

```
DEPENDENCIES -- list of R packages required to reproduce the analysis
data/wave*/phone_survey -- depersonified participant-level data from the phone
                           survey in the respective wave
data/wave1/paper_survey -- depersonified clinic paper-based survey data from
                           wave 1
data/wave*/test_results -- depersonified data with SARS-CoV-2 antibody test results
                           by manufacturer (A — Sugentech, B — Abbott, C — Genetico
                           Coronapass, D - VectorBest) from the respective wave
data/wave2/other_tests --  depersonified data with other blood test results
                           (Vitamin D, Helicobacter pylori Immunoglobulin G,
                           Hemoglobin A0, Cholesterin, Triglycerides, etc.) from
                           wave 2

data/validation_of_covid_tests -- data for validation of assays' perfomance study:
data/validation_of_covid_tests/full_dataset.rda -- cross-validation sample
data/validation_of_covid_tests/full_*test name*.rda -- full-validation sample for
                                                       *test name*
data/validation_of_covid_tests/test_nab.rda -- data with neutralization test results
data/validation_of_covid_tests/functions_for_se_sp.R -- auxiliary functions for
                                                        assays' perfomance evaluation 
data/validation_of_covid_tests/se_sp_roc_calculation.R -- evaluation of assays'
                                                          perfomance code
data/validation_of_covid_tests/nab_analysis.R -- analysis of the data with
                                                          neutralization test results

data/variants -- data on the monthly counts of SARS-CoV-2 variants of concern (VOC)
                 in St. Petersburg from the Smorodintsev Research Institute of
                 Influenza

data/spb_map -- Saint Petersburg district boundaries shapefile from OpenStreetMap

data/kouzh_2018 -- data from the 2016 round of the Comprehensive Monitoring of Living
                   Conditions household survey (available at
                   https://www.gks.ru/free_doc/new_site/KOUZ18/index.html)

code/analysis/helper_functions -- auxiliary functions
code/analysis/preliminary -- analysis at the onset of the study
code/analysis/wave* -- analysis of the data from the waves 1 to 3
code/analysis/scientific_reports_paper -- the code required to replicate the Sci.Rep
                                          paper
code/analysis/validation_of_covid_tests_paper -- the code required to replicate the
                                                 Journal of Medical Virology paper
code/analysis/pandemic_course_paper -- the code required to replicate the Course of
                                       COVID-19 pandemic paper

estimates/wave* -- results of the seroprevalence model estimates from the respective
                   wave: seroprevalence by model and variable level
estimates/pandemic_course_paper -- results of the auxiliary estimates for the
                                   Course of COVID-19 pandemic paper:
estimates/pandemic_course_paper/prevalence_by_agegroup_sex.rdata -- fine-grained
                                                                    seroprevalence by
                                                                    sex and age group
                                                                    for wave 3
estimates/pandemic_course_paper/sample_*.rdata -- sampling results from the Bayesian
                                                  evidence synthesis model
estimates/pandemic_course_paper/ir_irf_results_object.rdata -- estimated per-wave IR/
                                                               IFR from the Bayesian
                                                               evidence synthesis model
estimates/pandemic_course_paper/ir_irf_agesex_results_object.rdata -- estimated IR/IFR
                                                                      by age/sex group 
                                                                      from the Bayesian
                                                                      evidence synthesis
                                                                      model
```

The code for the Course of COVID-19 pandemic paper also relies on the data from https://github.com/alexei-kouprianov/COVID-19.SPb repository that gathers the federal and city data on the progress of the pandemic and related indicators.

---

<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Study start date: **May 27, 2020**
- Study end date: **Ongoing**
- Protocol: **[Study of the Spread of COVID-19 in Saint Petersburg Protocol in English](https://eusp.org/sites/default/files/inline-files/EU_SG-Russian-Covid-Serosurvey-Protocol-CDRU-001_en.pdf)**
- Preprints: **—**
- Publications: [Seroprevalence of SARS-CoV-2 antibodies in Saint Petersburg, Russia: a population-based study](https://doi.org/10.1038/s41598-021-92206-y), [Evaluation of the performance of SARS-CoV-2 antibody assays for the longitudinal population-based study of COVID-19 spread in St. Petersburg, Russia](https://doi.org/10.1002/jmv.27126)

## Licence
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />
Creative Commons License Attribution 4.0 International (CC BY 4.0).

## Contacts
Anton Barchuk, MD PhD
abarchuk@eu.spb.ru

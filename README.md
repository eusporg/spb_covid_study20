# Study of the Spread of COVID-19 in Saint Petersburg, Russia

Data and replication files for Study of the Spread of COVID-19 in Saint Petersburg, Russia ([NCT04406038](https://clinicaltrials.gov/ct2/show/NCT04406038)) ([ISRCTN11060415](https://www.isrctn.com/ISRCTN11060415)).

This repository features the data and replication files for the analysis of the seroprevalence and spread of COVID-19 in St. Petersburg, Russia.

## Short study summary

Seroprevalence study of COVID-19 in Saint Petersburg, Russia is a regional longitudinal cohort study aiming to evaluate the spread dynamics of the COVID-19 disease in the population of Saint Petersburg. Clinically asymptomatic adults are sampled from the population using random digit dialing and tested for the presence of SARS-CoV-2-specific antibodies in the blood serum. Data collection and serial sampling of the same individuals spans four weeks and is conducted every two weeks in order to understand both the spread of the virus in the population.

## Structure
```
data/phone_survey -- depersonified phone survey participant-level data
data/paper_survey -- depersonified clinic paper-based survey data
data/spb_map -- Saint Petersburg district boundaries shapefile from OpenStreetMap
data/test_results -- data with test results by manufacturer (A — Sugentech, B — Abbott, C — Genetico)
code/analysis/helper_functions -- auxiliary functions 
code/analysis/preliminary -- analysis at the onset of the study
code/analysis/wave1 -- analysis of the data from the first wave
estimates/wave1 -- results of the model estimates from the first wave
validation_of_covid_test/data -- data for validation of assays' perfomance study:
validation_of_covid_test/data/full_dataset.rda -- cross-validation sample
validation_of_covid_test/data/full_*test name*.rda -- full-validation sample for *test name*
validation_of_covid_test/data/test_nab.rda -- data with neutralization test results
validation_of_covid_test/functions_for_se_sp.R -- auxiliary functions for assays' perfomance evaluation 
validation_of_covid_test/se_sp_roc_calculation.R -- evaluation of assays' perfomance 
validation_of_covid_test/nab_analysis.R -- analysis of the data with neutralization test results
```

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

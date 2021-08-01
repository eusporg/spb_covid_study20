# This is NAB analysis for the "Evaluation of the performance of
# SARS-­CoV­-2 antibody assays for a longitudinal population-­based study of COVID-­19
# spread in St. Petersburg, Russia" paper (https://doi.org/10.1002/jmv.27126)
# ---------------------------------------------------------------------------- # 
library("spearmanCI")
library("tidyverse")
library("lubridate")
library("miscFuncs")
library("magrittr")
library("ggrepel")
library("stringr")
library("janitor")
library("pander")
library("epiR")
library("pROC")
library("ggthemes")
library("scales")
# ---------------------------------------------------------------------------- # 

# Declare working directory beforehand in an environment variable
# SPB_COVID_STUDY_PATH = "path_to_your_folder"
# with the aid of file.edit(file.path("~", ".Renviron")); file.rename(file.path("~", ".Renviron.R"), file.path("~", ".Renviron"))
# Restart R session for the changes to make effect
setwd(Sys.getenv('SPB_COVID_STUDY_PATH'))

load("data/validation_of_covid_tests/test_nab.rda")
source("code/analysis/validation_of_covid_tests_paper/functions_for_se_sp.R")

# Sensitivity and specificity       

abbott_1.4_nab_20   <- epi_custom(data = test_nab, x = "MNT 1:20", y = "CMIA Abbott 1.4")
abbott_1.4_nab_80   <- epi_custom(data = test_nab, x = "MNT 1:80", y = "CMIA Abbott 1.4")
abbott_1.0_nab_20   <- epi_custom(data = test_nab, x = "MNT 1:20", y = "CMIA Abbott 1.0")
abbott_1.0_nab_80   <- epi_custom(data = test_nab, x = "MNT 1:80", y = "CMIA Abbott 1.0")
genetico_1.0_nab_20 <- epi_custom(data = test_nab, x = "MNT 1:20", y = "ELISA Coronapass 1.0")
genetico_1.0_nab_80 <- epi_custom(data = test_nab, x = "MNT 1:80", y = "ELISA Coronapass 1.0")
vector_1.1_nab_20   <- epi_custom(data = test_nab, x = "MNT 1:20", y = "ELISA Vector-best 1.1")
vector_1.1_nab_80   <- epi_custom(data = test_nab, x = "MNT 1:80", y = "ELISA Vector-best 1.1")



# Kappa

k_genetico_20   <- kappa_custom(data = test_nab, x = "MNT 1:20", y = "ELISA Coronapass 1.0")
k_vector_20     <- kappa_custom(data = test_nab, x = "MNT 1:20", y = "ELISA Vector-best 1.1")
k_abbott_1.0_20 <- kappa_custom(data = test_nab, x = "MNT 1:20", y = "CMIA Abbott 1.0")
k_abbott_1.4_20 <- kappa_custom(data = test_nab, x = "MNT 1:20", y = "CMIA Abbott 1.4")

k_genetico_80   <- kappa_custom(data = test_nab, x = "MNT 1:80", y = "ELISA Coronapass 1.0")
k_vector_80     <- kappa_custom(data = test_nab, x = "MNT 1:80", y = "ELISA Vector-best 1.1")
k_abbott_1.0_80 <- kappa_custom(data = test_nab, x = "MNT 1:80", y = "CMIA Abbott 1.0")
k_abbott_1.4_80 <- kappa_custom(data = test_nab, x = "MNT 1:80", y = "CMIA Abbott 1.4")


kappa_80 <- tibble(k_genetico_80$kappa) %>%
  bind_rows(k_vector_80$kappa)       %>%
  bind_rows(k_abbott_1.0_80$kappa)   %>%
  bind_rows(k_abbott_1.4_80$kappa)   %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "kappa", 
         titer = "80")

kappa_20 <- tibble(k_genetico_20$kappa) %>%
  bind_rows(k_vector_20$kappa) %>%
  bind_rows(k_abbott_1.0_20$kappa) %>%
  bind_rows(k_abbott_1.4_20$kappa) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "kappa", 
         titer = "20")

pindex_80 <- tibble(k_genetico_80$pindex) %>%
  bind_rows(k_vector_80$pindex) %>%
  bind_rows(k_abbott_1.0_80$pindex) %>%
  bind_rows(k_abbott_1.4_80$pindex) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "pindex", 
         titer = "80")

pindex_20 <- tibble(k_genetico_20$pindex) %>%
  bind_rows(k_vector_20$pindex) %>%
  bind_rows(k_abbott_1.0_20$pindex) %>%
  bind_rows(k_abbott_1.4_20$pindex) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "pindex", 
         titer = "20")

bindex_80 <- tibble(k_genetico_80$bindex) %>%
  bind_rows(k_vector_80$bindex) %>%
  bind_rows(k_abbott_1.0_80$bindex) %>%
  bind_rows(k_abbott_1.4_80$bindex) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "bindex", 
         titer = "80")

bindex_20 <- tibble(k_genetico_20$bindex) %>%
  bind_rows(k_vector_20$bindex) %>%
  bind_rows(k_abbott_1.0_20$bindex) %>%
  bind_rows(k_abbott_1.4_20$bindex) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "bindex", 
         titer = "20")

pabak_80 <- tibble(k_genetico_80$pabak) %>%
  bind_rows(k_vector_80$pabak) %>%
  bind_rows(k_abbott_1.0_80$pabak) %>%
  bind_rows(k_abbott_1.4_80$pabak) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "pabak", 
         titer = "80")

pabak_20 <- tibble(k_genetico_20$pabak) %>%
  bind_rows(k_vector_20$pabak) %>%
  bind_rows(k_abbott_1.0_20$pabak) %>%
  bind_rows(k_abbott_1.4_20$pabak) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "pabak", 
         titer = "20")


kappa <- kappa_80      %>%
  full_join(kappa_20)  %>%
  full_join(pindex_80) %>%
  full_join(pindex_20) %>%
  full_join(bindex_80) %>%
  full_join(bindex_20) %>%
  full_join(pabak_80)  %>%
  full_join(pabak_20)  %>%
  mutate(
    test2 = case_when(
      test == "genetico" ~ "ELISA Coronapass",
      test == "vector"   ~ "ELISA Vector-Best",
      test == "abbott1"  ~ "CMIA Abbott 1.0" ,
      test == "abbott14" ~ "CMIA Abbott 1.4"
    )
  ) %>% 
  mutate(across(c(est, lower, upper), ~ round(., 3))) %>% 
  mutate(estimate = paste0(est, ' (', lower, "-", upper,")"))

kappa_wide <- kappa %>%
  dplyr::select(test2, type, titer, estimate) %>%
  arrange(type, test2) %>%
  pivot_wider(names_from  = "type", 
              values_from = "estimate") %>%
  relocate(test2, titer, pindex, bindex, pabak, kappa) %>% 
  arrange(titer)

# latextable(kappa_wide)


# Agreement
se_nab_20 <- tibble(genetico_1.0_nab_20$rval$se) %>%
  bind_rows(vector_1.1_nab_20$rval$se)   %>%
  bind_rows(abbott_1.0_nab_20$rval$se)   %>%
  bind_rows(abbott_1.4_nab_20$rval$se)   %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "PPA", 
         titer = "20")

se_nab_80 <- tibble(genetico_1.0_nab_80$rval$se) %>%
  bind_rows(vector_1.1_nab_80$rval$se) %>%
  bind_rows(abbott_1.0_nab_80$rval$se) %>%
  bind_rows(abbott_1.4_nab_80$rval$se) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "PPA", 
         titer = "80")

sp_nab_20 <- tibble(genetico_1.0_nab_20$rval$sp) %>%
  bind_rows(vector_1.1_nab_20$rval$sp) %>%
  bind_rows(abbott_1.0_nab_20$rval$sp) %>%
  bind_rows(abbott_1.4_nab_20$rval$sp) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "NPA",  
         titer = "20")


sp_nab_80 <- tibble(genetico_1.0_nab_80$rval$sp) %>%
  bind_rows(vector_1.1_nab_80$rval$sp) %>%
  bind_rows(abbott_1.0_nab_80$rval$sp) %>%
  bind_rows(abbott_1.4_nab_80$rval$sp) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "NPA", 
         titer = "80")

ac_nab_20 <- tibble(genetico_1.0_nab_20$rval$diag.acc) %>%
  bind_rows(vector_1.1_nab_20$rval$diag.acc) %>%
  bind_rows(abbott_1.0_nab_20$rval$diag.acc) %>%
  bind_rows(abbott_1.4_nab_20$rval$diag.acc) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "OPA", 
         titer = "20")


ac_nab_80 <- tibble(genetico_1.0_nab_80$rval$diag.acc) %>%
  bind_rows(vector_1.1_nab_80$rval$diag.acc) %>%
  bind_rows(abbott_1.0_nab_80$rval$diag.acc) %>%
  bind_rows(abbott_1.4_nab_80$rval$diag.acc) %>%
  mutate(test  = c("genetico", "vector", "abbott1", "abbott14"),
         type  = "OPA", 
         titer = "80")


se_sp_nab <- se_nab_20 %>%
  full_join(se_nab_80) %>%
  full_join(sp_nab_20) %>%
  full_join(sp_nab_80) %>%
  full_join(ac_nab_20) %>%
  full_join(ac_nab_80) %>%
  mutate(
    test2 = case_when(
      test == "genetico" ~ "ELISA Coronapass",
      test == "vector"   ~ "ELISA Vector-Best",
      test == "abbott1"  ~ "CMIA Abbott 1.0" ,
      test == "abbott14" ~ "CMIA Abbott 1.4" ,
      test == "Nab"      ~ "MNA100 Titer"
    )
  ) %>% 
  mutate(across(c(est, lower, upper), ~ round(., 3))) %>% 
  mutate(estimate = paste0(100 * est, ' (', 100 * lower, "-", 100 * upper, ")"))

se_sp_nab_wide <- se_sp_nab %>%
  dplyr::select(test2, type, titer, estimate) %>%
  arrange(type, test2) %>%
  pivot_wider(names_from  = "type", 
              values_from = "estimate") %>%
  relocate(test2, titer, PPA, NPA, OPA) %>% 
  arrange(titer)

latextable(se_sp_nab_wide)

# correlation coefficient

cor.test(  test_nab$abbott, test_nab$Nab, method = c("spearman"), conf.level = 0.95)
spearmanCI(test_nab$abbott, test_nab$Nab, method = "Euclidean",   level      = 0.95, plot = FALSE)


cor.test(  test_nab$genetico, test_nab$Nab, method = c("spearman"), conf.level = 0.95)
spearmanCI(test_nab$genetico, test_nab$Nab, method = "Euclidean",   level      = 0.95, plot = FALSE)


cor.test(  test_nab$vector, test_nab$Nab, method = c("spearman"), conf.level = 0.95)
spearmanCI(test_nab$vector, test_nab$Nab, method = "Euclidean",   level      = 0.95, plot = FALSE)



# plot figure 2

nab_plot <- test_nab %>%
  dplyr::select(id:Nab) %>% 
  pivot_longer(-c(id, Nab), 
               names_to  = "test2", 
               values_to = "Index") %>%
  mutate(
    test = case_when(
      test2 == "genetico" ~ "ELISA Coronapass",
      test2 == "vector"   ~ "ELISA Vector-Best" ,
      test2 == "abbott"   ~ "CMIA Abbott")) %>%
  rename(`MNA100 Titer` = Nab) %>%
  dplyr::select(-test2)

fig_2 <- ggplot(data = nab_plot, aes(y = Index, x = `MNA100 Titer`)) +
  geom_jitter(size = 0.8) +
  theme_bw() +
  scale_x_continuous(breaks = c(5, 10, 20, 40, 80, 160, 320, 640), trans = "log2") +
  facet_wrap(. ~ factor(test, levels = c("ELISA Coronapass", "CMIA Abbott", "ELISA Vector-Best")),
             scales = "free_y") +
  geom_hline(yintercept = 1,  linetype = "dashed", size = 1, color = "grey50") +
  theme(
    legend.position   = "none",
    axis.text.x       = element_text(colour = "black", size = 10, hjust = 1, vjust = 1),
    legend.key        = element_blank(),
    legend.background = element_blank(),
    axis.text.y       = element_text(colour = "black", size = 12),
    legend.title      = element_blank(),
    legend.text       = element_blank(),
    strip.text.x      = element_text(size = 12, face = "bold", color = "black"),
    strip.text.y      = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x      = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y      = element_blank(),
    strip.background  = element_blank(),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    strip.placement = "outside") + 
  geom_vline(xintercept = 80,  linetype = "dashed", size = 1, color = "grey50") + 
  xlab("Inverse neutralising MNA titer")

#pdf("plot2.pdf", width = 10, height = 4)
#fig_2
#dev.off()

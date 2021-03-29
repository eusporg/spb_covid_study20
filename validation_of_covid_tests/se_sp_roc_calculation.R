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

# load input data

load("data/full_dataset.rda")
load("data/full_abbott.rda")
load("data/full_genetico.rda")
load("data/full_vector.rda")
load("data/full_roche.rda")
source("functions_for_se_sp.R")

# Specificity and sensitivity for full validation

full_abbott_table_1.4   <- epi_custom(data = full_abbott,   x = "truth", y = "CMIA Abbott 1.4")
full_abbott_table_1.0   <- epi_custom(data = full_abbott,   x = "truth", y = "CMIA Abbott 1.0")
full_genetico_table_1.0 <- epi_custom(data = full_genetico, x = "truth", y = "ELISA Coronapass 1.0")
full_vector_table_1.1   <- epi_custom(data = full_vector,   x = "truth", y = "ELISA Vector-best 1.1")

full_roche_table_1.0    <- ci_for_roche(full_roche)


full_se_table <- tibble(full_genetico_table_1.0$rval$se) %>%
  bind_rows(full_vector_table_1.1$rval$se)   %>%
  bind_rows(full_abbott_table_1.0$rval$se)   %>%
  bind_rows(full_abbott_table_1.4$rval$se)   %>%
  bind_rows(full_roche_table_1.0) %>% 
  mutate(test     = c("genetico", "vector", "abbott1", "abbott14", "roche"),
         estimate = "sensitivity", 
         type     = "full")

full_sp_table <- tibble(full_genetico_table_1.0$rval$sp) %>%
  bind_rows(full_vector_table_1.1$rval$sp) %>%
  bind_rows(full_abbott_table_1.0$rval$sp) %>%
  bind_rows(full_abbott_table_1.4$rval$sp) %>%
  mutate(test     = c("genetico", "vector", "abbott1", "abbott14"),
         estimate = "specificity", 
         type     = "full")


# Specificity and sensitivity for cross-validation data (46 and 40 samples)

cross_abbott_table_1.4    <- epi_custom(data = full_dataset, x = "truth", y = "CMIA Abbott 1.4")
cross_abbott_table_1.0    <- epi_custom(data = full_dataset, x = "truth", y = "CMIA Abbott 1.0")
cross_genetico_table_1.0  <- epi_custom(data = full_dataset, x = "truth", y = "ELISA Coronapass 1.0")
cross_vector_table_1.1    <- epi_custom(data = full_dataset, x = "truth", y = "ELISA Vector-best 1.1")

cross_roche_table_1.0     <- ci_for_roche(full_dataset) 


cross_se_table <- tibble(cross_genetico_table_1.0$rval$se) %>%
  bind_rows(cross_vector_table_1.1$rval$se)    %>%
  bind_rows(cross_roche_table_1.0)  %>%
  bind_rows(cross_abbott_table_1.0$rval$se)    %>%
  bind_rows(cross_abbott_table_1.4$rval$se)    %>%
  mutate(test     = c("genetico", "vector", "roche", "abbott1", "abbott14"),
         estimate = "sensitivity", 
         type     = "cross", 
         n        = 46)

cross_sp_table <- tibble(cross_genetico_table_1.0$rval$sp) %>%
  bind_rows(cross_vector_table_1.1$rval$sp) %>%
  bind_rows(cross_abbott_table_1.0$rval$sp) %>%
  bind_rows(cross_abbott_table_1.4$rval$sp) %>%
  mutate(test     = c("genetico", "vector", "abbott1", "abbott14"),
         estimate = "specificity", 
         type     = "cross", 
         n        = 41)

se_sp <- full_se_table %>%
  full_join(full_sp_table) %>%
  full_join(cross_sp_table) %>%
  full_join(cross_se_table) %>%
  mutate(
    test2 = case_when(
      test == "genetico"            ~ "ELISA Coronapass",
      test == "vector"              ~ "ELISA Vector-Best" ,
      test == "abbott1"             ~ "CMIA Abbott 1.0" ,
      test == "abbott14"            ~ "CMIA Abbott 1.4" ,
      test == "roche"               ~ "CMIA Roche")) %>%
  rename(dataset = type, 
         e       = estimate) %>% 
  mutate(across(c(est, lower, upper), ~ round(., 3))) %>% 
  mutate(estimate = paste0(100 * est, ' (', 100 * lower, "-", 100 * upper, ")"))

se_sp <- se_sp %>% 
  dplyr::select(test2, e, dataset, estimate) %>%
  arrange(dataset, test2) %>%
  pivot_wider(names_from  = "e", 
              values_from = "estimate") %>%
  relocate(test2, dataset, sensitivity, specificity)

  
se_sp_full <- se_sp %>%
  filter(dataset == "full")  %>%
  mutate(N_se = c(65, 65, 61, 92, 93),
         N_sp = c(60, 60, 0,  60, 48)) %>%
  dplyr::select(-dataset) %>%
  relocate(test2, N_se, sensitivity, N_sp, specificity) 

se_sp_cross <- se_sp %>%
  filter(dataset == "cross") %>%
  mutate(N_se = 46, 
         N_sp = 41) %>%
  select(-dataset)  %>%
  relocate(test2, N_se, sensitivity, N_sp, specificity) 

# latextable(se_sp_cross)
# latextable(se_sp_full)


# ROC curves

roc_full_abbott    <- roc(pcr ~ abbott,     data = full_abbott)
roc_full_vector    <- roc(pcr ~ vector,     data = full_vector)
roc_full_genetico  <- roc(pcr ~ genetico,   data = full_genetico)
roc_cross_abbott   <- roc(pcr ~ abbott,     data = full_dataset)
roc_cross_vector   <- roc(pcr ~ vector,     data = full_dataset)
roc_cross_genetico <- roc(pcr ~ genetico,   data = full_dataset)


set.seed(123)
ci_full_g  <- ci.auc(roc_full_genetico,   method = "b")
ci_full_a  <- ci.auc(roc_full_abbott,     method = "b")
ci_full_v  <- ci.auc(roc_full_vector,     method = "b")
ci_cross_g <- ci.auc(roc_cross_genetico,  method = "b")
ci_cross_a <- ci.auc(roc_cross_abbott,    method = "b")
ci_cross_v <- ci.auc(roc_cross_vector,    method = "b")



auc <- round(c(roc_full_genetico$auc,
             roc_full_abbott$auc,
             roc_full_vector$auc,
             roc_cross_genetico$auc,
             roc_cross_abbott$auc,
             roc_cross_genetico$auc), 2)

ci.low <- round(c(ci_full_g[1],
            ci_full_a[1],
            ci_full_v[1],
            ci_cross_g[1],
            ci_cross_a[1],
            ci_cross_v[1]), 2)

ci.upp <- round(c(ci_full_g[3],
            ci_full_a[3],
            ci_full_v[3],
            ci_cross_g[3],
            ci_cross_a[3],
            ci_cross_v[3]), 2)


test_full_ga  <- roc.test(roc_full_genetico, roc_full_abbott,   method = "b", paired = FALSE)
test_full_gv  <- roc.test(roc_full_genetico, roc_full_vector,   method = "b", paired = FALSE)
test_cross_ga <- roc.test(roc_cross_genetico, roc_cross_abbott, method = "b", paired = TRUE)
test_cross_gv <- roc.test(roc_cross_genetico, roc_cross_vector, method = "b", paired = TRUE)


p.value <- round(c(NA_real_, test_full_ga$p.value, test_full_gv$p.value, 
                   NA_real_, test_cross_ga$p.value, test_cross_gv$p.value), 3)

full_cross <- c("full", "full", "full", "cross", "cross", "cross")
test       <- c("ELISA Coronapass", "CMIA Abbott", "ELISA Vector-best",
                "ELISA Coronapass", "CMIA Abbott", "ELISA Vector-best")


auc.table <- tibble(test, 
                    full_cross, 
                    auc, 
                    ci.low, 
                    ci.upp, 
                    p.value) %>%
  mutate(auc2 = paste0(auc, ' (', ci.low, "-" , ci.upp, ")"))

auc.table_full <- auc.table %>% 
  filter(full_cross == "full") %>%
  dplyr::select(-c(full_cross, auc, ci.low, ci.upp)) %>%
  relocate(test, auc2, p.value)

auc.table_cross <- auc.table %>%
  filter(full_cross == "cross")  %>%
  dplyr::select(-c(full_cross, auc, ci.low, ci.upp)) %>%
  relocate(test, auc2, p.value)

# options("scipen" = 99, "digits" = 4)
# latextable(auc.table_cross)
# latextable(auc.table_full)



# plot AUC

to_string <- as_labeller(c(genetico = "ELISA Coronapass", 
                           vector = "ELISA Vector-best",
                           abbott = "ELISA Abbott"))

roc.list <- roc(pcr ~ genetico + abbott + vector, data = full_dataset)
g.list   <- ggroc(roc.list)

fig_1 <- g.list +
  facet_grid(. ~ name, labeller = to_string) +
  theme(legend.position = "none") +
  geom_line(size = 1) +
  theme_bw() +
  expand_limits(y = 0) +
  scale_color_tableau() +
  scale_y_continuous(breaks = pretty_breaks()) +
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
    axis.title        = element_text(size = 12, face = "bold", color = "black"),
    strip.background  = element_blank(),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    strip.placement = "outside") + 
  geom_abline(intercept = 1, slope = 1, color = "grey50", size = 0.8, linetype = 2) + 
  labs(y = "Sensitivity", 
       x = "1 - Specificity")


pdf("plot1.pdf", width = 8, height = 3)
fig_1
dev.off()



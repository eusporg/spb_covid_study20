ci_for_roche <- function(df) {
  
  n_roche               <- sum(!is.na(df$`CMIA Roche 1.0`))
  success_roche         <- sum(df$truth  == df$`CMIA Roche 1.0`, na.rm = TRUE)
  ci_roche              <- binom.test(success_roche, n_roche)
  return(list(est   = ci_roche$estimate,
              lower = ci_roche$conf.int[1],
              upper = ci_roche$conf.int[2]))
  
}


epi_custom <- function(data, x, y) { 
  
  tab <- table(pull(data, x), pull(data, y))
  
  epi.tests(dat = tab)
  
}


ci_auc <- partial(ci.auc, conf.level = 0.95, method = c("bootstrap"))


kappa_custom <- function(data, x, y) { 
  
  tab <- table(pull(data, x), pull(data, y))
  
  epi.kappa(tab, method = "fleiss", alternative = "two.sided", conf.level = 0.95)
  
  
}
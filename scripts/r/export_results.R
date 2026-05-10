# =============================================================================
# EXPORT REGRESSION RESULTS AS CSV
# =============================================================================

library(tidyverse)
library(fixest)
library(zoo)

# Hilfsfunktion: Koeffizient, SE, p-Wert, Sterne extrahieren
extract_coef <- function(model, var, model_name, spec, period, n_obs) {
  s <- summary(model)
  coefs <- s$coeftable
  
  if (!var %in% rownames(coefs)) return(NULL)
  
  est  <- coefs[var, "Estimate"]
  se   <- coefs[var, "Std. Error"]
  pval <- coefs[var, ncol(coefs)]
  
  stars <- case_when(
    pval < 0.001 ~ "***",
    pval < 0.01  ~ "**",
    pval < 0.05  ~ "*",
    pval < 0.1   ~ ".",
    TRUE         ~ ""
  )
  
  tibble(
    model    = model_name,
    spec     = spec,
    period   = period,
    variable = var,
    estimate = round(est, 4),
    se       = round(se, 4),
    pvalue   = round(pval, 4),
    stars    = stars,
    obs      = n_obs
  )
}

results <- bind_rows(
  extract_coef(ols_h1_mm,   "subsidy_x_fueldep_hbsir",     "OLS",  "H1", "MM 2005-2020",   5760),
  extract_coef(sls_h1_mm,   "fit_subsidy_x_fueldep_hbsir", "2SLS", "H1", "MM 2005-2020",   5400),
  extract_coef(sls_h2_mm,   "fit_subsidy_x_fueldep_c",     "2SLS", "H2", "MM 2005-2020",   5400),
  extract_coef(sls_h2_mm,   "fueldep_c",                   "2SLS", "H2", "MM 2005-2020",   5400),
  extract_coef(ols_h1_full, "subsidy_x_fueldep_hbsir",     "OLS",  "H1", "Full 2005-2022", 6480),
  extract_coef(sls_h1_full, "fit_subsidy_x_fueldep_hbsir", "2SLS", "H1", "Full 2005-2022", 6120),
  extract_coef(sls_h2_full, "fit_subsidy_x_fueldep_c",     "2SLS", "H2", "Full 2005-2022", 6120),
  extract_coef(sls_h2_full, "fueldep_c",                   "2SLS", "H2", "Full 2005-2022", 6120)
) %>%
  mutate(first_stage_F = case_when(
    period == "MM 2005-2020"   & spec == "H1" & model == "2SLS" ~ round(f_h1_mm, 2),
    period == "MM 2005-2020"   & spec == "H2" & model == "2SLS" ~ round(f_h2_mm, 2),
    period == "Full 2005-2022" & spec == "H1" & model == "2SLS" ~ round(f_h1_full, 2),
    period == "Full 2005-2022" & spec == "H2" & model == "2SLS" ~ round(f_h2_full, 2),
    TRUE ~ NA_real_
  ))

print(results)
write_csv(results, "~/Desktop/Regression/regression_results_main.csv")
cat("Saved: regression_results_main.csv\n")
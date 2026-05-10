library(tidyverse)
library(fixest)
library(zoo)

# =============================================================================
# DATA LOADING
# =============================================================================

panel <- read_csv("~/Desktop/panel_2005_2022.csv")

# =============================================================================
# COMBINED ONSET VARIABLE
# 2005-2020: onset_mm (Mass Mobilization)
# 2021-2022: onset_acled (ACLED, threshold = 1)
# =============================================================================

panel <- panel %>%
  mutate(onset = case_when(
    year <= 2020 ~ onset_mm,
    year >= 2021 ~ onset_acled
  ),
  time_id = year * 100 + month)

cat("Onset rate (combined):", round(mean(panel$onset, na.rm = TRUE) * 100, 2), "%\n")
cat("NAs in onset:", sum(is.na(panel$onset)), "\n")

# =============================================================================
# OLS: LINEAR PROBABILITY MODEL
# Province FE + Month-Year FE, clustered SE at province level
# =============================================================================

# H1: Baseline — effect of subsidy gap x fuel dependency on protest onset
ols_baseline <- feols(
  onset ~ subsidy_x_fueldep_hbsir | province + time_id,
  data    = panel,
  cluster = ~province
)

cat("\n=== OLS BASELINE ===\n")
print(summary(ols_baseline))

# H1 + H2: Add main effects separately to check components
# bmgap2015adj = subsidy gap (national, time-varying)
# fuel_dependency_hbsir = fuel dependency (province, time-varying)
ols_components <- feols(
  onset ~ bmgap2015adj + fuel_dependency_hbsir + subsidy_x_fueldep_hbsir | 
    province + time_id,
  data    = panel,
  cluster = ~province
)

cat("\n=== OLS WITH COMPONENTS ===\n")
print(summary(ols_components))

# =============================================================================
# RESULTS TABLE
# =============================================================================

etable(ols_baseline, ols_components,
       cluster = ~province,
       title   = "OLS Results: Subsidy Cuts and Protest Onset (LPM)")
# =============================================================================
# full_panel_appendix_b1.R
# Full Panel Analysis (2005-2022) — Appendix B1
# Author: Rosalie Adams (s4835859)
# Note: Standalone script — loads all data independently
# =============================================================================

library(tidyverse)
library(fixest)
library(zoo)
library(writexl)

# =============================================================================
# DATEN LADEN
# =============================================================================

brent <- read_csv("~/Desktop/Brent/DCOILBRENTEU (1).csv")

brent_monthly <- brent %>%
  rename(date = observation_date,
         brent_price = DCOILBRENTEU) %>%
  filter(!is.na(brent_price)) %>%
  mutate(date  = as.Date(date),
         year  = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarise(brent_price = mean(brent_price, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(year, month) %>%
  mutate(
    brent_ma12  = rollmean(brent_price, k = 12,
                           fill = NA, align = "right"),
    brent_shock = brent_price - lag(brent_ma12, 1)
  )

panel_full <- read_csv("~/Desktop/Panel/panel_2005_2022.csv") %>%
  mutate(
    onset = case_when(
      year <= 2020 ~ onset_mm,
      year >= 2021 ~ onset_acled
    ),
    month = as.integer(as.character(month))
  ) %>%
  left_join(brent_monthly %>%
              select(year, month, brent_shock),
            by = c("year", "month")) %>%
  mutate(
    brent_x_fueldep = brent_shock * fuel_dependency_hbsir,
    month = as.factor(month)
  )

cat("Panel rows:", nrow(panel_full), "\n")
cat("Years:", min(panel_full$year), "-", max(panel_full$year), "\n")
cat("Onset rate:", round(mean(panel_full$onset, na.rm = TRUE) * 100, 2), "%\n")

# =============================================================================
# 2SLS H1 — Full Panel ohne Controls
# =============================================================================

second_stage_full_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_full,
  cluster = ~province
)

cat("\n=== 2SLS H1 (Full Panel 2005-2022, ohne Controls) ===\n")
print(summary(second_stage_full_h1))
print(fitstat(second_stage_full_h1, "ivf"))

# =============================================================================
# 2SLS H2 — Full Panel ohne Controls
# =============================================================================

panel_full <- panel_full %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir -
      mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_full_h2 <- feols(
  onset ~ fueldep_c + bmgap_c | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_full,
  cluster = ~province
)

cat("\n=== 2SLS H2 (Full Panel 2005-2022, ohne Controls) ===\n")
print(summary(second_stage_full_h2))
print(fitstat(second_stage_full_h2, "ivf"))

# =============================================================================
# TABELLE EXPORTIEREN — Appendix B1
# =============================================================================

b1_results <- data.frame(
  Variable = c(
    "SubsidyExposure (instrumented)", "",
    "SubsidyExposure x FuelDep (instrumented)", "",
    "FuelDep (centered)", "",
    "Subsidy (centered)", "",
    "Province FE", "Month FE",
    "First-stage F", "Wu-Hausman p",
    "Observations", "Adjusted R2"
  ),
  H1 = c(
    round(coef(second_stage_full_h1)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(se(second_stage_full_h1)["fit_subsidy_x_fueldep_hbsir"], 3), ")"),
    "", "", "", "", "", "",
    "Yes", "Yes", "1,113", "< 0.001", "6,120", "0.052"
  ),
  H2 = c(
    "", "",
    round(coef(second_stage_full_h2)["fit_subsidy_x_fueldep_c"], 3),
    paste0("(", round(se(second_stage_full_h2)["fit_subsidy_x_fueldep_c"], 3), ")"),
    round(coef(second_stage_full_h2)["fueldep_c"], 3),
    paste0("(", round(se(second_stage_full_h2)["fueldep_c"], 3), ")"),
    round(coef(second_stage_full_h2)["bmgap_c"], 3),
    paste0("(", round(se(second_stage_full_h2)["bmgap_c"], 3), ")"),
    "Yes", "Yes", "1,204", "0.011", "6,120", "0.045"
  )
)

write_xlsx(
  list(B1_FullPanel = b1_results),
  "~/Desktop/full_panel_appendix_b1.xlsx"
)

cat("B1 exportiert!\n")

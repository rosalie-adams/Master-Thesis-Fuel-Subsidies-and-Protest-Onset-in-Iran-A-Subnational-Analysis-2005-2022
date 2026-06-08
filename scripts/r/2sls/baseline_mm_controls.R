# baseline_mm_with_controls.R
# MM-only Baseline (2005-2020) with Controls


library(writexl)
library(tidyverse)
library(fixest)
library(zoo)
library(readxl)


panel_raw <- read_csv("~/Desktop/Panel/panel_2005_2022.csv")

unemployment <- read_csv("~/Desktop/Iran open data unemployment/unemployment_provincial_2005_2020.csv") %>%
  rename(year = gregorian_year)

inflation <- read_csv("~/Desktop/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_115367.csv",
                      skip = 4) %>%
  filter(`Country Code` == "IRN") %>%
  select(-`Country Name`, -`Country Code`, 
         -`Indicator Name`, -`Indicator Code`) %>%
  pivot_longer(everything(), 
               names_to = "year", 
               values_to = "inflation_rate") %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2005 & year <= 2020) %>%
  filter(!is.na(inflation_rate))

sanctions <- read_excel(
  "~/Desktop/Controls/Laudati Sanctions Data/LP Iran Sanctions Data/data_master_quarterly.xlsx"
) %>%
  select(quarter, s_t, s_dummy) %>%
  mutate(
    year  = as.integer(substr(quarter, 1, 4)),
    q     = as.integer(substr(quarter, 7, 7)),
    month = case_when(
      q == 1 ~ list(c(1L, 2L, 3L)),
      q == 2 ~ list(c(4L, 5L, 6L)),
      q == 3 ~ list(c(7L, 8L, 9L)),
      q == 4 ~ list(c(10L, 11L, 12L))
    )
  ) %>%
  tidyr::unnest(month) %>%
  filter(year >= 2005 & year <= 2020) %>%
  select(year, month, s_t, s_dummy)

brent <- read_csv("~/Desktop/Brent/DCOILBRENTEU (1).csv")

# =============================================================================
# BRENT SHOCK
# =============================================================================

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

# =============================================================================
# PANEL KONSTRUKTION: MM-only 2005-2020 mit allen Controls
# =============================================================================

panel <- panel_raw %>%
  filter(year <= 2020) %>%
  mutate(
    onset   = onset_mm,
    month   = as.integer(as.character(month)),
    time_id = year * 100 + month
  ) %>%
  left_join(brent_monthly %>% 
              select(year, month, brent_shock),
            by = c("year", "month")) %>%
  left_join(unemployment, 
            by = c("province", "year")) %>%
  left_join(inflation, 
            by = "year") %>%
  left_join(sanctions, 
            by = c("year", "month")) %>%
  mutate(
    brent_x_fueldep = brent_shock * fuel_dependency_hbsir,
    month           = as.factor(month)
  )

# Checks
cat("Panel rows:", nrow(panel), "\n")
cat("Years:", min(panel$year), "-", max(panel$year), "\n")
cat("Onset rate:", round(mean(panel$onset, na.rm = TRUE) * 100, 2), "%\n")
cat("NAs unemployment:", sum(is.na(panel$unemployment_rate)), "\n")
cat("NAs inflation:", sum(is.na(panel$inflation_rate)), "\n")
cat("NAs sanctions:", sum(is.na(panel$s_t)), "\n")
cat("NAs brent_shock:", sum(is.na(panel$brent_shock)), "\n")

# =============================================================================
# OLS BASELINE H1 — MM mit Controls
# =============================================================================

ols_h1 <- feols(
  onset ~ subsidy_x_fueldep_hbsir + 
    unemployment_rate + inflation_rate + s_t | 
    province + month,
  data    = panel,
  cluster = ~province
)

cat("\n=== OLS H1 (MM 2005-2020, mit Controls) ===\n")
print(summary(ols_h1))

# =============================================================================
# 2SLS H1 — MM mit Controls
# =============================================================================

second_stage_h1 <- feols(
  onset ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel,
  cluster = ~province
)

cat("\n=== 2SLS H1 (MM 2005-2020, mit Controls) ===\n")
print(summary(second_stage_h1))
print(fitstat(second_stage_h1, "ivf"))

# =============================================================================
# 2SLS H2 — MM mit Controls (zentriert)
# =============================================================================

panel <- panel %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - 
      mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_h2 <- feols(
  onset ~ fueldep_c + bmgap_c + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel,
  cluster = ~province
)

cat("\n=== 2SLS H2 (MM 2005-2020, mit Controls) ===\n")
print(summary(second_stage_h2))
print(fitstat(second_stage_h2, "ivf"))

# =============================================================================
# TABELLEN EXPORTIEREN — MM-only mit Controls
# =============================================================================

library(writexl)

# H1 Ergebnisse mit SE
h1_results <- data.frame(
  Variable = c("SubsidyExposure (instrumented)",
               "",
               "Unemployment Rate",
               "",
               "Inflation Rate",
               "",
               "Sanctions (s_t)",
               "",
               "Province FE",
               "Month FE",
               "First-stage F",
               "Wu-Hausman p",
               "Observations",
               "Adjusted R²"),
  MM_only_with_controls = c("-1.758***",
                            "(0.215)",
                            "0.003",
                            "(0.005)",
                            "-0.002***",
                            "(0.000)",
                            "-0.053***",
                            "(0.008)",
                            "Yes",
                            "Yes",
                            "1,398",
                            "< 0.001",
                            "5,310",
                            "0.074")
)

# H2 Ergebnisse mit SE
h2_results <- data.frame(
  Variable = c("SubsidyExposure × FuelDep (instrumented)",
               "",
               "FuelDep (centered)",
               "",
               "Subsidy (centered)",
               "",
               "Unemployment Rate",
               "",
               "Inflation Rate",
               "",
               "Sanctions (s_t)",
               "",
               "Province FE",
               "Month FE",
               "First-stage F",
               "Wu-Hausman p",
               "Observations",
               "Adjusted R²"),
  MM_only_with_controls = c("-1.369***",
                            "(0.348)",
                            "0.389*",
                            "(0.149)",
                            "-0.015",
                            "(0.040)",
                            "0.003",
                            "(0.003)",
                            "-0.002***",
                            "(0.000)",
                            "0.057***",
                            "(0.006)",
                            "Yes",
                            "Yes",
                            "1,641",
                            "< 0.001",
                            "5,310",
                            "0.063")
)

write_xlsx(
  list(H1 = h1_results, H2 = h2_results),
  "~/Desktop/baseline_mm_controls_tables.xlsx"
)

cat("Tabellen exportiert!\n")

# OLS H1 Ergebnisse
ols_h1_results <- data.frame(
  Variable = c("SubsidyExposure",
               "",
               "Unemployment Rate",
               "",
               "Inflation Rate",
               "",
               "Sanctions (s_t)",
               "",
               "Province FE",
               "Month FE",
               "Observations",
               "Adjusted R²"),
  MM_only_with_controls = c(
    coef(ols_h1)["subsidy_x_fueldep_hbsir"] %>% round(3),  # keine Sternchen!,
    paste0("(", se(ols_h1)["subsidy_x_fueldep_hbsir"] %>% round(3), ")"),
    coef(ols_h1)["unemployment_rate"] %>% round(3),
    paste0("(", se(ols_h1)["unemployment_rate"] %>% round(3), ")"),
    coef(ols_h1)["inflation_rate"] %>% round(3) %>% paste0("***"),
    paste0("(", se(ols_h1)["inflation_rate"] %>% round(3), ")"),
    coef(ols_h1)["s_t"] %>% round(3) %>% paste0("***"),
    paste0("(", se(ols_h1)["s_t"] %>% round(3), ")"),
    "Yes",
    "Yes",
    nobs(ols_h1),
    r2(ols_h1, "ar2") %>% round(3)
  )
)

write_xlsx(
  list(OLS_H1 = ols_h1_results, H1 = h1_results, H2 = h2_results),
  "~/Desktop/baseline_mm_controls_tables.xlsx"
)

cat("Tabellen exportiert!\n")

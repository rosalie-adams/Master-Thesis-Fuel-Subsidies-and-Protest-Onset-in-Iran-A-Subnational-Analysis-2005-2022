# =============================================================================
# descriptive_statistics.R
# Descriptive Statistics — Full Panel 2005-2022
# Author: Rosalie Adams (s4835859)
# Date: April 2026
# =============================================================================

library(tidyverse)
library(zoo)

# =============================================================================
# DATA LOADING
# =============================================================================

panel <- read_csv("~/Desktop/Panel/panel_2005_2022.csv")
brent <- read_csv("~/Desktop/Brent/DCOILBRENTEU (1).csv")

brent_monthly <- brent %>%
  rename(date = observation_date, brent_price = DCOILBRENTEU) %>%
  filter(!is.na(brent_price)) %>%
  mutate(date  = as.Date(date),
         year  = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m"))) %>%
  group_by(year, month) %>%
  summarise(brent_price = mean(brent_price, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, month) %>%
  mutate(
    brent_ma12  = rollmean(brent_price, k = 12, fill = NA, align = "right"),
    brent_shock = brent_price - lag(brent_ma12, 1)
  )

panel <- panel %>%
  mutate(
    onset = case_when(
      year <= 2020 ~ onset_mm,
      year >= 2021 ~ onset_acled
    )
  ) %>%
  left_join(brent_monthly %>% select(year, month, brent_shock),
            by = c("year", "month"))

# =============================================================================
# OVERALL DESCRIPTIVE STATISTICS
# =============================================================================

vars <- c("onset", "bmgap2015adj", "fuel_dependency_hbsir",
          "subsidy_x_fueldep_hbsir", "brent_shock", "price_usd_2015")

desc_stats <- panel %>%
  select(all_of(vars)) %>%
  summarise(across(everything(), list(
    n    = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd   = ~sd(., na.rm = TRUE),
    min  = ~min(., na.rm = TRUE),
    p25  = ~quantile(., 0.25, na.rm = TRUE),
    p50  = ~median(., na.rm = TRUE),
    p75  = ~quantile(., 0.75, na.rm = TRUE),
    max  = ~max(., na.rm = TRUE)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(desc_stats)
write_csv(desc_stats, "~/Desktop/Regression/descriptive_statistics.csv")
cat("Saved: descriptive_statistics.csv\n")

# =============================================================================
# ONSET RATES BY YEAR
# =============================================================================

onset_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    onset_rate    = mean(onset, na.rm = TRUE) * 100,
    n_onsets      = sum(onset, na.rm = TRUE),
    n_obs         = n()
  ) %>%
  arrange(year)

print(onset_by_year)
write_csv(onset_by_year, "~/Desktop/Regression/onset_by_year.csv")
cat("Saved: onset_by_year.csv\n")

# =============================================================================
# FUEL DEPENDENCY BY PROVINCE (mean across years)
# =============================================================================

fueldep_by_province <- panel %>%
  group_by(province) %>%
  summarise(
    fueldep_mean = mean(fuel_dependency_hbsir, na.rm = TRUE),
    fueldep_sd   = sd(fuel_dependency_hbsir, na.rm = TRUE),
    fueldep_min  = min(fuel_dependency_hbsir, na.rm = TRUE),
    fueldep_max  = max(fuel_dependency_hbsir, na.rm = TRUE)
  ) %>%
  arrange(desc(fueldep_mean))

print(fueldep_by_province)
write_csv(fueldep_by_province, "~/Desktop/Regression/fueldep_by_province.csv")
cat("Saved: fueldep_by_province.csv\n")

# =============================================================================
# SUBSIDY GAP BY YEAR
# =============================================================================

subsidy_by_year <- panel %>%
  group_by(year) %>%
  summarise(
    bmgap_mean = mean(bmgap2015adj, na.rm = TRUE),
    price_mean = mean(price_usd_2015, na.rm = TRUE)
  ) %>%
  arrange(year)

print(subsidy_by_year)
write_csv(subsidy_by_year, "~/Desktop/Regression/subsidy_by_year.csv")
cat("Saved: subsidy_by_year.csv\n")
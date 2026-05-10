# =============================================================================
# 2sls_analysis_full_panel.R
# 2SLS Analysis: Full Panel 2005-2022 (MM + ACLED)
# Author: Rosalie Adams (s4835859)
# Supervisor: Babak RezaeeDaryakenari
# Date: April 2026
# =============================================================================

library(tidyverse)
library(fixest)
library(zoo)

# =============================================================================
# DATA LOADING
# =============================================================================

panel <- read_csv("~/Desktop/Panel/panel_2005_2022.csv")
brent <- read_csv("~/Desktop/Brent/DCOILBRENTEU (1).csv")

# =============================================================================
# BRENT SHOCK CONSTRUCTION
# Monthly average of daily prices, deviation from 12-month moving average
# =============================================================================

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

# =============================================================================
# PANEL CONSTRUCTION
# Combined onset: MM 2005-2020, ACLED 2021-2022
# =============================================================================

panel <- panel %>%
  mutate(
    onset   = case_when(
      year <= 2020 ~ onset_mm,
      year >= 2021 ~ onset_acled
    ),
    month   = as.integer(as.character(month)),
    time_id = year * 100 + month
  ) %>%
  left_join(brent_monthly %>% select(year, month, brent_shock),
            by = c("year", "month")) %>%
  mutate(
    brent_x_fueldep = brent_shock * fuel_dependency_hbsir,
    month           = as.factor(month)
  )

cat("Panel rows:", nrow(panel), "\n")
cat("Years:", min(panel$year), "-", max(panel$year), "\n")
cat("Onset rate (combined):", round(mean(panel$onset, na.rm = TRUE) * 100, 2), "%\n")
cat("Onset rate MM (2005-2020):", round(mean(panel$onset_mm, na.rm = TRUE) * 100, 2), "%\n")
cat("Onset rate ACLED (2021-2022):",
    round(mean(panel$onset_acled[panel$year >= 2021], na.rm = TRUE) * 100, 2), "%\n")
cat("NAs in brent_shock:", sum(is.na(panel$brent_shock)), "\n")
cat("NAs in onset:", sum(is.na(panel$onset)), "\n")

# =============================================================================
# OLS BASELINE — H1
# =============================================================================

ols_h1 <- feols(
  onset ~ subsidy_x_fueldep_hbsir | province + month,
  data    = panel,
  cluster = ~province
)

cat("\n=== OLS H1 (full panel 2005-2022) ===\n")
print(summary(ols_h1))

# =============================================================================
# 2SLS BASELINE — H1
# =============================================================================

first_stage <- feols(
  subsidy_x_fueldep_hbsir ~ brent_x_fueldep | province + month,
  data    = panel,
  cluster = ~province
)

second_stage_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel,
  cluster = ~province
)

cat("\n=== FIRST STAGE (full panel 2005-2022) ===\n")
print(summary(first_stage))
cat("IV F-statistic:")
print(fitstat(second_stage_h1, "ivf"))

cat("\n=== 2SLS H1 (full panel 2005-2022) ===\n")
print(summary(second_stage_h1))

etable(ols_h1, first_stage, second_stage_h1,
       cluster = ~province,
       title   = "Baseline Results: Full Panel 2005-2022")
# =============================================================================
# 2SLS H2 — FULL PANEL 2005-2022
# =============================================================================

panel <- panel %>%
  mutate(
    month   = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c
  ) %>%
  mutate(month = as.factor(month))

second_stage_h2 <- feols(
  onset ~ fueldep_c + bmgap_c | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel,
  cluster = ~province
)

cat("\n=== 2SLS H2 (full panel 2005-2022) ===\n")
print(summary(second_stage_h2))
cat("IV F-statistic:")
print(fitstat(second_stage_h2, "ivf"))
# =============================================================================
# ROBUSTNESS CHECK 1: TERZILEN (full panel)
# =============================================================================

panel <- panel %>% 
  mutate(
    month = as.integer(as.character(month)),
    fueldep_tercile = ntile(fuel_dependency_hbsir, 3)
  ) %>%
  mutate(month = as.factor(month))

panel_low_t  <- panel %>% filter(fueldep_tercile == 1)
panel_mid_t  <- panel %>% filter(fueldep_tercile == 2)
panel_high_t <- panel %>% filter(fueldep_tercile == 3)

second_stage_low_t <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_low_t,
  cluster = ~province
)

second_stage_mid_t <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_mid_t,
  cluster = ~province
)

second_stage_high_t <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_high_t,
  cluster = ~province
)

cat("\n=== TERZILEN (full panel) ===\n")
cat("Low tercile:  ", coef(second_stage_low_t)["fit_subsidy_x_fueldep_hbsir"], "\n")
cat("Mid tercile:  ", coef(second_stage_mid_t)["fit_subsidy_x_fueldep_hbsir"], "\n")
cat("High tercile: ", coef(second_stage_high_t)["fit_subsidy_x_fueldep_hbsir"], "\n")

confint(second_stage_low_t)
confint(second_stage_mid_t)
confint(second_stage_high_t)
# =============================================================================
# ROBUSTNESS CHECK 2: CBI ALTERNATIVE FUEL DEPENDENCY (full panel)
# Coverage: 2005-2015 only
# =============================================================================

cbi <- read_csv("~/Desktop/Fuel Dependency/CBI Transport Distribution Rate per Province - Tabellenblatt1 (1).csv")

cbi_long <- cbi %>%
  select(`Gregorian Year`,
         `Azarbaijan East`, `Azarbaijan West`, Ardabil, Isfahan, Ilam,
         Boushehr, `Tehran + Alborz`, `Chaharmahal e Bakhtiari`,
         `Khorasan South`, `Khorasan Razavi`, `Khorasan North`,
         Khozestan, Zanjun, Semnan, `Sistan & Balochestan`,
         Fars, Qazvin, Qom, Kordestan, Kerman, Kermanshah,
         `Kohgiluyeh and Boyer-Ahmad`, Golestan, Gilan, Lorestan,
         Mazandaran, Markazi, Hormozgan, Hamadan, Yazd) %>%
  pivot_longer(-`Gregorian Year`,
               names_to = "province_cbi",
               values_to = "fueldep_cbi") %>%
  rename(year = `Gregorian Year`) %>%
  mutate(province = recode(province_cbi,
                           "Azarbaijan East"          = "East Azerbaijan",
                           "Azarbaijan West"          = "West Azerbaijan",
                           "Boushehr"                 = "Bushehr",
                           "Tehran + Alborz"          = "Tehran",
                           "Chaharmahal e Bakhtiari"  = "Chaharmahal and Bakhtiari",
                           "Khorasan South"           = "South Khorasan",
                           "Khorasan Razavi"          = "Razavi Khorasan",
                           "Khorasan North"           = "North Khorasan",
                           "Khozestan"                = "Khuzestan",
                           "Zanjun"                   = "Zanjan",
                           "Sistan & Balochestan"     = "Sistan and Baluchestan",
                           "Kordestan"                = "Kurdistan",
                           "Hamadan"                  = "Hamedan"
  )) %>%
  select(province, year, fueldep_cbi) %>%
  filter(!is.na(fueldep_cbi))

# Merge into full panel (2005-2015 only)
panel_cbi <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  filter(year <= 2015) %>%
  left_join(cbi_long, by = c("province", "year")) %>%
  mutate(
    month                 = as.factor(month),
    subsidy_x_fueldep_cbi = bmgap2015adj * fueldep_cbi,
    brent_x_fueldep_cbi   = brent_shock * fueldep_cbi,
    bmgap_c_cbi           = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_cbi_c         = fueldep_cbi - mean(fueldep_cbi, na.rm = TRUE),
    subsidy_x_fueldep_cbi_c = bmgap_c_cbi * fueldep_cbi_c,
    brent_x_fueldep_cbi_c   = brent_shock * fueldep_cbi_c
  )

cat("NAs in fueldep_cbi:", sum(is.na(panel_cbi$fueldep_cbi)), "\n")

# H1 with CBI
second_stage_cbi_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_cbi ~ brent_x_fueldep_cbi,
  data    = panel_cbi,
  cluster = ~province
)

# H2 with CBI
second_stage_cbi_h2 <- feols(
  onset ~ fueldep_cbi_c + bmgap_c_cbi | province + month |
    subsidy_x_fueldep_cbi_c ~ brent_x_fueldep_cbi_c,
  data    = panel_cbi,
  cluster = ~province
)

cat("\n=== CBI ROBUSTNESS H1 (full panel) ===\n")
print(summary(second_stage_cbi_h1))
cat("IV F-statistic:")
print(fitstat(second_stage_cbi_h1, "ivf"))

cat("\n=== CBI ROBUSTNESS H2 (full panel) ===\n")
print(summary(second_stage_cbi_h2))
cat("IV F-statistic:")
print(fitstat(second_stage_cbi_h2, "ivf"))
# =============================================================================
# ROBUSTNESS CHECK 3: SANCTIONS CONTROL (full panel)
# =============================================================================

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

# Merge into full panel
panel_sanctions <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(sanctions, by = c("year", "month")) %>%
  mutate(month = as.factor(month))

cat("NAs in s_t after merge:", sum(is.na(panel_sanctions$s_t)), "\n")

# H1 + continuous sanctions
second_stage_sanctions_cont <- feols(
  onset ~ s_t | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_sanctions,
  cluster = ~province
)

# H1 + binary sanctions dummy
second_stage_sanctions_bin <- feols(
  onset ~ s_dummy | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_sanctions,
  cluster = ~province
)

# H2 + sanctions (centered)
panel_sanctions <- panel_sanctions %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_sanctions_h2_cont <- feols(
  onset ~ fueldep_c + bmgap_c + s_t | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_sanctions,
  cluster = ~province
)

second_stage_sanctions_h2_bin <- feols(
  onset ~ fueldep_c + bmgap_c + s_dummy | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_sanctions,
  cluster = ~province
)

cat("\n=== H1 + SANCTIONS CONTINUOUS (full panel) ===\n")
print(summary(second_stage_sanctions_cont))

cat("\n=== H1 + SANCTIONS DUMMY (full panel) ===\n")
print(summary(second_stage_sanctions_bin))

cat("\n=== H2 + SANCTIONS CONTINUOUS (full panel) ===\n")
print(summary(second_stage_sanctions_h2_cont))

cat("\n=== H2 + SANCTIONS DUMMY (full panel) ===\n")
print(summary(second_stage_sanctions_h2_bin))
# =============================================================================
# ROBUSTNESS CHECK 4: LEAVE-ONE-OUT (full panel)
# =============================================================================

provinces <- unique(panel$province)

# H1 leave-one-out
loo_results_h1 <- tibble(province_left_out = character(),
                         coef = numeric(),
                         se   = numeric())

for (p in provinces) {
  panel_loo <- panel %>% filter(province != p)
  
  m <- feols(
    onset ~ 1 | province + month |
      subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
    data    = panel_loo,
    cluster = ~province
  )
  
  loo_results_h1 <- loo_results_h1 %>%
    add_row(
      province_left_out = p,
      coef = coef(m)["fit_subsidy_x_fueldep_hbsir"],
      se   = se(m)["fit_subsidy_x_fueldep_hbsir"]
    )
}

cat("\n=== LEAVE-ONE-OUT H1 (full panel) ===\n")
print(loo_results_h1 %>% arrange(coef))
cat("Range:", round(min(loo_results_h1$coef), 3),
    "to", round(max(loo_results_h1$coef), 3), "\n")
cat("Baseline: -1.315\n")

# H2 leave-one-out
loo_results_h2 <- tibble(province_left_out = character(),
                         coef = numeric(),
                         se   = numeric())

for (p in provinces) {
  panel_loo <- panel %>%
    filter(province != p) %>%
    mutate(
      month               = as.integer(as.character(month)),
      bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
      fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
      subsidy_x_fueldep_c = bmgap_c * fueldep_c,
      brent_x_fueldep_c   = brent_shock * fueldep_c,
      month               = as.factor(month)
    )
  
  m <- feols(
    onset ~ fueldep_c + bmgap_c | province + month |
      subsidy_x_fueldep_c ~ brent_x_fueldep_c,
    data    = panel_loo,
    cluster = ~province
  )
  
  loo_results_h2 <- loo_results_h2 %>%
    add_row(
      province_left_out = p,
      coef = coef(m)["fit_subsidy_x_fueldep_c"],
      se   = se(m)["fit_subsidy_x_fueldep_c"]
    )
}

cat("\n=== LEAVE-ONE-OUT H2 (full panel) ===\n")
print(loo_results_h2 %>% arrange(coef))
cat("Range:", round(min(loo_results_h2$coef), 3),
    "to", round(max(loo_results_h2$coef), 3), "\n")
cat("Baseline: -0.920\n")
# =============================================================================
# ROBUSTNESS CHECK 5: SUBPERIODEN (full panel)
# =============================================================================

# Grüne Bewegung (2008-2011)
panel_green <- panel %>% filter(year >= 2008 & year <= 2011)

cat("Green Movement observations:", nrow(panel_green), "\n")
cat("Onset rate:", round(mean(panel_green$onset, na.rm = TRUE) * 100, 2), "%\n")

second_stage_green_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_green,
  cluster = ~province
)

panel_green <- panel_green %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_green_h2 <- feols(
  onset ~ fueldep_c + bmgap_c | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_green,
  cluster = ~province
)

cat("\n=== H1 GRÜNE BEWEGUNG (2008-2011) ===\n")
print(summary(second_stage_green_h1))

cat("\n=== H2 GRÜNE BEWEGUNG (2008-2011) ===\n")
print(summary(second_stage_green_h2))

# Wirtschaftsproteste (2017-2019)
panel_econ <- panel %>% filter(year >= 2017 & year <= 2019)

cat("\nEconomic protest observations:", nrow(panel_econ), "\n")
cat("Onset rate:", round(mean(panel_econ$onset, na.rm = TRUE) * 100, 2), "%\n")

second_stage_econ_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_econ,
  cluster = ~province
)

panel_econ <- panel_econ %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_econ_h2 <- feols(
  onset ~ fueldep_c + bmgap_c | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_econ,
  cluster = ~province
)

cat("\n=== H1 WIRTSCHAFTSPROTESTE (2017-2019) ===\n")
print(summary(second_stage_econ_h1))

cat("\n=== H2 WIRTSCHAFTSPROTESTE (2017-2019) ===\n")
print(summary(second_stage_econ_h2))
# =============================================================================
# ROBUSTNESS CHECK 6: UNEMPLOYMENT CONTROL (full panel)
# =============================================================================

unemployment <- read_csv("~/Desktop/unemployment_provincial_2005_2020.csv") %>%
  rename(year = gregorian_year)

panel_unemp <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(unemployment, by = c("province", "year")) %>%
  mutate(month = as.factor(month))

cat("NAs in unemployment_rate:", sum(is.na(panel_unemp$unemployment_rate)), "\n")

# H1 + unemployment
second_stage_unemp_h1 <- feols(
  onset ~ unemployment_rate | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_unemp,
  cluster = ~province
)

# H2 + unemployment
panel_unemp <- panel_unemp %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_unemp_h2 <- feols(
  onset ~ fueldep_c + bmgap_c + unemployment_rate | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_unemp,
  cluster = ~province
)

cat("\n=== H1 + UNEMPLOYMENT (full panel) ===\n")
print(summary(second_stage_unemp_h1))

cat("\n=== H2 + UNEMPLOYMENT (full panel) ===\n")
print(summary(second_stage_unemp_h2))

# =============================================================================
# ROBUSTNESS CHECK 7: INFLATION CONTROL (full panel)
# =============================================================================

inflation <- read_csv(
  "~/Desktop/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_115367.csv",
  skip = 4
) %>%
  filter(`Country Code` == "IRN") %>%
  select(-`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  pivot_longer(everything(), names_to = "year", values_to = "inflation_rate") %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2005 & year <= 2022) %>%
  filter(!is.na(inflation_rate))

panel_inflation <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(inflation, by = "year") %>%
  mutate(month = as.factor(month))

cat("NAs in inflation_rate:", sum(is.na(panel_inflation$inflation_rate)), "\n")

# H1 + inflation
second_stage_inflation_h1 <- feols(
  onset ~ inflation_rate | province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_inflation,
  cluster = ~province
)

# H2 + inflation
panel_inflation <- panel_inflation %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    brent_x_fueldep_c   = brent_shock * fueldep_c,
    month               = as.factor(month)
  )

second_stage_inflation_h2 <- feols(
  onset ~ fueldep_c + bmgap_c + inflation_rate | province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_inflation,
  cluster = ~province
)

cat("\n=== H1 + INFLATION (full panel) ===\n")
print(summary(second_stage_inflation_h1))

cat("\n=== H2 + INFLATION (full panel) ===\n")
print(summary(second_stage_inflation_h2))

# =============================================================================
# ROBUSTNESS CHECK 8: BENZIN + CNG (full panel)
# =============================================================================

fuel_cng <- read_csv(
  "~/Desktop/Fuel Dependency/fuel_dependency_hbsir_benzin_cng_2005_2022.csv"
) %>%
  mutate(province = str_replace_all(province, "_", " "),
         province = recode(province,
                           "Hamadan"                    = "Hamedan",
                           "Kohgiluyeh and Boyer Ahmad" = "Kohgiluyeh and Boyer-Ahmad")) %>%
  rename(fueldep_cng = fuel_dependency_hbsir) %>%
  select(province, year, fueldep_cng)

panel_cng <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(fuel_cng, by = c("province", "year")) %>%
  mutate(
    month                 = as.factor(month),
    subsidy_x_fueldep_cng = bmgap2015adj * fueldep_cng,
    brent_x_fueldep_cng   = brent_shock * fueldep_cng,
    bmgap_c               = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_cng_c         = fueldep_cng - mean(fueldep_cng, na.rm = TRUE),
    subsidy_x_fueldep_cng_c = bmgap_c * fueldep_cng_c,
    brent_x_fueldep_cng_c   = brent_shock * fueldep_cng_c
  )

cat("NAs in fueldep_cng:", sum(is.na(panel_cng$fueldep_cng)), "\n")

# H1
second_stage_cng_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_cng ~ brent_x_fueldep_cng,
  data    = panel_cng,
  cluster = ~province
)

# H2
second_stage_cng_h2 <- feols(
  onset ~ fueldep_cng_c + bmgap_c | province + month |
    subsidy_x_fueldep_cng_c ~ brent_x_fueldep_cng_c,
  data    = panel_cng,
  cluster = ~province
)

cat("\n=== H1 BENZIN + CNG (full panel) ===\n")
print(summary(second_stage_cng_h1))

cat("\n=== H2 BENZIN + CNG (full panel) ===\n")
print(summary(second_stage_cng_h2))
# =============================================================================
# ROBUSTNESS CHECK 9: TIME-CONSTANT FUEL DEPENDENCY (full panel)
# =============================================================================

fueldep_constant <- panel %>%
  group_by(province) %>%
  summarise(fueldep_constant = mean(fuel_dependency_hbsir, na.rm = TRUE))

panel_constant <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(fueldep_constant, by = "province") %>%
  mutate(
    month                          = as.factor(month),
    subsidy_x_fueldep_constant     = bmgap2015adj * fueldep_constant,
    brent_x_fueldep_constant       = brent_shock * fueldep_constant,
    bmgap_c                        = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_constant_c             = fueldep_constant - mean(fueldep_constant, na.rm = TRUE),
    subsidy_x_fueldep_constant_c   = bmgap_c * fueldep_constant_c,
    brent_x_fueldep_constant_c     = brent_shock * fueldep_constant_c
  )

# H1
second_stage_constant_h1 <- feols(
  onset ~ 1 | province + month |
    subsidy_x_fueldep_constant ~ brent_x_fueldep_constant,
  data    = panel_constant,
  cluster = ~province
)

# H2
second_stage_constant_h2 <- feols(
  onset ~ fueldep_constant_c + bmgap_c | province + month |
    subsidy_x_fueldep_constant_c ~ brent_x_fueldep_constant_c,
  data    = panel_constant,
  cluster = ~province
)

cat("\n=== H1 TIME-CONSTANT FUEL DEPENDENCY (full panel) ===\n")
print(summary(second_stage_constant_h1))

cat("\n=== H2 TIME-CONSTANT FUEL DEPENDENCY (full panel) ===\n")
# =============================================================================
# ROBUSTNESS CHECK: PROBIT (MLE) — MM PERIOD (2005-2020)
# =============================================================================

# MM Panel neu laden
panel_mm_probit <- read_csv("~/Desktop/Panel/panel_2005_2022.csv") %>%
  filter(year <= 2020) %>%
  mutate(month = as.factor(month))

# Probit H1 — MM
probit_h1_mm <- feglm(
  onset_mm ~ subsidy_x_fueldep_hbsir | province + month,
  data   = panel_mm_probit,
  family = binomial(link = "probit"),
  cluster = ~province
)

# Probit H2 — MM (centered)
panel_mm_probit <- panel_mm_probit %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    month               = as.factor(month)
  )

probit_h2_mm <- feglm(
  onset_mm ~ subsidy_x_fueldep_c + fueldep_c + bmgap_c | province + month,
  data   = panel_mm_probit,
  family = binomial(link = "probit"),
  cluster = ~province
)

cat("\n=== PROBIT H1 (MM 2005-2020) ===\n")
print(summary(probit_h1_mm))

cat("\n=== PROBIT H2 (MM 2005-2020) ===\n")
print(summary(probit_h2_mm))

# =============================================================================
# ROBUSTNESS CHECK: PROBIT (MLE)
# Author: Rosalie Adams (s4835859)
# Date: April 2026
#
# Probit models as robustness check for the linear probability model (LPM)
# estimated via 2SLS. No IV — endogeneity not addressed here.
# Serves as check on functional form assumption of LPM.
# =============================================================================

library(tidyverse)
library(fixest)

# =============================================================================
# MM PERIOD (2005-2020)
# =============================================================================

panel_mm_probit <- read_csv("~/Desktop/Panel/panel_2005_2022.csv") %>%
  filter(year <= 2020) %>%
  mutate(month = as.factor(month))

# H1 — MM
probit_h1_mm <- feglm(
  onset_mm ~ subsidy_x_fueldep_hbsir | province + month,
  data    = panel_mm_probit,
  family  = binomial(link = "probit"),
  cluster = ~province
)

# H2 — MM (centered)
panel_mm_probit <- panel_mm_probit %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    month               = as.factor(month)
  )

probit_h2_mm <- feglm(
  onset_mm ~ subsidy_x_fueldep_c + fueldep_c + bmgap_c | province + month,
  data    = panel_mm_probit,
  family  = binomial(link = "probit"),
  cluster = ~province
)

cat("\n=== PROBIT H1 (MM 2005-2020) ===\n")
print(summary(probit_h1_mm))

cat("\n=== PROBIT H2 (MM 2005-2020) ===\n")
print(summary(probit_h2_mm))

# =============================================================================
# FULL PANEL (2005-2022)
# =============================================================================

panel_full_probit <- read_csv("~/Desktop/Panel/panel_2005_2022.csv") %>%
  mutate(
    onset = case_when(
      year <= 2020 ~ onset_mm,
      year >= 2021 ~ onset_acled
    ),
    month = as.factor(month)
  )

# H1 — full panel
probit_h1_full <- feglm(
  onset ~ subsidy_x_fueldep_hbsir | province + month,
  data    = panel_full_probit,
  family  = binomial(link = "probit"),
  cluster = ~province
)

# H2 — full panel (centered)
panel_full_probit <- panel_full_probit %>%
  mutate(
    month               = as.integer(as.character(month)),
    bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_c           = fuel_dependency_hbsir - mean(fuel_dependency_hbsir, na.rm = TRUE),
    subsidy_x_fueldep_c = bmgap_c * fueldep_c,
    month               = as.factor(month)
  )

probit_h2_full <- feglm(
  onset ~ subsidy_x_fueldep_c + fueldep_c + bmgap_c | province + month,
  data    = panel_full_probit,
  family  = binomial(link = "probit"),
  cluster = ~province
)

cat("\n=== PROBIT H1 (full panel 2005-2022) ===\n")
print(summary(probit_h1_full))

cat("\n=== PROBIT H2 (full panel 2005-2022) ===\n")
print(summary(probit_h2_full))

# =============================================================================
# RESULTS TABLE
# =============================================================================

etable(probit_h1_mm, probit_h2_mm, probit_h1_full, probit_h2_full,
       cluster = ~province,
       title   = "Probit Results: MLE Robustness Check")
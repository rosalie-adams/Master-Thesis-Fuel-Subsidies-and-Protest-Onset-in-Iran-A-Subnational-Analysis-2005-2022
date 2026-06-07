# =============================================================================
# final_robustness_checks_mm.R
# Robustness Checks — MM-only (2005-2020)
# Author: Rosalie Adams (s4835859)
# Note: Run baseline_mm_controls.R first to load panel and objects
# =============================================================================

library(tidyverse)
library(fixest)
library(zoo)
library(readxl)
library(writexl)
library(ggplot2)

# =============================================================================
# C1: Robustness Check — Tercile Subsamples (MM-only 2005-2020)
# =============================================================================

panel_mm_tercile <- panel %>%
  filter(year <= 2020) %>%
  mutate(
    month = as.integer(as.character(month)),
    onset = onset_mm,
    fueldep_tercile = ntile(fuel_dependency_hbsir, 3),
    month = as.factor(month)
  )

panel_low_t  <- panel_mm_tercile %>% filter(fueldep_tercile == 1)
panel_mid_t  <- panel_mm_tercile %>% filter(fueldep_tercile == 2)
panel_high_t <- panel_mm_tercile %>% filter(fueldep_tercile == 3)

second_stage_low_t <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_low_t,
  cluster = ~province
)

second_stage_mid_t <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_mid_t,
  cluster = ~province
)

second_stage_high_t <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
  data    = panel_high_t,
  cluster = ~province
)

cat("\n=== C1: TERCILE SUBSAMPLES (MM-only 2005-2020) ===\n")
cat("Low tercile:  ", round(coef(second_stage_low_t)["fit_subsidy_x_fueldep_hbsir"], 3), "\n")
cat("Mid tercile:  ", round(coef(second_stage_mid_t)["fit_subsidy_x_fueldep_hbsir"], 3), "\n")
cat("High tercile: ", round(coef(second_stage_high_t)["fit_subsidy_x_fueldep_hbsir"], 3), "\n")
cat("\nKonfidenzintervalle:\n")
print(confint(second_stage_low_t))
print(confint(second_stage_mid_t))
print(confint(second_stage_high_t))

tercile_results <- data.frame(
  Tercile = c("Low (1)", "", "Mid (2)", "", "High (3)", ""),
  Coefficient = c(
    round(coef(second_stage_low_t)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(confint(second_stage_low_t)["fit_subsidy_x_fueldep_hbsir", 1], 3),
           ", ", round(confint(second_stage_low_t)["fit_subsidy_x_fueldep_hbsir", 2], 3), ")"),
    round(coef(second_stage_mid_t)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(confint(second_stage_mid_t)["fit_subsidy_x_fueldep_hbsir", 1], 3),
           ", ", round(confint(second_stage_mid_t)["fit_subsidy_x_fueldep_hbsir", 2], 3), ")"),
    round(coef(second_stage_high_t)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(confint(second_stage_high_t)["fit_subsidy_x_fueldep_hbsir", 1], 3),
           ", ", round(confint(second_stage_high_t)["fit_subsidy_x_fueldep_hbsir", 2], 3), ")")
  )
)

write_xlsx(
  list(C1_Terciles = tercile_results),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C1 exportiert!\n")

# =============================================================================
# C2: Robustness Check — CBI Alternative Fuel Dependency (MM-only, 2005-2015)
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

panel_cbi <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  filter(year <= 2015) %>%
  left_join(cbi_long, by = c("province", "year")) %>%
  mutate(
    month                   = as.factor(month),
    subsidy_x_fueldep_cbi   = bmgap2015adj * fueldep_cbi,
    brent_x_fueldep_cbi     = brent_shock * fueldep_cbi,
    bmgap_c_cbi             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_cbi_c           = fueldep_cbi - mean(fueldep_cbi, na.rm = TRUE),
    subsidy_x_fueldep_cbi_c = bmgap_c_cbi * fueldep_cbi_c,
    brent_x_fueldep_cbi_c   = brent_shock * fueldep_cbi_c
  )

second_stage_cbi_h1 <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_cbi ~ brent_x_fueldep_cbi,
  data    = panel_cbi,
  cluster = ~province
)

second_stage_cbi_h2 <- feols(
  onset_mm ~ fueldep_cbi_c + bmgap_c_cbi + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_cbi_c ~ brent_x_fueldep_cbi_c,
  data    = panel_cbi,
  cluster = ~province
)

cat("\n=== C2: CBI H1 (MM-only 2005-2015) ===\n")
print(summary(second_stage_cbi_h1))
cat("\n=== C2: CBI H2 (MM-only 2005-2015) ===\n")
print(summary(second_stage_cbi_h2))

cbi_results <- data.frame(
  Specification = c("CBI H1", "", "CBI H2 Interaction", ""),
  Estimate = c(
    round(coef(second_stage_cbi_h1)["fit_subsidy_x_fueldep_cbi"], 3),
    paste0("(", round(se(second_stage_cbi_h1)["fit_subsidy_x_fueldep_cbi"], 3), ")"),
    round(coef(second_stage_cbi_h2)["fit_subsidy_x_fueldep_cbi_c"], 3),
    paste0("(", round(se(second_stage_cbi_h2)["fit_subsidy_x_fueldep_cbi_c"], 3), ")")
  ),
  P_value = c("< 0.001", "", "0.914", "")
)

write_xlsx(
  list(
    C1_Terciles = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI      = cbi_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C2 exportiert!\n")

# =============================================================================
# C3: Robustness Check — Leave-One-Out (MM-only 2005-2020)
# =============================================================================

provinces <- unique(panel$province)

loo_results_h1 <- tibble(province_left_out = character(),
                         coef = numeric(),
                         se   = numeric())

for (p in provinces) {
  panel_loo <- panel %>% filter(province != p)
  m <- feols(
    onset_mm ~ unemployment_rate + inflation_rate + s_t | 
      province + month |
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
    onset_mm ~ fueldep_c + bmgap_c + 
      unemployment_rate + inflation_rate + s_t | 
      province + month |
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

cat("\n=== C3: LEAVE-ONE-OUT H1 (MM-only 2005-2020) ===\n")
print(loo_results_h1 %>% arrange(coef))
cat("Range:", round(min(loo_results_h1$coef), 3),
    "to", round(max(loo_results_h1$coef), 3), "\n")
cat("Baseline: -1.758\n")

cat("\n=== C3: LEAVE-ONE-OUT H2 (MM-only 2005-2020) ===\n")
print(loo_results_h2 %>% arrange(coef))
cat("Range:", round(min(loo_results_h2$coef), 3),
    "to", round(max(loo_results_h2$coef), 3), "\n")
cat("Baseline: -1.369\n")

write_xlsx(
  list(
    C1_Terciles = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI      = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO      = bind_rows(
      loo_results_h1 %>% mutate(Hypothesis = "H1"),
      loo_results_h2 %>% mutate(Hypothesis = "H2")
    )
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C3 exportiert!\n")

# =============================================================================
# C4: Robustness Check — Subperiod Regressions (MM-only)
# =============================================================================

panel_green <- panel %>% filter(year >= 2008 & year <= 2011)

second_stage_green_h1 <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
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
  onset_mm ~ fueldep_c + bmgap_c + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_green,
  cluster = ~province
)

panel_econ <- panel %>% filter(year >= 2017 & year <= 2019)

second_stage_econ_h1 <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
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
  onset_mm ~ fueldep_c + bmgap_c + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_c ~ brent_x_fueldep_c,
  data    = panel_econ,
  cluster = ~province
)

cat("\n=== C4: H1 GREEN MOVEMENT (2008-2011) ===\n")
print(summary(second_stage_green_h1))
cat("\n=== C4: H2 GREEN MOVEMENT (2008-2011) ===\n")
print(summary(second_stage_green_h2))
cat("\n=== C4: H1 ECONOMIC PROTESTS (2017-2019) ===\n")
print(summary(second_stage_econ_h1))
cat("\n=== C4: H2 ECONOMIC PROTESTS (2017-2019) ===\n")
print(summary(second_stage_econ_h2))

subperiod_results <- data.frame(
  Specification = c(
    "Green Movement H1", "",
    "Green Movement H2 Interaction", "",
    "Economic Protests H1", "",
    "Economic Protests H2 Interaction", ""
  ),
  Estimate = c(
    round(coef(second_stage_green_h1)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(se(second_stage_green_h1)["fit_subsidy_x_fueldep_hbsir"], 3), ")"),
    round(coef(second_stage_green_h2)["fit_subsidy_x_fueldep_c"], 3),
    paste0("(", round(se(second_stage_green_h2)["fit_subsidy_x_fueldep_c"], 3), ")"),
    round(coef(second_stage_econ_h1)["fit_subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(se(second_stage_econ_h1)["fit_subsidy_x_fueldep_hbsir"], 3), ")"),
    round(coef(second_stage_econ_h2)["fit_subsidy_x_fueldep_c"], 3),
    paste0("(", round(se(second_stage_econ_h2)["fit_subsidy_x_fueldep_c"], 3), ")")
  ),
  P_value = c("< 0.001", "", "0.009", "", "< 0.001", "", "0.983", "")
)

write_xlsx(
  list(
    C1_Terciles   = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C3_LOO"),
    C4_Subperiods = subperiod_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C4 exportiert!\n")

# =============================================================================
# C5: Robustness Check — Gasoline + CNG Fuel Dependency (MM-only 2005-2020)
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
    month                   = as.factor(month),
    subsidy_x_fueldep_cng   = bmgap2015adj * fueldep_cng,
    brent_x_fueldep_cng     = brent_shock * fueldep_cng,
    bmgap_c                 = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_cng_c           = fueldep_cng - mean(fueldep_cng, na.rm = TRUE),
    subsidy_x_fueldep_cng_c = bmgap_c * fueldep_cng_c,
    brent_x_fueldep_cng_c   = brent_shock * fueldep_cng_c
  )

second_stage_cng_h1 <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_cng ~ brent_x_fueldep_cng,
  data    = panel_cng,
  cluster = ~province
)

second_stage_cng_h2 <- feols(
  onset_mm ~ fueldep_cng_c + bmgap_c + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_cng_c ~ brent_x_fueldep_cng_c,
  data    = panel_cng,
  cluster = ~province
)

cat("\n=== C5: H1 GASOLINE + CNG (MM-only 2005-2020) ===\n")
print(summary(second_stage_cng_h1))
cat("\n=== C5: H2 GASOLINE + CNG (MM-only 2005-2020) ===\n")
print(summary(second_stage_cng_h2))

cng_results <- data.frame(
  Specification = c("H1 Gasoline + CNG", "",
                    "H2 Interaction Gasoline + CNG", ""),
  Estimate = c(
    round(coef(second_stage_cng_h1)["fit_subsidy_x_fueldep_cng"], 3),
    paste0("(", round(se(second_stage_cng_h1)["fit_subsidy_x_fueldep_cng"], 3), ")"),
    round(coef(second_stage_cng_h2)["fit_subsidy_x_fueldep_cng_c"], 3),
    paste0("(", round(se(second_stage_cng_h2)["fit_subsidy_x_fueldep_cng_c"], 3), ")")
  ),
  P_value = c("< 0.001", "", "< 0.001", "")
)

write_xlsx(
  list(
    C1_Terciles   = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C3_LOO"),
    C4_Subperiods = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C4_Subperiods"),
    C5_CNG        = cng_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C5 exportiert!\n")

# =============================================================================
# C6: Robustness Check — Time-Constant Fuel Dependency (MM-only 2005-2020)
# =============================================================================

fueldep_constant <- panel %>%
  group_by(province) %>%
  summarise(fueldep_constant = mean(fuel_dependency_hbsir, na.rm = TRUE))

panel_constant <- panel %>%
  mutate(month = as.integer(as.character(month))) %>%
  left_join(fueldep_constant, by = "province") %>%
  mutate(
    month                        = as.factor(month),
    subsidy_x_fueldep_constant   = bmgap2015adj * fueldep_constant,
    brent_x_fueldep_constant     = brent_shock * fueldep_constant,
    bmgap_c                      = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
    fueldep_constant_c           = fueldep_constant - mean(fueldep_constant, na.rm = TRUE),
    subsidy_x_fueldep_constant_c = bmgap_c * fueldep_constant_c,
    brent_x_fueldep_constant_c   = brent_shock * fueldep_constant_c
  )

second_stage_constant_h1 <- feols(
  onset_mm ~ unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_constant ~ brent_x_fueldep_constant,
  data    = panel_constant,
  cluster = ~province
)

second_stage_constant_h2 <- feols(
  onset_mm ~ fueldep_constant_c + bmgap_c + 
    unemployment_rate + inflation_rate + s_t | 
    province + month |
    subsidy_x_fueldep_constant_c ~ brent_x_fueldep_constant_c,
  data    = panel_constant,
  cluster = ~province
)

cat("\n=== C6: H1 TIME-CONSTANT FUEL DEPENDENCY (MM-only 2005-2020) ===\n")
print(summary(second_stage_constant_h1))
cat("\n=== C6: H2 TIME-CONSTANT FUEL DEPENDENCY (MM-only 2005-2020) ===\n")
print(summary(second_stage_constant_h2))

constant_results <- data.frame(
  Specification = c("H1 Time-Constant FuelDep", "",
                    "H2 Interaction Time-Constant FuelDep", ""),
  Estimate = c(
    round(coef(second_stage_constant_h1)["fit_subsidy_x_fueldep_constant"], 3),
    paste0("(", round(se(second_stage_constant_h1)["fit_subsidy_x_fueldep_constant"], 3), ")"),
    round(coef(second_stage_constant_h2)["fit_subsidy_x_fueldep_constant_c"], 3),
    paste0("(", round(se(second_stage_constant_h2)["fit_subsidy_x_fueldep_constant_c"], 3), ")")
  ),
  P_value = c("< 0.001", "", "0.939", "")
)

write_xlsx(
  list(
    C1_Terciles   = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C3_LOO"),
    C4_Subperiods = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C4_Subperiods"),
    C5_CNG        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C5_CNG"),
    C6_TimeConst  = constant_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C6 exportiert!\n")

# =============================================================================
# C7: Robustness Check — Probit (MM-only 2005-2020)
# =============================================================================

probit_h1 <- feglm(
  onset_mm ~ subsidy_x_fueldep_hbsir + 
    unemployment_rate + inflation_rate + s_t | 
    province + month,
  data    = panel,
  family  = binomial(link = "probit"),
  cluster = ~province
)

probit_h2 <- feglm(
  onset_mm ~ subsidy_x_fueldep_c + fueldep_c + bmgap_c +
    unemployment_rate + inflation_rate + s_t | 
    province + month,
  data    = panel,
  family  = binomial(link = "probit"),
  cluster = ~province
)

cat("\n=== C7: PROBIT H1 (MM-only 2005-2020) ===\n")
print(summary(probit_h1))
cat("\n=== C7: PROBIT H2 (MM-only 2005-2020) ===\n")
print(summary(probit_h2))

probit_results <- data.frame(
  Specification = c("Probit H1", "",
                    "Probit H2 Interaction", "",
                    "Probit H2 FuelDep", ""),
  Estimate = c(
    round(coef(probit_h1)["subsidy_x_fueldep_hbsir"], 3),
    paste0("(", round(se(probit_h1)["subsidy_x_fueldep_hbsir"], 3), ")"),
    round(coef(probit_h2)["subsidy_x_fueldep_c"], 3),
    paste0("(", round(se(probit_h2)["subsidy_x_fueldep_c"], 3), ")"),
    round(coef(probit_h2)["fueldep_c"], 3),
    paste0("(", round(se(probit_h2)["fueldep_c"], 3), ")")
  ),
  P_value = c("0.367", "", "0.632", "", "< 0.001", "")
)

write_xlsx(
  list(
    C1_Terciles   = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C3_LOO"),
    C4_Subperiods = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C4_Subperiods"),
    C5_CNG        = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C5_CNG"),
    C6_TimeConst  = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C6_TimeConst"),
    C7_Probit     = probit_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C7 exportiert!\n")

# =============================================================================
# C8: Urbanisierung — Korrelation mit Fuel Dependency
# =============================================================================

panel <- panel %>%
  mutate(urbanization_cluster = case_when(
    province %in% c("Tehran") ~ 1,
    province %in% c("Isfahan", "Razavi Khorasan", "Khuzestan",
                    "Qom", "Semnan", "Yazd", "Qazvin",
                    "East Azerbaijan", "Markazi") ~ 2,
    province %in% c("Ardabil", "West Azerbaijan", "Golestan",
                    "Zanjan", "Mazandaran", "Gilan", "Fars",
                    "Lorestan", "Ilam", "Kohgiluyeh and Boyer-Ahmad",
                    "Chaharmahal and Bakhtiari", "Hamedan",
                    "Kermanshah", "Kurdistan") ~ 3,
    province %in% c("Bushehr", "Hormozgan", "North Khorasan",
                    "South Khorasan", "Kerman",
                    "Sistan and Baluchestan") ~ 4
  ))

urban_cor <- cor(panel$urbanization_cluster,
                 panel$fuel_dependency_hbsir,
                 use = "complete.obs")

cat("\n=== C8: URBANISIERUNG ===\n")
cat("Korrelation FuelDep ~ Urbanisierung:", round(urban_cor, 3), "\n")

urban_province <- panel %>%
  group_by(province, urbanization_cluster) %>%
  summarise(mean_fueldep = mean(fuel_dependency_hbsir, na.rm = TRUE),
            .groups = "drop")

ggplot(urban_province,
       aes(x = urbanization_cluster,
           y = mean_fueldep,
           label = province)) +
  geom_point() +
  geom_text(hjust = -0.1, size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Urbanization Cluster (1=most urban, 4=most rural)",
       y = "Mean Fuel Dependency",
       title = "Fuel Dependency vs. Urbanization Level") +
  theme_minimal()

ggsave("~/Desktop/urbanization_fueldep_plot.png", width = 10, height = 6)

urban_results <- data.frame(
  Statistic = c("Correlation FuelDep ~ Urbanization Cluster",
                "N Provinces",
                "Source"),
  Value = c(round(urban_cor, 3), "30", "Enayatrad et al. (2019), HBSIR")
)

write_xlsx(
  list(
    C1_Terciles     = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C1_Terciles"),
    C2_CBI          = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C2_CBI"),
    C3_LOO          = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C3_LOO"),
    C4_Subperiods   = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C4_Subperiods"),
    C5_CNG          = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C5_CNG"),
    C6_TimeConst    = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C6_TimeConst"),
    C7_Probit       = read_xlsx("~/Desktop/robustness_checks_final.xlsx", sheet = "C7_Probit"),
    C8_Urbanization = urban_results
  ),
  "~/Desktop/robustness_checks_final.xlsx"
)
cat("C8 exportiert!\n")
cat("\n=== ALLE ROBUSTHEITSCHECKS ABGESCHLOSSEN ===\n")
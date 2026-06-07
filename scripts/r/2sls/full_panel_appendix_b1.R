> # =============================================================================
> # full_panel_appendix.R
  > # Full Panel Analysis (2005-2022) — Appendix B1
  > # Author: Rosalie Adams (s4835859)
  > # Note: Standalone script — loads all data independently
  > # =============================================================================
> 
  > library(tidyverse)
> library(fixest)
> library(zoo)
> library(writexl)
> 
  > # =============================================================================
> # DATEN LADEN
  > # =============================================================================
> 
  > brent <- read_csv("~/Desktop/Brent/DCOILBRENTEU (1).csv")
  Rows: 4694 Columns: 2                                                     
  ── Column specification ──────────────────────────────────────────────────
  Delimiter: ","
  dbl  (1): DCOILBRENTEU
  date (1): observation_date
  
  ℹ Use `spec()` to retrieve the full column specification for this data.
  ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
  > 
    > brent_monthly <- brent %>%
    +     rename(date = observation_date, 
                 +            brent_price = DCOILBRENTEU) %>%
    +     filter(!is.na(brent_price)) %>%
    +     mutate(date  = as.Date(date),
                 +            year  = as.integer(format(date, "%Y")),
                 +            month = as.integer(format(date, "%m"))) %>%
    +     group_by(year, month) %>%
    +     summarise(brent_price = mean(brent_price, na.rm = TRUE), 
                    +               .groups = "drop") %>%
    +     arrange(year, month) %>%
    +     mutate(
      +         brent_ma12  = rollmean(brent_price, k = 12, 
                                       +                                fill = NA, align = "right"),
      +         brent_shock = brent_price - lag(brent_ma12, 1)
      +     )
  > 
    > panel_full <- read_csv("~/Desktop/Panel/panel_2005_2022.csv") %>%
    +     mutate(
      +         onset = case_when(
        +             year <= 2020 ~ onset_mm,
        +             year >= 2021 ~ onset_acled
        +         ),
      +         month = as.integer(as.character(month))
      +     ) %>%
    +     left_join(brent_monthly %>% 
                      +                   select(year, month, brent_shock),
                    +               by = c("year", "month")) %>%
    +     mutate(
      +         brent_x_fueldep = brent_shock * fuel_dependency_hbsir,
      +         month = as.factor(month)
      +     )
  Rows: 6480 Columns: 14                                                    
  ── Column specification ──────────────────────────────────────────────────
  Delimiter: ","
  chr  (2): province, source
  dbl (12): year, month, n_events_mm, onset_mm, n_events_acled, onset_ac...
  
  ℹ Use `spec()` to retrieve the full column specification for this data.
  ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
  > 
    > cat("Panel rows:", nrow(panel_full), "\n")
  Panel rows: 6480 
  > cat("Years:", min(panel_full$year), "-", max(panel_full$year), "\n")
  Years: 2005 - 2022 
  > cat("Onset rate:", round(mean(panel_full$onset, na.rm = TRUE) * 100, 2), "%\n")
  Onset rate: 6.23 %
  > 
    > # =============================================================================
  > # 2SLS H1 — Full Panel ohne Controls
    > # =============================================================================
  > 
    > second_stage_full_h1 <- feols(
      +     onset ~ 1 | province + month |
        +         subsidy_x_fueldep_hbsir ~ brent_x_fueldep,
      +     data    = panel_full,
      +     cluster = ~province
      + )
  NOTE: 360 observations removed because of NA values (IV: 0/360).
  > 
    > cat("\n=== 2SLS H1 (Full Panel 2005-2022, ohne Controls) ===\n")
  
  === 2SLS H1 (Full Panel 2005-2022, ohne Controls) ===
    > print(summary(second_stage_full_h1))
  TSLS estimation
  |- D.V.   : onset
  |- Endo.  : subsidy_x_fueldep_hbsir
  |- Instr. : brent_x_fueldep
  |
    |=> Second Stage
  |   Dep. Var.: onset
  Observations: 6,120
  Fixed-effects: province: 30,  month: 12
  Standard-errors: Clustered (province) 
  Estimate Std. Error  t value   Pr(>|t|)    
  fit_subsidy_x_fueldep_hbsir -1.31524   0.114945 -11.4423 2.8531e-12 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  RMSE: 0.247401     Adj. R2: 0.051954
  Within R2: 0.009161
  F-test (1st stage), subsidy_x_fueldep_hbsir: stat = 1,113.256, p < 2.2e-16  , on 1 and 6,107 DoF.
  Wu-Hausman: stat =    55.693, p = 9.663e-14, on 1 and 6,077 DoF.
  > print(fitstat(second_stage_full_h1, "ivf"))
  F-test (1st stage), subsidy_x_fueldep_hbsir: stat = 1,113.3, p < 2.2e-16, on 1 and 6,107 DoF.
  > 
    > # =============================================================================
  > # 2SLS H2 — Full Panel ohne Controls
    > # =============================================================================
  > 
    > panel_full <- panel_full %>%
    +     mutate(
      +         month               = as.integer(as.character(month)),
      +         bmgap_c             = bmgap2015adj - mean(bmgap2015adj, na.rm = TRUE),
      +         fueldep_c           = fuel_dependency_hbsir - 
        +             mean(fuel_dependency_hbsir, na.rm = TRUE),
      +         subsidy_x_fueldep_c = bmgap_c * fueldep_c,
      +         brent_x_fueldep_c   = brent_shock * fueldep_c,
      +         month               = as.factor(month)
      +     )
  > 
    > second_stage_full_h2 <- feols(
      +     onset ~ fueldep_c + bmgap_c | province + month |
        +         subsidy_x_fueldep_c ~ brent_x_fueldep_c,
      +     data    = panel_full,
      +     cluster = ~province
      + )
  NOTE: 360 observations removed because of NA values (IV: 0/360).
  > 
    > cat("\n=== 2SLS H2 (Full Panel 2005-2022, ohne Controls) ===\n")
  
  === 2SLS H2 (Full Panel 2005-2022, ohne Controls) ===
    > print(summary(second_stage_full_h2))
  TSLS estimation
  |- D.V.   : onset
  |- Endo.  : subsidy_x_fueldep_c
  |- Instr. : brent_x_fueldep_c
  |
    |=> Second Stage
  |   Dep. Var.: onset
  Observations: 6,120
  Fixed-effects: province: 30,  month: 12
  Standard-errors: Clustered (province) 
  Estimate Std. Error  t value Pr(>|t|)    
  fit_subsidy_x_fueldep_c -0.919707   0.300476 -3.06084 0.004722 ** 
    fueldep_c                0.175233   0.104121  1.68299 0.103115    
  bmgap_c                 -0.028139   0.025810 -1.09023 0.284590    
  ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  RMSE: 0.242034     Adj. R2: 0.044846
  Within R2: 0.00206 
  F-test (1st stage), subsidy_x_fueldep_c: stat = 1,204.0599, p < 2.2e-16 , on 1 and 6,105 DoF.
  Wu-Hausman: stat =     6.5448, p = 0.010543, on 1 and 6,075 DoF.
  > print(fitstat(second_stage_full_h2, "ivf"))
  F-test (1st stage), subsidy_x_fueldep_c: stat = 1,204.1, p < 2.2e-16, on 1 and 6,105 DoF.
  > 
    > # =============================================================================
  > # TABELLE EXPORTIEREN — Appendix B1
    > # =============================================================================
  > 
    > b1_results <- data.frame(
      +     Variable = c(
        +         "SubsidyExposure (instrumented)", "",
        +         "SubsidyExposure × FuelDep (instrumented)", "",
        +         "FuelDep (centered)", "",
        +         "Subsidy (centered)", "",
        +         "Province FE", "Month FE",
        +         "First-stage F", "Wu-Hausman p",
        +         "Observations", "Adjusted R²"
        +     ),
      +     H1 = c(
        +         round(coef(second_stage_full_h1)["fit_subsidy_x_fueldep_hbsir"], 3),
        +         paste0("(", round(se(second_stage_full_h1)["fit_subsidy_x_fueldep_hbsir"], 3), ")"),
        +         "", "", "", "", "", "",
        +         "Yes", "Yes", "1,113", "< 0.001", "6,120", "0.052"
        +     ),
      +     H2 = c(
        +         "", "",
        +         round(coef(second_stage_full_h2)["fit_subsidy_x_fueldep_c"], 3),
        +         paste0("(", round(se(second_stage_full_h2)["fit_subsidy_x_fueldep_c"], 3), ")"),
        +         round(coef(second_stage_full_h2)["fueldep_c"], 3),
        +         paste0("(", round(se(second_stage_full_h2)["fueldep_c"], 3), ")"),
        +         round(coef(second_stage_full_h2)["bmgap_c"], 3),
        +         paste0("(", round(se(second_stage_full_h2)["bmgap_c"], 3), ")"),
        +         "Yes", "Yes", "1,204", "0.011", "6,120", "0.045"
        +     )
      + )
  > 
    > write_xlsx(
      +     list(B1_FullPanel = b1_results),
      +     "~/Desktop/full_panel_appendix_b1.xlsx"
      + )
  > 
    > cat("B1 exportiert!\n")
  B1 exportiert!
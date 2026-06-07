# Fuel Subsidies and Protest Onset in Iran: A Subnational Analysis, 2005–2022
**Author:** Rosalie Adams (s4835859)  
**Supervisor:** Dr. Babak RezaeeDaryakenari  
**Institution:** Leiden University, 2026

---

## Overview

This repository contains all replication materials for the master's thesis examining the subnational relationship between fuel subsidy cuts and protest onset in Iran. The primary analysis uses a province-month panel covering 30 Iranian provinces from January 2005 to December 2020, estimated via Two-Stage Least Squares (2SLS) with controls for unemployment, inflation, and sanctions exposure.

---

## How to Replicate

### Step 1 — Fuel Dependency Construction (Python)
```bash
python3 scripts/python/hbsir_fuel_dependency_v3.py   # gasoline only (primary)
python3 scripts/python/hbsir_fuel_dependency_v5.py   # gasoline + CNG (robustness)
python3 scripts/python/hbsir_unemployment.py          # provincial unemployment
```
Requires: `pip install hbsir pandas`

### Step 2 — Primary Analysis (R)
Run in this order:
```r
source("scripts/r/2sls/baseline_mm_controls.R")          # Tables 1–3 (primary results)
source("scripts/r/2sls/final_robustness_checks_mm.R")    # Tables C1–C8 (robustness)
source("scripts/r/2sls/full_panel_appendix_b1.R")        # Table B1 (full panel)
```
Requires: `tidyverse`, `fixest`, `zoo`, `readxl`, `writexl`, `ggplot2`

### Step 3 — Figures and Additional Checks
```r
source("scripts/r/final_subsidy_validation_plot.R")  # Figure 1
source("scripts/r/correlation_centering_check.R")    # Appendix A1
source("scripts/r/descriptive_statistics.R")         # Descriptive statistics
source("scripts/r/bmgap_validation.R")               # Subsidy gap validation
```

---

## Repository Structure


data/
raw/                          # Raw input data
processed/
fuel dependency/            # Provincial fuel dependency (HBSIR)
panel/                      # Province-month panels
protest/                    # Protest data and consistency checks
results/                    # Regression output tables
v1_original/              # Pre-revision results (old specification)
subsidy/                    # Subsidy gap construction
correlation_centering_check.csv
unemployment_provincial_2005_2020.csv
figures/
figure1_subsidy_validation.png
scripts/
python/
hbsir_fuel_dependency_v3.py   # Fuel dependency (gasoline only)
hbsir_fuel_dependency_v5.py   # Fuel dependency (gasoline + CNG)
hbsir_unemployment.py         # Unemployment construction
r/
2sls/
baseline_mm_controls.R         # PRIMARY baseline analysis
final_robustness_checks_mm.R   # Robustness checks C1–C8
full_panel_appendix_b1.R       # Appendix B1 full panel
panel/
panel_construction_2005_2022.R
panel_construction_invariant.R
bmgap_validation.R
correlation_centering_check.R
descriptive_statistics.R
final_subsidy_validation_plot.R
unemployment_construction.R
v1_original/                    # Pre-revision scripts (old specification)

---

## Key Variables

| Variable | Description | Source |
|---|---|---|
| `onset_mm` | Protest onset (MM Dataset, 2005–2020) | Clark & Regan (2016) |
| `bmgap2015adj` | Implicit subsidy gap (USD/liter, 2015 constant) | Ross et al. (2017); own extension |
| `fuel_dependency_hbsir` | Provincial gasoline share of transport expenditure | HBSIR (Statistical Center of Iran) |
| `brent_shock` | Brent price deviation from 12-month moving average | FRED |
| `s_t` | Sanctions exposure index (continuous) | Laudati & Pesaran (2023) |

---

## Notes

- **Primary analysis:** MM-only panel, 2005–2020 (N = 5,760)
- **Supplementary analysis:** Full panel 2005–2022 — Appendix B1
- **Instrument:** Brent shock × provincial fuel dependency
- Files in `v1_original/` reflect the pre-revision specification and are retained for transparency

---

## Citation

Adams, R. (2026). *Fuel subsidies and protest onset in Iran: A subnational analysis, 2005–2022* [Master's thesis]. Leiden University.

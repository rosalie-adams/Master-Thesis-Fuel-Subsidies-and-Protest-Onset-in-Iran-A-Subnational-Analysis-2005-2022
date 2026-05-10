library(tidyverse)

# Load data
protest <- read_csv("~/Desktop/Protest/mm_acled_consistency_panel.csv")
fuel    <- read_csv("~/Desktop/Fuel Dependency/fuel_dependency_hbsir_2005_2022.csv")
subsidy <- read_csv("~/Desktop/Subsidy/Gasoline Prices Master - subsidy_final.csv")

# Prepare subsidy: split date into year and month
subsidy <- subsidy %>%
  mutate(year  = as.integer(substr(date, 1, 4)),
         month = as.integer(substr(date, 6, 7))) %>%
  select(year, month, price_usd_2015, benchmark_2015_adj,
         bmgap2015adj, price_growth_3m)

# Prepare fuel: replace underscores with spaces, fix province name mismatches
fuel <- fuel %>%
  mutate(province = str_replace_all(province, "_", " "),
         province = recode(province,
                           "Hamadan"                    = "Hamedan",
                           "Kohgiluyeh and Boyer Ahmad" = "Kohgiluyeh and Boyer-Ahmad")) %>%
  select(province, year, fuel_dependency_hbsir)

# Build panel
panel <- protest %>%
  left_join(subsidy, by = c("year", "month")) %>%
  left_join(fuel,    by = c("province", "year")) %>%
  mutate(subsidy_x_fueldep_hbsir = bmgap2015adj * fuel_dependency_hbsir) %>%
  arrange(province, year, month)

# Save
write_csv(panel, "~/Desktop/panel_2005_2022.csv")
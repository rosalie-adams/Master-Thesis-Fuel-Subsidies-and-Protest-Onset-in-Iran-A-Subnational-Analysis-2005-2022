> # Korrelationstabelle für Annex
  > library(tidyverse)
> 
  > cors <- data.frame(
    +     Specification = c("Uncentered: Interaction × Subsidy",
                            +                       "Uncentered: Interaction × Fuel Dependency",
                            +                       "Centered: Interaction × Subsidy",
                            +                       "Centered: Interaction × Fuel Dependency"),
    +     r = c(
      +         cor(panel$subsidy_x_fueldep_hbsir, panel$bmgap2015adj, use = "complete.obs"),
      +         cor(panel$subsidy_x_fueldep_hbsir, panel$fuel_dependency_hbsir, use = "complete.obs"),
      +         cor(panel$subsidy_x_fueldep_c, panel$bmgap_c, use = "complete.obs"),
      +         cor(panel$subsidy_x_fueldep_c, panel$fueldep_c, use = "complete.obs")
      +     )
    + ) %>%
    +     mutate(r = round(r, 3))
  > 
    > print(cors)
  Specification      r
  1         Uncentered: Interaction × Subsidy  0.619
  2 Uncentered: Interaction × Fuel Dependency -0.445
  3           Centered: Interaction × Subsidy  0.068
  4   Centered: Interaction × Fuel Dependency  0.065
  > write_csv(cors, "~/Desktop/correlation_centering_check.csv")
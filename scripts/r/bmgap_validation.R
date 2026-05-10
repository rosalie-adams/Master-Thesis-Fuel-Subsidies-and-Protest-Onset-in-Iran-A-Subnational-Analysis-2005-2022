library(dplyr)

df_raw <- read.csv(file.choose())

df_raw$date <- as.Date(paste0(df_raw$date, "-01"))

df <- df_raw %>%
  select(date, price_usd_2015, benchmark_2015_adj,
         bmgap2015adj, bmgap_own, abs_diff) %>%
  mutate(
    period = ifelse(date < as.Date("2015-07-01"),
                    "Von Uexküll range (2005–2015)",
                    "Own extension (2015–2022)")
  )

cat("=== Von Uexküll range (2005-01 to 2015-06) ===\n")
vx <- df %>% filter(period == "Von Uexküll range (2005–2015)")
cat(sprintf("  Mean abs_diff  : %.4f USD/L\n", mean(vx$abs_diff, na.rm = TRUE)))
cat(sprintf("  Median         : %.4f USD/L\n", median(vx$abs_diff, na.rm = TRUE)))
cat(sprintf("  Max            : %.4f USD/L\n", max(vx$abs_diff, na.rm = TRUE)))
cat(sprintf("  %% within 0.05 : %.1f%%\n", 100 * mean(vx$abs_diff < 0.05, na.rm = TRUE)))
cat(sprintf("  %% within 0.10 : %.1f%%\n", 100 * mean(vx$abs_diff < 0.10, na.rm = TRUE)))

cat("\n=== Outliers > 0.15 USD/L (Von Uexküll range) ===\n")
df %>%
  filter(period == "Von Uexküll range (2005–2015)", abs_diff > 0.15) %>%
  mutate(date_label = format(date, "%Y-%m")) %>%
  select(date_label, price_usd_2015, benchmark_2015_adj, bmgap2015adj, bmgap_own, abs_diff) %>%
  print()
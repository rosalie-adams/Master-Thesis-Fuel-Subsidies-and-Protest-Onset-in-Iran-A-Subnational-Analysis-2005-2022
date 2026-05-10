# =============================================================
# unemployment_construction.R
# Construction of Provincial Unemployment Rate 2005-2020
# Author: Rosalie Adams (s4835859)
# Supervisor: Babak RezaeeDaryakenari
# Date: April 2026
#
# This script combines three datasets from the Statistical
# Center of Iran (via Iran Open Data Center) to construct
# a province-year panel of unemployment rates covering
# the full study period 2005-2020.
#
# Sources:
# Dataset 1: Iranian Labour Market Index (1384-1391)
#   Table 2, p. 92
# Dataset 2: Labour Force Survey 1394 (1392-1394)
#   Table 47, p. 380
# Dataset 3: Statistical Center of Iran (1395-1399)
#
# Output: unemployment_provincial_2005_2020.csv
#   30 provinces x 16 years = 480 observations
# =============================================================

library(readr)
library(dplyr)
library(tidyr)

# All three raw CSVs are stored directly on the Desktop
data_path <- '~/Desktop/'

# =============================================================
# STEP 1: Load and reshape Dataset 1 (1384-1391 / 2005-2012)
# =============================================================

unemp1 <- read_csv(paste0(data_path,
  'sciehe171-unemployment_rate-1385-1391en.CSV')) %>%
  filter(Provinces != 'Whole country') %>%
  rename(province_raw = Provinces) %>%
  pivot_longer(
    cols          = -province_raw,
    names_to      = 'persian_year',
    values_to     = 'unemployment_rate'
  ) %>%
  mutate(
    persian_year   = as.integer(gsub('[^0-9]', '', persian_year)),
    gregorian_year = persian_year + 621
  )

cat('Dataset 1 loaded:', nrow(unemp1), 'rows,',
    length(unique(unemp1$province_raw)), 'provinces,',
    'years:', paste(sort(unique(unemp1$gregorian_year)), collapse = ' '), '\n')

# =============================================================
# STEP 2: Load and reshape Dataset 2 (1392-1394 / 2013-2015)
#
# This dataset includes male, female, and combined columns.
# We keep only the combined (Male and Female) column.
# =============================================================

unemp2 <- read_csv(paste0(data_path,
  'sciehe1624-unemployment_rate-1392-1394en.CSV')) %>%
  rename(province_raw = `Gender and province`) %>%
  filter(trimws(province_raw) != 'Whole country') %>%
  select(
    province_raw,
    `1392 Unemployment rate – population aged 15 and over - Male and Female (Percentage)`,
    `1393 Unemployment rate - population aged 15 and over - Male and Female (Percentage)`,
    `1394 Unemployment rate - population aged 15 and over – Male and Female (Percentage)`
  ) %>%
  rename(
    '1392' = `1392 Unemployment rate – population aged 15 and over - Male and Female (Percentage)`,
    '1393' = `1393 Unemployment rate - population aged 15 and over - Male and Female (Percentage)`,
    '1394' = `1394 Unemployment rate - population aged 15 and over – Male and Female (Percentage)`
  ) %>%
  pivot_longer(
    cols          = -province_raw,
    names_to      = 'persian_year',
    values_to     = 'unemployment_rate'
  ) %>%
  mutate(
    persian_year   = as.integer(persian_year),
    gregorian_year = persian_year + 621
  )

cat('Dataset 2 loaded:', nrow(unemp2), 'rows,',
    length(unique(unemp2$province_raw)), 'provinces,',
    'years:', paste(sort(unique(unemp2$gregorian_year)), collapse = ' '), '\n')

# =============================================================
# STEP 3: Load and reshape Dataset 3 (1395-1399 / 2016-2020)
# =============================================================

unemp3 <- read_csv(paste0(data_path,
  'Unemployment rate by province (2015 to 2019).CSV')) %>%
  rename(province_raw = year) %>%
  filter(trimws(province_raw) != 'the whole country') %>%
  select(province_raw, `1395`, `1396`, `1397`, `1398*`, `1399*`) %>%
  rename('1398' = `1398*`, '1399' = `1399*`) %>%
  pivot_longer(
    cols          = -province_raw,
    names_to      = 'persian_year',
    values_to     = 'unemployment_rate'
  ) %>%
  mutate(
    persian_year   = as.integer(persian_year),
    gregorian_year = persian_year + 621
  )

cat('Dataset 3 loaded:', nrow(unemp3), 'rows,',
    length(unique(unemp3$province_raw)), 'provinces,',
    'years:', paste(sort(unique(unemp3$gregorian_year)), collapse = ' '), '\n')

# =============================================================
# STEP 4: Combine all three datasets
# =============================================================

unemp_all <- bind_rows(unemp1, unemp2, unemp3)

cat('\nCombined:', nrow(unemp_all), 'rows,',
    'years:', paste(sort(unique(unemp_all$gregorian_year)), collapse = ' '), '\n')

# =============================================================
# STEP 5: Standardise province names
#
# Province names differ across datasets and sources.
# All names are recoded to match the panel naming convention.
# =============================================================

unemp_all <- unemp_all %>%
  mutate(province = dplyr::recode(trimws(province_raw),
    'Azerbaijan  East'            = 'East Azerbaijan',
    'East Azarbaijan'             = 'East Azerbaijan',
    'Azerbaijan  West'            = 'West Azerbaijan',
    'Western Azerbaijan'          = 'West Azerbaijan',
    'Esfahan'                     = 'Isfahan',
    'Chahar Mahaal and Bakhtiari' = 'Chaharmahal and Bakhtiari',
    'Khorasan  South'             = 'South Khorasan',
    'southern Khorasan'           = 'South Khorasan',
    'Khorasan  Razavi'            = 'Razavi Khorasan',
    'Khorasan Razavi'             = 'Razavi Khorasan',
    'Khorasan  North'             = 'North Khorasan',
    'Hamadan'                     = 'Hamedan',
    'Kohgiluyeh and Boyer-Ahmad'  = 'Kohgiluyeh and Boyer-Ahmad',
    'Kohgiloyeh and Boyerahmad'   = 'Kohgiluyeh and Boyer-Ahmad',
    'Sistan and Baluchestan'      = 'Sistan and Baluchestan'
  ))

# =============================================================
# STEP 6: Merge Alborz into Tehran
#
# Alborz was separated from Tehran as a distinct province in
# 1390 (2011). For consistency with all other variables in
# the panel, Alborz is merged into Tehran throughout:
# - Where both exist: Tehran = mean(Tehran, Alborz)
# - Where only Tehran exists: Tehran value unchanged
# =============================================================

alborz <- unemp_all %>%
  filter(province == 'Alborz') %>%
  select(persian_year, gregorian_year, alborz_rate = unemployment_rate)

tehran <- unemp_all %>%
  filter(province == 'Tehran') %>%
  left_join(alborz, by = c('persian_year', 'gregorian_year')) %>%
  mutate(unemployment_rate = ifelse(!is.na(alborz_rate),
                                     (unemployment_rate + alborz_rate) / 2,
                                     unemployment_rate)) %>%
  select(-alborz_rate)

# Remove Tehran, Alborz, and national rows, then add merged Tehran back
unemp_final <- unemp_all %>%
  filter(!province %in% c('Tehran', 'Alborz', 'Whole country', 'Whole country ')) %>%
  bind_rows(tehran) %>%
  select(province, gregorian_year, unemployment_rate) %>%
  arrange(province, gregorian_year)

# =============================================================
# STEP 7: Validate
# =============================================================

cat('\n=== VALIDATION ===\n')
cat('Final rows:', nrow(unemp_final), '(expected: 480 = 30 x 16)\n')
cat('Provinces:', length(unique(unemp_final$province)), '\n')
cat('Years:', paste(sort(unique(unemp_final$gregorian_year)), collapse = ' '), '\n')
cat('Min unemployment rate:', round(min(unemp_final$unemployment_rate, na.rm = TRUE), 2), '\n')
cat('Max unemployment rate:', round(max(unemp_final$unemployment_rate, na.rm = TRUE), 2), '\n')
cat('Mean unemployment rate:', round(mean(unemp_final$unemployment_rate, na.rm = TRUE), 2), '\n')
cat('NA values:', sum(is.na(unemp_final$unemployment_rate)), '\n')
cat('\nProvince list:\n')
print(sort(unique(unemp_final$province)))

# =============================================================
# STEP 8: Save
# =============================================================

write_csv(unemp_final, '~/Desktop/unemployment_provincial_2005_2020.csv')
cat('\nSaved: unemployment_provincial_2005_2020.csv\n')

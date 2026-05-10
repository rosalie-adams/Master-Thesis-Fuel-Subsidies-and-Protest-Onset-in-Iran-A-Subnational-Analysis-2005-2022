# hbsir_unemployment.py
# Construction of provincial unemployment rate from HBSIR microdata
# Source: Household Budget Survey of Iran (HBSIR)
# Years: 1384-1399 (2005-2020)
# Author: Rosalie Adams (s4835859)

import hbsir
import pandas as pd

years = list(range(1384, 1400))

# Load employment_income table with province attribute
print("Loading employment_income...")
df = hbsir.load_table("employment_income", years=years)
df = hbsir.add_attribute(df, name="Province")
print("Loaded:", df.shape)

# Keep only employed and unemployed (drop NaN)
df = df[df['Employment_Status'].isin(['Employed', 'Unemployed'])]

# Count employed and unemployed per province and year
counts = df.groupby(['Year', 'Province', 'Employment_Status']).size().unstack(fill_value=0).reset_index()

# Calculate unemployment rate
counts['unemployment_rate'] = counts['Unemployed'] / (counts['Employed'] + counts['Unemployed']) * 100

# Add gregorian year
counts['gregorian_year'] = counts['Year'] + 621

# Clean province names
counts['Province'] = counts['Province'].str.replace('_', ' ')

# Keep relevant columns
result = counts[['Province', 'Year', 'gregorian_year', 'unemployment_rate']]

print("\nSample output:")
print(result.head(10))
print("\nShape:", result.shape)
print("\nYear range:", result['gregorian_year'].min(), "-", result['gregorian_year'].max())
print("Provinces:", result['Province'].nunique())

# Save
result.to_csv('/Users/roseadams/Desktop/unemployment_hbsir.csv', index=False)
print("\nSaved!")

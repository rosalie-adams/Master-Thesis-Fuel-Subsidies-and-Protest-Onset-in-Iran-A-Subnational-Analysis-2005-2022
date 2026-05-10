"""
hbsir_fuel_dependency_v5.py
============================
Computes gasoline dependency per province and year from HBSIR household survey.

Definition:
    Share of gasoline expenditure (code 72211) in total transport expenditure,
    averaged across households at province level.

Features:
    - Saves after every year — safe to interrupt and restart
    - Skips already completed years on restart
    - Merges Alborz into Tehran throughout

Output:
    fuel_dependency_hbsir_2005_2022.csv

Usage:
    python3 hbsir_fuel_dependency_v5.py
"""

import hbsir
import pandas as pd
from pathlib import Path

# ── Configuration ──────────────────────────────────────────────────────────────

PERSIAN_YEARS = list(range(1384, 1402))
YEAR_MAP = {py: py - 1384 + 2005 for py in PERSIAN_YEARS}

FUEL_CODES = [72211, 72213]  # gasoline + CNG
PROVISION_METHOD = "Purchase"
PROVINCE_MERGE = {"Alborz": "Tehran"}

OUTPUT_FILE = "fuel_dependency_hbsir_benzin_cng_2005_2022.csv"


# ── Load already completed years ───────────────────────────────────────────────

def load_existing() -> pd.DataFrame:
    if Path(OUTPUT_FILE).exists():
        df = pd.read_csv(OUTPUT_FILE)
        print(f"Resuming — already completed: {sorted(df['persian_year'].unique())}")
        return df
    return pd.DataFrame()


# ── Per-year computation ───────────────────────────────────────────────────────

def compute_year(persian_year: int) -> pd.DataFrame:
    gregorian_year = YEAR_MAP[persian_year]
    print(f"Loading {persian_year} (= {gregorian_year})...", flush=True)

    table = hbsir.load_table("Expenditures", years=[persian_year])
    table = hbsir.add_attribute(table, name="Province")

    transport = table[
        (table["Commodity_Code"] >= 72000) &
        (table["Commodity_Code"] < 74000) &
        (table["Provision_Method"] == PROVISION_METHOD)
    ].copy()

    if transport.empty:
        print(f"  WARNING: No transport data for {persian_year}")
        return pd.DataFrame()

    transport["Province"] = transport["Province"].replace(PROVINCE_MERGE)
    transport["is_fuel"] = transport["Commodity_Code"].isin(FUEL_CODES)

    hh = (
        transport
        .groupby(["ID", "Province"])
        .apply(lambda x: pd.Series({
            "fuel_exp":      x.loc[x["is_fuel"], "Gross_Expenditure"].sum(),
            "transport_exp": x["Gross_Expenditure"].sum()
        }))
        .reset_index()
    )

    hh = hh[hh["transport_exp"] > 0].copy()
    if hh.empty:
        print(f"  WARNING: No valid households for {persian_year}")
        return pd.DataFrame()

    hh["fuel_share"] = hh["fuel_exp"] / hh["transport_exp"]

    prov = (
        hh.groupby("Province")["fuel_share"]
        .mean()
        .reset_index()
        .rename(columns={"Province": "province", "fuel_share": "fuel_dependency_hbsir"})
    )

    prov["year"] = gregorian_year
    prov["persian_year"] = persian_year

    print(f"  -> {len(prov)} provinces, mean = {prov['fuel_dependency_hbsir'].mean():.3f}")
    return prov


# ── Main ───────────────────────────────────────────────────────────────────────

def main():
    existing = load_existing()
    done_years = set(existing["persian_year"].tolist()) if not existing.empty else set()

    results = [existing] if not existing.empty else []

    for persian_year in PERSIAN_YEARS:
        if persian_year in done_years:
            print(f"Skipping {persian_year} (already done)")
            continue

        try:
            df = compute_year(persian_year)
            if not df.empty:
                results.append(df)
                # Save after every year
                panel = pd.concat(results, ignore_index=True)
                panel = panel[["province", "year", "persian_year", "fuel_dependency_hbsir"]]
                panel = panel.sort_values(["province", "year"]).reset_index(drop=True)
                panel.to_csv(OUTPUT_FILE, index=False)
                print(f"  Saved to {OUTPUT_FILE}")
        except Exception as e:
            print(f"  ERROR for {persian_year}: {e}")
            continue

    print("\nDone.")
    print(f"Output: {OUTPUT_FILE}")


if __name__ == "__main__":
    main()

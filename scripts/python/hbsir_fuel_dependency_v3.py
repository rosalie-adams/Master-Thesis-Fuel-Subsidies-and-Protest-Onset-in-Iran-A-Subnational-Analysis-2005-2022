"""
hbsir_fuel_dependency_v3.py
============================
Berechnet Fuel Dependency pro Provinz und Jahr aus HBSIR-Haushaltsdaten.

Definition Fuel Dependency:
    Anteil der Kraftstoffausgaben (Benzin, Diesel, Gas, Schmierstoffe)
    an den gesamten Transportausgaben eines Haushalts,
    aggregiert auf Provinzebene als gewichteter Durchschnitt.

Output:
    fuel_dependency_hbsir_2005_2022.csv
    Spalten: province, year, fuel_dependency_hbsir

Ausführung:
    python hbsir_fuel_dependency_v3.py

Voraussetzungen:
    pip install hbsir pandas
"""

import hbsir
import pandas as pd
from pathlib import Path

# ── Konfiguration ──────────────────────────────────────────────────────────────

# Persische Jahre 1384–1401 = Gregorianisch 2005–2022
PERSIAN_YEARS = list(range(1384, 1402))

# Kraftstoff-Codes (aus HBSIR Commodity Classification, Kategorie 0722):
#   72211 = Benzin (Hauptspezifikation)
#
# Ausgeschlossen:
#   72212 = Diesel/Gasoil (fast ausschließlich Schwerlast/Busse, keine PKW)
#   72213 = CNG/Gas — obwohl verbreitet, unterliegt CNG einem separaten
#            Subventionssystem und ist von Benzinpreisschocks entkoppelt.
#            Da die unabhängige Variable der Benzinpreis ist, soll Fuel
#            Dependency nur Benzin-Exposition messen.
#   72214 = Motoröl, Bremsflüssigkeit etc. (keine Kraftstoffe)
#
# Robustness Check: FUEL_CODES = [72211, 72213] (Benzin + CNG)
FUEL_CODES = [72211]

# Alle Transport-Codes (Kategorie 07 = Transport):
#   072xx = Betrieb privater Fahrzeuge (inkl. Kraftstoffe, Reparatur)
#   073xx = Personentransportdienstleistungen
# Fuel Dependency = Kraftstoff / gesamte Transportausgaben
# Wir verwenden nur "Purchase" als Provision_Method (keine Sachleistungen etc.)
PROVISION_METHOD = "Purchase"

OUTPUT_FILE = "fuel_dependency_hbsir_2005_2022.csv"

# ── Mapping: Persisches Jahr → Gregorianisches Jahr ───────────────────────────

def persian_to_gregorian(persian_year: int) -> int:
    """Konvertiert persisches Jahr in gregorianisches Jahr (Näherung: +621)."""
    return persian_year - 621


# ── Alborz-Merge: Alborz → Tehran ────────────────────────────────────────────
# Alborz wurde 2011 (1390) als eigene Provinz aus Tehran herausgelöst.
# Für Konsistenz mit MM-Daten mergen wir Alborz zu Tehran.

ALBORZ_MERGE = {
    "Alborz": "Tehran"
}


# ── Hauptfunktion ─────────────────────────────────────────────────────────────

def compute_fuel_dependency_year(persian_year: int) -> pd.DataFrame:
    """
    Lädt Transportausgaben für ein Jahr, fügt Provinz hinzu,
    und berechnet Fuel Dependency pro Provinz.

    Returns:
        DataFrame mit Spalten: province, fuel_dependency_hbsir
    """
    print(f"  Lade Jahr {persian_year}...")

    # Transportausgaben laden (Kategorie 07)
    table = hbsir.load_table("Expenditures", years=[persian_year])
    table = hbsir.add_attribute(table, name="Province")

    # Nur Transport-Codes behalten (72xxx und 73xxx)
    transport_mask = (
        (table["Commodity_Code"] >= 72000) &
        (table["Commodity_Code"] < 74000)
    )
    transport = table[transport_mask].copy()

    # Nur käuflich erworbene Güter (keine Sachleistungen, Eigenproduktion etc.)
    transport = transport[transport["Provision_Method"] == PROVISION_METHOD]

    if transport.empty:
        print(f"  WARNUNG: Keine Daten für Jahr {persian_year}")
        return pd.DataFrame(columns=["province", "fuel_dependency_hbsir"])

    # Alborz → Tehran mergen
    transport["Province"] = transport["Province"].replace(ALBORZ_MERGE)

    # Kraftstoff-Flag
    transport["is_fuel"] = transport["Commodity_Code"].isin(FUEL_CODES)

    # Aggregation auf Haushaltsebene: Summe Kraftstoff + Summe Transport
    hh_agg = (
        transport
        .groupby(["ID", "Province"])
        .apply(lambda x: pd.Series({
            "fuel_exp":      x.loc[x["is_fuel"],  "Expenditure"].sum(),
            "transport_exp": x["Expenditure"].sum()
        }))
        .reset_index()
    )

    # Nur Haushalte mit positiven Transportausgaben
    hh_agg = hh_agg[hh_agg["transport_exp"] > 0].copy()

    if hh_agg.empty:
        print(f"  WARNUNG: Keine gültigen Haushalte für Jahr {persian_year}")
        return pd.DataFrame(columns=["province", "fuel_dependency_hbsir"])

    # Fuel Dependency auf Haushaltsebene
    hh_agg["fuel_share"] = hh_agg["fuel_exp"] / hh_agg["transport_exp"]

    # Provinzdurchschnitt (ungewichtet; für gewichteten Durchschnitt
    # müsste man Sampling Weights hinzuziehen — hier einfacher Mittelwert)
    prov_agg = (
        hh_agg
        .groupby("Province")["fuel_share"]
        .mean()
        .reset_index()
        .rename(columns={"Province": "province", "fuel_share": "fuel_dependency_hbsir"})
    )

    print(f"  → {len(prov_agg)} Provinzen, Fuel Dep. Ø = {prov_agg['fuel_dependency_hbsir'].mean():.3f}")
    return prov_agg


def main():
    results = []

    for persian_year in PERSIAN_YEARS:
        gregorian_year = persian_to_gregorian(persian_year)
        print(f"\nJahr {persian_year} (= {gregorian_year}):")

        try:
            df_year = compute_fuel_dependency_year(persian_year)
            if not df_year.empty:
                df_year["year"] = gregorian_year
                df_year["persian_year"] = persian_year
                results.append(df_year)
        except Exception as e:
            print(f"  FEHLER für Jahr {persian_year}: {e}")
            continue

    if not results:
        print("\nFEHLER: Keine Daten geladen.")
        return

    # Zusammenführen
    panel = pd.concat(results, ignore_index=True)

    # Spaltenreihenfolge
    panel = panel[["province", "year", "persian_year", "fuel_dependency_hbsir"]]

    # Sortieren
    panel = panel.sort_values(["province", "year"]).reset_index(drop=True)

    # Überblick
    print("\n─── Zusammenfassung ───────────────────────────────────────────")
    print(f"Provinzen: {panel['province'].nunique()}")
    print(f"Jahre (gregorianisch): {sorted(panel['year'].unique())}")
    print(f"Gesamt Zeilen: {len(panel)}")
    print(f"\nFuel Dependency Statistik:")
    print(panel["fuel_dependency_hbsir"].describe().round(4))
    print()
    print("Provinzen:")
    print(sorted(panel["province"].unique()))

    # Speichern
    panel.to_csv(OUTPUT_FILE, index=False)
    print(f"\n✓ Gespeichert: {OUTPUT_FILE}")


if __name__ == "__main__":
    main()

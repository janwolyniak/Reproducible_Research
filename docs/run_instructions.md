# Run Instructions

These commands define the current Python entry points for the reproduction.
Later phases will add the cross-sectional and panel model outputs behind the
same `scripts/run_all.py` interface.

## Local Environment

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

## Validate Inputs

```bash
python scripts/validate_project.py
```

The validation command checks the tracked paper, R workflows, processed input
data, reference R outputs, helper scripts, and expected output directories.

## Run Current Pipeline

```bash
python scripts/run_all.py
```

At the Phase 1 baseline, this command creates expected output directories,
validates the project layout, regenerates `docs/phase0_audit.md`, refreshes the
Phase 2 shared data inventory in `docs/data_inventory.csv` and
`docs/data_dictionary.md`, and writes a run log to
`outputs/logs/run_all_summary.txt`.

## Inventory Source Data

```bash
python scripts/inventory_data.py
```

This command reads every tracked `.rds` and `.dta` source registered in
`src/repro_research/paths.py`, records row counts, column counts, detected
country/year identifier columns, missing-cell counts, column names, and dtypes,
then refreshes the shared Phase 2 data dictionary scaffold.

## RDS Conversion Helper

The existing helper remains available:

```bash
python helpers/rds_to_csv.py cross-section/data --output outputs/intermediate/cross_section_csv
python helpers/rds_to_csv.py panel/data_panel --output outputs/intermediate/panel_csv
```

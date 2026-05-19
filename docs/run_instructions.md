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
validates the project layout, regenerates `docs/phase0_audit.md`, and writes a
run log to `outputs/logs/run_all_summary.txt`.

## RDS Conversion Helper

The existing helper remains available:

```bash
python helpers/rds_to_csv.py cross-section/data --output outputs/intermediate/cross_section_csv
python helpers/rds_to_csv.py panel/data_panel --output outputs/intermediate/panel_csv
```

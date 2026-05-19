# Reproducible_Research
project for the Reproducible Research classes, Quantitative Finance

Authors:
- [Jan Wołyniak](https://github.com/janwolyniak)
- [Kinga Kucharska](https://github.com/kinga-kucharska)
- [Iwo Wiszejko](https://github.com/IWiszejko)

## Current Python Baseline

Install the constrained dependencies:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

Validate the tracked inputs and expected output directories:

```bash
python scripts/validate_project.py
```

Run the current reproducibility pipeline:

```bash
python scripts/run_all.py
```

At the Phase 1 baseline, `run_all.py` validates the repository, regenerates the
Phase 0 audit, regenerates the Phase 2 shared data inventory, and writes a
local run log. Detailed commands are in `docs/run_instructions.md`.

Regenerate only the data inventory and shared data dictionary:

```bash
python scripts/inventory_data.py
```

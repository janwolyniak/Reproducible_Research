# Reproducible Research: Constitutional Compliance and Growth

This project reproduces the empirical workflows from Kinga Kucharska's thesis,
`2400-LIC-FII.pdf`, in Python. It rebuilds the original R cross-sectional and
panel analyses, regenerates the tables and figures, and renders HTML reports
from a public Docker image.

Authors:

- [Jan Wolyniak](https://github.com/janwolyniak)
- [Kinga Kucharska](https://github.com/kinga-kucharska)
- [Iwo Wiszejko](https://github.com/IWiszejko)

## Docker Reproduction

The public Docker image is:

```text
janwolyniak/reproducible-research-lic-fii
```

Run the complete analysis and report generation:

```bash
mkdir -p rr-final-demo/outputs/report
cd rr-final-demo
docker pull janwolyniak/reproducible-research-lic-fii
docker run --rm \
  -v "$(pwd)/outputs/report:/app/outputs/report" \
  janwolyniak/reproducible-research-lic-fii
```

PowerShell uses backticks for line continuation:

```powershell
mkdir rr-final-demo
mkdir rr-final-demo\outputs
mkdir rr-final-demo\outputs\report
cd rr-final-demo
docker pull janwolyniak/reproducible-research-lic-fii
docker run --rm `
  -v "${PWD}/outputs/report:/app/outputs/report" `
  janwolyniak/reproducible-research-lic-fii
```

The single deliverable lands in `rr-final-demo/outputs/report/`:

| File | Purpose |
| --- | --- |
| `report.html` | The presentation report: full study walkthrough, R-vs-Python comparison, figures, and interpretation. |

On macOS:

```bash
open outputs/report/report.html
```

## What the Container Runs

The Docker default command is:

```bash
python scripts/generate_report.py
```

It runs:

1. source and output-directory validation,
2. cross-sectional data preparation and OLS reproduction,
3. panel data preparation, estimators, specification tests, and diagnostics,
4. generated-output validation,
5. Quarto rendering of the presentation report to `report.html`.

## Research Design

The reproduction covers two empirical designs:

- **Cross-section:** 2010-2019 country averages and OLS specifications.
- **Panel:** 1990-2020 country-year data with pooled, between,
  first-difference, fixed-effects, and random-effects models.

The R source of truth is:

- `cross-section/inorder.R`
- `panel/inorder_panel.R`
- `cross-section/tables_and_other/`
- `panel/panel_tables/`

## Repository Structure

| Path | Purpose |
| --- | --- |
| `src/repro_research/` | Python package for data loading, transformations, models, diagnostics, validation, and orchestration. |
| `scripts/` | Command-line entry points for validation, pipeline runs, and report generation. |
| `report/` | Quarto source (`report.qmd`) for the presentation report. |
| `cross-section/` | Original cross-sectional R workflow, source data, and reference outputs. |
| `panel/` | Original panel R workflow, source data, and reference outputs. |
| `docs/` | Final methodology, limitations, contribution, AI-use, and reproduction-contract documents. |
| `outputs/` | Regenerated Python tables, figures, logs, and report output directory. |

## Local Run

Docker is the intended reproducibility path. For local development:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
python -m pip install --no-deps -e .
```

Run the Python pipeline:

```bash
python scripts/run_all.py
python scripts/validate_project.py --generated-outputs
```

Render reports locally:

```bash
python scripts/generate_report.py
```

Local report rendering requires Quarto on `PATH`.

## Documentation

| Document | Purpose |
| --- | --- |
| `docs/reproduction_contract.md` | Source data, model targets, artifact map, and tolerance policy. |
| `docs/methodology.md` | How the Python reproduction is implemented. |
| `docs/limitations.md` | Known numerical and implementation differences from the R workflow. |
| `docs/ai_disclosure.md` | AI-use disclosure. |
| `docs/contributions.md` | Contributor roles and Git evidence. |

## Docker Image Maintenance

Build and publish the image:

```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t janwolyniak/reproducible-research-lic-fii \
  --push .
```

The image should remain public and should not include local secrets. The
`.dockerignore` excludes local environments, git metadata, caches, generated
report files, `.env` files, key files, token files, and secret folders.

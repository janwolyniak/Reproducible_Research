# Run Instructions

These commands define the Python entry points for the reproduction. The full
pipeline rebuilds the data inventory, cross-sectional outputs, panel outputs,
and generated reproduction notes behind the same `scripts/run_all.py`
interface.

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

## Run Full Pipeline

```bash
python scripts/run_all.py
```

This command creates expected output directories, validates the project layout,
regenerates `docs/phase0_audit.md`, refreshes the Phase 2 data inventory,
rebuilds the cross-sectional and panel tables/figures, regenerates the
component reproduction notes, writes `outputs/logs/run_all_summary.txt`, and
checks that the expected regenerated output files exist.

For faster local checks that only need tables and Markdown notes:

```bash
python scripts/run_all.py --skip-plots
```

`--skip-plots` skips the cross-sectional and panel PNG refreshes and therefore
does not run the complete generated-output manifest check by default. Add
`--strict-validation` when you intentionally want that full manifest check after
a plot-skipping run.

## Generate the Presentation Report

The reviewer-facing report and slide deck are built with Quarto from
`report/report.qmd` and `report/slides.qmd`. The orchestrator runs the full
pipeline first, then renders both outputs to `outputs/report/`:

```bash
python scripts/generate_report.py
```

Useful flags:

- `--skip-pipeline` — re-render the report without rerunning the analysis.
- `--report-only` / `--slides-only` — render just one of the two targets.
- `--skip-plots` / `--strict-validation` — forwarded to the pipeline step.

Quarto must be available on `PATH`. The Docker image ships Quarto, so the
zero-setup path is to run this script inside the container (see below).

## Component Output Directories

The cross-sectional and panel entry points can write to alternate review
directories without disturbing the default `outputs/` tree:

```bash
python scripts/run_cross_section.py --output-dir outputs/review/cross_section --docs-dir outputs/review/docs --skip-plots
python scripts/run_panel.py --output-dir outputs/review/panel --docs-dir outputs/review/docs --skip-plots
```

Both commands keep the default behavior when the flags are omitted. The
`--skip-plots` option is useful for quick coefficient-table or documentation
checks; omit it for submission artifacts.

## Validate Regenerated Outputs

```bash
python scripts/validate_project.py --generated-outputs
```

Run this after `python scripts/run_all.py` when checking a fresh clone or a
submission bundle. It verifies the tracked source inputs, the Phase 5
comparison documents, and the complete regenerated output set listed in
`outputs/README.md`.

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

## Docker Workflow

Build the reproducibility image:

```bash
docker build -t janwolyniak/reproducible-research-lic-fii:phase6 .
```

Run the full pipeline **and** render the presentation report with the image
default command:

```bash
docker run --rm -v "$(pwd)/outputs:/app/outputs" \
  janwolyniak/reproducible-research-lic-fii:phase6
```

The Dockerfile default command is `python scripts/generate_report.py`. It runs
the full analysis, validates the manifest, and writes the report and slides
to `/app/outputs/report/` — bind-mounted back to `./outputs/report/` on the
host.

Use Docker Compose when you want every regenerated file (not just the report)
written back to the checkout:

```bash
docker compose run --rm reproduction
```

The compose service bind-mounts the repository to `/app`, so final outputs land
in `docs/`, `outputs/cross_section/`, `outputs/panel/`, and
`outputs/report/`. Scratch CSVs and run logs are regenerated under the ignored
`outputs/intermediate/` and `outputs/logs/` directories.

The Docker Hub target for the final image is:

```text
janwolyniak/reproducible-research-lic-fii:phase6
```

After Jan pushes that tag, reviewers can skip the local build:

```bash
docker pull janwolyniak/reproducible-research-lic-fii:phase6
docker run --rm -v "$(pwd)/outputs:/app/outputs" \
  janwolyniak/reproducible-research-lic-fii:phase6
```

The pulled image runs the analysis and renders the report end-to-end. The two
artefacts to open after the run are:

- `outputs/report/report.html` — full reviewer report.
- `outputs/report/slides.html` — Reveal.js deck used in the presentation.

Troubleshooting:

- Start Docker Desktop or the Docker daemon if `docker build` cannot connect.
- Rebuild with `docker build --no-cache -t janwolyniak/reproducible-research-lic-fii:phase6 .`
  if a cached dependency layer is stale.
- On Linux, a compose run may leave host-mounted output files owned by root.
  Use the plain `docker run --rm ...` command when host ownership matters, or
  adjust ownership after the run.

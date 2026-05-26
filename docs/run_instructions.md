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

The reviewer-facing artifacts are built by `scripts/generate_report.py`. The
orchestrator runs the full pipeline first, validates the generated outputs,
renders the Quarto report/decks, then executes and exports
`notebooks/final_presentation_report.ipynb` to `outputs/report/`:

```bash
python scripts/generate_report.py
```

Useful flags:

- `--skip-pipeline` — re-render the report without rerunning the analysis.
- `--report-only` / `--slides-only` — render just one of the two targets.
- `--skip-notebook` — skip the executable notebook export.
- `--skip-plots` / `--strict-validation` — forwarded to the pipeline step.

Quarto must be available on `PATH` for the Quarto artifacts. The notebook
export uses the Jupyter dependencies in `requirements.txt`. The Docker image
ships both Quarto and the notebook dependencies, so the zero-setup path is to
run this script inside the container (see below).

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

The final presentation path is pull-run-open. The instructor pulls the public
Docker Hub image directly and does not build locally:

```bash
docker pull janwolyniak/reproducible-research-lic-fii
mkdir -p outputs/report
docker run --rm -v "$(pwd)/outputs/report:/app/outputs/report" \
  janwolyniak/reproducible-research-lic-fii
```

PowerShell:

```powershell
docker pull janwolyniak/reproducible-research-lic-fii
mkdir outputs
mkdir outputs/report
docker run --rm `
  -v "${PWD}/outputs/report:/app/outputs/report" `
  janwolyniak/reproducible-research-lic-fii
```

The Dockerfile default command is `python scripts/generate_report.py`. It runs
the full analysis, validates the manifest, and writes the report artifacts
to `/app/outputs/report/`, which is bind-mounted back to
`./outputs/report/` on the host. The intermediate regenerated analysis outputs
remain inside the container unless you use the compose workflow below.

Open these files after the run:

- `outputs/report/final_presentation_report.html` — main step-by-step notebook
  report for the presentation.
- `outputs/report/slides.html` — short Reveal.js deck.
- `outputs/report/report.html` — full reviewer report.
- `outputs/report/slides_visual.html` — visual backup deck.

## Docker Image Maintenance

Build and push the public reproducibility image:

```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t janwolyniak/reproducible-research-lic-fii \
  --push .
```

The image must remain public on Docker Hub and must not contain secrets.
This project needs no API keys at runtime. `.dockerignore` excludes local
virtual environments, git metadata, caches, logs, generated report outputs,
`.env` files, key files, token files, and secret folders.

Quick image smoke test after pushing:

```bash
docker pull janwolyniak/reproducible-research-lic-fii
docker run --rm janwolyniak/reproducible-research-lic-fii
```

Use Docker Compose when you want every regenerated file (not just the report)
written back to the checkout:

```bash
docker compose run --rm reproduction
```

The compose service bind-mounts the repository to `/app`, so final outputs land
in `docs/`, `outputs/cross_section/`, `outputs/panel/`, and
`outputs/report/`. Scratch CSVs and run logs are regenerated under the ignored
`outputs/intermediate/` and `outputs/logs/` directories.

Troubleshooting:

- Start Docker Desktop or the Docker daemon if `docker build` cannot connect.
- Rebuild with `docker build --no-cache -t janwolyniak/reproducible-research-lic-fii .`
  if a cached dependency layer is stale.
- On Linux, a compose run may leave host-mounted output files owned by root.
  Use the plain `docker run --rm ...` command when host ownership matters, or
  adjust ownership after the run.

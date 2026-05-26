# Reproducible Research: Constitutional Compliance and Growth

Python reproduction of Kinga Kucharska's thesis,
`2400-LIC-FII.pdf`, on constitutional compliance and economic growth.
The project rebuilds the original R cross-sectional and panel workflows,
regenerates the empirical outputs, and packages the final report workflow in a
public Docker image.

Authors:

- [Jan Wolyniak](https://github.com/janwolyniak)
- [Kinga Kucharska](https://github.com/kinga-kucharska)
- [Iwo Wiszejko](https://github.com/IWiszejko)

## Docker Reproduction

The instructor should **pull the public image from Docker Hub and run it**.
Nothing needs to be built locally on the instructor's laptop.

Public image:

```text
janwolyniak/reproducible-research-lic-fii
```

### 1. Create an empty run directory

```bash
mkdir rr-final-demo
cd rr-final-demo
mkdir -p outputs/report
```

### 2. Pull the public Docker image

```bash
docker pull janwolyniak/reproducible-research-lic-fii
```

### 3. Run the full analysis and report generation

macOS / Linux / Git Bash:

```bash
docker run --rm \
  -v "$(pwd)/outputs/report:/app/outputs/report" \
  janwolyniak/reproducible-research-lic-fii
```

PowerShell:

```powershell
docker run --rm `
  -v "${PWD}/outputs/report:/app/outputs/report" `
  janwolyniak/reproducible-research-lic-fii
```

The container default command is:

```bash
python scripts/generate_report.py
```

It performs the complete reviewer workflow:

1. validates the tracked source inputs and project layout,
2. runs the cross-sectional data preparation and OLS reproduction,
3. runs the panel data preparation, model families, tests, and diagnostics,
4. validates the regenerated output manifest,
5. renders the Quarto report and Reveal.js decks,
6. executes `notebooks/final_presentation_report.ipynb` and exports it as HTML.

### 4. Open the generated presentation artifacts

After the container exits successfully, open:

| File | Purpose |
| --- | --- |
| `outputs/report/final_presentation_report.html` | Main step-by-step notebook report for the presentation. |
| `outputs/report/slides.html` | Short technical Reveal.js slide deck. |
| `outputs/report/report.html` | Full reviewer report with all tables and figures. |
| `outputs/report/slides_visual.html` | Visual backup deck. |

For the 20-minute presentation, use
`outputs/report/final_presentation_report.html` as the main walkthrough and
`outputs/report/slides.html` as the concise slide view.

## Presentation Walkthrough

Recommended timing:

| Block | Time | Artifact section |
| --- | ---: | --- |
| Docker run and reproducibility contract | 2 min | notebook sections 1-2 |
| Source data and cross-sectional workflow | 5 min | notebook section 3 |
| Panel workflow and specification tests | 6 min | notebook section 4 |
| Cross-section vs panel interpretation | 4 min | notebook section 5 |
| Questions / deeper report drilldown | 3 min | `report.html` |

The final message to present is:

- Cross-sectional OLS does **not** find a statistically significant effect of
  aggregate constitutional compliance on GDP growth.
- The 1990-2020 fixed-effects panel model does find a positive within-country
  relationship between constitutional compliance and GDP growth.
- The public Docker image regenerates the results and the report without any
  local dependency installation.

## Project Objective

The source paper asks whether stronger compliance with constitutional rights is
associated with higher GDP growth. The reproduction covers two empirical
designs:

- a 2010-2019 cross-sectional OLS workflow built from the original
  `cross-section/` data and R scripts,
- a 1990-2020 panel workflow covering pooled, between, first-difference,
  fixed-effects, and random-effects models from the original `panel/` data and
  R scripts.

The reference implementation is the combination of `2400-LIC-FII.pdf`,
`cross-section/inorder.R`, `panel/inorder_panel.R`, and the exported R tables
and figures under `cross-section/tables_and_other/` and `panel/panel_tables/`.

## Repository Structure

| Path | Purpose |
| --- | --- |
| `src/repro_research/` | Python package for data loading, transformations, models, diagnostics, validation, and orchestration. |
| `scripts/` | Command-line entry points for the full pipeline, component runs, validation, and report generation. |
| `notebooks/final_presentation_report.ipynb` | Executable final notebook that is exported to HTML inside Docker. |
| `report/` | Quarto report and Reveal.js slide sources. |
| `cross-section/` | Original cross-sectional R workflow, source data, and reference outputs. |
| `panel/` | Original panel R workflow, source data, and reference outputs. |
| `docs/` | Reproduction contract, methodology, limitations, AI disclosure, contribution evidence, and run instructions. |
| `outputs/` | Regenerated Python tables, figures, intermediate files, logs, and final report artifacts. |
| `tests/` | Focused tests for critical loading, models, validation, Docker, and documentation contracts. |

## Quickstart

Local setup is optional for reviewers because the Docker image is the graded
runtime. Use these commands only when developing the project locally:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
python -m pip install --no-deps -e .
```

Validate inputs:

```bash
python scripts/validate_project.py
```

Run the full Python reproduction:

```bash
python scripts/run_all.py
```

Generate the notebook report, Quarto report, and slides locally:

```bash
python scripts/generate_report.py
```

Quarto must be installed locally for the Quarto artifacts. The notebook export
uses the Jupyter dependencies from `requirements.txt`.

## Docker Image Maintenance

The submitted image must be public on Docker Hub and must not contain secrets.
The repository does not require API keys, credentials, private tokens, or
external downloads at runtime.

Before building, keep secrets out of the repository and out of the Docker build
context. `.dockerignore` excludes local environments, git metadata, caches,
logs, generated report outputs, `.env` files, key files, token files, and
secret folders.

Build and push the public image:

```bash
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t janwolyniak/reproducible-research-lic-fii \
  --push .
```

Quick image smoke test after pushing:

```bash
docker pull janwolyniak/reproducible-research-lic-fii
docker run --rm janwolyniak/reproducible-research-lic-fii
```

Run with a host report mount when you want to inspect the generated report:

```bash
mkdir -p outputs/report
docker run --rm \
  -v "$(pwd)/outputs/report:/app/outputs/report" \
  janwolyniak/reproducible-research-lic-fii
```

## Output Descriptions

The master output index is `outputs/README.md`. The most important generated
artifact groups are:

- `outputs/report/`: final notebook report, Quarto report, and Reveal.js decks.
- `outputs/cross_section/`: cross-sectional descriptive statistics,
  correlations, OLS tables, diagnostics, VIFs, influence diagnostics, and
  figures.
- `outputs/panel/`: panel descriptive statistics, correlations, model-family
  tables, specification tests, diagnostics, fixed-effects robustness tables,
  and figures.
- `outputs/intermediate/`: regenerated prepared data snapshots for inspection.
- `outputs/logs/run_all_summary.txt`: run summary for the latest full pipeline.

Table-by-table and figure-by-figure reproduction status is documented in
`docs/reproduction_results.md`, with component detail in
`docs/cross_section_reproduction_results.md` and
`docs/panel_reproduction_results.md`.

## Documentation Map

| Document | Purpose |
| --- | --- |
| `docs/reproduction_contract.md` | Source audit, empirical targets, artifact map, and numerical tolerances. |
| `docs/methodology.md` | Explanation of how the Python reproduction was built. |
| `docs/limitations.md` | Known gaps, numerical-tolerance policy, and unresolved differences from the original R outputs. |
| `docs/run_instructions.md` | Detailed local, component, validation, helper, and Docker commands. |
| `docs/ai_disclosure.md` | AI-use disclosure required by the course outline. |
| `docs/source_annotations.md` | Notes on formulas, package APIs, and external-source reuse. |
| `docs/contributions.md` | Contribution roles and Git history evidence. |

## Contributor Roles

| Contributor | Primary role |
| --- | --- |
| Jan Wolyniak | Project structure, validation, orchestration, Docker, final integration, reviewer documentation. |
| Kinga Kucharska | Cross-sectional data preparation, cross-sectional model reproduction, and cross-sectional result notes. |
| Iwo Wiszejko | Panel data preparation, panel model reproduction, robust covariance outputs, and panel result notes. |

See `docs/contributions.md` for the exact verification command and recent
commit evidence used for the contribution split.

## Troubleshooting

- If Docker cannot connect to the daemon, start Docker Desktop or the local
  Docker service and rerun the command.
- If the image pull fails, confirm that
  `janwolyniak/reproducible-research-lic-fii` exists and is public on
  Docker Hub.
- If a host-mounted run leaves root-owned generated files on Linux, run the
  container without a host bind for a smoke test or adjust file ownership after
  the run.

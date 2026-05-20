# Reproducible Research: Constitutional Compliance and Growth

This repository is a Python reproduction of Kinga Kucharska's thesis,
`2400-LIC-FII.pdf`, on constitutional compliance and economic growth. The
project recreates the original cross-sectional and panel workflows that were
developed in R, documents the differences between the R and Python outputs, and
packages the reproduction in a Docker image for reviewer execution.

Authors:

- [Jan Wolyniak](https://github.com/janwolyniak)
- [Kinga Kucharska](https://github.com/kinga-kucharska)
- [Iwo Wiszejko](https://github.com/IWiszejko)

## Project Objective

The source paper asks whether stronger compliance with constitutional rights is
associated with higher GDP growth. The reproduction covers two empirical
designs:

- A 2010-2019 cross-sectional OLS workflow built from the original
  `cross-section/` data and R scripts.
- A 1990-2020 panel workflow covering pooled, between, first-difference,
  fixed-effects, and random-effects models from the original `panel/` data and
  R scripts.

The reference implementation is the combination of `2400-LIC-FII.pdf`,
`cross-section/inorder.R`, `panel/inorder_panel.R`, and the exported R tables
and figures under `cross-section/tables_and_other/` and `panel/panel_tables/`.

## Repository Structure

| Path | Purpose |
| --- | --- |
| `src/repro_research/` | Reusable Python package for data loading, transformations, models, diagnostics, output writing, validation, and orchestration. |
| `scripts/` | Command-line entry points for the full pipeline and component runs. |
| `cross-section/` | Original cross-sectional R workflow, source data, and reference outputs. |
| `panel/` | Original panel R workflow, source data, and reference outputs. |
| `docs/` | Reproduction contract, methodology, limitations, AI disclosure, contribution evidence, run instructions, and results comparisons. |
| `outputs/` | Regenerated Python tables, figures, intermediate inspection files, and local run logs. |
| `tests/` | Minimal tests for critical loading, model-output, Docker, validation, CLI, and documentation contracts. |

## Quickstart

Create a local environment and install the constrained dependencies:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

Validate the tracked source inputs and expected output directories:

```bash
python scripts/validate_project.py
```

Run the full Python reproduction:

```bash
python scripts/run_all.py
```

Validate that the regenerated output manifest is complete:

```bash
python scripts/validate_project.py --generated-outputs
```

For faster table-only checks, skip plot regeneration:

```bash
python scripts/run_all.py --skip-plots
```

Use `--strict-validation` with `--skip-plots` when the complete output
manifest should still be required after a plot-skipping run.

## Docker Reproduction

Build the reviewer image from a clean checkout:

```bash
docker build -t janwolyniak/reproducible-research-lic-fii:phase6 .
```

Run the complete reproduction pipeline in the container:

```bash
docker run --rm janwolyniak/reproducible-research-lic-fii:phase6
```

The default container command is `python scripts/run_all.py`. It writes the same
regenerated files under `/app/docs` and `/app/outputs` inside the container.

Use Docker Compose when regenerated files should be written back to the host
checkout:

```bash
docker compose run --rm reproduction
```

The final Docker Hub image is:

```text
janwolyniak/reproducible-research-lic-fii:phase6
```

If the image is already pushed, reviewers can run:

```bash
docker pull janwolyniak/reproducible-research-lic-fii:phase6
docker run --rm janwolyniak/reproducible-research-lic-fii:phase6
```

## Output Descriptions

The master output index is `outputs/README.md`. The most important generated
artifact groups are:

- `outputs/cross_section/`: cross-sectional descriptive statistics,
  correlations, OLS tables, diagnostics, VIFs, influence diagnostics, and
  figures.
- `outputs/panel/`: panel descriptive statistics, correlations, model-family
  tables, specification tests, diagnostics, fixed-effects robustness tables, and
  figures.
- `outputs/intermediate/`: regenerated prepared data snapshots for inspection.
- `outputs/logs/run_all_summary.txt`: local run summary for the latest full
  pipeline run.

The table-by-table and figure-by-figure reproduction status is documented in
`docs/reproduction_results.md`, with component detail in
`docs/cross_section_reproduction_results.md` and
`docs/panel_reproduction_results.md`.

## Documentation Map

| Document | Purpose |
| --- | --- |
| `docs/reproduction_contract.md` | Source audit, empirical targets, artifact map, and numerical tolerances. |
| `docs/methodology.md` | Plain-language explanation of how the Python reproduction was built. |
| `docs/limitations.md` | Known gaps, numerical-tolerance policy, and unresolved differences from the original R outputs. |
| `docs/run_instructions.md` | Detailed local, component, validation, helper, and Docker commands. |
| `docs/ai_disclosure.md` | AI-use disclosure required by the course outline. |
| `docs/source_annotations.md` | Notes on formulas, package APIs, and external-source reuse. |
| `docs/contributions.md` | Contribution roles and Git history evidence. |

## Contributor Roles

| Contributor | Primary role | Git evidence |
| --- | --- | --- |
| Jan Wolyniak | Project structure, validation, orchestration, Docker, final integration, reviewer documentation. | Commits such as `Add inventory data script and update documentation`, `Implement Docker support for reproducibility, including Dockerfile, docker-compose, and .dockerignore; update README and run instructions with Docker commands and troubleshooting.`, and `Add user-facing CLI options for reproducibility scripts and enhance output handling`. |
| Kinga Kucharska | Cross-sectional data preparation, cross-sectional model reproduction, and cross-sectional result notes. | Commits such as `Add cross-sectional data preparation`, `feat: update dependencies and enhance cross-section analysis`, and `Refactor output paths and update GDP column references`. |
| Iwo Wiszejko | Panel data preparation, panel model reproduction, robust covariance outputs, and panel result notes. | Commits such as `Add panel data preparation for Phase 2`, `Add Phase 4 panel reproduction (models, tests, robust SE, alt-FE)`, and `Add Phase 5 panel reproduction results`. |

See `docs/contributions.md` for the exact verification command and the recent
commit evidence used for the contribution split.

## Troubleshooting

- If Docker cannot connect to the daemon, start Docker Desktop or the local
  Docker service and rerun the command.
- If dependency installation fails during `docker build`, rebuild with
  `--no-cache` to avoid a stale layer:
  `docker build --no-cache -t janwolyniak/reproducible-research-lic-fii:phase6 .`.
- If a host-mounted compose run leaves root-owned generated files on Linux, run
  the plain `docker run --rm ...` command or adjust file ownership after the
  run.

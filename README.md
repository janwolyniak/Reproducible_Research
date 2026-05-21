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

## Docker Reproduction (presentation workflow)

The project is shipped as a public Docker image on Docker Hub. The image runs
the full pipeline **and** renders a Quarto report and a Reveal.js slide deck
that together form the basis of the presentation. The instructor pulls the
image directly — nothing is built on the lab laptop.

### One-command reviewer workflow

```bash
docker pull janwolyniak/reproducible-research-lic-fii:phase6

docker run --rm \
  -v "$(pwd)/outputs:/app/outputs" \
  janwolyniak/reproducible-research-lic-fii:phase6
```

After the container exits, open these two files from `outputs/report/`:

| File                          | Purpose                                       |
| ----------------------------- | --------------------------------------------- |
| `outputs/report/report.html`  | Full reviewer report (the executable report). |
| `outputs/report/slides.html`  | Reveal.js slide deck used in the presentation. |

Both files are self-contained HTML (no external assets). Open them in any
browser.

The default container command is `python scripts/generate_report.py`. It:

1. Validates the project layout and tracked source inputs.
2. Runs the cross-sectional and panel pipelines (`scripts/run_all.py`).
3. Validates the regenerated output manifest.
4. Renders `report/report.qmd` and `report/slides.qmd` with Quarto.
5. Drops the rendered artefacts into `/app/outputs/report/`.

### Quick smoke test without a host bind

If you only want to confirm that the container can pull, install, run the
pipeline, and render the report end-to-end (without writing the report back to
the host), run:

```bash
docker run --rm janwolyniak/reproducible-research-lic-fii:phase6
```

This is the shortest possible reviewer command. The rendered artefacts stay
inside the container layer; the container exit code (0 / non-zero) tells you
whether the full pipeline + render succeeded.

### Locally building the image (optional)

```bash
docker build -t janwolyniak/reproducible-research-lic-fii:phase6 .
```

The Dockerfile installs Quarto (matching the host architecture) on top of the
Python scientific stack, so a single image produces both the analysis and the
rendered report.

### Compose (when more than just the report needs to land on the host)

```bash
docker compose run --rm reproduction
```

The compose service bind-mounts the whole repository to `/app`, so every
regenerated CSV, HTML table, and figure (not just the report) is written back
to the host checkout.

### Published image

```text
janwolyniak/reproducible-research-lic-fii:phase6
```

> **Note for the reviewer.** The image is built and pushed by Jan Wołyniak.
> Please pull from Docker Hub rather than rebuilding locally during the lab
> session — the pull is the workflow that is graded.

## Presentation

> **Status: template / MVP draft.** The presentation materials below — both
> slide decks, both speaking scripts, the demo helper, and the structure of
> this section itself — are a **first pass**. They were prepared before the
> group meeting that will decide presentation format (option A vs B), final
> timing per speaker, and any feedback we still expect from the instructor.
> Everything in this section will be revised before the actual lab session.
> Treat the current content as a starting baseline to iterate from, not as
> finished material.

The 20-minute presentation (Reproducible Research course, 2026 summer term) is
driven by the same Docker image. The on-laptop demo is:

```bash
docker pull janwolyniak/reproducible-research-lic-fii:phase6
docker run --rm -v "$(pwd)/outputs:/app/outputs" \
  janwolyniak/reproducible-research-lic-fii:phase6
```

After the container exits, three artefacts are ready under
`outputs/report/`:

- `report.html` — the full reviewer report (step-by-step narrative).
- `slides.html` — technical Reveal.js deck (embedded tables, code).
- `slides_visual.html` — visual PPTX-style deck (big numbers, narrative).

Two presentation options are documented:

- [`docs/presentation/option_A_hybrid.md`](docs/presentation/option_A_hybrid.md) —
  technical slide deck driving the narrative, with three planned drilldowns
  into the full report.
- [`docs/presentation/option_B_visual.md`](docs/presentation/option_B_visual.md) —
  visual slide deck paired with Jan running `python scripts/demo.py` and the
  Docker pipeline live from a PowerShell terminal.

Shared per-block speaker text, anticipated Q&A, and the demo failover plan
live in [`docs/presentation/speaking_script.md`](docs/presentation/speaking_script.md).

The Quarto sources are:

- `report/report.qmd` — the full reviewer report.
- `report/slides.qmd` — the technical Reveal.js deck (option A).
- `report/slides_visual.qmd` — the visual Reveal.js deck (option B).
- `report/_quarto.yml` — Quarto project config.
- `scripts/demo.py` — terminal-side helper for option B.

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

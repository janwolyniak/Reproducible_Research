# TODO: Final Reproducible Research Project

## Objective

Reproduce the research already collected in this repository in Python, using the
original materials as the reference implementation:

- `2400-LIC-FII.pdf` as the paper to be reproduced.
- `cross-section/` as the cross-sectional R workflow and reference outputs.
- `panel/` as the panel-data R workflow and reference outputs.

The final submission must satisfy every requirement from `OUTLINE.md`: visible
GitHub contribution history, documented code and results, clean user-friendly
code, and a Docker-based reproducible environment that can be pulled and run by
the reviewer.

## Contributors and Ownership

| Contributor | Primary ownership | Required visible contribution evidence |
| --- | --- | --- |
| Jan Wolyniak | Python project structure, reproducibility pipeline, Docker, final integration | Commits adding the runnable project skeleton, orchestration script, Dockerfile, docker-compose setup, and final run instructions |
| Kinga Kucharska | Cross-sectional reproduction | Commits adding Python data loading, descriptive statistics, correlations, OLS models, robust standard errors, diagnostics, and exported cross-sectional tables/figures |
| Iwo Wiszejko | Panel-data reproduction | Commits adding Python panel-data preparation, pooled/between/first-difference/fixed/random effects models, specification tests, robust covariance estimates, and exported panel tables/figures |

Shared tasks must still have clear authorship in Git history. When work is
paired, the commit message or PR description should name the reviewer and the
main implementer.

## Phase 0: Source Audit and Reproduction Contract

- [x] Read `2400-LIC-FII.pdf` and write a concise reproduction summary in
  `docs/reproduction_contract.md`.
- [x] Identify the paper's research question, dependent variables, key
  explanatory variables, controls, model families, and expected tables/figures.
- [x] Map every relevant R artifact to the Python artifact that will replace it:
  `cross-section/inorder.R`, `panel/inorder_panel.R`, existing `.rds` data, HTML
  tables, plots, and diagnostics.
- [x] Decide which outputs are exact-reproduction targets and which are
  best-effort approximations due to library differences between R and Python.
- [x] Record all known limitations and acceptable numerical tolerances before
  writing the Python models.
- [x] Create an initial issue/section in this TODO for any missing data,
  ambiguous variables, or undocumented transformations found during the audit.

Owner: Jan, with review by Kinga and Iwo.

## Phase 0 Audit Issues and Decisions

The detailed audit contract is in `docs/reproduction_contract.md`. The machine-
generated audit report is in `docs/phase0_audit.md` (regenerate with
`python3 scripts/audit_phase0.py`). The following items must stay visible during
implementation phases.

- [x] Make the Phase 0 audit executable against the tracked data. The audit
  script reads tracked `.rds` and `.dta` inputs with Python dependencies when
  available and falls back to local `Rscript` for `.rds` files, fails non-zero
  on read errors or missing contract paths, and regenerates
  `docs/phase0_audit.md`.
- [x] Treat the tracked processed extracts as authoritative for this project.
  Upstream raw/bulk-download provenance is intentionally out of scope for this
  reproduction because `cross-section/data/` now contains source-level extracts:
  CCCD averages/details/growth files, V-Dem democracy averages,
  World Bank-style variable extracts, CEPII geography `geo_cepii.dta`, and
  grouping data in `oth_char.rds`.
- [x] Reconcile variable names before writing reusable loaders:
  `GDP_growth`/`GDPgrowth`, `tot`/`tot2`/`tot_growth`,
  `Country Code`/`Country_Code`/`Country_Name`/`Country`/`cname`,
  `country_name`/`country_text_id`/`iso3`/`iso3c`, and
  `gov_exp_milit`/`gov_exp_mil`. See `docs/phase0_audit.md`, section 5.
- [x] Map final cross-sectional variables to tracked input sources and document
  local lineage/attrition from source extracts into `model_data*.rds`. See
  `docs/phase0_audit.md`, sections 2-4.
- [x] Treat `cross-section/inorder.R` and `panel/inorder_panel.R` as workflow
  evidence, not executable scripts. They contain bare section labels,
  exploratory fragments, and unresolved object references.
- [x] Confirm the cross-sectional outlier rule in code. The paper says five
  outliers were removed and the cleaned dataset has 157 countries, while the
  main OLS tables use 121-122 observations after model-wise missingness. The
  audit (`docs/phase0_audit.md`, sections 6-7) confirms `model_data4` (162 rows)
  drops 5 countries -- Congo, Rep.; Lesotho; Luxembourg; Malta; Singapore --
  to produce `model_data4_o` (157 rows), recomputes the preferred-model
  influence diagnostics, and explains the 121-122 observation OLS table counts.
- [x] Record the panel diagnostic and robust-covariance caveat as a Phase 4
  implementation requirement. The actual LM/F/Hausman tests, serial
  correlation, heteroskedasticity, cross-sectional dependence, and
  Driscoll-Kraay/clustered covariance outputs remain in Phase 4 because they
  require the panel model implementation.
- [x] Keep HTML/CSS styling and exact plot pixels as best-effort targets; compare
  numerical table values and plot source data instead.

## Phase 1: Repository Structure and Environment Baseline

- [x] Create a clean Python project layout, for example:
  - `src/repro_research/` for reusable Python modules.
  - `scripts/` for command-line entry points.
  - `outputs/` for regenerated tables, figures, and logs.
  - `docs/` for reproduction notes, AI disclosure, and final run instructions.
- [x] Add dependency management with pinned or constrained versions
  (`requirements.txt`, `pyproject.toml`, or both).
- [x] Keep the existing `helpers/rds_to_csv.py` workflow available for converting
  `.rds` files into reproducible CSV inputs.
- [x] Add a single command such as `python scripts/run_all.py` that rebuilds all
  Python outputs from the tracked inputs.
- [x] Add a lightweight validation command that checks that required input data
  and expected output directories exist.
- [x] Update `.gitignore` so generated caches are ignored while final
  reproducibility artifacts remain trackable when appropriate.

Owner: Jan.

Implementation update, 2026-05-19:

- Added `pyproject.toml` and `requirements.txt` with constrained Python
  dependencies for data loading, statistics/econometrics, plotting, formatting,
  linting, and tests.
- Added the reusable package baseline in `src/repro_research/`, including
  path contracts, project validation, output directory creation, and pipeline
  orchestration.
- Added `scripts/validate_project.py` and `scripts/run_all.py`. The current
  Phase 1 `run_all` command validates all required tracked inputs/reference
  artifacts, creates expected output directories, regenerates
  `docs/phase0_audit.md`, and writes a local run log under `outputs/logs/`.
- Added `outputs/` placeholders, `outputs/README.md`,
  `docs/run_instructions.md`, and `docs/ai_disclosure.md`.
- Kept `helpers/rds_to_csv.py` available and included it in the documented
  run instructions.
- Updated `.gitignore` so local logs/intermediate caches are ignored while
  final cross-sectional and panel outputs remain trackable.
- Verification passed:
  `python3 scripts/validate_project.py`,
  `python3 scripts/run_all.py`,
  `python3 -m py_compile helpers/rds_to_csv.py scripts/audit_phase0.py scripts/validate_project.py scripts/run_all.py src/repro_research/__init__.py src/repro_research/paths.py src/repro_research/validation.py src/repro_research/pipeline.py`,
  `python3 -m ruff check src scripts helpers`, and
  `python3 -m black --check src scripts helpers`.

## Phase 2: Data Preparation in Python

- [x] Inventory all `.rds` inputs in `cross-section/data/` and
  `panel/data_panel/`. Shared inventory: `docs/data_inventory.csv` and the
  `Cross-Sectional Sources`/`Panel Sources` tables in `docs/data_dictionary.md`,
  regenerated by `python scripts/inventory_data.py`. Panel-specific schemas
  live in `src/repro_research/panel_data.py`; see also `docs/phase0_audit.md`
  sections 1-3.
  - [x] Cross-sectional inventory generated at
    `outputs/cross_section/cross_section_data_inventory.csv`.
- [x] Convert required `.rds` files to CSV or load them directly in Python with a
  documented, repeatable path. Shared loader: `repro_research.data_io`
  (`build_dataset_specs`, `load_dataset`, `read_rds_dataframe`). Panel side
  also loads directly via `pyreadr` in `src/repro_research/panel_data.py` and
  materialises prepared frames as CSV under `outputs/intermediate/panel/`.
  - [x] Cross-sectional loader added in
    `src/repro_research/cross_section_data.py`; it reads `.rds` inputs through
    `pyreadr` when available and falls back to local `Rscript`, with `.dta`
    support through pandas.
- [x] Build reusable data-loading functions with explicit schemas and clear
  handling of missing values, categorical fields, country identifiers, and year
  fields. Panel side: `load_panel("main"|"alt"|"small")` with `PanelSpec`
  schemas and shared `country_naming.to_canonical`.
  - [x] Cross-sectional loaders normalize country-name and country-code
    variants to `country_name` and `country_code`, validate required model
    columns for `model_data4`/`model_data4_o`, coerce numeric fields, and keep
    grouping fields such as `continent`, `legal_old_o`, and `landlocked`
    categorical.
- [x] Recreate transformations used by the R scripts, including logs, lags,
  interaction terms, outlier-filtered datasets, and country/year panel indexes.
  Panel side: `add_log_compliance`, `add_lag_log_gdppc`,
  `add_tot_trade_interaction`, `subset_period`, `subset_by_continent`,
  `subset_by_legal_origin` in `src/repro_research/panel_transforms.py`.
  - [x] Cross-sectional transformations add `log_GDPpc2015`, shifted compliance
    logs such as `log_cc_total_plus2`, canonical `terms_trade`,
    `terms_trade_x_trade`, and an explicit outlier flag comparing `model_data4`
    with `model_data4_o`.
- [x] Save intermediate clean datasets only when they improve reproducibility;
  otherwise regenerate them during the pipeline. Panel side: `scripts/
  run_panel_prep.py` writes CSV caches under `outputs/intermediate/panel/` and
  is wired into `pipeline.run_all`.
  - [x] Cross-sectional prepared model datasets are regenerated into ignored
    intermediate CSVs under `outputs/intermediate/`; reviewable inventory and
    preparation summaries are tracked under `outputs/cross_section/`.
- [x] Add a data dictionary in `docs/data_dictionary.md` for the variables used
  in the reproduced models. Shared source inventory, panel variables, and
  cross-sectional model variables are documented there.

Owners: Kinga for cross-sectional data; Iwo for panel data; Jan for shared
loader integration.

Jan implementation update, 2026-05-19:

- Added shared source readers in `src/repro_research/data_io.py` for tracked
  `.rds`, `.dta`, and `.csv` data, plus `DatasetSpec` metadata for
  cross-sectional and panel source groups.
- Added `scripts/inventory_data.py`, which inventories all tracked
  cross-sectional and panel source files and writes `docs/data_inventory.csv`
  plus the shared scaffold in `docs/data_dictionary.md`.
- Wired `scripts/run_all.py` to regenerate the data inventory after the Phase 0
  audit, so Jan's shared Phase 2 integration runs through the same
  reproducibility entry point.
- The remaining Phase 2 checkboxes are not fully completable by Jan alone:
  model-specific schemas, missing-value rules, categorical handling,
  transformations, and final variable definitions depend on Kinga's
  cross-sectional preparation code and Iwo's panel preparation code.

Kinga implementation update, 2026-05-19:

- Added `src/repro_research/cross_section_data.py` and
  `scripts/run_cross_section_data.py`.
- Added generated cross-sectional Phase 2 artifacts:
  `outputs/cross_section/cross_section_data_inventory.csv`,
  `outputs/cross_section/cross_section_preparation_summary.csv`, and
  `docs/data_dictionary.md`.
- Wired cross-sectional data preparation into `python scripts/run_all.py`.
- The tracked `model_data4.rds` lacks `tot2`, while `model_data4_o.rds` contains
  it. The loader therefore exposes canonical `terms_trade`, using `tot2` when
  available and falling back to `tot` otherwise. The preferred outlier-filtered
  cross-sectional model sample has 122 complete rows for the Phase 3 `re4_o`
  specification variables.

## Phase 3: Cross-Sectional Reproduction

- [x] Reproduce descriptive statistics from
  `cross-section/tables_and_other/descriptive stat.html`.
- [x] Reproduce Spearman correlation matrices and correlation plots from the
  cross-sectional workflow.
- [x] Recreate the main OLS specifications from `cross-section/inorder.R`,
  including the preferred models around GDP growth, constitutional compliance,
  trade, terms of trade, investment, government expenditure, and GDP per capita.
- [x] Implement robust standard errors where used in the R workflow.
- [x] Reproduce diagnostic checks: RESET, variance inflation factors, residual
  normality checks, heteroskedasticity tests, Durbin-Watson/autocorrelation
  checks, leverage, standardized residuals, and Cook's distance.
- [x] Recreate the outlier-filtered robustness regressions.
- [x] Recreate alternative institutional-variable regressions where they are
  part of the paper's argument.
- [x] Export publication-ready tables and figures to `outputs/cross_section/`.
- [x] Write `docs/cross_section_reproduction.md` explaining what matches the R
  outputs and where Python results differ.

Owner: Kinga. Reviewer: Jan.

Kinga implementation update, 2026-05-19:

- Added `src/repro_research/cross_section.py` and
  `scripts/run_cross_section.py`.
- Reproduced cross-sectional descriptive statistics, Spearman correlation
  matrix, grouped Spearman tests for `cc_total` and `cc_prop`, and a
  best-effort correlation plot under `outputs/cross_section/`.
- Recreated the main OLS model family from `cross-section/inorder.R`, including
  full-sample models, outlier-filtered robustness models, `cc_total_lv` and
  `cc_prop` compliance variants, and institutional alternatives for World Bank
  governance variables.
- Exported HC3 robust-standard-error coefficient tables, diagnostics, VIFs,
  influence diagnostics, and residual/Cook's-distance plots.
- Wrote `docs/cross_section_reproduction.md` with matching notes and the
  documented `model_data4` `tot2`/`terms_trade` fallback.
- Wired Phase 3 into `python scripts/run_all.py` and added a smoke test for the
  preferred outlier-filtered model sample.

## Phase 4: Panel Reproduction

- [x] Reproduce panel descriptive statistics and correlation outputs from
  `panel/inorder_panel.R`. See `outputs/panel/descriptive_statistics.{csv,html}`
  and `outputs/panel/spearman_correlation_matrix.{csv,html}` plus
  `outputs/panel/figures/correlation_plot.png`.
- [x] Build panel indexes by country and year. `repro_research.panel._indexed`
  applies `set_index(["country", "Year"])` for every model fit.
- [x] Recreate pooled OLS, between, first-difference, fixed-effects, and
  random-effects models. See `repro_research.panel.MODEL_SPECS` covering all
  five kinds for three blocks; outputs in `outputs/panel/models_*.{csv,html}`.
- [x] Recreate the reduced/small panel specifications used for 2010-2019 and
  the broader 1990-2020 comparison where applicable. `models_small_full`,
  `models_small_reduced`, and `models_main_reduced` cover the three blocks.
- [x] Reproduce specification tests: LM test, F test for fixed effects, and
  Hausman test. See `outputs/panel/specification_tests.{csv,html}`.
- [x] Reproduce diagnostic tests: serial correlation, heteroskedasticity, and
  cross-sectional dependence. See `outputs/panel/diagnostic_tests.{csv,html}`
  (Breusch-Godfrey panel proxy for the R `pbgtest` target, Breusch-Pagan on the
  fitted model design, Pesaran CD).
- [x] Implement robust covariance variants used in the R workflow, including
  Driscoll-Kraay, Arellano-style, clustered, or double-clustered standard errors
  where Python support is available. See `outputs/panel/fixed_effects_main.{csv,
  html}`; Driscoll-Kraay implemented locally with the
  `floor(4*(T/100)^(2/9))` lag rule.
- [x] Recreate alternative fixed-effects regressions for compliance components
  such as basic, civil, political, and property compliance. See
  `outputs/panel/fixed_effects_compliance_categories.{csv,html}`.
- [x] Export publication-ready tables and figures to `outputs/panel/`.
- [x] Write `docs/panel_reproduction.md` explaining what matches the R outputs
  and where Python results differ.

Owner: Iwo. Reviewer: Jan.

## Phase 5: Paper-Level Comparison and Results Documentation

- [x] Create a master results index in `outputs/README.md`.
- [x] Compare regenerated Python outputs against the existing R-generated HTML
  tables and plots. Panel side: side-by-side comparison in
  `docs/panel_reproduction_results.md` covering descriptive stats, FE main,
  alt-FE compliance components, specification tests, and diagnostic tests.
  Cross-section side: side-by-side comparison in
  `docs/cross_section_reproduction_results.md` covering descriptive stats,
  Spearman tests, OLS tables, institutional alternatives, diagnostics, and
  figures.
- [x] Add a table-by-table and figure-by-figure reproduction checklist in
  `docs/reproduction_results.md`. Master file to be stitched by Jan from the
  panel and cross-section sub-documents. Panel checklist already in
  `docs/panel_reproduction_results.md`; cross-section checklist already in
  `docs/cross_section_reproduction_results.md`.
- [x] Explain any non-matching results with concrete causes, such as different
  default covariance estimators, missing-value handling, lag construction, or
  package-level implementation differences. Panel-side causes documented:
  Driscoll-Kraay finite-sample correction differences between
  `linearmodels`/local SCC and R `sandwich::vcovSCC`, and the
  `plm::pbgtest` vs Breusch-Godfrey panel proxy substitution for the 2010-2019
  serial-correlation cell. Cross-section-side causes documented:
  `model_data2` vs exported descriptive-table sample differences and
  plot-rendering differences between R `corrplot` and Python Seaborn.
- [x] Include short interpretation notes so the reviewer can see how the Python
  results relate to the paper's economic argument. Panel interpretation notes
  added to `docs/panel_reproduction_results.md` (effect size, cross-section vs
  panel reconciliation, component decomposition, SE robustness, CD evidence).
  Cross-section interpretation notes added to
  `docs/cross_section_reproduction_results.md` (insignificant compliance
  coefficients, convergence/control-variable pattern, and institutional
  alternative context).
- [x] Ensure all final tables and figures can be regenerated from a clean clone.

Owners: Jan for integration; Kinga for cross-section notes; Iwo for panel notes.

Iwo implementation update, 2026-05-19:

- Added `docs/panel_reproduction_results.md` with the panel side of the
  Phase 5 paper-level comparison: per-artifact match status, side-by-side
  coefficient and standard-error tables for the main FE and alt-FE blocks
  against `panel/panel_tables/model_output.html` and `model_output2.html`,
  specification and diagnostic test comparisons against the R reference,
  and interpretation notes tying the Phase 4 numbers to the paper's
  economic argument.
- At the time of Iwo's update, the cross-section sub-doc and the master
  `docs/reproduction_results.md` index remained open.

Kinga implementation update, 2026-05-19:

- Added `docs/cross_section_reproduction_results.md` with the cross-sectional
  Phase 5 comparison against `cross-section/tables_and_other/`.
- Documented table-by-table and figure-by-figure status for descriptive
  statistics, Spearman tests, correlation plot, main OLS tables, compliance
  variants, institutional alternatives, diagnostics, and influence plots.
- Recorded concrete mismatch causes for the descriptive-statistics sample,
  and plot-rendering differences. The `rich`/`poor` Spearman rows are
  reproduced using the R-described median split on `GDPpc`.
- Added interpretation notes tying the cross-sectional null compliance result
  to the paper's economic argument and to the institutional-variable robustness
  checks.
- The remaining Phase 5 open items are Jan-owned integration tasks:
  `outputs/README.md`, master `docs/reproduction_results.md`, and clean-clone
  final verification.

Jan integration update, 2026-05-20:

- Reworked `outputs/README.md` into the master output index, covering the
  cross-sectional, panel, intermediate, and log artifacts plus the scripts that
  regenerate each group.
- Added `docs/reproduction_results.md` as the stitched Phase 5 master
  table-by-table and figure-by-figure checklist. It links the cross-section and
  panel sub-documents, records match statuses, explains documented mismatches,
  and states the clean-clone regeneration contract.
- Added `GENERATED_OUTPUT_FILES` and `validate_project(...,
  generated_outputs=True)` so `python scripts/validate_project.py
  --generated-outputs` verifies the expected final artifact manifest:
  regenerated tables, figures, intermediate CSVs, generated docs, Phase 5
  comparison docs, the output index, and the run summary. `python
  scripts/run_all.py` now runs this manifest check before returning success.
- Updated `README.md` and `docs/run_instructions.md` so reviewers can run the
  full pipeline and the generated-output validation explicitly.
- Verification passed in the repo-local virtualenv:
  `.venv/bin/python scripts/run_all.py`,
  `.venv/bin/python scripts/validate_project.py --generated-outputs`,
  `.venv/bin/python -m pytest` (`31 passed, 8 existing panel absorbed-variable
  warnings`), `.venv/bin/python -m ruff check src scripts helpers tests`, and
  `.venv/bin/python -m black --check src scripts helpers tests`.

## Phase 6: Docker and Full Reproducibility

- [x] Add a `Dockerfile` that installs Python, system dependencies, project
  dependencies, and any packages required for reading `.rds` files.
- [x] Add `docker-compose.yaml` with a service that runs the complete
  reproduction pipeline.
- [x] Add `.dockerignore` to keep the build context small and deterministic.
- [x] Make the default container command run the full reproduction or print a
  clear command for doing so.
- [x] Verify that a clean Docker build can run `python scripts/run_all.py`
  without local-machine assumptions.
- [x] Document exact Docker commands in `README.md`, including build, run,
  output location, and troubleshooting.
- [x] Build and push the final image to Docker Hub.
- [x] Record the Docker Hub image name and tag in `README.md`.

Owner: Jan. Reviewers: Kinga and Iwo must independently run the documented
Docker command before final submission.

Jan implementation update, 2026-05-20:

- Added `Dockerfile` based on `python:3.11-slim`, with scientific-Python build
  dependencies, non-interactive Python defaults, `MPLBACKEND=Agg`, constrained
  requirements installation, editable package installation, and default
  `CMD ["python", "scripts/run_all.py"]`.
- Added `docker-compose.yaml` with a `reproduction` service that builds and
  runs `janwolyniak/reproducible-research-lic-fii`, bind-mounting the
  checkout at `/app` so regenerated reviewer artifacts are written back to the
  repository.
- Added `.dockerignore` excluding local virtual environments, Git metadata,
  Python caches, test/lint caches, and ignored scratch outputs from the build
  context.
- Added `tests/test_docker_reproducibility.py` to lock the Docker default
  command, compose service image/command, and `.dockerignore` scratch-state
  exclusions.
- Updated `README.md` and `docs/run_instructions.md` with exact Docker build,
  run, compose, output-location, image-tag, pull, and troubleshooting commands.
- Docker Hub target image/tag:
  `janwolyniak/reproducible-research-lic-fii`.
- Verification passed on 2026-05-20:
  `docker build -t janwolyniak/reproducible-research-lic-fii .`,
  `docker run --rm janwolyniak/reproducible-research-lic-fii`,
  `docker compose config`, `.venv/bin/python -m pytest
  tests/test_docker_reproducibility.py tests/test_validation.py`,
  `.venv/bin/python -m ruff check src scripts helpers tests`, and
  `.venv/bin/python -m black --check src scripts helpers tests`.
- Docker Hub push passed on retry:
  `docker push janwolyniak/reproducible-research-lic-fii`, digest
  `sha256:293c9b92a4a76a1a936fba388188f6ea17c86d4a431269c5c8b2214835fc0ee4`,
  size 856.

Iwo reviewer verification update, 2026-05-20:

- Ran the Phase 6 reviewer Docker workflow on Windows 11, AMD64,
  Docker Desktop 29.3.1, Linux engine (default platform `linux/amd64`).
- Confirmed Docker daemon health with `docker info --format "Server Version:
  {{.ServerVersion}}"` (returned `29.3.1`).
- Issue found with the published Docker Hub image. `docker pull
  janwolyniak/reproducible-research-lic-fii` fails on AMD64 hosts with
  `no matching manifest for linux/amd64 in the manifest list entries: not
  found`. `docker manifest inspect` confirms the pushed image only ships
  `linux/arm64`. This means the reviewer pull command documented in
  `README.md` will fail on a typical x86_64 lab machine.
  - Recommended fix for Jan: rebuild and push as multi-arch via Buildx, for
    example
    `docker buildx build --platform linux/amd64,linux/arm64 -t
    janwolyniak/reproducible-research-lic-fii --push .`.
  - Until that push lands, the documented reviewer path is `git clone` +
    `docker compose run --rm reproduction`, which builds the image locally
    from the repo Dockerfile and works on AMD64.
- Local Dockerfile path verified end to end. `docker build -t lic-fii-local:
  test .` produced a 1.62 GB native AMD64 image without errors.
- Full pipeline verified via `docker compose run --rm reproduction` (after
  tagging the local image to satisfy the `image:` field in
  `docker-compose.yaml`). The container executed `python scripts/run_all.py`
  to exit code 0. Per-script statuses in `outputs/logs/run_all_summary.txt`:
  `audit_phase0.py: ok`, `run_panel_prep.py: ok`,
  `run_cross_section_data.py: ok`, `run_cross_section.py: ok`,
  `run_panel.py: ok`, `inventory_data.py: ok`.
- Host-side artifact regeneration confirmed. The compose volume mount wrote
  the full Phase 3, 4, and 5 artifact manifest back to the host: panel
  descriptive statistics, Spearman correlation matrix and figure, pooled/
  between/FD/FE/RE model tables, small full/reduced and main-reduced model
  tables, specification tests, diagnostic tests, fixed-effects main,
  fixed-effects compliance categories, plus the cross-sectional descriptive
  statistics, Spearman tables, OLS variants, diagnostics, VIFs, and figure
  set, plus the generated reviewer docs and data inventory.
- Headline panel result reproduced inside the container, matching Phase 4
  documentation: `log_cc_total` coefficient `+1.74` in the main fixed-effects
  block with Driscoll-Kraay standard error `0.29`.
- Only stderr noise was the known `AbsorbingEffectWarning` for `education`
  in `src/repro_research/panel.py:285`, already documented in Phase 7 as the
  "existing panel absorbed-variable warnings". No tracebacks, no failed
  steps, no missing outputs.
- Side effect to flag before submission: running the documented compose
  command on a host where `outputs/` is tracked in Git regenerates the
  numerical tables with slightly different trailing-digit precision than the
  versions currently committed. The differences are within reproduction
  tolerance, but the working tree shows ~25 modified CSV/HTML files after a
  clean container run. Worth deciding whether to commit the freshly
  regenerated outputs or to revert and keep the existing committed versions
  before final submission.

Kinga reviewer verification update, 2026-05-20:

- Ran the documented Phase 6 Docker Compose workflow by composing the
  reproduction service up and then composing it down.
- Confirmed that the compose-mounted output files were produced on the host
  side during the container run.
- Discarded the regenerated volume-mounted output files afterward so the
  reviewer verification did not leave unrelated generated-output changes in
  the working tree.
- Kinga's independent Phase 6 reviewer run is complete.

 
## Phase 7 

- [x] Format Python code with Black.
- [x] Lint Python code with Ruff.
- [x] Keep functions short and explicit where practical.
- [x] Use clear module boundaries: data loading, transformations, models,
  diagnostics, tables, figures, and orchestration.
- [x] Add meaningful command-line flags where useful, for example output
  directory, skip-plots, or strict-validation mode.
- [x] Avoid notebook-only execution; notebooks may be included only as optional
  exploration, not as the only reproducible path.
- [x] Add minimal tests for critical logic, especially data loading,
  transformations, and model-output smoke checks.

Owners: all contributors for their code; Jan for final consistency pass.

Jan final consistency update, 2026-05-20:

- Added user-facing CLI controls to the reproducibility entry points:
  `scripts/run_all.py --skip-plots`, `scripts/run_all.py --strict-validation`,
  and component-level `--output-dir`, `--docs-dir`, and `--skip-plots` flags for
  `scripts/run_cross_section.py` and `scripts/run_panel.py`.
- Preserved the default no-notebook reproducibility path: `python
  scripts/run_all.py` still rebuilds the full source audit, data inventory,
  cross-sectional outputs, panel outputs, docs, run log, and generated-output
  manifest.
- Kept code boundaries aligned with the existing package layout:
  `cross_section_data`/`panel_data` for loading, `panel_transforms` for
  transformations, `cross_section`/`panel` for models, diagnostics, tables, and
  figures, `validation` for manifest checks, and `pipeline` for orchestration.
- Added smoke coverage proving custom output directories and plot-skipping
  behavior for cross-sectional and panel writers.
- Verification passed in the repo-local virtualenv:
  `.venv/bin/python -m pytest tests/test_cross_section.py::test_write_cross_section_outputs_supports_custom_dir_without_plots tests/test_panel_analysis.py::test_write_panel_outputs_supports_custom_dir_without_plots`,
  `.venv/bin/python scripts/run_all.py --help`,
  `.venv/bin/python scripts/run_cross_section.py --help`, and
  `.venv/bin/python scripts/run_panel.py --help`.
- Final verification also passed:
  `.venv/bin/python -m black --check src scripts helpers tests`,
  `.venv/bin/python -m ruff check src scripts helpers tests`,
  `.venv/bin/python -m pytest` (`35 passed`, with the existing panel
  absorbed-variable warnings), `.venv/bin/python scripts/run_all.py`, and
  `.venv/bin/python scripts/validate_project.py --generated-outputs`.

## Phase 8: Documentation Required by `OUTLINE.md`

- [x] Update `README.md` with project objective, source paper, repository
  structure, quickstart, local run instructions, Docker run instructions, output
  descriptions, and contributor roles.
- [x] Add `docs/ai_disclosure.md` stating how AI tools were used, which model
  was used, and which parts were human-reviewed.
- [x] Add source annotations for externally sourced code snippets or formulas.
- [x] Add `docs/methodology.md` describing the reproduction method in plain
  language.
- [x] Add `docs/limitations.md` describing reproducibility gaps, numerical
  tolerances, and unresolved differences from the original R outputs.
- [x] Ensure the project can be understood by a reviewer after cloning the repo,
  without private local paths or hidden setup steps.

Owners: Jan for README and AI disclosure; Kinga for cross-sectional methodology;
Iwo for panel methodology.

Jan implementation update, 2026-05-20:

- Reworked `README.md` into the reviewer entry point required by `OUTLINE.md`:
  project objective, source paper and R references, repository structure,
  quickstart, local run, Docker run, output descriptions, documentation map,
  contributor roles, and troubleshooting.
- Updated `docs/ai_disclosure.md` to state the AI-assisted scope, model used
  (OpenAI Codex, GPT-5 class coding assistant), human-review responsibility,
  and the project source-annotation location.
- Added `docs/methodology.md` describing the source materials, reproduction
  contract, data preparation, cross-sectional workflow, panel workflow, full
  pipeline, and result interpretation in reviewer-facing language.
- Added `docs/limitations.md` covering scope limits, numerical tolerances,
  cross-sectional differences, panel differences, environment limits, and
  remaining reviewer caveats.
- Added `docs/source_annotations.md`, recording that no external code snippets
  were copied verbatim and annotating the formulas/package APIs used for OLS,
  diagnostics, correlations, panel models, specification tests, and robust
  covariance estimates.
- Added the new static reviewer docs to the project validation contract and
  added `tests/test_reviewer_documentation.py` to protect the documentation
  coverage required by `OUTLINE.md`.

## Phase 9: GitHub Contribution Evidence

- [x] Each contributor makes commits under their own GitHub identity.
- [x] Commit messages are descriptive and imperative, for example `Add panel
  fixed-effects reproduction`.
- [x] Use branches or pull requests if practical so review history is visible.
- [x] Keep final generated outputs, documentation, and code changes organized in
  reviewable commits.
- [x] Before submission, add a contribution summary table to `README.md` or
  `docs/contributions.md`.
- [x] Verify that GitHub history visibly supports the contribution split claimed
  in the documentation.

Owners: all contributors.

Jan implementation update, 2026-05-20:

- Added a contributor-role table to `README.md`.
- Added `docs/contributions.md` with the exact verification command
  `git log --format='%h %an <%ae> %s' --max-count=40`, the contribution split,
  recent commit evidence for Jan, Kinga, and Iwo, and final public-history
  review notes.
- Verified the local Git history contains authored commits from all three
  contributors, including Kinga's cross-sectional commits
  `Add cross-sectional data preparation` and
  `feat: update dependencies and enhance cross-section analysis`, Iwo's panel
  commits `Add panel data preparation for Phase 2`,
  `Add Phase 4 panel reproduction (models, tests, robust SE, alt-FE)`, and
  `Add Phase 5 panel reproduction results`, and Jan's integration/Docker
  commits.
- Added documentation-contract tests that require all three contributors and
  representative contribution messages to remain visible in
  `docs/contributions.md`.

## Phase 10: Presentation Preparation

- [x] Prepare a roughly 20-minute presentation covering the research question,
  original paper, data, Python reproduction, Docker workflow, and main results.
- [x] Include all team members in the speaking plan.
- [x] Include a short live or recorded code demonstration using the documented
  run command.
- [x] Show the Docker workflow because the project will be run after pulling
  from GitHub in the lab.
- [x] Prepare fallback screenshots or pre-rendered outputs in case the lab
  environment has limited time or network issues.

Owners: all contributors. Jan covers reproducibility and Docker; Kinga covers
cross-section; Iwo covers panel.

Iwo implementation update, 2026-05-21:

**MVP status notice.** Everything added in this update is a template /
first-pass version. It satisfies the Phase 10 checklist items mechanically
(a report, slides, a speaking script, a live-demo path) so the project can
be submitted in time, but the **exact form** of the presentation is not yet
decided — we are waiting on the group meeting to pick between option A
(hybrid: slides + report drilldowns) and option B (visual deck + Jan's
terminal demo), and on any further format guidance from the instructor.
All presentation files now carry an explicit "template / MVP" banner so
nobody mistakes them for finished material. Plan: revise after the group
decides and after we rehearse with a clock.

- Switched the presentation deliverable to a Quarto-rendered report and
  Reveal.js slide deck, both built by the same Docker image. Sources live
  under `report/` (`report/_quarto.yml`, `report/report.qmd`,
  `report/slides.qmd`).
- Added `scripts/generate_report.py` as the new default container command. It
  runs `scripts/run_all.py`, validates the regenerated output manifest, and
  then renders both `report.qmd` and `slides.qmd` to
  `outputs/report/{report.html, slides.html}`.
- Updated `Dockerfile` to install the Quarto CLI (architecture-aware) on top
  of the Python scientific stack, register a `python3` Jupyter kernel for the
  Quarto chunks, and use `python scripts/generate_report.py` as `CMD`.
- Updated `docker-compose.yaml` to use the new entry point, refreshed
  `.dockerignore`, and updated `tests/test_docker_reproducibility.py` to lock
  the new defaults and the presence of the Quarto report assets.
- Added the reviewer-facing Docker workflow (`docker pull` + `docker run -v ...`)
  and the new presentation section to `README.md`; added the
  `python scripts/generate_report.py` workflow to `docs/run_instructions.md`.
- Added `docs/presentation/speaking_script.md` with the 20-minute timing
  breakdown across Jan/Kinga/Iwo, per-block speaker text, the live-demo
  action list, anticipated Q&A, and the pre-presentation checklist.
- Added `ipykernel`, `jupyter`, `nbclient`, and `nbformat` to
  `requirements.txt` and `pyproject.toml` because Quarto requires a Jupyter
  Python kernel to execute the `.qmd` Python blocks.
- Outstanding action for Jan before the lab session: rebuild the Docker Hub
  image as multi-arch (`docker buildx build --platform linux/amd64,linux/arm64
  -t janwolyniak/reproducible-research-lic-fii --push .`) so the
  `docker pull` step works on the AMD64 lab laptop. Tracked in NEXT_SESSION.md
  and in the pre-presentation checklist of the speaking script.

## Final Submission Checklist

- [x] Fresh clone works locally with documented commands.
- [x] Fresh Docker build works.
- [x] Docker image is pushed to Docker Hub and documented.
- [x] `python scripts/run_all.py` regenerates all expected outputs.
- [x] README explains the full workflow.
- [x] AI disclosure is present.
- [x] Contribution summary is present.
- [x] Cross-sectional outputs are regenerated in Python.
- [x] Panel outputs are regenerated in Python.
- [x] Reproduction limitations are documented.
- [x] Presentation materials are ready.

Final checklist reconciliation, 2026-05-20:

- Fresh local workflow is covered by Phase 5 and Phase 7 verification:
  documented local commands, generated-output validation, and full
  `.venv/bin/python scripts/run_all.py` execution passed.
- Fresh Docker build and run are covered by Phase 6 verification:
  `docker build -t janwolyniak/reproducible-research-lic-fii .`,
  `docker run --rm janwolyniak/reproducible-research-lic-fii`, and
  `docker compose config` passed.
- Docker Hub publication is documented in Phase 6 and `README.md`; the pushed
  image is `janwolyniak/reproducible-research-lic-fii` with digest
  `sha256:293c9b92a4a76a1a936fba388188f6ea17c86d4a431269c5c8b2214835fc0ee4`.
- The latest Phase 8/9 verification reran
  `.venv/bin/python scripts/run_all.py --skip-plots --strict-validation` and
  `.venv/bin/python scripts/validate_project.py --generated-outputs`, proving
  the Python cross-sectional and panel output manifests are present and
  regenerated by the documented pipeline.

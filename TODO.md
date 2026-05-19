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
  script reads tracked `.rds` and `.dta` inputs with Python, fails non-zero on
  read errors or missing contract paths, and regenerates
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
- [x] Convert required `.rds` files to CSV or load them directly in Python with a
  documented, repeatable path. Shared loader: `repro_research.data_io`
  (`build_dataset_specs`, `load_dataset`, `read_rds_dataframe`). Panel side
  also loads directly via `pyreadr` in `src/repro_research/panel_data.py` and
  materialises prepared frames as CSV under `outputs/intermediate/panel/`.
- [ ] Build reusable data-loading functions with explicit schemas and clear
  handling of missing values, categorical fields, country identifiers, and year
  fields. Panel side: `load_panel("main"|"alt"|"small")` with `PanelSpec`
  schemas and shared `country_naming.to_canonical`. Cross-section side pending.
- [ ] Recreate transformations used by the R scripts, including logs, lags,
  interaction terms, outlier-filtered datasets, and country/year panel indexes.
  Panel side: `add_log_compliance`, `add_lag_log_gdppc`,
  `add_tot_trade_interaction`, `subset_period`, `subset_by_continent`,
  `subset_by_legal_origin` in `src/repro_research/panel_transforms.py`. Cross-
  section outlier-filtered subset pending.
- [x] Save intermediate clean datasets only when they improve reproducibility;
  otherwise regenerate them during the pipeline. Panel side: `scripts/
  run_panel_prep.py` writes CSV caches under `outputs/intermediate/panel/` and
  is wired into `pipeline.run_all`.
- [ ] Add a data dictionary in `docs/data_dictionary.md` for the variables used
  in the reproduced models. Panel section added; cross-section section pending.

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

## Phase 3: Cross-Sectional Reproduction

- [ ] Reproduce descriptive statistics from
  `cross-section/tables_and_other/descriptive stat.html`.
- [ ] Reproduce Spearman correlation matrices and correlation plots from the
  cross-sectional workflow.
- [ ] Recreate the main OLS specifications from `cross-section/inorder.R`,
  including the preferred models around GDP growth, constitutional compliance,
  trade, terms of trade, investment, government expenditure, and GDP per capita.
- [ ] Implement robust standard errors where used in the R workflow.
- [ ] Reproduce diagnostic checks: RESET, variance inflation factors, residual
  normality checks, heteroskedasticity tests, Durbin-Watson/autocorrelation
  checks, leverage, standardized residuals, and Cook's distance.
- [ ] Recreate the outlier-filtered robustness regressions.
- [ ] Recreate alternative institutional-variable regressions where they are
  part of the paper's argument.
- [ ] Export publication-ready tables and figures to `outputs/cross_section/`.
- [ ] Write `docs/cross_section_reproduction.md` explaining what matches the R
  outputs and where Python results differ.

Owner: Kinga. Reviewer: Jan.

## Phase 4: Panel Reproduction

- [ ] Reproduce panel descriptive statistics and correlation outputs from
  `panel/inorder_panel.R`.
- [ ] Build panel indexes by country and year.
- [ ] Recreate pooled OLS, between, first-difference, fixed-effects, and
  random-effects models.
- [ ] Recreate the reduced/small panel specifications used for 2010-2019 and
  the broader 1990-2020 comparison where applicable.
- [ ] Reproduce specification tests: LM test, F test for fixed effects, and
  Hausman test.
- [ ] Reproduce diagnostic tests: serial correlation, heteroskedasticity, and
  cross-sectional dependence.
- [ ] Implement robust covariance variants used in the R workflow, including
  Driscoll-Kraay, Arellano-style, clustered, or double-clustered standard errors
  where Python support is available.
- [ ] Recreate alternative fixed-effects regressions for compliance components
  such as basic, civil, political, and property compliance.
- [ ] Export publication-ready tables and figures to `outputs/panel/`.
- [ ] Write `docs/panel_reproduction.md` explaining what matches the R outputs
  and where Python results differ.

Owner: Iwo. Reviewer: Jan.

## Phase 5: Paper-Level Comparison and Results Documentation

- [ ] Create a master results index in `outputs/README.md`.
- [ ] Compare regenerated Python outputs against the existing R-generated HTML
  tables and plots.
- [ ] Add a table-by-table and figure-by-figure reproduction checklist in
  `docs/reproduction_results.md`.
- [ ] Explain any non-matching results with concrete causes, such as different
  default covariance estimators, missing-value handling, lag construction, or
  package-level implementation differences.
- [ ] Include short interpretation notes so the reviewer can see how the Python
  results relate to the paper's economic argument.
- [ ] Ensure all final tables and figures can be regenerated from a clean clone.

Owners: Jan for integration; Kinga for cross-section notes; Iwo for panel notes.

## Phase 6: Docker and Full Reproducibility

- [ ] Add a `Dockerfile` that installs Python, system dependencies, project
  dependencies, and any packages required for reading `.rds` files.
- [ ] Add `docker-compose.yaml` with a service that runs the complete
  reproduction pipeline.
- [ ] Add `.dockerignore` to keep the build context small and deterministic.
- [ ] Make the default container command run the full reproduction or print a
  clear command for doing so.
- [ ] Verify that a clean Docker build can run `python scripts/run_all.py`
  without local-machine assumptions.
- [ ] Document exact Docker commands in `README.md`, including build, run,
  output location, and troubleshooting.
- [ ] Build and push the final image to Docker Hub.
- [ ] Record the Docker Hub image name and tag in `README.md`.

Owner: Jan. Reviewers: Kinga and Iwo must independently run the documented
Docker command before final submission.

## Phase 7: Clean Code and User-Friendly Interface

- [ ] Format Python code with Black.
- [ ] Lint Python code with Ruff.
- [ ] Keep functions short and explicit where practical.
- [ ] Use clear module boundaries: data loading, transformations, models,
  diagnostics, tables, figures, and orchestration.
- [ ] Add meaningful command-line flags where useful, for example output
  directory, skip-plots, or strict-validation mode.
- [ ] Avoid notebook-only execution; notebooks may be included only as optional
  exploration, not as the only reproducible path.
- [ ] Add minimal tests for critical logic, especially data loading,
  transformations, and model-output smoke checks.

Owners: all contributors for their code; Jan for final consistency pass.

## Phase 8: Documentation Required by `OUTLINE.md`

- [ ] Update `README.md` with project objective, source paper, repository
  structure, quickstart, local run instructions, Docker run instructions, output
  descriptions, and contributor roles.
- [ ] Add `docs/ai_disclosure.md` stating how AI tools were used, which model
  was used, and which parts were human-reviewed.
- [ ] Add source annotations for externally sourced code snippets or formulas.
- [ ] Add `docs/methodology.md` describing the reproduction method in plain
  language.
- [ ] Add `docs/limitations.md` describing reproducibility gaps, numerical
  tolerances, and unresolved differences from the original R outputs.
- [ ] Ensure the project can be understood by a reviewer after cloning the repo,
  without private local paths or hidden setup steps.

Owners: Jan for README and AI disclosure; Kinga for cross-sectional methodology;
Iwo for panel methodology.

## Phase 9: GitHub Contribution Evidence

- [ ] Each contributor makes commits under their own GitHub identity.
- [ ] Commit messages are descriptive and imperative, for example `Add panel
  fixed-effects reproduction`.
- [ ] Use branches or pull requests if practical so review history is visible.
- [ ] Keep final generated outputs, documentation, and code changes organized in
  reviewable commits.
- [ ] Before submission, add a contribution summary table to `README.md` or
  `docs/contributions.md`.
- [ ] Verify that GitHub history visibly supports the contribution split claimed
  in the documentation.

Owners: all contributors.

## Phase 10: Presentation Preparation

- [ ] Prepare a roughly 20-minute presentation covering the research question,
  original paper, data, Python reproduction, Docker workflow, and main results.
- [ ] Include all team members in the speaking plan.
- [ ] Include a short live or recorded code demonstration using the documented
  run command.
- [ ] Show the Docker workflow because the project will be run after pulling
  from GitHub in the lab.
- [ ] Prepare fallback screenshots or pre-rendered outputs in case the lab
  environment has limited time or network issues.

Owners: all contributors. Jan covers reproducibility and Docker; Kinga covers
cross-section; Iwo covers panel.

## Final Submission Checklist

- [ ] Fresh clone works locally with documented commands.
- [ ] Fresh Docker build works.
- [ ] Docker image is pushed to Docker Hub and documented.
- [ ] `python scripts/run_all.py` regenerates all expected outputs.
- [ ] README explains the full workflow.
- [ ] AI disclosure is present.
- [ ] Contribution summary is present.
- [ ] Cross-sectional outputs are regenerated in Python.
- [ ] Panel outputs are regenerated in Python.
- [ ] Reproduction limitations are documented.
- [ ] Presentation materials are ready.

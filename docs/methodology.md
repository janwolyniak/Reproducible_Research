# Methodology

This project reproduces the empirical work in `2400-LIC-FII.pdf` with Python
while using the original R materials as the reference implementation. The goal
is not to redesign the study. The goal is to rebuild the same data-preparation,
modeling, diagnostic, and output steps in a documented Python pipeline that a
reviewer can run from a clean checkout or from Docker.

## Source Materials

The reproduction uses four source groups:

| Source | Role |
| --- | --- |
| `2400-LIC-FII.pdf` | Paper-level research question, reported model families, tables, and substantive interpretation. |
| `cross-section/inorder.R` | Evidence for the cross-sectional workflow, variables, models, diagnostics, and exported R outputs. |
| `panel/inorder_panel.R` | Evidence for the panel workflow, model families, specification tests, diagnostics, and robust covariance choices. |
| `cross-section/data/` and `panel/data_panel/` | Tracked `.rds` and `.dta` inputs used by the Python pipeline. |

The exported R outputs under `cross-section/tables_and_other/` and
`panel/panel_tables/` are treated as reference outputs for table-by-table and
figure-by-figure comparison. The R scripts are treated as workflow evidence
rather than clean end-to-end scripts because they contain exploratory fragments
and unresolved object references.

## Reproduction Contract

The implementation starts from `docs/reproduction_contract.md`. That document
defines:

- The research question and model families.
- The dependent variables, compliance measures, controls, and transformations.
- The R-to-Python artifact map.
- Exact numeric targets and best-effort targets.
- Numerical tolerances for coefficients, descriptive statistics, p-values, and
  plot data.

The pipeline uses those targets as the contract for Python outputs and result
documentation.

## Data Preparation

The shared data-reading layer is in `src/repro_research/data_io.py`. It reads
tracked `.rds`, `.dta`, and `.csv` files using Python dependencies, with schemas
registered in `src/repro_research/paths.py` and component loaders in:

- `src/repro_research/cross_section_data.py`
- `src/repro_research/panel_data.py`

The preparation steps normalize country identifiers, coerce numeric columns,
preserve categorical grouping fields, and build the transformed variables used
by the R workflows. Important examples include shifted compliance logs such as
`log(cc_total + 2)`, the cross-sectional terms-of-trade mapping, panel lags of
GDP per capita, country-year indexes, and panel interaction terms.

Reviewable prepared data snapshots are regenerated under `outputs/intermediate/`
when they help audit the pipeline. Final source inventories and preparation
summaries are written under `docs/` and `outputs/cross_section/`.

## Cross-Sectional Workflow

The cross-sectional implementation lives in
`src/repro_research/cross_section.py` and is run with:

```bash
python scripts/run_cross_section.py
```

It reproduces:

- Descriptive statistics.
- Spearman correlations and grouped Spearman tests.
- Main OLS specifications.
- Outlier-filtered robustness regressions.
- Compliance-variable variants.
- Institutional-variable alternatives.
- Robust standard errors.
- RESET, normality, heteroskedasticity, Durbin-Watson, VIF, leverage,
  standardized residual, and Cook's-distance diagnostics.

Outputs are written to `outputs/cross_section/`.

## Panel Workflow

The panel implementation lives in `src/repro_research/panel.py` and is run with:

```bash
python scripts/run_panel.py
```

It reproduces:

- Panel descriptive statistics.
- Spearman correlations.
- Country-year panel indexing.
- Pooled, between, first-difference, fixed-effects, and random-effects models.
- Reduced and small-panel specification blocks.
- LM, fixed-effects F, and Hausman specification tests.
- Serial-correlation, heteroskedasticity, and cross-sectional-dependence
  diagnostics.
- Driscoll-Kraay, clustered, and related robust covariance variants where
  Python support or local implementation is available.
- Alternative fixed-effects regressions for compliance components.

Outputs are written to `outputs/panel/`.

## Full Pipeline

The single reviewer command is:

```bash
python scripts/run_all.py
```

That command validates the repository, rebuilds prepared data outputs, runs the
cross-sectional and panel reproductions, refreshes generated metadata, writes
`outputs/logs/run_all_summary.txt`, and checks the generated-output manifest.

The manifest can also be checked directly:

```bash
python scripts/validate_project.py --generated-outputs
```

Docker uses the same command as its default container entry point, so the local
and container workflows exercise the same reproduction path.

## Result Interpretation

The Python outputs are interpreted against the original paper and R outputs.
The short version is:

- The cross-sectional reproduction preserves the paper's main null result for
  constitutional-compliance coefficients after controls.
- The panel reproduction preserves the paper's main positive fixed-effects
  relationship, while documenting covariance and diagnostic-test differences
  caused by package-level implementation choices.

Known gaps and package-dependent differences are summarized in
`docs/limitations.md`.

# Panel Reproduction

This document summarises the Python implementation of Phase 4.
The source workflow is `panel/inorder_panel.R`; the tracked RDS
files are treated as authoritative because the R script is an
exploratory notebook-like script rather than a clean end-to-end run.

## Implemented Outputs

- `outputs/panel/descriptive_statistics.csv`
- `outputs/panel/descriptive_statistics.html`
- `outputs/panel/spearman_correlation_matrix.csv`
- `outputs/panel/spearman_correlation_matrix.html`
- `outputs/panel/figures/correlation_plot.png`
- `outputs/panel/panel_models.csv`
- `outputs/panel/panel_models.html`
- `outputs/panel/models_small_full.csv`
- `outputs/panel/models_small_full.html`
- `outputs/panel/models_small_reduced.csv`
- `outputs/panel/models_small_reduced.html`
- `outputs/panel/models_main_reduced.csv`
- `outputs/panel/models_main_reduced.html`
- `outputs/panel/specification_tests.csv`
- `outputs/panel/specification_tests.html`
- `outputs/panel/diagnostic_tests.csv`
- `outputs/panel/diagnostic_tests.html`
- `outputs/panel/fixed_effects_main.csv`
- `outputs/panel/fixed_effects_main.html`
- `outputs/panel/fixed_effects_compliance_categories.csv`
- `outputs/panel/fixed_effects_compliance_categories.html`

## Matching Notes

- Descriptive statistics and Spearman correlations follow the R
  workflow on numeric columns of the alt panel (`model_data2`).
  Plot pixels are best-effort; CSV values are the numerical
  reproduction target.
- Panel models are fit with `linearmodels`: `PooledOLS`,
  `BetweenOLS`, `FirstDifferenceOLS`, `PanelOLS(entity_effects=True)`,
  and `RandomEffects`. Three blocks cover the small-panel full
  specification (2010-2019), the small-panel reduced specification
  (drops `life_exp` and `education`, adds the `tot_growth*trade`
  interaction), and the broad 1990-2020 main panel reduced spec.
- Specification tests reproduce the LM test against pooled OLS
  (computed from pooled residuals using the Breusch-Pagan formula),
  the F test for fixed effects against pooled OLS (from
  `linearmodels.PanelResults.f_pooled`), and the Hausman test
  (using the inverse covariance of the parameter difference).
- Diagnostic tests cover Wooldridge serial correlation (auxiliary
  regression of first-differenced residuals with clustered SE),
  Breusch-Pagan heteroskedasticity on residuals against the fitted
  values, and the Pesaran cross-sectional dependence statistic on
  the residual correlation matrix.
- Robust covariance variants for the main FE specification include
  classical, Arellano (entity-clustered), double-clustered, and
  Driscoll-Kraay standard errors. Driscoll-Kraay is implemented in
  Python following the SCC formulation with the lag truncation rule
  `floor(4 * (T/100)**(2/9))` to match R's `sandwich::vcovSCC`.
- Alternative fixed-effects regressions replicate the R block that
  cycles `cc_basic`, `cc_civil`, `cc_polit`, `cc_prop` through the
  reduced fixed-effects specification, reporting both classical and
  Driscoll-Kraay standard errors.
- Numerical differences from R can arise from `plm` vs
  `linearmodels` finite-sample corrections and degrees-of-freedom
  conventions. The reproduction contract treats these as
  best-effort.

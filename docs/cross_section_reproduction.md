# Cross-Sectional Reproduction

This document summarizes the Python implementation of Phase 3.
The source workflow is `cross-section/inorder.R`; the tracked RDS
files are treated as authoritative because the R script is an
exploratory notebook-like script rather than a clean end-to-end run.

## Implemented Outputs

- `outputs/cross_section/descriptive_statistics.csv`
- `outputs/cross_section/descriptive_statistics.html`
- `outputs/cross_section/spearman_correlation_matrix.csv`
- `outputs/cross_section/spearman_correlation_matrix.html`
- `outputs/cross_section/figures/correlation_plot.png`
- `outputs/cross_section/spearman_growth_vs_cc_total.csv`
- `outputs/cross_section/spearman_growth_vs_cc_total.html`
- `outputs/cross_section/spearman_growth_vs_cc_prop.csv`
- `outputs/cross_section/spearman_growth_vs_cc_prop.html`
- `outputs/cross_section/ols_models.csv`
- `outputs/cross_section/ols_models.html`
- `outputs/cross_section/ols_compliance_variants.csv`
- `outputs/cross_section/ols_compliance_variants.html`
- `outputs/cross_section/ols_full_sample.csv`
- `outputs/cross_section/ols_full_sample.html`
- `outputs/cross_section/ols_main.csv`
- `outputs/cross_section/ols_main.html`
- `outputs/cross_section/ols_outlier_filtered.csv`
- `outputs/cross_section/ols_outlier_filtered.html`
- `outputs/cross_section/ols_institutional_variants.csv`
- `outputs/cross_section/ols_institutional_variants.html`
- `outputs/cross_section/diagnostics.csv`
- `outputs/cross_section/diagnostics.html`
- `outputs/cross_section/variance_inflation_factors.csv`
- `outputs/cross_section/variance_inflation_factors.html`
- `outputs/cross_section/re4_o_influence_diagnostics.csv`
- `outputs/cross_section/re4_o_influence_diagnostics.html`
- `outputs/cross_section/figures/re4_o_residuals_vs_fitted.png`
- `outputs/cross_section/figures/re4_o_residual_qq.png`
- `outputs/cross_section/figures/re4_o_cooks_distance.png`

## Matching Notes

- Descriptive statistics follow the R workflow by summarising numeric
  columns from `model_data2` and dropping the first reshaped row before
  rendering.
- The correlation matrix uses Spearman correlations on the same selected
  variable set. Plot pixels are best-effort; the CSV matrix is the
  numerical reproduction target.
- OLS tables use `statsmodels` OLS with HC3 robust standard errors,
  matching the R workflow's `sandwich::vcovHC(..., type = "HC3")`
  publication-table intent.
- `model_data4` has no `tot2` column in the tracked file, so the Python
  loader uses the canonical `terms_trade` fallback documented in Phase 2.
  The outlier-filtered models use `model_data4_o`, where `tot2` is
  present and mapped to `terms_trade`.
- Diagnostic p-values may differ at tiny finite-sample levels because
  Python and R expose slightly different defaults for some tests.

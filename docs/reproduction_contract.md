# Reproduction Contract

This contract defines the Python reproduction target before any model code is
written. The reference implementation is the thesis `2400-LIC-FII.pdf`, with
the existing R workflows and exported outputs used as executable evidence where
they are internally consistent.

## Source Summary

The reproduced paper is Kinga Kucharska, "Constitutional Compliance: The Hidden
Driver of Economic Growth?", University of Warsaw bachelor thesis, May 2025.
The empirical research question is:

> What is the impact of constitutional compliance on economic growth, measured
> by GDP growth?

The thesis hypothesis is that stronger compliance with constitutional rights
should have a positive effect on GDP growth. The paper tests this with two
empirical designs:

- A cross-sectional model for the 2010-2019 period.
- A panel model covering 1990-2020.

The thesis finds that cross-sectional OLS does not show a statistically
significant constitutional-compliance effect, while fixed-effects panel models
with Driscoll-Kraay robust standard errors show a positive and significant
relationship.

## Core Variables

### Dependent Variables

| Context | Variable | Meaning |
| --- | --- | --- |
| Cross-section | `GDP_growth` | GDP growth, annual percent, averaged or prepared for 2010-2019 |
| Panel | `GDPgrowth` | GDP growth, annual percent, country-year panel |

### Constitutional Compliance Variables

The main explanatory variable is the aggregate constitutional compliance index:

- `cc_total`, transformed in models as `log(cc_total + 2)`.

The reproduction must also cover the alternative compliance measures used in
the thesis and R scripts:

- `cc_total_lv`, Law and Versteeg-style aggregate, transformed as
  `log(cc_total_lv + 2)`.
- `cc_prop`, property rights, judicial independence, equality before the law,
  and rule of law.
- `cc_basic`, right to life, freedom from slavery, and protection from torture.
- `cc_civil`, free media, free speech, free movement, and religious freedom.
- `cc_polit`, freedom of association, freedom of assembly, and right to form
  parties.

The `+ 2` shift is part of the contract because the compliance indices can be
negative and the thesis explicitly applies logarithms after this shift.

### Controls

The controls to reproduce are:

- `fertility`
- `GDPpc2015`, or `lag(GDPpc2015, 2)` in panel models
- `gov_exp_reduced`
- `inf_def` in the cross-sectional workflow
- `inflation` in the panel workflow
- `investment`
- `life_exp`
- `education`
- `tot` or `tot2` in cross-sectional data, mapped to terms-of-trade growth
- `tot_growth` in panel data
- `trade`
- `tot2:trade` or `tot_growth:trade`
- `v2x_libdem`
- `landlocked`
- institutional alternatives: `rule_of_law`, `ctrl_of_corr`, `gov_eff`,
  `pol_stab`, `reg_q`, and `voice_and_acc`

The thesis removes `education`, `life_exp`, and `v2x_libdem` from the preferred
cross-sectional model because of missingness and multicollinearity. It removes
`education` and `life_exp` from the preferred panel model for the same practical
reasons.

## Model Families

### Cross-Sectional Reproduction

The Python cross-sectional reproduction must implement:

1. Descriptive statistics for numeric variables in the 2010-2019 data.
2. Spearman correlations and correlation plots for the selected variables.
3. OLS model 0 with the broad covariate set.
4. Preferred OLS models around:
   - `log(cc_total + 2)`
   - `log(cc_total_lv + 2)`
   - `log(cc_prop + 2)`
5. Robust standard errors for the publication tables, matching the R workflow's
   use of heteroskedasticity-consistent covariance where possible.
6. Outlier-filtered regressions based on the `model_data4_o` dataset.
7. Institutional-variable alternatives replacing constitutional compliance with
   World Bank governance indicators.
8. Diagnostics:
   - RESET
   - variance inflation factors
   - residual normality checks
   - Breusch-Pagan heteroskedasticity tests
   - Durbin-Watson checks
   - leverage, standardized residuals, and Cook's distance.

The preferred cross-sectional publication target is the set of tables matching
paper Table 3 and Appendix Tables A1-A5.

### Panel Reproduction

The Python panel reproduction must implement:

1. Panel descriptive statistics for 1990-2020.
2. Spearman correlations and correlation plot for panel variables.
3. Country-year panel indexing by `Country` and `Year`.
4. Pooled OLS, between, first-difference, fixed-effects, and random-effects
   models for the main specification.
5. Specification tests:
   - Lagrange multiplier test against pooled OLS.
   - F test for fixed effects against pooled OLS.
   - Hausman test for fixed versus random effects.
6. Diagnostics:
   - Wooldridge or equivalent serial-correlation test.
   - Breusch-Pagan heteroskedasticity test.
   - Pesaran CD cross-sectional dependence test.
7. Robust covariance variants where Python support is available:
   - Driscoll-Kraay as the main publication target.
   - Arellano-style and clustered covariance variants as diagnostic or
     robustness outputs.
8. Alternative fixed-effects regressions for `cc_basic`, `cc_civil`,
   `cc_polit`, and `cc_prop`.

The preferred panel publication target is paper Tables 4 and 5 and Appendix
Tables A6-A8.

## Artifact Map

| Current artifact | Role in audit | Python replacement target |
| --- | --- | --- |
| `2400-LIC-FII.pdf` | Paper and final reference tables | `docs/reproduction_contract.md`, later `docs/reproduction_results.md` |
| `cross-section/inorder.R` | Cross-sectional R workflow | `src/repro_research/cross_section.py` and `scripts/run_cross_section.py` |
| `panel/inorder_panel.R` | Panel R workflow | `src/repro_research/panel.py` and `scripts/run_panel.py` |
| `helpers/rds_to_csv.py` | Existing RDS conversion utility | Keep as shared helper; use directly or wrap from pipeline |
| `cross-section/data/*.rds` | Cross-sectional inputs and derived datasets | `data` loading module, optional converted CSV cache |
| `panel/data_panel/*.rds` | Panel inputs and derived datasets | `data` loading module, optional converted CSV cache |
| `cross-section/tables_and_other/descriptive stat.html` | Cross-sectional descriptive statistics | `outputs/cross_section/descriptive_statistics.csv` and `.html` |
| `cross-section/tables_and_other/Corrplot.png` | Cross-sectional correlation plot | `outputs/cross_section/correlation_plot.png` |
| `cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs total.html` | Cross-sectional grouped Spearman tests for `cc_total` | `outputs/cross_section/spearman_growth_vs_cc_total.csv` and `.html` |
| `cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs prop.html` | Cross-sectional grouped Spearman tests for `cc_prop` | `outputs/cross_section/spearman_growth_vs_cc_prop.csv` and `.html` |
| `cross-section/tables_and_other/reg_table1.html` | Main OLS models 1 and 2 | `outputs/cross_section/ols_main.csv` and `.html` |
| `cross-section/tables_and_other/reg_table2&3.html` | OLS models with compliance variants | `outputs/cross_section/ols_compliance_variants.csv` and `.html` |
| `cross-section/tables_and_other/reg_table_lv&prop.html` | OLS `cc_total_lv` and `cc_prop` subset | Covered by `ols_compliance_variants` |
| `cross-section/tables_and_other/reg_table_inst.html` | Institutional-variable alternatives | `outputs/cross_section/ols_institutional_variants.csv` and `.html` |
| `cross-section/tables_and_other/diagnostics.html` | Cross-sectional diagnostics | `outputs/cross_section/diagnostics.csv` and `.html` |
| `panel/panel_tables/descriptive.html` | Panel descriptive statistics | `outputs/panel/descriptive_statistics.csv` and `.html` |
| `panel/panel_tables/Corrplot.png` | Panel correlation plot | `outputs/panel/correlation_plot.png` |
| `panel/panel_tables/Specification tests.html` | LM, F, and Hausman tests | `outputs/panel/specification_tests.csv` and `.html` |
| `panel/panel_tables/Diagnostic tests.html` | Serial correlation, heteroskedasticity, and CD tests | `outputs/panel/diagnostic_tests.csv` and `.html` |
| `panel/panel_tables/model_output.html` | Main fixed-effects panel table | `outputs/panel/fixed_effects_main.csv` and `.html` |
| `panel/panel_tables/model_output2.html` | Compliance-category fixed-effects table | `outputs/panel/fixed_effects_compliance_categories.csv` and `.html` |
| `panel/panel_tables/Rplot*.pdf` | Panel exploratory plots | `outputs/panel/figures/` best-effort regenerated plots |

## Exact Targets and Best-Effort Targets

### Exact Reproduction Targets

These outputs should match the R reference numerically, subject to the tolerances
below:

- Dataset row and column counts after each canonical loading step.
- Descriptive-statistic values.
- Spearman rho coefficients and p-values.
- OLS and panel coefficient estimates.
- Standard errors where the same covariance estimator is available.
- Model sample sizes, R-squared values, adjusted R-squared values, residual
  standard errors, and F statistics where the Python model exposes equivalent
  definitions.
- Diagnostic and specification test p-values where the same test definition is
  available.

### Best-Effort Approximation Targets

These outputs are allowed to differ while preserving the same substantive
information:

- HTML styling previously produced by `stargazer` and `kableExtra`.
- Correlation plot ordering, color palettes, and exact text rendering.
- Residual, leverage, and Cook's-distance plots from R plotting libraries.
- Panel robust covariance estimators when Python and R use different finite
  sample corrections or degrees-of-freedom conventions.
- Specification tests where R's `plm` implementation has no exact Python
  equivalent.

## Numerical Tolerances

Use these tolerances for automated comparisons unless a later phase documents a
specific exception:

| Output type | Tolerance |
| --- | --- |
| Row counts and included observations | Exact match |
| Coefficients, descriptive statistics, rho values | Absolute tolerance `1e-6` before presentation rounding |
| Standard errors and p-values from equivalent estimators | Absolute tolerance `1e-5` before presentation rounding |
| Published table values rounded to 3 decimals | Must match displayed rounded value |
| Published table values rounded to 4 decimals | Must match displayed rounded value |
| Plot data series | Underlying data must match; rendered pixels are not exact targets |

If exact matching fails because of a known package convention, document the
package, estimator, and observed difference in `docs/reproduction_results.md`.

## Known Limitations and Audit Findings

1. The R scripts are not clean end-to-end reproduction scripts. They include
   section labels as bare text, exploratory commands, and unrelated example code.
2. Some R objects are referenced without being created in the script, including
   `model_data`, `model_data2`, `model_data4`, `model_data4_o`, `small_plm`,
   `small_pdata`, `small_pdata.frame`, `model`, `re4_olv`, and related objects.
   The `.rds` files are therefore treated as the current data source of record.
3. Variable names are inconsistent across artifacts, but the Phase 0 audit now
   inventories the variants that loaders must normalize:
   - `GDP_growth` in cross-sectional data versus `GDPgrowth` in panel data.
   - `tot`, `tot2`, and `tot_growth` across paper, R scripts, and data files.
   - `Country Code`, `Country_Code`, `Country_Name`, `Country`, and `cname`.
   - `country_name`, `country_text_id`, `iso3`, and `iso3c` in source extracts.
   - `gov_exp_milit` versus `gov_exp_mil`.
4. The cross-sectional paper text says five outliers were removed and refers to
   a final sample of 157 countries, but the main reported OLS tables use 121-122
   observations after model-wise missing-value handling. The Phase 0 audit now
   verifies both the five-country influence rule and the model-wise missingness
   counts from tracked files.
5. The panel script contains likely copy-paste issues in diagnostics, including
   tests run on an undefined `model` until it is later assigned as `fixed`.
   Panel diagnostics and robust-covariance checks are therefore a Phase 4 model
   implementation requirement, not a Phase 0 data-audit blocker. The Python
   reproduction must document any finite-sample correction differences from
   R `plm`.
6. Full upstream raw/bulk-download provenance is intentionally out of scope for
   this reproduction. The audit treats tracked source-level extracts in
   `cross-section/data/` as authoritative: CCCD averages/details/growth files,
   V-Dem democracy averages, World Bank-style variable extracts, CEPII geography
   `geo_cepii.dta`, and grouping data in `oth_char.rds`.
7. The PDF is the source for the research question, variable definitions, and
   reported target tables. The R exported HTML files are the source for exact
   machine-readable table values where parsing is easier than PDF extraction.

## Phase Acceptance Criteria

Phase 0 is complete when:

- This contract exists and is committed.
- `TODO.md` includes a visible audit issue section for missing data, ambiguous
  variables, and undocumented transformations.
- Later implementation phases can refer to this document for model scope,
  output scope, and numerical comparison rules.

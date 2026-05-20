# Reproduction Results

This is the Phase 5 master index for the Python reproduction. It stitches the
cross-sectional and panel comparison documents into one reviewer-facing
checklist and records which regenerated outputs match the original R artifacts.

Detailed sub-documents:

- `docs/cross_section_reproduction_results.md`
- `docs/panel_reproduction_results.md`

Regenerate the Python outputs and validate the final artifact set with:

```bash
python scripts/run_all.py
python scripts/validate_project.py --generated-outputs
```

## Status Levels

- **match**: rounded numerical values agree with the R reference.
- **partial**: the same statistical target is reproduced, but sample,
  covariance-estimator, or rendering conventions differ for a documented cause.
- **divergent**: the Python artifact is a documented best-effort substitute for
  an R procedure without an exact Python equivalent.
- **best-effort**: exact plot pixels or HTML styling are intentionally outside
  the reproduction contract.

## Master Artifact Checklist

| Section | R reference | Python artifact | Status | Notes |
| --- | --- | --- | :---: | --- |
| Cross-section | `cross-section/tables_and_other/descriptive stat.html` | `outputs/cross_section/descriptive_statistics.{csv,html}` | partial | Python follows the `model_data2` code path in `cross-section/inorder.R`; the committed R HTML appears to use the outlier/grouped sample. |
| Cross-section | `cross-section/tables_and_other/Corrplot.png` | `outputs/cross_section/spearman_correlation_matrix.{csv,html}` and `outputs/cross_section/figures/correlation_plot.png` | best-effort | Numeric Spearman correlations are regenerated; R `corrplot` glyphs and palette are not pixel targets. |
| Cross-section | `Spearman's rho Correlation Test Results growth vs total.html` | `outputs/cross_section/spearman_growth_vs_cc_total.{csv,html}` | match | All-country, rich/poor, continent, legal-origin, and colonization rows reproduce after rounding. |
| Cross-section | `Spearman's rho Correlation Test Results growth vs prop.html` | `outputs/cross_section/spearman_growth_vs_cc_prop.{csv,html}` | match | Same grouping rule as the `cc_total` table; rounded rho and p-values match. |
| Cross-section | `cross-section/tables_and_other/reg_table1.html` | `outputs/cross_section/ols_main.{csv,html}` | match | Coefficients, HC3 standard errors, observations, R-squared, and adjusted R-squared match after rounding. |
| Cross-section | `cross-section/tables_and_other/reg_table2&3.html` | `outputs/cross_section/ols_main.{csv,html}` and `outputs/cross_section/ols_compliance_variants.{csv,html}` | match | The R columns map to Python `re2_o`, `re4_o`, `re4_olv`, and `re4_oprop`. |
| Cross-section | `cross-section/tables_and_other/reg_table_lv&prop.html` | `outputs/cross_section/ols_compliance_variants.{csv,html}` | match | Alternative compliance measures match after rounding. |
| Cross-section | `cross-section/tables_and_other/reg_table_inst.html` | `outputs/cross_section/ols_institutional_variants.{csv,html}` | match | Institutional alternatives match signs, rounded coefficients, and rounded HC3 standard errors. |
| Cross-section | `cross-section/tables_and_other/diagnostics.html` | `outputs/cross_section/diagnostics.{csv,html}` | match | RESET, Shapiro-Wilk, Breusch-Pagan, and Durbin-Watson p-values match to four decimals for the reported models. |
| Cross-section | R diagnostic plots | `outputs/cross_section/figures/re4_o_*.png` and `outputs/cross_section/re4_o_influence_diagnostics.{csv,html}` | best-effort | Plot pixels are not exact targets; leverage, standardized residuals, and Cook's distance are exposed as regenerated data. |
| Panel | `panel/panel_tables/descriptive.html` | `outputs/panel/descriptive_statistics.{csv,html}` | match | Mean, median, min, max, and SD agree to four decimals for the paper variables. |
| Panel | `panel/panel_tables/Corrplot.png` | `outputs/panel/spearman_correlation_matrix.{csv,html}` and `outputs/panel/figures/correlation_plot.png` | partial | Spearman rho values are regenerated; rendering uses Seaborn instead of R `corrplot`. |
| Panel | `panel/panel_tables/model_output.html` | `outputs/panel/fixed_effects_main.{csv,html}` | partial | Coefficients, sample size, R-squared, sign, and significance pattern match; Driscoll-Kraay standard errors differ by implementation convention. |
| Panel | `panel/panel_tables/model_output2.html` | `outputs/panel/fixed_effects_compliance_categories.{csv,html}` | partial | All four compliance-component signs and significance verdicts match; finite-sample covariance conventions explain standard-error differences. |
| Panel | `panel/panel_tables/Specification tests.html` | `outputs/panel/specification_tests.{csv,html}` | match | LM, F, and Hausman p-values round to zero in both R and Python. |
| Panel | `panel/panel_tables/Diagnostic tests.html` | `outputs/panel/diagnostic_tests.{csv,html}` | divergent | Serial correlation uses a Breusch-Godfrey panel proxy because R `plm::pbgtest` has no exact Python replacement; BP and Pesaran CD conclusions match. |
| Panel | `panel/panel_tables/Rplot*.pdf` | `outputs/panel/figures/correlation_plot.png` and regenerated model tables | best-effort | Residual/influence plot pixels are outside the contract; numeric model outputs are the reproducibility target. |

## Cross-Sectional Result Summary

The cross-sectional reproduction supports the paper's null result for
constitutional compliance in 2010-2019 decade averages. In the preferred
outlier-filtered model, `log(cc_total + 2)` is negative and statistically
insignificant (`-0.144`, HC3 SE `0.305`, `n = 122`). The `cc_total_lv` and
`cc_prop` variants are also insignificant.

The control-variable pattern is stable: initial GDP per capita is negative and
significant, investment is positive and significant, and reduced government
expenditure is negative and significant. Institutional alternatives such as
rule of law, control of corruption, government effectiveness, and political
stability are positive and significant in the same model family.

## Panel Result Summary

The panel reproduction supports the paper's positive within-country result for
constitutional compliance. In the 1990-2020 fixed-effects model with the
`tot_growth * trade` interaction, Python estimates `log_cc_total = 1.741`
against the R estimate of `1.854`; observations (`2,233`), within R-squared
(`0.257`), sign, and significance verdict match.

The alternative compliance-component table reproduces the R pattern: basic,
civil, and property compliance are positive and significant, while political
rights are positive but not significant. The main differences are
standard-error conventions for robust covariance estimators and the documented
serial-correlation proxy.

## Non-Matching Results

The remaining non-matches have concrete causes:

- Cross-sectional descriptive statistics: the R script describes a
  `model_data2` summary, while the committed R HTML appears to use an
  outlier/grouped `model_data4.1`-style sample.
- Correlation plots: R uses `corrplot`; Python uses Seaborn. Numeric
  correlation matrices are the target.
- Panel Driscoll-Kraay standard errors: Python uses `linearmodels` plus a local
  SCC-style calculation with the documented lag rule, while the R reference
  uses `sandwich::vcovSCC`.
- Panel serial-correlation diagnostics: Python labels the substitute as a
  Breusch-Godfrey panel proxy because the R `plm::pbgtest` auxiliary regression
  is not exactly replicated.

## Clean-Clone Regeneration Contract

A reviewer starting from a clean clone should be able to run:

```bash
python -m pip install -r requirements.txt
python scripts/validate_project.py --create-output-dirs
python scripts/run_all.py
python scripts/validate_project.py --generated-outputs
```

`scripts/run_all.py` now runs the full Python pipeline and then checks that the
expected final artifact manifest exists: regenerated Phase 2-4 tables, figures,
intermediate CSVs, generated documentation, the Phase 5 comparison documents,
the output index, and the run summary. `scripts/validate_project.py
--generated-outputs` exposes the same check as a standalone command.

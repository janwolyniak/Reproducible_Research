# Cross-Sectional Reproduction Results

Phase 5 cross-section-side checklist. Each row pairs one R reference artifact
under `cross-section/tables_and_other/` with its regenerated Python artifact
under `outputs/cross_section/`.

Status levels:

- **match**: rounded numerical values agree with the R reference.
- **partial**: the same statistical target is reproduced, but the input sample,
  grouping rule, or rendered plot differs for a documented reason.
- **best-effort**: exact rendered styling or plot pixels are not reproduction
  targets under `docs/reproduction_contract.md`.

## Artifact Checklist

| R reference | Python artifact | Status | Notes |
| --- | --- | :---: | --- |
| `descriptive stat.html` | `descriptive_statistics.{csv,html}` | partial | The R script computes descriptive statistics from `model_data2`, which is what Python implements. The exported R HTML appears to have been produced from the outlier/grouped `model_data4.1`-style data because it includes `tot2`, `continent`-joined institutional variables, and 157-country counts. |
| `Corrplot.png` | `spearman_correlation_matrix.{csv,html}` and `figures/correlation_plot.png` | best-effort | Spearman rho values are regenerated for the same core variable set; R `corrplot` ordering, circle glyphs, and palette are not exact pixel targets. |
| `Spearman's rho Correlation Test Results growth vs total.html` | `spearman_growth_vs_cc_total.{csv,html}` | match | The all-country, rich/poor, continent, legal-origin, and colonization rows reproduce the R table. `rich` and `poor` are the two GDP-per-capita groups split at the sample median of `GDPpc`. |
| `Spearman's rho Correlation Test Results growth vs prop.html` | `spearman_growth_vs_cc_prop.{csv,html}` | match | Same grouping as the `cc_total` table; rounded rho and p-value cells match the R export. |
| `reg_table1.html` | `ols_main.{csv,html}` | match | Coefficients, HC3 robust standard errors, observations, R-squared, and adjusted R-squared match the R table after rounding. |
| `reg_table2&3.html` | `ols_main.{csv,html}` plus `ols_compliance_variants.{csv,html}` | match | The four R columns map to Python `re2_o`, `re4_o`, `re4_olv`, and `re4_oprop`; rounded coefficient and robust-SE cells match. |
| `reg_table_lv&prop.html` | `ols_compliance_variants.{csv,html}` | match | `cc_total_lv` and `cc_prop` variants match the R table after rounding. |
| `reg_table_inst.html` | `ols_institutional_variants.{csv,html}` | match | Institutional alternatives for rule of law, control of corruption, government effectiveness, political stability, regulatory quality, and voice/accountability match after rounding. |
| `diagnostics.html` | `diagnostics.{csv,html}` | match | RESET, Shapiro-Wilk, Breusch-Pagan, and Durbin-Watson p-values match the R table to four decimals for the ten reported models. Python additionally reports Jarque-Bera and Breusch-Godfrey lag-1 diagnostics. |

## Main OLS Tables

The table below checks the key publication targets from `reg_table1.html` and
`reg_table2&3.html`. R values are shown as printed in the exported HTML; Python
values are from the HC3 robust columns in the regenerated CSV files.

| Model | Term | R coefficient | R HC3 SE | Python coefficient | Python HC3 SE | Status |
| --- | --- | ---: | ---: | ---: | ---: | :---: |
| `re2_o` | `log(cc_total + 2)` | -0.138 | 0.318 | -0.138 | 0.318 | match |
| `re2_o` | `log(GDPpc2015)` | -0.537*** | 0.167 | -0.537*** | 0.167 | match |
| `re2_o` | `investment` | 0.099*** | 0.033 | 0.099*** | 0.033 | match |
| `re2_o` | `gov_exp_reduced` | -0.115** | 0.046 | -0.115** | 0.046 | match |
| `re4_o` | `log(cc_total + 2)` | -0.144 | 0.305 | -0.144 | 0.305 | match |
| `re4_o` | `log(GDPpc2015)` | -0.574*** | 0.117 | -0.574*** | 0.117 | match |
| `re4_olv` | `log(cc_total_lv + 2)` | -0.101 | 0.458 | -0.101 | 0.458 | match |
| `re4_oprop` | `log(cc_prop + 2)` | -0.017 | 0.390 | -0.017 | 0.390 | match |

Sample sizes also match: `re2_o`, `re4_o`, and `re4_oprop` use 122 complete
observations; `re4_olv` uses 121 because `cc_total_lv` has one missing value.

## Institutional Alternatives

The institutional replacement regressions reproduce the R sign and rounded
inference pattern exactly.

| Model | Institutional variable | R coefficient | R HC3 SE | Python coefficient | Python HC3 SE | Verdict |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| `inst1` | `rule_of_law` | 0.792** | 0.309 | 0.792** | 0.309 | positive, significant |
| `inst2` | `ctrl_of_corr` | 0.835*** | 0.236 | 0.835*** | 0.236 | positive, significant |
| `inst3` | `gov_eff` | 1.140*** | 0.338 | 1.140*** | 0.338 | positive, significant |
| `inst4` | `pol_stab` | 0.765** | 0.302 | 0.765** | 0.302 | positive, significant |
| `inst5` | `reg_q` | 0.627* | 0.376 | 0.627* | 0.376 | positive, weakly significant |
| `inst6` | `voice_and_acc` | 0.111 | 0.264 | 0.111 | 0.264 | not significant |

These rows are important for the paper-level interpretation: other institutional
quality measures are more consistently positive in the cross-section than the
constitutional-compliance index itself.

## Diagnostics

The diagnostics table is an exact rounded reproduction of
`cross-section/tables_and_other/diagnostics.html`.

| Model | R RESET | Py RESET | R Shapiro | Py Shapiro | R BP | Py BP | R DW | Py DW p-value |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `re2_o` | 0.9070 | 0.9070 | 0.1664 | 0.1664 | 0.0015 | 0.0015 | 0.4457 | 0.4457 |
| `re4_o` | 0.9874 | 0.9874 | 0.1748 | 0.1748 | 0.0125 | 0.0125 | 0.4895 | 0.4895 |
| `re4_olv` | 0.9494 | 0.9494 | 0.2403 | 0.2403 | 0.0087 | 0.0087 | 0.4902 | 0.4902 |
| `re4_oprop` | 0.8615 | 0.8615 | 0.2635 | 0.2635 | 0.0027 | 0.0027 | 0.5000 | 0.5000 |
| `inst1` | 0.9665 | 0.9665 | 0.2344 | 0.2344 | 0.0003 | 0.0003 | 0.3961 | 0.3961 |
| `inst2` | 0.8047 | 0.8047 | 0.1771 | 0.1771 | 0.0163 | 0.0163 | 0.3416 | 0.3416 |
| `inst3` | 0.6075 | 0.6075 | 0.3365 | 0.3365 | 0.0152 | 0.0152 | 0.3889 | 0.3889 |
| `inst4` | 0.9017 | 0.9017 | 0.3857 | 0.3857 | 0.0046 | 0.0046 | 0.4770 | 0.4770 |

Python also exports influence diagnostics for the preferred model in
`re4_o_influence_diagnostics.{csv,html}` and plots for residuals, Q-Q residual
normality, and Cook's distance. These are best-effort replacements for R's
interactive diagnostic plots and expose the underlying leverage, standardized
residual, and Cook's-distance values.

## Non-Matching Items And Causes

1. **Descriptive statistics use a different exported sample.** The code block in
   `cross-section/inorder.R` explicitly builds `descriptive_stat` from
   `model_data2`, but the committed R HTML includes variables that are only in
   the outlier-filtered institutional/grouping data, such as `tot2`,
   `continent`-joined institutional fields, and `ever_colonized`. Python follows
   the script code path for `descriptive_statistics.csv`; reviewers who need the
   committed R HTML sample can recompute the same summary from `model_data4.1`.
2. **Correlation plot pixels differ by design.** R uses `corrplot`; Python uses
   Seaborn. The numeric Spearman matrix is the reproduction target.

## Rich/Poor Split

The `rich` and `poor` rows in the Spearman tables use the split described in the
initial R files: countries above or at the median `GDPpc` are assigned to
`rich`, and countries below the median are assigned to `poor`. With that rule,
Python reproduces the R-exported rows after rounding:

| Variable pair | Group | R rho | Python rho | R two-sided p | Python two-sided p |
| --- | --- | ---: | ---: | ---: | ---: |
| `GDP_growth` vs `cc_total` | rich | -0.1722 | -0.1722 | 0.1316 | 0.1316 |
| `GDP_growth` vs `cc_total` | poor | -0.1277 | -0.1277 | 0.2646 | 0.2646 |
| `GDP_growth` vs `cc_prop` | rich | -0.2149 | -0.2149 | 0.0589 | 0.0589 |
| `GDP_growth` vs `cc_prop` | poor | -0.0472 | -0.0472 | 0.6815 | 0.6815 |

## Interpretation Notes

The cross-sectional reproduction supports the paper's reported null result for
constitutional compliance in decade averages. In the preferred outlier-filtered
model, `log(cc_total + 2)` is negative and not statistically significant
(`-0.144`, HC3 SE `0.305`, `n = 122`). The alternative compliance measures are
also insignificant: `log(cc_total_lv + 2) = -0.101` and
`log(cc_prop + 2) = -0.017`.

The control variables behave more consistently. Initial GDP per capita is
negative and significant, investment is positive and significant, and reduced
government expenditure is negative and significant. This is the standard
conditional-convergence pattern rather than evidence that constitutional
compliance explains cross-country growth differences in this averaged sample.

The institutional alternatives add useful context. Rule of law, control of
corruption, government effectiveness, and political stability are positive and
significant in the same cross-sectional specification. That means the
cross-section is capable of detecting institutional-growth relationships, but
the paper's constitutional-compliance measure does not carry that signal in the
2010-2019 averaged design.

## Regeneration

These cross-sectional results are regenerated by:

```bash
python scripts/run_all.py
```

`run_all` rebuilds the Phase 2 cross-sectional preparation artifacts, all
Phase 3 tables and figures, and this document's referenced CSV/HTML outputs.

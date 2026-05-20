# Source Annotations

This project does not copy externally sourced code snippets verbatim. The code
was written for this repository, using the original thesis, original R
workflows, Python package APIs, and standard econometric formulas as references.

## Project Sources

| Source | How it is used |
| --- | --- |
| `2400-LIC-FII.pdf` | Research question, reported empirical designs, variables, expected tables, and substantive interpretation. |
| `cross-section/inorder.R` | Cross-sectional workflow evidence: prepared datasets, transformations, OLS families, diagnostics, and exported reference outputs. |
| `panel/inorder_panel.R` | Panel workflow evidence: panel transformations, model families, specification tests, diagnostics, and robust covariance targets. |
| `cross-section/tables_and_other/` | Reference cross-sectional tables and figures for comparison. |
| `panel/panel_tables/` | Reference panel tables and figures for comparison. |

## Formulas and Statistical Procedures

The following formulas and procedures are standard methods implemented through
Python scientific libraries or local code:

| Procedure | Python implementation | Annotation |
| --- | --- | --- |
| OLS and robust covariance estimates | `statsmodels` | Used for cross-sectional regression tables and diagnostics. |
| RESET, Breusch-Pagan, Durbin-Watson, Jarque-Bera, Omnibus, and VIF diagnostics | `statsmodels` | Standard diagnostic procedures exposed by `statsmodels`; no package examples were copied. |
| Spearman correlations and tests | `scipy.stats` and pandas correlation methods | Used to reproduce the R correlation outputs numerically. |
| Panel models | `linearmodels` | Used for pooled, between, first-difference, fixed-effects, and random-effects specifications. |
| Hausman, LM, fixed-effects F, Pesaran CD, and serial-correlation proxy tests | Local code plus model outputs | Implemented from standard test definitions and documented as exact or approximate targets in the result notes. |
| Driscoll-Kraay-style robust covariance | `linearmodels` support and local covariance logic | Used as the Python counterpart to the R robust-covariance workflow, with finite-sample caveats documented in `docs/limitations.md`. |

## External Code Reuse Statement

No external code snippets, Stack Overflow answers, package documentation
examples, or generated boilerplate from third-party templates were copied into
the project. When package APIs are used, the implementation calls the package
interfaces directly and records package-dependent behavior in
`docs/reproduction_results.md` and `docs/limitations.md`.

If future edits adapt external snippets, add a row here with the source URL,
the adapted file, and the scope of the adaptation before submission.

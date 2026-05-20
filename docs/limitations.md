# Limitations

This document records the reproducibility gaps, numerical-tolerance policy, and
unresolved differences from the original R outputs. The detailed
table-by-table status is in `docs/reproduction_results.md`.

## Scope Limits

- The project reproduces the tracked thesis, R workflows, processed `.rds` and
  `.dta` inputs, and exported R outputs already present in this repository. It
  does not redownload or reconstruct every upstream raw data source.
- The R scripts are treated as workflow evidence, not as clean executable
  scripts. They contain exploratory fragments, bare section labels, and some
  objects that are not created inside the scripts.
- HTML styling and exact plot pixels are best-effort targets. Numerical table
  values and plot source data are the reproducibility targets.

## Numerical Tolerances

The reproduction contract uses these tolerances unless a component comparison
document records a specific exception:

| Output type | Target |
| --- | --- |
| Row counts and included observations | Exact match. |
| Coefficients, descriptive statistics, and correlation coefficients | Absolute tolerance `1e-6` before presentation rounding. |
| Standard errors and p-values from equivalent estimators | Absolute tolerance `1e-5` before presentation rounding. |
| Published rounded tables | Match the displayed rounded value when the same estimator and sample are available. |
| Figures | Match underlying data; exact pixels are not required. |

## Cross-Sectional Differences

- The Python descriptive-statistics output can differ from the exported R table
  when the R table uses a different prepared sample than the final model data.
  The comparison documents record the affected rows.
- Cross-sectional correlation plots use Python plotting defaults rather than
  R `corrplot` styling. The source correlation matrix is the comparison target.
- The `model_data4.rds` input lacks the `tot2` column while
  `model_data4_o.rds` contains it. The Python loader exposes a canonical
  `terms_trade` variable, using `tot2` when available and falling back to `tot`
  otherwise. This decision is documented in the cross-sectional reproduction
  notes.
- The outlier-filtered workflow follows the tracked `model_data4_o.rds`
  dataset and the confirmed five-country exclusion rule: Congo, Rep.; Lesotho;
  Luxembourg; Malta; and Singapore.

## Panel Differences

- Driscoll-Kraay standard errors are implemented in Python with available
  `linearmodels` support and local covariance logic where needed. Finite-sample
  corrections can differ from R `sandwich::vcovSCC`, so the coefficient targets
  are stricter than robust-standard-error targets.
- The R workflow's `plm::pbgtest` target is approximated with a documented
  Breusch-Godfrey-style panel proxy in Python. This is a method substitution,
  not an exact same-test reproduction.
- Some fixed-effects specifications absorb variables after country and year
  effects are included. The Python implementation keeps those warnings visible
  in tests and result notes rather than hiding the model-identification issue.
- Cross-sectional dependence diagnostics use a Python implementation of the
  Pesaran CD logic and can differ in small-sample details from R packages.

## Environment Limits

- The supported Python baseline is Python 3.11 with the constrained dependency
  set in `requirements.txt` and `pyproject.toml`.
- The Docker image is the preferred reviewer environment because it pins the
  operating-system-level setup needed for scientific Python and `.rds` reading.
- Local runs require the tracked data files to remain in their current
  repository paths. The pipeline intentionally avoids private absolute paths.

## Remaining Reviewer Caveats

- The generated outputs are reproducible from the tracked inputs, but the
  project does not prove that the tracked processed `.rds` files can be rebuilt
  from every original upstream source.
- Exact R table formatting, CSS, and plotting layouts are not preserved.
- Git contribution evidence depends on the public repository retaining the same
  commit authorship visible in the local history summarized in
  `docs/contributions.md`.

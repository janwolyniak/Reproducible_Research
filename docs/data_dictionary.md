# Data Dictionary

Variables and transformations used by the Python reproduction. The dictionary
covers what the Phase 4 panel models consume; the cross-section section is
populated by the cross-sectional lane.

Refer to `docs/phase0_audit.md` for the raw column inventories of every
tracked `.rds`/`.dta` input.

## Panel variables (1990-2020 and 2010-2019)

Loaded by `repro_research.panel_data.load_panel("main" | "alt" | "small")`.
The canonical key is `(country, Year)`; `country` is the stripped, canonical
version of `Country` produced by `repro_research.country_naming.to_canonical`.

| Variable | Type | Unit / scale | Source `.rds` | Notes |
| --- | --- | --- | --- | --- |
| `country` | string | name | derived | Canonical key. Trimmed copy of `Country`. |
| `Country` | string | name | `model_data*.rds` / `small_plm.rds` | Original column kept for cross-checks. |
| `cname` | string | short name | same | Original short-form column kept for cross-checks. |
| `Year` | int64 | calendar year | same | Cast from float64 on load. |
| `GDPgrowth` | float | annual % | same | Dependent variable in every Phase 4 model. |
| `cc_total` | float | index | same | Aggregate constitutional compliance index. |
| `cc_total_lv` | float | index | same | Law-and-Versteeg aggregate variant. |
| `cc_basic` | float | index | same | Right to life, slavery, torture protection. |
| `cc_civil` | float | index | same | Speech, media, movement, religion. |
| `cc_polit` | float | index | same | Association, assembly, parties. |
| `cc_prop` | float | index | same | Property, judicial independence, equality, rule of law. |
| `fertility` | float | births per woman | same | Cast via `pd.to_numeric` (small panel stores it as factor). |
| `GDPpc2015` | float | constant 2015 USD | same | Used lagged in the model formula. |
| `gov_exp_reduced` | float | % of GDP | same | Government consumption minus military & education. |
| `inflation` | float | annual % | same | GDP deflator or CPI depending on dataset. |
| `investment` | float | % of GDP | same | Gross capital formation. |
| `tot_growth` | float | annual % | same | Terms-of-trade growth used in panel formulas. |
| `trade` | float | % of GDP | same | Sum of imports and exports. |
| `education` | float | years / score | same | Dropped from the preferred panel model for missingness. |
| `life_exp` | float | years | `small_plm.rds` | Available on the small panel only. |
| `continent` | string | region | `model_data2.rds` | Used by alternative regional cuts. |
| `legal_old_o` | string | code | `model_data2.rds` | Legal-origin code (`fr`, `so`, `uk`, `ge`, `sc`). Cast from category. |
| `ever_colonized` | float | 0/1 | `model_data2.rds` | Indicator from `oth_char.rds` lineage. |

### Derived columns added by `repro_research.panel_transforms`

| Variable | Definition | Why |
| --- | --- | --- |
| `log_cc_total`, `log_cc_*` | `log(cc_* + 2)` | Compliance indices can be negative; the paper applies a `+2` shift before `log`. |
| `lag_GDPpc2015` | `GDPpc2015.shift(2)` within `country` | Mirrors R's `plm::lag(GDPpc2015, 2)`. First two years per country are NaN. |
| `lag_log_GDPpc2015` | `log(lag_GDPpc2015)` | Matches `log(lag(GDPpc2015, 2))` in the R formula. Lag is applied first, log second; NaN when the lagged value is non-positive. |
| `tot_growth_x_trade` | `tot_growth * trade` | Interaction term used in the small-panel and alt-FE specifications. |

### Reproducible derivation

```python
from repro_research.panel_data import load_panel
from repro_research.panel_transforms import prepare_for_models

df = prepare_for_models(load_panel("main"))   # 1990-2020 main panel
```

The `scripts/run_panel_prep.py` entry point materialises the same DataFrames as
CSV files under `outputs/intermediate/panel/{main,alt,small}.csv` for
downstream Phase 4 model fitting. CSV is chosen so the cache is inspectable
without `pyarrow`; Phase 4 reloads via `load_panel(...)` + `prepare_for_models(...)`.

## Cross-section variables

Populated by the cross-section lane.

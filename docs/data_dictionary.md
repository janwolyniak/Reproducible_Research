# Data Dictionary

This file is the Phase 2 shared data inventory for the Python reproduction. It records the tracked source datasets, their row and column counts, detected country/year identifier columns, and missing cell counts.

Model-specific variable definitions, transformations, and final schemas remain contributor-owned: Kinga for the cross-sectional reproduction and Iwo for the panel reproduction. They should extend this document once their Phase 2 preparation code fixes the final model variables.

The machine-readable inventory is regenerated at `docs/data_inventory.csv` by `python scripts/inventory_data.py` and `python scripts/run_all.py`.

## Cross-Sectional Sources

| Dataset | Rows | Columns | Country name | Country code | Year | Missing cells |
| --- | ---: | ---: | --- | --- | --- | ---: |
| `cross-section/data/CCCD_avg2010_19.rds` | 169 | 15 | cname | iso3c | - | 38 |
| `cross-section/data/CCCD_detailed_avg2010_19.rds` | 178 | 77 | - | Country Code | - | 1030 |
| `cross-section/data/CCCD_growth_final.rds` | 169 | 5 | - | Country Code | - | 36 |
| `cross-section/data/Democracy_avg2010_19.rds` | 179 | 7 | country_name | country_text_id | - | 0 |
| `cross-section/data/GDPpc_current_initial_Data.rds` | 265 | 3 | Country Name | Country Code | - | 4 |
| `cross-section/data/GDPpc_initial_in_2015usd_Data.rds` | 265 | 3 | Country Name | Country Code | - | 9 |
| `cross-section/data/cc_growth_yty.rds` | 167 | 5 | - | Country Code | - | 7 |
| `cross-section/data/edu_initial_Data.rds` | 265 | 3 | Country Name | Country Code | - | 79 |
| `cross-section/data/fertility_Data.rds` | 265 | 3 | Country Name | Country Code | - | 4 |
| `cross-section/data/gdp_growth_Data.rds` | 265 | 3 | Country Name | Country Code | - | 4 |
| `cross-section/data/geo_cepii.dta` | 238 | 34 | - | iso3 | - | 5 |
| `cross-section/data/gov_exp_Data.rds` | 265 | 3 | Country Name | Country Code | - | 39 |
| `cross-section/data/gov_exp_edu_Data.rds` | 265 | 3 | Country Name | Country Code | - | 31 |
| `cross-section/data/gov_milit_exp_Data.rds` | 265 | 3 | Country Name | Country Code | - | 60 |
| `cross-section/data/inflation_cpi_Data.rds` | 265 | 3 | Country Name | Country Code | - | 25 |
| `cross-section/data/inflation_gdpdeflator_Data.rds` | 265 | 3 | Country Name | Country Code | - | 5 |
| `cross-section/data/investment_Data.rds` | 265 | 3 | Country Name | Country Code | - | 41 |
| `cross-section/data/life_exp_initial_Data.rds` | 265 | 3 | Country Name | Country Code | - | 7 |
| `cross-section/data/model_data.rds` | 162 | 36 | Country Name | Country Code | - | 194 |
| `cross-section/data/model_data2.rds` | 158 | 44 | Country Name | Country_Code | - | 223 |
| `cross-section/data/model_data3.rds` | 162 | 112 | Country_Name | Country_Code | - | 663 |
| `cross-section/data/model_data4.1.rds` | 157 | 46 | Country_Name | Country_Code | - | 193 |
| `cross-section/data/model_data4.rds` | 162 | 42 | Country_Name | Country_Code | - | 194 |
| `cross-section/data/model_data4_o.rds` | 157 | 43 | Country_Name | Country_Code | - | 192 |
| `cross-section/data/oth_char.rds` | 220 | 4 | - | Country_Code | - | 18 |
| `cross-section/data/tot2.rds` | 271 | 2 | - | Country Code | - | 72 |
| `cross-section/data/tot_Data.rds` | 265 | 3 | Country Name | Country Code | - | 61 |
| `cross-section/data/trade_Data.rds` | 265 | 3 | Country Name | Country Code | - | 32 |

## Panel Sources

| Dataset | Rows | Columns | Country name | Country code | Year | Missing cells |
| --- | ---: | ---: | --- | --- | --- | ---: |
| `panel/data_panel/model_data.rds` | 4906 | 18 | cname | Country | Year | 6366 |
| `panel/data_panel/model_data2.rds` | 4786 | 21 | cname | Country | Year | 6189 |
| `panel/data_panel/small_plm.rds` | 1617 | 22 | cname | Country | Year | 2060 |

## Shared Loading Contract

- Use `repro_research.data_io.build_dataset_specs()` to list the tracked source datasets.
- Use `repro_research.data_io.load_dataset(spec)` for the default data frame from one source file.
- Use `repro_research.data_io.read_rds_dataframe(path)` when a contributor needs to load a specific `.rds` file directly.
- Treat `cross-section/data/` and `panel/data_panel/` as authoritative inputs; generated caches belong under `outputs/intermediate/` only when they make review easier.

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

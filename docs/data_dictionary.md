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

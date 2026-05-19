## Cross-Sectional Model Variables

This section covers the cross-sectional inputs prepared for Kinga's
Phase 2 work. It is appended by `scripts/run_cross_section_data.py`.

Canonical loader conventions:

- Country-name variants are exposed as `country_name`.
- Country-code variants are exposed as `country_code`.
- Cross-sectional model data add `log_GDPpc2015`, shifted compliance logs
  such as `log_cc_total_plus2`, and interaction terms such as
  `terms_trade_x_trade`. The canonical `terms_trade` uses `tot2` when
  available and falls back to `tot` for tracked artifacts where `tot2`
  is absent.
- `model_data4_o` is the outlier-filtered version of `model_data4`.

| Variable | Role | Datasets | Missing values |
| --- | --- | --- | ---: |
| `Country Code` | country_code | CCCD_detailed_avg2010_19, CCCD_growth_final, GDPpc_current_initial_Data, GDPpc_initial_in_2015usd_Data, cc_growth_yty, edu_initial_Data, fertility_Data, gdp_growth_Data, gov_exp_Data, gov_exp_edu_Data, gov_milit_exp_Data, inflation_cpi_Data, inflation_gdpdeflator_Data, investment_Data, life_exp_initial_Data, model_data, tot2, tot_Data, trade_Data | 5 |
| `Country Name` | country_name | GDPpc_current_initial_Data, GDPpc_initial_in_2015usd_Data, edu_initial_Data, fertility_Data, gdp_growth_Data, gov_exp_Data, gov_exp_edu_Data, gov_milit_exp_Data, inflation_cpi_Data, inflation_gdpdeflator_Data, investment_Data, life_exp_initial_Data, model_data, model_data2, tot_Data, trade_Data | 0 |
| `Country_Code` | country_code | model_data2, model_data3, model_data4, model_data4.1, model_data4_o, oth_char | 0 |
| `Country_Name` | country_name | model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `Fertility rate, total (births per woman)` | source_variable | fertility_Data | 4 |
| `GDP per capita in 2010 (constant 2015 USD)` | source_variable | GDPpc_initial_in_2015usd_Data | 9 |
| `GDP per capita in 2010 (current USD)` | source_variable | GDPpc_current_initial_Data | 4 |
| `GDP_growth` | economic_variable | gdp_growth_Data, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 10 |
| `GDPpc` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 6 |
| `GDPpc2015` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 18 |
| `General government final consumption expenditure (% of GDP)` | source_variable | gov_exp_Data | 39 |
| `Government expenditure on education, total (% of GDP)` | source_variable | gov_exp_edu_Data | 31 |
| `Gross capital formation (% of GDP)` | source_variable | investment_Data | 41 |
| `Inflation, GDP deflator (annual %)` | source_variable | inflation_gdpdeflator_Data | 5 |
| `Inflation, consumer prices (annual %)` | source_variable | inflation_cpi_Data | 25 |
| `Life expectancy at birth in 2010 (years)` | source_variable | life_exp_initial_Data | 7 |
| `Military expenditure (% of GDP)` | source_variable | gov_milit_exp_Data | 60 |
| `Net barter terms of trade index (2015 = 100)` | source_variable | tot_Data | 61 |
| `School enrollment in 2010, tertiary (gross %)` | source_variable | edu_initial_Data | 79 |
| `Trade (% of GDP)` | source_variable | trade_Data | 32 |
| `area` | source_variable | geo_cepii | 0 |
| `cap` | source_variable | geo_cepii | 0 |
| `cc_alt_cp` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 29 |
| `cc_alt_cp.x` | constitutional_compliance | model_data3 | 3 |
| `cc_alt_cp.y` | constitutional_compliance | model_data3 | 3 |
| `cc_alt_cp_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 37 |
| `cc_alt_cp_lv.x` | constitutional_compliance | model_data3 | 4 |
| `cc_alt_cp_lv.y` | constitutional_compliance | model_data3 | 4 |
| `cc_basic` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 9 |
| `cc_basic.x` | constitutional_compliance | model_data3 | 0 |
| `cc_basic.y` | constitutional_compliance | model_data3 | 0 |
| `cc_basic_growth` | constitutional_compliance | CCCD_growth_final, model_data2 | 16 |
| `cc_basic_growth_yty` | constitutional_compliance | cc_growth_yty, model_data2 | 0 |
| `cc_basic_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 107 |
| `cc_basic_lv.x` | constitutional_compliance | model_data3 | 14 |
| `cc_basic_lv.y` | constitutional_compliance | model_data3 | 14 |
| `cc_civil` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 9 |
| `cc_civil.x` | constitutional_compliance | model_data3 | 0 |
| `cc_civil.y` | constitutional_compliance | model_data3 | 0 |
| `cc_civil_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 37 |
| `cc_civil_lv.x` | constitutional_compliance | model_data3 | 4 |
| `cc_civil_lv.y` | constitutional_compliance | model_data3 | 4 |
| `cc_polit` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 29 |
| `cc_polit.x` | constitutional_compliance | model_data3 | 3 |
| `cc_polit.y` | constitutional_compliance | model_data3 | 3 |
| `cc_polit_growth` | constitutional_compliance | CCCD_growth_final, model_data2 | 23 |
| `cc_polit_growth_yty` | constitutional_compliance | cc_growth_yty, model_data2 | 10 |
| `cc_polit_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 58 |
| `cc_polit_lv.x` | constitutional_compliance | model_data3 | 7 |
| `cc_polit_lv.y` | constitutional_compliance | model_data3 | 7 |
| `cc_prop` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 9 |
| `cc_prop.x` | constitutional_compliance | model_data3 | 0 |
| `cc_prop.y` | constitutional_compliance | model_data3 | 0 |
| `cc_prop_growth` | constitutional_compliance | CCCD_growth_final, model_data2 | 16 |
| `cc_prop_growth_yty` | constitutional_compliance | cc_growth_yty, model_data2 | 2 |
| `cc_prop_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 23 |
| `cc_prop_lv.x` | constitutional_compliance | model_data3 | 2 |
| `cc_prop_lv.y` | constitutional_compliance | model_data3 | 2 |
| `cc_total` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 9 |
| `cc_total.x` | constitutional_compliance | model_data3 | 0 |
| `cc_total.y` | constitutional_compliance | model_data3 | 0 |
| `cc_total_growth` | constitutional_compliance | CCCD_growth_final, model_data2 | 16 |
| `cc_total_growth_yty` | constitutional_compliance | cc_growth_yty, model_data2 | 0 |
| `cc_total_lv` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 16 |
| `cc_total_lv.x` | constitutional_compliance | model_data3 | 1 |
| `cc_total_lv.y` | constitutional_compliance | model_data3 | 1 |
| `city_en` | source_variable | geo_cepii | 0 |
| `city_fr` | source_variable | geo_cepii | 0 |
| `citynum` | source_variable | geo_cepii | 5 |
| `cname` | country_name | CCCD_avg2010_19 | 0 |
| `cnum` | source_variable | geo_cepii | 0 |
| `colonizer1` | source_variable | geo_cepii | 0 |
| `colonizer2` | source_variable | geo_cepii | 0 |
| `colonizer3` | source_variable | geo_cepii | 0 |
| `colonizer4` | source_variable | geo_cepii | 0 |
| `continent` | categorical_or_grouping | geo_cepii, model_data4.1, oth_char | 0 |
| `country` | country_name | geo_cepii | 0 |
| `country_name` | country_name | Democracy_avg2010_19 | 0 |
| `country_text_id` | country_code | Democracy_avg2010_19 | 0 |
| `ctrl_of_corr` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |
| `df_right_assembly` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_association` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_equal` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_life` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_media` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_movement` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_parties` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 7 |
| `df_right_property` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_religion` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_slavery` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_speech` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_right_torture` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_state_judind` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `df_state_rol` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 1 |
| `dis_int` | source_variable | geo_cepii | 0 |
| `dj_index` | constitutional_compliance | CCCD_avg2010_19, CCCD_detailed_avg2010_19, model_data, model_data2, model_data4, model_data4.1, model_data4_o | 9 |
| `dj_index.x` | constitutional_compliance | model_data3 | 0 |
| `dj_index.y` | constitutional_compliance | model_data3 | 0 |
| `dj_right_assembly` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_association` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_censorship` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_death` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_equal` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_expression` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_freepress` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_life` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_movement` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_opinion` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_parties` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_prop_int` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_property` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_religion` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_slavery` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_right_torture` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_state_exprop` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_state_judind` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `dj_state_rol` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `education` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 273 |
| `ever_colonized` | categorical_or_grouping | model_data4.1, oth_char | 0 |
| `fertility` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `gap_right_assembly` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_assembly_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 31 |
| `gap_right_association` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_association_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 31 |
| `gap_right_equal` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_equal_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 13 |
| `gap_right_life` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_life_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 67 |
| `gap_right_media` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_media_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 77 |
| `gap_right_movement` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_movement_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 46 |
| `gap_right_parties` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 15 |
| `gap_right_parties_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 136 |
| `gap_right_property` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_property_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 37 |
| `gap_right_religion` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_religion_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 39 |
| `gap_right_slavery` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_slavery_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 188 |
| `gap_right_speech` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_speech_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 21 |
| `gap_right_torture` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_right_torture_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 84 |
| `gap_state_judind` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_state_judind_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 59 |
| `gap_state_rol` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 9 |
| `gap_state_rol_lv` | constitutional_compliance | CCCD_detailed_avg2010_19, model_data3 | 154 |
| `gov_eff` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |
| `gov_exp` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 71 |
| `gov_exp_edu` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 71 |
| `gov_exp_milit` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 90 |
| `gov_exp_reduced` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 196 |
| `inf_cpi` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 30 |
| `inf_def` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 6 |
| `investment` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 77 |
| `iso2` | source_variable | geo_cepii | 0 |
| `iso3` | country_code | geo_cepii | 0 |
| `iso3c` | country_code | CCCD_avg2010_19 | 0 |
| `landlocked` | categorical_or_grouping | geo_cepii, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `lang20_1` | source_variable | geo_cepii | 0 |
| `lang20_2` | source_variable | geo_cepii | 0 |
| `lang20_3` | source_variable | geo_cepii | 0 |
| `lang20_4` | source_variable | geo_cepii | 0 |
| `lang9_1` | source_variable | geo_cepii | 0 |
| `lang9_2` | source_variable | geo_cepii | 0 |
| `lang9_3` | source_variable | geo_cepii | 0 |
| `lang9_4` | source_variable | geo_cepii | 0 |
| `langoff_1` | source_variable | geo_cepii | 0 |
| `langoff_2` | source_variable | geo_cepii | 0 |
| `langoff_3` | source_variable | geo_cepii | 0 |
| `lat` | source_variable | geo_cepii | 0 |
| `legal_old_o` | categorical_or_grouping | model_data4.1, oth_char | 19 |
| `life_exp` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `lon` | source_variable | geo_cepii | 0 |
| `maincity` | source_variable | geo_cepii | 0 |
| `pays` | source_variable | geo_cepii | 0 |
| `pol_stab` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |
| `reg_q` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |
| `rule_of_law` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |
| `short_colonizer1` | source_variable | geo_cepii | 0 |
| `short_colonizer2` | source_variable | geo_cepii | 0 |
| `short_colonizer3` | source_variable | geo_cepii | 0 |
| `tot` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 6 |
| `tot2` | economic_variable | model_data4.1, model_data4_o, tot2 | 69 |
| `trade` | economic_variable | model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 71 |
| `v2x_delibdem` | institutional_control | Democracy_avg2010_19, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `v2x_egaldem` | institutional_control | Democracy_avg2010_19, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `v2x_libdem` | institutional_control | Democracy_avg2010_19, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `v2x_partipdem` | institutional_control | Democracy_avg2010_19, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `v2x_polyarchy` | institutional_control | Democracy_avg2010_19, model_data, model_data2, model_data3, model_data4, model_data4.1, model_data4_o | 0 |
| `vdem_respect_const` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 0 |
| `vdem_respect_const_ord` | source_variable | CCCD_detailed_avg2010_19, model_data3 | 0 |
| `voice_and_acc` | institutional_control | model_data4, model_data4.1, model_data4_o | 0 |

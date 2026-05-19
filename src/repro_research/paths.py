from __future__ import annotations

from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[2]

OUTPUT_DIRS = (
    "outputs",
    "outputs/cross_section",
    "outputs/panel",
    "outputs/logs",
    "outputs/intermediate",
)

CORE_INPUT_FILES = (
    "2400-LIC-FII.pdf",
    "OUTLINE.md",
    "TODO.md",
    "README.md",
    "docs/reproduction_contract.md",
    "scripts/audit_phase0.py",
    "helpers/rds_to_csv.py",
    "cross-section/inorder.R",
    "panel/inorder_panel.R",
)

CROSS_SECTION_DATA_FILES = (
    "cross-section/data/CCCD_avg2010_19.rds",
    "cross-section/data/CCCD_detailed_avg2010_19.rds",
    "cross-section/data/CCCD_growth_final.rds",
    "cross-section/data/Democracy_avg2010_19.rds",
    "cross-section/data/GDPpc_current_initial_Data.rds",
    "cross-section/data/GDPpc_initial_in_2015usd_Data.rds",
    "cross-section/data/cc_growth_yty.rds",
    "cross-section/data/edu_initial_Data.rds",
    "cross-section/data/fertility_Data.rds",
    "cross-section/data/gdp_growth_Data.rds",
    "cross-section/data/geo_cepii.dta",
    "cross-section/data/gov_exp_Data.rds",
    "cross-section/data/gov_exp_edu_Data.rds",
    "cross-section/data/gov_milit_exp_Data.rds",
    "cross-section/data/inflation_cpi_Data.rds",
    "cross-section/data/inflation_gdpdeflator_Data.rds",
    "cross-section/data/investment_Data.rds",
    "cross-section/data/life_exp_initial_Data.rds",
    "cross-section/data/model_data.rds",
    "cross-section/data/model_data2.rds",
    "cross-section/data/model_data3.rds",
    "cross-section/data/model_data4.1.rds",
    "cross-section/data/model_data4.rds",
    "cross-section/data/model_data4_o.rds",
    "cross-section/data/oth_char.rds",
    "cross-section/data/tot2.rds",
    "cross-section/data/tot_Data.rds",
    "cross-section/data/trade_Data.rds",
)

PANEL_DATA_FILES = (
    "panel/data_panel/model_data.rds",
    "panel/data_panel/model_data2.rds",
    "panel/data_panel/small_plm.rds",
)

REFERENCE_OUTPUT_FILES = (
    "cross-section/tables_and_other/descriptive stat.html",
    "cross-section/tables_and_other/Corrplot.png",
    "cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs total.html",
    "cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs prop.html",
    "cross-section/tables_and_other/reg_table1.html",
    "cross-section/tables_and_other/reg_table2&3.html",
    "cross-section/tables_and_other/reg_table_lv&prop.html",
    "cross-section/tables_and_other/reg_table_inst.html",
    "cross-section/tables_and_other/diagnostics.html",
    "panel/panel_tables/descriptive.html",
    "panel/panel_tables/Corrplot.png",
    "panel/panel_tables/Specification tests.html",
    "panel/panel_tables/Diagnostic tests.html",
    "panel/panel_tables/model_output.html",
    "panel/panel_tables/model_output2.html",
)

REQUIRED_PATHS = (
    *CORE_INPUT_FILES,
    *CROSS_SECTION_DATA_FILES,
    *PANEL_DATA_FILES,
    *REFERENCE_OUTPUT_FILES,
)


def resolve_repo_path(relative_path: str) -> Path:
    return PROJECT_ROOT / relative_path

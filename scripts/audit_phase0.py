"""Phase 0 source audit.

Inventories tracked data inputs, reconciles country and variable naming,
documents available source-to-model lineage, verifies the cross-sectional
outlier rule, and explains model-wise missingness from the tracked files.

The result is written to ``docs/phase0_audit.md``.

Usage:

    python3 scripts/audit_phase0.py [--report PATH]
"""
from __future__ import annotations

import argparse
import math
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable

REPO = Path(__file__).resolve().parent.parent
DATA_DIRS = [REPO / "cross-section" / "data", REPO / "panel" / "data_panel"]

COUNTRY_NAME_CANDIDATES = (
    "Country",
    "country",
    "country_name",
    "Country_Name",
    "Country Name",
    "cname",
)
COUNTRY_CODE_CANDIDATES = (
    "Country Code",
    "Country_Code",
    "country_text_id",
    "iso3",
    "ISO3",
    "iso3c",
)
YEAR_CANDIDATES = ("Year", "year", "YEAR")

NAME_VARIANT_GROUPS: dict[str, tuple[str, ...]] = {
    "GDP growth": ("GDP_growth", "GDPgrowth", "gdp_growth"),
    "Terms of trade": ("tot", "tot2", "tot_growth"),
    "Country identifier": (
        "Country",
        "country",
        "Country Code",
        "Country_Code",
        "Country_Name",
        "Country Name",
        "cname",
        "country_name",
        "country_text_id",
        "iso3",
        "iso3c",
    ),
    "Military government expenditure": ("gov_exp_milit", "gov_exp_mil"),
}

CONTRACT_PATHS = [
    "2400-LIC-FII.pdf",
    "cross-section/inorder.R",
    "panel/inorder_panel.R",
    "helpers/rds_to_csv.py",
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
    "panel/data_panel/model_data.rds",
    "panel/data_panel/model_data2.rds",
    "panel/data_panel/small_plm.rds",
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
]

SOURCE_FILES = [
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
    "cross-section/data/oth_char.rds",
    "cross-section/data/tot2.rds",
    "cross-section/data/tot_Data.rds",
    "cross-section/data/trade_Data.rds",
]

WORLD_BANK_VARIABLE_SOURCES = {
    "GDP_growth": "cross-section/data/gdp_growth_Data.rds",
    "fertility": "cross-section/data/fertility_Data.rds",
    "GDPpc2015": "cross-section/data/GDPpc_initial_in_2015usd_Data.rds",
    "GDPpc": "cross-section/data/GDPpc_current_initial_Data.rds",
    "gov_exp": "cross-section/data/gov_exp_Data.rds",
    "gov_exp_edu": "cross-section/data/gov_exp_edu_Data.rds",
    "gov_exp_milit": "cross-section/data/gov_milit_exp_Data.rds",
    "inf_cpi": "cross-section/data/inflation_cpi_Data.rds",
    "inf_def": "cross-section/data/inflation_gdpdeflator_Data.rds",
    "investment": "cross-section/data/investment_Data.rds",
    "life_exp": "cross-section/data/life_exp_initial_Data.rds",
    "education": "cross-section/data/edu_initial_Data.rds",
    "tot": "cross-section/data/tot_Data.rds",
    "trade": "cross-section/data/trade_Data.rds",
}

MODEL_SPECS = {
    "cross_section_model_1": [
        "GDP_growth",
        "fertility",
        "GDPpc2015",
        "inf_def",
        "investment",
        "gov_exp_reduced",
        "cc_total",
        "tot2",
        "trade",
    ],
    "cross_section_model_2": [
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "gov_exp_reduced",
        "cc_total",
        "tot2",
        "trade",
    ],
    "cross_section_model_2lv": [
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "gov_exp_reduced",
        "cc_total_lv",
        "tot2",
        "trade",
    ],
    "cross_section_model_2prop": [
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "gov_exp_reduced",
        "cc_prop",
        "tot2",
        "trade",
    ],
}


@dataclass
class FrameSummary:
    relpath: str
    object_name: str
    n_rows: int
    n_cols: int
    country_name_col: str | None
    country_code_col: str | None
    year_col: str | None
    n_unique_countries: int | None
    year_min: int | None
    year_max: int | None
    n_unique_years: int | None
    columns: list[tuple[str, str, int, float]] = field(default_factory=list)
    column_set: set[str] = field(default_factory=set)
    countries: set[str] = field(default_factory=set)
    country_codes: set[str] = field(default_factory=set)


def _load_dependencies():
    try:
        import numpy as np
        import pandas as pd
        import pyreadr
    except ImportError as exc:
        raise SystemExit(
            "Phase 0 audit requires pandas, numpy, and pyreadr to read tracked "
            "inputs. Install the project dependencies or run in the prepared env."
        ) from exc
    return np, pd, pyreadr


def _first_present(columns: Iterable[str], candidates: Iterable[str]) -> str | None:
    column_set = set(columns)
    for candidate in candidates:
        if candidate in column_set:
            return candidate
    return None


def _normalize_code(value: object) -> str:
    if value is None:
        return ""
    if isinstance(value, float) and math.isnan(value):
        return ""
    return str(value).strip().upper()


def _load_data_file(path: Path, pd, pyreadr) -> dict[str, object]:
    if path.suffix.lower() == ".dta":
        return {path.stem: pd.read_stata(path)}
    result = pyreadr.read_r(str(path))
    return {
        name if name is not None else path.stem: frame
        for name, frame in result.items()
        if hasattr(frame, "columns")
    }


def summarize_frame(frame, source: Path, object_name: str) -> FrameSummary:
    n_rows, n_cols = frame.shape
    country_name_col = _first_present(frame.columns, COUNTRY_NAME_CANDIDATES)
    country_code_col = _first_present(frame.columns, COUNTRY_CODE_CANDIDATES)
    year_col = _first_present(frame.columns, YEAR_CANDIDATES)

    country_col = country_name_col or country_code_col
    countries: set[str] = set()
    n_unique_countries: int | None = None
    if country_col is not None:
        raw_countries = frame[country_col].dropna().astype(str)
        countries = set(raw_countries)
        n_unique_countries = int(raw_countries.nunique(dropna=True))

    country_codes: set[str] = set()
    if country_code_col is not None:
        country_codes = {
            _normalize_code(value)
            for value in frame[country_code_col].dropna()
            if _normalize_code(value)
        }

    year_min = year_max = n_unique_years = None
    if year_col is not None:
        years = frame[year_col].dropna().astype(float)
        if not years.empty:
            year_min = int(years.min())
            year_max = int(years.max())
            n_unique_years = int(years.nunique())

    missing = frame.isna().sum()
    columns: list[tuple[str, str, int, float]] = []
    for column in frame.columns:
        n_missing = int(missing[column])
        pct_missing = round(n_missing / n_rows * 100, 1) if n_rows else 0.0
        columns.append((str(column), str(frame[column].dtype), n_missing, pct_missing))

    return FrameSummary(
        relpath=str(source.relative_to(REPO)).replace("\\", "/"),
        object_name=object_name if object_name else source.stem,
        n_rows=int(n_rows),
        n_cols=int(n_cols),
        country_name_col=country_name_col,
        country_code_col=country_code_col,
        year_col=year_col,
        n_unique_countries=n_unique_countries,
        year_min=year_min,
        year_max=year_max,
        n_unique_years=n_unique_years,
        columns=columns,
        column_set=set(map(str, frame.columns)),
        countries=countries,
        country_codes=country_codes,
    )


def collect_inputs() -> tuple[list[FrameSummary], dict[str, object], list[tuple[str, str]]]:
    _, pd, pyreadr = _load_dependencies()
    summaries: list[FrameSummary] = []
    frames: dict[str, object] = {}
    errors: list[tuple[str, str]] = []

    for data_dir in DATA_DIRS:
        if not data_dir.is_dir():
            errors.append((str(data_dir.relative_to(REPO)), "directory missing"))
            continue
        for data_file in sorted([*data_dir.glob("*.rds"), *data_dir.glob("*.dta")]):
            relpath = str(data_file.relative_to(REPO)).replace("\\", "/")
            try:
                objects = _load_data_file(data_file, pd, pyreadr)
            except Exception as exc:  # noqa: BLE001 - report every read failure.
                errors.append((relpath, str(exc)))
                continue
            if not objects:
                errors.append((relpath, "no data frame object found"))
                continue
            for object_name, frame in objects.items():
                frames[relpath] = frame
                summaries.append(summarize_frame(frame, data_file, object_name))

    return summaries, frames, errors


def build_name_variant_table(summaries: list[FrameSummary]) -> dict[str, dict[str, list[str]]]:
    table: dict[str, dict[str, list[str]]] = {}
    for group_name, variants in NAME_VARIANT_GROUPS.items():
        table[group_name] = {variant: [] for variant in variants}
        for summary in summaries:
            for variant in variants:
                if variant in summary.column_set:
                    table[group_name][variant].append(
                        f"{summary.relpath} ({summary.object_name})"
                    )
    return table


def contract_coverage() -> list[tuple[str, bool]]:
    return [(path, (REPO / path).exists()) for path in CONTRACT_PATHS]


def source_coverage(summaries: list[FrameSummary]) -> list[dict[str, object]]:
    by_path = {summary.relpath: summary for summary in summaries}
    target = by_path.get("cross-section/data/model_data4.1.rds")
    target_codes = target.country_codes if target else set()
    rows: list[dict[str, object]] = []

    for relpath in SOURCE_FILES:
        summary = by_path.get(relpath)
        if summary is None:
            rows.append(
                {
                    "path": relpath,
                    "rows": "-",
                    "codes": "-",
                    "matched_target": "-",
                    "target_missing": "-",
                    "extra_codes": "-",
                }
            )
            continue
        source_codes = summary.country_codes
        matched = source_codes & target_codes if target_codes else set()
        target_missing = target_codes - source_codes if target_codes else set()
        extra_codes = source_codes - target_codes if target_codes else set()
        rows.append(
            {
                "path": relpath,
                "rows": summary.n_rows,
                "codes": len(source_codes) if source_codes else "-",
                "matched_target": len(matched) if source_codes else "-",
                "target_missing": sorted(target_missing),
                "extra_codes": len(extra_codes) if source_codes else "-",
            }
        )
    return rows


def source_to_variable_map(frames: dict[str, object]) -> list[dict[str, str]]:
    final_path = "cross-section/data/model_data4.1.rds"
    final = frames.get(final_path)
    if final is None:
        return []

    rows: list[dict[str, str]] = []
    for variable in final.columns:
        sources: list[str] = []
        note = "direct tracked source"
        if variable in {"Country_Name", "Country_Code"}:
            sources = ["merged identifiers"]
        elif variable in WORLD_BANK_VARIABLE_SOURCES:
            sources = [WORLD_BANK_VARIABLE_SOURCES[variable]]
        elif variable == "gov_exp_reduced":
            sources = [
                "cross-section/data/gov_exp_Data.rds",
                "cross-section/data/gov_exp_edu_Data.rds",
                "cross-section/data/gov_milit_exp_Data.rds",
            ]
            note = "derived as government spending net of education and military"
        elif variable.startswith("cc_") or variable == "dj_index":
            sources = ["cross-section/data/CCCD_avg2010_19.rds"]
        elif variable.startswith("v2x_"):
            sources = ["cross-section/data/Democracy_avg2010_19.rds"]
        elif variable == "landlocked":
            sources = ["cross-section/data/geo_cepii.dta"]
        elif variable in {"continent", "ever_colonized"}:
            sources = ["cross-section/data/geo_cepii.dta", "cross-section/data/oth_char.rds"]
        elif variable == "legal_old_o":
            sources = ["cross-section/data/oth_char.rds"]
        elif variable == "tot2":
            sources = ["cross-section/data/tot2.rds"]
        elif variable in {
            "ctrl_of_corr",
            "gov_eff",
            "pol_stab",
            "reg_q",
            "rule_of_law",
            "voice_and_acc",
        }:
            sources = ["cross-section/data/model_data4.rds"]
            note = "available only in merged model data among tracked files"
        else:
            sources = ["unmapped"]
            note = "no direct tracked source identified"

        rows.append(
            {
                "variable": str(variable),
                "sources": ", ".join(sources),
                "note": note,
            }
        )
    return rows


def _add_tot2_if_needed(frame, frames: dict[str, object]):
    if "tot2" in frame.columns:
        return frame.copy()
    tot2 = frames.get("cross-section/data/tot2.rds")
    if tot2 is None:
        return frame.copy()
    if "Country Code" not in tot2.columns:
        return frame.copy()
    return frame.merge(
        tot2.rename(columns={"Country Code": "Country_Code"}),
        on="Country_Code",
        how="left",
    )


def outlier_rule_check(frames: dict[str, object]) -> dict[str, object]:
    model_data = frames.get("cross-section/data/model_data.rds")
    model_data4 = frames.get("cross-section/data/model_data4.rds")
    model_data4_o = frames.get("cross-section/data/model_data4_o.rds")

    rows = []
    for name, frame in (
        ("model_data", model_data),
        ("model_data4", model_data4),
        ("model_data4_o", model_data4_o),
    ):
        if frame is None:
            continue
        country_col = _first_present(frame.columns, COUNTRY_NAME_CANDIDATES)
        rows.append(
            {
                "dataset": name,
                "rows": len(frame),
                "countries": int(frame[country_col].nunique()) if country_col else "-",
                "country_col": country_col or "-",
            }
        )

    out: dict[str, object] = {"rows": rows}
    if model_data4 is None or model_data4_o is None:
        return out

    old_countries = set(model_data4["Country_Name"].dropna().astype(str))
    new_countries = set(model_data4_o["Country_Name"].dropna().astype(str))
    dropped = sorted(old_countries - new_countries)
    out["dropped_from_model_data4"] = dropped
    out["added_in_model_data4_o"] = sorted(new_countries - old_countries)

    try:
        import numpy as np
    except ImportError:
        out["regression_error"] = "numpy unavailable"
        return out

    frame = _add_tot2_if_needed(model_data4, frames)
    required = [
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "tot2",
        "trade",
        "gov_exp_reduced",
        "cc_total",
    ]
    data = frame.dropna(subset=required).copy()
    data = data[(data["GDPpc2015"] > 0) & (data["cc_total"] + 2 > 0)].copy()
    data["log_GDPpc2015"] = np.log(data["GDPpc2015"])
    data["log_cc_total_p2"] = np.log(data["cc_total"] + 2)
    data["tot2_trade"] = data["tot2"] * data["trade"]

    x_columns = [
        "log_GDPpc2015",
        "investment",
        "tot2_trade",
        "gov_exp_reduced",
        "log_cc_total_p2",
        "tot2",
        "trade",
    ]
    y = data["GDP_growth"].to_numpy(dtype=float)
    x = data[x_columns].to_numpy(dtype=float)
    x = np.column_stack([np.ones(len(x)), x])
    beta = np.linalg.lstsq(x, y, rcond=None)[0]
    residuals = y - x @ beta
    n_obs, n_params = x.shape
    mse = float((residuals @ residuals) / (n_obs - n_params))
    xtx_inv = np.linalg.pinv(x.T @ x)
    leverage = np.einsum("ij,jk,ik->i", x, xtx_inv, x)
    std_resid = residuals / (math.sqrt(mse) * np.sqrt(1 - leverage))
    cooks_d = (residuals**2 / (n_params * mse)) * (leverage / (1 - leverage) ** 2)

    leverage_threshold = 2 * n_params / n_obs
    cook_threshold = 4 / n_obs
    rule_mask = ((leverage > leverage_threshold) & (np.abs(std_resid) > 2)) | (
        leverage > 0.2
    )

    diagnostics = data[["Country_Name", "Country_Code"]].copy()
    diagnostics["leverage"] = leverage
    diagnostics["standardized_residual"] = std_resid
    diagnostics["cooks_distance"] = cooks_d
    diagnostics["rule_flag"] = rule_mask
    diagnostics["dropped_in_model_data4_o"] = diagnostics["Country_Name"].isin(dropped)

    flagged = diagnostics[diagnostics["rule_flag"]].copy()
    dropped_diag = diagnostics[diagnostics["dropped_in_model_data4_o"]].copy()

    out.update(
        {
            "regression_n": n_obs,
            "regression_parameters": n_params,
            "leverage_threshold": leverage_threshold,
            "cook_threshold": cook_threshold,
            "rule_flagged": flagged,
            "dropped_diagnostics": dropped_diag,
            "rule_matches_dropped": set(flagged["Country_Name"]) == set(dropped),
        }
    )
    return out


def model_missingness(frames: dict[str, object]) -> list[dict[str, object]]:
    base = frames.get("cross-section/data/model_data4_o.rds")
    if base is None:
        return []
    frame = _add_tot2_if_needed(base, frames)
    rows: list[dict[str, object]] = []

    for model_name, variables in MODEL_SPECS.items():
        available_vars = [variable for variable in variables if variable in frame.columns]
        missing_vars = [variable for variable in variables if variable not in frame.columns]
        data = frame[available_vars].copy()
        if "GDPpc2015" in data:
            data.loc[data["GDPpc2015"] <= 0, "GDPpc2015"] = float("nan")
        for compliance in ("cc_total", "cc_total_lv", "cc_prop"):
            if compliance in data:
                data.loc[data[compliance] + 2 <= 0, compliance] = float("nan")
        complete = data.dropna()
        var_losses = data.isna().sum().sort_values(ascending=False)
        loss_text = ", ".join(
            f"{name}: {int(count)}"
            for name, count in var_losses.items()
            if int(count) > 0
        )
        rows.append(
            {
                "model": model_name,
                "base_rows": len(frame),
                "complete_rows": len(complete),
                "dropped_rows": len(frame) - len(complete),
                "missing_vars": ", ".join(missing_vars) if missing_vars else "-",
                "variable_losses": loss_text if loss_text else "-",
            }
        )
    return rows


def attrition_summary(frames: dict[str, object]) -> list[tuple[str, int | str, str]]:
    ordered = [
        ("World Bank-style variable extracts", "cross-section/data/gdp_growth_Data.rds"),
        ("CCCD average compliance extract", "cross-section/data/CCCD_avg2010_19.rds"),
        ("V-Dem democracy extract", "cross-section/data/Democracy_avg2010_19.rds"),
        ("CEPII geography extract", "cross-section/data/geo_cepii.dta"),
        ("Merged cross-section base", "cross-section/data/model_data.rds"),
        ("Merged with source/group additions", "cross-section/data/model_data4.1.rds"),
        ("Preferred outlier-filtered data", "cross-section/data/model_data4_o.rds"),
    ]
    rows: list[tuple[str, int | str, str]] = []
    for label, path in ordered:
        frame = frames.get(path)
        if frame is None:
            rows.append((label, "-", path))
        else:
            rows.append((label, len(frame), path))
    return rows


def _fmt_list(values: Iterable[str], limit: int = 25) -> str:
    ordered = list(values)
    if not ordered:
        return "-"
    shown = ordered[:limit]
    suffix = "" if len(ordered) <= limit else f" ... ({len(ordered) - limit} more)"
    return ", ".join(shown) + suffix


def render_report(
    summaries: list[FrameSummary],
    frames: dict[str, object],
    variants: dict[str, dict[str, list[str]]],
    coverage: list[tuple[str, bool]],
    errors: list[tuple[str, str]],
) -> str:
    outlier = outlier_rule_check(frames)
    source_rows = source_coverage(summaries)
    variable_rows = source_to_variable_map(frames)
    missingness_rows = model_missingness(frames)
    attrition_rows = attrition_summary(frames)

    lines: list[str] = []
    lines.append("# Phase 0 Audit Report")
    lines.append("")
    lines.append(
        "Auto-generated by `scripts/audit_phase0.py`. The audit assumes tracked "
        "extracts in `cross-section/data/` are authoritative source-level inputs."
    )
    lines.append("")

    lines.append("## 1. Data inventory")
    lines.append("")
    lines.append("| File | Object | Rows | Cols | Country col | Year col | Countries | Years |")
    lines.append("| --- | --- | ---: | ---: | --- | --- | ---: | --- |")
    for summary in summaries:
        country_col = summary.country_name_col or summary.country_code_col or "-"
        year_col = summary.year_col or "-"
        years = (
            f"{summary.year_min}-{summary.year_max} ({summary.n_unique_years})"
            if summary.year_min is not None
            else "-"
        )
        countries = (
            summary.n_unique_countries if summary.n_unique_countries is not None else "-"
        )
        lines.append(
            f"| `{summary.relpath}` | `{summary.object_name}` | {summary.n_rows} | "
            f"{summary.n_cols} | {country_col} | {year_col} | {countries} | {years} |"
        )
    lines.append("")

    lines.append("## 2. Local lineage and attrition")
    lines.append("")
    lines.append("| Step | Rows | File |")
    lines.append("| --- | ---: | --- |")
    for label, rows, path in attrition_rows:
        lines.append(f"| {label} | {rows} | `{path}` |")
    lines.append("")
    lines.append(
        "This is local-data lineage, not upstream download provenance. The audit "
        "treats the tracked extracts as already prepared correctly."
    )
    lines.append("")

    lines.append("## 3. Country-code source coverage")
    lines.append("")
    lines.append(
        "Coverage is measured against `model_data4.1.rds` country codes, the "
        "widest tracked final cross-sectional model dataset."
    )
    lines.append("")
    lines.append("| Source file | Rows | Codes | Matched target codes | Target codes absent | Extra source codes |")
    lines.append("| --- | ---: | ---: | ---: | --- | ---: |")
    for row in source_rows:
        missing = row["target_missing"]
        missing_text = _fmt_list(missing) if isinstance(missing, list) else str(missing)
        lines.append(
            f"| `{row['path']}` | {row['rows']} | {row['codes']} | "
            f"{row['matched_target']} | {missing_text} | {row['extra_codes']} |"
        )
    lines.append("")

    lines.append("## 4. Source-to-variable map")
    lines.append("")
    lines.append("| Final variable | Tracked source | Note |")
    lines.append("| --- | --- | --- |")
    for row in variable_rows:
        lines.append(f"| `{row['variable']}` | {row['sources']} | {row['note']} |")
    lines.append("")

    lines.append("## 5. Variable-name variants")
    lines.append("")
    for group_name, hits in variants.items():
        lines.append(f"### {group_name}")
        lines.append("")
        for variant, files in hits.items():
            if files:
                lines.append(f"- `{variant}`")
                for file_path in files:
                    lines.append(f"  - {file_path}")
            else:
                lines.append(f"- `{variant}` - *(not present in any tracked file)*")
        lines.append("")

    lines.append("## 6. Cross-sectional outlier rule")
    lines.append("")
    lines.append("| Dataset | Rows | Unique countries | Country column |")
    lines.append("| --- | ---: | ---: | --- |")
    for row in outlier.get("rows", []):  # type: ignore[arg-type]
        lines.append(
            f"| `{row['dataset']}` | {row['rows']} | {row['countries']} | "
            f"`{row['country_col']}` |"
        )
    lines.append("")
    dropped = outlier.get("dropped_from_model_data4", [])
    lines.append(
        f"Countries in `model_data4` but missing from `model_data4_o` "
        f"({len(dropped)}): {_fmt_list(dropped)}."
    )
    if "regression_n" in outlier:
        lines.append("")
        lines.append(
            "Recomputed preferred OLS diagnostics from `model_data4.rds` after "
            "joining `tot2.rds` by country code."
        )
        lines.append("")
        lines.append(
            f"- Complete-case observations: {outlier['regression_n']}; "
            f"parameters including intercept: {outlier['regression_parameters']}."
        )
        lines.append(
            f"- Leverage threshold `2p/n`: {outlier['leverage_threshold']:.6f}; "
            f"Cook threshold `4/n`: {outlier['cook_threshold']:.6f}."
        )
        lines.append(
            f"- Rule `((leverage > 2p/n and abs(std_resid) > 2) or leverage > 0.2)` "
            f"matches dropped countries: {outlier['rule_matches_dropped']}."
        )
        lines.append("")
        lines.append("| Country | Code | Leverage | Std. residual | Cook's D | Rule flag | Dropped |")
        lines.append("| --- | --- | ---: | ---: | ---: | :---: | :---: |")
        diag = outlier["dropped_diagnostics"]
        for _, row in diag.sort_values("Country_Name").iterrows():
            lines.append(
                f"| {row['Country_Name']} | {row['Country_Code']} | "
                f"{row['leverage']:.6f} | {row['standardized_residual']:.6f} | "
                f"{row['cooks_distance']:.6f} | {bool(row['rule_flag'])} | "
                f"{bool(row['dropped_in_model_data4_o'])} |"
            )
    elif "regression_error" in outlier:
        lines.append("")
        lines.append(f"Regression diagnostic recomputation failed: {outlier['regression_error']}")
    lines.append("")

    lines.append("## 7. Model-wise missingness")
    lines.append("")
    lines.append(
        "Counts are based on `model_data4_o.rds` after joining `tot2.rds` where "
        "needed. They explain why the cleaned 157-country dataset becomes the "
        "121-122 observation OLS tables."
    )
    lines.append("")
    lines.append("| Model | Base rows | Complete rows | Dropped rows | Missing required vars | Variable-level missingness |")
    lines.append("| --- | ---: | ---: | ---: | --- | --- |")
    for row in missingness_rows:
        lines.append(
            f"| `{row['model']}` | {row['base_rows']} | {row['complete_rows']} | "
            f"{row['dropped_rows']} | {row['missing_vars']} | {row['variable_losses']} |"
        )
    lines.append("")

    lines.append("## 8. Per-file column detail")
    for summary in summaries:
        lines.append("")
        lines.append(f"### `{summary.relpath}` :: `{summary.object_name}`")
        lines.append("")
        lines.append("| Column | Dtype | Missing | % Missing |")
        lines.append("| --- | --- | ---: | ---: |")
        for name, dtype, n_missing, pct_missing in summary.columns:
            lines.append(f"| `{name}` | `{dtype}` | {n_missing} | {pct_missing}% |")
    lines.append("")

    lines.append("## 9. Contract artifact coverage")
    lines.append("")
    lines.append("| Path | Present |")
    lines.append("| --- | :---: |")
    for path, present in coverage:
        mark = "yes" if present else "**MISSING**"
        lines.append(f"| `{path}` | {mark} |")
    lines.append("")

    lines.append("## 10. Audit read errors")
    lines.append("")
    if errors:
        lines.append("| Path | Error |")
        lines.append("| --- | --- |")
        for path, error in errors:
            lines.append(f"| `{path}` | {error.replace(chr(10), ' ')} |")
    else:
        lines.append("No audit read errors.")
    lines.append("")

    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--report",
        type=Path,
        default=REPO / "docs" / "phase0_audit.md",
        help="Output markdown path.",
    )
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    summaries, frames, errors = collect_inputs()
    variants = build_name_variant_table(summaries)
    coverage = contract_coverage()
    report = render_report(summaries, frames, variants, coverage, errors)

    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(report, encoding="utf-8")

    missing_contract = sum(1 for _, present in coverage if not present)
    print(f"Wrote {args.report.relative_to(REPO)}")
    print(
        f"  files inspected: {len({summary.relpath for summary in summaries})}  "
        f"frames: {len(summaries)}  read errors: {len(errors)}  "
        f"missing contract paths: {missing_contract}"
    )

    return 1 if errors or missing_contract else 0


if __name__ == "__main__":
    sys.exit(main())

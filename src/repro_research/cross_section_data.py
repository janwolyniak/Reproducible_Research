from __future__ import annotations

import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd

from repro_research.paths import CROSS_SECTION_DATA_FILES, PROJECT_ROOT

COUNTRY_NAME_COLUMNS = (
    "Country_Name",
    "Country Name",
    "country_name",
    "Country",
    "country",
    "cname",
)
COUNTRY_CODE_COLUMNS = (
    "Country_Code",
    "Country Code",
    "country_text_id",
    "iso3",
    "iso3c",
)
MODEL_DATASETS = (
    "model_data",
    "model_data2",
    "model_data3",
    "model_data4",
    "model_data4_o",
)
SOURCE_DATASETS = (
    "CCCD_avg2010_19",
    "CCCD_detailed_avg2010_19",
    "CCCD_growth_final",
    "Democracy_avg2010_19",
    "cc_growth_yty",
    "edu_initial_Data",
    "fertility_Data",
    "GDPpc_current_initial_Data",
    "GDPpc_initial_in_2015usd_Data",
    "gdp_growth_Data",
    "geo_cepii",
    "gov_exp_Data",
    "gov_exp_edu_Data",
    "gov_milit_exp_Data",
    "inflation_cpi_Data",
    "inflation_gdpdeflator_Data",
    "investment_Data",
    "life_exp_initial_Data",
    "oth_char",
    "tot2",
    "tot_Data",
    "trade_Data",
)
REQUIRED_MODEL_COLUMNS = {
    "model_data4": (
        "Country_Name",
        "Country_Code",
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "trade",
        "gov_exp_reduced",
        "cc_total",
    ),
    "model_data4_o": (
        "Country_Name",
        "Country_Code",
        "GDP_growth",
        "GDPpc2015",
        "investment",
        "tot2",
        "trade",
        "gov_exp_reduced",
        "cc_total",
    ),
}
SHIFTED_LOG_COLUMNS = (
    "cc_total",
    "cc_total_lv",
    "cc_prop",
    "cc_basic",
    "cc_civil",
    "cc_polit",
)
PLAIN_LOG_COLUMNS = ("GDPpc2015", "GDPpc")
INTERACTION_COLUMNS = (("tot2", "trade"), ("tot", "trade"))


@dataclass(frozen=True)
class CrossSectionFrame:
    name: str
    path: Path
    frame: pd.DataFrame


@dataclass(frozen=True)
class CrossSectionPreparationResult:
    inventory_path: Path
    summary_path: Path
    model_data4_path: Path
    model_data4_o_path: Path
    data_dictionary_path: Path


def _read_rds_with_pyreadr(path: Path) -> pd.DataFrame:
    try:
        import pyreadr
    except ImportError as exc:
        raise RuntimeError("pyreadr is not installed") from exc

    result = pyreadr.read_r(str(path))
    if not result:
        raise ValueError(f"No objects found in {path}")
    frame = next(iter(result.values()))
    if not isinstance(frame, pd.DataFrame):
        raise TypeError(f"{path} does not contain a data frame")
    return frame


def _read_rds_with_rscript(path: Path) -> pd.DataFrame:
    script = """
args <- commandArgs(trailingOnly = TRUE)
input <- args[[1]]
output <- args[[2]]
obj <- readRDS(input)
if (!is.data.frame(obj)) {
  stop("RDS object is not a data.frame")
}
write.csv(obj, output, row.names = FALSE, na = "")
"""
    with tempfile.TemporaryDirectory() as tmpdir:
        csv_path = Path(tmpdir) / f"{path.stem}.csv"
        completed = subprocess.run(
            ["Rscript", "--vanilla", "-e", script, str(path), str(csv_path)],
            check=False,
            capture_output=True,
            text=True,
        )
        if completed.returncode != 0:
            message = completed.stderr.strip() or completed.stdout.strip()
            raise RuntimeError(f"Rscript could not read {path}: {message}")
        return pd.read_csv(csv_path)


def read_rds(path: Path) -> pd.DataFrame:
    """Read an RDS data frame with pyreadr, falling back to local Rscript."""
    try:
        return _read_rds_with_pyreadr(path)
    except RuntimeError as exc:
        if "pyreadr is not installed" not in str(exc):
            raise
        return _read_rds_with_rscript(path)


def read_cross_section_file(path: Path) -> pd.DataFrame:
    suffix = path.suffix.lower()
    if suffix == ".rds":
        return read_rds(path)
    if suffix == ".dta":
        return pd.read_stata(path)
    raise ValueError(f"Unsupported cross-sectional input type: {path}")


def cross_section_input_paths() -> list[Path]:
    return [PROJECT_ROOT / relative_path for relative_path in CROSS_SECTION_DATA_FILES]


def load_cross_section_inputs() -> dict[str, CrossSectionFrame]:
    frames: dict[str, CrossSectionFrame] = {}
    for path in cross_section_input_paths():
        frame = read_cross_section_file(path)
        frames[path.stem] = CrossSectionFrame(name=path.stem, path=path, frame=frame)
    return frames


def _first_present(columns: pd.Index, candidates: tuple[str, ...]) -> str | None:
    return next((candidate for candidate in candidates if candidate in columns), None)


def _column_role(column: str) -> str:
    if column in COUNTRY_NAME_COLUMNS:
        return "country_name"
    if column in COUNTRY_CODE_COLUMNS:
        return "country_code"
    if column.lower() == "year":
        return "year"
    if column in {"continent", "legal_old_o", "ever_colonized", "landlocked"}:
        return "categorical_or_grouping"
    if (
        column.startswith("cc_")
        or column.startswith("dj_")
        or column.startswith("gap_")
    ):
        return "constitutional_compliance"
    if column.startswith("v2x_") or column in {
        "rule_of_law",
        "ctrl_of_corr",
        "gov_eff",
        "pol_stab",
        "reg_q",
        "voice_and_acc",
    }:
        return "institutional_control"
    if column in {
        "GDP_growth",
        "fertility",
        "GDPpc2015",
        "GDPpc",
        "gov_exp",
        "gov_exp_edu",
        "gov_exp_milit",
        "gov_exp_reduced",
        "inf_cpi",
        "inf_def",
        "investment",
        "life_exp",
        "education",
        "tot",
        "tot2",
        "trade",
    }:
        return "economic_variable"
    return "source_variable"


def build_inventory(frames: dict[str, CrossSectionFrame]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for item in frames.values():
        frame = item.frame
        country_name = _first_present(frame.columns, COUNTRY_NAME_COLUMNS)
        country_code = _first_present(frame.columns, COUNTRY_CODE_COLUMNS)
        country_col = country_name or country_code
        countries = (
            int(frame[country_col].nunique(dropna=True)) if country_col else np.nan
        )
        rows.append(
            {
                "dataset": item.name,
                "path": str(item.path.relative_to(PROJECT_ROOT)).replace("\\", "/"),
                "rows": len(frame),
                "columns": len(frame.columns),
                "country_name_column": country_name,
                "country_code_column": country_code,
                "unique_countries": countries,
                "source_type": (
                    "derived_model" if item.name in MODEL_DATASETS else "source_extract"
                ),
            }
        )
    return pd.DataFrame(rows).sort_values(["source_type", "dataset"])


def normalize_country_fields(frame: pd.DataFrame) -> pd.DataFrame:
    out = frame.copy()
    country_name = _first_present(out.columns, COUNTRY_NAME_COLUMNS)
    country_code = _first_present(out.columns, COUNTRY_CODE_COLUMNS)
    if country_name and "country_name" not in out.columns:
        out["country_name"] = out[country_name].astype("string").str.strip()
    if country_code and "country_code" not in out.columns:
        out["country_code"] = out[country_code].astype("string").str.strip()
    return out


def coerce_cross_section_types(frame: pd.DataFrame) -> pd.DataFrame:
    out = frame.copy()
    for column in out.columns:
        if column in {
            "country_name",
            "country_code",
            *COUNTRY_NAME_COLUMNS,
            *COUNTRY_CODE_COLUMNS,
        }:
            out[column] = out[column].astype("string")
        elif column in {"continent", "legal_old_o"} or column == "landlocked":
            out[column] = out[column].astype("category")
        else:
            converted = pd.to_numeric(out[column], errors="coerce")
            out[column] = converted if converted.notna().any() else out[column]
    return out


def add_cross_section_transformations(frame: pd.DataFrame) -> pd.DataFrame:
    out = normalize_country_fields(coerce_cross_section_types(frame))
    if "terms_trade" not in out.columns:
        if "tot2" in out.columns:
            out["terms_trade"] = out["tot2"]
        elif "tot" in out.columns:
            out["terms_trade"] = out["tot"]
    for column in PLAIN_LOG_COLUMNS:
        if column in out.columns:
            out[f"log_{column}"] = np.log(out[column])
    for column in SHIFTED_LOG_COLUMNS:
        if column in out.columns:
            out[f"log_{column}_plus2"] = np.log(out[column] + 2)
    for left, right in INTERACTION_COLUMNS:
        if left in out.columns and right in out.columns:
            out[f"{left}_x_{right}"] = out[left] * out[right]
    if "terms_trade" in out.columns and "trade" in out.columns:
        out["terms_trade_x_trade"] = out["terms_trade"] * out["trade"]
    return out


def validate_model_schema(name: str, frame: pd.DataFrame) -> None:
    required = REQUIRED_MODEL_COLUMNS.get(name, ())
    missing = [column for column in required if column not in frame.columns]
    if missing:
        joined = ", ".join(missing)
        raise ValueError(f"{name} is missing required column(s): {joined}")


def prepare_model_datasets(
    frames: dict[str, CrossSectionFrame],
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    model_data4 = frames["model_data4"].frame
    model_data4_o = frames["model_data4_o"].frame
    validate_model_schema("model_data4", model_data4)
    validate_model_schema("model_data4_o", model_data4_o)

    prepared = add_cross_section_transformations(model_data4)
    prepared_o = add_cross_section_transformations(model_data4_o)

    outlier_codes = set(prepared["country_code"].dropna()) - set(
        prepared_o["country_code"].dropna()
    )
    prepared["is_outlier_removed_in_model_data4_o"] = prepared["country_code"].isin(
        outlier_codes
    )
    prepared_o["is_outlier_removed_in_model_data4_o"] = False

    summary = pd.DataFrame(
        [
            {
                "dataset": "model_data4",
                "rows": len(prepared),
                "unique_countries": prepared["country_code"].nunique(dropna=True),
                "model_ready_re4_rows": len(
                    prepared[
                        [
                            "GDP_growth",
                            "log_GDPpc2015",
                            "investment",
                            "terms_trade_x_trade",
                            "gov_exp_reduced",
                            "log_cc_total_plus2",
                            "terms_trade",
                            "trade",
                        ]
                    ].dropna()
                ),
                "removed_outlier_count": int(
                    prepared["is_outlier_removed_in_model_data4_o"].sum()
                ),
            },
            {
                "dataset": "model_data4_o",
                "rows": len(prepared_o),
                "unique_countries": prepared_o["country_code"].nunique(dropna=True),
                "model_ready_re4_rows": len(
                    prepared_o[
                        [
                            "GDP_growth",
                            "log_GDPpc2015",
                            "investment",
                            "terms_trade_x_trade",
                            "gov_exp_reduced",
                            "log_cc_total_plus2",
                            "terms_trade",
                            "trade",
                        ]
                    ].dropna()
                ),
                "removed_outlier_count": 0,
            },
        ]
    )
    return prepared, prepared_o, summary


def build_data_dictionary(frames: dict[str, CrossSectionFrame]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for item in frames.values():
        for column in item.frame.columns:
            rows.append(
                {
                    "variable": column,
                    "dataset": item.name,
                    "role": _column_role(column),
                    "dtype": str(item.frame[column].dtype),
                    "missing": int(item.frame[column].isna().sum()),
                    "non_missing": int(item.frame[column].notna().sum()),
                }
            )
    return pd.DataFrame(rows).sort_values(["role", "variable", "dataset"])


def render_data_dictionary(dictionary: pd.DataFrame) -> str:
    lines = [
        "## Cross-Sectional Model Variables",
        "",
        "This section covers the cross-sectional inputs prepared for Kinga's",
        "Phase 2 work. It is appended by `scripts/run_cross_section_data.py`.",
        "",
        "Canonical loader conventions:",
        "",
        "- Country-name variants are exposed as `country_name`.",
        "- Country-code variants are exposed as `country_code`.",
        "- Cross-sectional model data add `log_GDPpc2015`, shifted compliance logs",
        "  such as `log_cc_total_plus2`, and interaction terms such as",
        "  `terms_trade_x_trade`. The canonical `terms_trade` uses `tot2` when",
        "  available and falls back to `tot` for tracked artifacts where `tot2`",
        "  is absent.",
        "- `model_data4_o` is the outlier-filtered version of `model_data4`.",
        "",
        "| Variable | Role | Datasets | Missing values |",
        "| --- | --- | --- | ---: |",
    ]
    grouped = dictionary.groupby(["variable", "role"], dropna=False)
    for (variable, role), group in grouped:
        datasets = ", ".join(sorted(group["dataset"].astype(str).unique()))
        missing = int(group["missing"].sum())
        lines.append(f"| `{variable}` | {role} | {datasets} | {missing} |")
    lines.append("")
    return "\n".join(lines)


def write_cross_section_preparation_outputs() -> CrossSectionPreparationResult:
    frames = load_cross_section_inputs()
    inventory = build_inventory(frames)
    prepared, prepared_o, summary = prepare_model_datasets(frames)
    dictionary = build_data_dictionary(frames)

    output_dir = PROJECT_ROOT / "outputs" / "cross_section"
    intermediate_dir = PROJECT_ROOT / "outputs" / "intermediate"
    docs_dir = PROJECT_ROOT / "docs"
    output_dir.mkdir(parents=True, exist_ok=True)
    intermediate_dir.mkdir(parents=True, exist_ok=True)
    docs_dir.mkdir(parents=True, exist_ok=True)

    inventory_path = output_dir / "cross_section_data_inventory.csv"
    summary_path = output_dir / "cross_section_preparation_summary.csv"
    model_data4_path = intermediate_dir / "cross_section_model_data4_prepared.csv"
    model_data4_o_path = intermediate_dir / "cross_section_model_data4_o_prepared.csv"
    data_dictionary_path = docs_dir / "_data_dictionary_cross_section.md"

    inventory.to_csv(inventory_path, index=False)
    summary.to_csv(summary_path, index=False)
    prepared.to_csv(model_data4_path, index=False)
    prepared_o.to_csv(model_data4_o_path, index=False)
    data_dictionary_path.write_text(
        render_data_dictionary(dictionary).rstrip() + "\n",
        encoding="utf-8",
    )

    return CrossSectionPreparationResult(
        inventory_path=inventory_path,
        summary_path=summary_path,
        model_data4_path=model_data4_path,
        model_data4_o_path=model_data4_o_path,
        data_dictionary_path=data_dictionary_path,
    )

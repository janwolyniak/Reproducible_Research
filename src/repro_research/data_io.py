from __future__ import annotations

import json
import subprocess
import tempfile
from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path

import pandas as pd

from repro_research.paths import (
    CROSS_SECTION_DATA_FILES,
    PANEL_DATA_FILES,
    PROJECT_ROOT,
)

COUNTRY_NAME_CANDIDATES = (
    "Country Name",
    "Country_Name",
    "country_name",
    "cname",
)
COUNTRY_CODE_CANDIDATES = (
    "Country Code",
    "Country_Code",
    "Country",
    "iso3",
    "iso3c",
    "country_text_id",
)
YEAR_CANDIDATES = ("Year", "year")


@dataclass(frozen=True)
class DatasetSpec:
    name: str
    group: str
    relative_path: str
    expected_grain: str

    @property
    def path(self) -> Path:
        return PROJECT_ROOT / self.relative_path


@dataclass(frozen=True)
class DatasetInventoryRow:
    group: str
    dataset: str
    relative_path: str
    object_name: str
    rows: int
    columns: int
    country_name_column: str
    country_code_column: str
    year_column: str
    missing_cells: int
    column_names_json: str
    dtypes_json: str

    def as_dict(self) -> dict[str, object]:
        return {
            "group": self.group,
            "dataset": self.dataset,
            "relative_path": self.relative_path,
            "object_name": self.object_name,
            "rows": self.rows,
            "columns": self.columns,
            "country_name_column": self.country_name_column,
            "country_code_column": self.country_code_column,
            "year_column": self.year_column,
            "missing_cells": self.missing_cells,
            "column_names_json": self.column_names_json,
            "dtypes_json": self.dtypes_json,
        }


def build_dataset_specs() -> tuple[DatasetSpec, ...]:
    cross_section = tuple(
        DatasetSpec(
            name=Path(path).stem,
            group="cross_section",
            relative_path=path,
            expected_grain="country",
        )
        for path in CROSS_SECTION_DATA_FILES
        if Path(path).suffix.lower() in {".rds", ".dta"}
    )
    panel = tuple(
        DatasetSpec(
            name=Path(path).stem,
            group="panel",
            relative_path=path,
            expected_grain="country_year",
        )
        for path in PANEL_DATA_FILES
    )
    return (*cross_section, *panel)


def find_first_column(columns: Iterable[str], candidates: Iterable[str]) -> str:
    column_set = set(columns)
    return next((candidate for candidate in candidates if candidate in column_set), "")


def stable_dtype_label(series: pd.Series) -> str:
    """Return a reader-stable dtype label for generated inventories."""
    if isinstance(series.dtype, pd.CategoricalDtype) or pd.api.types.is_object_dtype(
        series
    ):
        return "str"
    if str(series.dtype) == "float64" and not series.hasnans:
        values = series.dropna()
        if not values.empty and bool(((values % 1).abs() < 1e-12).all()):
            return "int64"
    return str(series.dtype)


def read_rds_objects(path: Path) -> dict[str, pd.DataFrame]:
    try:
        import pyreadr
    except ImportError:
        return {path.stem: read_rds_with_rscript(path)}

    result = pyreadr.read_r(str(path))
    frames = {
        name if name is not None else path.stem: frame
        for name, frame in result.items()
        if isinstance(frame, pd.DataFrame)
    }
    if not frames:
        raise ValueError(f"No pandas-compatible data frames found in {path}.")
    return frames


def read_rds_with_rscript(path: Path) -> pd.DataFrame:
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


def read_rds_dataframe(path: Path, object_name: str | None = None) -> pd.DataFrame:
    frames = read_rds_objects(path)
    if object_name:
        try:
            return frames[object_name].copy()
        except KeyError as exc:
            available = ", ".join(sorted(frames))
            raise KeyError(
                f"{object_name} not found in {path}; available: {available}"
            ) from exc

    if len(frames) != 1:
        available = ", ".join(sorted(frames))
        raise ValueError(
            f"{path} contains multiple objects; choose one of: {available}"
        )
    return next(iter(frames.values())).copy()


def read_tabular_source(path: Path) -> dict[str, pd.DataFrame]:
    suffix = path.suffix.lower()
    if suffix == ".rds":
        return read_rds_objects(path)
    if suffix == ".dta":
        return {path.stem: pd.read_stata(path)}
    if suffix == ".csv":
        return {path.stem: pd.read_csv(path)}
    raise ValueError(f"Unsupported tabular source: {path}")


def load_dataset(spec: DatasetSpec, object_name: str | None = None) -> pd.DataFrame:
    if spec.path.suffix.lower() == ".rds":
        return read_rds_dataframe(spec.path, object_name=object_name)

    frames = read_tabular_source(spec.path)
    if object_name:
        return frames[object_name].copy()
    return next(iter(frames.values())).copy()


def summarize_dataset(spec: DatasetSpec) -> list[DatasetInventoryRow]:
    rows: list[DatasetInventoryRow] = []
    for object_name, frame in read_tabular_source(spec.path).items():
        columns = [str(column) for column in frame.columns]
        rows.append(
            DatasetInventoryRow(
                group=spec.group,
                dataset=spec.name,
                relative_path=spec.relative_path,
                object_name=object_name,
                rows=int(frame.shape[0]),
                columns=int(frame.shape[1]),
                country_name_column=find_first_column(
                    columns,
                    COUNTRY_NAME_CANDIDATES,
                ),
                country_code_column=find_first_column(
                    columns,
                    COUNTRY_CODE_CANDIDATES,
                ),
                year_column=find_first_column(columns, YEAR_CANDIDATES),
                missing_cells=int(frame.isna().sum().sum()),
                column_names_json=json.dumps(columns, ensure_ascii=True),
                dtypes_json=json.dumps(
                    {
                        str(column): stable_dtype_label(frame[column])
                        for column in frame.columns
                    },
                    ensure_ascii=True,
                    sort_keys=True,
                ),
            )
        )
    return rows


def build_inventory(specs: Iterable[DatasetSpec]) -> list[DatasetInventoryRow]:
    inventory: list[DatasetInventoryRow] = []
    for spec in specs:
        inventory.extend(summarize_dataset(spec))
    return inventory

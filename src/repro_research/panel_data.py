"""Panel data loaders for the reproduction pipeline.

Three tracked ``.rds`` inputs cover the panel reproduction:

- ``panel/data_panel/model_data.rds`` -- main 1990-2020 panel (4906 x 18).
- ``panel/data_panel/model_data2.rds`` -- ``model_data`` plus continent, legal
  origin and ever-colonised columns used by alternative cuts (4786 x 21).
- ``panel/data_panel/small_plm.rds`` -- restricted 2010-2019 panel that mirrors
  the cross-section variable set (1617 x 22).

Each loader applies the same minimal normalisation: read the ``.rds`` via
the shared data reader, cast ``Year`` to integer, coerce ``fertility`` to
``float`` (the R script does this manually for ``small_plm``), strip ``Country``
whitespace, add the canonical ``country`` column via
:mod:`repro_research.country_naming`, and assert ``(country, Year)`` uniqueness.
The panel ``MultiIndex`` is set during model fitting when needed.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import pandas as pd

from repro_research.country_naming import to_canonical
from repro_research.data_io import read_rds_dataframe
from repro_research.paths import PROJECT_ROOT

_MAIN_REQUIRED = (
    "Country",
    "Year",
    "GDPgrowth",
    "cc_total",
    "cc_total_lv",
    "cc_basic",
    "cc_civil",
    "cc_polit",
    "cc_prop",
    "GDPpc2015",
    "fertility",
    "gov_exp_reduced",
    "inflation",
    "investment",
    "tot_growth",
    "trade",
    "education",
)

_ALT_REQUIRED = _MAIN_REQUIRED + ("continent", "legal_old_o", "ever_colonized")

_SMALL_REQUIRED = (
    "Country",
    "Year",
    "GDPgrowth",
    "cc_total",
    "cc_total_lv",
    "cc_basic",
    "cc_civil",
    "cc_polit",
    "cc_prop",
    "GDPpc2015",
    "fertility",
    "gov_exp_reduced",
    "inflation",
    "investment",
    "tot_growth",
    "trade",
    "education",
    "life_exp",
)


@dataclass(frozen=True)
class PanelSpec:
    rel_path: str
    required_columns: tuple[str, ...]
    expected_rows: int
    expected_year_min: int
    expected_year_max: int


PANEL_SPECS: dict[str, PanelSpec] = {
    "main": PanelSpec(
        rel_path="panel/data_panel/model_data.rds",
        required_columns=_MAIN_REQUIRED,
        expected_rows=4906,
        expected_year_min=1990,
        expected_year_max=2020,
    ),
    "alt": PanelSpec(
        rel_path="panel/data_panel/model_data2.rds",
        required_columns=_ALT_REQUIRED,
        expected_rows=4786,
        expected_year_min=1990,
        expected_year_max=2020,
    ),
    "small": PanelSpec(
        rel_path="panel/data_panel/small_plm.rds",
        required_columns=_SMALL_REQUIRED,
        expected_rows=1617,
        expected_year_min=2010,
        expected_year_max=2019,
    ),
}


def _read_single_frame(path: Path) -> pd.DataFrame:
    return read_rds_dataframe(path)


def _normalise(df: pd.DataFrame, spec: PanelSpec) -> pd.DataFrame:
    missing = [c for c in spec.required_columns if c not in df.columns]
    if missing:
        raise KeyError(
            f"Panel input {spec.rel_path} is missing required columns: {missing}"
        )

    out = df.copy()
    out["Year"] = out["Year"].astype("int64")
    out["fertility"] = pd.to_numeric(out["fertility"], errors="coerce").astype(
        "float64"
    )
    out["Country"] = out["Country"].astype("string").str.strip()
    out = to_canonical(out, kind="panel")

    duplicates = out.duplicated(subset=["country", "Year"])
    if duplicates.any():
        offenders = (
            out.loc[duplicates, ["country", "Year"]].head(5).to_dict(orient="records")
        )
        raise ValueError(
            f"Panel input {spec.rel_path} contains duplicate (country, Year) rows: "
            f"{offenders}"
        )

    if "legal_old_o" in out.columns and isinstance(
        out["legal_old_o"].dtype, pd.CategoricalDtype
    ):
        out["legal_old_o"] = out["legal_old_o"].astype("string")

    return out.sort_values(["country", "Year"], ignore_index=True)


def load_panel(kind: str = "main") -> pd.DataFrame:
    """Load and normalise one of the tracked panel datasets.

    ``kind`` must be one of ``"main"``, ``"alt"``, or ``"small"``.
    """
    if kind not in PANEL_SPECS:
        raise ValueError(
            f"Unknown panel kind {kind!r}; expected one of {list(PANEL_SPECS)}"
        )
    spec = PANEL_SPECS[kind]
    raw = _read_single_frame(PROJECT_ROOT / spec.rel_path)
    return _normalise(raw, spec)


def load_panel_main() -> pd.DataFrame:
    return load_panel("main")


def load_panel_alt() -> pd.DataFrame:
    return load_panel("alt")


def load_panel_small() -> pd.DataFrame:
    return load_panel("small")

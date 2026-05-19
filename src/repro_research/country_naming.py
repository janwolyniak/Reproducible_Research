"""Canonical country naming shared by the panel and cross-section pipelines.

The tracked inputs use several country column conventions:

- Panel ``.rds`` files (``model_data``, ``model_data2``, ``small_plm``) carry
  both ``Country`` (human-readable) and ``cname`` (lowercase short form).
- Cross-section ``model_data*.rds`` files use ``Country Name`` /
  ``Country_Name`` plus ``Country Code`` / ``Country_Code``.
- The standalone World Bank-style extracts (``gdp_growth_Data.rds``,
  ``fertility_Data.rds`` and friends) use ``Country Name`` + ``Country Code``.

This module normalises all of them to a single ``country`` column, leaving the
source columns untouched so callers can still cross-reference the raw inputs.
"""

from __future__ import annotations

from typing import Literal

import pandas as pd

DatasetKind = Literal["panel", "cross_section_model", "world_bank_extract"]

_COUNTRY_NAME_BY_KIND: dict[DatasetKind, tuple[str, ...]] = {
    "panel": ("Country", "cname"),
    "cross_section_model": ("Country Name", "Country_Name", "Country", "cname"),
    "world_bank_extract": ("Country Name", "Country_Name"),
}

_COUNTRY_CODE_BY_KIND: dict[DatasetKind, tuple[str, ...]] = {
    "panel": (),
    "cross_section_model": ("Country Code", "Country_Code"),
    "world_bank_extract": ("Country Code", "Country_Code"),
}


def _pick(df: pd.DataFrame, candidates: tuple[str, ...]) -> str | None:
    for column in candidates:
        if column in df.columns:
            return column
    return None


def to_canonical(df: pd.DataFrame, kind: DatasetKind) -> pd.DataFrame:
    """Return ``df`` with an added canonical ``country`` column.

    The source country column is detected from the kind-specific candidate list
    and copied (with trailing whitespace stripped) into ``country``. When a
    country code column is available it is also surfaced as ``iso3``. The
    original columns are left in place so downstream code can keep using them.
    """
    name_col = _pick(df, _COUNTRY_NAME_BY_KIND[kind])
    if name_col is None:
        raise KeyError(
            f"No country name column found for kind={kind!r}. "
            f"Tried {_COUNTRY_NAME_BY_KIND[kind]}; got {list(df.columns)}"
        )

    result = df.copy()
    result["country"] = result[name_col].astype("string").str.strip()

    code_col = _pick(df, _COUNTRY_CODE_BY_KIND[kind])
    if code_col is not None:
        result["iso3"] = result[code_col].astype("string").str.strip()

    return result

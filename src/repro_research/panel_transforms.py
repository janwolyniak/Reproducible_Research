"""Panel transformations used by the Phase 4 model specifications.

The R workflow expresses transformations inside ``plm`` formulas, e.g.
``log(cc_total + 2)`` and ``log(lag(GDPpc2015, 2))``. The functions below
materialise the same transformations as explicit columns so the Python models
can stay declarative and the data dictionary can name them.

All functions are non-mutating: they return a new DataFrame and leave the input
untouched.
"""

from __future__ import annotations

from collections.abc import Iterable

import numpy as np
import pandas as pd

DEFAULT_COMPLIANCE_VARS = (
    "cc_total",
    "cc_total_lv",
    "cc_basic",
    "cc_civil",
    "cc_polit",
    "cc_prop",
)


def add_log_compliance(
    df: pd.DataFrame,
    variables: Iterable[str] = DEFAULT_COMPLIANCE_VARS,
    shift: float = 2.0,
) -> pd.DataFrame:
    """Add ``log_<var>`` columns equal to ``log(var + shift)``.

    The ``+ shift`` term comes straight from the paper (the compliance indices
    can be negative). Only ``variables`` that exist on ``df`` are processed so
    the same call works for the main, alt, and small panels.
    """
    out = df.copy()
    for var in variables:
        if var not in out.columns:
            continue
        shifted = out[var] + shift
        if (shifted.dropna() <= 0).any():
            raise ValueError(
                f"log_{var} would require log of a non-positive value after "
                f"shift={shift}; check the source column."
            )
        out[f"log_{var}"] = np.log(shifted)
    return out


def add_lag_log_gdppc(
    df: pd.DataFrame,
    column: str = "GDPpc2015",
    lag: int = 2,
    country_col: str = "country",
    year_col: str = "Year",
) -> pd.DataFrame:
    """Add ``lag_log_<column>`` = ``log(lag(column, lag))`` within each country.

    Matches the R formula ``log(lag(GDPpc2015, 2))`` used by ``plm``: the lag is
    applied first within the country panel, then the logarithm. The first
    ``lag`` years of every country are therefore NaN.
    """
    if column not in df.columns:
        raise KeyError(f"Column {column!r} not present on the panel frame")

    out = df.sort_values([country_col, year_col]).copy()
    lagged = out.groupby(country_col, observed=True)[column].shift(lag)
    out[f"lag_{column}"] = lagged
    with np.errstate(invalid="ignore", divide="ignore"):
        out[f"lag_log_{column}"] = np.log(lagged.where(lagged > 0))
    return out.reset_index(drop=True)


def add_tot_trade_interaction(
    df: pd.DataFrame,
    tot_col: str = "tot_growth",
    trade_col: str = "trade",
    out_col: str = "tot_growth_x_trade",
) -> pd.DataFrame:
    """Add the ``tot_growth * trade`` interaction column."""
    for col in (tot_col, trade_col):
        if col not in df.columns:
            raise KeyError(f"Column {col!r} not present on the panel frame")
    out = df.copy()
    out[out_col] = out[tot_col] * out[trade_col]
    return out


def subset_period(
    df: pd.DataFrame,
    start: int,
    end: int,
    year_col: str = "Year",
) -> pd.DataFrame:
    """Return rows whose year falls in the inclusive ``[start, end]`` range."""
    mask = df[year_col].between(start, end, inclusive="both")
    return df.loc[mask].reset_index(drop=True)


def subset_by_continent(df: pd.DataFrame, name: str) -> pd.DataFrame:
    """Return rows where ``continent == name``. Requires the alt panel."""
    if "continent" not in df.columns:
        raise KeyError(
            "continent column is only present on the alt panel (model_data2)."
        )
    return df.loc[df["continent"] == name].reset_index(drop=True)


def subset_by_legal_origin(df: pd.DataFrame, code: str) -> pd.DataFrame:
    """Return rows where ``legal_old_o == code``. Requires the alt panel."""
    if "legal_old_o" not in df.columns:
        raise KeyError(
            "legal_old_o column is only present on the alt panel (model_data2)."
        )
    return df.loc[df["legal_old_o"] == code].reset_index(drop=True)


def prepare_for_models(df: pd.DataFrame) -> pd.DataFrame:
    """Apply the canonical chain of transformations used by Phase 4 models."""
    out = add_log_compliance(df)
    out = add_lag_log_gdppc(out)
    out = add_tot_trade_interaction(out)
    return out

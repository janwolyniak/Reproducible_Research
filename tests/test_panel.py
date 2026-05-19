"""Tests for the panel loaders and transformations."""

from __future__ import annotations

import math

import numpy as np
import pandas as pd
import pytest

from repro_research.panel_data import PANEL_SPECS, load_panel
from repro_research.panel_transforms import (
    add_lag_log_gdppc,
    add_log_compliance,
    add_tot_trade_interaction,
    prepare_for_models,
    subset_period,
)


@pytest.fixture(scope="module")
def main_panel() -> pd.DataFrame:
    return load_panel("main")


@pytest.fixture(scope="module")
def alt_panel() -> pd.DataFrame:
    return load_panel("alt")


@pytest.fixture(scope="module")
def small_panel() -> pd.DataFrame:
    return load_panel("small")


@pytest.mark.parametrize("kind", list(PANEL_SPECS))
def test_loader_shape_and_coverage(kind: str) -> None:
    spec = PANEL_SPECS[kind]
    df = load_panel(kind)
    assert len(df) == spec.expected_rows
    assert int(df["Year"].min()) == spec.expected_year_min
    assert int(df["Year"].max()) == spec.expected_year_max
    assert df["Year"].dtype == np.int64
    assert df["fertility"].dtype == np.float64
    assert df["country"].notna().all()
    assert not df.duplicated(subset=["country", "Year"]).any()


def test_main_panel_country_count(main_panel: pd.DataFrame) -> None:
    assert main_panel["country"].nunique() == 170


def test_small_panel_country_count(small_panel: pd.DataFrame) -> None:
    assert small_panel["country"].nunique() == 167


def test_alt_panel_has_continent_and_legal(alt_panel: pd.DataFrame) -> None:
    assert {"continent", "legal_old_o", "ever_colonized"} <= set(alt_panel.columns)
    # legal_old_o is exposed as string after normalisation
    assert alt_panel["legal_old_o"].dtype == "string"


def test_add_log_compliance_positive(main_panel: pd.DataFrame) -> None:
    transformed = add_log_compliance(main_panel)
    for col in ("log_cc_total", "log_cc_basic", "log_cc_prop"):
        assert col in transformed.columns
        values = transformed[col].dropna()
        assert (values.notna()).all()
        assert math.isfinite(values.min()) and math.isfinite(values.max())


def test_add_lag_log_gdppc_first_two_years_nan(main_panel: pd.DataFrame) -> None:
    transformed = add_lag_log_gdppc(main_panel)
    by_country = transformed.groupby("country", observed=True)
    for _, group in by_country:
        first_two = group.head(2)["lag_GDPpc2015"]
        assert first_two.isna().all()


def test_add_tot_trade_interaction(main_panel: pd.DataFrame) -> None:
    transformed = add_tot_trade_interaction(main_panel)
    expected = main_panel["tot_growth"] * main_panel["trade"]
    pd.testing.assert_series_equal(
        transformed["tot_growth_x_trade"].reset_index(drop=True),
        expected.reset_index(drop=True),
        check_names=False,
    )


def test_subset_period_matches_small_panel_year_window(
    main_panel: pd.DataFrame, small_panel: pd.DataFrame
) -> None:
    small_years = subset_period(main_panel, 2010, 2019)
    assert int(small_years["Year"].min()) == 2010
    assert int(small_years["Year"].max()) == 2019
    # The small_plm panel restricts countries further, so it should be a subset
    # of the year-window slice when matched on country-year pairs.
    main_keys = set(
        map(tuple, small_years[["country", "Year"]].itertuples(index=False))
    )
    small_keys = set(
        map(tuple, small_panel[["country", "Year"]].itertuples(index=False))
    )
    assert small_keys <= main_keys


def test_prepare_for_models_adds_expected_columns(main_panel: pd.DataFrame) -> None:
    prepared = prepare_for_models(main_panel)
    expected = {
        "log_cc_total",
        "log_cc_basic",
        "log_cc_civil",
        "log_cc_polit",
        "log_cc_prop",
        "lag_GDPpc2015",
        "lag_log_GDPpc2015",
        "tot_growth_x_trade",
    }
    assert expected <= set(prepared.columns)

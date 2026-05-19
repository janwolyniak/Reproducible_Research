from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.cross_section import (  # noqa: E402
    MODEL_SPECS,
    correlation_matrix,
    diagnostics_table,
    fit_model,
    model_summary_table,
    select_model_group,
)
from repro_research.cross_section_data import (  # noqa: E402
    add_cross_section_transformations,
    load_cross_section_inputs,
)


def test_preferred_cross_section_model_has_expected_sample_size() -> None:
    frames = load_cross_section_inputs()
    prepared = add_cross_section_transformations(frames["model_data4_o"].frame)
    spec = next(item for item in MODEL_SPECS if item.name == "re4_o")

    result = fit_model(prepared, spec)

    assert int(result.nobs) == 122
    assert "log_cc_total_plus2" in result.params.index


def test_correlation_matrix_uses_complete_observations() -> None:
    frames = load_cross_section_inputs()
    prepared = add_cross_section_transformations(frames["model_data4"].frame)

    result = correlation_matrix(prepared)

    expected = prepared[list(result.columns)].dropna().corr(method="spearman")
    pd.testing.assert_frame_equal(result, expected)


def test_publication_model_groups_match_phase3_targets() -> None:
    frames = load_cross_section_inputs()
    prepared = {
        name: add_cross_section_transformations(item.frame)
        for name, item in frames.items()
        if name.startswith("model_data")
    }
    model_table, _ = model_summary_table(MODEL_SPECS, prepared)

    main = select_model_group(model_table, "ols_main", ("re2_o", "re4_o"))
    outlier = select_model_group(
        model_table,
        "ols_outlier_filtered",
        ("re1_o", "re2_o", "re3_o", "re4_o", "re5_o", "re6_o", "re7_o"),
    )

    assert tuple(main["model"].drop_duplicates()) == ("re2_o", "re4_o")
    assert set(outlier["model"]) == {
        "re1_o",
        "re2_o",
        "re3_o",
        "re4_o",
        "re5_o",
        "re6_o",
        "re7_o",
    }


def test_diagnostics_include_autocorrelation_p_value() -> None:
    frames = load_cross_section_inputs()
    prepared = add_cross_section_transformations(frames["model_data4_o"].frame)
    spec = next(item for item in MODEL_SPECS if item.name == "re4_o")
    result = fit_model(prepared, spec)

    diagnostics = diagnostics_table({"re4_o": result})

    assert "durbin_watson_stat" in diagnostics.columns
    assert "breusch_godfrey_lm_lag1_p_value" in diagnostics.columns
    assert diagnostics.loc[0, "breusch_godfrey_lm_lag1_p_value"] >= 0

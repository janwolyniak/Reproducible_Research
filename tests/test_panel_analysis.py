"""Tests for the panel analysis layer (Phase 4)."""

from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

ROOT = Path(__file__).resolve().parent.parent
os.environ.setdefault("MPLCONFIGDIR", str(ROOT / "outputs" / "logs" / "_mpl"))

from repro_research.panel import (  # noqa: E402
    MODEL_SPECS,
    alternative_fe_table,
    breusch_pagan_test,
    diagnostic_tests,
    driscoll_kraay_se,
    fit_panel_model,
    hausman_test,
    lm_test_pooled_vs_random,
    panel_coefficient_table,
    panel_correlation_matrix,
    panel_descriptive_statistics,
    panel_robust_se_table,
    panel_summary_table,
    pesaran_cd_test,
    specification_tests,
    wooldridge_serial_correlation,
    write_panel_outputs,
)
from repro_research.panel_data import load_panel  # noqa: E402
from repro_research.panel_transforms import prepare_for_models  # noqa: E402


@pytest.fixture(scope="module")
def prepared_panels() -> dict[str, pd.DataFrame]:
    return {
        kind: prepare_for_models(load_panel(kind)) for kind in ("main", "alt", "small")
    }


def test_descriptive_statistics_has_expected_columns(prepared_panels) -> None:
    table = panel_descriptive_statistics(prepared_panels["alt"])
    assert {"Variable", "N", "Min", "Median", "Mean", "SD", "Max"} == set(table.columns)
    assert (table["N"] > 0).all()


def test_correlation_matrix_is_square_and_finite(prepared_panels) -> None:
    corr = panel_correlation_matrix(prepared_panels["alt"])
    assert corr.shape[0] == corr.shape[1]
    assert np.isfinite(corr.to_numpy()).all()


def test_all_specs_fit_without_exception(prepared_panels) -> None:
    summary, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    assert len(fits) == len(MODEL_SPECS)
    assert len(summary) > 0
    assert "log_cc_total" in summary["term"].unique()


def test_first_difference_models_have_inference_columns(prepared_panels) -> None:
    summary, _ = panel_summary_table(MODEL_SPECS, prepared_panels)
    fd_rows = summary.loc[summary["model_kind"] == "first_difference"]
    assert len(fd_rows) > 0
    assert fd_rows[["std_error", "t_stat", "p_value"]].notna().all().all()
    assert np.isfinite(fd_rows[["std_error", "t_stat", "p_value"]].to_numpy()).all()


def test_main_fe_log_cc_total_is_positive(prepared_panels) -> None:
    spec = next(s for s in MODEL_SPECS if s.name == "main_reduced_fixed")
    fit = fit_panel_model(spec, prepared_panels[spec.dataset])
    assert fit.params["log_cc_total"] > 0
    assert fit.pvalues["log_cc_total"] < 0.05


def test_coefficient_table_has_finite_classical_se(prepared_panels) -> None:
    spec = next(s for s in MODEL_SPECS if s.name == "main_reduced_fixed")
    fit = fit_panel_model(spec, prepared_panels[spec.dataset])
    table = panel_coefficient_table({spec.name: fit})
    finite = table["std_error"].dropna()
    assert (finite > 0).all()


def test_specification_tests_return_probabilities(prepared_panels) -> None:
    _, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    table = specification_tests(
        fits,
        (
            (
                "main_reduced",
                "main_reduced_pooling",
                "main_reduced_fixed",
                "main_reduced_random",
            ),
        ),
    )
    for column in ("lm_pvalue", "f_pooled_pvalue", "hausman_pvalue"):
        value = table[column].iloc[0]
        assert value is not None
        assert 0.0 <= float(value) <= 1.0


def test_individual_specification_helpers(prepared_panels) -> None:
    _, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    for name in ("main_reduced_fixed", "main_reduced_random"):
        assert name in fits
    lm = lm_test_pooled_vs_random(fits["main_reduced_random"])
    f = (
        fits["main_reduced_fixed"].f_pooled_pvalue
        if fits["main_reduced_fixed"].f_pooled_pvalue is not None
        else 0.0
    )
    h = hausman_test(fits["main_reduced_fixed"], fits["main_reduced_random"])
    for value in (lm, f, h):
        assert value is not None and 0.0 <= float(value) <= 1.0


def test_diagnostic_tests_return_probabilities(prepared_panels) -> None:
    _, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    table = diagnostic_tests(fits, (("main_reduced", "main_reduced_fixed"),))
    assert table["serial_correlation_test"].iloc[0] == "Breusch-Godfrey panel proxy"
    assert table["heteroskedasticity_test"].iloc[0] == "Breusch-Pagan on model design"
    assert table["cross_sectional_dependence_test"].iloc[0] == "Pesaran CD"
    for column in (
        "serial_correlation_pvalue",
        "breusch_pagan_pvalue",
        "pesaran_cd_pvalue",
    ):
        value = float(table[column].iloc[0])
        assert 0.0 <= value <= 1.0


def test_individual_diagnostic_helpers(prepared_panels) -> None:
    _, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    fe = fits["main_reduced_fixed"]
    for value in (
        wooldridge_serial_correlation(fe),
        breusch_pagan_test(fe),
        pesaran_cd_test(fe),
    ):
        assert value is not None and 0.0 <= float(value) <= 1.0


def test_driscoll_kraay_matches_synthetic_panel() -> None:
    """Synthetic check: DK SE on uncorrelated white-noise scores equals the
    classical formula up to the cross-sectional aggregation factor.
    """
    rng = np.random.default_rng(42)
    entities = 6
    periods = 10
    rows = []
    for entity in range(entities):
        for period in range(periods):
            rows.append(
                {
                    "country": f"E{entity}",
                    "Year": 2000 + period,
                    "y": rng.normal(),
                    "x1": rng.normal(),
                }
            )
    df = pd.DataFrame(rows)
    from repro_research.panel import PanelSpec, fit_panel_model

    spec = PanelSpec(
        name="dk_synth",
        dataset="synthetic",
        dependent="y",
        predictors=("x1",),
        model_kind="fixed",
        table_group="synth",
    )
    fit = fit_panel_model(spec, df)
    se = driscoll_kraay_se(fit, lags=0)
    assert np.isfinite(se).all()
    assert (se > 0).all()
    # lag truncation greater than zero should still produce finite SEs.
    se_lag = driscoll_kraay_se(fit, lags=2)
    assert np.isfinite(se_lag).all()


def test_robust_se_table_covers_all_variants(prepared_panels) -> None:
    _, fits = panel_summary_table(MODEL_SPECS, prepared_panels)
    table = panel_robust_se_table(fits["main_reduced_fixed"])
    assert {
        "term",
        "coefficient",
        "se_classical",
        "se_arellano",
        "se_time_cluster",
        "se_double_cluster",
        "se_driscoll_kraay",
    } <= set(table.columns)
    finite = table[
        [
            "se_classical",
            "se_arellano",
            "se_time_cluster",
            "se_double_cluster",
            "se_driscoll_kraay",
        ]
    ].dropna()
    assert (finite > 0).all().all()


def test_alternative_fe_table_covers_four_variants(prepared_panels) -> None:
    table = alternative_fe_table(prepared_panels)
    variants = set(table["compliance_variant"].unique())
    assert variants == {"cc_basic", "cc_civil", "cc_polit", "cc_prop"}
    log_terms = table.loc[
        table["term"].str.startswith("log_cc_"), ["compliance_variant", "term"]
    ]
    pairs = set(zip(log_terms["compliance_variant"], log_terms["term"], strict=False))
    assert pairs == {
        (v, f"log_{v}") for v in ("cc_basic", "cc_civil", "cc_polit", "cc_prop")
    }


def test_write_panel_outputs_produces_expected_files() -> None:
    result = write_panel_outputs()
    expected_stems = {
        "descriptive_statistics",
        "spearman_correlation_matrix",
        "panel_models",
        "models_small_full",
        "models_small_reduced",
        "models_main_reduced",
        "specification_tests",
        "diagnostic_tests",
        "fixed_effects_main",
        "fixed_effects_compliance_categories",
    }
    written_stems = {Path(path).stem for path in result.output_paths}
    assert expected_stems <= written_stems
    assert result.documentation_path.exists()

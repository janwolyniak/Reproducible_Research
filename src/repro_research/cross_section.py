from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import statsmodels.api as sm
from scipy import stats
from statsmodels.stats.diagnostic import (
    acorr_breusch_godfrey,
    het_breuschpagan,
    linear_reset,
)
from statsmodels.stats.outliers_influence import OLSInfluence, variance_inflation_factor
from statsmodels.stats.stattools import durbin_watson, jarque_bera

from repro_research.cross_section_data import (
    CrossSectionFrame,
    add_cross_section_transformations,
    load_cross_section_inputs,
)
from repro_research.paths import PROJECT_ROOT

OUTPUT_DIR = PROJECT_ROOT / "outputs" / "cross_section"
FIGURE_DIR = OUTPUT_DIR / "figures"
DOCS_DIR = PROJECT_ROOT / "docs"

CORRELATION_VARIABLES = (
    "GDP_growth",
    "fertility",
    "GDPpc2015",
    "inf_def",
    "investment",
    "education",
    "gov_exp_reduced",
    "cc_total",
    "v2x_libdem",
    "terms_trade",
    "trade",
    "life_exp",
)


@dataclass(frozen=True)
class CrossSectionResult:
    output_paths: tuple[Path, ...]
    documentation_path: Path


@dataclass(frozen=True)
class ModelSpec:
    name: str
    dataset: str
    dependent: str
    predictors: tuple[str, ...]
    table_group: str


MAIN_PREDICTORS = (
    "log_GDPpc2015",
    "investment",
    "terms_trade_x_trade",
    "gov_exp_reduced",
    "log_cc_total_plus2",
    "terms_trade",
    "trade",
)

MODEL_SPECS = (
    ModelSpec(
        "re1",
        "model_data4",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "inf_def",
            "investment",
            "education",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "life_exp",
            "log_cc_total_plus2",
            "v2x_libdem",
            "terms_trade",
            "trade",
        ),
        "ols_full_sample",
    ),
    ModelSpec(
        "re2",
        "model_data4",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "inf_def",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_total_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_full_sample",
    ),
    ModelSpec(
        "re3",
        "model_data4",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_total_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_full_sample",
    ),
    ModelSpec("re4", "model_data4", "GDP_growth", MAIN_PREDICTORS, "ols_full_sample"),
    ModelSpec(
        "re5",
        "model_data4",
        "GDP_growth",
        (
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "terms_trade",
            "trade",
        ),
        "ols_full_sample",
    ),
    ModelSpec(
        "re6",
        "model_data4",
        "GDP_growth",
        ("log_GDPpc2015", "investment", "gov_exp_reduced", "terms_trade", "trade"),
        "ols_full_sample",
    ),
    ModelSpec(
        "re7",
        "model_data4",
        "GDP_growth",
        ("log_GDPpc2015", "investment", "gov_exp_reduced", "trade"),
        "ols_full_sample",
    ),
    ModelSpec(
        "re1_o",
        "model_data4_o",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "inf_def",
            "investment",
            "education",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "life_exp",
            "log_cc_total_plus2",
            "v2x_libdem",
            "terms_trade",
            "trade",
        ),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re2_o",
        "model_data4_o",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "inf_def",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_total_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re3_o",
        "model_data4_o",
        "GDP_growth",
        (
            "fertility",
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_total_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re4_o",
        "model_data4_o",
        "GDP_growth",
        MAIN_PREDICTORS,
        "ols_main",
    ),
    ModelSpec(
        "re5_o",
        "model_data4_o",
        "GDP_growth",
        (
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "terms_trade",
            "trade",
        ),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re6_o",
        "model_data4_o",
        "GDP_growth",
        ("log_GDPpc2015", "investment", "gov_exp_reduced", "terms_trade", "trade"),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re7_o",
        "model_data4_o",
        "GDP_growth",
        ("log_GDPpc2015", "investment", "gov_exp_reduced", "trade"),
        "ols_outlier_filtered",
    ),
    ModelSpec(
        "re4_olv",
        "model_data4_o",
        "GDP_growth",
        (
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_total_lv_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_compliance_variants",
    ),
    ModelSpec(
        "re4_oprop",
        "model_data4_o",
        "GDP_growth",
        (
            "log_GDPpc2015",
            "investment",
            "terms_trade_x_trade",
            "gov_exp_reduced",
            "log_cc_prop_plus2",
            "terms_trade",
            "trade",
        ),
        "ols_compliance_variants",
    ),
)

INSTITUTIONAL_VARIABLES = (
    "rule_of_law",
    "ctrl_of_corr",
    "gov_eff",
    "pol_stab",
    "reg_q",
    "voice_and_acc",
)

MODEL_TABLE_GROUPS = {
    "ols_full_sample": ("re1", "re2", "re3", "re4", "re5", "re6", "re7"),
    "ols_main": ("re2_o", "re4_o"),
    "ols_outlier_filtered": (
        "re1_o",
        "re2_o",
        "re3_o",
        "re4_o",
        "re5_o",
        "re6_o",
        "re7_o",
    ),
    "ols_compliance_variants": ("re4_olv", "re4_oprop"),
}


def _write_table(
    frame: pd.DataFrame, stem: str, output_dir: Path, *, index: bool = False
) -> tuple[Path, Path]:
    csv_path = output_dir / f"{stem}.csv"
    html_path = output_dir / f"{stem}.html"
    frame.to_csv(csv_path, index=index)
    frame.to_html(html_path, index=index, float_format=lambda value: f"{value:.6g}")
    return csv_path, html_path


def _display_path(path: Path) -> str:
    try:
        return path.relative_to(PROJECT_ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def _prepared_frames(frames: dict[str, CrossSectionFrame]) -> dict[str, pd.DataFrame]:
    prepared = {
        name: add_cross_section_transformations(item.frame)
        for name, item in frames.items()
        if name.startswith("model_data")
    }
    if "model_data4.1" in prepared:
        prepared["model_data4_grouped"] = prepared["model_data4.1"]
    return prepared


def descriptive_statistics(frame: pd.DataFrame) -> pd.DataFrame:
    numeric = frame.select_dtypes(include=[np.number])
    rows = []
    for column in numeric.columns:
        values = numeric[column].dropna()
        rows.append(
            {
                "Variable": column,
                "N": int(values.size),
                "Min": values.min(),
                "Median": values.median(),
                "Mean": values.mean(),
                "SD": values.std(ddof=1),
                "Max": values.max(),
            }
        )
    # The R workflow removes the first reshaped row before rendering.
    return pd.DataFrame(rows).iloc[1:].reset_index(drop=True)


def correlation_matrix(frame: pd.DataFrame) -> pd.DataFrame:
    columns = [column for column in CORRELATION_VARIABLES if column in frame.columns]
    complete = frame[columns].dropna()
    return complete.corr(method="spearman")


def write_correlation_plot(
    correlation: pd.DataFrame, figure_dir: Path = FIGURE_DIR
) -> Path:
    figure_dir.mkdir(parents=True, exist_ok=True)
    mask = np.triu(np.ones_like(correlation, dtype=bool))
    fig, ax = plt.subplots(figsize=(10, 8))
    sns.heatmap(
        correlation,
        mask=mask,
        cmap="BrBG",
        center=0,
        annot=True,
        fmt=".2f",
        linewidths=0.5,
        cbar_kws={"label": "Spearman rho"},
        ax=ax,
    )
    ax.set_title("Cross-sectional Spearman correlations")
    fig.tight_layout()
    path = figure_dir / "correlation_plot.png"
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


def _spearman_for_group(
    label: str, frame: pd.DataFrame, left: str, right: str
) -> dict[str, Any]:
    data = frame[[left, right]].dropna()
    if len(data) < 3:
        rho = np.nan
        p_two = np.nan
    else:
        result = stats.spearmanr(data[left], data[right])
        rho = float(result.statistic)
        p_two = float(result.pvalue)
    return {
        "Group": label,
        "N": int(len(data)),
        "rho": rho,
        "Alternative Two-Sided": p_two,
        "Alternative Less": p_two / 2 if np.isfinite(p_two) else np.nan,
        "Alternative Greater": 1 - (p_two / 2) if np.isfinite(p_two) else np.nan,
    }


def grouped_spearman(frame: pd.DataFrame, right: str) -> pd.DataFrame:
    groups: list[tuple[str, pd.DataFrame]] = [("without_grouping", frame)]
    if "GDPpc" in frame.columns:
        median = frame["GDPpc"].median(skipna=True)
        groups.extend(
            [
                ("rich", frame[frame["GDPpc"] >= median]),
                ("poor", frame[frame["GDPpc"] < median]),
            ]
        )
    if "continent" in frame.columns:
        for value in ("Africa", "America", "Asia", "Europe", "Pacific"):
            groups.append(
                (str(value), frame[frame["continent"].astype("string") == value])
            )
    if "legal_old_o" in frame.columns:
        for value in ("fr", "so", "uk", "ge", "sc"):
            groups.append(
                (
                    str(value).upper(),
                    frame[frame["legal_old_o"].astype("string") == value],
                )
            )
    if "ever_colonized" in frame.columns:
        groups.extend(
            [
                ("ever_colonized", frame[frame["ever_colonized"] == 1]),
                ("never_colonized", frame[frame["ever_colonized"] == 0]),
            ]
        )
    return pd.DataFrame(
        [
            _spearman_for_group(label, group, "GDP_growth", right)
            for label, group in groups
        ]
    )


def fit_model(
    frame: pd.DataFrame, spec: ModelSpec
) -> sm.regression.linear_model.RegressionResultsWrapper:
    columns = [spec.dependent, *spec.predictors]
    model_data = frame[columns].replace([np.inf, -np.inf], np.nan).dropna()
    y = model_data[spec.dependent]
    x = sm.add_constant(model_data[list(spec.predictors)], has_constant="add")
    return sm.OLS(y, x).fit()


def _stars(pvalue: float) -> str:
    if pvalue < 0.01:
        return "***"
    if pvalue < 0.05:
        return "**"
    if pvalue < 0.1:
        return "*"
    return ""


def model_coefficient_table(
    models: dict[str, sm.regression.linear_model.RegressionResultsWrapper],
) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for name, result in models.items():
        robust = result.get_robustcov_results(cov_type="HC3")
        params = pd.Series(robust.params, index=result.params.index)
        bse = pd.Series(robust.bse, index=result.params.index)
        tvalues = pd.Series(robust.tvalues, index=result.params.index)
        pvalues = pd.Series(robust.pvalues, index=result.params.index)
        for term in result.params.index:
            rows.append(
                {
                    "model": name,
                    "term": term,
                    "coefficient": params[term],
                    "robust_se_hc3": bse[term],
                    "robust_t": tvalues[term],
                    "p_value": pvalues[term],
                    "stars": _stars(float(pvalues[term])),
                    "nobs": int(result.nobs),
                    "r_squared": result.rsquared,
                    "adj_r_squared": result.rsquared_adj,
                    "aic": result.aic,
                    "bic": result.bic,
                }
            )
    return pd.DataFrame(rows)


def model_summary_table(
    specs: tuple[ModelSpec, ...],
    prepared: dict[str, pd.DataFrame],
) -> tuple[
    pd.DataFrame, dict[str, sm.regression.linear_model.RegressionResultsWrapper]
]:
    models = {spec.name: fit_model(prepared[spec.dataset], spec) for spec in specs}
    spec_lookup = {spec.name: spec for spec in specs}
    table = model_coefficient_table(models)
    table.insert(
        1, "dataset", table["model"].map(lambda value: spec_lookup[value].dataset)
    )
    table.insert(
        2,
        "table_group",
        table["model"].map(lambda value: spec_lookup[value].table_group),
    )
    return table, models


def select_model_group(
    model_table: pd.DataFrame, table_name: str, model_names: tuple[str, ...]
) -> pd.DataFrame:
    order = {name: index for index, name in enumerate(model_names)}
    group = model_table[model_table["model"].isin(model_names)].copy()
    group.insert(0, "_model_order", group["model"].map(order))
    group["table_group"] = table_name
    group = group.sort_values(["_model_order"], kind="stable")
    return group.drop(columns="_model_order").reset_index(drop=True)


def institutional_specs() -> tuple[ModelSpec, ...]:
    return tuple(
        ModelSpec(
            f"inst{index}",
            "model_data4_o",
            "GDP_growth",
            (
                "log_GDPpc2015",
                "investment",
                "terms_trade_x_trade",
                "gov_exp_reduced",
                variable,
                "terms_trade",
                "trade",
            ),
            "ols_institutional_variants",
        )
        for index, variable in enumerate(INSTITUTIONAL_VARIABLES, start=1)
    )


def diagnostics_table(
    models: dict[str, sm.regression.linear_model.RegressionResultsWrapper],
) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for name, result in models.items():
        reset = linear_reset(result, power=3, test_type="fitted", use_f=True)
        bp_lm, bp_lm_pvalue, bp_f, bp_f_pvalue = het_breuschpagan(
            result.resid, result.model.exog
        )
        jb_stat, jb_pvalue, skew, kurtosis = jarque_bera(result.resid)
        shapiro_stat, shapiro_pvalue = stats.shapiro(result.resid)
        bg_lm, bg_lm_pvalue, bg_f, bg_f_pvalue = acorr_breusch_godfrey(result, nlags=1)
        rows.append(
            {
                "model": name,
                "nobs": int(result.nobs),
                "reset_f": float(reset.fvalue),
                "reset_p_value": float(reset.pvalue),
                "shapiro_w": float(shapiro_stat),
                "shapiro_p_value": float(shapiro_pvalue),
                "jarque_bera": float(jb_stat),
                "jarque_bera_p_value": float(jb_pvalue),
                "residual_skew": float(skew),
                "residual_kurtosis": float(kurtosis),
                "breusch_pagan_lm": float(bp_lm),
                "breusch_pagan_lm_p_value": float(bp_lm_pvalue),
                "breusch_pagan_f": float(bp_f),
                "breusch_pagan_f_p_value": float(bp_f_pvalue),
                "durbin_watson_stat": float(durbin_watson(result.resid)),
                "breusch_godfrey_lm_lag1": float(bg_lm),
                "breusch_godfrey_lm_lag1_p_value": float(bg_lm_pvalue),
                "breusch_godfrey_f_lag1": float(bg_f),
                "breusch_godfrey_f_lag1_p_value": float(bg_f_pvalue),
            }
        )
    return pd.DataFrame(rows)


def vif_table(
    models: dict[str, sm.regression.linear_model.RegressionResultsWrapper],
) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for name, result in models.items():
        names = result.model.exog_names
        for index, term in enumerate(names):
            if term == "const":
                continue
            rows.append(
                {
                    "model": name,
                    "term": term,
                    "vif": float(variance_inflation_factor(result.model.exog, index)),
                }
            )
    return pd.DataFrame(rows)


def influence_table(
    result: sm.regression.linear_model.RegressionResultsWrapper,
    source: pd.DataFrame,
    spec: ModelSpec,
) -> pd.DataFrame:
    columns = [
        "country_name",
        "country_code",
        spec.dependent,
        *spec.predictors,
    ]
    available = [column for column in columns if column in source.columns]
    model_data = source[available].replace([np.inf, -np.inf], np.nan).dropna()
    influence = OLSInfluence(result)
    output = model_data[["country_name", "country_code", spec.dependent]].copy()
    output["residual"] = result.resid
    output["fitted"] = result.fittedvalues
    output["leverage"] = influence.hat_matrix_diag
    output["standardized_residual"] = influence.resid_studentized_internal
    output["cooks_distance"] = influence.cooks_distance[0]
    k = len(result.params)
    n = int(result.nobs)
    output["high_leverage_threshold"] = 2 * k / n
    output["high_cooks_distance_threshold"] = 4 / n
    output["high_leverage"] = output["leverage"] > output["high_leverage_threshold"]
    output["large_standardized_residual"] = output["standardized_residual"].abs() > 2
    output["high_cooks_distance"] = (
        output["cooks_distance"] > output["high_cooks_distance_threshold"]
    )
    return output.sort_values("cooks_distance", ascending=False)


def write_diagnostic_plots(
    result: sm.regression.linear_model.RegressionResultsWrapper,
    figure_dir: Path = FIGURE_DIR,
) -> tuple[Path, Path, Path]:
    figure_dir.mkdir(parents=True, exist_ok=True)
    influence = OLSInfluence(result)

    residual_path = figure_dir / "re4_o_residuals_vs_fitted.png"
    fig, ax = plt.subplots(figsize=(7, 5))
    ax.scatter(result.fittedvalues, result.resid, alpha=0.75)
    ax.axhline(0, color="black", linewidth=1)
    ax.set_xlabel("Fitted values")
    ax.set_ylabel("Residuals")
    ax.set_title("re4_o residuals vs fitted")
    fig.tight_layout()
    fig.savefig(residual_path, dpi=200)
    plt.close(fig)

    qq_path = figure_dir / "re4_o_residual_qq.png"
    fig = sm.qqplot(result.resid, line="45", fit=True)
    fig.suptitle("re4_o residual Q-Q plot")
    fig.tight_layout()
    fig.savefig(qq_path, dpi=200)
    plt.close(fig)

    cooks_path = figure_dir / "re4_o_cooks_distance.png"
    fig, ax = plt.subplots(figsize=(8, 5))
    cooks = influence.cooks_distance[0]
    ax.stem(np.arange(len(cooks)), cooks, markerfmt=",", basefmt=" ")
    ax.axhline(4 / int(result.nobs), color="red", linestyle="--", linewidth=1)
    ax.set_xlabel("Observation")
    ax.set_ylabel("Cook's distance")
    ax.set_title("re4_o Cook's distance")
    fig.tight_layout()
    fig.savefig(cooks_path, dpi=200)
    plt.close(fig)

    return residual_path, qq_path, cooks_path


def write_cross_section_documentation(
    paths: tuple[Path, ...], docs_dir: Path = DOCS_DIR
) -> Path:
    docs_dir.mkdir(parents=True, exist_ok=True)
    path = docs_dir / "cross_section_reproduction.md"
    artifact_lines = "\n".join(f"- `{_display_path(artifact)}`" for artifact in paths)
    path.write_text(
        "\n".join(
            [
                "# Cross-Sectional Reproduction",
                "",
                "This document summarizes the Python implementation of Phase 3.",
                "The source workflow is `cross-section/inorder.R`; the tracked RDS",
                "files are treated as authoritative because the R script is an",
                "exploratory notebook-like script rather than a clean end-to-end run.",
                "",
                "## Implemented Outputs",
                "",
                artifact_lines,
                "",
                "## Matching Notes",
                "",
                "- Descriptive statistics follow the R workflow by summarising numeric",
                "  columns from `model_data2` and dropping the first reshaped row before",
                "  rendering.",
                "- The correlation matrix uses Spearman correlations on the same selected",
                "  variable set and the same complete-observation rule as",
                '  `cor(..., use = "complete.obs", method = "spearman")` in R.',
                "  Plot pixels are best-effort; the CSV matrix is the numerical",
                "  reproduction target.",
                "- `ols_main` mirrors the R `reg_table1.html` target by exporting",
                "  `re2_o` and `re4_o`. `ols_outlier_filtered` keeps the full",
                "  `re1_o`-`re7_o` robustness family, so `re2_o` and `re4_o` also",
                "  appear there.",
                "- `ols_compliance_variants` mirrors the combined compliance table",
                "  target with `re4_olv` and `re4_oprop`; institutional alternatives",
                "  mirror the six World Bank governance replacements.",
                "- OLS tables use `statsmodels` OLS with HC3 robust standard errors,",
                '  matching the R workflow\'s `sandwich::vcovHC(..., type = "HC3")`',
                "  publication-table intent.",
                "- `model_data4` has no `tot2` column in the tracked file, so the Python",
                "  loader uses the canonical `terms_trade` fallback documented in Phase 2.",
                "  The outlier-filtered models use `model_data4_o`, where `tot2` is",
                "  present and mapped to `terms_trade`.",
                "- Diagnostics reproduce RESET, Shapiro-Wilk, Breusch-Pagan,",
                "  Durbin-Watson statistics, and lag-1 Breusch-Godfrey",
                "  autocorrelation p-values. Python does not expose an exact",
                "  `lmtest::dwtest` equivalent through `statsmodels`, so the",
                "  Breusch-Godfrey p-value is the documented autocorrelation",
                "  p-value target.",
                "- Diagnostic p-values may differ at tiny finite-sample levels because",
                "  Python and R expose slightly different defaults for some tests.",
                "",
            ]
        ),
        encoding="utf-8",
    )
    return path


def write_cross_section_outputs(
    output_dir: Path = OUTPUT_DIR,
    docs_dir: Path = DOCS_DIR,
    *,
    skip_plots: bool = False,
) -> CrossSectionResult:
    output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir = output_dir / "figures"
    if not skip_plots:
        figure_dir.mkdir(parents=True, exist_ok=True)
    docs_dir.mkdir(parents=True, exist_ok=True)

    frames = load_cross_section_inputs()
    prepared = _prepared_frames(frames)

    paths: list[Path] = []
    paths.extend(
        _write_table(
            descriptive_statistics(prepared["model_data2"]),
            "descriptive_statistics",
            output_dir,
        )
    )

    corr = correlation_matrix(prepared["model_data4"])
    paths.extend(
        _write_table(
            corr.reset_index().rename(columns={"index": "Variable"}),
            "spearman_correlation_matrix",
            output_dir,
        )
    )
    if not skip_plots:
        paths.append(write_correlation_plot(corr, figure_dir))

    grouped = prepared.get("model_data4_grouped", prepared["model_data4_o"])
    paths.extend(
        _write_table(
            grouped_spearman(grouped, "cc_total"),
            "spearman_growth_vs_cc_total",
            output_dir,
        )
    )
    paths.extend(
        _write_table(
            grouped_spearman(grouped, "cc_prop"),
            "spearman_growth_vs_cc_prop",
            output_dir,
        )
    )

    model_table, models = model_summary_table(MODEL_SPECS, prepared)
    paths.extend(_write_table(model_table, "ols_models", output_dir))
    for group_name, model_names in MODEL_TABLE_GROUPS.items():
        group = select_model_group(model_table, group_name, model_names)
        paths.extend(_write_table(group, group_name, output_dir))

    inst_table, inst_models = model_summary_table(institutional_specs(), prepared)
    paths.extend(_write_table(inst_table, "ols_institutional_variants", output_dir))

    diagnostic_models = {
        key: value
        for key, value in {**models, **inst_models}.items()
        if key in {"re2_o", "re4_o", "re4_olv", "re4_oprop"} or key.startswith("inst")
    }
    paths.extend(
        _write_table(diagnostics_table(diagnostic_models), "diagnostics", output_dir)
    )
    paths.extend(
        _write_table(
            vif_table(diagnostic_models), "variance_inflation_factors", output_dir
        )
    )

    re4_o_spec = next(spec for spec in MODEL_SPECS if spec.name == "re4_o")
    paths.extend(
        _write_table(
            influence_table(models["re4_o"], prepared["model_data4_o"], re4_o_spec),
            "re4_o_influence_diagnostics",
            output_dir,
        )
    )
    if not skip_plots:
        paths.extend(write_diagnostic_plots(models["re4_o"], figure_dir))

    documentation_path = write_cross_section_documentation(tuple(paths), docs_dir)
    return CrossSectionResult(
        output_paths=tuple(paths), documentation_path=documentation_path
    )

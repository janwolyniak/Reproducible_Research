"""Phase 4 panel reproduction.

Mirrors ``repro_research.cross_section`` for the panel side: descriptive stats,
correlation, regression families, specification tests, diagnostics, robust
covariance variants, and alternative compliance-component FE models. Outputs
match the structure produced by the R reference under ``panel/panel_tables/``.

The data come from :mod:`repro_research.panel_data` and
:mod:`repro_research.panel_transforms`; this module fits models on those
prepared frames using ``linearmodels`` (panel estimators), ``statsmodels``
(diagnostic tests), and a local Driscoll-Kraay implementation.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
import seaborn as sns  # noqa: E402
import statsmodels.api as sm  # noqa: E402
from linearmodels.panel import (  # noqa: E402
    BetweenOLS,
    FirstDifferenceOLS,
    PanelOLS,
    PooledOLS,
    RandomEffects,
)
from scipy import stats  # noqa: E402
from statsmodels.stats.diagnostic import het_breuschpagan  # noqa: E402

from repro_research.panel_data import load_panel  # noqa: E402
from repro_research.panel_transforms import prepare_for_models  # noqa: E402
from repro_research.paths import PROJECT_ROOT  # noqa: E402

OUTPUT_DIR = PROJECT_ROOT / "outputs" / "panel"
FIGURE_DIR = OUTPUT_DIR / "figures"
DOCS_DIR = PROJECT_ROOT / "docs"

ModelKind = Literal["pooling", "between", "first_difference", "fixed", "random"]

CORRELATION_VARIABLES = (
    "GDPgrowth",
    "fertility",
    "GDPpc2015",
    "inflation",
    "investment",
    "gov_exp_reduced",
    "cc_total",
    "tot_growth",
    "trade",
    "education",
    "life_exp",
)


@dataclass(frozen=True)
class PanelResult:
    output_paths: tuple[Path, ...]
    documentation_path: Path


@dataclass(frozen=True)
class PanelSpec:
    name: str
    dataset: str
    dependent: str
    predictors: tuple[str, ...]
    model_kind: ModelKind
    table_group: str


@dataclass(frozen=True)
class PanelFitResult:
    spec: PanelSpec
    params: pd.Series
    bse: pd.Series
    tvalues: pd.Series
    pvalues: pd.Series
    nobs: int
    rsquared: float
    rsquared_within: float | None
    rsquared_between: float | None
    rsquared_overall: float | None
    f_pooled_pvalue: float | None
    residuals: pd.Series
    fitted: pd.Series
    raw: Any


# ---------------------------------------------------------------------------
# Model specifications
# ---------------------------------------------------------------------------

_SMALL_PREDICTORS_FULL = (
    "log_cc_total",
    "life_exp",
    "education",
    "fertility",
    "lag_log_GDPpc2015",
    "gov_exp_reduced",
    "inflation",
    "investment",
    "tot_growth",
    "trade",
)
_SMALL_PREDICTORS_REDUCED = (
    "log_cc_total",
    "fertility",
    "lag_log_GDPpc2015",
    "gov_exp_reduced",
    "inflation",
    "investment",
    "tot_growth",
    "trade",
    "tot_growth_x_trade",
)
_MAIN_PREDICTORS_REDUCED = _SMALL_PREDICTORS_REDUCED

_KINDS: tuple[ModelKind, ...] = (
    "pooling",
    "between",
    "first_difference",
    "fixed",
    "random",
)


def _spec_block(
    prefix: str,
    dataset: str,
    predictors: tuple[str, ...],
    table_group: str,
) -> tuple[PanelSpec, ...]:
    return tuple(
        PanelSpec(
            name=f"{prefix}_{kind}",
            dataset=dataset,
            dependent="GDPgrowth",
            predictors=predictors,
            model_kind=kind,
            table_group=table_group,
        )
        for kind in _KINDS
    )


MODEL_SPECS: tuple[PanelSpec, ...] = (
    *_spec_block("small_full", "small", _SMALL_PREDICTORS_FULL, "models_small_full"),
    *_spec_block(
        "small_reduced", "small", _SMALL_PREDICTORS_REDUCED, "models_small_reduced"
    ),
    *_spec_block(
        "main_reduced", "main", _MAIN_PREDICTORS_REDUCED, "models_main_reduced"
    ),
)


_ALT_FE_VARIANTS = ("cc_basic", "cc_civil", "cc_polit", "cc_prop")


# ---------------------------------------------------------------------------
# Data preparation helpers
# ---------------------------------------------------------------------------


def _prepared_panels() -> dict[str, pd.DataFrame]:
    return {
        kind: prepare_for_models(load_panel(kind)) for kind in ("main", "alt", "small")
    }


def _indexed(df: pd.DataFrame) -> pd.DataFrame:
    return df.set_index(["country", "Year"]).sort_index()


def _design(
    df: pd.DataFrame, spec: PanelSpec
) -> tuple[pd.DataFrame, pd.Series, pd.DataFrame]:
    indexed = _indexed(df)
    columns = [spec.dependent, *spec.predictors]
    model_data = indexed[columns].replace([np.inf, -np.inf], np.nan).dropna()
    y = model_data[spec.dependent]
    x = model_data[list(spec.predictors)]
    return model_data, y, x


def _drop_degenerate_first_difference_terms(x: pd.DataFrame) -> pd.DataFrame:
    """Drop regressors whose first differences contain no usable variation."""
    diffs = x.groupby(level="country").diff()
    keep = []
    for column in x.columns:
        values = diffs[column].replace([np.inf, -np.inf], np.nan).dropna()
        if values.empty:
            continue
        if float(values.abs().max()) > 1e-12:
            keep.append(column)
    if not keep:
        raise ValueError("First-difference design has no non-degenerate regressors.")
    return x[keep]


# ---------------------------------------------------------------------------
# Descriptive statistics and correlation
# ---------------------------------------------------------------------------


def panel_descriptive_statistics(df: pd.DataFrame) -> pd.DataFrame:
    numeric = df.select_dtypes(include=[np.number])
    rows = []
    for column in numeric.columns:
        values = numeric[column].dropna()
        if values.empty:
            continue
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
    return pd.DataFrame(rows)


def panel_correlation_matrix(df: pd.DataFrame) -> pd.DataFrame:
    columns = [c for c in CORRELATION_VARIABLES if c in df.columns]
    complete = df[columns].dropna()
    return complete.corr(method="spearman")


def write_panel_correlation_plot(
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
    ax.set_title("Panel Spearman correlations")
    fig.tight_layout()
    path = figure_dir / "correlation_plot.png"
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


# ---------------------------------------------------------------------------
# Model fitting
# ---------------------------------------------------------------------------


def fit_panel_model(spec: PanelSpec, df: pd.DataFrame) -> PanelFitResult:
    _, y, x = _design(df, spec)
    exog = sm.add_constant(x, has_constant="add")

    if spec.model_kind == "pooling":
        raw = PooledOLS(y, exog).fit()
    elif spec.model_kind == "between":
        raw = BetweenOLS(y, exog).fit()
    elif spec.model_kind == "first_difference":
        # First-difference drops the constant by construction. Variables that do
        # not move within country become all-zero after differencing and must be
        # removed before fitting so inference columns remain meaningful.
        raw = FirstDifferenceOLS(y, _drop_degenerate_first_difference_terms(x)).fit()
    elif spec.model_kind == "fixed":
        raw = PanelOLS(y, exog, entity_effects=True, drop_absorbed=True).fit()
    elif spec.model_kind == "random":
        raw = RandomEffects(y, exog).fit()
    else:  # pragma: no cover - guarded by Literal
        raise ValueError(f"Unknown model kind: {spec.model_kind}")

    fitted_raw = raw.fitted_values
    fitted = (
        fitted_raw.iloc[:, 0] if isinstance(fitted_raw, pd.DataFrame) else fitted_raw
    )
    nan_index = raw.params.index

    try:
        bse = raw.std_errors
        tvalues = raw.tstats
        pvalues = raw.pvalues
    except np.linalg.LinAlgError:
        # Covariance ill-conditioned (e.g. perfect collinearity after FD or FE
        # demeaning on slowly-varying covariates). Surface NaNs so downstream
        # tables still render and the documentation can flag the model.
        bse = pd.Series(np.nan, index=nan_index, name="std_error")
        tvalues = pd.Series(np.nan, index=nan_index, name="tstat")
        pvalues = pd.Series(np.nan, index=nan_index, name="pvalue")

    return PanelFitResult(
        spec=spec,
        params=raw.params,
        bse=bse,
        tvalues=tvalues,
        pvalues=pvalues,
        nobs=int(raw.nobs),
        rsquared=float(getattr(raw, "rsquared", np.nan)),
        rsquared_within=float(getattr(raw, "rsquared_within", np.nan)),
        rsquared_between=float(getattr(raw, "rsquared_between", np.nan)),
        rsquared_overall=float(getattr(raw, "rsquared_overall", np.nan)),
        f_pooled_pvalue=_safe_pvalue(getattr(raw, "f_pooled", None)),
        residuals=raw.resids,
        fitted=fitted,
        raw=raw,
    )


def _safe_pvalue(test_result: Any) -> float | None:
    if test_result is None:
        return None
    pvalue = getattr(test_result, "pval", None)
    if pvalue is None:
        pvalue = getattr(test_result, "pvalue", None)
    return float(pvalue) if pvalue is not None else None


def _stars(pvalue: float) -> str:
    if pvalue < 0.01:
        return "***"
    if pvalue < 0.05:
        return "**"
    if pvalue < 0.1:
        return "*"
    return ""


def panel_coefficient_table(fits: dict[str, PanelFitResult]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    for name, fit in fits.items():
        for term in fit.params.index:
            rows.append(
                {
                    "model": name,
                    "dataset": fit.spec.dataset,
                    "model_kind": fit.spec.model_kind,
                    "term": term,
                    "coefficient": float(fit.params[term]),
                    "std_error": float(fit.bse[term]),
                    "t_stat": float(fit.tvalues[term]),
                    "p_value": float(fit.pvalues[term]),
                    "stars": _stars(float(fit.pvalues[term])),
                    "nobs": fit.nobs,
                    "r_squared": fit.rsquared,
                }
            )
    return pd.DataFrame(rows)


def panel_summary_table(
    specs: tuple[PanelSpec, ...], frames: dict[str, pd.DataFrame]
) -> tuple[pd.DataFrame, dict[str, PanelFitResult]]:
    fits: dict[str, PanelFitResult] = {}
    for spec in specs:
        fits[spec.name] = fit_panel_model(spec, frames[spec.dataset])
    return panel_coefficient_table(fits), fits


def select_model_group(
    summary: pd.DataFrame, table_group: str, model_names: tuple[str, ...]
) -> pd.DataFrame:
    return summary[summary["model"].isin(model_names)].copy().reset_index(drop=True)


# ---------------------------------------------------------------------------
# Specification tests: LM, F-pooled, Hausman
# ---------------------------------------------------------------------------


def lm_test_pooled_vs_random(fit_random: PanelFitResult) -> float | None:
    """Breusch-Pagan LM test for random effects against pooled OLS.

    Returns the p-value of the LM statistic exposed by ``linearmodels`` when
    available, otherwise ``None``.
    """
    raw = fit_random.raw
    for attr in ("variance_decomposition", "_components"):  # defensive
        if hasattr(raw, attr):
            break
    # linearmodels does not expose a direct LM test; we compute the
    # Breusch-Pagan LM statistic from pooled residuals on the same design.
    spec = fit_random.spec
    panels = _prepared_panels()
    pooled = fit_panel_model(
        PanelSpec(
            name=f"{spec.name}_pooled_for_lm",
            dataset=spec.dataset,
            dependent=spec.dependent,
            predictors=spec.predictors,
            model_kind="pooling",
            table_group=spec.table_group,
        ),
        panels[spec.dataset],
    )
    resid = pooled.residuals
    resid = resid.dropna()
    if resid.empty:
        return None
    by_entity = resid.groupby(level="country")
    sum_squared = float((by_entity.sum() ** 2).sum())
    squared_sum = float((resid**2).sum())
    n = int(resid.size)
    t_bar = float(by_entity.size().mean())
    if squared_sum == 0 or t_bar <= 1:
        return None
    lm_stat = (n * t_bar) / (2 * (t_bar - 1)) * ((sum_squared / squared_sum) - 1) ** 2
    return float(stats.chi2.sf(lm_stat, df=1))


def f_test_fixed_vs_pooled(fit_fixed: PanelFitResult) -> float | None:
    test = getattr(fit_fixed.raw, "f_pooled", None)
    return _safe_pvalue(test)


def hausman_test(fit_fixed: PanelFitResult, fit_random: PanelFitResult) -> float | None:
    common = fit_fixed.params.index.intersection(fit_random.params.index)
    common = [c for c in common if c != "const"]
    if not common:
        return None
    b_fe = fit_fixed.params.loc[common].to_numpy()
    b_re = fit_random.params.loc[common].to_numpy()
    cov_fe = fit_fixed.raw.cov.loc[common, common].to_numpy()
    cov_re = fit_random.raw.cov.loc[common, common].to_numpy()
    diff = b_fe - b_re
    cov_diff = cov_fe - cov_re
    try:
        inv = np.linalg.pinv(cov_diff)
    except np.linalg.LinAlgError:
        return None
    statistic = float(diff @ inv @ diff)
    if not np.isfinite(statistic) or statistic < 0:
        return None
    return float(stats.chi2.sf(statistic, df=len(common)))


def specification_tests(
    fits: dict[str, PanelFitResult], blocks: tuple[tuple[str, str, str, str], ...]
) -> pd.DataFrame:
    rows = []
    for label, _pooling_name, fixed_name, random_name in blocks:
        rows.append(
            {
                "block": label,
                "lm_pvalue": lm_test_pooled_vs_random(fits[random_name]),
                "f_pooled_pvalue": f_test_fixed_vs_pooled(fits[fixed_name]),
                "hausman_pvalue": hausman_test(fits[fixed_name], fits[random_name]),
            }
        )
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Diagnostic tests: Wooldridge, Breusch-Pagan, Pesaran CD
# ---------------------------------------------------------------------------


def panel_serial_correlation_test(fit: PanelFitResult) -> float | None:
    """Breusch-Godfrey-style panel serial-correlation proxy.

    R's ``plm::pbgtest`` is not exposed in Python. This uses the same diagnostic
    target: an auxiliary regression of residuals on their first within-country
    lag plus the fitted design matrix, with an LM statistic based on R-squared.
    """
    resid = fit.residuals.dropna()
    if resid.empty:
        return None
    lagged = resid.groupby(level="country").shift(1)
    exog = fit.raw.model.exog.dataframe.reindex(resid.index)
    aux = pd.concat({"resid": resid, "lag_resid": lagged}, axis=1).join(exog)
    aux = aux.replace([np.inf, -np.inf], np.nan).dropna()
    if len(aux) < 5:
        return None
    y = aux["resid"].to_numpy()
    x = sm.add_constant(aux.drop(columns="resid").to_numpy(), has_constant="add")
    model = sm.OLS(y, x).fit()
    statistic = len(aux) * model.rsquared
    return float(stats.chi2.sf(statistic, df=1))


def wooldridge_serial_correlation(fit: PanelFitResult) -> float | None:
    """Backward-compatible alias for the Phase 4 serial-correlation diagnostic."""
    return panel_serial_correlation_test(fit)


def breusch_pagan_test(fit: PanelFitResult) -> float | None:
    resid = fit.residuals.dropna()
    exog = fit.raw.model.exog.dataframe.reindex(resid.index)
    aux = exog.replace([np.inf, -np.inf], np.nan).dropna()
    common = resid.index.intersection(aux.index)
    if resid.empty or aux.empty or len(common) < 5:
        return None
    lm, lm_pvalue, _, _ = het_breuschpagan(
        resid.loc[common].to_numpy(),
        sm.add_constant(aux.loc[common].to_numpy(), has_constant="add"),
    )
    return float(lm_pvalue)


def pesaran_cd_test(fit: PanelFitResult) -> float | None:
    resid = fit.residuals.dropna()
    if resid.empty:
        return None
    pivot = resid.unstack("country")
    if pivot.shape[1] < 2:
        return None
    corr = pivot.corr(min_periods=3)
    n = corr.shape[0]
    iu = np.triu_indices(n, k=1)
    rho_pairs = corr.to_numpy()[iu]
    rho_pairs = rho_pairs[np.isfinite(rho_pairs)]
    if rho_pairs.size == 0:
        return None
    t_mean = pivot.count().mean()
    cd = np.sqrt(2 * t_mean / (n * (n - 1))) * rho_pairs.sum()
    return float(2 * (1 - stats.norm.cdf(abs(cd))))


def diagnostic_tests(
    fits: dict[str, PanelFitResult], labels: tuple[tuple[str, str], ...]
) -> pd.DataFrame:
    rows = []
    for label, fit_name in labels:
        fit = fits[fit_name]
        rows.append(
            {
                "block": label,
                "serial_correlation_test": "Breusch-Godfrey panel proxy",
                "serial_correlation_pvalue": panel_serial_correlation_test(fit),
                "heteroskedasticity_test": "Breusch-Pagan on model design",
                "breusch_pagan_pvalue": breusch_pagan_test(fit),
                "cross_sectional_dependence_test": "Pesaran CD",
                "pesaran_cd_pvalue": pesaran_cd_test(fit),
            }
        )
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Driscoll-Kraay robust covariance
# ---------------------------------------------------------------------------


def driscoll_kraay_se(fit: PanelFitResult, lags: int | None = None) -> pd.Series:
    """Driscoll-Kraay (Newey-West with cross-sectional aggregation) SEs.

    Implements the variant described in Driscoll & Kraay (1998) using residuals
    and the design matrix from a ``linearmodels`` fit. The default lag rule
    ``floor(4 * (T/100)**(2/9))`` matches R's ``vcovSCC`` default.
    """
    raw = fit.raw
    resid = fit.residuals.dropna()
    exog = raw.model.exog.dataframe.loc[resid.index]
    h_it = exog.mul(resid, axis=0)
    h_time = h_it.groupby(level="Year").sum()
    T = h_time.shape[0]
    if lags is None:
        lags = max(int(np.floor(4 * (T / 100) ** (2 / 9))), 0)
    centred = h_time - h_time.mean()
    s = centred.T @ centred / T
    for lag in range(1, lags + 1):
        weight = 1 - lag / (lags + 1)
        lead = centred.iloc[lag:].to_numpy()
        lag_block = centred.iloc[:-lag].to_numpy()
        gamma = lead.T @ lag_block / T
        s = s + weight * (gamma + gamma.T)
    xtx = (exog.T @ exog).to_numpy()
    xtx_inv = np.linalg.pinv(xtx)
    n = exog.shape[0]
    vcov = (n / T) * xtx_inv @ s.to_numpy() @ xtx_inv
    return pd.Series(np.sqrt(np.diag(vcov)), index=exog.columns)


def panel_robust_se_table(fit: PanelFitResult) -> pd.DataFrame:
    raw = fit.raw
    rows = []
    dk_se = driscoll_kraay_se(fit)
    arellano = raw.model.fit(cov_type="clustered", cluster_entity=True)
    double_cluster = raw.model.fit(
        cov_type="clustered", cluster_entity=True, cluster_time=True
    )
    time_cluster = raw.model.fit(cov_type="clustered", cluster_time=True)
    classical = raw
    for term in fit.params.index:
        rows.append(
            {
                "term": term,
                "coefficient": float(fit.params[term]),
                "se_classical": float(classical.std_errors[term]),
                "se_arellano": float(arellano.std_errors[term]),
                "se_time_cluster": float(time_cluster.std_errors[term]),
                "se_double_cluster": float(double_cluster.std_errors[term]),
                "se_driscoll_kraay": float(dk_se.get(term, np.nan)),
            }
        )
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Alternative fixed-effects regressions (compliance components)
# ---------------------------------------------------------------------------


def alternative_fe_table(frames: dict[str, pd.DataFrame]) -> pd.DataFrame:
    main = frames["main"]
    rows: list[dict[str, Any]] = []
    for variant in _ALT_FE_VARIANTS:
        predictors = (
            f"log_{variant}",
            "fertility",
            "lag_log_GDPpc2015",
            "gov_exp_reduced",
            "inflation",
            "investment",
            "tot_growth",
            "trade",
            "tot_growth_x_trade",
        )
        spec = PanelSpec(
            name=f"fe_{variant}",
            dataset="main",
            dependent="GDPgrowth",
            predictors=predictors,
            model_kind="fixed",
            table_group="fixed_effects_compliance_categories",
        )
        fit = fit_panel_model(spec, main)
        dk = driscoll_kraay_se(fit)
        for term in fit.params.index:
            rows.append(
                {
                    "compliance_variant": variant,
                    "term": term,
                    "coefficient": float(fit.params[term]),
                    "se_classical": float(fit.bse[term]),
                    "se_driscoll_kraay": float(dk.get(term, np.nan)),
                    "p_value": float(fit.pvalues[term]),
                    "nobs": fit.nobs,
                }
            )
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Output writing
# ---------------------------------------------------------------------------


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


def _format_pvalue(value: object) -> str:
    if value is None or pd.isna(value):
        return "NA"
    return f"{float(value):.4g}"


def _markdown_table(frame: pd.DataFrame, columns: tuple[str, ...]) -> list[str]:
    rows = [
        "| " + " | ".join(columns) + " |",
        "| " + " | ".join("---" for _ in columns) + " |",
    ]
    for _, row in frame.iterrows():
        values = []
        for column in columns:
            value = row[column]
            values.append(
                _format_pvalue(value) if isinstance(value, float) else str(value)
            )
        rows.append("| " + " | ".join(values) + " |")
    return rows


def _first_difference_omissions(summary: pd.DataFrame) -> list[str]:
    lines = []
    for spec in MODEL_SPECS:
        if spec.model_kind != "first_difference":
            continue
        terms = set(summary.loc[summary["model"] == spec.name, "term"])
        omitted = sorted(set(spec.predictors) - terms)
        if omitted:
            lines.append(
                f"- `{spec.name}` drops {', '.join(f'`{term}`' for term in omitted)} "
                "because the first-differenced regressor has no within-country "
                "variation in the complete model sample."
            )
    return lines


def write_panel_documentation(
    paths: tuple[Path, ...],
    summary: pd.DataFrame,
    spec_tests: pd.DataFrame,
    diag_tests: pd.DataFrame,
    robust_se: pd.DataFrame,
    docs_dir: Path = DOCS_DIR,
) -> Path:
    docs_dir.mkdir(parents=True, exist_ok=True)
    path = docs_dir / "panel_reproduction.md"
    artifact_lines = "\n".join(f"- `{_display_path(artifact)}`" for artifact in paths)
    fd_omissions = _first_difference_omissions(summary)
    if not fd_omissions:
        fd_omissions = ["- No first-difference regressors were dropped."]
    spec_display = spec_tests.rename(
        columns={
            "lm_pvalue": "LM p-value",
            "f_pooled_pvalue": "FE F-test p-value",
            "hausman_pvalue": "Hausman p-value",
        }
    )
    diag_display = diag_tests.rename(
        columns={
            "serial_correlation_pvalue": "Serial-correlation p-value",
            "breusch_pagan_pvalue": "Breusch-Pagan p-value",
            "pesaran_cd_pvalue": "Pesaran CD p-value",
        }
    )
    robust_columns = ", ".join(
        f"`{col}`" for col in robust_se.columns if col.startswith("se_")
    )
    path.write_text(
        "\n".join(
            [
                "# Panel Reproduction",
                "",
                "This document summarises the Python implementation of Phase 4.",
                "The source workflow is `panel/inorder_panel.R`; the tracked RDS",
                "files are treated as authoritative because the R script is an",
                "exploratory notebook-like script rather than a clean end-to-end run.",
                "",
                "## Implemented Outputs",
                "",
                artifact_lines,
                "",
                "## Matching Notes",
                "",
                "- Descriptive statistics and Spearman correlations follow the R",
                "  workflow on numeric columns of the alt panel (`model_data2`).",
                "  Plot pixels are best-effort; CSV values are the numerical",
                "  reproduction target.",
                "- Panel models are fit with `linearmodels`: `PooledOLS`,",
                "  `BetweenOLS`, `FirstDifferenceOLS`, `PanelOLS(entity_effects=True)`,",
                "  and `RandomEffects`. Three blocks cover the small-panel full",
                "  specification (2010-2019), the small-panel reduced specification",
                "  (drops `life_exp` and `education`, adds the `tot_growth*trade`",
                "  interaction), and the broad 1990-2020 main panel reduced spec.",
                "- First-difference models remove regressors that become all-zero",
                "  after within-country differencing, matching the effective model",
                "  fit rather than emitting blank inference columns.",
                *fd_omissions,
                "- Specification tests reproduce the LM test target against pooled",
                "  OLS (computed from pooled residuals using the Breusch-Pagan LM",
                "  formula), the F test for fixed effects against pooled OLS (from",
                "  `linearmodels.PanelResults.f_pooled`), and the Hausman test",
                "  (using the inverse covariance of the parameter difference).",
                "- Diagnostic tests map the R workflow's `pbgtest`, `bptest`, and",
                "  `pcdtest` calls to explicit Python implementations. The serial",
                "  correlation column is labelled as a Breusch-Godfrey panel proxy",
                "  because `plm::pbgtest` is not available in Python; the",
                "  Breusch-Pagan test uses the fitted model design matrix, and",
                "  Pesaran CD uses the residual correlation matrix.",
                "- Robust covariance variants for the main FE specification include",
                f"  {robust_columns}. This covers the R workflow's classical,",
                "  Arellano/entity-clustered, time-clustered, double-clustered, and",
                "  Driscoll-Kraay standard errors. Driscoll-Kraay is implemented in",
                "  Python following the SCC formulation with the lag truncation rule",
                "  `floor(4 * (T/100)**(2/9))` to match R's `sandwich::vcovSCC`.",
                "- Alternative fixed-effects regressions replicate the R block that",
                "  cycles `cc_basic`, `cc_civil`, `cc_polit`, `cc_prop` through the",
                "  reduced fixed-effects specification, reporting both classical and",
                "  Driscoll-Kraay standard errors.",
                "- Numerical differences from R can arise from `plm` vs",
                "  `linearmodels` finite-sample corrections and degrees-of-freedom",
                "  conventions. The reproduction contract treats these as",
                "  best-effort.",
                "",
                "## Current Specification-Test Evidence",
                "",
                *_markdown_table(
                    spec_display,
                    ("block", "LM p-value", "FE F-test p-value", "Hausman p-value"),
                ),
                "",
                "## Current Diagnostic-Test Evidence",
                "",
                *_markdown_table(
                    diag_display,
                    (
                        "block",
                        "serial_correlation_test",
                        "Serial-correlation p-value",
                        "heteroskedasticity_test",
                        "Breusch-Pagan p-value",
                        "cross_sectional_dependence_test",
                        "Pesaran CD p-value",
                    ),
                ),
                "",
            ]
        ),
        encoding="utf-8",
    )
    return path


def write_panel_outputs(
    output_dir: Path = OUTPUT_DIR,
    docs_dir: Path = DOCS_DIR,
    *,
    skip_plots: bool = False,
) -> PanelResult:
    output_dir.mkdir(parents=True, exist_ok=True)
    figure_dir = output_dir / "figures"
    if not skip_plots:
        figure_dir.mkdir(parents=True, exist_ok=True)
    docs_dir.mkdir(parents=True, exist_ok=True)

    frames = _prepared_panels()
    paths: list[Path] = []

    paths.extend(
        _write_table(
            panel_descriptive_statistics(frames["alt"]),
            "descriptive_statistics",
            output_dir,
        )
    )

    corr = panel_correlation_matrix(frames["alt"])
    paths.extend(
        _write_table(
            corr.reset_index().rename(columns={"index": "Variable"}),
            "spearman_correlation_matrix",
            output_dir,
        )
    )
    if not skip_plots:
        paths.append(write_panel_correlation_plot(corr, figure_dir))

    summary, fits = panel_summary_table(MODEL_SPECS, frames)
    paths.extend(_write_table(summary, "panel_models", output_dir))
    for table_group in (
        "models_small_full",
        "models_small_reduced",
        "models_main_reduced",
    ):
        group = select_model_group(
            summary,
            table_group,
            tuple(spec.name for spec in MODEL_SPECS if spec.table_group == table_group),
        )
        paths.extend(_write_table(group, table_group, output_dir))

    spec_blocks = (
        ("small_full", "small_full_pooling", "small_full_fixed", "small_full_random"),
        (
            "small_reduced",
            "small_reduced_pooling",
            "small_reduced_fixed",
            "small_reduced_random",
        ),
        (
            "main_reduced",
            "main_reduced_pooling",
            "main_reduced_fixed",
            "main_reduced_random",
        ),
    )
    spec_tests = specification_tests(fits, spec_blocks)
    paths.extend(_write_table(spec_tests, "specification_tests", output_dir))

    diag_blocks = (
        ("small_full", "small_full_fixed"),
        ("small_reduced", "small_reduced_fixed"),
        ("main_reduced", "main_reduced_fixed"),
    )
    diag_tests = diagnostic_tests(fits, diag_blocks)
    paths.extend(_write_table(diag_tests, "diagnostic_tests", output_dir))

    robust_se = panel_robust_se_table(fits["main_reduced_fixed"])
    paths.extend(_write_table(robust_se, "fixed_effects_main", output_dir))

    paths.extend(
        _write_table(
            alternative_fe_table(frames),
            "fixed_effects_compliance_categories",
            output_dir,
        )
    )

    documentation_path = write_panel_documentation(
        tuple(paths), summary, spec_tests, diag_tests, robust_se, docs_dir
    )
    return PanelResult(output_paths=tuple(paths), documentation_path=documentation_path)

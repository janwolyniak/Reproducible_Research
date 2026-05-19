"""Phase 0 source audit.

Inventories every tracked data input under ``cross-section/data`` and
``panel/data_panel``, reconciles the variable-name variants flagged in
``docs/reproduction_contract.md``, checks the cross-sectional outlier rule
numerically, and verifies that every artifact path referenced by the contract
exists on disk.

The result is written to ``docs/phase0_audit.md``.

Usage::

    py scripts/audit_phase0.py [--report PATH]
"""
from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable

import pandas as pd
import pyreadr


REPO = Path(__file__).resolve().parent.parent
RDS_DIRS = [REPO / "cross-section" / "data", REPO / "panel" / "data_panel"]

COUNTRY_NAME_CANDIDATES = (
    "Country",
    "country",
    "country_name",
    "Country_Name",
    "Country Name",
    "cname",
)
COUNTRY_CODE_CANDIDATES = (
    "Country Code",
    "Country_Code",
    "country_text_id",
    "iso3",
    "ISO3",
    "iso3c",
)
YEAR_CANDIDATES = ("Year", "year", "YEAR")

NAME_VARIANT_GROUPS: dict[str, tuple[str, ...]] = {
    "GDP growth": ("GDP_growth", "GDPgrowth", "gdp_growth"),
    "Terms of trade": ("tot", "tot2", "tot_growth"),
    "Country identifier": (
        "Country",
        "country",
        "Country Code",
        "Country_Code",
        "Country_Name",
        "Country Name",
        "cname",
    ),
    "Military government expenditure": ("gov_exp_milit", "gov_exp_mil"),
}

# Files the contract assumes are present.
CONTRACT_PATHS = [
    "2400-LIC-FII.pdf",
    "cross-section/inorder.R",
    "panel/inorder_panel.R",
    "helpers/rds_to_csv.py",
    "cross-section/data/CCCD_avg2010_19.rds",
    "cross-section/data/CCCD_detailed_avg2010_19.rds",
    "cross-section/data/CCCD_growth_final.rds",
    "cross-section/data/Democracy_avg2010_19.rds",
    "cross-section/data/GDPpc_current_initial_Data.rds",
    "cross-section/data/GDPpc_initial_in_2015usd_Data.rds",
    "cross-section/data/cc_growth_yty.rds",
    "cross-section/data/edu_initial_Data.rds",
    "cross-section/data/fertility_Data.rds",
    "cross-section/data/gdp_growth_Data.rds",
    "cross-section/data/geo_cepii.dta",
    "cross-section/data/gov_exp_Data.rds",
    "cross-section/data/gov_exp_edu_Data.rds",
    "cross-section/data/gov_milit_exp_Data.rds",
    "cross-section/data/inflation_cpi_Data.rds",
    "cross-section/data/inflation_gdpdeflator_Data.rds",
    "cross-section/data/investment_Data.rds",
    "cross-section/data/life_exp_initial_Data.rds",
    "cross-section/data/model_data.rds",
    "cross-section/data/model_data4.rds",
    "cross-section/data/model_data4_o.rds",
    "cross-section/data/oth_char.rds",
    "cross-section/data/tot2.rds",
    "cross-section/data/tot_Data.rds",
    "cross-section/data/trade_Data.rds",
    "panel/data_panel/model_data.rds",
    "panel/data_panel/model_data2.rds",
    "panel/data_panel/small_plm.rds",
    "cross-section/tables_and_other/descriptive stat.html",
    "cross-section/tables_and_other/Corrplot.png",
    "cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs total.html",
    "cross-section/tables_and_other/Spearman's rho Correlation Test Results growth vs prop.html",
    "cross-section/tables_and_other/reg_table1.html",
    "cross-section/tables_and_other/reg_table2&3.html",
    "cross-section/tables_and_other/reg_table_lv&prop.html",
    "cross-section/tables_and_other/reg_table_inst.html",
    "cross-section/tables_and_other/diagnostics.html",
    "panel/panel_tables/descriptive.html",
    "panel/panel_tables/Corrplot.png",
    "panel/panel_tables/Specification tests.html",
    "panel/panel_tables/Diagnostic tests.html",
    "panel/panel_tables/model_output.html",
    "panel/panel_tables/model_output2.html",
]


@dataclass
class FrameSummary:
    relpath: str
    object_name: str
    n_rows: int
    n_cols: int
    country_name_col: str | None
    country_code_col: str | None
    year_col: str | None
    n_unique_countries: int | None
    year_min: int | None
    year_max: int | None
    n_unique_years: int | None
    columns: list[tuple[str, str, int, float]] = field(default_factory=list)
    column_set: set[str] = field(default_factory=set)


def _first_present(df: pd.DataFrame, candidates: Iterable[str]) -> str | None:
    for c in candidates:
        if c in df.columns:
            return c
    return None


def _load(path: Path) -> dict[str, pd.DataFrame]:
    if path.suffix.lower() == ".dta":
        return {path.stem: pd.read_stata(path)}

    result = pyreadr.read_r(str(path))
    return {k: v for k, v in result.items() if isinstance(v, pd.DataFrame)}


def summarize_frame(df: pd.DataFrame, source: Path, obj_name: str) -> FrameSummary:
    n_rows, n_cols = df.shape
    country_name_col = _first_present(df, COUNTRY_NAME_CANDIDATES)
    country_code_col = _first_present(df, COUNTRY_CODE_CANDIDATES)
    year_col = _first_present(df, YEAR_CANDIDATES)

    n_unique_countries: int | None = None
    if country_name_col is not None:
        n_unique_countries = int(df[country_name_col].nunique(dropna=True))
    elif country_code_col is not None:
        n_unique_countries = int(df[country_code_col].nunique(dropna=True))

    year_min = year_max = n_unique_years = None
    if year_col is not None:
        years = pd.to_numeric(df[year_col], errors="coerce").dropna()
        if not years.empty:
            year_min = int(years.min())
            year_max = int(years.max())
            n_unique_years = int(years.nunique())

    miss = df.isna().sum()
    columns: list[tuple[str, str, int, float]] = []
    for col in df.columns:
        m = int(miss[col])
        pct = round(m / n_rows * 100, 1) if n_rows else 0.0
        columns.append((str(col), str(df[col].dtype), m, pct))

    return FrameSummary(
        relpath=str(source.relative_to(REPO)).replace("\\", "/"),
        object_name=obj_name,
        n_rows=int(n_rows),
        n_cols=int(n_cols),
        country_name_col=country_name_col,
        country_code_col=country_code_col,
        year_col=year_col,
        n_unique_countries=n_unique_countries,
        year_min=year_min,
        year_max=year_max,
        n_unique_years=n_unique_years,
        columns=columns,
        column_set=set(df.columns),
    )


def collect_summaries() -> tuple[list[FrameSummary], dict[Path, dict[str, pd.DataFrame]]]:
    summaries: list[FrameSummary] = []
    raw: dict[Path, dict[str, pd.DataFrame]] = {}
    for d in RDS_DIRS:
        if not d.is_dir():
            print(f"[WARN] missing dir: {d}", file=sys.stderr)
            continue
        files = sorted([*d.glob("*.rds"), *d.glob("*.dta")])
        for data_file in files:
            try:
                objs = _load(data_file)
            except Exception as exc:
                print(f"[ERROR] cannot read {data_file}: {exc}", file=sys.stderr)
                continue
            if not objs:
                print(f"[WARN] no DataFrame in {data_file}", file=sys.stderr)
                continue
            raw[data_file] = objs
            for name, df in objs.items():
                display = name if name else data_file.stem
                summaries.append(summarize_frame(df, data_file, display))
    return summaries, raw


def build_name_variant_table(summaries: list[FrameSummary]) -> dict[str, dict[str, list[str]]]:
    table: dict[str, dict[str, list[str]]] = {}
    for group_name, variants in NAME_VARIANT_GROUPS.items():
        table[group_name] = {v: [] for v in variants}
        for s in summaries:
            for v in variants:
                if v in s.column_set:
                    table[group_name][v].append(f"{s.relpath} ({s.object_name})")
    return table


def outlier_rule_check(
    raw: dict[Path, dict[str, pd.DataFrame]],
) -> dict[str, object]:
    """Compare cross-section model_data4 vs model_data4_o.

    The contract notes the paper says five outliers were removed leaving 157
    countries, while OLS tables show 121-122 observations.
    """
    xs_dir = REPO / "cross-section" / "data"
    candidates = {
        "model_data": xs_dir / "model_data.rds",
        "model_data4": xs_dir / "model_data4.rds",
        "model_data4_o": xs_dir / "model_data4_o.rds",
    }
    frames: dict[str, pd.DataFrame] = {}
    for key, path in candidates.items():
        if path in raw:
            obj = next(iter(raw[path].values()))
            frames[key] = obj

    out: dict[str, object] = {}
    rows = []
    for key, df in frames.items():
        cn = _first_present(df, COUNTRY_NAME_CANDIDATES) or _first_present(
            df, COUNTRY_CODE_CANDIDATES
        )
        n_countries = df[cn].nunique(dropna=True) if cn else None
        rows.append({"dataset": key, "rows": len(df), "countries": n_countries, "country_col": cn})
    out["rows"] = rows

    if "model_data4" in frames and "model_data4_o" in frames:
        a, b = frames["model_data4"], frames["model_data4_o"]
        ca = _first_present(a, COUNTRY_NAME_CANDIDATES) or _first_present(a, COUNTRY_CODE_CANDIDATES)
        cb = _first_present(b, COUNTRY_NAME_CANDIDATES) or _first_present(b, COUNTRY_CODE_CANDIDATES)
        if ca and cb:
            countries_a = set(a[ca].dropna().astype(str))
            countries_b = set(b[cb].dropna().astype(str))
            out["dropped_from_model_data4"] = sorted(countries_a - countries_b)
            out["added_in_model_data4_o"] = sorted(countries_b - countries_a)
    return out


def contract_coverage() -> list[tuple[str, bool]]:
    return [(p, (REPO / p).exists()) for p in CONTRACT_PATHS]


def render_report(
    summaries: list[FrameSummary],
    variants: dict[str, dict[str, list[str]]],
    outlier: dict[str, object],
    coverage: list[tuple[str, bool]],
) -> str:
    lines: list[str] = []
    lines.append("# Phase 0 Audit Report")
    lines.append("")
    lines.append(
        "Auto-generated by `scripts/audit_phase0.py`. Re-run after any change to "
        "the tracked data inputs or contract paths."
    )
    lines.append("")

    # 1. Inventory summary
    lines.append("## 1. Data inventory")
    lines.append("")
    lines.append("| File | Object | Rows | Cols | Country col | Year col | Countries | Years |")
    lines.append("| --- | --- | ---: | ---: | --- | --- | ---: | --- |")
    for s in summaries:
        ccol = s.country_name_col or s.country_code_col or "-"
        ycol = s.year_col or "-"
        years = (
            f"{s.year_min}-{s.year_max} ({s.n_unique_years})"
            if s.year_min is not None
            else "-"
        )
        countries = s.n_unique_countries if s.n_unique_countries is not None else "-"
        lines.append(
            f"| `{s.relpath}` | `{s.object_name}` | {s.n_rows} | {s.n_cols} | "
            f"{ccol} | {ycol} | {countries} | {years} |"
        )
    lines.append("")

    # 2. Per-file column detail
    lines.append("## 2. Per-file column detail")
    for s in summaries:
        lines.append("")
        lines.append(f"### `{s.relpath}` :: `{s.object_name}`")
        lines.append("")
        lines.append("| Column | Dtype | Missing | % Missing |")
        lines.append("| --- | --- | ---: | ---: |")
        for name, dtype, n_miss, pct in s.columns:
            lines.append(f"| `{name}` | `{dtype}` | {n_miss} | {pct}% |")
    lines.append("")

    # 3. Name-variant reconciliation
    lines.append("## 3. Variable-name variants")
    lines.append("")
    lines.append(
        "Each block lists where every named variant of a contested variable "
        "appears. An empty list means that variant is not used anywhere on disk."
    )
    for group_name, hits in variants.items():
        lines.append("")
        lines.append(f"### {group_name}")
        lines.append("")
        for variant, files in hits.items():
            if files:
                lines.append(f"- `{variant}`")
                for f in files:
                    lines.append(f"  - {f}")
            else:
                lines.append(f"- `{variant}` - *(not present in any tracked file)*")
    lines.append("")

    # 4. Outlier rule
    lines.append("## 4. Cross-sectional outlier rule")
    lines.append("")
    lines.append(
        "Paper claim: five outliers removed, final sample of 157 countries; "
        "main OLS tables report 121-122 observations after model-wise "
        "missingness. Observed counts in the tracked `.rds` files:"
    )
    lines.append("")
    lines.append("| Dataset | Rows | Unique countries | Country column |")
    lines.append("| --- | ---: | ---: | --- |")
    for row in outlier.get("rows", []):  # type: ignore[arg-type]
        ds = row["dataset"]
        nr = row["rows"]
        nc = row["countries"] if row["countries"] is not None else "-"
        col = row["country_col"] or "-"
        lines.append(f"| `{ds}` | {nr} | {nc} | `{col}` |")
    dropped = outlier.get("dropped_from_model_data4")
    added = outlier.get("added_in_model_data4_o")
    if dropped is not None:
        lines.append("")
        lines.append(
            f"Countries in `model_data4` but missing from `model_data4_o` "
            f"({len(dropped)}):"
        )
        lines.append("")
        if dropped:
            for c in dropped:  # type: ignore[union-attr]
                lines.append(f"- {c}")
        else:
            lines.append("- *(none)*")
    if added:
        lines.append("")
        lines.append(
            f"Countries in `model_data4_o` but not in `model_data4` "
            f"({len(added)}):"  # type: ignore[arg-type]
        )
        lines.append("")
        for c in added:  # type: ignore[union-attr]
            lines.append(f"- {c}")
    lines.append("")

    # 5. Contract coverage
    lines.append("## 5. Contract artifact coverage")
    lines.append("")
    lines.append("| Path | Present |")
    lines.append("| --- | :---: |")
    for path, present in coverage:
        mark = "yes" if present else "**MISSING**"
        lines.append(f"| `{path}` | {mark} |")
    missing = [p for p, ok in coverage if not ok]
    if missing:
        lines.append("")
        lines.append(f"{len(missing)} contract path(s) are missing on disk.")
    lines.append("")

    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--report",
        type=Path,
        default=REPO / "docs" / "phase0_audit.md",
        help="Output markdown path (default: docs/phase0_audit.md)",
    )
    args = parser.parse_args()

    summaries, raw = collect_summaries()
    variants = build_name_variant_table(summaries)
    outlier = outlier_rule_check(raw)
    coverage = contract_coverage()
    report = render_report(summaries, variants, outlier, coverage)

    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(report, encoding="utf-8")

    print(f"Wrote {args.report.relative_to(REPO)}")
    print(
        f"  files inspected: {len({s.relpath for s in summaries})}  "
        f"frames: {len(summaries)}  "
        f"missing contract paths: {sum(1 for _, ok in coverage if not ok)}"
    )

    # Sanity check: surface untracked variants no one expected.
    return 0


if __name__ == "__main__":
    sys.exit(main())

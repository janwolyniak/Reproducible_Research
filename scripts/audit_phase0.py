"""Phase 0 source audit.

Inventories every tracked data input under ``cross-section/data`` and
``panel/data_panel``, reconciles the variable-name variants flagged in
``docs/reproduction_contract.md``, checks the cross-sectional outlier rule
numerically, and verifies that every artifact path referenced by the contract
exists on disk.

The result is written to ``docs/phase0_audit.md``.

This script intentionally uses only the Python standard library and a local
``Rscript`` executable. Phase 0 runs before the Python environment baseline is
defined, so the audit should not depend on pandas or pyreadr being installed.

Usage::

    py scripts/audit_phase0.py [--report PATH]
"""
from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from urllib.parse import unquote


REPO = Path(__file__).resolve().parent.parent
DATA_DIRS = [REPO / "cross-section" / "data", REPO / "panel" / "data_panel"]

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

R_AUDIT_SCRIPT = r"""
args <- commandArgs(trailingOnly = TRUE)
path <- args[[1]]
stem <- tools::file_path_sans_ext(basename(path))

enc <- function(x) {
  text <- iconv(as.character(x), from = "", to = "UTF-8", sub = "byte")
  utils::URLencode(gsub("\n", " ", text, fixed = TRUE), reserved = TRUE)
}

first_present <- function(cols, candidates) {
  hit <- candidates[candidates %in% cols]
  if (length(hit) == 0) "" else hit[[1]]
}

emit <- function(...) {
  cat(paste(vapply(list(...), enc, character(1)), collapse = "\t"), "\n", sep = "")
}

as_frame_list <- function(obj) {
  if (is.data.frame(obj)) {
    out <- list(obj)
    names(out) <- stem
    return(out)
  }
  if (is.list(obj)) {
    keep <- vapply(obj, is.data.frame, logical(1))
    return(obj[keep])
  }
  list()
}

ext <- tolower(tools::file_ext(path))
obj <- if (ext == "dta") {
  foreign::read.dta(path, convert.factors = TRUE)
} else {
  readRDS(path)
}

frames <- as_frame_list(obj)
if (length(frames) == 0) {
  stop("No data.frame object found")
}

country_name_candidates <- c("Country", "country", "country_name", "Country_Name",
                             "Country Name", "cname")
country_code_candidates <- c("Country Code", "Country_Code", "country_text_id",
                             "iso3", "ISO3", "iso3c")
year_candidates <- c("Year", "year", "YEAR")

for (i in seq_along(frames)) {
  df <- frames[[i]]
  obj_name <- names(frames)[[i]]
  if (is.null(obj_name) || obj_name == "") obj_name <- stem
  cols <- names(df)

  country_name_col <- first_present(cols, country_name_candidates)
  country_code_col <- first_present(cols, country_code_candidates)
  year_col <- first_present(cols, year_candidates)
  country_col <- if (country_name_col != "") country_name_col else country_code_col

  countries <- ""
  if (country_col != "") countries <- length(unique(stats::na.omit(df[[country_col]])))

  year_min <- ""
  year_max <- ""
  year_n <- ""
  if (year_col != "") {
    years <- suppressWarnings(as.numeric(df[[year_col]]))
    years <- years[!is.na(years)]
    if (length(years) > 0) {
      year_min <- min(years)
      year_max <- max(years)
      year_n <- length(unique(years))
    }
  }

  emit("FRAME", obj_name, nrow(df), ncol(df), country_name_col, country_code_col,
       year_col, countries, year_min, year_max, year_n)

  if (country_col != "") {
    values <- sort(unique(as.character(stats::na.omit(df[[country_col]]))))
    for (value in values) {
      emit("COUNTRY", obj_name, country_col, value)
    }
  }

  for (col in cols) {
    values <- df[[col]]
    missing <- sum(is.na(values))
    pct <- if (nrow(df) == 0) 0 else round(missing / nrow(df) * 100, 1)
    dtype <- paste(class(values), collapse = ",")
    emit("COLUMN", obj_name, col, dtype, missing, pct)
  }
}
"""


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
    countries: set[str] = field(default_factory=set)


def _decode_fields(line: str) -> list[str]:
    return [unquote(part) for part in line.rstrip("\n").split("\t")]


def _none_if_empty(value: str) -> str | None:
    return value if value else None


def _int_or_none(value: str) -> int | None:
    if value == "":
        return None
    return int(float(value))


def _run_r_audit(path: Path, rscript: str) -> list[str]:
    result = subprocess.run(
        [rscript, "--vanilla", "-e", R_AUDIT_SCRIPT, str(path)],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        message = result.stderr.strip() or result.stdout.strip()
        raise RuntimeError(message)
    return result.stdout.splitlines()


def _parse_r_output(source: Path, lines: list[str]) -> list[FrameSummary]:
    relpath = str(source.relative_to(REPO)).replace("\\", "/")
    frames: dict[str, FrameSummary] = {}

    for line in lines:
        fields = _decode_fields(line)
        if not fields:
            continue
        kind = fields[0]
        if kind == "FRAME":
            obj_name = fields[1]
            frames[obj_name] = FrameSummary(
                relpath=relpath,
                object_name=obj_name,
                n_rows=int(fields[2]),
                n_cols=int(fields[3]),
                country_name_col=_none_if_empty(fields[4]),
                country_code_col=_none_if_empty(fields[5]),
                year_col=_none_if_empty(fields[6]),
                n_unique_countries=_int_or_none(fields[7]),
                year_min=_int_or_none(fields[8]),
                year_max=_int_or_none(fields[9]),
                n_unique_years=_int_or_none(fields[10]),
            )
        elif kind == "COUNTRY":
            obj_name = fields[1]
            if obj_name in frames:
                frames[obj_name].countries.add(fields[3])
        elif kind == "COLUMN":
            obj_name = fields[1]
            if obj_name in frames:
                name = fields[2]
                dtype = fields[3]
                n_miss = int(fields[4])
                pct = float(fields[5])
                frames[obj_name].columns.append((name, dtype, n_miss, pct))
                frames[obj_name].column_set.add(name)

    return list(frames.values())


def collect_summaries(rscript: str) -> tuple[list[FrameSummary], list[tuple[str, str]]]:
    summaries: list[FrameSummary] = []
    errors: list[tuple[str, str]] = []

    for data_dir in DATA_DIRS:
        if not data_dir.is_dir():
            errors.append((str(data_dir.relative_to(REPO)), "directory missing"))
            continue
        files = sorted([*data_dir.glob("*.rds"), *data_dir.glob("*.dta")])
        for data_file in files:
            try:
                output = _run_r_audit(data_file, rscript)
                summaries.extend(_parse_r_output(data_file, output))
            except Exception as exc:  # noqa: BLE001 - report all audit read failures.
                relpath = str(data_file.relative_to(REPO)).replace("\\", "/")
                errors.append((relpath, str(exc)))

    return summaries, errors


def build_name_variant_table(summaries: list[FrameSummary]) -> dict[str, dict[str, list[str]]]:
    table: dict[str, dict[str, list[str]]] = {}
    for group_name, variants in NAME_VARIANT_GROUPS.items():
        table[group_name] = {v: [] for v in variants}
        for summary in summaries:
            for variant in variants:
                if variant in summary.column_set:
                    table[group_name][variant].append(
                        f"{summary.relpath} ({summary.object_name})"
                    )
    return table


def outlier_rule_check(summaries: list[FrameSummary]) -> dict[str, object]:
    frames = {
        Path(summary.relpath).stem: summary
        for summary in summaries
        if summary.relpath
        in {
            "cross-section/data/model_data.rds",
            "cross-section/data/model_data4.rds",
            "cross-section/data/model_data4_o.rds",
        }
    }

    rows = []
    for key in ("model_data", "model_data4", "model_data4_o"):
        summary = frames.get(key)
        if summary is None:
            continue
        rows.append(
            {
                "dataset": key,
                "rows": summary.n_rows,
                "countries": summary.n_unique_countries,
                "country_col": summary.country_name_col or summary.country_code_col,
            }
        )

    out: dict[str, object] = {"rows": rows}
    if "model_data4" in frames and "model_data4_o" in frames:
        out["dropped_from_model_data4"] = sorted(
            frames["model_data4"].countries - frames["model_data4_o"].countries
        )
        out["added_in_model_data4_o"] = sorted(
            frames["model_data4_o"].countries - frames["model_data4"].countries
        )
    return out


def contract_coverage() -> list[tuple[str, bool]]:
    return [(p, (REPO / p).exists()) for p in CONTRACT_PATHS]


def render_report(
    summaries: list[FrameSummary],
    variants: dict[str, dict[str, list[str]]],
    outlier: dict[str, object],
    coverage: list[tuple[str, bool]],
    errors: list[tuple[str, str]],
) -> str:
    lines: list[str] = []
    lines.append("# Phase 0 Audit Report")
    lines.append("")
    lines.append(
        "Auto-generated by `scripts/audit_phase0.py`. Re-run after any change to "
        "the tracked data inputs or contract paths."
    )
    lines.append("")

    lines.append("## 1. Data inventory")
    lines.append("")
    lines.append("| File | Object | Rows | Cols | Country col | Year col | Countries | Years |")
    lines.append("| --- | --- | ---: | ---: | --- | --- | ---: | --- |")
    for summary in summaries:
        ccol = summary.country_name_col or summary.country_code_col or "-"
        ycol = summary.year_col or "-"
        years = (
            f"{summary.year_min}-{summary.year_max} ({summary.n_unique_years})"
            if summary.year_min is not None
            else "-"
        )
        countries = (
            summary.n_unique_countries if summary.n_unique_countries is not None else "-"
        )
        lines.append(
            f"| `{summary.relpath}` | `{summary.object_name}` | {summary.n_rows} | "
            f"{summary.n_cols} | {ccol} | {ycol} | {countries} | {years} |"
        )
    lines.append("")

    lines.append("## 2. Per-file column detail")
    for summary in summaries:
        lines.append("")
        lines.append(f"### `{summary.relpath}` :: `{summary.object_name}`")
        lines.append("")
        lines.append("| Column | Dtype | Missing | % Missing |")
        lines.append("| --- | --- | ---: | ---: |")
        for name, dtype, n_miss, pct in summary.columns:
            lines.append(f"| `{name}` | `{dtype}` | {n_miss} | {pct}% |")
    lines.append("")

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
                for file_path in files:
                    lines.append(f"  - {file_path}")
            else:
                lines.append(f"- `{variant}` - *(not present in any tracked file)*")
    lines.append("")

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
            for country in dropped:  # type: ignore[union-attr]
                lines.append(f"- {country}")
        else:
            lines.append("- *(none)*")
    if added:
        lines.append("")
        lines.append(
            f"Countries in `model_data4_o` but not in `model_data4` "
            f"({len(added)}):"  # type: ignore[arg-type]
        )
        lines.append("")
        for country in added:  # type: ignore[union-attr]
            lines.append(f"- {country}")
    lines.append("")

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

    lines.append("## 6. Audit read errors")
    lines.append("")
    if errors:
        lines.append("| Path | Error |")
        lines.append("| --- | --- |")
        for path, error in errors:
            clean_error = error.replace("\n", " ")
            lines.append(f"| `{path}` | {clean_error} |")
    else:
        lines.append("No audit read errors.")
    lines.append("")

    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--report",
        type=Path,
        default=REPO / "docs" / "phase0_audit.md",
        help="Output markdown path (default: docs/phase0_audit.md)",
    )
    parser.add_argument(
        "--rscript",
        default=shutil.which("Rscript") or "Rscript",
        help="Path to Rscript executable (default: discovered on PATH)",
    )
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()

    if shutil.which(args.rscript) is None and not Path(args.rscript).exists():
        print(
            "Rscript is required for the Phase 0 audit but was not found.",
            file=sys.stderr,
        )
        return 1

    summaries, errors = collect_summaries(args.rscript)
    variants = build_name_variant_table(summaries)
    outlier = outlier_rule_check(summaries)
    coverage = contract_coverage()
    report = render_report(summaries, variants, outlier, coverage, errors)

    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(report, encoding="utf-8")

    missing_contract = sum(1 for _, ok in coverage if not ok)
    print(f"Wrote {args.report.relative_to(REPO)}")
    print(
        f"  files inspected: {len({s.relpath for s in summaries})}  "
        f"frames: {len(summaries)}  "
        f"read errors: {len(errors)}  "
        f"missing contract paths: {missing_contract}"
    )

    if errors or missing_contract:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

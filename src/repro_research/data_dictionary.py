from __future__ import annotations

from collections import defaultdict
from pathlib import Path

import pandas as pd

from repro_research.data_io import DatasetInventoryRow
from repro_research.paths import PROJECT_ROOT

INVENTORY_COLUMNS = (
    "group",
    "dataset",
    "relative_path",
    "object_name",
    "rows",
    "columns",
    "country_name_column",
    "country_code_column",
    "year_column",
    "missing_cells",
    "column_names_json",
    "dtypes_json",
)

CONTRIBUTOR_FRAGMENTS: tuple[str, ...] = ()


def write_inventory_csv(rows: list[DatasetInventoryRow], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    frame = pd.DataFrame([row.as_dict() for row in rows], columns=INVENTORY_COLUMNS)
    frame.to_csv(output_path, index=False)
    return output_path


def build_data_dictionary_markdown(rows: list[DatasetInventoryRow]) -> str:
    grouped_rows: dict[str, list[DatasetInventoryRow]] = defaultdict(list)
    for row in rows:
        grouped_rows[row.group].append(row)

    lines = [
        "# Data Dictionary",
        "",
        "This file records the tracked source datasets, their row and column "
        "counts, detected country/year identifier columns, and missing cell "
        "counts.",
        "",
        "Model-specific variables and transformations are implemented in "
        "`src/repro_research/cross_section_data.py` and "
        "`src/repro_research/panel_data.py`.",
        "",
        "The machine-readable inventory is regenerated at "
        "`docs/data_inventory.csv` by `python scripts/inventory_data.py` and "
        "`python scripts/run_all.py`.",
        "",
    ]

    for group in sorted(grouped_rows):
        title = (
            "Cross-Sectional Sources" if group == "cross_section" else "Panel Sources"
        )
        lines.extend([f"## {title}", ""])
        lines.extend(
            [
                "| Dataset | Rows | Columns | Country name | Country code | Year | Missing cells |",
                "| --- | ---: | ---: | --- | --- | --- | ---: |",
            ]
        )
        for row in sorted(grouped_rows[group], key=lambda item: item.relative_path):
            lines.append(
                "| "
                f"`{row.relative_path}` | "
                f"{row.rows} | "
                f"{row.columns} | "
                f"{row.country_name_column or '-'} | "
                f"{row.country_code_column or '-'} | "
                f"{row.year_column or '-'} | "
                f"{row.missing_cells} |"
            )
        lines.append("")

    lines.extend(
        [
            "## Shared Loading Contract",
            "",
            "- Use `repro_research.data_io.build_dataset_specs()` to list the "
            "tracked source datasets.",
            "- Use `repro_research.data_io.load_dataset(spec)` for the default "
            "data frame from one source file.",
            "- Use `repro_research.data_io.read_rds_dataframe(path)` when a "
            "contributor needs to load a specific `.rds` file directly.",
            "- Treat `cross-section/data/` and `panel/data_panel/` as "
            "authoritative inputs; generated caches belong under "
            "`outputs/intermediate/` only when they make review easier.",
            "",
        ]
    )

    for fragment_rel in CONTRIBUTOR_FRAGMENTS:
        fragment_path = PROJECT_ROOT / fragment_rel
        if not fragment_path.is_file():
            continue
        fragment_body = fragment_path.read_text(encoding="utf-8").rstrip()
        if fragment_body:
            lines.append(fragment_body)
            lines.append("")

    return "\n".join(lines)


def write_data_dictionary(rows: list[DatasetInventoryRow], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(build_data_dictionary_markdown(rows), encoding="utf-8")
    return output_path

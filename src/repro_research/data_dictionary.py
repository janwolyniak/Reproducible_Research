from __future__ import annotations

from collections import defaultdict
from pathlib import Path

import pandas as pd

from repro_research.data_io import DatasetInventoryRow

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
        "This file is the Phase 2 shared data inventory for the Python "
        "reproduction. It records the tracked source datasets, their row and "
        "column counts, detected country/year identifier columns, and missing "
        "cell counts.",
        "",
        "Model-specific variable definitions, transformations, and final "
        "schemas remain contributor-owned: Kinga for the cross-sectional "
        "reproduction and Iwo for the panel reproduction. They should extend "
        "this document once their Phase 2 preparation code fixes the final "
        "model variables.",
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
    return "\n".join(lines)


def write_data_dictionary(rows: list[DatasetInventoryRow], output_path: Path) -> Path:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(build_data_dictionary_markdown(rows), encoding="utf-8")
    return output_path

from __future__ import annotations

import argparse
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.data_dictionary import (  # noqa: E402
    write_data_dictionary,
    write_inventory_csv,
)
from repro_research.data_io import build_dataset_specs, build_inventory  # noqa: E402


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Inventory tracked source datasets.")
    parser.add_argument(
        "--inventory-output",
        type=Path,
        default=ROOT / "docs" / "data_inventory.csv",
        help="CSV path for the machine-readable inventory.",
    )
    parser.add_argument(
        "--dictionary-output",
        type=Path,
        default=ROOT / "outputs" / "logs" / "data_dictionary.md",
        help="Markdown path for the shared data dictionary scaffold.",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    rows = build_inventory(build_dataset_specs())
    inventory_path = write_inventory_csv(rows, args.inventory_output)
    dictionary_path = write_data_dictionary(rows, args.dictionary_output)
    print(f"Wrote {inventory_path.relative_to(ROOT)}")
    print(f"Wrote {dictionary_path.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

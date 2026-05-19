from __future__ import annotations

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.cross_section_data import (  # noqa: E402
    write_cross_section_preparation_outputs,
)


def main() -> int:
    result = write_cross_section_preparation_outputs()
    for path in (
        result.inventory_path,
        result.summary_path,
        result.model_data4_path,
        result.model_data4_o_path,
        result.data_dictionary_path,
    ):
        print(f"Wrote {path.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

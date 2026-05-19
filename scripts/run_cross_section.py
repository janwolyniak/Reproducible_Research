from __future__ import annotations

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.cross_section import write_cross_section_outputs  # noqa: E402


def main() -> int:
    result = write_cross_section_outputs()
    for path in (*result.output_paths, result.documentation_path):
        print(f"Wrote {path.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

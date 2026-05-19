from __future__ import annotations

import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MPLCONFIGDIR = Path(os.environ.get("TMPDIR", "/tmp")) / "repro_research_mpl"
MPLCONFIGDIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(MPLCONFIGDIR))
sys.path.insert(0, str(ROOT / "src"))

from repro_research.panel import write_panel_outputs  # noqa: E402


def main() -> int:
    result = write_panel_outputs()
    for path in (*result.output_paths, result.documentation_path):
        print(f"Wrote {path.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

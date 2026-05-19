from __future__ import annotations

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.pipeline import run_all  # noqa: E402

if __name__ == "__main__":
    raise SystemExit(run_all())

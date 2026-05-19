"""Phase 2 panel preparation entry point.

Loads the three tracked panel datasets, applies the canonical transformations,
and writes the prepared frames as CSV under ``outputs/intermediate/panel/`` so
they are inspectable from any environment without extra dependencies. The
cached files are convenience artefacts only -- the full pipeline can rebuild
them on demand from the tracked ``.rds`` inputs.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.panel_data import PANEL_SPECS, load_panel
from repro_research.panel_transforms import prepare_for_models
from repro_research.paths import PROJECT_ROOT

_DEFAULT_OUT = PROJECT_ROOT / "outputs" / "intermediate" / "panel"


def _summarise(name: str, df: pd.DataFrame) -> str:
    years = df["Year"]
    return (
        f"{name}: rows={len(df)} cols={df.shape[1]} "
        f"countries={df['country'].nunique()} "
        f"years={int(years.min())}-{int(years.max())}"
    )


def run(output_dir: Path = _DEFAULT_OUT) -> int:
    output_dir.mkdir(parents=True, exist_ok=True)
    lines: list[str] = []
    for kind, spec in PANEL_SPECS.items():
        df = load_panel(kind)
        if len(df) != spec.expected_rows:
            print(
                f"[ERROR] {spec.rel_path}: expected {spec.expected_rows} rows, "
                f"got {len(df)}",
                file=sys.stderr,
            )
            return 1
        prepared = prepare_for_models(df)
        out_path = output_dir / f"{kind}.csv"
        prepared.to_csv(out_path, index=False)
        lines.append(_summarise(kind, prepared))
        lines.append(f"  -> {out_path.relative_to(PROJECT_ROOT)}")
    print("Panel preparation:")
    print("\n".join(f"  {line}" for line in lines))
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=_DEFAULT_OUT,
        help="Directory where the prepared CSV files are written.",
    )
    args = parser.parse_args()
    return run(args.output_dir)


if __name__ == "__main__":
    sys.exit(main())

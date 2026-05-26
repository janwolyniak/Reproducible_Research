from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MPLCONFIGDIR = ROOT / "outputs" / "intermediate" / "matplotlib"
MPLCONFIGDIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(MPLCONFIGDIR))
sys.path.insert(0, str(ROOT / "src"))

from repro_research.cross_section import write_cross_section_outputs  # noqa: E402


def _resolve(path: Path) -> Path:
    return path if path.is_absolute() else ROOT / path


def _display(path: Path) -> str:
    try:
        return path.relative_to(ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Regenerate cross-sectional reproduction tables and figures."
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=ROOT / "outputs" / "cross_section",
        help="Directory where cross-sectional tables and figures are written.",
    )
    parser.add_argument(
        "--docs-dir",
        type=Path,
        default=ROOT / "outputs" / "logs",
        help="Directory where the cross-sectional summary is written.",
    )
    parser.add_argument(
        "--skip-plots",
        action="store_true",
        help="Regenerate tables without refreshing PNG figures.",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    result = write_cross_section_outputs(
        output_dir=_resolve(args.output_dir),
        docs_dir=_resolve(args.docs_dir),
        skip_plots=args.skip_plots,
    )
    for path in (*result.output_paths, result.documentation_path):
        print(f"Wrote {_display(path)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

from __future__ import annotations

import argparse
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.pipeline import RunAllOptions, run_all  # noqa: E402


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the complete Python reproduction pipeline."
    )
    parser.add_argument(
        "--skip-plots",
        action="store_true",
        help="Regenerate tables and documentation without refreshing PNG figures.",
    )
    parser.add_argument(
        "--strict-validation",
        action="store_true",
        help="Require the complete generated-output manifest after the run.",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    strict_validation = args.strict_validation or not args.skip_plots
    options = RunAllOptions(
        skip_plots=args.skip_plots,
        strict_validation=strict_validation,
    )
    return run_all(options)


if __name__ == "__main__":
    raise SystemExit(main())

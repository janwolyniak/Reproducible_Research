from __future__ import annotations

import argparse
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.validation import validate_project  # noqa: E402


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Validate required project inputs and output directories."
    )
    parser.add_argument(
        "--create-output-dirs",
        action="store_true",
        help="Create the expected output directories before validation.",
    )
    parser.add_argument(
        "--generated-outputs",
        action="store_true",
        help="Also require the generated-output artifact manifest.",
    )
    return parser


def main() -> int:
    args = build_parser().parse_args()
    result = validate_project(
        create_output_dirs=args.create_output_dirs,
        generated_outputs=args.generated_outputs,
    )
    print(result.render())
    return 0 if result.ok else 1


if __name__ == "__main__":
    raise SystemExit(main())

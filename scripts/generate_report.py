"""Reviewer entry point: run the full pipeline, then publish the QMD report."""

from __future__ import annotations

import argparse
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.pipeline import RunAllOptions, run_all  # noqa: E402

REPORT_DIR = ROOT / "report"
OUTPUT_REPORT_DIR = ROOT / "outputs" / "report"


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Run the full Python reproduction pipeline and copy the Quarto "
            "source report to outputs/report/report.qmd."
        )
    )
    parser.add_argument(
        "--skip-pipeline",
        action="store_true",
        help=(
            "Skip running scripts/run_all.py first. Use this when the pipeline "
            "has already produced fresh outputs and only the report needs "
            "regeneration."
        ),
    )
    parser.add_argument(
        "--skip-plots",
        action="store_true",
        help="Pass --skip-plots to the pipeline.",
    )
    parser.add_argument(
        "--strict-validation",
        action="store_true",
        help="Pass --strict-validation to the pipeline.",
    )
    return parser


def _run_pipeline(skip_plots: bool, strict_validation: bool) -> int:
    options = RunAllOptions(
        skip_plots=skip_plots,
        strict_validation=strict_validation or not skip_plots,
    )
    print("[generate_report] Running full reproduction pipeline ...", flush=True)
    code = run_all(options)
    if code != 0:
        print(
            f"[generate_report] Pipeline exited with code {code}. Aborting report publication.",
            file=sys.stderr,
        )
    return code


def _publish_qmd_report(target: str) -> int:
    source = REPORT_DIR / target
    if not source.exists():
        print(f"[generate_report] Missing report source: {source}", file=sys.stderr)
        return 1

    OUTPUT_REPORT_DIR.mkdir(parents=True, exist_ok=True)
    destination = OUTPUT_REPORT_DIR / source.name
    shutil.copy2(source, destination)
    print(f"[generate_report] Wrote {destination}", flush=True)

    for html_path in (OUTPUT_REPORT_DIR / "report.html", REPORT_DIR / "report.html"):
        html_path.unlink(missing_ok=True)
    shutil.rmtree(REPORT_DIR / ".quarto", ignore_errors=True)
    return 0


def main() -> int:
    args = build_parser().parse_args()

    if not args.skip_pipeline:
        pipeline_code = _run_pipeline(args.skip_plots, args.strict_validation)
        if pipeline_code != 0:
            return pipeline_code

    code = _publish_qmd_report("report.qmd")
    if code != 0:
        print(
            "[generate_report] Report publication failed.",
            file=sys.stderr,
        )
        return 1

    print(
        "[generate_report] Done. Open outputs/report/report.qmd.",
        flush=True,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

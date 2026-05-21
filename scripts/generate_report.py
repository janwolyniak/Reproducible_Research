"""Reviewer entry point: run the full pipeline, then render Quarto report+slides.

This script is the default Docker command. It is also runnable locally if
Quarto is installed on the host.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
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
            "Run the full Python reproduction pipeline and render the Quarto "
            "report and slides to outputs/report/."
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
        help="Pass --skip-plots to the pipeline (still re-renders the report).",
    )
    parser.add_argument(
        "--strict-validation",
        action="store_true",
        help="Pass --strict-validation to the pipeline.",
    )
    parser.add_argument(
        "--report-only",
        action="store_true",
        help="Render only report.qmd (skip slides.qmd).",
    )
    parser.add_argument(
        "--slides-only",
        action="store_true",
        help="Render only slides.qmd (skip report.qmd).",
    )
    return parser


def _quarto_executable() -> str | None:
    return shutil.which("quarto")


def _run_pipeline(skip_plots: bool, strict_validation: bool) -> int:
    options = RunAllOptions(
        skip_plots=skip_plots,
        strict_validation=strict_validation or not skip_plots,
    )
    print("[generate_report] Running full reproduction pipeline ...", flush=True)
    code = run_all(options)
    if code != 0:
        print(
            f"[generate_report] Pipeline exited with code {code}. Aborting render.",
            file=sys.stderr,
        )
    return code


def _render_quarto(target: str, quarto_bin: str) -> int:
    print(f"[generate_report] Rendering {target} via Quarto ...", flush=True)
    completed = subprocess.run(
        [quarto_bin, "render", target, "--to", "html"],
        cwd=REPORT_DIR,
        check=False,
    )
    if completed.returncode != 0:
        print(
            f"[generate_report] Quarto failed for {target} (exit {completed.returncode}).",
            file=sys.stderr,
        )
        return completed.returncode

    rendered = REPORT_DIR / Path(target).with_suffix(".html").name
    if not rendered.exists():
        print(
            f"[generate_report] Quarto reported success but {rendered} is missing.",
            file=sys.stderr,
        )
        return 1

    destination = OUTPUT_REPORT_DIR / rendered.name
    OUTPUT_REPORT_DIR.mkdir(parents=True, exist_ok=True)
    shutil.copy2(rendered, destination)
    print(f"[generate_report] Wrote {destination}", flush=True)
    return 0


def main() -> int:
    args = build_parser().parse_args()

    if not args.skip_pipeline:
        pipeline_code = _run_pipeline(args.skip_plots, args.strict_validation)
        if pipeline_code != 0:
            return pipeline_code

    quarto_bin = _quarto_executable()
    if quarto_bin is None:
        print(
            "[generate_report] Quarto CLI not found on PATH. Install Quarto or "
            "run inside the project's Docker image, which ships Quarto.",
            file=sys.stderr,
        )
        return 2

    OUTPUT_REPORT_DIR.mkdir(parents=True, exist_ok=True)

    targets: list[str] = []
    if args.slides_only:
        targets = ["slides.qmd", "slides_visual.qmd"]
    elif args.report_only:
        targets = ["report.qmd"]
    else:
        targets = ["report.qmd", "slides.qmd", "slides_visual.qmd"]

    failures = 0
    for target in targets:
        code = _render_quarto(target, quarto_bin)
        if code != 0:
            failures += 1

    if failures:
        print(
            f"[generate_report] Rendering completed with {failures} failure(s).",
            file=sys.stderr,
        )
        return 1

    print(
        "[generate_report] Done. Open outputs/report/report.html and "
        "outputs/report/slides.html.",
        flush=True,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

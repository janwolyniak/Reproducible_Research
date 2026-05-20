from __future__ import annotations

import subprocess
import sys
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path

from repro_research.paths import PROJECT_ROOT
from repro_research.validation import validate_project


@dataclass(frozen=True)
class StepResult:
    name: str
    returncode: int
    stdout: str
    stderr: str

    @property
    def ok(self) -> bool:
        return self.returncode == 0


def _run_python_script(script: Path, *args: str) -> StepResult:
    completed = subprocess.run(
        [sys.executable, str(script), *args],
        cwd=PROJECT_ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    return StepResult(
        name=str(script.relative_to(PROJECT_ROOT)),
        returncode=completed.returncode,
        stdout=completed.stdout,
        stderr=completed.stderr,
    )


def _format_step(result: StepResult) -> str:
    status = "ok" if result.ok else f"failed ({result.returncode})"
    sections = [f"## {result.name}: {status}"]
    if result.stdout.strip():
        sections.extend(["", "stdout:", result.stdout.strip()])
    if result.stderr.strip():
        sections.extend(["", "stderr:", result.stderr.strip()])
    return "\n".join(sections)


def write_run_summary(step_results: list[StepResult]) -> Path:
    log_path = PROJECT_ROOT / "outputs" / "logs" / "run_all_summary.txt"
    timestamp = datetime.now(UTC).isoformat(timespec="seconds")
    lines = [
        "Reproducible Research pipeline run",
        f"timestamp_utc: {timestamp}",
        f"root: {PROJECT_ROOT}",
        "",
    ]
    lines.extend(_format_step(result) for result in step_results)
    log_path.write_text("\n\n".join(lines) + "\n", encoding="utf-8")
    return log_path


def run_all() -> int:
    validation = validate_project(create_output_dirs=True)
    if not validation.ok:
        print(validation.render())
        return 1

    steps = [
        _run_python_script(PROJECT_ROOT / "scripts" / "audit_phase0.py"),
        _run_python_script(PROJECT_ROOT / "scripts" / "run_panel_prep.py"),
        _run_python_script(PROJECT_ROOT / "scripts" / "run_cross_section_data.py"),
        _run_python_script(PROJECT_ROOT / "scripts" / "run_cross_section.py"),
        _run_python_script(PROJECT_ROOT / "scripts" / "run_panel.py"),
        _run_python_script(PROJECT_ROOT / "scripts" / "inventory_data.py"),
    ]
    log_path = write_run_summary(steps)
    output_validation = validate_project(generated_outputs=True)

    for step in steps:
        if step.stdout:
            print(step.stdout, end="")
        if step.stderr:
            print(step.stderr, file=sys.stderr, end="")
    print(f"Wrote {log_path.relative_to(PROJECT_ROOT)}")
    if not output_validation.ok:
        print(output_validation.render(), file=sys.stderr)

    failed = next((s for s in steps if not s.ok), None)
    if failed is not None:
        return failed.returncode
    return 0 if output_validation.ok else 1

from __future__ import annotations

from repro_research.paths import GENERATED_OUTPUT_FILES
from repro_research.validation import ensure_output_dirs, validate_project


def test_generated_output_validation_reports_missing_files(tmp_path) -> None:
    ensure_output_dirs(tmp_path)

    result = validate_project(tmp_path, generated_outputs=True)

    assert result.missing_generated_outputs == GENERATED_OUTPUT_FILES


def test_generated_output_validation_accepts_generated_files(tmp_path) -> None:
    ensure_output_dirs(tmp_path)
    for relative_path in GENERATED_OUTPUT_FILES:
        path = tmp_path / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("generated\n", encoding="utf-8")

    result = validate_project(tmp_path, generated_outputs=True)

    assert result.missing_generated_outputs == ()

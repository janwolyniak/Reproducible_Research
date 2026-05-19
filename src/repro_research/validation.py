from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from repro_research.paths import OUTPUT_DIRS, PROJECT_ROOT, REQUIRED_PATHS


@dataclass(frozen=True)
class ValidationResult:
    missing_paths: tuple[str, ...]
    missing_output_dirs: tuple[str, ...]

    @property
    def ok(self) -> bool:
        return not self.missing_paths and not self.missing_output_dirs

    def render(self) -> str:
        lines = ["Project validation"]
        lines.append(f"  root: {PROJECT_ROOT}")
        lines.append(f"  required paths missing: {len(self.missing_paths)}")
        lines.append(f"  output directories missing: {len(self.missing_output_dirs)}")

        if self.missing_paths:
            lines.append("")
            lines.append("Missing required inputs or reference artifacts:")
            lines.extend(f"  - {path}" for path in self.missing_paths)

        if self.missing_output_dirs:
            lines.append("")
            lines.append("Missing expected output directories:")
            lines.extend(f"  - {path}" for path in self.missing_output_dirs)

        return "\n".join(lines)


def ensure_output_dirs(root: Path = PROJECT_ROOT) -> None:
    for relative_path in OUTPUT_DIRS:
        (root / relative_path).mkdir(parents=True, exist_ok=True)


def validate_project(
    root: Path = PROJECT_ROOT,
    *,
    create_output_dirs: bool = False,
) -> ValidationResult:
    if create_output_dirs:
        ensure_output_dirs(root)

    missing_paths = tuple(path for path in REQUIRED_PATHS if not (root / path).exists())
    missing_output_dirs = tuple(
        path for path in OUTPUT_DIRS if not (root / path).is_dir()
    )
    return ValidationResult(
        missing_paths=missing_paths,
        missing_output_dirs=missing_output_dirs,
    )

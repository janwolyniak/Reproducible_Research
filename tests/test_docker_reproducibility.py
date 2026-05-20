from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def test_dockerfile_runs_full_pipeline_by_default() -> None:
    dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")

    assert "FROM python:3.11-slim" in dockerfile
    assert "MPLBACKEND=Agg" in dockerfile
    assert "python -m pip install -r requirements.txt" in dockerfile
    assert 'CMD ["python", "scripts/run_all.py"]' in dockerfile


def test_compose_service_uses_phase6_image_and_pipeline_command() -> None:
    compose = (ROOT / "docker-compose.yaml").read_text(encoding="utf-8")

    assert "reproduction:" in compose
    assert "janwolyniak/reproducible-research-lic-fii:phase6" in compose
    assert "command: python scripts/run_all.py" in compose
    assert "- .:/app" in compose


def test_dockerignore_excludes_local_and_generated_scratch_state() -> None:
    ignored = set((ROOT / ".dockerignore").read_text(encoding="utf-8").splitlines())

    expected = {
        ".git",
        ".venv",
        "__pycache__",
        ".pytest_cache",
        ".ruff_cache",
        "outputs/logs/*",
        "outputs/intermediate/*",
    }
    assert expected <= ignored

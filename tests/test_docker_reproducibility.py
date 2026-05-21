from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def test_dockerfile_renders_report_by_default() -> None:
    dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")

    assert "FROM python:3.11-slim" in dockerfile
    assert "MPLBACKEND=Agg" in dockerfile
    assert "python -m pip install -r requirements.txt" in dockerfile
    assert "quarto" in dockerfile.lower()
    assert 'CMD ["python", "scripts/generate_report.py"]' in dockerfile


def test_compose_service_uses_phase6_image_and_report_command() -> None:
    compose = (ROOT / "docker-compose.yaml").read_text(encoding="utf-8")

    assert "reproduction:" in compose
    assert "janwolyniak/reproducible-research-lic-fii:phase6" in compose
    assert "command: python scripts/generate_report.py" in compose
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
        "outputs/report/*",
    }
    assert expected <= ignored


def test_report_assets_exist() -> None:
    report_dir = ROOT / "report"
    assert (report_dir / "_quarto.yml").exists()
    assert (report_dir / "report.qmd").exists()
    assert (report_dir / "slides.qmd").exists()
    assert (report_dir / "slides_visual.qmd").exists()

    generator = ROOT / "scripts" / "generate_report.py"
    assert generator.exists()
    text = generator.read_text(encoding="utf-8")
    assert "quarto" in text.lower()
    assert "report.qmd" in text
    assert "slides.qmd" in text
    assert "slides_visual.qmd" in text


def test_demo_script_exists() -> None:
    demo = ROOT / "scripts" / "demo.py"
    assert demo.exists()
    text = demo.read_text(encoding="utf-8")
    assert "fixed_effects_main" in text
    assert "ols_main" in text

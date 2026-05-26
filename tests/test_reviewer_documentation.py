from __future__ import annotations

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def _read(relative_path: str) -> str:
    return (ROOT / relative_path).read_text(encoding="utf-8")


def test_readme_covers_outline_reviewer_requirements() -> None:
    readme = _read("README.md")

    expected_sections = [
        "## Project Objective",
        "## Repository Structure",
        "## Quickstart",
        "## Docker Reproduction",
        "## Output Descriptions",
        "## Contributor Roles",
    ]
    for section in expected_sections:
        assert section in readme

    expected_references = [
        "2400-LIC-FII.pdf",
        "cross-section/inorder.R",
        "panel/inorder_panel.R",
        "docs/methodology.md",
        "docs/limitations.md",
        "docs/ai_disclosure.md",
        "docs/contributions.md",
        "python scripts/run_all.py",
        "docker run --rm janwolyniak/reproducible-research-lic-fii",
    ]
    for reference in expected_references:
        assert reference in readme


def test_phase8_static_documents_are_present_and_linked() -> None:
    required_docs = [
        "docs/ai_disclosure.md",
        "docs/methodology.md",
        "docs/limitations.md",
        "docs/source_annotations.md",
    ]

    for relative_path in required_docs:
        text = _read(relative_path)
        assert "TODO" not in text
        assert len(text.splitlines()) > 5

    ai_disclosure = _read("docs/ai_disclosure.md")
    assert "OpenAI Codex" in ai_disclosure
    assert "human" in ai_disclosure.lower()

    source_annotations = _read("docs/source_annotations.md")
    assert "No external code snippets" in source_annotations
    assert "statsmodels" in source_annotations
    assert "linearmodels" in source_annotations


def test_contribution_document_records_all_project_authors() -> None:
    contributions = _read("docs/contributions.md")

    expected_authors = [
        "Jan Wolyniak",
        "Kinga Kucharska",
        "Iwo Wiszejko",
    ]
    for author in expected_authors:
        assert author in contributions

    assert "git log --format" in contributions
    assert "Add cross-sectional data preparation" in contributions
    assert "Add Phase 4 panel reproduction" in contributions
    assert "Implement Docker support" in contributions

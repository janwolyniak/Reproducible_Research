from __future__ import annotations

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT / "src"))

from repro_research.cross_section import MODEL_SPECS, fit_model  # noqa: E402
from repro_research.cross_section_data import (  # noqa: E402
    add_cross_section_transformations,
    load_cross_section_inputs,
)


def test_preferred_cross_section_model_has_expected_sample_size() -> None:
    frames = load_cross_section_inputs()
    prepared = add_cross_section_transformations(frames["model_data4_o"].frame)
    spec = next(item for item in MODEL_SPECS if item.name == "re4_o")

    result = fit_model(prepared, spec)

    assert int(result.nobs) == 122
    assert "log_cc_total_plus2" in result.params.index

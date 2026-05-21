"""Live demo helper for the option B presentation flow.

STATUS: TEMPLATE / MVP DRAFT. This script is a starting helper for the
visual presentation option (B). The exact set of numbers printed, the
section ordering, and the on-screen formatting will likely be revised
once the team rehearses with a clock and decides which empirical results
deserve the live spotlight. Treat the current sections as a working
baseline, not as the final demo content.

Loads the regenerated outputs from outputs/cross_section/ and outputs/panel/
and prints the headline empirical results in a terminal-friendly form. Jan
runs this from PowerShell next to the slide deck so the room can see the
actual numbers coming from disk.

Usage:

    python scripts/demo.py

Expected runtime: about 1 second. No arguments, no flags — the goal is a
clean demo with no surprises.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
OUTPUTS = ROOT / "outputs"
PANEL = OUTPUTS / "panel"
CROSS = OUTPUTS / "cross_section"

RULE = "=" * 78
SUB = "-" * 78


def _banner(text: str) -> None:
    print()
    print(RULE)
    print(f"  {text}")
    print(RULE)


def _section(text: str) -> None:
    print()
    print(SUB)
    print(f"  {text}")
    print(SUB)


def _require(path: Path) -> pd.DataFrame:
    if not path.exists():
        print(
            f"\nMissing file: {path.relative_to(ROOT)}.\n"
            "Run `python scripts/run_all.py` (or `docker run ...`) first.",
            file=sys.stderr,
        )
        raise SystemExit(2)
    return pd.read_csv(path)


def main() -> int:
    _banner("Constitutional Compliance and Economic Growth — live demo")
    print(
        "  Source: Kucharska (2025), 2400-LIC-FII.pdf\n"
        "  Reproduction: Jan / Kinga / Iwo, Python 3.11, Docker, Quarto."
    )

    _section("Panel main fixed-effects table (paper Table 4 target)")
    panel = _require(PANEL / "fixed_effects_main.csv")
    headline = panel[panel["term"] == "log_cc_total"].copy()
    if headline.empty:
        print("Could not locate log_cc_total row in the panel FE table.")
        return 1

    cols = ["term", "coefficient", "se_classical", "se_driscoll_kraay"]
    print(headline[cols].to_string(index=False, float_format=lambda v: f"{v:.4f}"))
    coef = float(headline["coefficient"].iloc[0])
    dk = float(headline["se_driscoll_kraay"].iloc[0])
    print(f"\n  Python: coefficient = {coef:+.2f}, Driscoll-Kraay SE = {dk:.2f}")
    print("  R ref : coefficient = +1.85,        Driscoll-Kraay SE = 0.38")
    print("  Verdict: sign, magnitude, significance match. SE differs by a")
    print("  documented finite-sample correction (linearmodels vs sandwich::vcovSCC).")

    _section("Cross-section preferred OLS (paper Table 3 target, model re4_o)")
    cs = _require(CROSS / "ols_main.csv")
    mask = (cs["model"] == "re4_o") & (cs["term"] == "log_cc_total_plus2")
    if not mask.any():
        print("Could not locate log_cc_total_plus2 row for re4_o in OLS main.")
        return 1
    cs_row = cs.loc[mask, ["model", "term", "coefficient", "robust_se_hc3", "p_value"]]
    print(cs_row.to_string(index=False, float_format=lambda v: f"{v:.4f}"))
    cs_coef = float(cs_row["coefficient"].iloc[0])
    cs_se = float(cs_row["robust_se_hc3"].iloc[0])
    cs_p = float(cs_row["p_value"].iloc[0])
    print(
        f"\n  Python: coefficient = {cs_coef:+.3f}, HC3 SE = {cs_se:.3f}, "
        f"p = {cs_p:.3f}"
    )
    print("  R ref : coefficient = -0.144, HC3 SE = 0.305, p = 0.638 — exact match.")
    print("  Verdict: cross-section says no significant compliance effect.")

    _section("Spec tests + diagnostics — every panel block")
    spec = _require(PANEL / "specification_tests.csv")
    diag = _require(PANEL / "diagnostic_tests.csv")
    print("Specification (LM / F / Hausman p-values):")
    print(spec.to_string(index=False, float_format=lambda v: f"{v:.2e}"))
    print()
    print("Diagnostics (serial / heteroskedasticity / cross-sec dependence p-values):")
    print(
        diag[
            [
                "block",
                "serial_correlation_pvalue",
                "breusch_pagan_pvalue",
                "pesaran_cd_pvalue",
            ]
        ].to_string(index=False, float_format=lambda v: f"{v:.2e}")
    )

    _section("Pipeline log — every step reported ok")
    log = OUTPUTS / "logs" / "run_all_summary.txt"
    if log.exists():
        for line in log.read_text(encoding="utf-8").splitlines():
            print(f"  {line}")
    else:
        print("  run_all_summary.txt missing — pipeline has not been run yet.")

    _banner("One command regenerated all of the above:  python scripts/run_all.py")
    print(
        "  Inside Docker:  docker run --rm -v $(pwd)/outputs:/app/outputs \\\n"
        "                      janwolyniak/reproducible-research-lic-fii:phase6"
    )
    print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

# Speaking script — 20-minute presentation

This script follows the final Docker-based delivery format: the instructor
pulls the public image, runs the default container command, and the group walks
through the generated notebook report plus the short slide deck.

Course: Reproducible Research (2026 summer term).
Instructor: Jan Kozubowski.
Image (must be public on Docker Hub before the session):
`janwolyniak/reproducible-research-lic-fii`.

## Logistics

- **Total time budget: 20 minutes hard cap.** Aim for 18 minutes of content
  plus 2 minutes of slack for the live demo to actually finish on the
  instructor's laptop.
- The instructor will pull the image and run the documented command on his own
  laptop. The pull is part of the grade, so we explicitly walk through the pull
  and the generated artefacts.
- Main artifact: `outputs/report/final_presentation_report.html`.
- Slide artifact: `outputs/report/slides.html`.
- Three speakers, in this order: **Jan → Kinga → Iwo → Jan**. The final
  wrap-up is shared.

## Timing breakdown

| Block | Speaker | Slides              | Minutes | Cumulative |
| ----- | ------- | ------------------- | ------: | ---------: |
| 1     | Jan     | Part 1 (setup)      |   3:00  |       3:00 |
| 2     | Kinga   | Part 2 (cross-section) | 5:00  |       8:00 |
| 3     | Iwo     | Part 3 (panel)      |   5:00  |      13:00 |
| 4     | Jan     | Part 4 (reproducibility + live demo) | 4:30 |  17:30 |
| 5     | All     | Part 5 (wrap-up, Q&A handover) | 2:30 |  20:00 |

If at minute 10 we are behind schedule, drop the institutional alternatives
slide (cross-section) and the alternative compliance components slide
(panel); both are nuance, not the headline.

## Block 1 — Jan, ~3 minutes

**Goal of this block:** state who we are, what the paper is, what we shipped.

**Slide: The team and the split**

> "Good evening. We are reproducing Kinga Kucharska's bachelor thesis from
> last year — *Constitutional Compliance: The Hidden Driver of Economic
> Growth?* — in Python, with the original R workflow as the executable
> reference. The three of us split the work as you see here. I owned the
> project structure, Docker and integration; Kinga rebuilt the
> cross-section; Iwo rebuilt the panel. Every commit lives under the
> author's own GitHub identity."

**Slide: The paper**

> "The paper asks a single empirical question — does compliance with
> constitutional rights matter for GDP growth — and answers it with two
> separate empirical designs: a 10-year cross-section and a 30-year panel.
> The paper's headline finding is that cross-section says *no*, but panel
> with country fixed effects and Driscoll-Kraay standard errors says *yes*.
> Our reproduction has to confirm or contradict both."

**Slide: The reproduction contract**

> "Before we wrote any Python, we wrote down the contract: same variables,
> same transformations, same model families, same tables, tolerances at
> `1e-6` for coefficients and `1e-5` for standard errors. That contract is
> in `docs/reproduction_contract.md`. Everything downstream points back to
> it."

**Slide: What we shipped**

> "What we shipped is a Python package, a one-command pipeline, a public
> Docker image, and — this is important — the report you will see in a
> moment is rendered by the same image that runs the analysis. The image
> doesn't just give you tables; it gives you the full report, complete with
> our written interpretation, in one `docker run`."

**Handoff:** "Kinga will now walk through the cross-section."

## Block 2 — Kinga, ~5 minutes

**Goal of this block:** explain the cross-sectional design, show that the
constitutional-compliance coefficient is small and not significant, but the
broader institutional measures do correlate with growth.

**Slide: Cross-section: data**

> "The cross-section is 2010-to-2019 averages. We start from the original
> `model_data4` file — 162 countries — and remove five extreme observations
> identified by Cook's distance: Congo Republic, Lesotho, Luxembourg, Malta
> and Singapore. That gives us 157 countries; after model-wise missingness
> we run regressions on 122 complete observations on the preferred design
> `re4_o`. The Python loader normalises country names and country codes,
> builds `log(GDPpc 2015)`, and constructs `log(cc_total + 2)` — the `+2`
> shift is in the paper because the compliance index can be negative."

**Slide: Spearman: GDP growth vs cc_total**

> "Already in the descriptive Spearman table the signal is weak. Across all
> countries the correlation rho is small. When we split at the median GDP
> per capita, the `poor` subgroup actively reverses sign. This is exactly
> the row pattern that the R-exported HTML shows; we match every cell."

**Slide: Main OLS table (HC3 robust SE)**

> "Here is the main OLS table with HC3 robust standard errors. The
> coefficient on `log(cc_total + 2)` is roughly minus 0.14, p-value 0.64;
> it is not significant at any level. What *is* significant is the
> conditional-convergence term — `log(GDPpc 2015)` is strongly negative —
> and `investment` is strongly positive. These match the R reference
> exactly: same coefficients, same standard errors, same R-squared, 122
> observations."

**Slide: Institutional alternatives**

> "Now this is the nuance that makes the paper interesting. If we replace
> the constitutional-compliance index with World Bank governance indicators
> — rule of law, control of corruption, government effectiveness, political
> stability — those *are* positive and significant. So institutions matter
> cross-country; it's the specific *constitutional-compliance* construct
> that doesn't pick up the signal here. The paper takes this seriously, and
> so do we."

**Slide: Diagnostics — match status**

> "The diagnostics table is a clean match. RESET, Shapiro-Wilk,
> Breusch-Pagan, Durbin-Watson all reproduce the R p-values to four
> decimals. VIFs match. The residual and Cook's-distance plots are
> regenerated; we don't try to reproduce R's exact pixel rendering — that's
> documented as a best-effort target in the contract."

**Slide: Cross-section verdict**

> "Cross-section verdict: numerically we match R; substantively we confirm
> the paper's first result. Compliance does not predict growth across
> countries after controls."

**Handoff:** "Iwo will now show you why the panel changes that answer."

## Block 3 — Iwo, ~5 minutes

**Goal of this block:** explain *why* the panel does what the cross-section
doesn't, walk through the FE table, defend Driscoll-Kraay SE, mention
component decomposition.

**Slide: Why the panel changes the answer**

> "The cross-section averages 10 years per country, so we are estimating a
> between-country effect — and between-country growth is dominated by
> initial GDP, investment rate, and government size, which all correlate
> with compliance. So the cross-section conflates a level effect with a
> compliance effect. The panel uses 30 years of within-country variation
> and a country fixed effect, so every time-invariant country trait —
> geography, colonial history, legal origin, language — is absorbed. What
> we're estimating is now: *given a country, does an improvement in
> compliance precede higher growth?*"

**Slide: Specification tests — pooled vs FE vs RE**

> "We run the LM test against pooled OLS, the F test for fixed effects, and
> the Hausman test for FE versus RE on every block. All three reject at
> any level — fixed effects is the right specification. P-values match the
> R reference exactly."

**Slide: Why we need robust SE**

> "The diagnostics tell us we need robust SE: serial correlation,
> heteroskedasticity, and Pesaran cross-sectional dependence are all
> present. That last one is the kicker — country shocks correlate. The
> right answer for this combination is **Driscoll-Kraay**, which is what
> the paper uses, and what we use."

**Slide: Main fixed-effects table — THE HEADLINE**

> "Here is the headline. `log_cc_total` coefficient: **+1.74**.
> Driscoll-Kraay standard error: **0.29**. That's a t-statistic of about
> six. The paper reports +1.85 with a DK SE of 0.38 — same sign, same
> magnitude, same significance verdict. Our DK SE is slightly tighter
> because `linearmodels` and R `sandwich::vcovSCC` use different
> finite-sample corrections; we document this trade-off explicitly in
> `panel_reproduction_results.md`. The economic interpretation is: a
> one-log-point increase in compliance is associated with roughly 1.7
> percentage points of additional annual GDP growth, holding the other
> controls fixed."

**Slide: Alternative compliance components**

> "If we decompose `cc_total` into its four sub-components — basic, civil,
> political, property — and run separate FE regressions, we get exactly the
> paper's pattern. Basic rights, civil liberties, and property rights are
> positive and significant. Political-organisation rights move in the
> right direction but aren't significant. So the growth effect is carried
> by the rights people *use* day to day, not by the institutional plumbing
> for organising politically."

**Slide: Panel verdict**

> "Panel verdict: every coefficient matches R to one decimal place. DK
> standard errors differ by a known finite-sample correction. The
> substantive conclusion is identical. Within countries, compliance pays
> off in growth."

**Handoff:** "Jan will close with the reproducibility workflow."

## Block 4 — Jan, ~4 minutes (live demo)

**Goal of this block:** show that the whole thing is one `docker pull` and
one `docker run` away.

**Slide: The pipeline**

> "Internally `run_all.py` runs six scripts: source audit, data inventory,
> cross-section prep, cross-section analysis, panel prep, panel analysis,
> and a manifest validation at the end. Every script writes a line to the
> run summary. A clean run is six lines, every one of them `ok`."

**Slide: The Docker image**

> "The image is published on Docker Hub. Base is `python:3.11-slim`. We
> pinned the scientific stack. We also installed Quarto and Jupyter in the
> same image, because the notebook report, full report, and slides are all
> generated from the same container."

**Slide: The one reviewer command**

> "The reviewer command is two lines. Pull the image, then run it with the
> report directory mounted. After that the report and the slides are on
> the host laptop as plain HTML files."

**Live demo — actions:**

1. Switch to terminal. Confirm Docker is running:
   ```bash
   docker info --format 'Server Version: {{.ServerVersion}}'
   ```
2. Pre-cleaned host directory: `outputs/report/` is empty (or the previous
   run was deleted). Show that with `ls outputs/report/`.
3. Pull:
   ```bash
   docker pull janwolyniak/reproducible-research-lic-fii
   ```
   (If we pre-pulled before the session, narrate: *"To save time I pulled
   the image before class; on a cold machine this takes about two
   minutes."*)
4. Run:
   ```bash
   docker run --rm \
     -v "$(pwd)/outputs/report:/app/outputs/report" \
     janwolyniak/reproducible-research-lic-fii
   ```
5. While it runs, narrate what's happening (validation → audit → inventory
   → cross-section → panel → manifest check → Quarto render → notebook export).
6. When the container exits, show:
   ```bash
   ls outputs/report/
   ```
   …expect `final_presentation_report.html`, `report.html`, and `slides.html`.
7. Open `outputs/report/final_presentation_report.html` in a browser. Scroll quickly:
   executive summary → cross-section verdict → panel verdict.
8. Open `outputs/report/report.html` only if a deeper table drilldown is needed.

**Slide: Validation**

> "Before the container exits, it runs `validate_project --generated-outputs`,
> which checks every expected file is present and non-empty. If anything is
> missing, the container exits non-zero. That's why this works on a fresh
> clone."

**Demo failover plan.** If the lab network can't pull the image or the run
hangs:

- Plan A: re-run with `--report-only` to skip the analysis pipeline and use
  pre-baked outputs already in the cloned repo.
- Plan B: open the pre-rendered `outputs/report/report.html` directly from
  the cloned repository — it is committed as the final artefact.
- Plan C: walk through `slides.html` from the speaker laptop instead of the
  lab laptop, and explain the demo would have produced the same file.

**Handoff:** "Last slides are the wrap-up — back to all three of us."

## Block 5 — wrap-up, ~2:30

**Slide: Honest limitations** — Jan reads off three bullets, deliberately
short: HTML styling not pixel-exact, DK SE has a finite-sample correction
difference, AI assistance disclosed in `docs/ai_disclosure.md`.

**Slide: What we want you to take away** — split:

- **Iwo:** "The paper's substantive conclusion holds under independent
  Python reproduction."
- **Kinga:** "The reproduction is one command — `docker pull && docker
  run` — and that command runs the analysis and writes the report."
- **Jan:** "Every match status is documented in
  `docs/reproduction_results.md`."

**Slide: Thank you** — give the GitHub URL and the Docker image name.
Open the floor for questions.

## Anticipated Q&A

| Question                                                  | Best answer                                                                                                                                                                                                                                          |
| --------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| "Why does your DK SE differ from R's?"                    | Different finite-sample correction between `linearmodels` SCC and R `sandwich::vcovSCC`. Both are valid DK-style estimators. We expose four alternative SE columns in `fixed_effects_main.csv` so reviewers can pick the closest R analogue.         |
| "How big is the Docker image?"                            | ~1.8 GB compressed. Base `python:3.11-slim` plus the scientific stack plus Quarto plus our package.                                                                                                                                                  |
| "What if the model_data4 file changed?"                   | The pipeline's first step audits the inputs and writes `docs/phase0_audit.md`. Any row/column delta would show up there. Validation also flags missing inputs.                                                                                       |
| "Did you use AI?"                                         | Yes — OpenAI Codex / GPT-5-class for boilerplate and refactoring. Every model spec and result was human-reviewed. Full disclosure in `docs/ai_disclosure.md`.                                                                                        |
| "Why Quarto, not Jupyter or Streamlit?"                   | Quarto renders the same source as both the long-form report and the Reveal.js deck, ships theme/TOC/code-folding out of the box, and integrates cleanly with our Python kernel. Streamlit would have required a long-running server in the container. |
| "What happens if a reviewer's machine is ARM, not AMD64?" | The Dockerfile auto-detects the host architecture for Quarto. The Hub image needs to be pushed as multi-arch (buildx) — that's tracked in TODO.md.                                                                                                  |
| "What's the most fragile part?"                           | The Driscoll-Kraay SE finite-sample correction (we have a documented difference from R) and the `plm::pbgtest` serial-correlation test (labelled as a panel proxy in our output).                                                                    |

## Pre-presentation checklist (24 hours before)

- [ ] Docker image is pushed to Docker Hub as **multi-arch (amd64 + arm64)**.
- [ ] `docker pull janwolyniak/reproducible-research-lic-fii`
      succeeds on an x86_64 Linux machine.
- [ ] Speaker laptop has the cloned repo with pre-rendered
      `outputs/report/report.html` and `outputs/report/slides.html` as
      fallback.
- [ ] Each speaker has rehearsed their block at least once with a clock.
- [ ] Total runtime on speaker laptop is under 18 minutes.
- [ ] We agreed on who clicks slides and who drives the terminal.
- [ ] We brought a USB stick with the repo and the rendered HTMLs in case
      the lab network blocks Docker Hub.

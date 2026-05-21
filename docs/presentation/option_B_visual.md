# Option B — Visual deck (PPTX-style) + Jan's terminal demo

> **Status: template / MVP draft.** This document is one of two competing
> options for the presentation format (option A is in
> `option_A_hybrid.md`). It was prepared before the group decided which
> option to use, before final feedback from Kozubowski, and before any
> rehearsal. **All content — slide-by-slide wording, terminal sequence,
> demo failover plans, timings — will be revised** once the group picks an
> option, Jan finalises the terminal-side helper, and we run the timing
> through with a clock. Treat this as a starting structure to iterate
> from, not as the final script.

The visual Reveal.js deck (`outputs/report/slides_visual.html`) drives the
presentation. It is deliberately **light on code and embedded tables** — big
type, big numbers, short bullets — so the room can follow from any seat.
The technical depth lives in the terminal: at planned moments, Jan switches
from the slides to a PowerShell window and runs **`python scripts/demo.py`**,
which loads the regenerated outputs from disk and prints the headline
numbers in real time.

This is the option for a team that prioritises **visual clarity** and trusts
that the live terminal will satisfy the "step by step how the app works"
requirement that Kozubowski stated in the email.

## Setup before walking into the room

1. Lab laptop has the cloned repo with the pre-rendered
   `outputs/report/slides_visual.html`, plus `outputs/report/report.html`
   as long-form fallback.
2. Browser fullscreen on `outputs/report/slides_visual.html` (F11).
3. **PowerShell window** open in `C:\Users\1\Downloads\Reproducible_Research`,
   visible on the second monitor or pre-arranged to come forward with
   `Alt`+`Tab`.
4. Pre-tested `python scripts/demo.py` from that PowerShell window. The
   command should run in ~1 second and produce three formatted sections of
   output. Resize the PowerShell window so the room can read it
   comfortably.
5. Optional pre-pull of the Docker image:
   `docker pull janwolyniak/reproducible-research-lic-fii:phase6`. If the
   lab network can pull it live, even better.

## Timing

| Block | Speaker | Slides                                       | Minutes | Cumulative |
| ----- | ------- | -------------------------------------------- | ------: | ---------: |
| 1     | Jan     | Title + "Who, what, why" (4 slides)          |   3:00  |       3:00 |
| 2     | Kinga   | "The cross-section" (5 slides)               |   4:30  |       7:30 |
| 3     | Iwo     | "The panel" (5 slides)                       |   4:30  |      12:00 |
| 4     | Jan     | "Reproducibility" + terminal demo (3 slides) |   6:00  |      18:00 |
| 5     | All     | Wrap-up + thanks (3 slides)                  |   2:00  |      20:00 |

Block 4 is longer than in option A because Jan runs both `demo.py` and the
Docker pipeline live, with narration.

## Block 1 — Jan, 3 minutes

Slides: title → "The team" → "The paper we reproduced" → "Two empirical
designs, one question".

**Title slide.** Hold for 3-4 seconds so the room can read.

> "Tonight we ask a question that sounds philosophical but turns out to
> have a number attached: **does the rule of law make countries rich?** We
> are reproducing Kinga Kucharska's thesis from last year, in Python, and
> we packaged the whole thing in one Docker pull so that this presentation
> is itself produced by the image we are about to demo."

**The team slide.** Three columns, three contributors.

> "Jan — me — owned the project structure, Docker and reproducibility.
> Kinga, who wrote the original paper, rebuilt the cross-section in
> Python. Iwo rebuilt the panel and the robust-SE machinery."

**The paper slide.** Read the callout out loud.

> "The paper's question is the one in the callout: does compliance with
> constitutional rights — what is *enforced*, not what is *written* —
> predict GDP growth? The hypothesis is that it does."

**Two designs slide.** Trigger the fragments left then right.

> "Two designs answer that question. The cross-section averages 10 years
> per country across 157 countries. The panel uses 30 years of
> country-year variation across about 2,200 observations. The first asks a
> between-country question. The second asks a within-country question.
> They give us different answers, and that contrast is the most
> interesting result of the entire reproduction."

**Handoff.** "Kinga will walk us through the cross-section."

## Block 2 — Kinga, 4:30

Slides: "What we expected" → "What we found" → "But other institutions do
show up" → "Cross-section takeaway".

**What we expected slide.** Three fragments, build up the story.

> "Before we ran anything, the theoretical prediction was clean: better
> rule of law, better-protected property rights, more investment, more
> growth. That story drives the entire World Bank governance literature."

**What we found slide.** Pause on the big number before reading the small
print under it.

> "Not significant. The cross-section coefficient on log compliance is
> minus 0.14, HC3 standard error 0.31, p-value 0.64. Sample size 122,
> outlier-filtered, matches the R reference exactly."

**But other institutions do show up slide.** Walk through the table top
to bottom.

> "If we swap the compliance index for any World Bank governance indicator,
> the coefficient flips: rule of law, control of corruption, government
> effectiveness, political stability all come back positive and
> significant. So institutions matter cross-country — it's the
> *constitutional-compliance* construct specifically that doesn't pick the
> signal up here."

**Cross-section takeaway slide.** Read the four incremental points; pause
after #4 because it's the bridge to the panel.

**Handoff.** "Iwo will show you why the panel changes the answer."

## Block 3 — Iwo, 4:30

Slides: "Why we trust the panel more" → "Why we need robust standard
errors" → "The headline result" → "What kinds of rights matter most" →
"Panel takeaway".

**Why we trust the panel more slide.** Two fragments.

> "Cross-section asks: *across* countries, are more compliant ones growing
> faster? The problem is that compliance correlates with geography,
> colonial history, legal origin — all the things we cannot observe
> directly. Panel with country fixed effects absorbs every one of those
> traits automatically. What we then estimate is, *for a given country*,
> whether becoming more compliant precedes higher growth."

**Why we need robust SE slide.** Read all three diagnostics, then the
callout.

> "Three things are simultaneously present: serial correlation,
> heteroskedasticity, and Pesaran cross-sectional dependence — all at
> p-values below 1%. The estimator built for this exact combination is
> Driscoll-Kraay. That is what the paper uses, and what we use."

**The headline result slide.** Pause on the big number.

> "Within countries, a one-log-point increase in compliance is associated
> with **plus 1.74 percentage points** of annual GDP growth, Driscoll-Kraay
> SE 0.29 — a t-statistic above six. The R reference is plus 1.85 with DK
> SE 0.38. Sign, magnitude, significance: all the same. The SE differs by
> a documented finite-sample correction."

**What kinds of rights matter most slide.** Walk the rows.

> "If we decompose the index into its four sub-components, basic rights,
> civil liberties, and property rights all come back positive and
> significant. Political-organisation rights move the right way but are
> not significant. The effect is carried by the rights people *use*, not
> by the rights to *organise*."

**Panel takeaway slide.** Read the four incremental points.

**Handoff.** "Jan will close with the reproducibility piece."

## Block 4 — Jan, 6 minutes (terminal demo)

Slides: "One command" → "What runs inside the container" → "Live — what
you should see right now".

**One command slide.** Read the two-line shell block out loud.

> "The reviewer workflow is two lines. Pull the image from Docker Hub,
> then run the image with the output directory mounted on the host."

**What runs inside the container slide.** Walk the bullets.

> "Inside that one container run: source audit and data inventory,
> cross-section preparation and analysis with diagnostics and figures,
> panel preparation and all five estimator families, specification tests,
> diagnostics, Driscoll-Kraay covariance, and then the report and these
> very slides are rendered by Quarto from the same Python kernel. Total
> time: about three minutes on a modern laptop."

**Live — what you should see right now slide.** Build the bullets, then
**switch to the terminal**.

### Terminal demo — exact sequence

The room is now looking at PowerShell.

1. **First show `demo.py`** as a fast confirmation that the regenerated
   numbers are real. Type and run:

   ```powershell
   python scripts/demo.py
   ```

   Output appears in about 1 second. Walk through it section by section:

   - "Panel main fixed-effects table" — point at the +1.74 coefficient and
     the 0.29 Driscoll-Kraay SE.
   - "Cross-section preferred OLS" — point at the -0.14 coefficient and
     the matching R reference.
   - "Spec tests + diagnostics" — point at the p-values that all round to
     zero, motivating Driscoll-Kraay.
   - "Pipeline log" — point at the six `ok` lines.

   Estimated time: 90 seconds with narration.

2. **Then run the Docker pipeline** for real:

   ```powershell
   docker run --rm -v "${PWD}/outputs:/app/outputs" `
     janwolyniak/reproducible-research-lic-fii:phase6
   ```

   While the container runs, narrate the pipeline phases (audit → inventory
   → cross-section → panel → manifest check → Quarto render). Estimated
   wall time: 2-3 minutes.

3. **When the container exits**, list the produced files:

   ```powershell
   Get-ChildItem outputs\report\
   ```

   Point at `report.html` and `slides_visual.html`. Open
   `outputs/report/report.html` in a new browser tab and scroll quickly
   through the executive summary, then close it and return to slides.

4. **Switch back to the slide deck** and resume with the wrap-up.

### Demo failover plan

If the live `docker run` stalls or the lab network blocks the pull:

- **Plan A:** Skip the `docker run` step. Say *"on a working network the
  container would run for three minutes here — the report you would have
  seen is already in the cloned repository at `outputs/report/report.html`
  from our last test."* Then open that pre-rendered file.
- **Plan B:** Run `python scripts/demo.py` only. It is offline and proves
  the numerical results without Docker.
- **Plan C:** Skip the terminal entirely and finish the slides; tell the
  room *"the live workflow is in the README and the report; happy to run
  it after the questions if anyone is interested."*

## Block 5 — Wrap-up, 2 minutes

Slides: "What we want you to remember" → "Honest limitations" → "Thank you".

**What we want you to remember.** Split between speakers:

- **Iwo:** "Cross-section says no, panel says yes — and that's the right
  story."
- **Kinga:** "One command reproduces every number."
- **Jan:** "Every result was checked against the R reference."

**Honest limitations.** Jan reads four bullets quickly.

**Thank you slide.** Open the floor for questions.

## Anticipated Q&A (same as the technical deck)

The Q&A list is shared with `speaking_script.md` — same questions, same
answers. Refer there for the table.

## Practice checklist (24h before)

- [ ] PowerShell window is open and a fresh `python scripts/demo.py`
      runs successfully without errors.
- [ ] Browser fullscreen mode on `slides_visual.html` works smoothly.
- [ ] Each speaker has rehearsed their block at least once with a clock.
- [ ] Total wall time under 18 minutes including the live demo.
- [ ] Pre-rendered `outputs/report/report.html` is available as fallback
      in case the live `docker run` fails.
- [ ] Docker image is multi-arch on Docker Hub (this is Jan's pre-session
      task — see TODO.md and NEXT_SESSION.md).
- [ ] One USB stick with the cloned repo (network failover).

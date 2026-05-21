# Option A — Hybrid: slides + 3 report drilldowns

> **Status: template / MVP draft.** This document is one of two competing
> options for the presentation format (option B is in
> `option_B_visual.md`). It was prepared before the group decided which
> option to use, before final feedback from Kozubowski, and before any
> rehearsal. **All content — drilldown moments, exact wording, timings,
> setup steps — will be revised** once the group picks an option and
> rehearses it. Use this as a starting structure to iterate from.

The technical Reveal.js deck (`outputs/report/slides.html`) drives the
narrative. At three specific moments, the speaker switches to the full
report (`outputs/report/report.html`) to show a longer table, a code block,
or a piece of prose. After the drilldown, we return to the slides.

This is the option for a team that wants **slide-paced presentation flow**
with **report-grade evidence** available on demand. The full speaking
sequence comes from `speaking_script.md`; this file adds the drilldowns
and the version-A logistics.

## Setup before walking into the room

1. Lab laptop has the cloned repo and the pre-rendered
   `outputs/report/{report.html, slides.html}` next to it.
2. Browser open on **two windows side by side**:
   - Left: `outputs/report/slides.html` (presentation mode, F11).
   - Right: `outputs/report/report.html` (long report, ready to scroll).
3. Two `Win`+`←` / `Win`+`→` snaps so Jan can switch quickly.
4. Terminal opened in `C:\Users\1\Downloads\Reproducible_Research` for the
   Docker live demo.

## Timing (matches the existing speaking script)

| Block | Speaker | Slides              | Minutes | Cumulative |
| ----- | ------- | ------------------- | ------: | ---------: |
| 1     | Jan     | Part 1 (setup)      |   3:00  |       3:00 |
| 2     | Kinga   | Part 2 (cross-section) | 5:00  |       8:00 |
| 3     | Iwo     | Part 3 (panel)      |   5:00  |      13:00 |
| 4     | Jan     | Part 4 (reproducibility + live demo) | 4:30 |  17:30 |
| 5     | All     | Part 5 (wrap-up)    |   2:30  |      20:00 |

## The three drilldowns

### Drilldown 1 — Kinga, after slide "Main OLS table (HC3 robust SE)"

Trigger: she just finished saying *"…it is not significant at any level. What
*is* significant is the convergence term and investment."*

**Action.** Switch to report window. Scroll to section **3.3 Main OLS
table**. Show the full `re4_o` block (122 observations).

**Talk.** Say:

> "If you want to see all the coefficients next to each other — convergence,
> investment, terms of trade, government — the full table is in section 3.3
> of the report. Everything is HC3-robust. Sample size: 122. R-squared:
> 0.47. This is exactly the table from Appendix A2 of the paper, regenerated
> by Python."

**Time on the drilldown:** 25 seconds. Then back to slides.

### Drilldown 2 — Iwo, after slide "Main fixed-effects table — THE HEADLINE"

Trigger: he just said *"…+1.74, DK SE 0.29, t-statistic about six."*

**Action.** Switch to report window. Scroll to section **4.6 Main
fixed-effects table — the paper's headline**.

**Talk.** Say:

> "Section 4.6 of the report carries the full table with all five SE
> variants: classical, Arellano, time-cluster, double-cluster, and
> Driscoll-Kraay. They are within roughly 30% of each other; the
> Driscoll-Kraay column is what the paper publishes. We also document the
> finite-sample correction difference between Python's local SCC and R's
> `sandwich::vcovSCC`."

**Time on the drilldown:** 25 seconds.

### Drilldown 3 — Jan, after slide "The one reviewer command"

Trigger: Jan just said *"…the rendered report and these slides are on the
host laptop as plain HTML files."*

**Action.** Switch to report window. Scroll to section **6.2 Docker** and
read out the command block.

**Talk.** Say:

> "Section 6.2 of the report shows the exact command, the bind-mount, and
> the artefacts that land on the host. Same image, same command, identical
> output. Now let me actually run it."

Then immediately switch to terminal and run the live demo.

**Time on the drilldown:** 20 seconds.

## What stays the same vs the existing speaking_script.md

Everything in `speaking_script.md` still applies: per-block text for Jan,
Kinga, Iwo and Jan, the live-demo action list, anticipated Q&A, the
pre-presentation checklist. Treat `speaking_script.md` as the master text
and this document as the **+3 drilldowns** delta.

## When NOT to do the drilldowns

If by minute 10 we are behind schedule, **skip drilldowns 1 and 2**. The
slides alone communicate the result; drilldowns are evidence-grade depth,
not required for the headline. Drilldown 3 (Jan, Docker section) stays
because it sets up the live demo.

## Practice checklist (24h before)

- [ ] Verified: pressing `Win`+`←` and `Win`+`→` snaps two browser
      windows on the speaker laptop.
- [ ] Verified: each speaker can find their drilldown section in
      `report.html` in under 5 seconds.
- [ ] Verified: drilldown text fits in 25 seconds without rushing.
- [ ] Verified: the speaker laptop has the report and slides rendered
      under `outputs/report/` (fallback for the Docker live demo).

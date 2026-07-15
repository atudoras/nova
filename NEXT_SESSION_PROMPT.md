# NOVA — next session prompt

Paste everything below the line into a fresh Claude Code session opened at
`/Users/alextudoras/Claude-Cowork/nova`.

---

You are continuing work on **NOVA**, my R package for multi-electrode array (MEA)
analysis, and its `case_studies/` portfolio. Read this whole brief before acting.

## Where you are

- **Repo: `/Users/alextudoras/Claude-Cowork/nova`.** Open the session here. (The previous
  session was accidentally started in `PhD/IMAGE_ANALYSIS` and worked via absolute paths —
  don't repeat that.)
- **Always `devtools::load_all(".")`. Never `library(NOVA)`.** The copy installed in my R
  library is **0.1.1** — three minor versions stale, predating the entire trajectory layer.
  Anything using the installed package silently runs different code. (Fixing that by running
  `devtools::install()` is on the list below — ask me first.)
- Everything is committed on `main`. **Nothing is pushed.** Don't push without asking.

## State — verified, not assumed

- **NOVA 0.4.0**: 158 tests passing; `R CMD check` = 0 errors, 0 warnings, 1 NOTE
  ("unable to verify current time", environmental).
- **Case study 01 (EPA/Shafer DNT, Brown et al. 2016) is shipped**: `run_discovery.R` →
  `summary.qmd` → committed `summary.html` + 3-page `summary.pdf`. Two independent
  adversarial reviews passed on it; the second caught a fabricated claim which is now fixed.
- **Case study 02 (raw spikes → metrics) is scaffolded only** — `SOURCE.md` template written,
  no dataset chosen, no code.
- 0.4.0 fixed a whole bug class: well IDs repeat on every plate, so `Experiment` is what
  identifies a well. Five functions each re-derived that and each got it wrong differently.
  Read `NEWS.md` for the full list — it is accurate and worth 5 minutes.

## Standing decisions — already made, do not re-open

1. Fix the package before building on it. (Done.)
2. Findings rank on **raw effect size only** (`|median log2 FC|`, raw `net_displacement`) —
   no spread-standardisation. Plus one validity guard: exclude near-zero **divisors**.
3. Curated summaries require **n ≥ 4 wells**, and must state how many comparisons that drops.
4. `case_studies/` lives inside the nova repo, `.Rbuildignore`'d; `data/` and `outputs/`
   gitignored; rendered HTML/PDF **are** committed — they are the portfolio.
5. EPA study: normalise to **same-plate vehicle controls at the same DIV** (not to a baseline
   timepoint); analysis window **DIV7–12**; drop the structural-NA metrics from the PCA.
6. Track B will pin `meaRtools` from the **CRAN archive** (it is archived and not installed;
   whether it builds against R 4.4.2 is **unverified** — check before planning on it).
7. renv is scoped to `case_studies/`, initialised **bare**. It is a manifest with opt-in
   restore, not an enforced environment. Don't "fix" that without asking.

## Work outstanding, in priority order

### 1. "Fix nova" — I said this and never specified it. Ask me what I meant if unsure, but these are the real candidates:

- **ggplot2 deprecation warnings flood every run.** `size` in `element_line()`/`element_rect()`
  (deprecated 3.4.0 → `linewidth`), `size` aesthetic for lines, `geom_errorbarh()` (deprecated
  4.0.0 → `geom_errorbar(orientation=)`). ggplot2 4.0.2 is installed. The warnings literally
  say "likely used in the NOVA package. Please report the issue at
  https://github.com/atudoras/nova/issues". Concentrated in `R/plots.R`, `R/metric_plots.R`,
  `R/dynamics_summary.R`.
- **`plot_mea_metric()` emits `Ignoring unknown labels: colour : "Treatment"`** on every call.
- **`perform_mea_pca()` is an exported `stop()` stub** — a landmine in the public API. Either
  deprecate it properly (`.Deprecated()`) or remove it.
- **The trajectory API is undocumented outside the quickstart.** `README.md` and
  `docs/user-guide/` never mention `nova_trajectory_summary()` / `nova_extract_trajectories()`
  / `nova_describe()`. The quickstart is the only path to the package's headline feature.
- **`CRAN-SUBMISSION` is stale** — records 0.1.0 from 2025-10-06 against a 0.4.0 DESCRIPTION.
- **Installed NOVA is 0.1.1.** Offer to `devtools::install()` so `library(NOVA)` stops being a trap.

### 2. Track B — case study 02, raw spikes → NOVA metrics

Bounded milestone, per the original plan:

- **3a.** Pick a small public spike-level dataset (meaRtools bundled example, or a small
  Zenodo/DANDI spike-list set). Record URL, licence, date, SHA-256 in `SOURCE.md` — verify
  the licence **on the page**, don't assume.
- **3b.** `02_raw_to_metrics/extract_metrics.R`: MFR, active electrodes by rate threshold,
  ISI-threshold bursts, network bursts, STTC synchrony from spike times. Reuse `meaRtools`
  where it already implements these and **cite it**. Wrap as an experimental
  `nova_metrics_from_spikes()`, kept **outside the package build** until validated against
  values the source reports independently.
- **3c.** Feed the result through `_shared/discovery_run.R` (it is dataset-agnostic; pass
  `divisor_column` if you normalise to something other than a baseline timepoint).
- **3d.** `summary.qmd` from `_shared/summary_template.qmd` → HTML + typst PDF. Then a
  fresh-context review subagent.
- Add to `ROADMAP.md`: "candidate feature — spike→metrics ingestion layer".

### 3. Wrap

- Fill the `case_studies/README.md` index row for study 02.
- Decide with me: push to GitHub? Merge? Version bump for the Track-B work?
- `renv::snapshot(project = "case_studies")` if deps changed.

### 4. Known cosmetic, low priority

- Page 2 of `01_epa_dnt_ontogeny/summary.pdf` is ~40% blank (Caveats start on page 3).
- The legend shows the data file's misspelling "Bisindolymaleimide 1" while prose uses the
  correct "bisindolylmaleimide-1". Not our error, but they disagree with each other.

## Traps — every one of these cost real time. Do not rediscover them.

- **Never `git checkout -- <path>` on uncommitted work.** I used it as mutation-test cleanup
  and destroyed every source fix in the session. **Commit first, then mutation-test in a
  disposable `cp -R` copy** — never in the working tree.
- **A test that passes on broken code is worthless.** After writing a regression test, revert
  the fix in a disposable copy and confirm the test fails. Two tests shipped this way before
  being caught; one tested a guard instead of the thing it named.
- **Render figures and LOOK at them.** A shipped caption named four compounds while the figure
  plotted six — including one unnamed line that was the highest in the panel. `pdftools::pdf_convert()`
  then read the PNG.
- **Verify every factual claim against a primary source, and tag it verified vs generated.**
  The worst defect of the last session was a *fabricated* compound classification —
  `ACTIVES <- c("Mevastatin", "Sodium Orthovanadate")` — hardcoded with no citation and
  contradicted by the paper's abstract (it is five actives, one negative). It survived into a
  headline I was asked to approve. Correcting it made the result *stronger*, which is the tell:
  there was never a reason to invent it.
- **Compute claims, don't assert them.** "Every one of the 25 is a decrease" was a static
  string in a paragraph where every other number was `sprintf`'d. It was 24 of 25.
- **Count wells with `nova_unit_id()` / `nova_unit_cols()`** (exported in 0.4.0).
  `n_distinct(Well)` pools well A1 across every plate into one replicate — in the EPA data
  that undercounts 28 of 32 conditions, 65 wells where there are 142. Undercounting replicates
  is the failure mode that makes a result look *better* than it is.
- **Quarto**: not on `PATH`. Use RStudio's bundled binary:
  `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto` (1.6.42, typst included,
  no LaTeX needed). **typst requires `fig-format: png`** — its SVG default needs cairo/X11,
  which is not installed. Do not `brew install` anything.
- **`log2` cannot represent zero.** Fully-silenced wells — the largest possible effects — cannot
  enter a log-ratio ranking. If you filter `> 0`, say so and say what it removed.

## How I work

- **Plan first, then a sign-off gate at each meaningful step.** Stop and show me before moving on.
  Nothing is "done" until its definition-of-done and a verification pass are met. For anything
  non-trivial, run a **fresh-context review subagent** over the diff/claims — and verify its
  findings yourself rather than agreeing or dismissing.
- **Bind to the real API.** Read the source; don't assume signatures.
- **Be direct.** No preamble. If I'm going off-track, say so. Push back on wired-wrong
  reasoning rather than agreeing. Resist the flattering reading of a result.
- **Stats honesty is not optional.** Everything in the portfolio is exploratory; flag
  batch/plate structure, multiple comparisons, small n, and violated assumptions unasked.
  Prediction ≠ causation.
- Git: `git add -p`, never `git add .`. Propose a commit message at each milestone.
  Never commit data.

## Start here

Confirm the state above (`git log --oneline -12`, `devtools::test()`, `R CMD check`), then tell
me what you think "fix nova" should cover and what you'd do first. **Do not start coding until
I sign off on the plan.**

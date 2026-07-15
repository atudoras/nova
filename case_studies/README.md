# NOVA case studies

Applying NOVA's standard analyses to published, openly available MEA data.

Each study runs in two stages, deliberately separated so that curation is
auditable rather than a matter of taste:

1. **Discovery (uncurated).** `_shared/discovery_run.R` runs NOVA end-to-end over
   *every* metric and *every* group — baseline normalisation, PCA, heatmaps,
   per-metric plots, trajectory summaries — dumps the lot to `outputs/discovery/`,
   and emits `findings.csv` / `findings.json` ranking what moved. Nothing is
   selected and nothing is concluded. The dump is gitignored; rerun to regenerate.
2. **Summary (curated).** `_shared/summary_template.qmd` takes the top findings
   **by effect-size rank**, not by eye, and renders one HTML notebook plus a
   compact PDF brief (≤3 pages). These *are* the portfolio and are committed.

## Studies

| # | Study | Data | Finding | Output |
|---|-------|------|---------|--------|
| 01 | [EPA/Shafer DNT ontogeny](01_epa_dnt_ontogeny/) | US EPA, Brown et al. 2016 (public domain) | A generic NOVA pass, given no compound-specific tuning, ranks the training set's two designated actives into 20 of the top 25 changes and never surfaces the designated negative. | [notebook](01_epa_dnt_ontogeny/summary.html) · [brief](01_epa_dnt_ontogeny/summary.pdf) |
| 02 | [Raw spikes → NOVA metrics](02_raw_to_metrics/) | public spike-time dataset (TBD) | — | scaffolded |

**Study 01 in one line:** exploratory, and it recovers labels the training set already
assigned — that is a claim about prediction, not about neurotoxicity in humans.

To reproduce study 01: download the file recorded in
[`01_epa_dnt_ontogeny/SOURCE.md`](01_epa_dnt_ontogeny/SOURCE.md) to
`01_epa_dnt_ontogeny/data/`, then from the repo root

```r
devtools::load_all(".")
source("case_studies/01_epa_dnt_ontogeny/run_discovery.R")
```

```sh
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to html
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to typst
```

Each study directory holds `SOURCE.md` (URL, licence, download date, checksum),
a `run_discovery.R`, a `summary.qmd`, and the rendered `summary.html` /
`summary.pdf`.

## Reproducing

These live inside the NOVA package repo and load the package **from source**, not
from your R library — the installed copy can be an older version with different
behaviour, and a portfolio that quietly reports different numbers than the code it
showcases is worse than no portfolio.

```r
# from the repo root
devtools::load_all(".")
source("case_studies/_shared/discovery_run.R")

processed <- process_mea_flexible("path/to/data", grouping_variables = "Treatment",
                                  baseline_timepoint = "baseline")
res <- discovery_run(processed, outdir = "case_studies/01_epa_dnt_ontogeny/outputs/discovery")
head(res$findings)
```

Render a summary (Quarto ≥ 1.4; typst is bundled, no LaTeX needed):

```sh
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to html
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to typst
```

If `quarto` is not on your `PATH`, RStudio ships one:
`/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto`.

`case_studies/` is excluded from the package build (`.Rbuildignore`), so none of
this affects `R CMD check`. Downloaded data (`data/`) and the discovery dump
(`outputs/`) are gitignored.

## Reproducibility (renv), and its one sharp edge

`renv.lock` records the exact version of every package these studies used —
126 of them, against R 4.4.2. It was snapshotted from the library that actually
produced the results, so it documents what ran rather than what was intended to
run.

Be clear about what that does and does not buy you. **The day-to-day workflow does
not go through renv.** Discovery runs from the repo root and Quarto renders from a
study subdirectory; neither starts R in `case_studies/`, so neither activates the
project library — both use your system library. The lockfile is a manifest plus an
opt-in restore, not an enforced environment.

To reproduce strictly, in a fresh clone:

```r
setwd("case_studies")
renv::restore()   # installs the recorded versions into an isolated library
```

**The sharp edge:** the project library starts empty. If you start R *in*
`case_studies/` before running `renv::restore()`, renv activates that empty library
and nothing — not even dplyr — is available. Work from the repo root, or restore
first.

## How findings are ranked

By **raw effect size**, with no significance testing:

| Kind | Effect | Ranked by |
|------|--------|-----------|
| `move` | median log2 fold-change vs baseline, per condition × metric × timepoint | \|effect\| |
| `loading` | PC1/PC2 variable loading | \|loading\| |
| `trajectory_distance` | net displacement from baseline in PC space | effect |
| `trajectory_wandering` | 1 − directness (so a wandering path ranks high like everything else) | effect |
| `group_separation` | distance between condition centroids in PC space | effect |

Every `move` carries its well and plate counts, because a large effect on three
wells is not the same result as a large effect on thirty — the ranking does not
know the difference, so the reader has to.

Wells whose baseline is near zero are **excluded** from the ranking. Normalisation
is a ratio, so a near-zero divisor manufactures enormous fold-changes that describe
the divisor rather than the biology; left in, they would own the top of the list.
The count of exclusions is reported in the scan provenance.

## What these findings are not

Exploratory, throughout. The discovery pass scans hundreds of condition × metric ×
timepoint combinations and sorts by effect size; sorting a large family of noisy
estimates puts the luckiest at the top, so the head of `findings` is biased upward
and would shrink on replication. No multiple-comparison correction is applied
because nothing here is a hypothesis test. Plate and batch structure is not
modelled, and timepoints are repeated measures on the same wells. That a metric
separates conditions says it predicts the label, not that it drives the phenotype.

Each `findings.json` carries its own scan provenance — how many metrics, groups and
timepoints were scanned, and how many comparisons that implies — so a ranking is
never read without the denominator that produced it.

# NOVA 0.1.1 — CRAN submission comments

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes

* checking for hidden files and directories: NOTE
  The `.claude/` directory is a local development workspace and is excluded
  from the build tarball via `.Rbuildignore`.

* checking for future file timestamps: NOTE
  File timestamps reflect the local development machine clock. These are
  not present in the submitted tarball and will not affect CRAN users.

## Test environments

* macOS 14 (local), R 4.4.3 — 0 errors, 0 warnings, 2 notes (as above)
* R-hub: ubuntu-latest (R-release) — checked via rhub::check_for_cran()
* R-hub: windows-latest (R-release) — checked via rhub::check_for_cran()

## New submission

This is a new CRAN submission.

NOVA provides tools for analysing Multi-Electrode Array (MEA) neuronal
recordings: PCA-based trajectory analysis, heatmap visualisation, and
per-metric plotting. It operates on pre-processed electrode metric CSVs
and produces publication-ready ggplot2 and pheatmap figures.

## Downstream dependencies

None — this is a new package with no reverse dependencies.

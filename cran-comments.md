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

* macOS 14 (local), R 4.5.x — 0 errors, 0 warnings, 2 notes (as above)
* R-hub: linux R-devel (ubuntu-latest) — Status: OK
* R-hub: windows R-devel (windows-latest) — Status: OK
* R-hub: ubuntu-release (R-release) — Status: OK

### Note on macOS ARM R-devel failure

R-hub macOS ARM (aarch64, R-devel r89717) reports an installation ERROR:
`Error in if (custom.bin) { : argument is of length zero`

This is a known R-devel regression in R's own binary package infrastructure on
aarch64-apple-darwin23 at this revision, not caused by NOVA. The package compiles
and loads correctly (all `** testing if installed package can be loaded` steps pass);
the crash occurs in R-devel's internal `* creating tarball` step. Linux R-devel,
Windows R-devel, and Ubuntu R-release all pass cleanly.

## New submission

This is a new CRAN submission.

NOVA provides tools for analysing Multi-Electrode Array (MEA) neuronal
recordings: PCA-based trajectory analysis, heatmap visualisation, and
per-metric plotting. It operates on pre-processed electrode metric CSVs
and produces publication-ready ggplot2 and pheatmap figures.

## Downstream dependencies

None — this is a new package with no reverse dependencies.

# NOVA 0.5.0

## Published data, and a smaller public API

### New

* **`process_mea_table()` — ingest any tidy metrics table.** Until now NOVA could only read
  the Axion CSV export, whose metadata lives on fixed rows 121–124. Published datasets
  essentially never look like that: they arrive as a plain table, one row per well ×
  timepoint with metrics in columns, or already in long form. This maps such a table onto
  the same schema, so everything downstream works unchanged.

  Two things it does that a hand-rolled `pivot_longer()` will not:

  - **`experiment` accepts several columns.** A well is only identified once you know its
    experiment, and the experiment is not always one column — in the EPA's DNT dataset,
    plate serial numbers are reused across culture dates (4 serials, 6 experiments), so
    keying on the serial merges two cultures into one well.
  - **`normalize = "control"`** divides each well by the control wells on its own plate at
    the same timepoint — the toxicology convention, and the only workable option when the
    earliest timepoint cannot serve as a reference (in a developmental assay every well may
    be silent at the first timepoint, leaving the ratio undefined). `normalize = "baseline"`
    keeps the existing per-well fold-change. Both yield `NA` against a zero divisor, never
    `Inf`, and the control path returns `Control_Value` so the divisor can be inspected
    rather than trusted.

  Verified by reproducing a bespoke adapter for a real published dataset exactly — same
  values, same normalisation, same 15,840 rows.

### Breaking

* **The public API is 18 functions, down from 28.** Ten internal helpers were exported by
  accident and are now internal: `aggregate_data`, `apply_scaling_enhanced`,
  `clean_heatmap_matrix`, `create_annotations_enhanced`, `create_color_palette_enhanced`,
  `handle_missing_values`, `null_coalesce`, `print_detailed_summary`, `quality_filter`,
  `setup_color_scheme`. None was documented as part of the workflow and none is used outside
  the package; if you call one, it is still reachable as `NOVA:::name()`.
* **`perform_mea_pca()` is removed.** It was exported and fully documented while its entire
  body was `stop()` — a manual page for a function that could never run. Use
  `pca_analysis_enhanced()`, which is what its error message already said.

### Fixed

* **The package no longer emits ggplot2 deprecation warnings.** A full pipeline run produced
  five distinct warnings on every call — `size` in `element_line()`/`element_rect()` and on
  line geoms (`linewidth` since 3.4.0), `geom_errorbarh()` (`geom_errorbar(orientation = "y")`
  since 4.0.0), and a `labs()` entry for an aesthetic the plot never mapped, which made
  ggplot report "Ignoring unknown labels" on every `plot_mea_metric()` call. Warning noise
  that users learn to scroll past is how a real warning gets missed. Now zero, with a
  source-level test so it stays that way.
* **`nova_unit_cols()` warns when it falls back to `Well` alone** rather than narrowing
  identity in silence. Pass `warn = FALSE` for genuinely single-experiment data.

# NOVA 0.4.0

## Bug fixes: plate identity

Wells are named identically on every plate (`A1` exists everywhere), so `Experiment` is
what tells two of them apart. Several places did not use it, and the result was wrong
numbers rather than an error.

**Results will change.** Multi-plate analyses change the most — values normalised against
the wrong plate are corrected, and wells previously pooled across plates are now separate
replicates, which generally *widens* error bands that were too narrow. Single-plate
datasets are mostly unaffected, with one exception: `plot_pca_trajectories_general()`
previously collapsed every well on a plate into a single trajectory (see below).

This is a minor-version bump rather than a patch because a documented default changes
(`sample_id_components`), numeric results change, and `plot_pca_trajectories_general()`
changes what it returns.

* **`process_mea_flexible()` normalised wells to other plates' baselines.** The baseline
  lookup was keyed on `Well` + `Variable` + grouping variables but not `Experiment`, so
  every plate's baseline matched every plate's wells. The join fanned out — 5270 rows
  became 5766 on the bundled example — and emitted values normalised against the wrong
  plate alongside the correct ones. `Experiment` is now part of the baseline key, and
  both a non-unique baseline key and any row inflation now warn rather than passing
  silently downstream.

* **`nova_trajectory_summary()` could draw a flat line at zero.** `plot_data` folded
  `Well` into the `Sample` string and dropped it, so replicate auto-detection fell
  through to `Sample` — an ID that embeds the timepoint. Each "replicate" then spanned a
  single timepoint, making every distance-from-baseline exactly 0, and the
  distance-from-baseline figure rendered as a flat zero line with no warning. `Sample`
  is no longer a candidate replicate column, and a `unit_var` yielding one timepoint per
  unit now warns and falls back to the group-mean trajectory. The `metrics` table was
  never affected.

* **`unit_var` now accepts multiple columns**, and auto-detects as
  `c("Experiment", "Well")` when both are present. `"Well"` alone merges the same well
  from different plates into one replicate, which understates the spread: on the bundled
  example, `pbs` at `2h` moves from 0.550 ± 0.071 (n = 4) to 0.941 ± 0.323 (n = 5).
  Passing several columns previously raised a length-2 condition error that an internal
  `tryCatch` swallowed, so the bands vanished while the function reported "no replicate
  column found" and `params$unit_var` still listed the requested columns. Missing columns
  are now reported and dropped, and `params$unit_var` names the columns actually used.

* **`plot_pca_trajectories_general()` pooled wells that share an ID across plates.**
  `well_id` was derived by string-splitting `individual_var` on `_`, which assumed that
  column held a composite `Sample` ID. It now uses the real `Well` column, qualified by
  `Experiment`. On the bundled example this returns 23 well trajectories where it
  previously returned 16: the old count merged `A1` on one plate with `A1` on another.
  `n_wells` counts wells rather than plates. Single-plate results are unchanged, but note
  that a caller passing a data frame that already carried an `Experiment` column would
  previously have had *every* well on a plate collapsed into one trajectory.

* **New: `nova_unit_cols()` / `nova_unit_id()`** — the columns that identify one
  replicate well, and their collapsed label. Every bug in this release came from a
  function deciding for itself what identified a well, and each decided differently;
  these exist so callers can ask instead of re-deriving. `n_distinct(Well)` counts the
  same well ID from six plates as one replicate — on a six-plate dataset that
  understates replication roughly two-fold, and understated precision is the one error
  that makes a result look *better* than it is.

* **`pca_plots_enhanced()` failed with more than 20 groups.** The colour vector was
  truncated at the length of the built-in palette while the names vector kept every
  level, so the assignment died on a length mismatch — a compound × dose design reaches
  20 immediately. Colours now interpolate past the palette. Shapes cannot be
  interpolated, so beyond the 25 available they repeat and now warn, rather than
  truncating or indexing into `NA` and silently dropping points.

* **`create_mea_heatmaps_enhanced(split_by = "combination")` errored on normal data.**
  Rows were keyed on `Well` alone, so a well carrying different treatments on different
  plates produced duplicate row names and the call failed with
  `duplicate 'row.names' are not allowed`; where treatments happened to agree it instead
  pooled both plates' wells into one row without saying so. Rows are now keyed on the
  well's full identity.

* **`sample_id_columns` now does something.** It was documented as identifying individual
  samples but keyed no aggregation anywhere. It now keys the rows of the
  `split_by = "combination"` heatmap, and defaults to `c("Experiment", "Well")` rather
  than `c("Well")`. The main per-group heatmaps pool replicate wells by design and are
  unaffected.

* **`find_mea_metadata_row()` located only one of the three labels it searches.** Matching
  was exact, but Axion writes qualified labels — `Treatment/ID` and `Exclude/Include`, and
  `Well Averages` for the wells row — so only `Genotype` was ever found and the rest fell
  back to hardcoded row numbers. This was latent, since the constants match the current
  layout, but it meant the row search was inert: a genuinely shifted export would have
  been misread silently. Matching is now anchored at a word boundary, so `Well` matches
  `Well Averages` but not `Wellington`, and the fallback is unchanged. (The wells row is
  derived as `Treatment` − 1 rather than searched, so it was never affected in practice.)

* **`pca_analysis_enhanced()` silently merged samples across plates.**
  `sample_id_components` now defaults to
  `c("Experiment", "Well", "Timepoint", "Treatment", "Genotype")`, and components present
  in the data are carried through to `plot_data` instead of being dissolved into
  `Sample`. Observations sharing a `Sample` are still averaged, but that now warns.
  Sample ID strings gain an `Experiment` prefix.

* **`discover_mea_structure()` reported `30min` as a candidate baseline.** Detection used
  an unanchored pattern, and `"30min"` contains `"0min"`. It now uses the package's own
  baseline vocabulary plus a parsed time of zero, and returns candidates best-first.

* **`pca_plots_enhanced()` errored on `shape_variable = NULL`.** Opting out of an
  aesthetic hit `if (logical(0))`. `NULL` is now honoured as "do not map this aesthetic",
  including the single-genotype case the quickstart documents.

* **`Example/nova_quickstart.R` did not run.** It passed a `timepoints_order` argument
  that `process_mea_flexible()` does not accept, and called `create_mea_trajectories()`,
  which does not exist — the current function is `nova_trajectory_summary()`. It also
  read `pca_results$plots$scatter` and `$plots$elbow` (there is no `$plots` element; the
  elbow is `$elbow_plot`) and looked for `pheatmap` objects one level above where they
  live, so the scatter, scree, and heatmap panels were silently never written. It now
  runs end-to-end and picks the baseline by dynamical order.

# NOVA 0.3.0

## A simpler, honest trajectory layer

This release replaces the exploratory 0.2.0 "dynamics" module with a single,
robust summary that matches what MEA timecourse data can actually support.
With only a handful of timepoints and a few replicate wells, quantities like
velocity, "stable vs unstable" regimes, and Markov transition models were
over-fitting noise, so they have been removed in favour of plain, defensible
descriptors.

* **`nova_trajectory_summary()`** — describes how each condition moves away from
  baseline through state space: net displacement, total path length, directness
  (`net / path`), and the timepoint of peak displacement. Returns two figures —
  distance-from-baseline over time (mean ± SEM across replicate wells) and a
  PC-space trajectory map — plus the metrics table.
* **`nova_describe()`** — a cautious, rule-based plain-language summary (no
  AI/API) describing *what happened*, not an inferred mechanism.
* **`nova_order_timepoints()` / `nova_time_to_minutes()`** — robust timepoint
  parsing: baseline first, and `min` / `h` / `s` / `day` / `DIV` / compound
  (`1h30`) labels ordered by real elapsed time (fixes `1h15` sorting before `1h`).
* **`nova_theme()` / `nova_palette()`** — consistent, publication-ready styling.
* **No new dependencies.** `MASS` / `dtw` / `igraph` / `patchwork` are no longer
  used.

### Removed (from 0.2.0)

`nova_state_geometry()`, `nova_transition_matrix()`,
`nova_trajectory_similarity()`, `nova_dynamical_regime()`, `nova_landscape()`,
and `nova_dynamics()` are removed: they implied a rigor the typical dataset does
not have. The robust parts they shared (timepoint ordering, trajectory
extraction, describe) live on in the functions above.

### Unchanged

All original visualization functions (`process_mea_flexible`,
`pca_analysis_enhanced`, `plot_pca_trajectories_general`,
`create_mea_heatmaps_enhanced`, `plot_mea_metric`, …) are **fully backward
compatible** — no signature, return value, or export changed.

---

# NOVA 0.2.0

Introduced an exploratory `nova_dynamics` module (state geometry, transitions,
similarity, regime detection, landscapes). Superseded by 0.3.0, which pares it
down to `nova_trajectory_summary()` after the dynamical-systems metrics proved
too rich for typical MEA timecourse data.

---

# NOVA 0.1.6

## Example Dataset Update

* Replaced private example data with publicly shareable WT neuron data
  (MEA012, MEA013)
* Example dataset now uses WT neurons with neuronal agonist treatments:
  PBS, KCl, DHPG, Gabazine (MEA012) and PBS, AMPA, KA, DHPG, Gabazine (MEA013)
* Updated all example scripts (nova_quickstart.R, 01_compute.R, 02_plot.R,
  mea-analysis-example.Rmd) to reflect WT-only dataset structure
* GENOTYPE_COLUMN now defaults to NULL in quickstart for WT-only datasets
* Removed compiled HTML and figure outputs generated from private dataset

---

# NOVA 0.1.1

## CRAN Resubmission Fixes

* Removed all commented code from examples and function bodies
* Fixed DESCRIPTION to spell out Multi-Electrode Array (MEA) without quotes
* Added newlines at end of all R source files
* Fixed parse errors in documentation examples

## Initial CRAN Release

This is the first release of NOVA (Neuroactivity Omics Visualization and Analysis), a comprehensive toolkit for analyzing and visualizing Multi-Electrode Array (MEA) data.

### Main Features

* **Data Processing**: Flexible MEA data processing with automatic structure discovery
  - `discover_mea_structure()`: Automatically detects data structure
  - `process_mea_flexible()`: Processes MEA data with quality filtering options
  - `handle_missing_values()`: Multiple strategies for handling missing data

* **Principal Component Analysis**: 
  - `pca_analysis_enhanced()`: Comprehensive PCA with visualization
  - `analyze_pca_variable_importance_general()`: Variable importance analysis
  - `plot_pca_trajectories_general()`: Trajectory visualization across conditions

* **Heatmap Visualization**:
  - `create_mea_heatmaps_enhanced()`: Publication-ready heatmaps with multiple scaling options
  - Support for hierarchical clustering and custom annotations

* **Utility Functions**: Helper functions for data manipulation, scaling, and color schemes

### Documentation

* Comprehensive function documentation with examples
* Vignettes demonstrating typical workflows
* Support for various MEA data formats
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
# NOVA 0.2.0

## New: `nova_dynamics` — a dynamical-systems toolkit

NOVA now formalises neuronal networks as trajectories through latent state space.
The new module operates on PCA (or UMAP / any embedding) coordinates and never
re-runs or replaces PCA. All functions accept a `pca_analysis_enhanced()` result,
a bare data frame, or a `nova_trajectories` object, and add **no new hard
dependencies** (Dynamic Time Warping and Frechet distance are implemented in base
R; `MASS`, `dtw`, `igraph`, `patchwork` are optional `Suggests`).

* `nova_state_geometry()` — trajectory length, net displacement, straightness,
  tortuosity, velocity, acceleration, directional persistence; overlay,
  velocity-coloured, and displacement-from-baseline figures.
* `nova_transition_matrix()` — k-means network states, empirical Markov
  transition matrix, occupancy, recurrent/transient classification, transition
  heatmap, and state-flow diagram.
* `nova_trajectory_similarity()` — Dynamic Time Warping, Frechet, Euclidean, and
  cosine trajectory distances with hierarchical clustering and a dendrogram.
* `nova_dynamical_regime()` — rule-based classification into stable / convergent
  / divergent / oscillatory / transitional, with a confidence and a fully
  auditable, tunable threshold block.
* `nova_landscape()` — state-occupancy density and pseudo-potential
  (`U = -log p`) landscapes with optional trajectory overlay.
* `nova_describe()` — rule-based natural-language interpretation of any dynamics
  result (no LLM/API).
* `nova_dynamics()` — one-call wrapper running the full pipeline.

## New: robust timepoint handling (correctness fix)

* `nova_order_timepoints()` and `nova_time_to_minutes()` parse heterogeneous
  labels (`30s`, `45min`, `1h`, `1h30min`, `1h15`, `DIV7`, bare numerics),
  always place baseline first, and order by **real elapsed time**. This fixes
  alphabetical mis-ordering of compound labels (e.g. `1h15` before `1h`) that
  affected timepoint sequences in trajectory plots. Existing functions are
  unchanged; the helper is available for explicit `timepoint_order =`.

## Other

* New vignette `NOVA_Dynamics_Tutorial` (self-contained: uses the bundled MEA
  data when present, otherwise a designed synthetic dataset).
* New `nova_theme()` / `nova_palette()` extend the existing visual language for
  consistent, publication-ready dynamics figures.
* Added `ROADMAP.md` specifying future modules (attractors, resilience,
  criticality, learning, closed-loop) — interfaces only, not implemented.
* Full backward compatibility: no existing function signature, return value, or
  export was modified.

---

# NOVA 0.1.6

## Example Dataset Update

* Replaced private example data (MEA022b/MEA022c, Mavs KO genotypes) with
  publicly available WT neuron data (MEA012, MEA013)
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
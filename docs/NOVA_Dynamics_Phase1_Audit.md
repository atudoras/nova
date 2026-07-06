# NOVA — Phase 1 Repository Audit

**Audited version:** NOVA 0.1.5 (`DESCRIPTION`), `NEWS.md` references an in-progress 0.1.6 example-data refresh.
**Audit date:** 2026-06-22
**Purpose:** Establish the existing architecture, data contracts, and visual conventions that the `nova_dynamics` submodule must integrate with *without breaking backward compatibility*.

---

## 1. Existing Structure

### 1.1 Package architecture

NOVA is a standard, CRAN-oriented R package (roxygen2 7.3.3, GPL-3, `Depends: R >= 4.1.0`). Source is organised by analysis stage rather than by object, which is the convention the dynamics module follows:

| File | Lines | Responsibility |
|------|-------|----------------|
| `R/data_handling.R` | 708 | Discovery + ingestion of Axion MEA CSV exports; baseline normalisation. `discover_mea_structure()`, `process_mea_flexible()`. |
| `R/pca_analysis.R` | 582 | PCA on the processed feature matrix. `pca_analysis_enhanced()` (the workhorse), `perform_mea_pca()` (a deprecation stub). |
| `R/plots.R` | 2783 | All major visual outputs: `pca_plots_enhanced()`, **`plot_pca_trajectories_general()`**, `create_mea_heatmaps_enhanced()`, `analyze_pca_variable_importance_general()`. |
| `R/metric_plots.R` | 193 | Single-metric bar/box/violin/line plots (`plot_mea_metric()`). |
| `R/utilities.R` | 439 | Missing-value handling, quality filtering, scaling, colour scheme setup. |
| `R/globals.R`, `R/imports.R`, `R/zzz.R` | 50 | `utils::globalVariables`, namespace imports, `.onLoad`. |

Pipeline shape:

```
process_mea_flexible()  ->  pca_analysis_enhanced()  ->  plot_pca_trajectories_general()
        (ingest/normalise)        (embed)                    (visualise dynamics, informally)
```

### 1.2 Core exported functions

`discover_mea_structure`, `process_mea_flexible`, `handle_missing_values`, `quality_filter`,
`pca_analysis_enhanced`, `perform_mea_pca`, `analyze_pca_variable_importance_general`,
`pca_plots_enhanced`, `plot_pca_trajectories_general`,
`create_mea_heatmaps_enhanced`, `clean_heatmap_matrix`, `plot_mea_metric`,
plus helpers (`aggregate_data`, `apply_scaling_enhanced`, `create_annotations_enhanced`,
`create_color_palette_enhanced`, `null_coalesce`, `print_detailed_summary`, `setup_color_scheme`).

### 1.3 Dependencies

- **Imports (hard):** tidyverse subset (`dplyr`, `tidyr`, `purrr`, `tibble`, `stringr`, `rlang`), `ggplot2`, `ggrepel`, `pheatmap`, `gridExtra`, `viridis`, `RColorBrewer`, `scales`, `readr`, `readxl`, `writexl`, `DT`, `knitr`.
- **Suggests:** `testthat (>= 3.0.0)`, `rmarkdown`, `utils`.
- **Design implication:** NOVA keeps a deliberately tidyverse + ggplot2 footprint and has not yet pulled in graph/time-series machinery. To preserve this, `nova_dynamics` adds **no new hard dependencies**: DTW and Fréchet distance are implemented in base R; `igraph`, `patchwork`, `dtw`, `MASS` are referenced only as optional `Suggests` with graceful fallbacks.

### 1.4 Data structures (the contracts the module binds to)

**A. `process_mea_flexible()` returns a list** with (among others):
`raw_data`, `normalized_data` (long format, fold-change to baseline in `Normalized_Value`), `processed_data`, `processing_params` (incl. `grouping_variables`), `processing_timestamp`.

Long-format columns include: `Well`, `Treatment`, `Genotype`, `Timepoint`, `Variable`, `Value` / `Normalized_Value`, `Experiment`.

**B. `pca_analysis_enhanced()` returns a list** whose central element is **`plot_data`** — the canonical state-space table the dynamics module consumes:

| Column | Meaning |
|--------|---------|
| `PC1`, `PC2`, … | State-space coordinates (the latent state `state(t)`). |
| `Sample` | Unique sample id (`Well_Timepoint_Treatment_Genotype`). |
| `Experiment` | MEA plate id (e.g. `MEA012`). |
| `Treatment`, `Genotype`, `Well` | Grouping / replicate identifiers. |
| `Timepoint` | Time label (`baseline`, `0min`, `15min`, `30min`, `1h`, `1h30min`, `2h`, …). |

Verified on the bundled `pca_output`: 329 rows × 8 cols; PC1 = 39.6 % variance, PC2 = 25.8 %.

This is the single most important contract: **`nova_dynamics` reads `pca_output$plot_data` (or a bare data frame with the same columns) and never re-runs PCA.** That satisfies the "operate on PCA coordinates / UMAP / future latent spaces" requirement — any embedding that produces `(dim1, dim2, time, group)` is consumable.

### 1.5 Existing plotting framework

- **Engine:** ggplot2 throughout; `pheatmap` for heatmaps; `gridExtra::grid.arrange` for composition.
- **Themes:** `theme_minimal()` / `theme_bw()` with manual `theme()` tuning.
- **Qualitative palette (used by trajectories):** `c("#E31A1C","#FF7F00","#FDBF6F","#33A02C","#1F78B4","#6A3D9A","#B15928","#FB9A99","#A6CEE3","#B2DF8A")` (a Paired-style ramp, `colorRampPalette`-extended).
- **Continuous palette:** `viridis`.
- **Convention:** functions return *named lists of ggplot objects* plus the underlying data frames, with an optional `save_plots`/`output_dir` side-channel. `nova_dynamics` mirrors this exactly (every analysis returns `$plots`, `$<metric tables>`, and an S3 class for `nova_describe()`).

---

## 2. Existing Concepts (and where dynamics formalises them)

| Concept | Where it lives now | Status | How `nova_dynamics` formalises it |
|---|---|---|---|
| **Dimensionality reduction** | `pca_analysis_enhanced()` | Mature | Consumed, never replaced. Module is embedding-agnostic. |
| **Trajectory code** | `plot_pca_trajectories_general()` (`R/plots.R:603`) | Mature but *descriptive* — it draws mean paths per group across ordered timepoints and computes per-step means/SE. | Promoted from *drawing* a path to *quantifying* it: length, velocity, acceleration, tortuosity, directional persistence (`nova_state_geometry()`). |
| **Longitudinal / maturation analysis** | Same trajectory function (timepoints = DIV or minutes) | Present | Same machinery; the new time parser makes non-uniform spacing (min/h/day) first-class so velocity uses real Δt. |
| **Drug-perturbation analysis** | Trajectories grouped by `Treatment` | Present | `nova_trajectory_similarity()` compares perturbation paths (DTW/Fréchet/Euclidean/cosine) → dendrogram. |
| **Clustering** | *Implicit only* — `stat_ellipse()` in `pca_plots_enhanced()` (`R/plots.R:262`), hierarchical clustering inside heatmaps (`pheatmap`). | Partial — no explicit state-cluster object. | `nova_transition_matrix()` introduces explicit state discretisation (k-means on pooled embedding) → occupancy + transition probabilities. |
| **Correlation analysis** | `analyze_pca_variable_importance_general()` (loadings, variable contributions) | Mature | Untouched; remains the "which metrics drive the axes" layer. |
| **Stability / convergence** | *Absent* — never quantified. | Gap | `nova_dynamical_regime()` (stable / convergent / divergent / oscillatory / transitional) + `nova_landscape()` (occupancy density / pseudo-potential). |
| **Interpretation** | *Absent* — every function emits numbers/plots only. | Gap (explicitly called out in the brief) | `nova_describe()` rule-based natural-language layer. |

### 2.1 The timepoint-ordering limitation (root-cause, not symptom)

`plot_pca_trajectories_general()` orders time via a **hardcoded literal list** (`R/plots.R:715-721`): `baseline_patterns`, `minute_patterns = c("0min",…,"60min")`, `hour_patterns = c("1h","1h30min",…)`. Labels absent from these lists (e.g. `1h15`, `1h45`, `90min`, `45s`, `DIV9`) fall through to **alphabetical** sorting — which places `1h15` before `1h` and `90min` before `15min`. The bundled MEA012 plate literally contains `1h15`, `1h30`, `1h45`, so this is a live correctness risk, not hypothetical.

**Fix shipped in the module:** `nova_order_timepoints()` parses any `w/d/h/min/s`, compound (`1h30`, `1h30min`), `DIVn`, and bare-numeric label to a numeric time in minutes, always sorts `baseline`/`pre`/`BL` first, and degrades to alphabetical only for genuinely unparseable labels. The existing trajectory function is left untouched (backward compatibility); the new function is offered as the canonical ordering helper and is used everywhere in `nova_dynamics`.

---

## 3. Module Interaction Map

```
                 ┌──────────────────────┐
  Axion CSVs ──> │ process_mea_flexible │ ── normalized_data (fold-change vs baseline)
                 └──────────┬───────────┘
                            v
                 ┌──────────────────────┐      plot_data: PC1,PC2,Timepoint,
                 │ pca_analysis_enhanced│ ───> Treatment,Genotype,Well,Experiment
                 └──────────┬───────────┘
            ┌───────────────┼───────────────────────────┐
            v               v                           v
  pca_plots_enhanced  plot_pca_trajectories_   analyze_pca_variable_
  (scatter/ellipse)   general (mean paths)     importance_general (loadings)
            │               │
            │               │   ── EXISTING boundary ──
            v               v
        ┌───────────────────────────────────────────────────────────┐
        │                     nova_dynamics  (NEW)                    │
        │  reads plot_data ↓                                          │
        │  nova_extract_trajectories()  → ordered state paths         │
        │     ├─ nova_state_geometry()        (length/vel/accel/tort) │
        │     ├─ nova_transition_matrix()     (k-means states + flow) │
        │     ├─ nova_trajectory_similarity() (DTW/Fréchet/Euclid/cos)│
        │     ├─ nova_dynamical_regime()      (regime + confidence)   │
        │     └─ nova_landscape()             (occupancy / potential) │
        │  nova_describe()  → natural-language interpretation         │
        └───────────────────────────────────────────────────────────┘
```

The dynamics layer attaches strictly *downstream* of PCA and *parallel* to the existing trajectory/loadings plots. No existing function signature, return value, or NAMESPACE export is modified — new symbols only.

---

## 4. Backward-compatibility guarantees adopted for Phase 2+

1. **Additive only.** New `R/dynamics_*.R` files; no edits to existing function bodies or signatures.
2. **No new hard dependencies.** New `Imports` are limited to base-shipped `stats`/`grDevices`/`MASS`; `igraph`, `patchwork`, `dtw` are `Suggests` with `requireNamespace()` guards and base-R fallbacks, so the package builds and every example runs even if they are absent.
3. **Same I/O idiom.** Each analysis returns a named list (`$plots`, metric tables) carrying an S3 class for dispatch; `save_plots`/`output_dir` side-channel preserved.
4. **Same visual language.** A shared `nova_theme()` + `nova_palette()` extend (do not replace) the existing `theme_minimal` + Paired-style palette, with `baseline` always first in time-ordered figures.
5. **Reuses the data contract.** Consumes `pca_output$plot_data`; also accepts a bare data frame, so UMAP/other embeddings work today.

---

*End of Phase 1 audit.*

# NOVA Workflow Optimization Design
*Date: 2026-03-24*

## Overview

Add a two-script workflow layer on top of the existing NOVA package that enables:
1. Compute-once (all analyses), flexible variable assignment from auto-discovered metadata
2. Plot-many-times with a clean tune block — filters display without losing data, shows plots interactively and saves to organized folders

No changes to the core package API. New scripts only.

---

## Problem Statement

The existing package functions are rigorous but require users to:
- Know hardcoded row positions (121–168) for metadata
- Construct complex nested-list parameters (e.g. `timepoint_fusions`)
- Re-run expensive computations when only adjusting plot aesthetics
- Manually organize output files

---

## Design

### Two-Script Architecture

```
01_compute.R   — discover → assign variables → compute all → save nova_results.rds
02_plot.R      — load rds → TUNE block → filter → show + save organized figures
```

### `01_compute.R`

**Section 1 — Discovery**
- User sets `DATA_DIR` and runs `discover_mea_structure()`
- Console prints all detected metadata columns with unique values
- Message instructs user to fill in Section 2 before continuing

**Section 2 — Variable Assignment**
- `VARIABLE_ROLES` list maps role names to actual column names found in data
- `TIMEPOINTS_ORDER` for ordering
- `BASELINE` for normalization reference
- All NULLable — roles the user doesn't have are simply skipped

```r
VARIABLE_ROLES <- list(
  timepoint = "Timepoint",
  treatment = "Treatment",
  genotype  = "Genotype",
  group     = NULL,
  exclude   = "Exclude"
)
```

**Section 3 — Compute All**
Runs in sequence, storing results:
1. `process_mea_flexible()` — raw + normalized data
2. `pca_analysis_enhanced()` — PCA scores, loadings, elbow plot
3. `plot_pca_trajectories_general()` — trajectory data
4. `create_mea_heatmaps_enhanced()` — heatmap matrices
5. Variable importance analysis

Saves everything to `nova_results.rds` with a `session_info` timestamp. Prints completion summary.

---

### `02_plot.R`

**Tune Block (only section user edits)**

```r
RESULTS_PATH     <- "nova_results.rds"
OUTPUT_DIR       <- "figures/"

# --- Filter (NULL = show all) ---
SHOW_TIMEPOINTS  <- NULL
SHOW_TREATMENTS  <- NULL
SHOW_GENOTYPES   <- NULL

# --- Aesthetics ---
COLOR_BY         <- "Treatment"
SHAPE_BY         <- "Genotype"
FACET_BY         <- NULL
CUSTOM_COLORS    <- NULL   # or named vector

# --- Sizes ---
POINT_SIZE       <- 3
LINE_SIZE        <- 1
FONT_SIZE        <- 12
FIGURE_WIDTH     <- 12
FIGURE_HEIGHT    <- 10
```

**Execution**
1. Load `.rds`
2. Apply display filters — filters a view, never the stored object
3. Generate each plot:
   - Print to viewer (via `print()`)
   - Save to organized subfolder
4. Print summary of what was shown and saved

**Output folder structure**
```
figures/
├── pca/
│   ├── pca_scatter_{COLOR_BY}_{SHAPE_BY}.pdf
│   └── pca_elbow.pdf
├── trajectories/
│   └── pca_trajectories_{group}.pdf
├── heatmaps/
│   └── mea_heatmap_{COLOR_BY}.pdf
└── variable_importance/
    └── top_variables_PC1_PC2.pdf
```

---

## Key Principles

- **No information loss**: filters in `02_plot.R` are display-only; the `.rds` always holds all data
- **Flexible variable roles**: any role can be NULL if the experiment doesn't have that metadata
- **Show + save**: every plot is printed to the R viewer AND written to disk
- **No recomputation**: re-running `02_plot.R` with different aesthetics takes seconds
- **Robust to missing metadata**: code checks which roles are non-NULL before using them

---

## Files to Create

| File | Purpose |
|------|---------|
| `01_compute.R` | Discovery + variable assignment + full computation |
| `02_plot.R` | Load results + tune block + show/save figures |

## Files to Fix (existing package bugs)

| File | Issue |
|------|-------|
| `pca_analysis.R` | `perform_mea_pca` stub always throws — remove or implement |
| `pca_analysis.R` | Local `null_coalesce` duplicates `utilities.R` export |
| `pca_analysis.R` | Excel load in `tryCatch` has scoping bug — inner assignment doesn't propagate |
| `data_handling.R` | Magic row numbers (121–168) should be named constants |

---

## Out of Scope

- Parallelization of multi-experiment processing
- Interactive Shiny UI
- Changes to exported function signatures

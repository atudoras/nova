# NOVA Workflow Optimization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix 4 existing package bugs and create two workflow scripts (`01_compute.R`, `02_plot.R`) that let users run all analyses once and iterate on publication-ready figures without recomputation.

**Architecture:** Two-phase separation — `01_compute.R` discovers data, lets user assign variable roles, runs all analyses, saves a single `.rds`. `02_plot.R` loads that `.rds`, applies display-only filters, prints every plot to viewer, and saves to organized `figures/` subfolders. The stored `.rds` is never mutated by `02_plot.R`.

**Tech Stack:** R, testthat (≥3.0.0), ggplot2, readxl, writexl, NOVA package functions

---

## Task 1: Set up test infrastructure

**Files:**
- Create: `tests/testthat.R`
- Create: `tests/testthat/test-utilities.R`

**Step 1: Create test entry point**

```r
# tests/testthat.R
library(testthat)
library(NOVA)
test_check("NOVA")
```

**Step 2: Write failing tests for `null_coalesce`**

```r
# tests/testthat/test-utilities.R
test_that("null_coalesce returns lhs when not NULL", {
  expect_equal(null_coalesce(5, 10), 5)
  expect_equal(null_coalesce("a", "b"), "a")
  expect_equal(null_coalesce(FALSE, TRUE), FALSE)
})

test_that("null_coalesce returns rhs when lhs is NULL", {
  expect_equal(null_coalesce(NULL, 10), 10)
  expect_equal(null_coalesce(NULL, "b"), "b")
})
```

**Step 3: Run tests to verify they pass (utilities.R already has this function)**

```bash
cd "NOVA copy 2/.claude/worktrees/crazy-heisenberg"
Rscript -e "devtools::test()"
```
Expected: tests PASS (function already exists, this establishes the baseline)

**Step 4: Commit**

```bash
git add tests/testthat.R tests/testthat/test-utilities.R
git commit -m "test: add test infrastructure and null_coalesce tests"
```

---

## Task 2: Fix `perform_mea_pca` stub

The exported `perform_mea_pca` in `pca_analysis.R:19-22` always throws a stop() error, which is confusing — users may call it expecting it to work.

**Files:**
- Modify: `R/pca_analysis.R:19-22`

**Step 1: Write failing test**

```r
# Add to tests/testthat/test-pca-analysis.R (create file)
test_that("perform_mea_pca redirects to pca_analysis_enhanced with helpful message", {
  expect_error(
    perform_mea_pca(data.frame(), variables = NULL),
    regexp = "pca_analysis_enhanced"   # error message must mention the right function
  )
})
```

**Step 2: Run to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'pca-analysis')"
```
Expected: FAIL — current message says "template function", not "pca_analysis_enhanced"

**Step 3: Replace stub with a redirect error**

In `R/pca_analysis.R`, replace lines 19–22:

```r
perform_mea_pca <- function(data, variables = NULL, scale = TRUE, center = TRUE, ...) {
  stop(
    "perform_mea_pca() is not implemented. ",
    "Use pca_analysis_enhanced() instead — it accepts a data frame, ",
    "a processing result, or a file path. See ?pca_analysis_enhanced."
  )
}
```

**Step 4: Run tests**

```bash
Rscript -e "devtools::test(filter = 'pca-analysis')"
```
Expected: PASS

**Step 5: Commit**

```bash
git add R/pca_analysis.R tests/testthat/test-pca-analysis.R
git commit -m "fix: improve perform_mea_pca stub error to redirect to pca_analysis_enhanced"
```

---

## Task 3: Remove duplicate `null_coalesce` in `pca_analysis_enhanced`

`pca_analysis.R:210` defines a local `null_coalesce` that shadows the exported one in `utilities.R`. The local definition is identical — it is dead weight.

**Files:**
- Modify: `R/pca_analysis.R` (around line 210)

**Step 1: Write test confirming package-level function is used**

```r
# Add to tests/testthat/test-pca-analysis.R
test_that("null_coalesce is not redefined inside pca_analysis_enhanced body", {
  fn_body <- deparse(body(pca_analysis_enhanced))
  local_def <- any(grepl("null_coalesce\\s*<-\\s*function", fn_body))
  expect_false(local_def,
    info = "null_coalesce should not be locally redefined inside pca_analysis_enhanced")
})
```

**Step 2: Run to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'pca-analysis')"
```
Expected: FAIL

**Step 3: Delete the local definition**

In `R/pca_analysis.R`, find and delete this block (around line 209–211):

```r
  # Define null_c operator for NULL coalescing if not already defined
  null_coalesce <- function(x, y) if (is.null(x)) y else x
```

The package-level `null_coalesce` from `utilities.R` is already exported and available.

**Step 4: Run tests**

```bash
Rscript -e "devtools::test()"
```
Expected: all PASS

**Step 5: Commit**

```bash
git add R/pca_analysis.R tests/testthat/test-pca-analysis.R
git commit -m "fix: remove duplicate null_coalesce definition inside pca_analysis_enhanced"
```

---

## Task 4: Fix Excel loading scoping bug in `pca_analysis_enhanced`

In `R/pca_analysis.R:162-181`, the `data_path` branch uses nested `tryCatch` with assignments inside error handler functions. In R, those inner assignments are local to the handler closure — `normalized_data` and `value_column` never propagate back to the outer function scope.

**Files:**
- Modify: `R/pca_analysis.R:162-181`
- Modify: `tests/testthat/test-pca-analysis.R`

**Step 1: Write a failing test**

This requires a tiny helper to create a fake Excel file in a temp dir.

```r
# Add to tests/testthat/test-pca-analysis.R
test_that("pca_analysis_enhanced loads from raw_data sheet when normalized_data missing", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Build minimal wide-format data that pca_analysis_enhanced can use
  fake_data <- data.frame(
    Variable  = paste0("V", 1:5),
    Sample_A  = rnorm(5),
    Sample_B  = rnorm(5),
    stringsAsFactors = FALSE
  )
  # Pivot to long format matching expected structure
  long_data <- tidyr::pivot_longer(fake_data, -Variable,
                                    names_to = "Sample", values_to = "Value")

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(raw_data = long_data), path = tmp)

  # Should NOT throw a scoping-related "object not found" error
  expect_no_error({
    result <- pca_analysis_enhanced(
      data_path      = tmp,
      value_column   = "Value",
      variable_column = "Variable",
      verbose        = FALSE
    )
  })
  unlink(tmp)
})
```

**Step 2: Run to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'pca-analysis')"
```
Expected: FAIL (scoping error — `normalized_data` stays NULL)

**Step 3: Replace the scoping-broken block**

In `R/pca_analysis.R`, replace the entire `else if (!is.null(data_path))` block (lines ~162-181) with:

```r
  # Option 2: Load from Excel file path
  else if (!is.null(data_path)) {
    if (verbose) cat("Loading data from Excel file:", data_path, "\n")
    if (!file.exists(data_path)) stop("Data file not found: ", data_path)

    sheets_to_try <- c("normalized_data", "raw_data")
    loaded <- NULL

    for (sheet in sheets_to_try) {
      loaded <- tryCatch(
        readxl::read_excel(data_path, sheet = sheet),
        error = function(e) NULL
      )
      if (!is.null(loaded)) {
        if (verbose) cat("Loaded sheet:", sheet, "\n")
        if (sheet == "raw_data" &&
            !"Normalized_Value" %in% names(loaded) &&
            "Value" %in% names(loaded)) {
          value_column <- "Value"
        }
        break
      }
    }

    if (is.null(loaded)) {
      stop("Could not load data from 'normalized_data' or 'raw_data' sheets in: ", data_path)
    }

    normalized_data <- loaded
  }
```

**Step 4: Run tests**

```bash
Rscript -e "devtools::test()"
```
Expected: all PASS

**Step 5: Commit**

```bash
git add R/pca_analysis.R tests/testthat/test-pca-analysis.R
git commit -m "fix: resolve scoping bug in pca_analysis_enhanced Excel loading branch"
```

---

## Task 5: Replace magic row numbers with named constants

Row numbers 121–168 appear 8+ times across `data_handling.R` with no explanation. Replace with named constants defined once at the top of the file.

**Files:**
- Modify: `R/data_handling.R` (top of file + all usages)

**Step 1: Write test confirming constants exist**

```r
# Create tests/testthat/test-data-handling.R
test_that("MEA file structure constants are defined and correct", {
  expect_equal(NOVA:::MEA_ROW_WELLS,      121L)
  expect_equal(NOVA:::MEA_ROW_TREATMENT,  122L)
  expect_equal(NOVA:::MEA_ROW_GENOTYPE,   123L)
  expect_equal(NOVA:::MEA_ROW_EXCLUDE,    124L)
  expect_equal(NOVA:::MEA_ROW_VARS_START, 125L)
  expect_equal(NOVA:::MEA_ROW_VARS_END,   168L)
})
```

**Step 2: Run to verify it fails**

```bash
Rscript -e "devtools::test(filter = 'data-handling')"
```
Expected: FAIL — constants don't exist yet

**Step 3: Add constants at top of `data_handling.R` (after the file comment, before first `#'`)**

```r
# MEA file structure constants — row positions in standard MEA CSV format
MEA_ROW_WELLS      <- 121L  # Well identifiers (A1, A2, B1, etc.)
MEA_ROW_TREATMENT  <- 122L  # Treatment conditions
MEA_ROW_GENOTYPE   <- 123L  # Genotype information
MEA_ROW_EXCLUDE    <- 124L  # Exclusion flags
MEA_ROW_VARS_START <- 125L  # First measured variable row
MEA_ROW_VARS_END   <- 168L  # Last measured variable row
MEA_MIN_ROWS       <- 124L  # Minimum rows required in a valid CSV
```

**Step 4: Replace all magic numbers in `data_handling.R`**

In `discover_mea_structure()`:
- Replace `if (nrow(raw_data) < 124)` → `if (nrow(raw_data) < MEA_MIN_ROWS)`
- Replace `if (nrow(raw_data) >= 124)` → `if (nrow(raw_data) >= MEA_MIN_ROWS)`
- Replace `well_row <- 121` → `well_row <- MEA_ROW_WELLS`
- Replace `treatment_row <- 122` → `treatment_row <- MEA_ROW_TREATMENT`
- Replace `genotype_row <- 123` → `genotype_row <- MEA_ROW_GENOTYPE`
- Replace `exclude_row <- 124` → `exclude_row <- MEA_ROW_EXCLUDE`
- Replace `if (nrow(raw_data) >= 168)` → `if (nrow(raw_data) >= MEA_ROW_VARS_END)`
- Replace `variables <- unlist(raw_data[125:168, 1])` → `variables <- unlist(raw_data[MEA_ROW_VARS_START:MEA_ROW_VARS_END, 1])`

In `process_mea_flexible()`:
- Replace `if (nrow(raw) < 168)` → `if (nrow(raw) < MEA_ROW_VARS_END)`
- Replace `well_ids   <- unlist(raw[121, -1])` → `well_ids   <- unlist(raw[MEA_ROW_WELLS, -1])`
- Replace `treatments <- unlist(raw[122, -1])` → `treatments <- unlist(raw[MEA_ROW_TREATMENT, -1])`
- Replace `genotypes  <- unlist(raw[123, -1])` → `genotypes  <- unlist(raw[MEA_ROW_GENOTYPE, -1])`
- Replace `exclude    <- unlist(raw[124, -1])` → `exclude    <- unlist(raw[MEA_ROW_EXCLUDE, -1])`
- Replace `variable_names <- unlist(raw[125:168, 1])` → `variable_names <- unlist(raw[MEA_ROW_VARS_START:MEA_ROW_VARS_END, 1])`
- Replace `values_matrix <- raw[125:168, -1]` → `values_matrix  <- raw[MEA_ROW_VARS_START:MEA_ROW_VARS_END, -1]`

**Step 5: Run all tests**

```bash
Rscript -e "devtools::test()"
```
Expected: all PASS

**Step 6: Build check**

```bash
Rscript -e "devtools::check()"
```
Expected: 0 errors, 0 warnings

**Step 7: Commit**

```bash
git add R/data_handling.R tests/testthat/test-data-handling.R
git commit -m "refactor: replace magic MEA row numbers with named constants"
```

---

## Task 6: Create `01_compute.R`

**Files:**
- Create: `Example/01_compute.R`

**Step 1: Create the file**

```r
# =============================================================================
# NOVA Workflow — 01_compute.R
# Run this script ONCE per dataset. It discovers your data, lets you assign
# variable roles, computes all analyses, and saves results to an .rds file.
# Then open 02_plot.R to generate and fine-tune your figures.
# =============================================================================
library(NOVA)

# =============================================================================
# STEP 1 — POINT TO YOUR DATA
# Edit DATA_DIR to the folder containing your MEA experiment subfolders.
# =============================================================================
DATA_DIR     <- "path/to/your/MEA_data"   # <-- EDIT THIS
RESULTS_PATH <- "nova_results.rds"         # where computed results will be saved

# =============================================================================
# STEP 2 — DISCOVER YOUR DATA
# Run from here to the end of this section. Read the output carefully,
# then fill in STEP 3 before running further.
# =============================================================================
cat("\n========================================\n")
cat("  NOVA DATA DISCOVERY\n")
cat("========================================\n\n")

if (!dir.exists(DATA_DIR)) stop("DATA_DIR not found: ", DATA_DIR)

discovery <- discover_mea_structure(DATA_DIR, verbose = FALSE)

cat("Experiments found (", discovery$experiment_count, "):\n  ",
    paste(names(discovery$experiments), collapse = ", "), "\n\n", sep = "")

cat("Timepoints detected:\n  ",
    paste(discovery$all_timepoints, collapse = ", "), "\n", sep = "")

if (length(discovery$potential_baselines) > 0) {
  cat("Likely baseline(s):\n  ",
      paste(discovery$potential_baselines, collapse = ", "), "\n", sep = "")
}

# Show metadata values from first experiment so user knows what to assign
cat("\n--- METADATA FOUND IN YOUR FILES ---\n")
cat("(Use these values when filling in VARIABLE_ROLES below)\n\n")

first_exp <- discovery$experiments[[1]]
if (!is.null(first_exp$metadata)) {
  m <- first_exp$metadata
  if (!is.null(m$treatments) && length(m$treatments) > 0) {
    cat("Row 122 [Treatment candidates]:\n  ",
        paste(head(unique(m$treatments), 15), collapse = ", "), "\n\n", sep = "")
  }
  if (!is.null(m$genotypes) && length(m$genotypes) > 0) {
    cat("Row 123 [Genotype candidates]:\n  ",
        paste(head(unique(m$genotypes), 15), collapse = ", "), "\n\n", sep = "")
  }
}

cat("Measured variables (first 10 of", length(discovery$all_variables), "):\n  ",
    paste(head(discovery$all_variables, 10), collapse = "\n  "), "\n", sep = "")
if (length(discovery$all_variables) > 10) {
  cat("  ... and", length(discovery$all_variables) - 10, "more\n")
}

cat("\n========================================\n")
cat("  NEXT: Edit STEP 3 below, then re-run\n")
cat("  from STEP 3 to the end of the script.\n")
cat("========================================\n\n")

# =============================================================================
# STEP 3 — ASSIGN YOUR VARIABLE ROLES
# Map each semantic role to the column name in your data.
# Set a role to NULL if your experiment does not have it.
# Column names must match exactly what is in your CSV metadata rows.
# =============================================================================

VARIABLE_ROLES <- list(
  treatment = "Treatment",   # main experimental condition  (row 122 in CSV)
  genotype  = "Genotype",    # strain / genotype            (row 123 in CSV)
  group     = NULL           # any extra grouping variable, or NULL
)

# Timepoint order for trajectory plots (edit to match your experiment)
# Leave as discovery$all_timepoints to use auto-detected order.
TIMEPOINTS_ORDER <- discovery$all_timepoints
# Example: TIMEPOINTS_ORDER <- c("baseline", "0min", "15min", "30min", "1h", "2h")

# Timepoint to use for fold-change normalization.
# Set to NULL to skip normalization and work with raw values.
BASELINE <- if (length(discovery$potential_baselines) > 0) {
  discovery$potential_baselines[1]
} else {
  NULL
}

# =============================================================================
# STEP 4 — COMPUTE EVERYTHING
# Do not edit below this line unless you know what you are doing.
# =============================================================================

# Build grouping column vector from assigned roles (skip NULLs)
grouping_cols <- unname(unlist(Filter(Negate(is.null), VARIABLE_ROLES)))

cat("========================================\n")
cat("  COMPUTING ALL ANALYSES\n")
cat("========================================\n\n")
cat("Grouping variables:", paste(grouping_cols, collapse = ", "), "\n")
cat("Baseline timepoint:", if (is.null(BASELINE)) "none (using raw values)" else BASELINE, "\n\n")

# --- 4a. Process MEA data ---
cat("--- Processing MEA data ---\n")
processed <- process_mea_flexible(
  main_dir           = DATA_DIR,
  grouping_variables = grouping_cols,
  baseline_timepoint = BASELINE,
  verbose            = TRUE
)

# --- 4b. PCA ---
cat("\n--- Running PCA ---\n")
pca_results <- pca_analysis_enhanced(
  processing_result  = processed,
  grouping_variables = grouping_cols,
  verbose            = TRUE
)

# --- 4c. PCA trajectories ---
cat("\n--- Computing PCA trajectories ---\n")
trajectories <- tryCatch({
  plot_pca_trajectories_general(
    pca_results,
    timepoint_order     = TIMEPOINTS_ORDER,
    trajectory_grouping = grouping_cols
  )
}, error = function(e) {
  cat("  Note: trajectory plot skipped —", e$message, "\n")
  NULL
})

# --- 4d. Heatmaps ---
cat("\n--- Generating heatmaps ---\n")
heatmaps <- tryCatch({
  create_mea_heatmaps_enhanced(
    processing_result = processed,
    grouping_columns  = grouping_cols
  )
}, error = function(e) {
  cat("  Note: heatmap generation skipped —", e$message, "\n")
  NULL
})

# =============================================================================
# STEP 5 — SAVE RESULTS
# =============================================================================
nova_results <- list(
  processed    = processed,
  pca          = pca_results,
  trajectories = trajectories,
  heatmaps     = heatmaps,
  config = list(
    data_dir         = DATA_DIR,
    variable_roles   = VARIABLE_ROLES,
    grouping_cols    = grouping_cols,
    timepoints_order = TIMEPOINTS_ORDER,
    baseline         = BASELINE
  ),
  session_info = list(
    timestamp    = Sys.time(),
    r_version    = R.version.string,
    nova_version = as.character(packageVersion("NOVA"))
  )
)

saveRDS(nova_results, RESULTS_PATH)

cat("\n========================================\n")
cat("  DONE\n")
cat("========================================\n")
cat("Results saved to:", RESULTS_PATH, "\n\n")
cat("Next step: open 02_plot.R, set RESULTS_PATH to\n")
cat("  '", RESULTS_PATH, "'\n", sep = "")
cat("and tune your figures.\n\n")
```

**Step 2: Smoke-test the script parses without error**

```bash
Rscript -e "parse(file = 'Example/01_compute.R')"
```
Expected: no output (clean parse)

**Step 3: Commit**

```bash
git add Example/01_compute.R
git commit -m "feat: add 01_compute.R workflow script — discovery, variable assignment, full computation"
```

---

## Task 7: Create `02_plot.R`

**Files:**
- Create: `Example/02_plot.R`

**Step 1: Create the file**

```r
# =============================================================================
# NOVA Workflow — 02_plot.R
# Load results computed by 01_compute.R, tune display parameters,
# and generate figures. Every plot is shown in the viewer AND saved to disk.
# Only edit the TUNE block below — re-run anytime to update figures.
# =============================================================================
library(NOVA)
library(ggplot2)

# =============================================================================
# TUNE YOUR PLOTS HERE
# This is the only section you need to edit.
# =============================================================================

RESULTS_PATH   <- "nova_results.rds"  # path to file saved by 01_compute.R
OUTPUT_DIR     <- "figures/"          # root folder for saved figures

# --- Filter what to display (NULL = show all) --------------------------------
# These filters are display-only — the .rds is never modified.
SHOW_TIMEPOINTS <- NULL    # e.g. c("baseline", "1h", "2h")
SHOW_TREATMENTS <- NULL    # e.g. c("Control", "Drug_A")
SHOW_GENOTYPES  <- NULL    # e.g. c("WT", "KO")

# --- Aesthetic mapping -------------------------------------------------------
COLOR_BY <- "Treatment"    # variable mapped to point/line color
SHAPE_BY <- "Genotype"     # variable mapped to point shape (or NULL)
FACET_BY <- NULL           # variable for panel faceting (or NULL)

# --- Colors ------------------------------------------------------------------
# NULL = automatic scientific palette based on number of groups.
# To override, provide a named vector matching your group values:
#   CUSTOM_COLORS <- c("Control" = "#0066CC", "Drug_A" = "#CC0000")
CUSTOM_COLORS <- NULL

# --- Sizes and fonts ---------------------------------------------------------
POINT_SIZE    <- 3
LINE_SIZE     <- 1
FONT_SIZE     <- 12
FIGURE_WIDTH  <- 12
FIGURE_HEIGHT <- 10
DPI           <- 300

# =============================================================================
# LOAD & VALIDATE  (do not edit below)
# =============================================================================
if (!file.exists(RESULTS_PATH)) {
  stop(
    "Results file not found: ", RESULTS_PATH,
    "\nRun 01_compute.R first to generate it."
  )
}

cat("\n========================================\n")
cat("  NOVA FIGURE GENERATION\n")
cat("========================================\n\n")

nova   <- readRDS(RESULTS_PATH)
config <- nova$config

cat("Results from   :", format(nova$session_info$timestamp), "\n")
cat("Data directory :", config$data_dir, "\n")
cat("Grouping vars  :", paste(config$grouping_cols, collapse = ", "), "\n\n")

# Validate aesthetic variables against what is actually in the data
available_vars <- names(nova$pca$plot_data)

check_var <- function(var_name, var_label) {
  if (!is.null(var_name) && !var_name %in% available_vars) {
    warning(var_label, " '", var_name, "' not found in data. Available: ",
            paste(available_vars, collapse = ", "))
    return(NULL)
  }
  var_name
}

COLOR_BY <- check_var(COLOR_BY, "COLOR_BY")
SHAPE_BY <- check_var(SHAPE_BY, "SHAPE_BY")
FACET_BY <- check_var(FACET_BY, "FACET_BY")

# =============================================================================
# APPLY DISPLAY FILTERS
# Filters a copy of plot_data — nova object is never mutated.
# =============================================================================
plot_data <- nova$pca$plot_data

n_before <- nrow(plot_data)

if (!is.null(SHOW_TIMEPOINTS) && "Timepoint" %in% names(plot_data)) {
  plot_data <- plot_data[plot_data$Timepoint %in% SHOW_TIMEPOINTS, ]
  cat("Timepoint filter:", paste(SHOW_TIMEPOINTS, collapse = ", "), "\n")
}

if (!is.null(SHOW_TREATMENTS) && !is.null(COLOR_BY) && COLOR_BY %in% names(plot_data)) {
  plot_data <- plot_data[plot_data[[COLOR_BY]] %in% SHOW_TREATMENTS, ]
  cat("Treatment filter:", paste(SHOW_TREATMENTS, collapse = ", "), "\n")
}

if (!is.null(SHOW_GENOTYPES) && !is.null(SHAPE_BY) && SHAPE_BY %in% names(plot_data)) {
  plot_data <- plot_data[plot_data[[SHAPE_BY]] %in% SHOW_GENOTYPES, ]
  cat("Genotype filter :", paste(SHOW_GENOTYPES, collapse = ", "), "\n")
}

cat("Samples shown   :", nrow(plot_data), "of", n_before, "\n\n")

if (nrow(plot_data) == 0) {
  stop("No samples remain after filtering. Check SHOW_* parameters.")
}

# =============================================================================
# CREATE OUTPUT FOLDER STRUCTURE
# =============================================================================
subdirs <- c("pca", "trajectories", "heatmaps", "variable_importance")
for (d in file.path(OUTPUT_DIR, subdirs)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

saved_files <- character(0)

# Helper: save a ggplot and record the path
save_plot <- function(p, path) {
  ggsave(path, plot = p, width = FIGURE_WIDTH, height = FIGURE_HEIGHT, dpi = DPI)
  saved_files <<- c(saved_files, path)
  cat("  Saved:", path, "\n")
}

# Helper: save a non-ggplot (e.g. pheatmap) via pdf device
save_base_plot <- function(p, path) {
  pdf(path, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
  print(p)
  dev.off()
  saved_files <<- c(saved_files, path)
  cat("  Saved:", path, "\n")
}

# =============================================================================
# PCA SCATTER PLOTS
# =============================================================================
cat("--- PCA scatter plots ---\n")

pca_plots_out <- pca_plots_enhanced(
  pca_output         = nova$pca,
  plot_data          = plot_data,
  color_variable     = COLOR_BY,
  shape_variable     = SHAPE_BY,
  pannels_var        = FACET_BY,
  save_plots         = FALSE,
  verbose            = FALSE
)

for (plot_name in names(pca_plots_out$plots)) {
  p <- pca_plots_out$plots[[plot_name]]
  if (inherits(p, "ggplot")) {
    print(p)
    save_plot(p, file.path(OUTPUT_DIR, "pca", paste0(plot_name, ".pdf")))
  }
}

# Elbow plot
if (!is.null(nova$pca$elbow_plot)) {
  print(nova$pca$elbow_plot)
  ggsave(file.path(OUTPUT_DIR, "pca", "pca_elbow.pdf"),
         plot = nova$pca$elbow_plot, width = 10, height = 6, dpi = DPI)
  saved_files <- c(saved_files, file.path(OUTPUT_DIR, "pca", "pca_elbow.pdf"))
  cat("  Saved:", file.path(OUTPUT_DIR, "pca", "pca_elbow.pdf"), "\n")
}

# =============================================================================
# TRAJECTORY PLOTS
# =============================================================================
if (!is.null(nova$trajectories)) {
  cat("\n--- Trajectory plots ---\n")

  traj_items <- if (inherits(nova$trajectories, "ggplot")) {
    list(pca_trajectories = nova$trajectories)
  } else if (is.list(nova$trajectories)) {
    nova$trajectories
  } else {
    list()
  }

  for (plot_name in names(traj_items)) {
    p <- traj_items[[plot_name]]
    if (inherits(p, "ggplot")) {
      print(p)
      save_plot(p, file.path(OUTPUT_DIR, "trajectories", paste0(plot_name, ".pdf")))
    }
  }
}

# =============================================================================
# HEATMAPS
# =============================================================================
if (!is.null(nova$heatmaps)) {
  cat("\n--- Heatmaps ---\n")

  heatmap_items <- if (is.list(nova$heatmaps) && !inherits(nova$heatmaps, "ggplot")) {
    nova$heatmaps
  } else {
    list(heatmap = nova$heatmaps)
  }

  for (plot_name in names(heatmap_items)) {
    p <- heatmap_items[[plot_name]]
    if (!is.null(p)) {
      if (inherits(p, "ggplot")) {
        print(p)
        save_plot(p, file.path(OUTPUT_DIR, "heatmaps", paste0(plot_name, ".pdf")))
      } else {
        # pheatmap / base graphics
        print(p)
        save_base_plot(p, file.path(OUTPUT_DIR, "heatmaps", paste0(plot_name, ".pdf")))
      }
    }
  }
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n========================================\n")
cat("  DONE —", length(saved_files), "figures saved\n")
cat("========================================\n")
cat("Output folder:", normpath(OUTPUT_DIR), "\n\n")
cat("To update figures: edit the TUNE block above and re-run.\n")
cat("To recompute data: re-run 01_compute.R.\n\n")
```

**Step 2: Smoke-test parses without error**

```bash
Rscript -e "parse(file = 'Example/02_plot.R')"
```
Expected: no output

**Step 3: Commit**

```bash
git add Example/02_plot.R
git commit -m "feat: add 02_plot.R workflow script — load results, tune aesthetics, show and save figures"
```

---

## Task 8: Run full package check

**Step 1: Run devtools::check()**

```bash
Rscript -e "devtools::check()"
```
Expected: 0 errors, 0 warnings, ≤1 note (the `normpath` function in 02_plot.R — replace with `normalizePath` if the check flags it)

**Step 2: If `normpath` is flagged**, replace in `02_plot.R`:

```r
# Replace:
cat("Output folder:", normpath(OUTPUT_DIR), "\n\n")
# With:
cat("Output folder:", normalizePath(OUTPUT_DIR, mustWork = FALSE), "\n\n")
```

**Step 3: Final commit**

```bash
git add -A
git commit -m "fix: use normalizePath in 02_plot.R for cross-platform compatibility"
```

---

## Summary of deliverables

| File | Type | What changed |
|------|------|-------------|
| `R/pca_analysis.R` | Fix | Stub error message, duplicate null_coalesce removed, Excel scoping bug fixed |
| `R/data_handling.R` | Refactor | Magic row numbers → named constants |
| `tests/testthat.R` | New | Test entry point |
| `tests/testthat/test-utilities.R` | New | null_coalesce tests |
| `tests/testthat/test-pca-analysis.R` | New | Stub, duplicate, Excel scoping tests |
| `tests/testthat/test-data-handling.R` | New | MEA constants test |
| `Example/01_compute.R` | New | Discovery + variable assignment + full computation |
| `Example/02_plot.R` | New | Load + tune + show + save organized figures |

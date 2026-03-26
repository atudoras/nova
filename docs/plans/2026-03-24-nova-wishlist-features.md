# NOVA Wishlist Features Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task.

**Goal:** Add five quality-of-life improvements: smart CSV row detection, raw-data heatmaps, per-metric bar/box/violin plots, heatmap filter/split arguments, and a no-code quickstart script + user guide with real screenshots.

**Architecture:** Batch A (Tasks 1-2) fixes fragile parsing and missing raw-data support in existing functions. Batch B (Tasks 3-4) adds new analytical capabilities. Task 5 is a user-facing entry point script. Task 6 is documentation. All new behaviour is test-driven; all new parameters are additive (no breaking changes).

**Tech Stack:** R, ggplot2, pheatmap, devtools/testthat, rmarkdown

---

## Background: MEA CSV File Format

Axion BioSystems MEA CSVs have a fixed structure where:
- **Col 1** of rows 121-124 contains the *labels*: the first data column is unlabeled (empty), then row 121 col 1 = blank (well-IDs are across the row), row 122 col 1 = "Treatment", row 123 col 1 = "Genotype", row 124 col 1 = "Exclude"
- The current parser hard-codes row numbers. If Axion software ever inserts an extra summary row, or the user has a slightly different export, everything silently shifts.
- The robust fix: scan column 1 downward from row 100 looking for the string "Treatment". Anchor all other rows *relative* to that discovered row.

---

## Task 1 (Batch A): Smart metadata-row detection in `data_handling.R`

**Files:**
- Modify: `R/data_handling.R` — add `find_mea_metadata_row()` helper; use it in both `discover_mea_structure` (lines ~126-154) and `process_mea_flexible` (lines ~422-446)
- Test: `tests/testthat/test-data-handling.R`

### Step 1 — Write the failing tests

Add to `tests/testthat/test-data-handling.R`:

```r
# ── find_mea_metadata_row ──────────────────────────────────────────────────────

test_that("find_mea_metadata_row finds Treatment at standard row 122", {
  # Build a minimal fake CSV raw table: 170 rows x 3 cols
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[122, 1] <- "Treatment"
  raw[123, 1] <- "Genotype"
  raw[124, 1] <- "Exclude"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 122L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Genotype"),  123L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Exclude"),   124L)
})

test_that("find_mea_metadata_row finds Treatment when shifted to row 123", {
  # Simulate the 'extra empty row' problem the user reported
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[123, 1] <- "Treatment"   # shifted by 1
  raw[124, 1] <- "Genotype"
  raw[125, 1] <- "Exclude"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 123L)
})

test_that("find_mea_metadata_row falls back to constant when label absent", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  # No "Treatment" label anywhere

  result <- NOVA:::find_mea_metadata_row(raw, "Treatment", fallback = MEA_ROW_TREATMENT)
  expect_equal(result, MEA_ROW_TREATMENT)  # 122L
})

test_that("find_mea_metadata_row is case-insensitive", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[122, 1] <- "treatment"   # lowercase

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 122L)
})
```

### Step 2 — Run tests to confirm they fail

```bash
cd "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features"
Rscript -e "devtools::test(filter='data-handling')" 2>&1 | tail -12
```
Expected: `FAIL 4` — `find_mea_metadata_row` does not exist yet.

### Step 3 — Add `find_mea_metadata_row` helper to `data_handling.R`

Insert after the constants block (line 11), before line 13 (`#' Discover MEA Data Structure`):

```r
# ---------------------------------------------------------------------------
# Internal helper: find a metadata row by scanning column 1 for a label
# ---------------------------------------------------------------------------
# Returns the integer row index where `label` appears in column 1 of `raw`
# (a data.frame or matrix). Searches rows `search_from` through `nrow(raw)`.
# Case-insensitive exact match. If not found, returns `fallback`.
find_mea_metadata_row <- function(raw, label, search_from = 100L, fallback = NULL) {
  col1 <- trimws(as.character(unlist(raw[seq(search_from, nrow(raw)), 1])))
  hit  <- which(tolower(col1) == tolower(label))
  if (length(hit) > 0L) return(as.integer(search_from + hit[1L] - 1L))
  fallback
}
```

### Step 4 — Wire the helper into `discover_mea_structure` (around line 126)

Replace the static assignments:
```r
# BEFORE (lines ~131-134):
well_row      <- MEA_ROW_WELLS
treatment_row <- MEA_ROW_TREATMENT
genotype_row  <- MEA_ROW_GENOTYPE
exclude_row   <- MEA_ROW_EXCLUDE
```
With:
```r
# AFTER — detect from labels; fall back to constants if not found
treatment_row <- find_mea_metadata_row(raw_data, "Treatment", fallback = MEA_ROW_TREATMENT)
genotype_row  <- find_mea_metadata_row(raw_data, "Genotype",  fallback = MEA_ROW_GENOTYPE)
exclude_row   <- find_mea_metadata_row(raw_data, "Exclude",   fallback = MEA_ROW_EXCLUDE)
well_row      <- treatment_row - 1L   # Wells row is always directly above Treatment
vars_start    <- exclude_row   + 1L   # Variables start directly after Exclude
vars_end      <- min(nrow(raw_data), exclude_row + 45L)  # 44 variable rows max
```

Also update the variables extraction block (~line 153):
```r
# BEFORE:
if (nrow(raw_data) >= MEA_ROW_VARS_END) {
  variables <- unlist(raw_data[MEA_ROW_VARS_START:MEA_ROW_VARS_END, 1])

# AFTER:
if (nrow(raw_data) >= vars_end) {
  variables <- unlist(raw_data[vars_start:vars_end, 1])
```

### Step 5 — Wire the helper into `process_mea_flexible` (around line 422)

Replace the static row usage in the `tryCatch` file-reading block:
```r
# BEFORE (lines ~432-435):
well_ids   <- unlist(raw[MEA_ROW_WELLS, -1])
treatments <- unlist(raw[MEA_ROW_TREATMENT, -1])
genotypes  <- unlist(raw[MEA_ROW_GENOTYPE, -1])
exclude    <- unlist(raw[MEA_ROW_EXCLUDE, -1])

# BEFORE (lines ~445-446):
variable_names <- unlist(raw[MEA_ROW_VARS_START:MEA_ROW_VARS_END, 1])
values_matrix  <- raw[MEA_ROW_VARS_START:MEA_ROW_VARS_END, -1]
```
With:
```r
# AFTER — detect row positions from labels
treatment_row  <- find_mea_metadata_row(raw, "Treatment", fallback = MEA_ROW_TREATMENT)
genotype_row   <- find_mea_metadata_row(raw, "Genotype",  fallback = MEA_ROW_GENOTYPE)
exclude_row    <- find_mea_metadata_row(raw, "Exclude",   fallback = MEA_ROW_EXCLUDE)
well_row       <- treatment_row - 1L
vars_start     <- exclude_row   + 1L
vars_end       <- min(nrow(raw), exclude_row + 45L)

well_ids   <- unlist(raw[well_row,       -1])
treatments <- unlist(raw[treatment_row,  -1])
genotypes  <- unlist(raw[genotype_row,   -1])
exclude    <- unlist(raw[exclude_row,    -1])

variable_names <- unlist(raw[vars_start:vars_end, 1])
values_matrix  <- raw[vars_start:vars_end, -1]
```

Also update the row-count guard (line ~426) to use the dynamic `vars_end`:
```r
# BEFORE:
if (nrow(raw) < MEA_ROW_VARS_END) {
  warning("File ", filename, " has insufficient rows (", nrow(raw), " < 168)")
  next
}

# AFTER:
if (nrow(raw) < MEA_MIN_ROWS) {
  warning("File ", filename, " has insufficient rows (", nrow(raw), " < ", MEA_MIN_ROWS, ")")
  next
}
```

### Step 6 — Run tests to confirm they pass

```bash
Rscript -e "devtools::test(filter='data-handling')" 2>&1 | tail -8
```
Expected: `FAIL 0 | PASS 10` (6 old + 4 new).

### Step 7 — Commit

```bash
git add R/data_handling.R tests/testthat/test-data-handling.R
git commit -m "feat: smart MEA row detection by label scan, not hardcoded position

find_mea_metadata_row() anchors on 'Treatment' in col 1 and derives
all other metadata rows relative to it. Falls back to constants when
label is absent. Fixes breakage when source CSV has an extra blank row.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Task 2 (Batch A): Raw-data support in `create_mea_heatmaps_enhanced`

**Context:** `create_mea_heatmaps_enhanced` already silently switches to `value_column = "Value"` when `processing_result$normalized_data` is NULL. However:
1. The function signature defaults to `value_column = "Normalized_Value"`, meaning passing a raw `data=` frame directly fails.
2. There is no explicit `use_raw` escape hatch for experiments without a baseline.
3. Plot titles/axis labels still say "Normalized" even when raw data is displayed.

**Files:**
- Modify: `R/plots.R` — `create_mea_heatmaps_enhanced` signature + title logic (~lines 1453, 1525-1548)
- Test: `tests/testthat/test-heatmaps.R` (new file)

### Step 1 — Write the failing tests

Create `tests/testthat/test-heatmaps.R`:

```r
library(testthat)
library(NOVA)

# Helper: minimal long-format data frame mimicking process_mea_flexible output
make_raw_data <- function() {
  data.frame(
    Well       = rep(c("A1", "B1"), each = 4),
    Treatment  = rep(c("Control", "Drug"), each = 4),
    Genotype   = "WT",
    Timepoint  = "baseline",
    Variable   = rep(c("Mean Firing Rate (Hz)", "Burst Rate (Hz)"), 4),
    Value      = runif(8, 0, 5),
    stringsAsFactors = FALSE
  )
}

test_that("create_mea_heatmaps_enhanced accepts raw data frame with Value column", {
  df <- make_raw_data()
  expect_no_error(
    create_mea_heatmaps_enhanced(
      data         = df,
      value_column = "Value",
      verbose      = FALSE,
      save_plots   = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced use_raw=TRUE auto-switches value_column", {
  # Simulate a processing_result that only has raw_data
  pr <- list(
    raw_data        = make_raw_data(),
    normalized_data = NULL,
    config_used     = NULL
  )
  expect_no_error(
    create_mea_heatmaps_enhanced(
      processing_result = pr,
      use_raw           = TRUE,
      verbose           = FALSE,
      save_plots        = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced title says 'Raw' not 'Normalized' when use_raw=TRUE", {
  pr <- list(
    raw_data        = make_raw_data(),
    normalized_data = NULL,
    config_used     = NULL
  )
  result <- create_mea_heatmaps_enhanced(
    processing_result = pr,
    use_raw           = TRUE,
    verbose           = FALSE,
    save_plots        = FALSE
  )
  # The value_column in result metadata should not be "Normalized_Value"
  expect_false(isTRUE(result$metadata$value_column == "Normalized_Value"))
})
```

### Step 2 — Run tests to confirm they fail

```bash
Rscript -e "devtools::test(filter='heatmaps')" 2>&1 | tail -8
```
Expected: `FAIL 3` — `use_raw` parameter does not exist yet.

### Step 3 — Implement `use_raw` parameter in `create_mea_heatmaps_enhanced`

**3a.** Add `use_raw = FALSE` to the function signature (after line 1484):
```r
create_mea_heatmaps_enhanced <- function(
    ...
    min_observations = 3,
    use_raw = FALSE          # <-- ADD THIS
) {
```

**3b.** In the DATA INPUT HANDLING block (around line 1525), extend the `processing_result` branch:
```r
# BEFORE (lines 1525-1534):
if (!is.null(processing_result$normalized_data)) {
  data <- processing_result$normalized_data
  if (verbose) cat("Using normalized data\n")
} else if (!is.null(processing_result$raw_data)) {
  data <- processing_result$raw_data
  value_column <- "Value"
  if (verbose) cat("Using raw data (no normalization found)\n")
} else {
  stop("Processing result does not contain usable data")
}

# AFTER:
if (use_raw) {
  # Explicit opt-in to raw data (e.g. developmental experiments with no baseline)
  if (!is.null(processing_result$raw_data)) {
    data         <- processing_result$raw_data
    value_column <- "Value"
    if (verbose) cat("Using raw data (use_raw = TRUE)\n")
  } else if (!is.null(processing_result$normalized_data)) {
    data <- processing_result$normalized_data   # fall forward if only normalized exists
    if (verbose) cat("use_raw=TRUE but only normalized data found; using normalized\n")
  } else {
    stop("Processing result does not contain usable data")
  }
} else {
  if (!is.null(processing_result$normalized_data)) {
    data <- processing_result$normalized_data
    if (verbose) cat("Using normalized data\n")
  } else if (!is.null(processing_result$raw_data)) {
    data         <- processing_result$raw_data
    value_column <- "Value"
    if (verbose) cat("Using raw data (normalized_data absent)\n")
  } else {
    stop("Processing result does not contain usable data")
  }
}
data_label <- if (value_column == "Value") "Raw Value" else "Normalized Value"
```

**3c.** When building heatmap titles (search for `"Z-score"` or `"Normalized"` in `create_mea_heatmaps_enhanced`): append `data_label` where appropriate so titles read "MEA Variables (Raw Value, Z-score)" vs "MEA Variables (Normalized Value, Z-score)".

### Step 4 — Run tests to confirm they pass

```bash
Rscript -e "devtools::test(filter='heatmaps')" 2>&1 | tail -8
```
Expected: `FAIL 0 | PASS 3`.

### Step 5 — Commit

```bash
git add R/plots.R tests/testthat/test-heatmaps.R
git commit -m "feat: add use_raw param to create_mea_heatmaps_enhanced

Experiments without a baseline (e.g. developmental) can now pass
use_raw=TRUE to plot raw MEA values instead of normalized values.
Title/label reflects data type. Falls back gracefully when only one
data type is available.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Task 3 (Batch B): `plot_mea_metric()` — bar/box/violin plots per metric

**Context:** Users currently export `all_data` to CSV and plot in Prism. This function covers the full workflow inside R: filter to one metric → aggregate → ggplot2. Supports bar+error, box, violin, and line-over-time.

**Files:**
- Create: `R/metric_plots.R`
- Test: `tests/testthat/test-metric-plots.R`
- Modify: `NAMESPACE` (auto via devtools), `R/NOVA-package.R` if it exists

### Step 1 — Write the failing tests

Create `tests/testthat/test-metric-plots.R`:

```r
library(testthat)
library(NOVA)

# Minimal processed data (long format, as returned by process_mea_flexible)
make_processed <- function() {
  set.seed(42)
  expand.grid(
    Well      = c("A1", "A2", "B1", "B2"),
    Timepoint = c("baseline", "1h", "2h"),
    Variable  = c("Mean Firing Rate (Hz)", "Burst Rate (Hz)"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      Treatment         = ifelse(Well %in% c("A1","A2"), "PBS", "KA"),
      Genotype          = ifelse(Well %in% c("A1","B1"), "WT",  "KO"),
      Value             = runif(dplyr::n(), 0, 10),
      Normalized_Value  = runif(dplyr::n(), 0.5, 2)
    )
}

test_that("plot_mea_metric returns a ggplot object", {
  df <- make_processed()
  p  <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)")
  expect_s3_class(p, "gg")
})

test_that("plot_mea_metric errors informatively for unknown metric", {
  df <- make_processed()
  expect_error(
    plot_mea_metric(df, metric = "Not A Real Metric"),
    regexp = "not found"
  )
})

test_that("plot_mea_metric respects filter_treatments", {
  df  <- make_processed()
  p   <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)",
                          filter_treatments = "PBS")
  # Extract data used by ggplot
  pd  <- ggplot2::ggplot_build(p)$data[[1]]
  # Only PBS wells: A1, A2 -> 2 wells x 3 timepoints
  expect_lte(nrow(pd), 6)
})

test_that("plot_mea_metric facet_by creates faceted plot", {
  df <- make_processed()
  p  <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)",
                         facet_by = "Genotype")
  expect_true(!is.null(p$facet))
})

test_that("plot_mea_metric plot_type='box' works", {
  df <- make_processed()
  expect_no_error(
    plot_mea_metric(df, metric = "Mean Firing Rate (Hz)", plot_type = "box")
  )
})
```

### Step 2 — Run tests to confirm they fail

```bash
Rscript -e "devtools::test(filter='metric-plots')" 2>&1 | tail -8
```
Expected: `FAIL 5` — function does not exist yet.

### Step 3 — Implement `plot_mea_metric()`

Create `R/metric_plots.R`:

```r
# metric_plots.R
# Per-metric bar, box, violin, and line plots for MEA data

#' Plot a Single MEA Metric Across Conditions
#'
#' Creates a bar (mean + error), box, violin, or line plot for one measured
#' variable from processed MEA data. Replaces the manual Prism workflow for
#' exploring individual metrics.
#'
#' @param data Data frame — long-format MEA data as returned by
#'   \code{process_mea_flexible()} (must contain columns \code{Variable} and
#'   at least one of \code{Value} / \code{Normalized_Value}).
#' @param metric Character. Exact name of the variable to plot (must match a
#'   value in the \code{Variable} column).
#' @param x_var Character. Column to use as the x-axis (default \code{"Timepoint"}).
#' @param group_by Character. Column to use for fill/colour grouping
#'   (default \code{"Treatment"}).
#' @param facet_by Character or NULL. Column name for faceting (e.g.
#'   \code{"Genotype"}). NULL = no facets.
#' @param filter_timepoints Character vector or NULL. Subset to these timepoints.
#' @param filter_treatments Character vector or NULL. Subset to these treatments.
#' @param filter_genotypes  Character vector or NULL. Subset to these genotypes.
#' @param value_column Character. Which column holds the numeric values.
#'   Defaults to \code{"Normalized_Value"} if present, else \code{"Value"}.
#' @param error_type Character. Error bar type: \code{"sem"} (default),
#'   \code{"sd"}, or \code{"ci95"}.
#' @param plot_type Character. \code{"bar"} (default), \code{"box"},
#'   \code{"violin"}, or \code{"line"}.
#' @param colors Named character vector of colours, or NULL for ggplot2 defaults.
#' @param show_points Logical. Overlay individual data points (default TRUE).
#' @param point_alpha Numeric. Transparency of data points (default 0.6).
#' @param title Character or NULL. Plot title. NULL = metric name.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Bar plot: Mean Firing Rate by Treatment over time
#' plot_mea_metric(processed$all_data, "Mean Firing Rate (Hz)")
#'
#' # Violin split by genotype
#' plot_mea_metric(processed$all_data, "Burst Rate (Hz)",
#'                 plot_type = "violin", facet_by = "Genotype")
#'
#' # Only PBS and KA, only baseline and 1h
#' plot_mea_metric(processed$all_data, "Mean Firing Rate (Hz)",
#'                 filter_treatments = c("PBS", "KA"),
#'                 filter_timepoints = c("baseline", "1h"))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_boxplot geom_violin geom_line
#'   geom_point geom_errorbar stat_summary facet_wrap labs theme_bw theme
#'   scale_fill_manual scale_colour_manual element_text position_dodge
#' @importFrom dplyr filter group_by summarise mutate n
#' @export
plot_mea_metric <- function(
    data,
    metric,
    x_var             = "Timepoint",
    group_by          = "Treatment",
    facet_by          = NULL,
    filter_timepoints = NULL,
    filter_treatments = NULL,
    filter_genotypes  = NULL,
    value_column      = NULL,
    error_type        = c("sem", "sd", "ci95"),
    plot_type         = c("bar", "box", "violin", "line"),
    colors            = NULL,
    show_points       = TRUE,
    point_alpha       = 0.6,
    title             = NULL
) {
  error_type <- match.arg(error_type)
  plot_type  <- match.arg(plot_type)

  # ── resolve value column ────────────────────────────────────────────────────
  if (is.null(value_column)) {
    value_column <- if ("Normalized_Value" %in% names(data)) "Normalized_Value" else "Value"
  }
  y_label <- if (value_column == "Normalized_Value") "Normalized Value" else "Value"

  # ── validate metric ─────────────────────────────────────────────────────────
  if (!"Variable" %in% names(data)) stop("'data' must contain a 'Variable' column")
  if (!metric %in% data$Variable) {
    avail <- paste(head(unique(data$Variable), 6), collapse = ", ")
    stop("Metric '", metric, "' not found in data$Variable. Available (first 6): ", avail)
  }

  # ── filter ──────────────────────────────────────────────────────────────────
  d <- data[data$Variable == metric, , drop = FALSE]
  if (!is.null(filter_timepoints) && "Timepoint" %in% names(d))
    d <- d[d$Timepoint %in% filter_timepoints, , drop = FALSE]
  if (!is.null(filter_treatments) && "Treatment" %in% names(d))
    d <- d[d$Treatment %in% filter_treatments, , drop = FALSE]
  if (!is.null(filter_genotypes)  && "Genotype"  %in% names(d))
    d <- d[d$Genotype  %in% filter_genotypes,  , drop = FALSE]
  if (nrow(d) == 0) stop("No data remaining after applying filters")

  # Rename value_column to .value for clean aes() mapping
  d$.value <- as.numeric(d[[value_column]])

  # ── compute error bars (for bar + line) ─────────────────────────────────────
  sem_fn <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  ci95_fn <- function(x) {
    qt(0.975, df = max(1, sum(!is.na(x)) - 1)) * sem_fn(x)
  }
  err_fn <- switch(error_type,
    sem  = sem_fn,
    sd   = function(x) sd(x, na.rm = TRUE),
    ci95 = ci95_fn
  )

  group_vars <- unique(c(x_var, group_by, facet_by))
  group_vars <- group_vars[group_vars %in% names(d)]

  summ <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      mean_val = mean(.value, na.rm = TRUE),
      err      = err_fn(.value),
      n        = sum(!is.na(.value)),
      .groups  = "drop"
    )

  # ── base aes mapping ────────────────────────────────────────────────────────
  dodge <- ggplot2::position_dodge(width = 0.8)

  # ── build plot ──────────────────────────────────────────────────────────────
  p <- ggplot2::ggplot()

  if (plot_type == "bar") {
    p <- p +
      ggplot2::geom_bar(
        data     = summ,
        ggplot2::aes(x = .data[[x_var]], y = mean_val, fill = .data[[group_by]]),
        stat     = "identity", position = dodge, alpha = 0.85
      ) +
      ggplot2::geom_errorbar(
        data  = summ,
        ggplot2::aes(x = .data[[x_var]],
                     ymin = mean_val - err, ymax = mean_val + err,
                     group = .data[[group_by]]),
        width = 0.2, position = dodge
      )

  } else if (plot_type == "line") {
    p <- p +
      ggplot2::geom_line(
        data = summ,
        ggplot2::aes(x = .data[[x_var]], y = mean_val,
                     colour = .data[[group_by]], group = .data[[group_by]])
      ) +
      ggplot2::geom_errorbar(
        data  = summ,
        ggplot2::aes(x = .data[[x_var]],
                     ymin = mean_val - err, ymax = mean_val + err,
                     colour = .data[[group_by]], group = .data[[group_by]]),
        width = 0.15
      )

  } else if (plot_type == "box") {
    p <- p +
      ggplot2::geom_boxplot(
        data  = d,
        ggplot2::aes(x = .data[[x_var]], y = .value, fill = .data[[group_by]]),
        position = dodge, outlier.shape = NA, alpha = 0.75
      )

  } else if (plot_type == "violin") {
    p <- p +
      ggplot2::geom_violin(
        data  = d,
        ggplot2::aes(x = .data[[x_var]], y = .value, fill = .data[[group_by]]),
        position = dodge, alpha = 0.75, trim = FALSE
      )
  }

  # ── overlay individual points ────────────────────────────────────────────────
  if (show_points && plot_type %in% c("bar", "line", "violin")) {
    p <- p +
      ggplot2::geom_point(
        data     = d,
        ggplot2::aes(x = .data[[x_var]], y = .value,
                     colour = .data[[group_by]], group = .data[[group_by]]),
        position = ggplot2::position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1),
        alpha    = point_alpha, size = 2
      )
  }

  # ── facet ────────────────────────────────────────────────────────────────────
  if (!is.null(facet_by) && facet_by %in% names(d)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_by)))
  }

  # ── colours ──────────────────────────────────────────────────────────────────
  if (!is.null(colors)) {
    p <- p +
      ggplot2::scale_fill_manual(values   = colors) +
      ggplot2::scale_colour_manual(values = colors)
  }

  # ── labels and theme ─────────────────────────────────────────────────────────
  p <- p +
    ggplot2::labs(
      title = if (is.null(title)) metric else title,
      x     = x_var,
      y     = y_label,
      fill  = group_by,
      colour = group_by
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  p
}
```

### Step 4 — Run tests to confirm they pass

```bash
Rscript -e "devtools::test(filter='metric-plots')" 2>&1 | tail -8
```
Expected: `FAIL 0 | PASS 5`.

### Step 5 — Commit

```bash
git add R/metric_plots.R tests/testthat/test-metric-plots.R
git commit -m "feat: add plot_mea_metric() for per-variable bar/box/violin/line plots

New exported function replaces manual Prism workflow. Accepts long-format
processed data, filters to a single metric, aggregates with SEM/SD/CI95,
and returns a ggplot with optional faceting and individual point overlay.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Task 4 (Batch B): Filter/split arguments for `create_mea_heatmaps_enhanced`

**Context:** The user asked for Seurat-style `group.by` / `split.by` arguments on heatmaps. We add:
- `filter_timepoints`, `filter_treatments`, `filter_genotypes` — display-only row subsetting
- `split_by` — creates one heatmap per unique level of the specified column

**Files:**
- Modify: `R/plots.R` — `create_mea_heatmaps_enhanced` signature + filter/split block
- Test: `tests/testthat/test-heatmaps.R` (extend existing file)

### Step 1 — Add tests to `test-heatmaps.R`

```r
test_that("create_mea_heatmaps_enhanced filter_treatments subsets data", {
  df <- data.frame(
    Well      = rep(c("A1","B1"), each = 2),
    Treatment = rep(c("PBS","KA"), each = 2),
    Genotype  = "WT",
    Timepoint = "baseline",
    Variable  = rep(c("Firing Rate","Burst Rate"), 2),
    Value     = runif(8),
    stringsAsFactors = FALSE
  )
  # Should not error and should only use PBS data
  expect_no_error(
    create_mea_heatmaps_enhanced(
      data                = df,
      value_column        = "Value",
      filter_treatments   = "PBS",
      verbose             = FALSE,
      save_plots          = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced split_by returns one result per level", {
  df <- data.frame(
    Well      = rep(c("A1","A2","B1","B2"), each = 2),
    Treatment = rep(c("PBS","PBS","KA","KA"), each = 2),
    Genotype  = rep(c("WT","KO","WT","KO"), each = 2),
    Timepoint = "baseline",
    Variable  = rep(c("Firing Rate","Burst Rate"), 4),
    Value     = runif(8),
    stringsAsFactors = FALSE
  )
  result <- create_mea_heatmaps_enhanced(
    data         = df,
    value_column = "Value",
    split_by     = "Genotype",
    verbose      = FALSE,
    save_plots   = FALSE
  )
  expect_true("split_results" %in% names(result))
  expect_equal(length(result$split_results), 2)  # WT and KO
  expect_true(all(c("WT","KO") %in% names(result$split_results)))
})
```

### Step 2 — Run tests to confirm they fail

```bash
Rscript -e "devtools::test(filter='heatmaps')" 2>&1 | tail -8
```
Expected: `FAIL 2` — new parameters don't exist yet.

### Step 3 — Add parameters to `create_mea_heatmaps_enhanced`

**3a.** Add to function signature (after `use_raw = FALSE`):
```r
    filter_timepoints  = NULL,   # e.g. c("baseline", "1h")
    filter_treatments  = NULL,   # e.g. c("PBS", "KA")
    filter_genotypes   = NULL,   # e.g. c("WT")
    split_by           = NULL    # e.g. "Genotype" -- one heatmap per level
```

**3b.** After the DATA INPUT HANDLING block and required-column validation (around line 1549), add a filtering + split block:

```r
  # ── display-only filters ──────────────────────────────────────────────────
  # These narrow what gets shown without mutating the source data.
  if (!is.null(filter_timepoints) && timepoint_column %in% names(data)) {
    data <- data[data[[timepoint_column]] %in% filter_timepoints, , drop = FALSE]
    if (verbose) cat("Filtered to timepoints:", paste(filter_timepoints, collapse=", "), "\n")
  }
  if (!is.null(filter_treatments) && "Treatment" %in% names(data)) {
    data <- data[data$Treatment %in% filter_treatments, , drop = FALSE]
    if (verbose) cat("Filtered to treatments:", paste(filter_treatments, collapse=", "), "\n")
  }
  if (!is.null(filter_genotypes) && "Genotype" %in% names(data)) {
    data <- data[data$Genotype %in% filter_genotypes, , drop = FALSE]
    if (verbose) cat("Filtered to genotypes:", paste(filter_genotypes, collapse=", "), "\n")
  }
  if (nrow(data) == 0) stop("No data remaining after applying filters.")

  # ── split_by: run once per level, return list ─────────────────────────────
  if (!is.null(split_by) && split_by %in% names(data)) {
    levels_to_split <- sort(unique(data[[split_by]]))
    if (verbose) cat("split_by =", split_by, "->", length(levels_to_split), "groups\n")
    split_results <- lapply(stats::setNames(levels_to_split, levels_to_split), function(lvl) {
      sub_data <- data[data[[split_by]] == lvl, , drop = FALSE]
      create_mea_heatmaps_enhanced(
        data               = sub_data,
        value_column       = value_column,
        variable_column    = variable_column,
        grouping_columns   = grouping_columns,
        sample_id_columns  = sample_id_columns,
        timepoint_column   = timepoint_column,
        scale_method       = scale_method,
        aggregation_method = aggregation_method,
        cluster_rows       = cluster_rows,
        cluster_cols       = cluster_cols,
        create_individual_heatmaps   = create_individual_heatmaps,
        create_combined_heatmap      = create_combined_heatmap,
        create_variable_correlation  = create_variable_correlation,
        save_plots         = save_plots,
        output_dir         = if (!is.null(output_dir)) file.path(output_dir, lvl) else NULL,
        verbose            = FALSE,
        return_data        = return_data
      )
    })
    return(list(split_by = split_by, split_results = split_results))
  }
  # (rest of existing function continues unchanged after this block)
```

### Step 4 — Run tests to confirm they pass

```bash
Rscript -e "devtools::test(filter='heatmaps')" 2>&1 | tail -8
```
Expected: `FAIL 0 | PASS 5`.

### Step 5 — Commit

```bash
git add R/plots.R tests/testthat/test-heatmaps.R
git commit -m "feat: add filter/split arguments to create_mea_heatmaps_enhanced

filter_timepoints/treatments/genotypes narrow displayed data (Seurat-style).
split_by runs one heatmap per level and returns a named list — e.g.
split_by='Genotype' yields list(WT=..., KO=...).
No breaking changes; all new params default to NULL.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Task 5: `nova_quickstart.R` — single-file entry point for non-coders

**Goal:** A user with zero coding experience opens this one file, sets `DATA_DIR` on line 1, and runs the whole thing. Every other decision has a clear default with comments explaining the trade-offs.

**Files:**
- Create: `Example/nova_quickstart.R`

### Step 1 — Create the script

No tests for this task (it's a user-facing template, not library code). Write and inspect it:

```r
# =============================================================================
# NOVA Quickstart Script
# =============================================================================
# HOW TO USE:
#   1. Set DATA_DIR below to the folder that contains your MEA experiment
#      folders (each folder should be named like "MEA001", "MEA022b", etc.)
#   2. Run the entire script (Ctrl+A → Run, or source())
#   3. Figures are saved in DATA_DIR/nova_output/ and shown in the Viewer
# =============================================================================

# ── STEP 1: Set your data folder ─────────────────────────────────────────────
DATA_DIR <- "path/to/your/MEA/data"   # <<< CHANGE THIS

# ── STEP 2: Optional — name what your columns represent ──────────────────────
# If your CSV rows are labelled differently, change these strings to match.
# Leave as NULL if you don't have that metadata column.
TREATMENT_COLUMN <- "Treatment"   # or NULL
GENOTYPE_COLUMN  <- "Genotype"    # or NULL

# ── STEP 3: Optional — narrow what gets plotted ───────────────────────────────
# Leave as NULL to include everything; fill in to filter.
# Examples:
#   SHOW_TREATMENTS  <- c("PBS", "KA")
#   SHOW_GENOTYPES   <- c("Mavs fl/fl")
#   SHOW_TIMEPOINTS  <- c("baseline", "1h", "2h")
SHOW_TREATMENTS  <- NULL
SHOW_GENOTYPES   <- NULL
SHOW_TIMEPOINTS  <- NULL

# ── STEP 4: Figure appearance ────────────────────────────────────────────────
FIGURE_WIDTH  <- 12   # inches
FIGURE_HEIGHT <- 10   # inches
DPI           <- 300  # 300 for publication, 150 for quick preview

# =============================================================================
# (Everything below runs automatically — no changes needed)
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("NOVA", quietly = TRUE)) {
    message("Installing NOVA from local source...")
    devtools::install(file.path(dirname(rstudioapi::getSourceEditorContext()$path), ".."),
                      quiet = TRUE)
  }
  library(NOVA)
})

OUT_DIR <- file.path(DATA_DIR, "nova_output")
dir.create(file.path(OUT_DIR, "pca"),         recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "heatmaps"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "trajectories"),recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "metrics"),     recursive = TRUE, showWarnings = FALSE)

message("\n=== Step 1/4: Discovering data structure ===")
discovery <- discover_mea_structure(DATA_DIR)
cat("Found", discovery$experiment_count, "experiment(s)\n")
cat("Timepoints:", paste(discovery$all_timepoints, collapse=", "), "\n")

# Infer baseline: first timepoint containing 'baseline' or '0' or 'pre'
baseline_guess <- discovery$potential_baselines[1]
if (is.na(baseline_guess)) {
  cat("No clear baseline found — using raw (un-normalized) data.\n")
}

# Build grouping columns from what's available
grouping_cols <- c(TREATMENT_COLUMN, GENOTYPE_COLUMN)
grouping_cols <- grouping_cols[!is.null(grouping_cols)]

message("\n=== Step 2/4: Processing data ===")
processed <- process_mea_flexible(
  main_dir             = DATA_DIR,
  grouping_variables   = grouping_cols,
  timepoints_order     = discovery$all_timepoints,
  baseline_timepoint   = baseline_guess,
  verbose              = TRUE
)

message("\n=== Step 3/4: Computing PCA, trajectories, heatmaps ===")
use_norm <- !is.na(baseline_guess) && !is.null(processed$normalized_data)
data_for_analysis <- if (use_norm) processed$normalized_data else processed$raw_data
val_col           <- if (use_norm) "Normalized_Value" else "Value"

pca_results  <- pca_analysis_enhanced(data_for_analysis,
                                       grouping_variables = grouping_cols,
                                       value_column       = val_col,
                                       verbose            = FALSE)

trajectories <- create_mea_trajectories(data_for_analysis,
                                         grouping_cols    = grouping_cols,
                                         value_column     = val_col,
                                         verbose          = FALSE)

heatmaps     <- create_mea_heatmaps_enhanced(
                  processing_result  = processed,
                  use_raw            = !use_norm,
                  filter_timepoints  = SHOW_TIMEPOINTS,
                  filter_treatments  = SHOW_TREATMENTS,
                  filter_genotypes   = SHOW_GENOTYPES,
                  save_plots         = FALSE,
                  verbose            = FALSE)

message("\n=== Step 4/4: Saving figures to ", OUT_DIR, " ===")

save_plot <- function(p, path, w = FIGURE_WIDTH, h = FIGURE_HEIGHT) {
  ggplot2::ggsave(path, plot = p, width = w, height = h, dpi = DPI)
  print(p)    # also show in Viewer
  invisible(p)
}

# PCA
if (!is.null(pca_results$plots$scatter)) {
  save_plot(pca_results$plots$scatter, file.path(OUT_DIR, "pca", "pca_scatter.pdf"))
}
if (!is.null(pca_results$plots$elbow)) {
  save_plot(pca_results$plots$elbow,   file.path(OUT_DIR, "pca", "pca_elbow.pdf"))
}

# Trajectories
for (nm in names(trajectories$plots)) {
  save_plot(trajectories$plots[[nm]], file.path(OUT_DIR, "trajectories", paste0(nm, ".pdf")))
}

# Heatmaps
if (!is.null(heatmaps) && is.list(heatmaps)) {
  hm_plots <- heatmaps[sapply(heatmaps, inherits, what = "pheatmap")]
  for (nm in names(hm_plots)) {
    pdf(file.path(OUT_DIR, "heatmaps", paste0(nm, ".pdf")),
        width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
    print(hm_plots[[nm]])
    dev.off()
  }
}

message("\nDone! All figures saved to: ", OUT_DIR)
```

### Step 2 — Smoke-test that it parses without error

```bash
Rscript -e "parse(file='Example/nova_quickstart.R'); cat('Parse OK\n')"
```
Expected: `Parse OK`.

### Step 3 — Commit

```bash
git add Example/nova_quickstart.R
git commit -m "feat: add nova_quickstart.R single-file no-code entry point

One file for users without R experience. Set DATA_DIR, run, get figures.
Auto-detects baseline, falls back to raw data for developmental experiments,
respects filter/split options from the TUNE block.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Task 6: Implementation Guide (R Markdown + real screenshots)

**Goal:** A knittable `docs/user-guide/NOVA-User-Guide.Rmd` that documents the full workflow with screenshots from the real MEA Neuronal Agonists example run.

**Files:**
- Create: `docs/user-guide/NOVA-User-Guide.Rmd`
- Create: `docs/user-guide/figures/` — copy PNG previews from `/tmp/nova_preview/` or regenerate at 150 dpi

### Step 1 — Gather screenshots

Copy (or regenerate at 150 dpi) from the Example run:
```bash
mkdir -p "docs/user-guide/figures"
# Copy the preview PNGs we already generated
cp /tmp/nova_preview/*.png "docs/user-guide/figures/" 2>/dev/null || true
# Also copy any PDFs generated by the quickstart run if available
```

If `/tmp/nova_preview/` has been cleaned up, regenerate:
```bash
cd "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features"
Rscript scripts/regenerate_guide_figures.R   # created in sub-step below
```

### Step 2 — Create figure-regeneration helper script

Create `scripts/regenerate_guide_figures.R`:
```r
# Regenerates all user-guide figures from the cached nova_results.rds
# Run once after any visual changes to the package.
suppressMessages({ devtools::load_all("."); library(ggplot2) })

OUT <- "docs/user-guide/figures"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

nova <- readRDS("/tmp/nova_results.rds")   # or adjust path

save_png <- function(p, name, w=1400, h=900, res=120) {
  png(file.path(OUT, paste0(name, ".png")), width=w, height=h, res=res)
  print(p); dev.off()
  cat("Saved:", name, "\n")
}

# PCA scatter
save_png(nova$pca$plots$scatter_treatment_genotype, "pca_primary")
save_png(nova$pca$plots$elbow,                      "pca_elbow", w=900, h=600)

# Trajectories: combined avg
save_png(nova$trajectories$plots[["combined_avg"]], "traj_combined_avg", h=800)

# Heatmap: treatment
pheatmap::pheatmap(nova$heatmaps$treatment_heatmap$scaled_data,
  annotation_col = nova$heatmaps$treatment_heatmap$annotation,
  main = "MEA Variables by Treatment (Z-score)")
dev.copy(png, file.path(OUT, "heatmap_treatment.png"), width=1200, height=900, res=120)
dev.off()

cat("\nAll figures saved to", OUT, "\n")
```

### Step 3 — Write the R Markdown guide

Create `docs/user-guide/NOVA-User-Guide.Rmd`:

````markdown
---
title: "NOVA: Multi-Electrode Array Analysis in R"
subtitle: "User Guide — with real examples from MEA Neuronal Agonists"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    highlight: tango
    self_contained: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE)
```

# Overview

NOVA is an R package for analysing **Multi-Electrode Array (MEA)** recordings.
It handles the full pipeline from raw Axion Biosystems CSVs to publication-ready figures:

1. **Discover** — scan a folder, report experiments / timepoints / treatments
2. **Process** — load CSVs, assign metadata, optionally normalise to baseline
3. **Analyse** — PCA, activity trajectories, heatmaps
4. **Plot** — flexible per-metric bar/box/violin plots, heatmap filtering

---

# Installation

```{r, eval=FALSE}
# From local source (development version)
devtools::install("path/to/NOVA")

library(NOVA)
```

---

# Quickstart (no coding required)

For users who just want figures: open `Example/nova_quickstart.R`, set `DATA_DIR`
to your data folder, and run the whole script. That's it.

```
DATA_DIR <- "~/MyMEAExperiments/MEA_NeuronalAgonists"
```

Everything else is automatic: baseline detection, normalization, PCA, trajectories,
heatmaps — all figures saved to `DATA_DIR/nova_output/`.

---

# Step-by-step workflow

## 1. Discover your data

```{r}
library(NOVA)

discovery <- discover_mea_structure(
  main_dir = "~/MyMEAExperiments/MEA_NeuronalAgonists"
)
```

**What you get back:**

```
=== DISCOVERING MEA DATA STRUCTURE ===
Found 2 experiment(s): MEA022b, MEA022c
Timepoints: baseline, 30min, 1h, 2h, 4h, 24h, 48h
Treatments: PBS, Gabazine, KA, NMDA
Genotypes:  Mavs fl/fl, Mavs ko
Variables:  22 MEA metrics (Mean Firing Rate, Burst Rate, ...)
```

Use `discovery$potential_baselines` to see which timepoint NOVA suggests for
normalisation.

---

## 2. Process your data

```{r}
processed <- process_mea_flexible(
  main_dir           = DATA_DIR,
  grouping_variables = c("Treatment", "Genotype"),
  timepoints_order   = discovery$all_timepoints,
  baseline_timepoint = "baseline"     # or NULL for developmental experiments
)
```

> **No baseline?** Set `baseline_timepoint = NULL`. Heatmaps will automatically
> use raw values rather than fold-change.

---

## 3. PCA

```{r}
pca_results <- pca_analysis_enhanced(
  processed$normalized_data,
  grouping_variables = c("Treatment", "Genotype")
)
```

### PCA scatter — Treatment colour, Genotype shape

```{r, echo=FALSE, eval=TRUE}
knitr::include_graphics("figures/pca_primary.png")
```

*PC1 explains 40.3 % of variance, PC2 explains 25.3 % (224 samples from
MEA022b + MEA022c combined).*

### Elbow plot — how many PCs to retain?

```{r, echo=FALSE, eval=TRUE}
knitr::include_graphics("figures/pca_elbow.png")
```

---

## 4. Trajectories

```{r}
trajectories <- create_mea_trajectories(
  processed$normalized_data,
  grouping_cols = c("Treatment", "Genotype")
)
```

### Combined average trajectory

```{r, echo=FALSE, eval=TRUE}
knitr::include_graphics("figures/traj_combined_avg.png")
```

**Split by a different column:** simply change `grouping_cols`:

```{r}
# Split only by Treatment (ignoring Genotype)
traj_by_tx <- create_mea_trajectories(
  processed$normalized_data,
  grouping_cols = "Treatment"
)
```

---

## 5. Heatmaps

```{r}
heatmaps <- create_mea_heatmaps_enhanced(
  processing_result = processed
)
```

### MEA Variables by Treatment (Z-score)

```{r, echo=FALSE, eval=TRUE}
knitr::include_graphics("figures/heatmap_treatment.png")
```

### Filter to a subset

```{r}
# Only KA and PBS, only baseline and 2h
create_mea_heatmaps_enhanced(
  processing_result  = processed,
  filter_treatments  = c("PBS", "KA"),
  filter_timepoints  = c("baseline", "2h")
)
```

### Split by genotype (one heatmap per genotype)

```{r}
create_mea_heatmaps_enhanced(
  processing_result = processed,
  split_by          = "Genotype"
)
# Returns: list(WT = <heatmap>, KO = <heatmap>)
```

---

## 6. Per-metric bar/box plots

```{r}
# Bar plot: Mean Firing Rate by Treatment over time
plot_mea_metric(
  processed$normalized_data,
  metric  = "Mean Firing Rate (Hz)",
  x_var   = "Timepoint",
  group_by = "Treatment"
)
```

```{r}
# Violin split by Genotype, only PBS and KA
plot_mea_metric(
  processed$normalized_data,
  metric            = "Burst Rate (Hz)",
  plot_type         = "violin",
  facet_by          = "Genotype",
  filter_treatments = c("PBS","KA")
)
```

---

# Customising figures

All plot functions accept `colors` (named vector), `title`, `x_var`, and the
filter arguments. The `02_plot.R` workflow script adds a `TUNE` block at the
top where you set sizes, colors, and filters once and they propagate to every
figure automatically.

---

# Common issues

| Problem | Solution |
|---------|----------|
| "File has insufficient rows" | Check that your CSV has ≥ 124 rows. NOVA now detects the metadata rows by label ("Treatment"), so a single extra blank row no longer breaks parsing. |
| Heatmap errors on developmental data | Pass `use_raw = TRUE` or `baseline_timepoint = NULL`. |
| Wrong treatments shown | Pass `filter_treatments = c("PBS","KA")` to any plot function. |

---

# Function reference

| Function | What it does |
|----------|--------------|
| `discover_mea_structure()` | Scan folder, report experiments and variables |
| `process_mea_flexible()` | Load CSVs, normalise, return long-format data |
| `pca_analysis_enhanced()` | PCA with scatter, elbow, loading plots |
| `create_mea_trajectories()` | PCA trajectory over time by group |
| `create_mea_heatmaps_enhanced()` | Enhanced pheatmap with filter/split |
| `plot_mea_metric()` | Bar/box/violin/line for one metric |
| `analyze_pca_variable_importance_general()` | Variable contribution to PCs |

````

### Step 4 — Verify the Rmd knits

```bash
Rscript -e "rmarkdown::render('docs/user-guide/NOVA-User-Guide.Rmd', quiet=TRUE); cat('Knit OK\n')"
```
Expected: `Knit OK` and `docs/user-guide/NOVA-User-Guide.html` created.

### Step 5 — Commit

```bash
git add docs/user-guide/ scripts/regenerate_guide_figures.R
git commit -m "docs: add NOVA User Guide with real example screenshots

Covers full workflow: discover -> process -> PCA -> trajectories ->
heatmaps -> per-metric plots. Includes real PNGs from MEA Neuronal
Agonists example run. Includes filter/split and raw-data examples.
Knits to self-contained HTML.

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

---

## Final: run full test suite

```bash
cd "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features"
Rscript -e "devtools::test()" 2>&1 | tail -8
```
Expected: `FAIL 0 | WARN ≤ 2 | PASS ≥ 30`.

Then invoke `superpowers:finishing-a-development-branch` to merge / PR.

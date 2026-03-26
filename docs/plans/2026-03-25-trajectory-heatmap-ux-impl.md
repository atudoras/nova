# Trajectory & Heatmap UX Enhancements — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `color_by` trajectory parameter (colour by Treatment, label genotype at end), clearer start/end markers, timepoint subtitle, combination heatmap, and fix the Enhancement Summary & User Guide docs.

**Architecture:** All R changes are in `R/plots.R`. The `color_by` param is threaded into all three sub-plot builders inside `plot_pca_trajectories_general` (individual, averaged, combined). The heatmap `split_by = "combination"` branch mirrors the existing Genotype/Treatment branches. Docs are `.Rmd` files re-knitted to self-contained HTML after edits.

**Tech Stack:** R, ggplot2, ggrepel (new import), pheatmap, rmarkdown, testthat 3.x

---

## Task 1 — Add ggrepel to DESCRIPTION & NAMESPACE

**Files:**
- Modify: `DESCRIPTION` (line 36, after `gridExtra`)
- Modify: `R/plots.R` (line 3, existing `@importFrom` block)

### Step 1 — Edit DESCRIPTION

In `DESCRIPTION`, change:

```
    gridExtra (>= 2.3.0)
```

to:

```
    gridExtra (>= 2.3.0),
    ggrepel (>= 0.9.0)
```

### Step 2 — Add importFrom in plots.R

At line 3 of `R/plots.R`, the existing line reads:
```r
#' @importFrom dplyr filter mutate select group_by summarise arrange %>% n case_when bind_rows full_join rename distinct first last n_distinct row_number
```
Add a new line directly after it:
```r
#' @importFrom ggrepel geom_text_repel
```

### Step 3 — Regenerate NAMESPACE

```r
devtools::document()
```
Expected: NAMESPACE now contains `importFrom(ggrepel,geom_text_repel)`

### Step 4 — Verify package loads

```r
devtools::load_all()
```
Expected: no errors.

### Step 5 — Commit

```bash
git add DESCRIPTION NAMESPACE R/plots.R
git commit -m "deps: add ggrepel >= 0.9.0 import for trajectory end-labels"
```

---

## Task 2 — Trajectory: color_by parameter + start/end markers + timepoint subtitle

**Files:**
- Modify: `R/plots.R` lines 603–1400 (`plot_pca_trajectories_general`)
- Test: `tests/testthat/test-pca-analysis.R`

### Step 1 — Write failing tests

Append to `tests/testthat/test-pca-analysis.R`:

```r
# ── trajectory color_by tests ──────────────────────────────────────────────

make_mini_pca <- function() {
  # Minimal pca_results list that plot_pca_trajectories_general will accept
  plot_data <- data.frame(
    PC1       = c(-1, 0, 1, -0.8, 0.2, 1.2),
    PC2       = c(-1, 0, 1, 0.8, -0.2, -1.2),
    Timepoint = rep(c("0min", "30min", "60min"), 2),
    Treatment = rep(c("PBS", "KA"), each = 3),
    Genotype  = rep(c("WT", "KO"), each = 3),
    Experiment = rep(c("Exp1", "Exp2"), each = 3),
    stringsAsFactors = FALSE
  )
  list(plot_data = plot_data)
}

test_that("plot_pca_trajectories_general accepts color_by = 'Treatment'", {
  skip_if_not_installed("ggrepel")
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment", "Genotype"),
    color_by            = "Treatment",
    save_plots          = FALSE,
    verbose             = FALSE
  )
  expect_type(result, "list")
})

test_that("color_by = 'group' still works (backward compat)", {
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment", "Genotype"),
    color_by            = "group",
    save_plots          = FALSE,
    verbose             = FALSE
  )
  expect_type(result, "list")
})

test_that("combined averaged plot subtitle contains timepoints", {
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment", "Genotype"),
    save_plots          = FALSE,
    verbose             = FALSE
  )
  # The combined averaged plot subtitle should mention the timepoints
  comb_avg <- result$plots$combined_average
  subtitle <- comb_avg$labels$subtitle
  expect_true(!is.null(subtitle) && nchar(subtitle) > 0)
})
```

### Step 2 — Run tests to confirm they fail

```r
devtools::test(filter = "pca-analysis")
```
Expected: 3 failures — "color_by" not a recognized argument.

### Step 3 — Implement color_by in the function signature

In `R/plots.R` at line 622 (end of function signature), add `color_by = "group"`:

```r
plot_pca_trajectories_general <- function(pca_results,
                                          pc_x = "PC1",
                                          pc_y = "PC2",
                                          trajectory_grouping = NULL,
                                          timepoint_var = "Timepoint",
                                          timepoint_order = NULL,
                                          individual_var = "Experiment",
                                          point_size = 3,
                                          alpha = 0.7,
                                          line_size = 2,
                                          smooth_lines = FALSE,
                                          color_palette = NULL,
                                          color_by = "group",
                                          save_plots = FALSE,
                                          output_dir = NULL,
                                          plot_prefix = "PCA_trajectories",
                                          width = 12,
                                          height = 8,
                                          dpi = 150,
                                          return_list = TRUE,
                                          verbose = TRUE) {
```

### Step 4 — Build the colour-mapping helper

Immediately after the block that computes `unique_groups` (around line 760),
insert this helper block:

```r
  # ── color_by helper ──────────────────────────────────────────────────────
  color_by <- match.arg(color_by, c("group", "Treatment"))

  if (color_by == "Treatment" && "Treatment" %in% names(plot_data)) {
    treatment_vals  <- unique(plot_data$Treatment)
    n_treat         <- length(treatment_vals)
    treat_colors    <- if (!is.null(color_palette) && length(color_palette) >= n_treat) {
                         color_palette[seq_len(n_treat)]
                       } else {
                         colorRampPalette(c(
                           "#E31A1C","#FF7F00","#33A02C","#1F78B4",
                           "#6A3D9A","#B15928","#FB9A99","#A6CEE3"
                         ))(n_treat)
                       }
    names(treat_colors) <- treatment_vals
    # For each group_id derive its treatment colour
    group_treatment_map <- plot_data %>%
      dplyr::distinct(group_id, Treatment) %>%
      dplyr::mutate(plot_color = treat_colors[Treatment])
    active_palette <- setNames(group_treatment_map$plot_color,
                               group_treatment_map$group_id)
    # Genotype label column for end points
    plot_data <- plot_data %>%
      dplyr::left_join(
        plot_data %>% dplyr::distinct(group_id, Genotype),
        by = "group_id"
      )
  } else {
    active_palette <- color_palette %||% colorRampPalette(c(
      "#E31A1C","#FF7F00","#FDBF6F","#33A02C","#1F78B4",
      "#6A3D9A","#B15928","#FB9A99","#A6CEE3","#B2DF8A"
    ))(n_groups)
    names(active_palette) <- unique_groups
  }
  # ── timepoint subtitle ────────────────────────────────────────────────────
  tp_ordered  <- if (!is.null(timepoint_order)) timepoint_order else
                   sort(unique(plot_data[[timepoint_var]]))
  tp_subtitle <- paste0("Timepoints: ", paste(tp_ordered, collapse = " \u2192 "))
```

(`%||%` is already available via `rlang`.)

### Step 5 — Update first_last_points to use start/end shapes + genotype label

Find the block starting at line ~1233 that builds `first_last_points` for
the combined-average plot. Replace the `last_label` and `first_label` assignments
and the two `geom_point` + `geom_text` layers that follow:

```r
  first_last_points <- group_average_trajectories %>%
    dplyr::group_by(group_id) %>%
    dplyr::arrange(time_rank) %>%
    dplyr::summarise(
      first_x     = dplyr::first(avg_x),
      first_y     = dplyr::first(avg_y),
      last_x      = dplyr::last(avg_x),
      last_y      = dplyr::last(avg_y),
      last_label  = if (color_by == "Treatment" && "Genotype.y" %in% names(.))
                      dplyr::last(as.character(Genotype.y))
                    else
                      dplyr::last(as.character(.data[[timepoint_var]])),
      .groups = "drop"
    )

  # start = open diamond (shape 5), end = filled circle (shape 21)
  p_comb_avg <- p_comb_avg +
    geom_point(data = first_last_points,
               aes(x = first_x, y = first_y, color = group_id),
               shape = 5, size = point_size * 1.4, stroke = 1.4) +
    geom_point(data = first_last_points,
               aes(x = last_x, y = last_y, color = group_id),
               shape = 21, fill = "black", size = point_size * 1.2, stroke = 1.2) +
    ggrepel::geom_text_repel(
               data = first_last_points,
               aes(x = last_x, y = last_y, label = last_label, color = group_id),
               fontface = "bold", size = point_size * 0.9,
               box.padding = 0.35, point.padding = 0.3,
               show.legend = FALSE) +
    annotate("text", x = -Inf, y = Inf,
             label = "\u25c7 = start   \u25cf = end",
             hjust = -0.1, vjust = 1.4, size = 3, color = "gray40")
```

Apply the same `shape = 5` / `shape = 21` swap to the `first_last_combined`
individual-trajectory block (~line 1179).

### Step 6 — Add subtitle to labs() calls

For every `labs(title = ...)` call inside `plot_pca_trajectories_general`, append:

```r
labs(title = "...", subtitle = tp_subtitle, x = pc_x, y = pc_y)
```

There are ~5 such calls. Search for `labs(title =` within lines 950–1400 of
`plots.R` and add `, subtitle = tp_subtitle` to each.

### Step 7 — Update scale_color_manual to use active_palette

Every `scale_color_manual(values = color_palette, ...)` call inside the
function should be updated to `scale_color_manual(values = active_palette, ...)`.
Search for `scale_color_manual(values = color_palette` within lines 950–1400
and replace all.

### Step 8 — Run tests

```r
devtools::test(filter = "pca-analysis")
```
Expected: all 3 new tests PASS.

### Step 9 — Commit

```bash
git add R/plots.R tests/testthat/test-pca-analysis.R
git commit -m "feat: add color_by, genotype end-labels, shaped markers, timepoint subtitle to trajectories"
```

---

## Task 3 — Heatmap: split_by = "combination"

**Files:**
- Modify: `R/plots.R` around line 1453 (`create_mea_heatmaps_enhanced`)
- Test: `tests/testthat/test-heatmaps.R`

### Step 1 — Write failing test

Append to `tests/testthat/test-heatmaps.R`:

```r
test_that("split_by = 'combination' creates a combination result", {
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
    split_by     = "combination",
    verbose      = FALSE,
    save_plots   = FALSE
  )
  expect_true("combination_result" %in% names(result))
  # Should have a pheatmap object
  expect_s3_class(result$combination_result$heatmap, "pheatmap")
})
```

### Step 2 — Run to confirm failure

```r
devtools::test(filter = "heatmaps")
```
Expected: FAIL — `split_by = 'combination'` not recognised.

### Step 3 — Implement the combination branch

Find the block in `create_mea_heatmaps_enhanced` that handles `split_by`.
Look for the if/else chain near line ~1600 that checks
`if (!is.null(split_by))`. Add a new branch **before** the existing
`if (split_by == "Genotype")` check:

```r
  # ── combination branch ───────────────────────────────────────────────────
  if (!is.null(split_by) && split_by == "combination") {
    if (!all(c("Treatment", "Genotype") %in% names(data))) {
      warning("split_by = 'combination' requires Treatment and Genotype columns.")
      split_by <- NULL
    } else {
      data$combo_label <- paste(data$Treatment, data$Genotype, sep = "_")

      # Build two annotation data frames
      ann_row <- data %>%
        dplyr::distinct(Well, Treatment, Genotype) %>%
        dplyr::arrange(Treatment, Genotype) %>%
        tibble::column_to_rownames("Well")

      # Pivot to wide matrix (wells × variables)
      mat <- data %>%
        dplyr::select(Well, Variable = !!sym(variable_column %||% "Variable"),
                      Value = !!sym(value_column)) %>%
        tidyr::pivot_wider(names_from = Variable, values_from = Value,
                           values_fn = mean) %>%
        tibble::column_to_rownames("Well") %>%
        as.matrix()

      # Align annotation to matrix rows
      ann_row <- ann_row[rownames(mat), , drop = FALSE]

      combo_hmap <- pheatmap::pheatmap(
        mat,
        annotation_row  = ann_row,
        cluster_rows    = TRUE,
        cluster_cols    = TRUE,
        show_rownames   = TRUE,
        show_colnames   = TRUE,
        main            = "MEA Heatmap — Treatment \u00d7 Genotype",
        silent          = TRUE
      )

      result[["combination_result"]] <- list(
        heatmap = combo_hmap,
        data    = mat,
        annotation = ann_row
      )
    }
  }
```

> **Note:** `variable_column` and `value_column` are already in scope as
> function parameters. `%||%` from rlang is imported.

### Step 4 — Run tests

```r
devtools::test(filter = "heatmaps")
```
Expected: all heatmap tests PASS.

### Step 5 — Commit

```bash
git add R/plots.R tests/testthat/test-heatmaps.R
git commit -m "feat: add split_by='combination' treatment×genotype heatmap"
```

---

## Task 4 — User Guide: GitHub install + example data section

**Files:**
- Modify: `docs/user-guide/NOVA-User-Guide.Rmd` (near top, before first workflow section)

### Step 1 — Add installation section

After the `## Introduction` / package overview section (look for the first `##`
heading in the Rmd), insert the following new section:

````markdown
## Getting Started

### Option A — Install from GitHub (recommended for latest version)

```{r install-github, eval=FALSE}
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Install NOVA from GitHub
devtools::install_github("atudoras/nova")
```

Once installed, load the package and point to your data folder:

```{r load-pkg, eval=FALSE}
library(NOVA)

# Option 1 — use your own MEA export folder
DATA_DIR <- "/path/to/your/MEA/exports"

# Option 2 — use the bundled demo data (no download needed)
DATA_DIR <- system.file("extdata", package = "NOVA")
# If that returns "", download the Example folder from GitHub:
# https://github.com/atudoras/nova/tree/main/Example
# and set DATA_DIR <- "/path/where/you/saved/Example"
```

### Option B — Install from CRAN (coming soon)

```{r install-cran, eval=FALSE}
install.packages("NOVA")
```

> **Tip:** After `devtools::install_github()` the package is your official,
> working copy — identical to running it from source.  Re-install any time
> with the same command to get the latest version.

---
````

### Step 2 — Re-knit the User Guide

```r
setwd("docs/user-guide")
rmarkdown::render("NOVA-User-Guide.Rmd", output_dir = ".")
```
Expected: `NOVA-User-Guide.html` rebuilt with no errors.

### Step 3 — Commit

```bash
git add docs/user-guide/NOVA-User-Guide.Rmd docs/user-guide/NOVA-User-Guide.html
git commit -m "docs: add GitHub install + example data location to user guide"
```

---

## Task 5 — Enhancement Summary: per-metric figures + customizing fix

**Files:**
- Modify: `docs/NOVA-Enhancement-Summary.Rmd`
- Re-knit to: `docs/NOVA-Enhancement-Summary.html`

### Step 1 — Generate three metric plot figures

Add this chunk **inside** the Enhancement 5 section (look for the
`## Enhancement 5` heading), replacing or supplementing the `eval=FALSE`
code chunk:

````markdown
```{r metric-examples, eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, fig.show='hide'}
library(NOVA)
# Toy dataset matching what plot_mea_metric() expects
set.seed(42)
toy <- data.frame(
  Treatment = rep(c("PBS","KA","Gabazine"), each = 12),
  Genotype  = rep(rep(c("WT","KO"), each = 6), 3),
  Timepoint = rep(rep(c("0min","30min","60min"), each = 2), 6),
  MFR       = c(rnorm(12, 2, 0.5), rnorm(12, 4, 0.8), rnorm(12, 1.5, 0.4)),
  stringsAsFactors = FALSE
)
p_bar <- plot_mea_metric(toy, metric = "MFR", plot_type = "bar",
                          group_by = "Treatment", verbose = FALSE)
p_box <- plot_mea_metric(toy, metric = "MFR", plot_type = "box",
                          group_by = "Treatment", verbose = FALSE)
p_vio <- plot_mea_metric(toy, metric = "MFR", plot_type = "violin",
                          group_by = "Treatment", verbose = FALSE)
fig_dir <- "user-guide/figures"
ggplot2::ggsave(file.path(fig_dir, "metric_bar.png"),    p_bar, width=6, height=4, dpi=120)
ggplot2::ggsave(file.path(fig_dir, "metric_box.png"),    p_box, width=6, height=4, dpi=120)
ggplot2::ggsave(file.path(fig_dir, "metric_violin.png"), p_vio, width=6, height=4, dpi=120)
```
````

Then immediately after, add three display chunks:

````markdown
**Bar plot** — mean ± SEM per group:

```{r metric-bar-fig, echo=FALSE, out.width="70%"}
knitr::include_graphics("user-guide/figures/metric_bar.png")
```

**Box plot** — full distribution:

```{r metric-box-fig, echo=FALSE, out.width="70%"}
knitr::include_graphics("user-guide/figures/metric_box.png")
```

**Violin plot** — density shape:

```{r metric-violin-fig, echo=FALSE, out.width="70%"}
knitr::include_graphics("user-guide/figures/metric_violin.png")
```
````

### Step 2 — Fix the customizing-figures section

Find the section titled something like `## Customizing Figures` or
`### Customizing` in the Rmd. Replace the vague prose block with:

````markdown
### Customizing Figures

NOVA passes styling parameters directly to ggplot2.  The three most
useful are shown below with a visual result:

```{r custom-code, eval=FALSE}
# Custom colour palette
p <- plot_mea_metric(
  data        = results$processed_data,
  metric      = "MFR",
  plot_type   = "bar",
  group_by    = "Treatment",
  custom_colors = c("PBS" = "#1F78B4", "KA" = "#E31A1C", "Gabazine" = "#33A02C")
)

# Add your own ggplot2 theme on top
p + ggplot2::theme(
  axis.text  = ggplot2::element_text(size = 14),
  plot.title = ggplot2::element_text(face = "bold")
)
```

```{r custom-color-fig, echo=FALSE, out.width="60%"}
knitr::include_graphics("user-guide/figures/s3_custom_color.png")
```

| Parameter | Controls | Example |
|-----------|----------|---------|
| `custom_colors` | Fill colour per group | `c("KA" = "#E31A1C")` |
| `plot_type` | `"bar"`, `"box"`, `"violin"`, `"line"` | `"violin"` |
| `error_type` | Error bars: `"sem"`, `"sd"`, `"ci95"` | `"ci95"` |
| `group_by` | X-axis grouping variable | `"Genotype"` |
| `facet_by` | Facet panels | `"Timepoint"` |
````

### Step 3 — Re-knit the Enhancement Summary

```r
setwd("docs")
rmarkdown::render("NOVA-Enhancement-Summary.Rmd", output_dir = ".")
```
Expected: `NOVA-Enhancement-Summary.html` rebuilt, three new metric figures
embedded, customizing section shows code + image + table.

### Step 4 — Commit

```bash
git add docs/NOVA-Enhancement-Summary.Rmd docs/NOVA-Enhancement-Summary.html \
        docs/user-guide/figures/metric_bar.png \
        docs/user-guide/figures/metric_box.png \
        docs/user-guide/figures/metric_violin.png
git commit -m "docs: add metric plot examples and fix customizing section in enhancement summary"
```

---

## Task 6 — Full test run, push, confirm HTML

### Step 1 — Run full test suite

```r
devtools::test()
```
Expected: 0 FAIL, 0 ERROR. Note WARN count (acceptable if pre-existing).

### Step 2 — Quick check

```r
devtools::check(cran = FALSE)
```
Expected: 0 ERRORs, 0 WARNINGs on new code.

### Step 3 — Confirm HTML file exists

```bash
ls -lh docs/NOVA-Enhancement-Summary.html
```
Expected: file exists, > 1 MB (self-contained with embedded images).

### Step 4 — Push branch

```bash
git push origin claude/nova-features
```

### Step 5 — Tell the user

- `docs/NOVA-Enhancement-Summary.html` — send to supervisor, opens in any browser
- `docs/user-guide/NOVA-User-Guide.html` — full illustrated guide
- PR #1 on GitHub is updated with all commits

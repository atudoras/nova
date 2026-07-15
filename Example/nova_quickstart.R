# =============================================================================
# NOVA Quickstart Script
# =============================================================================
# HOW TO USE:
#   1. Set DATA_DIR below to the folder that contains your MEA experiment
#      folders (each folder should be named like "MEA001", "MEA002", etc.)
#   2. Run the entire script (Ctrl+A -> Run, or source())
#   3. Figures are saved in DATA_DIR/nova_output/ and shown in the Viewer
# =============================================================================

# -- STEP 1: Set your data folder ---------------------------------------------
DATA_DIR <- "path/to/your/MEA/data"   # <<< CHANGE THIS

# -- STEP 2: Optional -- name what your columns represent ---------------------
# If your CSV rows are labelled differently, change these strings to match.
# Set GENOTYPE_COLUMN to NULL if all wells share the same genotype (WT-only datasets).
TREATMENT_COLUMN <- "Treatment"   # or NULL
GENOTYPE_COLUMN  <- NULL          # set to "Genotype" if your data has multiple genotypes

# -- STEP 3: Optional -- narrow what gets plotted -----------------------------
# Leave as NULL to include everything; fill in to filter.
# Examples:
#   SHOW_TREATMENTS  <- c("PBS", "KA")
#   SHOW_TIMEPOINTS  <- c("baseline", "1h", "2h")
SHOW_TREATMENTS  <- NULL
SHOW_TIMEPOINTS  <- NULL

# -- STEP 4: Confidence ellipses ----------------------------------------------
# Set to TRUE to add 95% CI ellipses to your PCA scatter plot.
# Each group gets a filled ellipse showing where 95% of its samples fall.
# Great for seeing whether treatment groups are truly separated.
SHOW_ELLIPSES <- TRUE    # TRUE / FALSE
ELLIPSE_LEVEL <- 0.95   # 0.95 = 95% CI, 0.68 = ±1 SD (tighter), 0.99 = wider
ELLIPSE_ALPHA <- 0.12   # fill transparency: 0 = invisible, 1 = solid

# -- STEP 5: Figure appearance ------------------------------------------------
FIGURE_WIDTH  <- 12   # inches
FIGURE_HEIGHT <- 10   # inches
DPI           <- 300  # 300 for publication, 150 for quick preview

# =============================================================================
# (Everything below runs automatically -- no changes needed)
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
dir.create(file.path(OUT_DIR, "pca"),          recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "heatmaps"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "trajectories"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "metrics"),      recursive = TRUE, showWarnings = FALSE)

message("\n=== Step 1/4: Discovering data structure ===")
discovery <- discover_mea_structure(DATA_DIR)
cat("Found", discovery$experiment_count, "experiment(s)\n")
cat("Timepoints:", paste(discovery$all_timepoints, collapse=", "), "\n")

# Infer baseline: the earliest timepoint in dynamical order (baseline-like
# labels sort first, then real elapsed time), so normalization uses the start of
# the timecourse rather than whichever label happens to match a pattern first.
baseline_guess <- nova_order_timepoints(discovery$all_timepoints)[1]
if (length(discovery$potential_baselines) == 0) {
  cat("No baseline-like label found -- normalizing to earliest timepoint:", baseline_guess, "\n")
} else {
  cat("Using baseline:", baseline_guess, "\n")
}

# Build grouping columns from what's available
grouping_cols <- c(TREATMENT_COLUMN, GENOTYPE_COLUMN)
grouping_cols <- grouping_cols[!is.null(grouping_cols)]

message("\n=== Step 2/4: Processing data ===")
processed <- process_mea_flexible(
  main_dir             = DATA_DIR,
  grouping_variables   = grouping_cols,
  baseline_timepoint   = baseline_guess,
  verbose              = TRUE
)

message("\n=== Step 3/4: Computing PCA, trajectories, heatmaps ===")
use_norm <- !is.null(baseline_guess) && !is.null(processed$normalized_data)
data_for_analysis <- if (use_norm) processed$normalized_data else processed$raw_data
val_col           <- if (use_norm) "Normalized_Value" else "Value"

pca_results  <- pca_analysis_enhanced(data_for_analysis,
                                       grouping_variables = grouping_cols,
                                       value_column       = val_col,
                                       verbose            = FALSE)

# Describes how far each condition moved from baseline, how directly, and when.
# Well is picked up automatically as the replicate unit for the error bands.
trajectories <- nova_trajectory_summary(pca_results, verbose = FALSE)

heatmaps     <- create_mea_heatmaps_enhanced(
                  processing_result  = processed,
                  use_raw            = !use_norm,
                  filter_timepoints  = SHOW_TIMEPOINTS,
                  filter_treatments  = SHOW_TREATMENTS,
                  save_plots         = FALSE,
                  verbose            = FALSE)

message("\n=== Step 4/4: Saving figures to ", OUT_DIR, " ===")

save_plot <- function(p, path, w = FIGURE_WIDTH, h = FIGURE_HEIGHT) {
  ggplot2::ggsave(path, plot = p, width = w, height = h, dpi = DPI)
  print(p)    # also show in Viewer
  invisible(p)
}

# PCA scatter plots (pca_analysis_enhanced computes the PCA; pca_plots_enhanced draws it)
pca_plots <- pca_plots_enhanced(
  pca_output         = pca_results,
  grouping_variables = grouping_cols,
  color_variable     = TREATMENT_COLUMN,
  shape_variable     = GENOTYPE_COLUMN,
  save_plots         = FALSE,
  verbose            = FALSE
)
for (nm in names(pca_plots$plots)) {
  save_plot(pca_plots$plots[[nm]], file.path(OUT_DIR, "pca", paste0("pca_", nm, ".pdf")))
}

# PCA with 95% confidence ellipses
if (SHOW_ELLIPSES && !is.null(pca_results$plot_data)) {
  pd <- pca_results$plot_data
  p_ell <- ggplot2::ggplot(
    pd,
    ggplot2::aes(x = .data[["PC1"]], y = .data[["PC2"]],
                 colour = .data[[grouping_cols[1]]], fill = .data[[grouping_cols[1]]])
  ) +
    ggplot2::stat_ellipse(type = "norm", level = ELLIPSE_LEVEL,
                          geom = "polygon", alpha = ELLIPSE_ALPHA, linewidth = 0) +
    ggplot2::stat_ellipse(type = "norm", level = ELLIPSE_LEVEL, linewidth = 1.1) +
    ggplot2::geom_point(size = 2.5, alpha = 0.5, shape = 16) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 4, shape = 18) +
    ggplot2::labs(
      x       = paste0("PC1 (", round(pca_results$variance_explained[1], 1), "% variance)"),
      y       = paste0("PC2 (", round(pca_results$variance_explained[2], 1), "% variance)"),
      caption = paste0(ELLIPSE_LEVEL * 100, "% confidence ellipses  ·  diamond = group centroid")
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 13)
  save_plot(p_ell, file.path(OUT_DIR, "pca", "pca_ellipses.pdf"))
}

# Scree / elbow: how many components actually carry the variance
if (!is.null(pca_results$elbow_plot)) {
  save_plot(pca_results$elbow_plot, file.path(OUT_DIR, "pca", "pca_elbow.pdf"))
}

# Trajectories
for (nm in names(trajectories$plots)) {
  save_plot(trajectories$plots[[nm]], file.path(OUT_DIR, "trajectories", paste0(nm, ".pdf")))
}

# Heatmaps: each entry is a list carrying the pheatmap object alongside its data
if (!is.null(heatmaps) && is.list(heatmaps)) {
  for (nm in setdiff(names(heatmaps), "metadata")) {
    hm <- heatmaps[[nm]]$heatmap
    if (is.null(hm) || !inherits(hm, "pheatmap")) next
    pdf(file.path(OUT_DIR, "heatmaps", paste0(nm, ".pdf")),
        width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
    print(hm)
    dev.off()
  }
}

# Plain-language read-out of the trajectory metrics
message("\n=== Trajectory summary ===")
print(trajectories$metrics)
nova_describe(trajectories)

message("\nDone! All figures saved to: ", OUT_DIR)

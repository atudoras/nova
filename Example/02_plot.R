# =============================================================================
# NOVA Workflow - 02_plot.R
# Load results computed by 01_compute.R, tune display parameters,
# and generate figures. Every plot is shown in the viewer AND saved to disk.
# Only edit the TUNE block below - re-run anytime to update figures.
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
# These filters are display-only - the .rds file is never modified.
SHOW_TIMEPOINTS <- NULL    # e.g. c("baseline", "1h", "2h")
SHOW_TREATMENTS <- NULL    # e.g. c("PBS", "KA")

# --- Aesthetic mapping -------------------------------------------------------
COLOR_BY <- "Treatment"    # variable mapped to point/line color
SHAPE_BY <- NULL           # set to "Genotype" if your data has multiple genotypes
FACET_BY <- NULL           # variable for panel faceting (or NULL)

# --- 95% Confidence ellipses -------------------------------------------------
# Ellipses show the spread of each group in PCA space — one of the most useful
# ways to see whether treatment groups are truly separated.
#
# SHOW_ELLIPSES = TRUE/FALSE — whether to draw them at all
# ELLIPSE_LEVEL = confidence level (0.95 = 95% CI, 0.68 = 1 SD, 0.99 = wider)
# ELLIPSE_ALPHA = fill transparency: 0 = fully transparent, 1 = fully opaque
#                 (0.10–0.15 works well for 4+ overlapping groups)
# ELLIPSE_LINE  = border linewidth (1.0–1.5 is clean for publication)
#
# Tip: set ELLIPSE_LEVEL <- 0.68 to show ±1 SD regions (tighter, less overlap)
SHOW_ELLIPSES <- TRUE
ELLIPSE_LEVEL <- 0.95
ELLIPSE_ALPHA <- 0.12
ELLIPSE_LINE  <- 1.1

# --- Colors ------------------------------------------------------------------
# NULL = automatic scientific palette based on number of groups.
# To override, provide a named vector matching your group values exactly:
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
# LOAD & VALIDATE  (do not edit below this line)
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
# Filters a copy of plot_data - the nova object loaded above is never mutated.
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

# Apply size and font settings from TUNE block
# These set ggplot2 defaults so all plots respect the TUNE parameters.
ggplot2::update_geom_defaults("point", list(size = POINT_SIZE))
ggplot2::update_geom_defaults("line",  list(linewidth = LINE_SIZE))

# Helper: apply font size theme to any ggplot
apply_theme <- function(p) {
  p + ggplot2::theme(
    text         = ggplot2::element_text(size = FONT_SIZE),
    axis.title   = ggplot2::element_text(size = FONT_SIZE),
    axis.text    = ggplot2::element_text(size = FONT_SIZE * 0.85),
    legend.text  = ggplot2::element_text(size = FONT_SIZE * 0.85),
    legend.title = ggplot2::element_text(size = FONT_SIZE)
  )
}

# Helper: save a ggplot and record the path
save_plot <- function(p, path) {
  ggplot2::ggsave(path, plot = p, width = FIGURE_WIDTH, height = FIGURE_HEIGHT, dpi = DPI)
  saved_files <<- c(saved_files, path)
  cat("  Saved:", path, "\n")
}

# Helper: save a non-ggplot (e.g. pheatmap) via pdf device
save_base_plot <- function(p, path) {
  grDevices::pdf(path, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
  print(p)
  grDevices::dev.off()
  saved_files <<- c(saved_files, path)
  cat("  Saved:", path, "\n")
}

# =============================================================================
# PCA SCATTER PLOTS
# =============================================================================
cat("--- PCA scatter plots ---\n")

pca_plots_out <- pca_plots_enhanced(
  pca_output     = nova$pca,
  plot_data      = plot_data,
  color_variable = COLOR_BY,
  shape_variable = SHAPE_BY,
  pannels_var    = FACET_BY,
  save_plots     = FALSE,
  verbose        = FALSE
)

for (plot_name in names(pca_plots_out$plots)) {
  p <- pca_plots_out$plots[[plot_name]]
  if (inherits(p, "ggplot")) {
    p <- apply_theme(p)
    print(p)
    save_plot(p, file.path(OUTPUT_DIR, "pca", paste0(plot_name, ".pdf")))
  }
}

# --- Ellipse plot (generated here for full customization control) -----------
# pca_plots_enhanced also generates a 'color_with_ellipses' plot automatically,
# but this version lets you tune level, alpha, and line weight from the TUNE block.
if (SHOW_ELLIPSES && !is.null(COLOR_BY)) {
  p_ell <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x      = .data[["PC1"]],
      y      = .data[["PC2"]],
      colour = .data[[COLOR_BY]],
      fill   = .data[[COLOR_BY]]
    )
  ) +
    # Filled ellipse polygon
    ggplot2::stat_ellipse(
      type      = "norm",
      level     = ELLIPSE_LEVEL,
      geom      = "polygon",
      alpha     = ELLIPSE_ALPHA,
      linewidth = 0
    ) +
    # Ellipse border
    ggplot2::stat_ellipse(
      type      = "norm",
      level     = ELLIPSE_LEVEL,
      linewidth = ELLIPSE_LINE
    ) +
    # Individual points
    ggplot2::geom_point(
      size  = POINT_SIZE,
      alpha = 0.5,
      shape = 16
    ) +
    # Group centroids
    ggplot2::stat_summary(
      fun   = mean,
      geom  = "point",
      size  = POINT_SIZE * 1.5,
      shape = 18
    ) +
    ggplot2::labs(
      x      = paste0("PC1 (", round(nova$pca$variance_explained[1], 1), "% variance)"),
      y      = paste0("PC2 (", round(nova$pca$variance_explained[2], 1), "% variance)"),
      colour = COLOR_BY,
      fill   = COLOR_BY,
      caption = paste0(ELLIPSE_LEVEL * 100, "% confidence ellipses  ·  diamond = group centroid")
    ) +
    ggplot2::coord_fixed()

  if (!is.null(CUSTOM_COLORS)) {
    p_ell <- p_ell +
      ggplot2::scale_colour_manual(values = CUSTOM_COLORS) +
      ggplot2::scale_fill_manual(values = CUSTOM_COLORS)
  }

  p_ell <- apply_theme(p_ell)
  print(p_ell)
  save_plot(p_ell, file.path(OUTPUT_DIR, "pca", "pca_ellipses.pdf"))
}

# Elbow plot
if (!is.null(nova$pca$elbow_plot)) {
  nova$pca$elbow_plot <- apply_theme(nova$pca$elbow_plot)
  print(nova$pca$elbow_plot)
  ggplot2::ggsave(
    file.path(OUTPUT_DIR, "pca", "pca_elbow.pdf"),
    plot = nova$pca$elbow_plot, width = FIGURE_WIDTH, height = FIGURE_HEIGHT, dpi = DPI
  )
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
      p <- apply_theme(p)
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
        p <- apply_theme(p)
        print(p)
        save_plot(p, file.path(OUTPUT_DIR, "heatmaps", paste0(plot_name, ".pdf")))
      } else {
        # pheatmap / base graphics output
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
cat("  DONE -", length(saved_files), "figures saved\n")
cat("========================================\n")
cat("Output folder:", normalizePath(OUTPUT_DIR, mustWork = FALSE), "\n\n")
cat("To update figures: edit the TUNE block above and re-run.\n")
cat("To recompute data: re-run 01_compute.R.\n\n")

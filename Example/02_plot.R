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
SHOW_TREATMENTS <- NULL    # e.g. c("Control", "Drug_A")
SHOW_GENOTYPES  <- NULL    # e.g. c("WT", "KO")

# --- Aesthetic mapping -------------------------------------------------------
COLOR_BY <- "Treatment"    # variable mapped to point/line color
SHAPE_BY <- "Genotype"     # variable mapped to point shape (or NULL)
FACET_BY <- NULL           # variable for panel faceting (or NULL)

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

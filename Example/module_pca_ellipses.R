# =============================================================================
# NOVA Module — PCA 95% Confidence Ellipses
# =============================================================================
# Run AFTER 01_compute.R (or any script that saves nova_results.rds).
# Produces a single publication-ready PCA scatter plot with filled confidence
# ellipses per group — one of the clearest ways to show group separation.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to your .rds file
#   2. Customize the TUNE block below
#   3. Run — figure saves to OUTPUT_FILE and opens in the Viewer
# =============================================================================
library(NOVA)
library(ggplot2)

# =============================================================================
# TUNE YOUR ELLIPSE PLOT HERE
# =============================================================================

RESULTS_PATH <- "nova_results.rds"     # from 01_compute.R
OUTPUT_FILE  <- "figures/pca/pca_ellipses.pdf"

# --- Which variable to color by ----------------------------------------------
COLOR_BY <- "Treatment"   # must exist in your data (check with names(nova$pca$plot_data))

# --- Confidence level --------------------------------------------------------
# 0.95 = 95% CI   — standard, shows broader spread
# 0.68 = ±1 SD    — tighter ellipses, less overlap between groups
# 0.99 = 99% CI   — very wide, use when groups are well-separated
ELLIPSE_LEVEL <- 0.95

# --- Ellipse appearance ------------------------------------------------------
ELLIPSE_ALPHA <- 0.12    # fill transparency: 0 = invisible, 1 = fully opaque
                          # 0.08–0.15 works well for 3–6 overlapping groups
ELLIPSE_LINE  <- 1.1     # border linewidth (0.8 = thin, 1.5 = bold)

# --- Point appearance --------------------------------------------------------
POINT_SIZE  <- 2.5   # individual sample points
POINT_ALPHA <- 0.50  # point transparency (lower = more transparent)
SHOW_CENTROID <- TRUE  # TRUE = show group mean as a diamond

# --- Timepoint filter --------------------------------------------------------
# NULL = pool all timepoints (often the clearest view of group separation)
# e.g. c("1h", "2h") = only show post-treatment timepoints
SHOW_TIMEPOINTS <- NULL

# --- Custom colors -----------------------------------------------------------
# NULL = automatic palette. To set manually:
#   CUSTOM_COLORS <- c("Control" = "#6B7280", "Drug_A" = "#EF4444")
CUSTOM_COLORS <- NULL

# --- Output size -------------------------------------------------------------
FIGURE_WIDTH  <- 7
FIGURE_HEIGHT <- 5.5
DPI           <- 300

# =============================================================================
# (Everything below runs automatically)
# =============================================================================

if (!file.exists(RESULTS_PATH)) {
  stop("Results file not found: ", RESULTS_PATH, "\nRun 01_compute.R first.")
}

nova     <- readRDS(RESULTS_PATH)
pd       <- nova$pca$plot_data
var_exp  <- nova$pca$variance_explained

# Apply timepoint filter if set
if (!is.null(SHOW_TIMEPOINTS) && "Timepoint" %in% names(pd)) {
  pd <- pd[pd$Timepoint %in% SHOW_TIMEPOINTS, ]
  if (nrow(pd) == 0) stop("No data left after timepoint filter. Check SHOW_TIMEPOINTS.")
}

if (!COLOR_BY %in% names(pd)) {
  stop("COLOR_BY = '", COLOR_BY, "' not found. Available columns: ",
       paste(names(pd), collapse = ", "))
}

n_groups <- length(unique(pd[[COLOR_BY]]))
if (n_groups < 3) {
  message("Note: fewer than 3 groups — ellipses may not be very meaningful.")
}

# Build plot
p <- ggplot(pd, aes(x = .data[["PC1"]], y = .data[["PC2"]],
                    colour = .data[[COLOR_BY]], fill = .data[[COLOR_BY]])) +

  # Filled ellipse polygon
  stat_ellipse(
    type      = "norm",
    level     = ELLIPSE_LEVEL,
    geom      = "polygon",
    alpha     = ELLIPSE_ALPHA,
    linewidth = 0
  ) +

  # Ellipse border
  stat_ellipse(
    type      = "norm",
    level     = ELLIPSE_LEVEL,
    linewidth = ELLIPSE_LINE,
    alpha     = 0.9
  ) +

  # Individual sample points
  geom_point(size = POINT_SIZE, alpha = POINT_ALPHA, shape = 16) +

  # Group centroid (mean position)
  { if (SHOW_CENTROID)
      stat_summary(fun = mean, geom = "point", size = POINT_SIZE * 1.8, shape = 18)
    else
      NULL
  } +

  labs(
    x       = paste0("PC1 (", round(var_exp[1], 1), "% variance)"),
    y       = paste0("PC2 (", round(var_exp[2], 1), "% variance)"),
    colour  = COLOR_BY,
    fill    = COLOR_BY,
    caption = paste0(
      ELLIPSE_LEVEL * 100, "% confidence ellipses",
      if (SHOW_CENTROID) "  ·  diamond = group centroid" else "",
      if (!is.null(SHOW_TIMEPOINTS)) paste0("  ·  timepoints: ", paste(SHOW_TIMEPOINTS, collapse = ", ")) else "  ·  all timepoints pooled"
    )
  ) +

  coord_fixed() +

  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "#E5E7EB", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.caption     = element_text(size = 8.5, colour = "#9CA3AF", hjust = 0.5,
                                    margin = margin(t = 8))
  )

# Apply custom colors if provided
if (!is.null(CUSTOM_COLORS)) {
  p <- p +
    scale_colour_manual(values = CUSTOM_COLORS) +
    scale_fill_manual(values = CUSTOM_COLORS)
}

# Show in Viewer
print(p)

# Save
dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
ggsave(OUTPUT_FILE, plot = p, width = FIGURE_WIDTH, height = FIGURE_HEIGHT, dpi = DPI, bg = "white")
message("Saved: ", normalizePath(OUTPUT_FILE, mustWork = FALSE))

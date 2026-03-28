# =============================================================================
# NOVA — Trajectory Module
# =============================================================================
# PURPOSE:
#   Plots how each treatment group moves through PCA space over time.
#   Each line = one group's average path from baseline to final timepoint.
#
# REQUIRES:
#   Run 01_compute.R first — this module loads the saved nova_results.rds file.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to wherever you saved nova_results.rds
#   2. Edit the SETTINGS section below (just change the values you need)
#   3. Run the whole script  →  figure saves to OUTPUT_DIR automatically
# =============================================================================

library(NOVA)

# =============================================================================
# PATHS
# =============================================================================

RESULTS_PATH <- "nova_results.rds"          # path to your saved results file
OUTPUT_DIR   <- "nova_output/trajectories"  # folder where the figure will be saved

# =============================================================================
# SETTINGS — edit these to customise the plot
# =============================================================================

# --- Which data to show ---
# Set to NULL to include everything, or list specific values to filter
SHOW_TREATMENTS <- NULL       # e.g. c("PBS", "KA", "NMDA")  or NULL for all
SHOW_GENOTYPES  <- NULL       # e.g. c("WT")                  or NULL for all
SHOW_TIMEPOINTS <- NULL       # e.g. c("baseline","1h","2h")  or NULL for all

# --- Timepoint order ---
# Define the chronological order of your timepoints.
# If NULL, NOVA will try to sort them automatically.
TIMEPOINT_ORDER <- NULL       # e.g. c("baseline","15min","30min","1h","2h")

# --- Grouping ---
# Which variable(s) define a "trajectory" — one line per unique combination.
TRAJECTORY_GROUPING <- c("Treatment")    # e.g. c("Treatment") or c("Treatment","Genotype")

# --- Which PCs to plot ---
PC_X <- 1    # horizontal axis  (1 = PC1)
PC_Y <- 2    # vertical axis    (2 = PC2)

# --- Colors ---
# NULL = automatic palette.
# To use custom colors, provide a named vector matching your group values:
#   CUSTOM_COLORS <- c("PBS" = "#6B7280", "KA" = "#EF4444", "NMDA" = "#10B981")
CUSTOM_COLORS <- NULL

# --- Point and line size ---
POINT_SIZE <- 3      # size of the dots at each timepoint
LINE_SIZE  <- 1.2    # thickness of the trajectory lines

# --- Save settings ---
FIGURE_WIDTH  <- 8      # inches
FIGURE_HEIGHT <- 7      # inches
DPI           <- 300

# =============================================================================
# LOAD RESULTS  (no need to edit below this line)
# =============================================================================

if (!file.exists(RESULTS_PATH)) {
  stop("Results file not found: ", RESULTS_PATH,
       "\nRun 01_compute.R first to generate it.")
}

message("Loading results from: ", RESULTS_PATH)
results <- readRDS(RESULTS_PATH)

pca_data <- results$pca

# Apply filters
plot_data <- pca_data$plot_data

if (!is.null(SHOW_TREATMENTS) && "Treatment" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Treatment %in% SHOW_TREATMENTS, ]

if (!is.null(SHOW_GENOTYPES) && "Genotype" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Genotype %in% SHOW_GENOTYPES, ]

if (!is.null(SHOW_TIMEPOINTS) && "Timepoint" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Timepoint %in% SHOW_TIMEPOINTS, ]

pca_filtered        <- pca_data
pca_filtered$plot_data <- plot_data

# =============================================================================
# GENERATE FIGURE
# =============================================================================

message("Generating trajectory plot...")

p <- plot_pca_trajectories_general(
  pca_output          = pca_filtered,
  timepoint_order     = TIMEPOINT_ORDER,
  trajectory_grouping = TRAJECTORY_GROUPING,
  pc_x                = PC_X,
  pc_y                = PC_Y,
  point_size          = POINT_SIZE,
  line_size           = LINE_SIZE,
  color_palette       = CUSTOM_COLORS,
  save_plots          = FALSE     # saving handled below so you keep control of path
)

# =============================================================================
# SAVE
# =============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(OUTPUT_DIR,
  paste0("trajectories_PC", PC_X, "_vs_PC", PC_Y, ".pdf"))

pdf(out_file, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
if (is.list(p)) lapply(p, print) else print(p)
dev.off()

message("Saved: ", out_file)

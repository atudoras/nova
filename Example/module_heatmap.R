# =============================================================================
# NOVA — Heatmap Module
# =============================================================================
# PURPOSE:
#   Generates a clustered heatmap of MEA metrics across treatments, genotypes,
#   and timepoints. Each row = one metric; each column = one group/timepoint.
#
# REQUIRES:
#   Run 01_compute.R first — this module loads the saved nova_results.rds file.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to wherever you saved nova_results.rds
#   2. Edit the SETTINGS section below
#   3. Run the whole script  →  figure saves to OUTPUT_DIR automatically
# =============================================================================

library(NOVA)

# =============================================================================
# PATHS
# =============================================================================

RESULTS_PATH <- "nova_results.rds"       # path to your saved results file
OUTPUT_DIR   <- "nova_output/heatmaps"   # folder where the figure will be saved

# =============================================================================
# SETTINGS — edit these to customise the plot
# =============================================================================

# --- Grouping ---
# Which variables define the columns of the heatmap.
# Each unique combination gets its own column.
GROUPING_COLUMNS <- c("Treatment", "Timepoint")   # e.g. c("Treatment") or c("Treatment","Genotype","Timepoint")

# --- Split into panels ---
# If you want one heatmap per genotype (or per treatment), set SPLIT_BY.
# Set to NULL for a single combined heatmap.
SPLIT_BY <- NULL      # e.g. "Genotype"  or  NULL

# --- Filter what to include ---
# Set to NULL to include everything, or list specific values.
SHOW_TREATMENTS <- NULL     # e.g. c("PBS", "KA")      or NULL for all
SHOW_GENOTYPES  <- NULL     # e.g. c("WT", "KO")       or NULL for all
SHOW_TIMEPOINTS <- NULL     # e.g. c("baseline", "2h") or NULL for all

# --- Raw vs normalised values ---
# TRUE  = show raw (un-normalised) electrode values
# FALSE = show baseline-normalised values (default, recommended)
USE_RAW <- FALSE

# --- Clustering ---
CLUSTER_ROWS <- TRUE    # cluster metrics (rows)?
CLUSTER_COLS <- TRUE    # cluster groups (columns)?

# --- Save settings ---
FIGURE_WIDTH  <- 10     # inches
FIGURE_HEIGHT <- 8      # inches
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

# =============================================================================
# GENERATE FIGURE
# =============================================================================

message("Generating heatmap...")

hm <- create_mea_heatmaps_enhanced(
  processing_result = results$processed,
  grouping_columns  = GROUPING_COLUMNS,
  split_by          = SPLIT_BY,
  filter_treatments = SHOW_TREATMENTS,
  filter_genotypes  = SHOW_GENOTYPES,
  filter_timepoints = SHOW_TIMEPOINTS,
  use_raw           = USE_RAW,
  cluster_rows      = CLUSTER_ROWS,
  cluster_cols      = CLUSTER_COLS,
  save_plots        = FALSE
)

# =============================================================================
# SAVE
# =============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(OUTPUT_DIR, "heatmap.pdf")

# pheatmap objects need the pdf() device (not ggsave)
pdf(out_file, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
if (is.list(hm) && !inherits(hm, "pheatmap")) {
  lapply(hm, function(h) {
    if (!is.null(h)) grid::grid.newpage(); grid::grid.draw(h$gtable)
  })
} else if (!is.null(hm)) {
  grid::grid.newpage()
  grid::grid.draw(hm$gtable)
}
dev.off()

message("Saved: ", out_file)

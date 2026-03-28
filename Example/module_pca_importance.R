# =============================================================================
# NOVA — PCA Variable Importance Module
# =============================================================================
# PURPOSE:
#   Shows which MEA metrics drive the PCA axes most. Produces a ranked bar
#   chart and a loading plot so you know which variables to focus on.
#
# REQUIRES:
#   Run 01_compute.R first — this module loads the saved nova_results.rds file.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to wherever you saved nova_results.rds
#   2. Edit the SETTINGS section below
#   3. Run the whole script  →  figures save to OUTPUT_DIR automatically
# =============================================================================

library(NOVA)

# =============================================================================
# PATHS
# =============================================================================

RESULTS_PATH <- "nova_results.rds"              # path to your saved results file
OUTPUT_DIR   <- "nova_output/variable_importance"  # folder where figures will be saved

# =============================================================================
# SETTINGS — edit these to customise the analysis
# =============================================================================

# --- Which PCs to analyse ---
PC_X <- 1    # primary axis   (1 = PC1)
PC_Y <- 2    # secondary axis (2 = PC2)

# --- How many top variables to show ---
# This controls how many metrics appear in the bar chart and loading plot.
TOP_N <- 15    # show the 15 most important variables  (increase or decrease as needed)

# --- Minimum loading threshold ---
# Variables with absolute loading below this value on BOTH selected PCs
# will be excluded from plots to reduce clutter.
MIN_LOADING <- 0.1    # set to 0 to show all variables

# --- Save settings ---
FIGURE_WIDTH  <- 10     # inches
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

# =============================================================================
# GENERATE FIGURES
# =============================================================================

message("Analysing variable importance for PC", PC_X, " and PC", PC_Y, "...")

importance <- analyze_pca_variable_importance_general(
  pca_output            = results$pca,
  pc_x                  = PC_X,
  pc_y                  = PC_Y,
  top_n_display         = TOP_N,
  min_loading_threshold = MIN_LOADING,
  save_plots            = FALSE
)

# =============================================================================
# SAVE
# =============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(OUTPUT_DIR,
  paste0("variable_importance_PC", PC_X, "_PC", PC_Y, ".pdf"))

pdf(out_file, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
if (is.list(importance$plots)) {
  lapply(importance$plots, function(p) if (!is.null(p)) print(p))
} else if (!is.null(importance$plots)) {
  print(importance$plots)
}
dev.off()

# Also save the importance table as CSV for easy inspection
csv_file <- file.path(OUTPUT_DIR,
  paste0("variable_importance_PC", PC_X, "_PC", PC_Y, ".csv"))
if (!is.null(importance$importance_table)) {
  write.csv(importance$importance_table, csv_file, row.names = FALSE)
  message("Importance table saved: ", csv_file)
}

message("Saved: ", out_file)

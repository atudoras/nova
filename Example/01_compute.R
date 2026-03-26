# =============================================================================
# NOVA Workflow - 01_compute.R
# Run this script ONCE per dataset. It discovers your data, lets you assign
# variable roles, computes all analyses, and saves results to an .rds file.
# Then open 02_plot.R to generate and fine-tune your figures.
# =============================================================================
library(NOVA)

# =============================================================================
# STEP 1 - POINT TO YOUR DATA
# Edit DATA_DIR to the folder containing your MEA experiment subfolders.
# =============================================================================
DATA_DIR     <- "path/to/your/MEA_data"   # <-- EDIT THIS
RESULTS_PATH <- "nova_results.rds"         # where computed results will be saved

# =============================================================================
# STEP 2 - DISCOVER YOUR DATA
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
# STEP 3 - ASSIGN YOUR VARIABLE ROLES
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
# STEP 4 - COMPUTE EVERYTHING
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
  cat("  Note: trajectory plot skipped -", e$message, "\n")
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
  cat("  Note: heatmap generation skipped -", e$message, "\n")
  NULL
})

# =============================================================================
# STEP 5 - SAVE RESULTS
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

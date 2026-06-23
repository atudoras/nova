# =============================================================================
# NOVA Dynamics Quickstart Script  (new in NOVA 0.2.0)
# =============================================================================
# WHAT THIS DOES:
#   Takes your MEA data all the way to a DYNAMICS analysis -- it asks not just
#   "where is each network in PCA space?" but "where is it MOVING, what state is
#   it approaching, and how stable is that?". It runs the standard NOVA workflow
#   (discover -> process -> PCA) and then the new nova_dynamics layer:
#   trajectory geometry, state transitions, trajectory similarity, dynamical
#   regime detection, occupancy landscape, and an automated written summary.
#
# HOW TO USE:
#   1. Set DATA_DIR to your MEA data folder (same layout as nova_quickstart.R).
#      If you leave it unset, the script runs on the bundled Example data so you
#      can see it work immediately.
#   2. Run the whole script (Ctrl+A -> Run, or source()).
#   3. Figures are saved under <DATA_DIR>/nova_dynamics_output/ and the written
#      interpretation is printed to the console.
# =============================================================================

# -- STEP 1: Set your data folder ---------------------------------------------
DATA_DIR <- "path/to/your/MEA/data"   # <<< CHANGE THIS (or leave to use the demo data)

# -- STEP 2: Tell NOVA how to group trajectories ------------------------------
GROUP_BY     <- "Treatment"   # one trajectory per level of this column
REPLICATE_BY <- "Well"        # replicate unit for transition counting (or NULL)

# -- STEP 3: Options ----------------------------------------------------------
SIMILARITY_METHOD <- "dtw"    # "dtw", "frechet", "euclidean", or "cosine"
N_STATES          <- 4        # number of network states for the transition matrix
SAVE_FIGURES      <- TRUE     # save PNGs to <DATA_DIR>/nova_dynamics_output/
FIG_W <- 8; FIG_H <- 6; DPI <- 300

# =============================================================================
# (Everything below runs automatically -- no changes needed)
# =============================================================================

suppressPackageStartupMessages({
  library(NOVA)
  library(ggplot2)
})

# --- Locate data: your folder if set, otherwise the bundled demo data --------
use_demo <- !dir.exists(DATA_DIR)
if (use_demo) {
  demo <- "MEA Neuronal Agonists"
  if (!dir.exists(demo) && dir.exists(file.path("Example", demo)))
    demo <- file.path("Example", demo)
  DATA_DIR <- demo
  EXPERIMENTS <- c("MEA012", "MEA013")          # public WT plates in the demo set
  message("DATA_DIR not found -- running on the bundled demo data (", DATA_DIR, ").")
} else {
  EXPERIMENTS <- NULL                            # NULL = use every experiment found
}

if (use_demo) {
  OUT_DIR <- file.path(tempdir(), "nova_dynamics_output")
} else {
  OUT_DIR <- file.path(DATA_DIR, "nova_dynamics_output")
}
if (SAVE_FIGURES) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Standard NOVA workflow: process -> PCA ----------------------------------
message("\n=== 1/3  Processing + PCA ===")
proc <- process_mea_flexible(
  main_dir             = DATA_DIR,
  selected_experiments = EXPERIMENTS,
  grouping_variables   = c("Experiment", GROUP_BY, REPLICATE_BY),
  baseline_timepoint   = "baseline",            # baseline is normalised + shown first
  verbose              = FALSE)

# keep GROUP_BY and the replicate column in the PCA metadata so the transition
# matrix can count state changes per replicate
pca <- pca_analysis_enhanced(processing_result = proc,
                             grouping_variables = c(GROUP_BY, REPLICATE_BY),
                             verbose = FALSE)

# Correct, baseline-first timepoint ordering (handles 1h15 / 1h30 / 1h45 etc.)
tp_order <- nova_order_timepoints(pca$plot_data$Timepoint)
message("Timepoints: ", paste(tp_order, collapse = " -> "))

# --- Dynamics: run the whole pipeline in one call ----------------------------
# Geometry/regime/similarity/landscape summarise ONE trajectory per group
# (clean and interpretable); the transition matrix automatically uses the
# replicate column (e.g. Well) found in the data to count per-replicate moves.
message("\n=== 2/3  Dynamics ===")
dyn <- nova_dynamics(
  pca,
  group_var         = GROUP_BY,
  timepoint_order   = tp_order,
  similarity_method = SIMILARITY_METHOD,
  k                 = N_STATES,
  verbose           = FALSE)

print(dyn)

# --- Save the key figures ----------------------------------------------------
if (SAVE_FIGURES) {
  figs <- list(
    geometry_overlay     = dyn$geometry$plots$overlay,
    geometry_velocity    = dyn$geometry$plots$velocity,
    displacement         = dyn$geometry$plots$displacement,
    transition_heatmap   = dyn$transitions$plots$heatmap,
    state_flow           = dyn$transitions$plots$flow,
    similarity_dendro    = dyn$similarity$plots$dendrogram,
    regime_overlay       = dyn$regime$plots$overlay,
    landscape_density    = dyn$landscape$plots$density,
    landscape_potential  = dyn$landscape$plots$potential)
  for (nm in names(figs)) {
    if (!is.null(figs[[nm]]))
      ggsave(file.path(OUT_DIR, paste0(nm, ".png")), figs[[nm]],
             width = FIG_W, height = FIG_H, dpi = DPI)
  }
  message("Saved ", length(figs), " figures to: ", OUT_DIR)
}

# --- 3/3  Automated written interpretation -----------------------------------
message("\n=== 3/3  Interpretation ===\n")
invisible(nova_describe(dyn))

# --- What you have now -------------------------------------------------------
# dyn$geometry      : per-group path length, straightness, velocity, ...
# dyn$transitions   : network-state transition matrix + occupancy
# dyn$similarity    : trajectory distance matrix + clustering
# dyn$regime        : stable / convergent / divergent / oscillatory / transitional
# dyn$landscape     : occupancy density + pseudo-potential
# Each dyn$<analysis>$plots is a list of ggplot objects you can restyle/save.

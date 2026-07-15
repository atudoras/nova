# case_studies/01_epa_dnt_ontogeny/run_discovery.R
#
# Stage 1 for case study 01. Run from the repo root:
#   devtools::load_all(".")
#   source("case_studies/01_epa_dnt_ontogeny/run_discovery.R")
#
# Writes the uncurated exploratory dump plus findings.csv/json to
# outputs/discovery/ (gitignored). Nothing here selects a result.

suppressPackageStartupMessages(library(dplyr))

STUDY   <- file.path("case_studies", "01_epa_dnt_ontogeny")
SHARED  <- file.path("case_studies", "_shared")
CSV     <- file.path(STUDY, "data", "brown2016_mea_dnt.csv")
OUTDIR  <- file.path(STUDY, "outputs", "discovery")

source(file.path(SHARED, "adapt_epa.R"))
source(file.path(SHARED, "discovery_run.R"))

if (!file.exists(CSV)) {
  stop("EPA data not found at ", CSV, ".\nSee ", file.path(STUDY, "SOURCE.md"),
       " for the URL and checksum.", call. = FALSE)
}

# --- analysis window --------------------------------------------------------
# DIV 5 is excluded, and not for convenience. Values are expressed as a fraction
# of the vehicle controls on the same plate at the same DIV, and at DIV 5 those
# controls are themselves silent: the control mean is exactly 0 for network
# spikes in 4 of 6 experiments, so the ratio is undefined for 63-81% of wells on
# the network metrics. That is the assay working -- cortical networks have not
# formed at DIV 5, which is what it exists to measure -- but it means DIV 5
# cannot be expressed on this scale at all.
#
# The alternative was to keep DIV 5 and drop to the three metrics whose controls
# are active then (firing rate, active electrodes, synchrony), which would have
# discarded precisely the network-formation endpoints the assay is built around.
# DIV 7/9/12 carry all eight metrics with 0.0% undefined, and no imputation.
ANALYSIS_TIMEPOINTS <- c("DIV7", "DIV9", "DIV12")

adapted <- adapt_epa(CSV, control_scope = "plate", verbose = TRUE)

d <- adapted$normalized_data %>%
  filter(.data$Timepoint %in% ANALYSIS_TIMEPOINTS,
         .data$Variable %in% adapted$pca_metrics)

stopifnot(sum(is.na(d$Normalized_Value)) == 0)

message("\n=== ANALYSIS WINDOW ===")
message("  timepoints: ", paste(ANALYSIS_TIMEPOINTS, collapse = ", "),
        "  (DIV5 excluded: controls silent, ratio undefined)")
message("  metrics: ", dplyr::n_distinct(d$Variable),
        "  | conditions: ", dplyr::n_distinct(d$Treatment),
        "  | wells: ", nrow(unique(d[, c("Experiment", "Well")])))
message("  undefined normalisations remaining: ", sum(is.na(d$Normalized_Value)))

res <- discovery_run(
  d,
  outdir             = OUTDIR,
  group_var          = "Treatment",
  baseline_timepoint = "DIV7",           # earliest in the analysis window
  value_column       = "Normalized_Value",
  # Values are a fraction of the plate's vehicle controls, not of each well's own
  # DIV7 reading, so the quantity that must not be ~0 is the control mean. Every
  # DIV is a real measurement here -- none is 1 by construction -- so none is
  # dropped from the move ranking.
  divisor_column     = "Control_Value",
  verbose            = TRUE
)

# Keep the adapter's own provenance beside the findings, so the dump is readable
# without re-deriving how the data got there.
saveRDS(list(adapted_params = adapted$processing_params,
             analysis_timepoints = ANALYSIS_TIMEPOINTS,
             pca_metrics = adapted$pca_metrics),
        file.path(OUTDIR, "adapter_params.rds"))

message("\nDiscovery complete -> ", OUTDIR)
invisible(res)

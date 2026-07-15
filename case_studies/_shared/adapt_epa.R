# case_studies/_shared/adapt_epa.R
#
# Maps the EPA/Shafer DNT MEA export (Brown et al. 2016) into the NOVA processed
# schema. See ../01_epa_dnt_ontogeny/SOURCE.md for provenance and for the two
# structural facts that shape this file:
#
#   1. `Plate.SN` is a physical plate serial number and plates are REUSED across
#      culture dates (4 serials, 6 experiments). Experiment identity is therefore
#      (date, Plate.SN). Keying on the serial alone merges two different cultures
#      into one well identity -- the cross-plate collision NOVA 0.4.0 fixed.
#
#   2. DIV 5 cannot serve as a normalisation baseline: 42-92% of wells read
#      exactly zero there, because cortical networks are near-silent at DIV 5 and
#      their development is the phenomenon being measured. Dividing by it would
#      discard most of the data and keep the already-active wells -- a selection
#      effect dressed as a result.
#
# So normalisation here is to the plate's vehicle controls at the same DIV
# (the toxicology convention), not to each well's own earliest timepoint.
#
# This bypasses process_mea_flexible(): that function parses the Axion row-121
# CSV export, and this is a published tidy table. It emits the same schema
# directly and does its own normalisation.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Metrics the publisher says were not used: "Note: the parameters CVtime and
# CVnetwork were not used." (data.gov dataset description)
EPA_EXCLUDED_METRICS <- c("cv.time", "cv.network")

# Network-spike and burst-interval metrics are 33-42% NA, and that NA is
# structural: a well with no bursts has no burst duration. It means "did not
# burst", not "measurement lost", so mean-imputing it would invent burst
# statistics for wells that never burst. These are kept in the long table (where
# NA can stay NA) but excluded from the PCA metric set.
EPA_STRUCTURAL_NA_METRICS <- c(
  "mean.isis", "mean.dur", "mean.IBIs",
  "ns.peak.m", "ns.durn.m", "ns.mean.insis", "ns.durn.sd", "ns.mean.spikes.in.ns"
)

EPA_METRIC_LABELS <- c(
  meanfiringrate               = "Mean Firing Rate (Hz)",
  burst.per.min                = "Burst Rate (bursts/min)",
  mean.isis                    = "Mean ISI (s)",
  per.spikes.in.burst          = "Spikes in Burst (%)",
  mean.dur                     = "Burst Duration (s)",
  mean.IBIs                    = "Inter-Burst Interval (s)",
  nAE                          = "Number of Active Electrodes",
  nABE                         = "Number of Bursting Electrodes",
  ns.n                         = "Number of Network Spikes",
  ns.peak.m                    = "Network Spike Peak",
  ns.durn.m                    = "Network Spike Duration (s)",
  ns.percent.of.spikes.in.ns   = "Spikes in Network Spike (%)",
  ns.mean.insis                = "Network Spike ISI (s)",
  ns.durn.sd                   = "Network Spike Duration SD",
  ns.mean.spikes.in.ns         = "Spikes per Network Spike",
  r                            = "Synchrony Index (r)"
)

#' Adapt the EPA DNT MEA export to the NOVA processed schema
#'
#' @param path Path to the downloaded CSV.
#' @param control_scope Which vehicle-control wells a treated well is normalised
#'   against, within its own experiment and DIV. `"plate"` (default) uses every
#'   dose-0 well on the plate (n = 7-12); `"compound"` uses only that compound's
#'   own dose-0 wells (n = 2). "plate" is the more stable estimate but assumes a
#'   common vehicle across compounds on a plate -- the file records no vehicle,
#'   only that all doses are in uM, so that assumption is unverified. Use
#'   `"compound"` as a sensitivity check.
#' @param verbose Logical.
#' @return A list shaped like a `process_mea_flexible()` result: `raw_data`,
#'   `normalized_data`, `processing_params`, `experiment_name`, plus
#'   `pca_metrics` (the metrics safe to feed a PCA).
adapt_epa <- function(path,
                      control_scope = c("plate", "compound"),
                      verbose = TRUE) {

  control_scope <- match.arg(control_scope)
  if (!file.exists(path)) stop("EPA CSV not found: ", path, call. = FALSE)

  raw <- utils::read.csv(path, stringsAsFactors = FALSE)

  required <- c("date", "Plate.SN", "DIV", "well", "trt", "dose")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("Not the expected EPA export -- missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  metric_cols <- setdiff(names(raw),
                         c(required, "units", "file.name", EPA_EXCLUDED_METRICS))

  long <- raw %>%
    # Experiment identity: the culture, not the piece of plastic.
    mutate(
      Experiment = paste(.data$date, .data$Plate.SN, sep = "_"),
      Well       = .data$well,
      # "DIV5" parses to real time via nova_time_to_minutes(); nova_order_timepoints()
      # then sorts DIV5 < DIV7 < DIV9 < DIV12 rather than alphabetically.
      Timepoint  = paste0("DIV", .data$DIV),
      Compound   = .data$trt,
      Dose       = .data$dose,
      Is_Control = .data$dose == 0,
      Treatment  = ifelse(.data$dose == 0,
                          paste0(.data$trt, " (vehicle)"),
                          sprintf("%s %g uM", .data$trt, .data$dose)),
      Culture_Date = as.character(.data$date),
      Plate_SN     = .data$Plate.SN
    ) %>%
    tidyr::pivot_longer(cols = all_of(metric_cols),
                        names_to = "Metric_Raw", values_to = "Value") %>%
    mutate(Variable = dplyr::coalesce(EPA_METRIC_LABELS[.data$Metric_Raw],
                                      .data$Metric_Raw)) %>%
    as.data.frame()

  # --- normalise to same-plate, same-DIV vehicle controls --------------------
  ctrl_key <- c("Experiment", "Timepoint", "Variable")
  if (control_scope == "compound") ctrl_key <- c(ctrl_key, "Compound")

  controls <- long %>%
    filter(.data$Is_Control) %>%
    group_by(across(all_of(ctrl_key))) %>%
    summarise(Control_Value = mean(.data$Value, na.rm = TRUE),
              n_control_wells = dplyr::n_distinct(.data$Well),
              .groups = "drop")

  normalized <- long %>%
    left_join(controls, by = ctrl_key) %>%
    mutate(
      # A ratio against a control mean of zero is undefined, not infinite. At
      # DIV 5 many controls are genuinely silent, so this is expected and is
      # left as NA rather than papered over.
      Normalized_Value = ifelse(
        is.na(.data$Control_Value) | .data$Control_Value == 0,
        NA_real_, .data$Value / .data$Control_Value)
    ) %>%
    as.data.frame()

  # A left_join must not add rows: the control table is keyed one row per
  # (experiment, DIV, metric[, compound]), so anything else means the key is wrong.
  if (nrow(normalized) != nrow(long)) {
    stop("Control join changed the row count (", nrow(long), " -> ", nrow(normalized),
         "). The control key is not unique.", call. = FALSE)
  }

  schema_cols <- c("Variable", "Well", "Value", "Treatment", "Experiment",
                   "Timepoint", "Compound", "Dose", "Is_Control",
                   "Culture_Date", "Plate_SN")
  raw_data <- long[, schema_cols, drop = FALSE]
  raw_data$Original_Timepoint <- raw_data$Timepoint
  normalized_data <- normalized[, c(schema_cols, "Normalized_Value",
                                    "Control_Value", "n_control_wells"), drop = FALSE]
  normalized_data$Original_Timepoint <- normalized_data$Timepoint

  pca_metrics <- setdiff(
    unname(EPA_METRIC_LABELS[setdiff(metric_cols, EPA_STRUCTURAL_NA_METRICS)]), NA)

  if (verbose) {
    message("=== EPA ADAPTER ===")
    message("  rows: ", nrow(raw_data), " (", nrow(raw), " wide x ",
            length(metric_cols), " metrics)")
    message("  experiments (date x plate): ", dplyr::n_distinct(raw_data$Experiment),
            " | plate serials: ", dplyr::n_distinct(raw$Plate.SN),
            "  <- serials are reused; identity is the pair")
    message("  wells (experiment x well): ",
            nrow(unique(raw_data[, c("Experiment", "Well")])),
            " | well IDs alone: ", dplyr::n_distinct(raw_data$Well))
    message("  timepoints: ", paste(NOVA::nova_order_timepoints(raw_data$Timepoint),
                                    collapse = ", "))
    message("  conditions: ", dplyr::n_distinct(raw_data$Treatment),
            " | compounds: ", dplyr::n_distinct(raw_data$Compound))
    message("  normalised to: ", control_scope, "-level vehicle controls, same DIV")
    message("  metrics for PCA: ", length(pca_metrics), " of ", length(metric_cols),
            " (dropped ", length(EPA_STRUCTURAL_NA_METRICS), " with structural NA, ",
            length(EPA_EXCLUDED_METRICS), " unused per EPA)")
    nz <- sum(is.na(normalized_data$Normalized_Value))
    message("  undefined normalisations (control mean = 0): ", nz, " of ",
            nrow(normalized_data), " (", round(100 * nz / nrow(normalized_data), 1), "%)")
  }

  list(
    raw_data = raw_data,
    normalized_data = normalized_data,
    pca_metrics = pca_metrics,
    processing_params = list(
      source_file = path,
      control_scope = control_scope,
      excluded_metrics = EPA_EXCLUDED_METRICS,
      structural_na_metrics = EPA_STRUCTURAL_NA_METRICS,
      grouping_variables = "Treatment",
      baseline_timepoint = NULL   # normalisation is vs control, not vs a timepoint
    ),
    experiment_name = "EPA_Brown2016_DNT"
  )
}

# table_input.R
# process_mea_table(): ingest an already-tidy metrics table.
#
# process_mea_flexible() parses the Axion "Neural Metrics" CSV export, which is a
# fixed layout with metadata on rows 121-124. Published datasets almost never look
# like that -- they arrive as a plain table, one row per well x timepoint with
# metrics in columns, or already in long form. This function maps such a table onto
# the same schema so the rest of NOVA works unchanged.

#' Process an already-tidy MEA metrics table
#'
#' Maps a published or hand-assembled metrics table onto NOVA's processed schema,
#' so that data which never came from an Axion CSV export can still be fed to
#' \code{\link{pca_analysis_enhanced}}, \code{\link{nova_trajectory_summary}},
#' \code{\link{create_mea_heatmaps_enhanced}} and \code{\link{plot_mea_metric}}.
#'
#' Accepts wide input (one row per well x timepoint, metrics in columns) or long
#' input (one row per well x timepoint x metric). Optionally normalises, either to
#' each well's own baseline timepoint or to control wells measured alongside it.
#'
#' @section Experiment identity:
#' \code{experiment} may name several columns, and often must. Well IDs repeat on
#' every plate, so a well is only identified once its experiment is known — and the
#' experiment itself is not always one column. In the EPA DNT dataset, plate serial
#' numbers are reused across culture dates, so identity is \code{c("date", "Plate.SN")}:
#' keying on the serial alone merges two different cultures into one well. Pass every
#' column needed; they are pasted into a single \code{Experiment}.
#'
#' @section Normalisation:
#' \describe{
#'   \item{\code{"none"}}{Leave values as they are. \code{Normalized_Value} is not created.}
#'   \item{\code{"baseline"}}{Each well against its own value at \code{baseline_timepoint},
#'     keyed on experiment + well + metric + grouping. A fold-change over time.}
#'   \item{\code{"control"}}{Each well against the mean of the control wells in the same
#'     experiment at the same timepoint — the toxicology convention, and the only option
#'     when the earliest timepoint is not a usable reference (e.g. a developmental assay
#'     where every well is silent at the first timepoint, so the ratio is undefined).}
#' }
#' Both are ratios: asymmetric (halving is 0.5, doubling is 2.0) and undefined against a
#' zero divisor, which yields \code{NA} rather than \code{Inf}.
#'
#' @param data A data frame.
#' @param experiment Character vector of column(s) identifying the experiment / plate /
#'   culture. Pasted together when more than one. See the section above — this is the
#'   argument most often got wrong.
#' @param well Column naming the well.
#' @param timepoint Column naming the timepoint. Values are parsed by
#'   \code{\link{nova_time_to_minutes}}, which understands \code{"DIV7"}, \code{"1h30"},
#'   \code{"90min"} and bare numbers, so \code{"DIV"} columns work directly.
#' @param treatment Column naming the condition/group.
#' @param genotype Optional column naming the genotype.
#' @param metrics Character vector of metric columns (wide input). Leave \code{NULL} for
#'   long input and supply \code{variable_column}/\code{value_column} instead.
#' @param variable_column,value_column Metric-name and value columns (long input).
#' @param exclude_metrics Metric columns to drop, e.g. ones the publisher states were
#'   not used.
#' @param metric_labels Optional named character vector renaming metrics for display,
#'   \code{c(meanfiringrate = "Mean Firing Rate (Hz)")}.
#' @param timepoint_prefix Optional prefix for bare numeric timepoints, e.g. \code{"DIV"}
#'   turns \code{5} into \code{"DIV5"}.
#' @param normalize One of \code{"none"}, \code{"baseline"}, \code{"control"}.
#' @param baseline_timepoint Timepoint label used when \code{normalize = "baseline"}.
#'   Defaults to the first in \code{\link{nova_order_timepoints}} order.
#' @param control Logical vector, one per row of \code{data}, marking control wells.
#'   Required when \code{normalize = "control"}. Given explicitly rather than guessed:
#'   e.g. \code{control = df$dose == 0}.
#' @param control_within Optional extra columns a control must match before it can serve
#'   as a reference, e.g. \code{"Compound"} to use each compound's own vehicle wells
#'   rather than every control on the plate. Fewer, better-matched controls versus more,
#'   more stable ones — a real trade-off, so it is yours to make.
#' @param verbose Logical.
#'
#' @return A list shaped like \code{\link{process_mea_flexible}}'s: \code{raw_data},
#'   \code{normalized_data} (\code{NULL} when \code{normalize = "none"}),
#'   \code{processing_params}, \code{experiment_name}. When
#'   \code{normalize = "control"}, \code{normalized_data} also carries
#'   \code{Control_Value} and \code{n_control_wells}, so the divisor can be inspected
#'   rather than taken on trust.
#'
#' @examples
#' # A wide published table: one row per well x timepoint, metrics in columns.
#' # Note plate P2 reads twice P1 throughout -- a plate effect, which is exactly
#' # what normalisation has to respect rather than smear across plates.
#' df <- expand.grid(well = c("A1", "A2"), plate = c("P1", "P2"), div = c(5, 7),
#'                   stringsAsFactors = FALSE)
#' df$drug        <- ifelse(df$well == "A1", "ctrl", "cpd")
#' df$firing_rate <- c(1, 2, 2, 4, 1.5, 3, 3, 6)
#' df$n_bursts    <- c(2, 5, 4, 10, 3, 6, 6, 12)
#'
#' # Each well against its own DIV5 reading.
#' res <- process_mea_table(
#'   df, experiment = "plate", well = "well", timepoint = "div",
#'   treatment = "drug", metrics = c("firing_rate", "n_bursts"),
#'   timepoint_prefix = "DIV", normalize = "baseline", verbose = FALSE
#' )
#' head(res$normalized_data)
#'
#' # Or against the control wells on the same plate at the same timepoint.
#' res2 <- process_mea_table(
#'   df, experiment = "plate", well = "well", timepoint = "div",
#'   treatment = "drug", metrics = c("firing_rate", "n_bursts"),
#'   timepoint_prefix = "DIV", normalize = "control",
#'   control = df$drug == "ctrl", verbose = FALSE
#' )
#' head(res2$normalized_data)
#'
#' # Plate serials reused across culture dates? Identity is both columns.
#' # process_mea_table(df, experiment = c("date", "plate"), ...)
#' @export
process_mea_table <- function(data,
                              experiment,
                              well,
                              timepoint,
                              treatment,
                              genotype = NULL,
                              metrics = NULL,
                              variable_column = NULL,
                              value_column = NULL,
                              exclude_metrics = NULL,
                              metric_labels = NULL,
                              timepoint_prefix = NULL,
                              normalize = c("none", "baseline", "control"),
                              baseline_timepoint = NULL,
                              control = NULL,
                              control_within = NULL,
                              verbose = TRUE) {

  normalize <- match.arg(normalize)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # --- validate the mapping before touching anything -------------------------
  named <- c(experiment = experiment, well = well, timepoint = timepoint,
             treatment = treatment, genotype = genotype,
             variable_column = variable_column, value_column = value_column,
             control_within = control_within)
  missing <- setdiff(unname(named), names(data))
  if (length(missing)) {
    stop("Column(s) not found in `data`: ", paste(missing, collapse = ", "),
         ".\nAvailable: ", paste(names(data), collapse = ", "), call. = FALSE)
  }

  long_input <- !is.null(variable_column) && !is.null(value_column)
  if (is.null(metrics) && !long_input) {
    stop("Supply either `metrics` (wide input) or both `variable_column` and ",
         "`value_column` (long input).", call. = FALSE)
  }
  if (!is.null(metrics)) {
    missing_m <- setdiff(metrics, names(data))
    if (length(missing_m)) {
      stop("`metrics` column(s) not found: ", paste(missing_m, collapse = ", "),
           call. = FALSE)
    }
  }

  # --- identity --------------------------------------------------------------
  # Pasted, not chosen: when several columns are needed to identify an experiment,
  # dropping any of them merges distinct cultures into one well.
  data$.experiment <- if (length(experiment) > 1L) {
    do.call(paste, c(data[experiment], sep = "_"))
  } else {
    as.character(data[[experiment]])
  }

  tp <- as.character(data[[timepoint]])
  if (!is.null(timepoint_prefix)) tp <- paste0(timepoint_prefix, tp)
  data$.timepoint <- tp

  # --- reshape ---------------------------------------------------------------
  if (long_input) {
    long <- data
    long$Variable <- as.character(long[[variable_column]])
    long$Value    <- as.numeric(long[[value_column]])
  } else {
    keep_metrics <- setdiff(metrics, exclude_metrics)
    if (!length(keep_metrics)) stop("No metrics left after `exclude_metrics`.", call. = FALSE)
    long <- tidyr::pivot_longer(data, cols = dplyr::all_of(keep_metrics),
                               names_to = "Variable", values_to = "Value")
    long <- as.data.frame(long, stringsAsFactors = FALSE)
    long$Value <- as.numeric(long$Value)
  }
  if (!is.null(exclude_metrics)) {
    long <- long[!long$Variable %in% exclude_metrics, , drop = FALSE]
  }
  if (!is.null(metric_labels)) {
    hit <- long$Variable %in% names(metric_labels)
    long$Variable[hit] <- unname(metric_labels[long$Variable[hit]])
  }

  # `control` is indexed against the ORIGINAL rows, so it has to be carried across
  # the reshape rather than re-evaluated afterwards.
  if (!is.null(control)) {
    if (!is.logical(control) || length(control) != nrow(data)) {
      stop("`control` must be a logical vector with one element per row of `data` ",
           "(got ", class(control)[1], " of length ", length(control), ").", call. = FALSE)
    }
    data$.is_control <- control
    if (long_input) {
      long$Is_Control <- data$.is_control
    } else {
      long$Is_Control <- rep(data$.is_control, each = length(setdiff(metrics, exclude_metrics)))
    }
  }

  out <- data.frame(
    Variable   = long$Variable,
    Well       = as.character(long[[well]]),
    Value      = long$Value,
    Treatment  = as.character(long[[treatment]]),
    Experiment = long$.experiment,
    Timepoint  = long$.timepoint,
    stringsAsFactors = FALSE
  )
  if (!is.null(genotype)) out$Genotype <- as.character(long[[genotype]])
  if (!is.null(control))  out$Is_Control <- long$Is_Control
  for (cw in control_within) out[[cw]] <- long[[cw]]
  out$Original_Timepoint <- out$Timepoint

  # Carry any remaining descriptive columns through untouched: a study's own
  # covariates (dose, culture date, compound) are usually what makes its figures
  # interpretable, and re-joining them later is where identity gets lost.
  passthrough <- setdiff(names(data),
                         c(experiment, well, timepoint, treatment, genotype,
                           metrics, variable_column, value_column,
                           ".experiment", ".timepoint", ".is_control"))
  for (p in passthrough) {
    if (!p %in% names(out)) out[[p]] <- long[[p]]
  }

  dup <- anyDuplicated(out[, c("Experiment", "Well", "Timepoint", "Variable")])
  if (dup > 0) {
    warning("`data` holds more than one value per (experiment, well, timepoint, metric) ",
            "-- row ", dup, " is a repeat. Downstream steps will average them. If these ",
            "are distinct samples, a column separating them is missing from `experiment`.")
  }

  if (verbose) {
    message("=== PROCESSING MEA TABLE ===")
    message("  rows: ", nrow(out), " (", nrow(data), " input rows x ",
            dplyr::n_distinct(out$Variable), " metrics)")
    message("  experiments: ", dplyr::n_distinct(out$Experiment),
            " (from ", paste(experiment, collapse = " + "), ")")
    message("  wells: ", nrow(unique(out[, c("Experiment", "Well")])),
            " | well IDs alone: ", dplyr::n_distinct(out$Well))
    message("  timepoints: ", paste(nova_order_timepoints(out$Timepoint), collapse = ", "))
    message("  conditions: ", dplyr::n_distinct(out$Treatment))
  }

  normalized <- NULL
  if (normalize == "baseline") {
    if (is.null(baseline_timepoint)) {
      baseline_timepoint <- nova_order_timepoints(out$Timepoint)[1]
      if (verbose) message("  baseline: ", baseline_timepoint, " (earliest)")
    }
    normalized <- .nova_normalize_baseline(out, baseline_timepoint, verbose)
  } else if (normalize == "control") {
    if (is.null(control)) {
      stop("`normalize = \"control\"` needs `control`: a logical vector marking the ",
           "control wells, e.g. control = df$dose == 0.", call. = FALSE)
    }
    normalized <- .nova_normalize_control(out, control_within, verbose)
  }

  list(
    raw_data = out,
    normalized_data = normalized,
    processing_params = list(
      experiment = experiment, well = well, timepoint = timepoint,
      treatment = treatment, genotype = genotype,
      metrics = metrics, exclude_metrics = exclude_metrics,
      normalize = normalize, baseline_timepoint = baseline_timepoint,
      control_within = control_within,
      grouping_variables = c("Treatment", if (!is.null(genotype)) "Genotype")
    ),
    experiment_name = paste(sort(unique(out$Experiment)), collapse = "_")
  )
}

# Internal: fold-change against each well's own baseline timepoint.
.nova_normalize_baseline <- function(d, baseline_timepoint, verbose = TRUE) {
  if (!baseline_timepoint %in% d$Timepoint) {
    stop("`baseline_timepoint` '", baseline_timepoint, "' is not present. Available: ",
         paste(unique(d$Timepoint), collapse = ", "), call. = FALSE)
  }
  key <- c("Experiment", "Well", "Variable", "Treatment")
  key <- key[key %in% names(d)]

  base <- d[d$Timepoint == baseline_timepoint, c(key, "Value"), drop = FALSE]
  names(base)[names(base) == "Value"] <- "Baseline_Value"
  if (anyDuplicated(base[, key, drop = FALSE]) > 0) {
    stop("More than one baseline row per (", paste(key, collapse = ", "),
         "). The baseline is ambiguous; add the column that separates them to ",
         "`experiment`.", call. = FALSE)
  }

  n_before <- nrow(d)
  out <- merge(d, base, by = key, all.x = TRUE, sort = FALSE)
  if (nrow(out) != n_before) {
    stop("Baseline join changed the row count (", n_before, " -> ", nrow(out),
         "). The baseline key is not unique.", call. = FALSE)
  }
  out$Normalized_Value <- ifelse(is.na(out$Baseline_Value) | out$Baseline_Value == 0,
                                 NA_real_, out$Value / out$Baseline_Value)
  out$Baseline_Value <- NULL
  .nova_report_undefined(out, "baseline is 0", verbose)
  out
}

# Internal: fraction of the control wells in the same experiment and timepoint.
.nova_normalize_control <- function(d, control_within = NULL, verbose = TRUE) {
  if (!"Is_Control" %in% names(d)) stop("Internal: Is_Control missing.", call. = FALSE)
  if (!any(d$Is_Control)) stop("`control` marks no rows as controls.", call. = FALSE)

  key <- c("Experiment", "Timepoint", "Variable", control_within)
  ctrl <- d[d$Is_Control, , drop = FALSE] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(Control_Value = mean(.data$Value, na.rm = TRUE),
                     n_control_wells = dplyr::n_distinct(.data$Well),
                     .groups = "drop") |>
    as.data.frame()

  n_before <- nrow(d)
  out <- merge(d, ctrl, by = key, all.x = TRUE, sort = FALSE)
  if (nrow(out) != n_before) {
    stop("Control join changed the row count (", n_before, " -> ", nrow(out),
         "). The control key is not unique.", call. = FALSE)
  }
  out$Normalized_Value <- ifelse(is.na(out$Control_Value) | out$Control_Value == 0,
                                 NA_real_, out$Value / out$Control_Value)
  if (verbose) {
    message("  normalised to: control wells within (", paste(key, collapse = ", "), ")")
    message("  control wells per reference: ",
            paste(range(ctrl$n_control_wells), collapse = "-"))
  }
  .nova_report_undefined(out, "control mean is 0", verbose)
  out
}

# Internal: a ratio against zero is undefined, not infinite. Say how often.
.nova_report_undefined <- function(d, why, verbose) {
  n <- sum(is.na(d$Normalized_Value))
  if (n > 0 && isTRUE(verbose)) {
    message("  undefined normalisations (", why, "): ", n, " of ", nrow(d),
            " (", round(100 * n / nrow(d), 1), "%) -> NA")
  }
  invisible(NULL)
}

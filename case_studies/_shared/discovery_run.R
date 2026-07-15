# case_studies/_shared/discovery_run.R
#
# Stage 1 of every case study: the uncurated "what's there" scan.
#
# Takes any dataset already in the NOVA processed schema, runs the standard
# NOVA analyses over every metric and every group, writes the full exploratory
# output to disk, and emits a machine-readable `findings` table ranking what
# moved. It selects nothing and concludes nothing -- stage 2 curates from
# `findings`.
#
# EXPLORATORY BY CONSTRUCTION. This scans many metrics x groups x timepoints and
# sorts by effect size. Sorting a large family of noisy estimates puts the
# luckiest at the top, so the head of `findings` is inflated by selection. Nothing
# here is a tested claim, and no p-value is computed -- ranking by significance
# across a scan this wide would only dress the same problem in inferential
# clothing.
#
# Expected input schema (what process_mea_flexible() emits):
#   Variable | Well | Value | Normalized_Value | Treatment | Experiment |
#   Timepoint | Original_Timepoint
# `Experiment` is load-bearing: well IDs repeat across plates, so it is what
# distinguishes two wells. See NOVA 0.4.0 NEWS.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.dr_msg <- function(verbose, ...) if (isTRUE(verbose)) message("  ", ...)

.dr_save <- function(plot, path, width = 9, height = 6, dpi = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(ggplot2::ggsave(path, plot = plot, width = width,
                                   height = height, dpi = dpi))
  invisible(path)
}

# Rank within a finding kind, largest effect first. Ties keep a stable order so
# reruns produce identical files.
.dr_rank <- function(df) {
  if (nrow(df) == 0L) return(df)
  df <- df[order(-df$effect, df$group, df$metric, df$timepoint), , drop = FALSE]
  df$rank_within_kind <- seq_len(nrow(df))
  df
}

#' Flag observations whose normalisation divisor is too close to zero
#'
#' Normalisation is a ratio, so a divisor near zero produces a fold-change of
#' arbitrary size that reflects the divisor rather than anything about the well.
#' Left in, these dominate an effect-size ranking with pure arithmetic artefacts.
#' The threshold is scale-free -- relative to each metric's own typical divisor --
#' because metrics span spike counts and sub-hertz rates.
#'
#' **The divisor is not always the baseline timepoint.** Normalising each well to
#' its own earliest timepoint makes the divisor that well's baseline `Value`;
#' normalising to same-plate vehicle controls (the toxicology convention) makes it
#' a separate control column. Guarding the wrong quantity is worse than not
#' guarding, so the caller says which it is.
#'
#' @param d Processed data, long format.
#' @param baseline_timepoint Label of the baseline timepoint. Used only when
#'   `divisor_column` is NULL.
#' @param divisor_column Name of a column holding the actual divisor (e.g.
#'   `"Control_Value"`). When NULL, the divisor is taken to be each well's own
#'   `Value` at `baseline_timepoint`.
#' @param rel_floor Fraction of a metric's median divisor below which the divisor
#'   is considered unusable (default 0.001).
#' @return `d` with a logical `near_zero_baseline` column added.
.dr_flag_near_zero_baseline <- function(d, baseline_timepoint,
                                        divisor_column = NULL, rel_floor = 0.001) {

  if (!is.null(divisor_column)) {
    if (!divisor_column %in% names(d)) {
      stop("`divisor_column` '", divisor_column, "' is not a column.", call. = FALSE)
    }
    d$.baseline_value <- d[[divisor_column]]
  } else {
    key <- c("Experiment", "Well", "Variable")
    key <- key[key %in% names(d)]

    base <- d[d$Timepoint == baseline_timepoint, c(key, "Value"), drop = FALSE]
    names(base)[names(base) == "Value"] <- ".baseline_value"
    base <- base[!duplicated(base[, key, drop = FALSE]), , drop = FALSE]

    d <- merge(d, base, by = key, all.x = TRUE, sort = FALSE)
  }

  scale_by_metric <- d %>%
    filter(!is.na(.data$.baseline_value), .data$.baseline_value > 0) %>%
    group_by(.data$Variable) %>%
    summarise(.metric_scale = stats::median(.data$.baseline_value, na.rm = TRUE),
              .groups = "drop")

  d <- merge(d, scale_by_metric, by = "Variable", all.x = TRUE, sort = FALSE)
  d$near_zero_baseline <- is.na(d$.baseline_value) |
    d$.baseline_value <= 0 |
    (!is.na(d$.metric_scale) & d$.baseline_value < rel_floor * d$.metric_scale)
  d
}

# ---------------------------------------------------------------------------
# Finding builders. Each returns a data frame with the shared findings columns.
# ---------------------------------------------------------------------------

.dr_findings_moves <- function(d, group_var, baseline_timepoint,
                               drop_baseline_timepoint = TRUE) {
  unit_key <- intersect(c("Experiment", "Well"), names(d))

  usable <- d %>%
    filter(!.data$near_zero_baseline,
           is.finite(.data$Normalized_Value),
           .data$Normalized_Value > 0)

  # When each well is normalised to its own baseline timepoint, that timepoint is
  # 1.0 by construction and carries no information. When the divisor is something
  # else -- vehicle controls, say -- every timepoint is a real measurement and
  # dropping one would discard a third of the evidence.
  if (isTRUE(drop_baseline_timepoint)) {
    usable <- usable %>% filter(.data$Timepoint != baseline_timepoint)
  }

  if (nrow(usable) == 0L) return(NULL)

  usable$.log2fc <- log2(usable$Normalized_Value)
  usable$.unit <- do.call(paste, c(usable[unit_key], sep = "_"))

  usable %>%
    group_by(.data[[group_var]], .data$Variable, .data$Timepoint) %>%
    summarise(
      median_log2_fc = stats::median(.data$.log2fc, na.rm = TRUE),
      n_wells        = dplyr::n_distinct(.data$.unit),
      n_plates       = if ("Experiment" %in% names(usable)) {
                         dplyr::n_distinct(.data$Experiment)
                       } else NA_integer_,
      .groups = "drop"
    ) %>%
    transmute(
      kind        = "move",
      group       = as.character(.data[[group_var]]),
      metric      = as.character(.data$Variable),
      timepoint   = as.character(.data$Timepoint),
      component   = NA_character_,
      effect      = abs(.data$median_log2_fc),
      effect_type = "abs_median_log2_fc",
      direction   = ifelse(.data$median_log2_fc >= 0, "up", "down"),
      n_wells     = .data$n_wells,
      n_plates    = .data$n_plates
    ) %>%
    as.data.frame()
}

.dr_findings_loadings <- function(pca, n_pcs = 2L) {
  rot <- pca$pca_result$rotation
  if (is.null(rot)) return(NULL)
  n_pcs <- min(n_pcs, ncol(rot))
  ve <- pca$variance_explained

  out <- lapply(seq_len(n_pcs), function(i) {
    pc <- colnames(rot)[i]
    data.frame(
      kind        = "loading",
      group       = NA_character_,
      metric      = rownames(rot),
      timepoint   = NA_character_,
      component   = pc,
      effect      = abs(rot[, i]),
      effect_type = "abs_loading",
      direction   = ifelse(rot[, i] >= 0, "up", "down"),
      n_wells     = NA_integer_,
      n_plates    = NA_integer_,
      variance_explained = if (!is.null(ve) && pc %in% names(ve)) as.numeric(ve[[pc]]) else NA_real_,
      stringsAsFactors = FALSE, row.names = NULL
    )
  })
  do.call(rbind, out)
}

.dr_findings_trajectory <- function(traj) {
  m <- traj$metrics
  if (is.null(m) || nrow(m) == 0L) return(NULL)
  rbind(
    data.frame(
      kind = "trajectory_distance", group = m$group, metric = NA_character_,
      timepoint = m$peak_timepoint, component = NA_character_,
      effect = m$net_displacement, effect_type = "net_displacement",
      direction = NA_character_, n_wells = NA_integer_, n_plates = NA_integer_,
      stringsAsFactors = FALSE
    ),
    # Directness is reported as 1 - directness so that "least direct path" ranks
    # like every other finding: larger effect = more notable.
    data.frame(
      kind = "trajectory_wandering", group = m$group, metric = NA_character_,
      timepoint = NA_character_, component = NA_character_,
      effect = ifelse(is.na(m$directness), NA_real_, 1 - m$directness),
      effect_type = "one_minus_directness",
      direction = NA_character_, n_wells = NA_integer_, n_plates = NA_integer_,
      stringsAsFactors = FALSE
    )
  )
}

.dr_findings_separation <- function(pca, group_var, dims = c("PC1", "PC2")) {
  pd <- pca$plot_data
  if (is.null(pd) || !group_var %in% names(pd)) return(NULL)
  dims <- dims[dims %in% names(pd)]
  if (length(dims) < 2L) return(NULL)

  cent <- pd %>%
    group_by(.data[[group_var]]) %>%
    summarise(across(all_of(dims), \(v) mean(v, na.rm = TRUE)), .groups = "drop")
  if (nrow(cent) < 2L) return(NULL)

  pairs <- utils::combn(nrow(cent), 2)
  out <- lapply(seq_len(ncol(pairs)), function(k) {
    i <- pairs[1, k]; j <- pairs[2, k]
    dist <- sqrt(sum((as.numeric(cent[i, dims]) - as.numeric(cent[j, dims]))^2))
    data.frame(
      kind = "group_separation",
      group = paste(cent[[group_var]][i], "vs", cent[[group_var]][j]),
      metric = NA_character_, timepoint = NA_character_,
      component = paste(dims, collapse = "/"),
      effect = dist, effect_type = "centroid_distance",
      direction = NA_character_, n_wells = NA_integer_, n_plates = NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Main entry point
# ---------------------------------------------------------------------------

#' Run the generic NOVA discovery pass over a processed dataset
#'
#' @param processed Either a `process_mea_flexible()` result or a data frame in
#'   the NOVA processed schema.
#' @param outdir Directory for the exploratory dump (gitignored by convention).
#' @param group_var Grouping column defining conditions (default "Treatment").
#' @param baseline_timepoint Baseline label. Inferred via `nova_order_timepoints()`
#'   when NULL.
#' @param value_column Value column for PCA. Inferred: prefers Normalized_Value.
#' @param divisor_column Column holding the quantity `Normalized_Value` was divided
#'   by, when that is not each well's own baseline timepoint — e.g.
#'   `"Control_Value"` for normalisation against vehicle controls. Supplying it
#'   makes the near-zero guard check the real divisor, and keeps every timepoint in
#'   the move ranking (none of them is 1 by construction).
#' @param top_metrics How many metrics get a per-metric plot (0 = all). Plotting
#'   every metric x group is the slow step; what is skipped is logged, never
#'   silently dropped.
#' @param verbose Logical.
#' @return Invisibly, a list with `findings`, `pca`, `trajectory` and `manifest`.
discovery_run <- function(processed,
                          outdir,
                          group_var = "Treatment",
                          baseline_timepoint = NULL,
                          value_column = NULL,
                          divisor_column = NULL,
                          top_metrics = 0L,
                          verbose = TRUE) {

  stopifnot(is.character(outdir), length(outdir) == 1L)
  if (!requireNamespace("NOVA", quietly = TRUE)) {
    stop("NOVA is not available. Run devtools::load_all() at the package root first.")
  }

  d <- if (is.data.frame(processed)) processed else {
    processed$normalized_data %||% processed$raw_data
  }
  if (is.null(d) || nrow(d) == 0L) stop("No usable data in `processed`.")

  if (is.null(value_column)) {
    value_column <- if ("Normalized_Value" %in% names(d)) "Normalized_Value" else "Value"
  }
  if (!group_var %in% names(d)) {
    stop("`group_var` '", group_var, "' is not a column. Available: ",
         paste(names(d), collapse = ", "))
  }
  if (is.null(baseline_timepoint)) {
    baseline_timepoint <- NOVA::nova_order_timepoints(d$Timepoint)[1]
  }

  if (verbose) {
    message("=== DISCOVERY PASS ===")
    message("  rows: ", nrow(d),
            " | metrics: ", dplyr::n_distinct(d$Variable),
            " | groups: ", dplyr::n_distinct(d[[group_var]]),
            " | timepoints: ", dplyr::n_distinct(d$Timepoint))
    message("  baseline: ", baseline_timepoint, " | value column: ", value_column)
  }

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  manifest <- character(0)

  # -- validity guard --------------------------------------------------------
  d <- .dr_flag_near_zero_baseline(d, baseline_timepoint, divisor_column = divisor_column)
  n_flagged <- sum(d$near_zero_baseline, na.rm = TRUE)
  if (n_flagged > 0 && verbose) {
    .dr_msg(verbose, "excluded from ranking: ", n_flagged, " observation(s) of ",
            nrow(d), " whose divisor (",
            if (is.null(divisor_column)) paste0("own ", baseline_timepoint, " value") else divisor_column,
            ") is ~0 -- fold-change undefined")
  }

  # -- PCA -------------------------------------------------------------------
  .dr_msg(verbose, "PCA ...")
  pca <- NOVA::pca_analysis_enhanced(
    normalized_data = d, grouping_variables = group_var,
    value_column = value_column, verbose = FALSE)

  pca_plots <- NOVA::pca_plots_enhanced(
    pca_output = pca, grouping_variables = group_var,
    color_variable = group_var, shape_variable = NULL,
    save_plots = FALSE, verbose = FALSE)

  for (nm in names(pca_plots$plots)) {
    manifest <- c(manifest, .dr_save(pca_plots$plots[[nm]],
                                     file.path(outdir, "pca", paste0("pca_", nm, ".pdf"))))
  }
  if (!is.null(pca$elbow_plot)) {
    manifest <- c(manifest, .dr_save(pca$elbow_plot, file.path(outdir, "pca", "pca_scree.pdf")))
  }

  # -- trajectories ----------------------------------------------------------
  .dr_msg(verbose, "trajectories ...")
  traj <- NOVA::nova_trajectory_summary(pca, group_var = group_var, verbose = FALSE)
  for (nm in names(traj$plots)) {
    manifest <- c(manifest, .dr_save(traj$plots[[nm]],
                                     file.path(outdir, "trajectories", paste0(nm, ".pdf"))))
  }

  # -- heatmaps --------------------------------------------------------------
  .dr_msg(verbose, "heatmaps ...")
  hm <- tryCatch(
    NOVA::create_mea_heatmaps_enhanced(
      data = d, value_column = value_column, grouping_columns = group_var,
      save_plots = FALSE, verbose = FALSE),
    error = function(e) { .dr_msg(verbose, "heatmaps skipped: ", conditionMessage(e)); NULL })

  if (!is.null(hm)) {
    for (nm in setdiff(names(hm), "metadata")) {
      obj <- hm[[nm]]$heatmap
      if (is.null(obj) || !inherits(obj, "pheatmap")) next
      p <- file.path(outdir, "heatmaps", paste0(nm, ".pdf"))
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      grDevices::pdf(p, width = 10, height = 8); print(obj); grDevices::dev.off()
      manifest <- c(manifest, p)
    }
  }

  # -- per-metric plots ------------------------------------------------------
  metrics <- unique(d$Variable)
  plotted <- if (top_metrics > 0L && top_metrics < length(metrics)) {
    metrics[seq_len(top_metrics)]
  } else metrics
  if (length(plotted) < length(metrics)) {
    message("  NOTE: plotting ", length(plotted), " of ", length(metrics),
            " metrics; skipped: ",
            paste(setdiff(metrics, plotted), collapse = ", "))
  }
  .dr_msg(verbose, "per-metric plots (", length(plotted), ") ...")
  for (m in plotted) {
    p <- tryCatch(
      NOVA::plot_mea_metric(d, metric = m, group_by = group_var,
                            value_column = value_column, plot_type = "box"),
      error = function(e) NULL)
    if (is.null(p)) next
    fname <- paste0(gsub("[^A-Za-z0-9]+", "_", m), ".pdf")
    manifest <- c(manifest, .dr_save(p, file.path(outdir, "metrics", fname)))
  }

  # -- findings --------------------------------------------------------------
  .dr_msg(verbose, "ranking findings ...")
  parts <- list(
    .dr_rank(.dr_findings_moves(d, group_var, baseline_timepoint,
                                drop_baseline_timepoint = is.null(divisor_column))),
    .dr_rank(.dr_findings_loadings(pca)),
    .dr_rank(.dr_findings_trajectory(traj)),
    .dr_rank(.dr_findings_separation(pca, group_var))
  )
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, parts)

  all_cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(p) {
    for (cc in setdiff(all_cols, names(p))) p[[cc]] <- NA
    p[, all_cols, drop = FALSE]
  })
  findings <- do.call(rbind, parts)
  findings <- findings[!is.na(findings$effect), , drop = FALSE]

  # Provenance travels with the findings: a ranking is not interpretable without
  # knowing what was scanned to produce it.
  attr(findings, "scan") <- list(
    n_metrics = length(metrics), n_groups = dplyr::n_distinct(d[[group_var]]),
    n_timepoints = dplyr::n_distinct(d$Timepoint),
    baseline = baseline_timepoint, value_column = value_column,
    n_excluded_near_zero_baseline = n_flagged,
    n_comparisons_scanned = sum(findings$kind == "move")
  )

  utils::write.csv(findings, file.path(outdir, "findings.csv"), row.names = FALSE)
  manifest <- c(manifest, file.path(outdir, "findings.csv"))

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(
      list(scan = attr(findings, "scan"),
           caveat = paste("Exploratory. Effects are ranked across many metrics x",
                          "groups x timepoints; the top of this list is inflated by",
                          "selection. No hypothesis was tested."),
           findings = findings),
      file.path(outdir, "findings.json"), auto_unbox = TRUE, pretty = TRUE, digits = 6)
    manifest <- c(manifest, file.path(outdir, "findings.json"))
  }

  out <- list(findings = findings, pca = pca, trajectory = traj,
              data = d, manifest = manifest)

  # Stage 2 renders from this rather than recomputing: the summary must show the
  # same figures the findings were ranked from, not a fresh run that could drift.
  saveRDS(out, file.path(outdir, "discovery.rds"))
  manifest <- c(manifest, file.path(outdir, "discovery.rds"))
  out$manifest <- manifest

  if (verbose) {
    message("  wrote ", length(manifest), " file(s) to ", outdir)
    message("  findings: ", nrow(findings), " across ",
            paste(unique(findings$kind), collapse = ", "))
  }

  invisible(out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

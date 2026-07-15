# dynamics_summary.R
# nova_trajectory_summary(): a simple, honest description of how each condition's
# network moved away from baseline through PCA (or any embedding) state space.
#
# Why this is deliberately simple ------------------------------------------
# A typical MEA timecourse has only a handful of timepoints and a modest number
# of replicate wells. That is enough to describe *how far a condition moved from
# baseline and whether it went straight out or wandered* -- but NOT enough to
# fit velocities, accelerations, "stable vs unstable" regimes, or Markov
# transition models without dressing up noise as dynamics. This function
# therefore reports only robust, descriptive quantities and leans on the
# replicate structure (mean +/- SEM across wells) for its key figure.

# Internal: distance-from-baseline (first timepoint) for every point of every
# trajectory in a nova_trajectories table.
.nova_disp_from_baseline <- function(tr, dims) {
  out <- list()
  for (id in unique(tr$traj_id)) {
    s <- tr[tr$traj_id == id, , drop = FALSE]
    s <- s[order(s$time_rank), , drop = FALSE]
    P <- as.matrix(s[, dims, drop = FALSE])
    b <- P[1, ]
    s$disp <- sqrt(rowSums((P - matrix(b, nrow(P), ncol(P), byrow = TRUE))^2))
    out[[id]] <- s
  }
  do.call(rbind, out)
}

#' Summarise how conditions move through state space
#'
#' A compact, descriptive summary of each group's trajectory through an
#' embedding (PCA / UMAP / latent) relative to its baseline. It reports how far
#' each condition moved, whether it moved directly or wandered, and when its
#' displacement peaked -- and draws the two figures that this kind of data
#' actually supports: distance-from-baseline over time (with replicate error
#' bands) and a state-space trajectory map. It deliberately does *not* compute
#' velocities, regimes, or transition models, which require far richer time
#' series.
#'
#' @param x A \code{pca_analysis_enhanced()} result, a data frame with embedding
#'   + timepoint + grouping columns, or a \code{nova_trajectories} object.
#' @param dims Embedding columns (default \code{c("PC1","PC2")}); the first two
#'   are plotted, all are used for distances.
#' @param group_var Grouping column (auto-detected if \code{NULL}).
#' @param unit_var Replicate column(s) for the error bands. Auto-detected as
#'   \code{c("Experiment", "Well")} when both are present, since well IDs repeat
#'   across plates; set \code{NULL} to disable the bands.
#' @param timepoint_var,timepoint_order Timepoint column / explicit order
#'   (otherwise \code{nova_order_timepoints()}, baseline first).
#' @param verbose Logical.
#' @return An object of class \code{nova_trajectory_summary} with:
#'   \code{metrics} (per group: net displacement, path length, directness =
#'   net/path, peak timepoint, peak/final displacement), \code{displacement}
#'   (per group x timepoint mean +/- SEM), \code{trajectories} (group-mean
#'   paths), and \code{plots} (\code{displacement}, \code{map}).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0, 1, 2, 3, 0, 1, 0, 1),
#'   PC2 = c(0, 0, 0, 0, 0, 1, 0, 1),
#'   Treatment = rep(c("Direct", "Wander"), each = 4),
#'   Well = rep(c("W1", "W1", "W2", "W2"), 2),
#'   Timepoint = rep(c("baseline", "30min", "1h", "2h"), 2)
#' )
#' s <- nova_trajectory_summary(df, group_var = "Treatment", verbose = FALSE)
#' s$metrics
#' @export
nova_trajectory_summary <- function(x,
                                    dims = c("PC1", "PC2"),
                                    group_var = NULL,
                                    unit_var = NULL,
                                    timepoint_var = "Timepoint",
                                    timepoint_order = NULL,
                                    verbose = TRUE) {

  # auto-detect the replicate column(s) for the error bands.
  # "Sample" is deliberately not a candidate: it is a per-observation ID built
  # from the timepoint among other things, so each "replicate" would span a
  # single timepoint and its distance from its own baseline would be 0 by
  # construction -- error bands that are silently, uniformly zero.
  if (!inherits(x, "nova_trajectories") && is.null(unit_var)) {
    pd <- if (is.list(x) && "plot_data" %in% names(x)) x$plot_data else x
    unit_var <- .nova_unit_cols(pd)
    if (length(unit_var) == 0L) {
      unit_var <- .nova_resolve_col(pd, "ID", "ID")
      if (is.na(unit_var)) unit_var <- NULL
    }
  }

  # group-mean trajectory (one path per condition)
  trg <- nova_extract_trajectories(x, dims = dims, group_var = group_var,
                                   unit_var = NULL, timepoint_var = timepoint_var,
                                   timepoint_order = timepoint_order)
  dims <- attr(trg, "dims"); var_exp <- attr(trg, "variance_explained")
  tp_order <- attr(trg, "timepoint_order"); group_var <- attr(trg, "group_var")

  # per-replicate trajectories (for the error bands), if a unit column exists
  tru <- NULL
  if (!is.null(unit_var)) {
    tru <- tryCatch(
      nova_extract_trajectories(x, dims = dims, group_var = group_var, unit_var = unit_var,
                                timepoint_var = timepoint_var, timepoint_order = timepoint_order),
      # Defensive, and currently unreachable: every stop() in
      # nova_extract_trajectories() depends on arguments this call shares with the
      # unwrapped group-mean call above, which would already have failed, and the
      # unit_var-specific branch warns rather than stops. It stays because a
      # swallowed error here is indistinguishable from "no replicate column
      # found" -- which is exactly how the multi-column failure hid.
      error = function(e) {
        warning("Could not build per-replicate trajectories from unit_var '",
                paste(unit_var, collapse = ", "), "': ", conditionMessage(e),
                ". Falling back to the group-mean trajectory without error bands.")
        NULL
      })
    # Report the columns actually used, not the ones requested: extraction drops
    # any that are missing, and params claiming otherwise would misdescribe the
    # figure it accompanies.
    unit_var <- if (is.null(tru)) NULL else attr(tru, "unit_var")
  }

  # A unit resolving to one timepoint cannot describe movement: each unit is its
  # own baseline, so every displacement is 0. Fall back to the group-mean path --
  # correct, just without bands -- rather than drawing a flat line at zero and
  # presenting it as a measurement.
  if (!is.null(tru) && nrow(tru) > 0L && max(table(tru$traj_id)) < 2L) {
    warning("`unit_var` '", paste(unit_var, collapse = ", "), "' has one timepoint per unit, ",
            "so it cannot describe movement over time and no error bands are available. ",
            "Pass a replicate column measured repeatedly across timepoints (e.g. 'Well'). ",
            "Falling back to the group-mean trajectory.")
    tru <- NULL
    unit_var <- NULL
  }

  # --- metrics from the group-mean trajectory --------------------------------
  gd <- .nova_disp_from_baseline(trg, dims)
  metrics <- do.call(rbind, lapply(unique(gd$group), function(g) {
    s <- gd[gd$group == g, , drop = FALSE]; s <- s[order(s$time_rank), ]
    P <- as.matrix(s[, dims, drop = FALSE]); n <- nrow(P)
    path <- if (n >= 2L) sum(sqrt(rowSums((P[-1, , drop = FALSE] - P[-n, , drop = FALSE])^2))) else 0
    net  <- s$disp[n]
    pk   <- which.max(s$disp)
    data.frame(group = g, net_displacement = net, path_length = path,
               directness = if (path > 0) net / path else NA_real_,
               peak_timepoint = as.character(s$time_label[pk]),
               peak_displacement = s$disp[pk], final_displacement = net,
               stringsAsFactors = FALSE)
  }))
  metrics <- tibble::as_tibble(metrics)

  # --- distance-from-baseline over time (mean +/- SEM across replicates) ------
  if (!is.null(tru)) {
    ud <- .nova_disp_from_baseline(tru, dims)
    agg <- ud |>
      dplyr::group_by(.data$group, .data$time_label, .data$time_rank) |>
      dplyr::summarise(mean_disp = mean(.data$disp, na.rm = TRUE),
                       sem_disp  = stats::sd(.data$disp, na.rm = TRUE) / sqrt(dplyr::n()),
                       n = dplyr::n(), .groups = "drop")
  } else {
    agg <- gd[, c("group", "time_label", "time_rank")]
    agg$mean_disp <- gd$disp; agg$sem_disp <- NA_real_; agg$n <- 1L
  }
  agg$time_label <- factor(agg$time_label, levels = tp_order)
  agg <- agg[order(agg$group, agg$time_rank), ]

  groups <- unique(trg$group)
  pal <- stats::setNames(nova_palette(length(groups)), groups)
  xl <- .nova_axis_label(dims[1], var_exp); yl <- .nova_axis_label(dims[2], var_exp)

  # --- plot 1: distance from baseline over time -------------------------------
  p_disp <- ggplot2::ggplot(agg, ggplot2::aes(.data$time_label, .data$mean_disp,
                            colour = .data$group, group = .data$group))
  if (any(!is.na(agg$sem_disp))) {
    p_disp <- p_disp + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$mean_disp - .data$sem_disp,
                   ymax = .data$mean_disp + .data$sem_disp, fill = .data$group),
      alpha = 0.15, colour = NA) +
      ggplot2::scale_fill_manual(values = pal, guide = "none")
  }
  p_disp <- p_disp +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = pal, name = group_var) +
    ggplot2::labs(title = "Distance from baseline over time",
                  subtitle = "How far each condition's network moved in state space (mean +/- SEM across replicates)",
                  x = "Timepoint", y = "Distance from baseline (PC units)",
                  caption = "nova_trajectory_summary()") +
    nova_theme()

  # --- plot 2: state-space trajectory map -------------------------------------
  base_pts <- trg[trg$time_rank == 1, , drop = FALSE]
  p_map <- ggplot2::ggplot(trg, ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]],
                          group = .data$group, colour = .data$group)) +
    ggplot2::geom_path(linewidth = 1.1, lineend = "round",
                       arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed")) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_point(data = base_pts, shape = 22, size = 3.6, fill = "white", colour = "grey20") +
    ggplot2::scale_colour_manual(values = pal, name = group_var) +
    ggplot2::labs(title = "State-space trajectories",
                  subtitle = "Square = baseline; arrowhead = final timepoint",
                  x = xl, y = yl, caption = "nova_trajectory_summary()") +
    nova_theme()

  if (verbose) {
    message("nova_trajectory_summary: ", length(groups), " conditions, ",
            length(tp_order), " timepoints",
            if (is.null(tru)) " (no replicate column found -> no error bands)" else "")
  }

  structure(list(
    metrics = metrics, displacement = tibble::as_tibble(agg), trajectories = trg,
    plots = list(displacement = p_disp, map = p_map),
    params = list(dims = dims, group_var = group_var, unit_var = unit_var,
                  timepoint_order = tp_order)
  ), class = c("nova_trajectory_summary", "list"))
}

#' @export
print.nova_trajectory_summary <- function(x, ...) {
  cat("<nova_trajectory_summary>\n")
  cat("  conditions:", nrow(x$metrics), "| timepoints:", length(x$params$timepoint_order), "\n")
  print(x$metrics)
  invisible(x)
}

# ---------------------------------------------------------------------------
# nova_describe(): plain-language interpretation (rule-based, no AI/API)
# ---------------------------------------------------------------------------

#' Plain-language interpretation of a trajectory summary
#'
#' Turns a \code{nova_trajectory_summary()} result into a short, cautious
#' narrative -- describing what happened (how far each condition moved, how
#' directly, and when it peaked) without over-claiming dynamical mechanism.
#'
#' @param x A \code{nova_trajectory_summary} object.
#' @param ... Unused.
#' @return A character vector of sentences (printed, returned invisibly).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0, 2, 3, 3, 0, 0.1, 0, 0.1), PC2 = c(0, 0, 0, 0, 0, 0, 0, 0),
#'   Treatment = rep(c("Mover", "Still"), each = 4),
#'   Timepoint = rep(c("baseline", "30min", "1h", "2h"), 2))
#' nova_describe(nova_trajectory_summary(df, group_var = "Treatment", verbose = FALSE))
#' @export
nova_describe <- function(x, ...) UseMethod("nova_describe")

#' @export
nova_describe.default <- function(x, ...) {
  message("nova_describe() has no interpreter for this object class.")
  invisible(NULL)
}

#' @export
nova_describe.nova_trajectory_summary <- function(x, ...) {
  m <- x$metrics[order(-x$metrics$net_displacement), , drop = FALSE]
  last_tp <- utils::tail(x$params$timepoint_order, 1)

  lead <- sprintf(
    "Across %d conditions, '%s' moved farthest from baseline (%.2f PC units) and '%s' moved least (%.2f).",
    nrow(m), m$group[1], m$net_displacement[1], m$group[nrow(m)], m$net_displacement[nrow(m)])

  lines <- vapply(seq_len(nrow(m)), function(i) {
    r <- m[i, ]
    dir <- if (is.na(r$directness)) "negligible movement"
           else if (r$directness >= 0.8) "a fairly direct path"
           else if (r$directness >= 0.5) "a moderately direct path"
           else "a wandering path"
    timing <- if (r$peak_displacement > 0 && r$final_displacement < 0.8 * r$peak_displacement)
                sprintf(", peaking near %s then partly returning toward baseline", r$peak_timepoint)
              else if (!identical(r$peak_timepoint, last_tp))
                sprintf(", with most of the change by %s", r$peak_timepoint)
              else ""
    sprintf("'%s' moved %.2f PC units from baseline via %s%s.",
            r$group, r$net_displacement, dir, timing)
  }, character(1))

  .nova_emit(c("Trajectory summary", lead, lines))
}

# Internal: print a narrative block and return it invisibly.
.nova_emit <- function(lines) {
  cat("-- ", lines[1], " --\n", sep = "")
  for (b in lines[-1]) {
    cat(strwrap(b, width = 92, prefix = "  "), sep = "\n")
    cat("\n")
  }
  invisible(lines)
}

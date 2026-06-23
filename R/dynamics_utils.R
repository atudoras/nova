# dynamics_utils.R
# Foundational layer for the nova_dynamics submodule.
#
# This file provides the three primitives every dynamics analysis depends on:
#   (1) robust timepoint parsing / ordering   -> nova_order_timepoints()
#   (2) extraction of ordered state-space trajectories from a PCA result
#                                              -> nova_extract_trajectories()
#   (3) a shared professional visual language  -> nova_theme(), nova_palette()
#
# Design notes
# ------------
# * Embedding-agnostic: anything carrying (dim1, dim2, time, group) works,
#   so PCA, UMAP, or a future latent space are all consumable.
# * No new hard dependencies. Uses the base-R native pipe |> (R >= 4.1, which
#   NOVA already requires) and fully-qualified pkg::fun() calls to keep the
#   NAMESPACE footprint identical to the rest of the package.

# ---------------------------------------------------------------------------
# Internal: parse a single timepoint label to minutes.
# Returns NA_real_ for baseline-like or genuinely unparseable labels.
# ---------------------------------------------------------------------------
.nova_baseline_patterns <- c("baseline", "base", "basal", "pre", "pretreatment",
                             "bl", "control", "ctrl", "untreated", "div0", "t0", "0h0")

.nova_is_baseline <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% .nova_baseline_patterns
}

.nova_time_to_minutes_one <- function(x) {
  s <- tolower(trimws(as.character(x)))
  if (is.na(s) || s == "") return(NA_real_)
  if (s %in% .nova_baseline_patterns) return(NA_real_)

  # Days-in-vitro: DIV7, div 7  -> express in minutes (days * 1440)
  m <- regmatches(s, regexec("^div\\s*([0-9]+\\.?[0-9]*)$", s))[[1]]
  if (length(m) == 2L) return(as.numeric(m[2]) * 24 * 60)

  # Compound hour+minute shorthand: 1h30, 1h30min, 2h15m, 1h05
  m <- regmatches(s, regexec("^([0-9]+)\\s*h\\s*([0-9]+)\\s*(min|m|mins|minutes)?$", s))[[1]]
  if (length(m) == 4L) return(as.numeric(m[2]) * 60 + as.numeric(m[3]))

  # Pure hours: 1h, 2 h, 1hr, 3hours
  m <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)\\s*(h|hr|hrs|hour|hours)$", s))[[1]]
  if (length(m) == 3L) return(as.numeric(m[2]) * 60)

  # Pure minutes: 0min, 15min, 90 min, 30m, 45mins
  m <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)\\s*(m|min|mins|minute|minutes)$", s))[[1]]
  if (length(m) == 3L) return(as.numeric(m[2]))

  # Pure seconds: 30s, 500 sec, 90seconds
  m <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)\\s*(s|sec|secs|second|seconds)$", s))[[1]]
  if (length(m) == 3L) return(as.numeric(m[2]) / 60)

  # Pure weeks / days: 2w, 3 weeks, 1d, 7days
  m <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)\\s*(w|wk|wks|week|weeks)$", s))[[1]]
  if (length(m) == 3L) return(as.numeric(m[2]) * 7 * 24 * 60)
  m <- regmatches(s, regexec("^([0-9]+\\.?[0-9]*)\\s*(d|day|days)$", s))[[1]]
  if (length(m) == 3L) return(as.numeric(m[2]) * 24 * 60)

  # Bare number -> assume minutes (Axion exports commonly use "0", "15", "30")
  if (grepl("^[0-9]+\\.?[0-9]*$", s)) return(as.numeric(s))

  NA_real_
}

#' Parse timepoint labels to numeric minutes
#'
#' Vectorised, dependency-free parser that converts heterogeneous MEA timepoint
#' labels to a common numeric scale (minutes). Recognises baseline-like labels
#' (returned as \code{NA}), compound shorthands (\code{"1h30"}, \code{"1h30min"}),
#' minutes/hours/seconds/days/weeks, days-in-vitro (\code{"DIV7"}), and bare
#' numerics (assumed minutes).
#'
#' @param x Character (or factor) vector of timepoint labels.
#' @return Numeric vector of minutes; \code{NA} for baseline-like or unparseable labels.
#' @examples
#' nova_time_to_minutes(c("baseline", "0min", "1h", "1h30", "90min", "DIV7"))
#' @export
nova_time_to_minutes <- function(x) {
  vapply(as.character(x), .nova_time_to_minutes_one, numeric(1), USE.NAMES = FALSE)
}

#' Order timepoint labels into a biologically correct sequence
#'
#' Produces an ordering in which baseline-like labels always come first, the
#' remaining labels are sorted by parsed real time (minutes), and any genuinely
#' unparseable labels are appended alphabetically. This is the canonical
#' replacement for hardcoded timepoint lists: it correctly orders compound
#' labels such as \code{"1h15"} / \code{"1h30"} / \code{"1h45"} that naive
#' alphabetical sorting mis-ranks.
#'
#' @param timepoints Character/factor vector of timepoint labels (duplicates allowed).
#' @param baseline_first Logical; force baseline-like labels to the front (default \code{TRUE}).
#' @return Character vector of unique labels in dynamical order.
#' @examples
#' nova_order_timepoints(c("1h30", "baseline", "15min", "1h", "2h", "0min", "1h15"))
#' @export
nova_order_timepoints <- function(timepoints, baseline_first = TRUE) {
  tps <- unique(as.character(timepoints))
  tps <- tps[!is.na(tps)]
  if (length(tps) == 0L) return(character(0))

  is_base <- .nova_is_baseline(tps)
  base_lab <- tps[is_base]
  rest     <- tps[!is_base]

  # Baseline group: order by a sensible synonym priority, then by appearance.
  if (length(base_lab) > 1L) {
    prio <- match(tolower(base_lab), .nova_baseline_patterns)
    base_lab <- base_lab[order(prio, base_lab)]
  }

  if (length(rest) > 0L) {
    mins <- nova_time_to_minutes(rest)
    parsed   <- rest[!is.na(mins)]
    unparsed <- rest[is.na(mins)]
    parsed   <- parsed[order(mins[!is.na(mins)])]
    unparsed <- sort(unparsed)
    rest <- c(parsed, unparsed)
  }

  if (baseline_first) c(base_lab, rest) else c(rest, base_lab)
}

# ---------------------------------------------------------------------------
# Internal: resolve a column name from a set of candidates present in data.
# ---------------------------------------------------------------------------
.nova_resolve_col <- function(data, preferred, candidates) {
  cand <- unique(c(preferred, candidates))
  hit <- cand[cand %in% names(data)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

#' Extract ordered state-space trajectories from a PCA (or embedding) result
#'
#' Converts a NOVA PCA result (or any data frame carrying embedding coordinates,
#' a timepoint column, and a grouping column) into a tidy table of ordered
#' trajectories suitable for every \code{nova_dynamics} analysis. Each trajectory
#' is one path through state space: replicate observations are averaged within
#' each (group [, unit], timepoint), then ordered by parsed real time with
#' baseline first.
#'
#' @param pca_results Either the list returned by \code{pca_analysis_enhanced()}
#'   (uses its \code{$plot_data}) or a data frame with the same columns.
#' @param dims Character vector of embedding coordinate columns
#'   (default \code{c("PC1","PC2")}). Length >= 2.
#' @param group_var Grouping column defining distinct trajectories
#'   (e.g. \code{"Treatment"}). Auto-detected if \code{NULL}.
#' @param unit_var Optional replicate-unit column (e.g. \code{"Well"} or
#'   \code{"Experiment"}). If \code{NULL}, one mean trajectory per group is
#'   returned; if supplied, one trajectory per (group, unit).
#' @param timepoint_var Timepoint column (auto-detected among common names).
#' @param timepoint_order Optional explicit ordering; otherwise computed by
#'   \code{nova_order_timepoints()}.
#' @return A tibble (class \code{nova_trajectories}) with columns
#'   \code{traj_id, group, [unit], time_label, time_rank, time_numeric},
#'   the requested \code{dims}, and \code{n_obs}; carrying attributes
#'   \code{dims}, \code{group_var}, \code{unit_var}, \code{timepoint_order},
#'   and \code{variance_explained} (when available).
#' @examples
#' df <- data.frame(
#'   PC1 = rnorm(12), PC2 = rnorm(12),
#'   Treatment = rep(c("A", "B"), each = 6),
#'   Timepoint = rep(c("baseline", "30min", "1h"), 4)
#' )
#' tr <- nova_extract_trajectories(df, group_var = "Treatment")
#' @export
nova_extract_trajectories <- function(pca_results,
                                      dims = c("PC1", "PC2"),
                                      group_var = NULL,
                                      unit_var = NULL,
                                      timepoint_var = "Timepoint",
                                      timepoint_order = NULL) {

  # --- resolve the state-space table -----------------------------------------
  if (is.list(pca_results) && "plot_data" %in% names(pca_results)) {
    pd <- pca_results$plot_data
    var_exp <- pca_results$variance_explained
  } else if (is.data.frame(pca_results)) {
    pd <- pca_results
    var_exp <- NULL
  } else {
    stop("`pca_results` must be a data frame or a list with a `plot_data` element.")
  }
  pd <- as.data.frame(pd, stringsAsFactors = FALSE)

  if (length(dims) < 2L) stop("`dims` must name at least two coordinate columns.")
  missing_dims <- setdiff(dims, names(pd))
  if (length(missing_dims) > 0L) {
    stop("Embedding columns not found: ", paste(missing_dims, collapse = ", "),
         ". Available: ", paste(grep("^PC|UMAP|^Dim", names(pd), value = TRUE), collapse = ", "))
  }

  timepoint_var <- .nova_resolve_col(pd, timepoint_var,
                                     c("Timepoint", "Time", "timepoint", "time", "Time_point"))
  if (is.na(timepoint_var)) stop("No timepoint column found in the data.")

  if (is.null(group_var)) {
    group_var <- .nova_resolve_col(pd, "Treatment",
                                   c("Treatment", "Genotype", "Condition", "Group"))
    if (is.na(group_var)) stop("No grouping column found; please pass `group_var`.")
  } else if (!group_var %in% names(pd)) {
    stop("`group_var` '", group_var, "' not found in the data.")
  }

  if (!is.null(unit_var) && !unit_var %in% names(pd)) {
    warning("`unit_var` '", unit_var, "' not found; collapsing to one trajectory per group.")
    unit_var <- NULL
  }

  # --- time ordering ----------------------------------------------------------
  if (is.null(timepoint_order)) {
    timepoint_order <- nova_order_timepoints(pd[[timepoint_var]])
  }
  pd <- pd[as.character(pd[[timepoint_var]]) %in% timepoint_order, , drop = FALSE]
  pd$.time_label <- factor(as.character(pd[[timepoint_var]]), levels = timepoint_order)

  # --- aggregate replicates ---------------------------------------------------
  grp_cols <- c(group_var, if (!is.null(unit_var)) unit_var, ".time_label")
  agg <- pd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(dims), \(v) mean(v, na.rm = TRUE)),
      n_obs = dplyr::n(),
      .groups = "drop"
    )

  # --- numeric time axis (real minutes; baseline placed one median-gap before) -
  agg$time_label <- as.character(agg$.time_label)
  agg$time_rank  <- as.integer(factor(agg$.time_label, levels = timepoint_order))
  mins <- nova_time_to_minutes(timepoint_order)
  if (all(is.na(mins))) {
    rank_to_num <- stats::setNames(seq_along(timepoint_order), timepoint_order)
  } else {
    finite <- mins[is.finite(mins)]
    gap <- if (length(finite) > 1L) stats::median(diff(sort(finite))) else 1
    mins_filled <- mins
    base_idx <- which(is.na(mins) & .nova_is_baseline(timepoint_order))
    if (length(base_idx) > 0L) mins_filled[base_idx] <- min(finite) - gap * seq(length(base_idx), 1)
    still_na <- which(is.na(mins_filled))
    if (length(still_na) > 0L) mins_filled[still_na] <- still_na  # fallback to rank
    rank_to_num <- stats::setNames(mins_filled, timepoint_order)
  }
  agg$time_numeric <- as.numeric(rank_to_num[agg$time_label])

  # --- assemble tidy trajectory table ----------------------------------------
  agg$group <- as.character(agg[[group_var]])
  if (!is.null(unit_var)) {
    agg$unit <- as.character(agg[[unit_var]])
    agg$traj_id <- paste(agg$group, agg$unit, sep = " | ")
    keep <- c("traj_id", "group", "unit", "time_label", "time_rank", "time_numeric", dims, "n_obs")
  } else {
    agg$traj_id <- agg$group
    keep <- c("traj_id", "group", "time_label", "time_rank", "time_numeric", dims, "n_obs")
  }

  out <- agg[, keep, drop = FALSE]
  out <- out[order(out$traj_id, out$time_rank), , drop = FALSE]
  out <- tibble::as_tibble(out)

  attr(out, "dims") <- dims
  attr(out, "group_var") <- group_var
  attr(out, "unit_var") <- unit_var
  attr(out, "timepoint_var") <- timepoint_var
  attr(out, "timepoint_order") <- timepoint_order
  attr(out, "variance_explained") <- var_exp
  class(out) <- c("nova_trajectories", class(out))
  out
}

# ---------------------------------------------------------------------------
# Visual language
# ---------------------------------------------------------------------------

#' NOVA dynamics ggplot2 theme
#'
#' A polished, publication-oriented theme that extends the package's existing
#' \code{theme_minimal} aesthetic with a light panel border, bold titles, muted
#' gridlines, and a left-aligned caption slot for method annotations.
#'
#' @param base_size Base font size (default 12).
#' @param base_family Base font family (default "").
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + nova_theme()
#' @export
nova_theme <- function(base_size = 12, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle   = ggplot2::element_text(colour = "grey30", size = base_size - 1),
      plot.caption    = ggplot2::element_text(colour = "grey50", size = base_size - 3, hjust = 0),
      axis.title      = ggplot2::element_text(face = "bold"),
      axis.text       = ggplot2::element_text(colour = "grey20"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      panel.border    = ggplot2::element_rect(fill = NA, colour = "grey85", linewidth = 0.5),
      axis.ticks      = ggplot2::element_line(colour = "grey70", linewidth = 0.3),
      legend.title    = ggplot2::element_text(face = "bold"),
      legend.key.size = ggplot2::unit(0.9, "lines"),
      strip.text      = ggplot2::element_text(face = "bold")
    )
}

#' NOVA qualitative / sequential colour palette
#'
#' Returns colours consistent with the existing NOVA trajectory palette
#' (a Paired-style qualitative ramp) or a viridis sequential ramp, so dynamics
#' figures match the rest of the package.
#'
#' @param n Number of colours required.
#' @param type \code{"qual"} (categorical groups) or \code{"seq"} (continuous).
#' @return Character vector of \code{n} hex colours.
#' @examples
#' nova_palette(4)
#' nova_palette(7, type = "seq")
#' @export
nova_palette <- function(n, type = c("qual", "seq")) {
  type <- match.arg(type)
  if (type == "seq") {
    return(viridis::viridis(n))
  }
  base_cols <- c("#E31A1C", "#FF7F00", "#FDBF6F", "#33A02C", "#1F78B4",
                 "#6A3D9A", "#B15928", "#FB9A99", "#A6CEE3", "#B2DF8A")
  if (n <= length(base_cols)) base_cols[seq_len(n)]
  else grDevices::colorRampPalette(base_cols)(n)
}

# dynamics_regime.R
# nova_dynamical_regime(): classify each trajectory's qualitative dynamical
# behaviour from its geometry.
#
# Scientific rationale ------------------------------------------------------
# Dynamical systems are characterised less by where they are than by how they
# move. From a single trajectory we cannot fit a vector field, but we CAN read
# robust geometric signatures that map onto canonical regimes:
#   * stable       : network barely moves (small path relative to state scale)
#   * convergent   : directed motion that DECELERATES toward a settling point
#                    (speed_late << speed_early, final speed low) -> approaching
#                    an attractor / fixed point
#   * divergent    : directed motion that ACCELERATES / keeps growing in net
#                    displacement -> leaving a region, no settling
#   * oscillatory  : much motion but little net progress, with direction
#                    reversals (low straightness, low/negative persistence)
#   * transitional : directed, ~constant-speed drift between regions (the
#                    "in transit, not yet settled" default)
#
# The classifier is rule-based and fully transparent (no black box, no LLM).
# Each regime gets a 0..1 score from soft thresholds; the argmax is the call and
# the top-vs-second margin gives a confidence. With few timepoints, confidence
# is deliberately capped (see DATA-SUFFICIENCY note below).

# ===========================================================================
# TUNABLE DECISION THRESHOLDS  (the scientific knobs - edit these 6 numbers)
# ---------------------------------------------------------------------------
# All movement quantities are normalised by the state-space scale (RMS of
# per-dimension SD of the pooled embedding), so thresholds are unit-free and
# transferable across datasets / embeddings.
# ===========================================================================
.nova_regime_thresholds <- list(
  stable_path     = 0.60,  # total path < this * scale  -> "stable" (barely moves)
  straight_hi     = 0.55,  # straightness above this    -> motion counts as "directed"
  persist_lo      = 0.20,  # directional persistence below this -> reversing / random
  decel_converge  = 0.60,  # speed_late/speed_early below this   -> "convergent"
  accel_diverge   = 1.40,  # speed_late/speed_early above this   -> "divergent"
  soft_width      = 0.18   # logistic width for the soft scores (smaller = sharper)
)

# Internal: smooth 0..1 gate.
.nova_sig <- function(z, w) 1 / (1 + exp(-z / w))

# Internal: regime scores for one trajectory's feature set.
.nova_regime_scores <- function(f, th) {
  # f: list(path_n, net_n, straight, persist, decel, final_speed_n)
  w <- th$soft_width
  stable <- .nova_sig(th$stable_path - f$path_n, w)                      # small path
  directed <- .nova_sig(f$straight - th$straight_hi, w)                  # straight motion
  reversing <- .nova_sig(th$persist_lo - f$persist, w)                   # low/neg persistence

  convergent <- directed * .nova_sig(th$decel_converge - f$decel, w)    # directed + slowing
  divergent  <- directed * .nova_sig(f$decel - th$accel_diverge, w)     # directed + speeding
  oscillatory <- (1 - directed) * reversing * (1 - stable)               # busy but going nowhere
  # transitional: directed, neither clearly settling nor diverging
  transitional <- directed * (1 - convergent) * (1 - divergent) * (1 - stable)

  s <- c(stable = stable, convergent = convergent, divergent = divergent,
         oscillatory = oscillatory, transitional = transitional)
  # ensure non-degenerate
  s[!is.finite(s)] <- 0
  s
}

#' Dynamical regime detection
#'
#' Classifies each trajectory as \code{stable}, \code{convergent},
#' \code{divergent}, \code{oscillatory}, or \code{transitional} using rule-based
#' scores derived from trajectory geometry. Returns the classification, a
#' confidence (top-vs-second-score margin, capped when timepoints are few), and
#' the full score matrix for transparency.
#'
#' @param x A PCA result, embedding data frame, or \code{nova_trajectories}.
#' @param dims,group_var,unit_var,timepoint_var,timepoint_order Extraction args.
#' @param thresholds Named list overriding \code{.nova_regime_thresholds}.
#' @param verbose Logical.
#' @return Object of class \code{nova_dynamical_regime} with \code{classification}
#'   (tibble: traj_id, group, regime, confidence, features), \code{scores}
#'   (matrix), and \code{plots} (\code{overlay}, \code{scores}).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0,2,3,3.2,3.25, 0,1,0,1,0),
#'   PC2 = c(0,0,0,0,0,       0,1,0,1,0),
#'   Treatment = rep(c("Settler","Oscillator"), each = 5),
#'   Timepoint = rep(c("baseline","15min","30min","1h","2h"), 2)
#' )
#' r <- nova_dynamical_regime(df, group_var = "Treatment", verbose = FALSE)
#' r$classification[, c("group","regime","confidence")]
#' @export
nova_dynamical_regime <- function(x,
                                  dims = c("PC1", "PC2"),
                                  group_var = NULL,
                                  unit_var = NULL,
                                  timepoint_var = "Timepoint",
                                  timepoint_order = NULL,
                                  thresholds = NULL,
                                  verbose = TRUE) {
  th <- modifyList(.nova_regime_thresholds, thresholds %||% list())

  tr <- if (inherits(x, "nova_trajectories")) x else
    nova_extract_trajectories(x, dims = dims, group_var = group_var,
                              unit_var = unit_var, timepoint_var = timepoint_var,
                              timepoint_order = timepoint_order)
  dims <- attr(tr, "dims"); var_exp <- attr(tr, "variance_explained")

  M <- as.matrix(tr[, dims, drop = FALSE])
  scale <- sqrt(mean(apply(M, 2, stats::var, na.rm = TRUE)))
  if (!is.finite(scale) || scale <= 0) scale <- 1

  ids <- unique(tr$traj_id)
  rows <- list(); score_mat <- matrix(NA_real_, length(ids), 5,
                                      dimnames = list(ids, c("stable","convergent","divergent","oscillatory","transitional")))
  for (id in ids) {
    s <- tr[tr$traj_id == id, , drop = FALSE]; s <- s[order(s$time_rank), ]
    P <- as.matrix(s[, dims, drop = FALSE])
    m <- .nova_traj_metrics(P, s$time_numeric)
    # early vs late speed (split by elapsed time)
    seg <- m$seg
    if (nrow(seg) >= 2L) {
      t_mid <- cumsum(seg$dt) - seg$dt / 2
      half <- max(t_mid) / 2
      early <- mean(seg$speed[t_mid <= half], na.rm = TRUE)
      late  <- mean(seg$speed[t_mid >  half], na.rm = TRUE)
      decel <- if (is.finite(early) && early > 0) late / early else 1
    } else decel <- 1
    f <- list(
      path_n   = m$path_length / scale,
      net_n    = m$net_displacement / scale,
      straight = ifelse(is.finite(m$straightness), m$straightness, 0),
      persist  = ifelse(is.finite(m$directional_persistence), m$directional_persistence, 0),
      decel    = ifelse(is.finite(decel), decel, 1),
      final_speed_n = m$final_speed / scale
    )
    sc <- .nova_regime_scores(f, th)
    score_mat[id, ] <- sc
    ord <- order(sc, decreasing = TRUE)
    top <- names(sc)[ord[1]]
    margin <- (sc[ord[1]] - sc[ord[2]]) / (sc[ord[1]] + 1e-9)
    # DATA-SUFFICIENCY: cap confidence by number of timepoints (n>=5 -> full)
    cap <- min(1, (m$n_points - 1) / 4)
    conf <- max(0, min(1, margin)) * cap
    rows[[id]] <- data.frame(
      traj_id = id, group = s$group[1], regime = top, confidence = round(conf, 3),
      path_norm = round(f$path_n, 3), net_norm = round(f$net_n, 3),
      straightness = round(f$straight, 3), persistence = round(f$persist, 3),
      decel_ratio = round(f$decel, 3), n_points = m$n_points,
      stringsAsFactors = FALSE
    )
  }
  classification <- tibble::as_tibble(do.call(rbind, rows))

  regime_levels <- c("stable","convergent","divergent","oscillatory","transitional")
  regime_cols <- stats::setNames(
    c("#1F78B4","#33A02C","#E31A1C","#FF7F00","#6A3D9A"), regime_levels)
  tr2 <- merge(tr, classification[, c("traj_id","regime","confidence")], by = "traj_id")
  tr2 <- tr2[order(tr2$traj_id, tr2$time_rank), ]
  tr2$regime <- factor(tr2$regime, levels = regime_levels)

  xl <- .nova_axis_label(dims[1], var_exp); yl <- .nova_axis_label(dims[2], var_exp)
  base_pts <- tr2[!duplicated(tr2$traj_id), ]

  overlay <- ggplot2::ggplot(tr2, ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]],
                              group = .data$traj_id, colour = .data$regime)) +
    ggplot2::geom_path(linewidth = 1.2, lineend = "round",
                       arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed")) +
    ggplot2::geom_point(size = 2, alpha = 0.85) +
    ggplot2::geom_point(data = base_pts, shape = 22, size = 3.4, fill = "white", colour = "grey20") +
    ggplot2::scale_colour_manual(values = regime_cols, drop = FALSE, name = "Regime") +
    ggplot2::labs(title = "Dynamical regime classification",
                  subtitle = "Trajectories coloured by detected regime - square = baseline",
                  x = xl, y = yl, caption = "nova_dynamical_regime()") +
    nova_theme()

  sm <- as.data.frame(as.table(score_mat))
  names(sm) <- c("traj_id", "regime", "score")
  sm$regime <- factor(sm$regime, levels = regime_levels)
  scores_plot <- ggplot2::ggplot(sm, ggplot2::aes(.data$regime, .data$traj_id, fill = .data$score)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$score)), size = 3, colour = "grey15") +
    ggplot2::scale_fill_gradient(low = "white", high = "#33A02C", name = "Score", limits = c(0, 1)) +
    ggplot2::labs(title = "Regime score matrix",
                  subtitle = "Rule-based score per regime (argmax = call)",
                  x = NULL, y = NULL, caption = "nova_dynamical_regime()") +
    nova_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  if (verbose) {
    tab <- table(classification$regime)
    message("nova_dynamical_regime: ",
            paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", "))
  }

  structure(list(
    classification = classification, scores = score_mat, thresholds = th,
    plots = list(overlay = overlay, scores = scores_plot),
    params = list(dims = dims, group_var = attr(tr, "group_var"), state_scale = scale)
  ), class = c("nova_dynamical_regime", "nova_dynamics_result", "list"))
}

#' @export
print.nova_dynamical_regime <- function(x, ...) {
  cat("<nova_dynamical_regime>\n")
  print(x$classification[, c("traj_id", "group", "regime", "confidence")])
  invisible(x)
}

# Internal null-coalescing operator (kept local to avoid clashing with exported one)
`%||%` <- function(a, b) if (is.null(a)) b else a

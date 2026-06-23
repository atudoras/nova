# dynamics_describe.R
# nova_describe(): rule-based natural-language interpretation of dynamics results,
# and nova_dynamics(): a one-call wrapper running the full pipeline.
#
# Rationale -----------------------------------------------------------------
# Most neuroscience software reports numbers and leaves interpretation to the
# reader. nova_describe() closes that gap with a transparent, deterministic
# template engine: it maps the SAME thresholds used by the analyses onto plain
# sentences. No model, no API - every clause is traceable to a metric.

# Internal: pick a phrase from numeric value against ordered breakpoints.
.nova_bin <- function(v, breaks, labels) {
  if (!is.finite(v)) return(labels[1])
  labels[findInterval(v, breaks) + 1L]
}

#' Natural-language interpretation of a dynamics result
#'
#' Generic interpreter that turns any \code{nova_dynamics_result} into a concise,
#' human-readable narrative. Dispatches on the result class.
#'
#' @param x A result from \code{nova_state_geometry()},
#'   \code{nova_dynamical_regime()}, \code{nova_trajectory_similarity()},
#'   \code{nova_transition_matrix()}, \code{nova_landscape()}, or
#'   \code{nova_dynamics()}.
#' @param ... Unused.
#' @return A character vector of sentences (invisibly printed by default).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0,2,3,3.2, 0,1,0,1), PC2 = c(0,0,0,0, 0,1,0,1),
#'   Treatment = rep(c("Settler","Oscillator"), each = 4),
#'   Timepoint = rep(c("baseline","30min","1h","2h"), 2))
#' nova_describe(nova_state_geometry(df, group_var = "Treatment", verbose = FALSE))
#' @export
nova_describe <- function(x, ...) UseMethod("nova_describe")

#' @export
nova_describe.default <- function(x, ...) {
  msg <- "No interpreter is available for this object class."
  message(msg); invisible(msg)
}

#' @export
nova_describe.nova_state_geometry <- function(x, ...) {
  s <- x$summary
  out <- character(0)
  for (i in seq_len(nrow(s))) {
    r <- s[i, ]
    directedness <- .nova_bin(r$straightness, c(0.4, 0.75),
                              c("a wandering, indirect", "a moderately directed", "a highly directed"))
    persist <- .nova_bin(r$directional_persistence, c(0.0, 0.5),
                         c("frequent direction reversals", "a meandering heading", "strongly persistent heading"))
    speed_kin <- if (is.finite(r$final_speed) && is.finite(r$mean_speed)) {
      if (r$final_speed < 0.6 * r$mean_speed) "decelerating toward a settling state"
      else if (r$final_speed > 1.4 * r$mean_speed) "accelerating, with no sign of settling"
      else "moving at a roughly steady rate"
    } else "of indeterminate kinetics"
    out <- c(out, sprintf(
      "%s '%s' follows %s path through state space (straightness %.2f, tortuosity %.2f), with %s; it is %s. Total reconfiguration (path length) is %.2f, ending %.2f units from baseline.",
      x$params$group_var, r$group, directedness, r$straightness,
      ifelse(is.finite(r$tortuosity), r$tortuosity, Inf), persist, speed_kin,
      r$path_length, r$net_displacement))
  }
  out <- c("State-space geometry", out)
  .nova_emit(out)
}

#' @export
nova_describe.nova_dynamical_regime <- function(x, ...) {
  c0 <- x$classification
  gloss <- c(
    stable       = "remains near a fixed configuration (little net movement), consistent with a network at or near an attractor",
    convergent   = "moves in a directed fashion and then decelerates, consistent with approach toward a stable network state",
    divergent    = "moves in a directed fashion and keeps accelerating away, consistent with an unstable or driven departure from baseline",
    oscillatory  = "moves substantially but makes little net progress with direction reversals, consistent with cyclic or limit-cycle-like dynamics",
    transitional = "drifts steadily between regions without clearly settling, consistent with an ongoing state transition")
  out <- character(0)
  for (i in seq_len(nrow(c0))) {
    r <- c0[i, ]
    conf_word <- .nova_bin(r$confidence, c(0.34, 0.67), c("tentatively", "moderately confidently", "confidently"))
    cav <- if (r$n_points < 4) " (few timepoints: interpret cautiously)" else ""
    out <- c(out, sprintf("%s '%s' is %s classified as %s: it %s%s.",
                          x$params$group_var, r$group, conf_word, toupper(r$regime),
                          gloss[[r$regime]], cav))
  }
  out <- c("Dynamical regimes", out)
  .nova_emit(out)
}

#' @export
nova_describe.nova_trajectory_similarity <- function(x, ...) {
  D <- x$distance; ids <- rownames(D)
  ut <- which(upper.tri(D), arr.ind = TRUE)
  vals <- D[ut]
  closest <- ut[which.min(vals), ]; farthest <- ut[which.max(vals), ]
  out <- c("Trajectory similarity",
    sprintf("Compared %d trajectories using the %s distance.", length(ids), x$method),
    sprintf("The most similar pair is '%s' and '%s' (distance %.3f); the most distinct is '%s' and '%s' (distance %.3f).",
            ids[closest[1]], ids[closest[2]], min(vals),
            ids[farthest[1]], ids[farthest[2]], max(vals)))
  if (!is.null(x$clusters)) {
    nb <- length(unique(x$clusters))
    out <- c(out, sprintf("Hierarchical clustering separates them into %d group(s) of similar dynamics.", nb))
  } else {
    out <- c(out, "The dendrogram groups trajectories whose paths share shape and timing.")
  }
  .nova_emit(out)
}

#' @export
nova_describe.nova_transition_matrix <- function(x, ...) {
  occ <- x$occupancy; st <- x$states
  dom <- names(which.max(occ))
  P <- x$transition; diag0 <- P; diag(diag0) <- NA
  strongest <- which(diag0 == max(diag0, na.rm = TRUE), arr.ind = TRUE)[1, ]
  rec <- st$state[st$role == "recurrent"]; tra <- st$state[st$role == "transient"]
  out <- c("State transitions",
    sprintf("State space was coarse-grained into %d network states. The most occupied is %s (%.0f%% of samples).",
            x$k, dom, 100 * max(occ)),
    sprintf("State %s shows the highest persistence (self-transition P = %.2f).",
            rownames(P)[which.max(diag(P))], max(diag(P))),
    sprintf("The strongest between-state flow is %s -> %s (P = %.2f).",
            rownames(P)[strongest[1]], colnames(P)[strongest[2]], P[strongest[1], strongest[2]]),
    sprintf("Recurrent (metastable) state(s): %s. Transient state(s): %s.",
            ifelse(length(rec) > 0, paste(rec, collapse = ", "), "none"),
            ifelse(length(tra) > 0, paste(tra, collapse = ", "), "none")),
    "Note: with few timepoints these probabilities summarise observed flow rather than a fitted Markov model.")
  .nova_emit(out)
}

#' @export
nova_describe.nova_landscape <- function(x, ...) {
  z <- x$density$z
  # count local maxima (basins) above 25% of global peak
  nr <- nrow(z); nc <- ncol(z); thr <- 0.25 * max(z)
  peaks <- 0L
  for (i in 2:(nr - 1)) for (j in 2:(nc - 1)) {
    v <- z[i, j]
    if (v > thr && v >= max(z[(i-1):(i+1), (j-1):(j+1)])) peaks <- peaks + 1L
  }
  basin_word <- if (peaks <= 1) "a single dominant basin (one preferred network configuration)"
                else sprintf("%d distinct basins (multiple preferred configurations / metastability)", peaks)
  out <- c("State landscape",
    sprintf("The occupancy landscape over %d samples shows %s.", x$params$n_points, basin_word),
    "Valleys in the pseudo-potential (U = -log p) mark frequently occupied, attractor-like regions; ridges mark rarely visited transition zones.",
    "This is a descriptive density heuristic, not a fitted dynamical potential.")
  .nova_emit(out)
}

#' @export
nova_describe.nova_dynamics <- function(x, ...) {
  parts <- list()
  for (nm in c("geometry", "regime", "transitions", "similarity", "landscape")) {
    if (!is.null(x[[nm]])) parts[[nm]] <- nova_describe(x[[nm]])
  }
  txt <- unlist(lapply(parts, function(p) c(p, "")), use.names = FALSE)
  invisible(txt)
}

# Internal: print a narrative block and return it invisibly.
.nova_emit <- function(lines) {
  header <- lines[1]; body <- lines[-1]
  cat("-- ", header, " --\n", sep = "")
  for (b in body) {
    cat(strwrap(b, width = 92, prefix = "  "), sep = "\n")
    cat("\n")
  }
  invisible(lines)
}

#' Run the full NOVA dynamics pipeline
#'
#' Convenience wrapper that extracts trajectories once and runs the requested
#' dynamics analyses, returning a single object that \code{nova_describe()} can
#' narrate end-to-end.
#'
#' @param x A PCA result or embedding data frame.
#' @param analyses Character vector; any of \code{"geometry"}, \code{"regime"},
#'   \code{"transitions"}, \code{"similarity"}, \code{"landscape"}
#'   (default: all).
#' @param dims,group_var,unit_var,timepoint_var,timepoint_order Extraction args.
#' @param similarity_method Method for \code{nova_trajectory_similarity()}.
#' @param k Number of states for \code{nova_transition_matrix()}.
#' @param verbose Logical.
#' @return Object of class \code{nova_dynamics} holding the sub-results in
#'   \code{$geometry}, \code{$regime}, \code{$transitions}, \code{$similarity},
#'   \code{$landscape}, plus the shared \code{$trajectories}.
#' @examples
#' df <- data.frame(
#'   PC1 = c(0,2,3,3.2, 0,1,0,1), PC2 = c(0,0,0,0, 0,1,0,1),
#'   Treatment = rep(c("A","B"), each = 4),
#'   Timepoint = rep(c("baseline","30min","1h","2h"), 2))
#' dyn <- nova_dynamics(df, analyses = c("geometry","regime"),
#'                      group_var = "Treatment", verbose = FALSE)
#' @export
nova_dynamics <- function(x,
                          analyses = c("geometry", "regime", "transitions", "similarity", "landscape"),
                          dims = c("PC1", "PC2"),
                          group_var = NULL,
                          unit_var = NULL,
                          timepoint_var = "Timepoint",
                          timepoint_order = NULL,
                          similarity_method = "dtw",
                          k = NULL,
                          verbose = TRUE) {
  analyses <- match.arg(analyses, several.ok = TRUE)
  tr <- nova_extract_trajectories(x, dims = dims, group_var = group_var,
                                  unit_var = unit_var, timepoint_var = timepoint_var,
                                  timepoint_order = timepoint_order)
  res <- list(trajectories = tr)
  if ("geometry" %in% analyses)
    res$geometry <- nova_state_geometry(tr, verbose = verbose)
  if ("regime" %in% analyses)
    res$regime <- nova_dynamical_regime(tr, verbose = verbose)
  if ("similarity" %in% analyses && length(unique(tr$traj_id)) >= 2L)
    res$similarity <- nova_trajectory_similarity(tr, method = similarity_method, verbose = verbose)
  if ("landscape" %in% analyses)
    res$landscape <- nova_landscape(x, dims = dims, group_var = group_var,
                                    timepoint_var = timepoint_var,
                                    timepoint_order = timepoint_order, verbose = verbose)
  if ("transitions" %in% analyses)
    res$transitions <- tryCatch(
      nova_transition_matrix(x, k = k, dims = dims, group_var = group_var,
                             unit_var = unit_var, timepoint_var = timepoint_var,
                             timepoint_order = timepoint_order, verbose = verbose),
      error = function(e) { if (verbose) message("  transitions skipped: ", conditionMessage(e)); NULL })
  structure(res, class = c("nova_dynamics", "list"))
}

#' @export
print.nova_dynamics <- function(x, ...) {
  cat("<nova_dynamics>\n")
  cat("  trajectories:", length(unique(x$trajectories$traj_id)), "\n")
  cat("  analyses:", paste(setdiff(names(x), "trajectories"), collapse = ", "), "\n")
  cat("  -> nova_describe() for interpretation; $<analysis>$plots for figures\n")
  invisible(x)
}

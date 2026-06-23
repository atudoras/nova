# dynamics_transitions.R
# nova_transition_matrix(): discretise the continuous embedding into a small set
# of "network states", then estimate how trajectories move between them.
#
# Scientific rationale ------------------------------------------------------
# A continuous trajectory can be coarse-grained into discrete macrostates
# (attractor basins / metastable regimes). k-means on the pooled embedding
# defines K macrostates by proximity; each recording is assigned to one. Walking
# each trajectory in time and counting state(t) -> state(t+1) yields an empirical
# Markov transition matrix P (row-stochastic). From P we read:
#   * occupancy        : fraction of all samples in each state (where networks live)
#   * self-transition  : P[i,i] = persistence / dwell tendency of a state
#   * recurrent states : revisited / high-dwell (>= median dwell)  -> metastable
#   * transient states : passed through once (low dwell, low occupancy)
# CAVEAT (surfaced by nova_describe): with few timepoints the Markov estimate is
# coarse; treat P as a descriptive flow summary, not a fitted stochastic model.

# Internal: classify states as recurrent / transient from P and occupancy.
.nova_state_roles <- function(P, occupancy) {
  K <- nrow(P)
  dwell <- diag(P)
  inflow <- colSums(P) - diag(P)               # arrivals from other states
  recurrent <- (dwell >= stats::median(dwell)) | (occupancy >= stats::median(occupancy))
  role <- ifelse(recurrent, "recurrent", "transient")
  data.frame(state = rownames(P), occupancy = occupancy, self_transition = dwell,
             inflow = inflow, role = role, stringsAsFactors = FALSE)
}

#' Network-state transition analysis
#'
#' Coarse-grains the embedding into \code{k} discrete network states (k-means)
#' and estimates the empirical Markov transition matrix from the time-ordered
#' trajectories, together with state occupancy and a recurrent/transient
#' classification. Returns a transition heatmap and a state-flow diagram laid out
#' in the embedding plane.
#'
#' @param x A PCA result, embedding data frame, or \code{nova_trajectories}.
#'   Transition counting requires per-replicate paths, so when \code{x} is not
#'   already extracted, \code{unit_var} defaults to a detected replicate column.
#' @param k Number of network states (default \code{min(4, n_timepoints)}).
#' @param dims,group_var,unit_var,timepoint_var,timepoint_order Extraction args.
#' @param by_group Logical; if \code{TRUE}, also return one transition matrix per
#'   group (default \code{FALSE}).
#' @param seed Integer RNG seed for reproducible k-means (default 1).
#' @param verbose Logical.
#' @return Object of class \code{nova_transition_matrix} with \code{transition}
#'   (matrix), \code{occupancy}, \code{states} (roles), \code{centroids},
#'   \code{assignments}, optional \code{by_group}, and \code{plots}
#'   (\code{heatmap}, \code{flow}).
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   PC1 = c(rnorm(20), rnorm(20, 5)), PC2 = rnorm(40),
#'   Treatment = rep(c("A","B"), each = 20),
#'   Well = rep(rep(paste0("W", 1:4), each = 5), 2),
#'   Timepoint = rep(c("baseline","30min","1h","1h30min","2h"), 8)
#' )
#' tm <- nova_transition_matrix(df, k = 3, verbose = FALSE)
#' tm$transition
#' @export
nova_transition_matrix <- function(x,
                                   k = NULL,
                                   dims = c("PC1", "PC2"),
                                   group_var = NULL,
                                   unit_var = NULL,
                                   timepoint_var = "Timepoint",
                                   timepoint_order = NULL,
                                   by_group = FALSE,
                                   seed = 1,
                                   verbose = TRUE) {

  # We need per-unit paths to count transitions. Detect a replicate column.
  if (!inherits(x, "nova_trajectories") && is.null(unit_var)) {
    pd <- if (is.list(x) && "plot_data" %in% names(x)) x$plot_data else x
    unit_var <- .nova_resolve_col(pd, "Well", c("Well", "Experiment", "Sample", "ID"))
    if (is.na(unit_var)) unit_var <- NULL
  }
  tr <- if (inherits(x, "nova_trajectories")) x else
    nova_extract_trajectories(x, dims = dims, group_var = group_var,
                              unit_var = unit_var, timepoint_var = timepoint_var,
                              timepoint_order = timepoint_order)
  dims <- attr(tr, "dims"); var_exp <- attr(tr, "variance_explained")

  n_tp <- length(attr(tr, "timepoint_order"))
  if (is.null(k)) k <- min(4L, max(2L, n_tp))
  k <- max(2L, min(k, nrow(tr) - 1L))

  M <- as.matrix(tr[, dims, drop = FALSE])
  set.seed(seed)
  km <- stats::kmeans(M, centers = k, nstart = 25)
  tr$state <- factor(paste0("S", km$cluster), levels = paste0("S", seq_len(k)))
  centroids <- as.data.frame(km$centers); names(centroids) <- dims
  centroids$state <- paste0("S", seq_len(k))

  occupancy <- as.numeric(table(tr$state)) / nrow(tr)
  names(occupancy) <- paste0("S", seq_len(k))

  count_transitions <- function(d) {
    P <- matrix(0, k, k, dimnames = list(paste0("S", 1:k), paste0("S", 1:k)))
    for (id in unique(d$traj_id)) {
      s <- d[d$traj_id == id, , drop = FALSE]; s <- s[order(s$time_rank), ]
      st <- as.integer(s$state)
      if (length(st) >= 2L)
        for (t in seq_len(length(st) - 1L)) P[st[t], st[t + 1L]] <- P[st[t], st[t + 1L]] + 1
    }
    rs <- rowSums(P)
    Pn <- P; nz <- rs > 0
    Pn[nz, ] <- P[nz, ] / rs[nz]
    list(counts = P, prob = Pn)
  }

  tt <- count_transitions(tr)
  P <- tt$prob
  states_df <- .nova_state_roles(P, occupancy)

  by_group_res <- NULL
  if (by_group) {
    by_group_res <- lapply(split(tr, tr$group), function(d) count_transitions(d)$prob)
  }

  # Heatmap of transition probabilities ---------------------------------------
  long <- expand.grid(from = rownames(P), to = colnames(P), stringsAsFactors = FALSE)
  long$prob <- P[cbind(match(long$from, rownames(P)), match(long$to, colnames(P)))]
  long$from <- factor(long$from, levels = rev(rownames(P)))
  long$to   <- factor(long$to, levels = colnames(P))
  heatmap <- ggplot2::ggplot(long, ggplot2::aes(.data$to, .data$from, fill = .data$prob)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$prob > 0, sprintf("%.2f", .data$prob), "")),
                       size = 3.4, colour = "grey15") +
    ggplot2::scale_fill_gradient(low = "white", high = "#1F78B4", name = "P(to | from)",
                                 limits = c(0, 1)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "State transition matrix",
                  subtitle = paste0(k, " network states (k-means) - row-stochastic"),
                  x = "To state", y = "From state",
                  caption = "nova_transition_matrix() - P[i,i] on diagonal = persistence") +
    nova_theme()

  # State-flow diagram in the embedding plane ---------------------------------
  flow <- .nova_flow_plot(tr, centroids, P, occupancy, dims, var_exp)

  if (verbose) message("nova_transition_matrix: ", k, " states - ",
                       sum(states_df$role == "recurrent"), " recurrent, ",
                       sum(states_df$role == "transient"), " transient")

  structure(list(
    transition = P, counts = tt$counts, occupancy = occupancy,
    states = states_df, centroids = centroids, assignments = tr[, c("traj_id", "group", "time_label", "state")],
    by_group = by_group_res, k = k,
    plots = list(heatmap = heatmap, flow = flow),
    params = list(dims = dims, group_var = attr(tr, "group_var"))
  ), class = c("nova_transition_matrix", "nova_dynamics_result", "list"))
}

# Internal: state-flow diagram (nodes at centroids, curved edges weighted by P).
.nova_flow_plot <- function(tr, centroids, P, occupancy, dims, var_exp) {
  k <- nrow(centroids)
  edges <- expand.grid(from = seq_len(k), to = seq_len(k))
  edges <- edges[edges$from != edges$to, , drop = FALSE]
  edges$prob <- P[cbind(edges$from, edges$to)]
  edges <- edges[edges$prob > 0, , drop = FALSE]
  edges[[paste0(dims[1])]]      <- centroids[edges$from, dims[1]]
  edges[[paste0(dims[2])]]      <- centroids[edges$from, dims[2]]
  edges[[paste0(dims[1], "end")]] <- centroids[edges$to, dims[1]]
  edges[[paste0(dims[2], "end")]] <- centroids[edges$to, dims[2]]
  centroids$occupancy <- occupancy[centroids$state]

  xl <- .nova_axis_label(dims[1], var_exp); yl <- .nova_axis_label(dims[2], var_exp)

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = tr, ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]]),
                        colour = "grey80", size = 1.1, alpha = 0.5)
  if (nrow(edges) > 0L) {
    p <- p + ggplot2::geom_curve(data = edges,
        ggplot2::aes(x = .data[[dims[1]]], y = .data[[dims[2]]],
                     xend = .data[[paste0(dims[1], "end")]],
                     yend = .data[[paste0(dims[2], "end")]],
                     linewidth = .data$prob, alpha = .data$prob),
        curvature = 0.22, colour = "#6A3D9A",
        arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed")) +
      ggplot2::scale_linewidth_continuous(range = c(0.3, 2.6), name = "P(transition)") +
      ggplot2::scale_alpha_continuous(range = c(0.35, 0.95), guide = "none")
  }
  p +
    ggplot2::geom_point(data = centroids,
        ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]], size = .data$occupancy),
        colour = "#E31A1C", fill = "#E31A1C", alpha = 0.85, shape = 21, stroke = 0) +
    ggplot2::geom_text(data = centroids,
        ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]], label = .data$state),
        colour = "white", fontface = "bold", size = 3.2) +
    ggplot2::scale_size_continuous(range = c(5, 13), name = "Occupancy") +
    ggplot2::labs(title = "State-flow diagram",
                  subtitle = "Node = network state (size = occupancy) - arrow = transition probability",
                  x = xl, y = yl, caption = "nova_transition_matrix()") +
    nova_theme()
}

#' @export
print.nova_transition_matrix <- function(x, ...) {
  cat("<nova_transition_matrix> k =", x$k, "states\n")
  print(round(x$transition, 3))
  cat("\noccupancy:\n"); print(round(x$occupancy, 3))
  invisible(x)
}

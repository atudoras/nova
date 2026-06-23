# dynamics_similarity.R
# nova_trajectory_similarity(): compare whole trajectories (drugs, genotypes,
# maturation conditions) as curves, not as point clouds.
#
# Scientific rationale ------------------------------------------------------
# Two perturbations can reach a similar endpoint by very different routes, or
# diverge early then re-converge. Endpoint statistics miss this; trajectory
# distances capture the *shape and timing* of the response. We provide four
# complementary metrics:
#   * "euclidean": mean pointwise distance on a common time grid -> sensitive to
#                  WHEN states differ (timing-locked comparison).
#   * "dtw":       Dynamic Time Warping -> aligns curves allowing local time
#                  stretching; sensitive to PATH SHAPE regardless of kinetics.
#   * "frechet":   discrete Frechet ("dog-walking") distance -> worst-case
#                  separation under monotonic alignment; robust to sampling.
#   * "cosine":    1 - mean cosine of corresponding STEP vectors -> compares
#                  DIRECTION of motion (shape) ignoring magnitude/speed.
# DTW and Frechet are implemented in base R (no `dtw` dependency required);
# if the `dtw` package is installed it is used as a cross-check accelerator.

# Internal: resample a trajectory (point matrix P) to `m` equally spaced points
# along its own arc length (for Euclidean comparison of unequal-length paths).
.nova_resample <- function(P, m) {
  n <- nrow(P)
  if (n == m && n > 1) return(P)
  if (n == 1) return(P[rep(1, m), , drop = FALSE])
  d <- c(0, cumsum(sqrt(rowSums((P[-1, , drop = FALSE] - P[-n, , drop = FALSE])^2))))
  if (utils::tail(d, 1) == 0) return(P[rep(1, m), , drop = FALSE])
  u <- d / utils::tail(d, 1)
  grid <- seq(0, 1, length.out = m)
  apply(P, 2, function(col) stats::approx(u, col, xout = grid)$y)
}

# Internal: native multivariate DTW distance (Euclidean local cost),
# normalised by warping-path length for comparability across pairs.
.nova_dtw <- function(A, B) {
  if (requireNamespace("dtw", quietly = TRUE)) {
    cm <- as.matrix(stats::dist(rbind(A, B)))[seq_len(nrow(A)), nrow(A) + seq_len(nrow(B)), drop = FALSE]
    al <- dtw::dtw(cm, distance.only = TRUE)
    return(al$normalizedDistance)
  }
  n <- nrow(A); m <- nrow(B)
  C <- matrix(0, n, m)
  for (i in seq_len(n)) C[i, ] <- sqrt(colSums((t(B) - A[i, ])^2))
  D <- matrix(Inf, n + 1, m + 1); D[1, 1] <- 0
  for (i in seq_len(n)) for (j in seq_len(m))
    D[i + 1, j + 1] <- C[i, j] + min(D[i, j + 1], D[i + 1, j], D[i, j])
  # normalise by path length (n + m), the standard symmetric normalisation
  D[n + 1, m + 1] / (n + m)
}

# Internal: discrete Frechet distance (Eiter & Mannila 1994) via memoised DP.
.nova_frechet <- function(A, B) {
  n <- nrow(A); m <- nrow(B)
  ca <- matrix(-1, n, m)
  d <- function(i, j) sqrt(sum((A[i, ] - B[j, ])^2))
  for (i in seq_len(n)) for (j in seq_len(m)) {
    cost <- d(i, j)
    if (i == 1 && j == 1) ca[i, j] <- cost
    else if (i == 1)      ca[i, j] <- max(ca[i, j - 1], cost)
    else if (j == 1)      ca[i, j] <- max(ca[i - 1, j], cost)
    else                  ca[i, j] <- max(min(ca[i - 1, j], ca[i - 1, j - 1], ca[i, j - 1]), cost)
  }
  ca[n, m]
}

# Internal: cosine-of-steps distance (direction-of-motion shape).
.nova_cosine_steps <- function(A, B) {
  m <- min(nrow(A), nrow(B))
  if (m < 2) return(NA_real_)
  A <- .nova_resample(A, m); B <- .nova_resample(B, m)
  Va <- A[-1, , drop = FALSE] - A[-m, , drop = FALSE]
  Vb <- B[-1, , drop = FALSE] - B[-m, , drop = FALSE]
  cs <- vapply(seq_len(m - 1), function(k) {
    den <- sqrt(sum(Va[k, ]^2)) * sqrt(sum(Vb[k, ]^2))
    if (den < .Machine$double.eps) NA_real_ else sum(Va[k, ] * Vb[k, ]) / den
  }, numeric(1))
  1 - mean(cs, na.rm = TRUE)        # 0 = identical direction, 2 = opposite
}

#' Trajectory similarity and clustering
#'
#' Computes a pairwise distance matrix between trajectories using a chosen
#' metric, then clusters them (hierarchical) and returns a dendrogram. Useful
#' for grouping drugs by mechanism, comparing genotypes, or relating maturation
#' conditions by the shape of their state-space path.
#'
#' @param x A PCA result, embedding data frame, or \code{nova_trajectories}.
#' @param method One of \code{"dtw"}, \code{"frechet"}, \code{"euclidean"},
#'   \code{"cosine"} (default \code{"dtw"}).
#' @param dims,group_var,unit_var,timepoint_var,timepoint_order Passed to
#'   \code{nova_extract_trajectories()} when extraction is needed. By default one
#'   trajectory per \code{group_var} is compared; set \code{unit_var} to compare
#'   individual replicates.
#' @param n_clusters Optional integer; if given, cluster assignments via
#'   \code{cutree()} are returned.
#' @param verbose Logical.
#' @return Object of class \code{nova_trajectory_similarity} with
#'   \code{distance} (matrix), \code{hclust}, \code{clusters} (if requested),
#'   and \code{plots} (\code{dendrogram}, \code{heatmap}).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0,1,2,3, 0,1,2,3, 0,-1,-2,-3),
#'   PC2 = c(0,0,0,0, 0,1,2,3, 0,0,0,0),
#'   Treatment = rep(c("A","B","C"), each = 4),
#'   Timepoint = rep(c("baseline","30min","1h","2h"), 3)
#' )
#' s <- nova_trajectory_similarity(df, method = "frechet", verbose = FALSE)
#' s$distance
#' @export
nova_trajectory_similarity <- function(x,
                                       method = c("dtw", "frechet", "euclidean", "cosine"),
                                       dims = c("PC1", "PC2"),
                                       group_var = NULL,
                                       unit_var = NULL,
                                       timepoint_var = "Timepoint",
                                       timepoint_order = NULL,
                                       n_clusters = NULL,
                                       verbose = TRUE) {
  method <- match.arg(method)
  tr <- if (inherits(x, "nova_trajectories")) x else
    nova_extract_trajectories(x, dims = dims, group_var = group_var,
                              unit_var = unit_var, timepoint_var = timepoint_var,
                              timepoint_order = timepoint_order)
  dims <- attr(tr, "dims")

  ids <- unique(tr$traj_id)
  if (length(ids) < 2L) stop("Need at least two trajectories to compare; got ", length(ids), ".")
  paths <- lapply(ids, function(id) {
    s <- tr[tr$traj_id == id, , drop = FALSE]; s <- s[order(s$time_rank), ]
    as.matrix(s[, dims, drop = FALSE])
  })
  names(paths) <- ids

  K <- length(ids)
  D <- matrix(0, K, K, dimnames = list(ids, ids))
  common_m <- stats::median(vapply(paths, nrow, integer(1)))
  for (i in seq_len(K - 1L)) for (j in (i + 1L):K) {
    A <- paths[[i]]; B <- paths[[j]]
    dij <- switch(method,
      dtw       = .nova_dtw(A, B),
      frechet   = .nova_frechet(A, B),
      cosine    = .nova_cosine_steps(A, B),
      euclidean = {
        m <- max(nrow(A), nrow(B), common_m)
        mean(sqrt(rowSums((.nova_resample(A, m) - .nova_resample(B, m))^2)))
      })
    D[i, j] <- D[j, i] <- dij
  }

  dd <- stats::as.dist(D)
  hc <- stats::hclust(dd, method = "average")
  clusters <- if (!is.null(n_clusters)) stats::cutree(hc, k = min(n_clusters, K)) else NULL

  # Dendrogram (ggplot rebuild of hclust, no extra deps) ----------------------
  dendro <- .nova_ggdendro(hc, method)

  # Distance heatmap ----------------------------------------------------------
  ord <- hc$order
  long <- expand.grid(row = ids[ord], col = ids[ord], stringsAsFactors = FALSE)
  long$dist <- D[cbind(match(long$row, ids), match(long$col, ids))]
  long$row <- factor(long$row, levels = ids[ord])
  long$col <- factor(long$col, levels = ids[ord])
  heatmap <- ggplot2::ggplot(long, ggplot2::aes(.data$col, .data$row, fill = .data$dist)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1, name = "Distance") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Trajectory distance matrix",
                  subtitle = paste0("Metric: ", method, " - ordered by hierarchical clustering"),
                  x = NULL, y = NULL, caption = "nova_trajectory_similarity()") +
    nova_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (verbose) message("nova_trajectory_similarity: ", K, " trajectories, method = ", method,
                       if (method == "dtw" && !requireNamespace("dtw", quietly = TRUE))
                         " (native DP; install 'dtw' for the C implementation)" else "")

  structure(list(
    distance = D, hclust = hc, clusters = clusters, method = method,
    plots = list(dendrogram = dendro, heatmap = heatmap),
    params = list(dims = dims, group_var = attr(tr, "group_var"))
  ), class = c("nova_trajectory_similarity", "nova_dynamics_result", "list"))
}

# Internal: minimal ggplot dendrogram from an hclust object (no ggdendro dep).
.nova_ggdendro <- function(hc, method) {
  merge <- hc$merge; height <- hc$height; order <- hc$order
  n <- nrow(merge) + 1L
  xpos <- numeric(n); xpos[order] <- seq_len(n)
  node_x <- numeric(nrow(merge)); node_y <- height
  get_xy <- function(k) if (k < 0) c(xpos[-k], 0) else c(node_x[k], node_y[k])
  segs <- data.frame()
  for (i in seq_len(nrow(merge))) {
    l <- get_xy(merge[i, 1]); r <- get_xy(merge[i, 2])
    node_x[i] <- (l[1] + r[1]) / 2
    h <- height[i]
    segs <- rbind(segs,
      data.frame(x = l[1], xend = l[1], y = l[2], yend = h),
      data.frame(x = r[1], xend = r[1], y = r[2], yend = h),
      data.frame(x = l[1], xend = r[1], y = h,    yend = h))
  }
  labs <- data.frame(x = seq_len(n), label = hc$labels[order])
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = segs,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      linewidth = 0.7, colour = "grey25", lineend = "round") +
    ggplot2::geom_text(data = labs,
      ggplot2::aes(x = .data$x, y = -max(height) * 0.03, label = .data$label),
      angle = 45, hjust = 1, size = 3.4) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.18, 0.05))) +
    ggplot2::labs(title = "Trajectory dendrogram",
                  subtitle = paste0("Hierarchical clustering (average linkage) on ", method, " distance"),
                  x = NULL, y = "Distance", caption = "nova_trajectory_similarity()") +
    nova_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
}

#' @export
print.nova_trajectory_similarity <- function(x, ...) {
  cat("<nova_trajectory_similarity> method =", x$method, "\n")
  print(round(x$distance, 3))
  if (!is.null(x$clusters)) { cat("\nclusters:\n"); print(x$clusters) }
  invisible(x)
}

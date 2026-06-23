# dynamics_landscape.R
# nova_landscape(): visualise where in state space neuronal networks spend time.
#
# Scientific rationale ------------------------------------------------------
# If many samples (across replicates, timepoints, conditions) revisit the same
# embedding region, that region behaves like an attractor basin. The density of
# occupied states approximates the system's preferred configurations. Following
# the standard "energy-landscape" heuristic, a pseudo-potential
#     U(x) = -log p(x)
# turns high-density regions into valleys (low U) and rarely visited regions
# into hills (high U). This is a DESCRIPTIVE heuristic, not a fitted potential;
# nova_describe() states that caveat explicitly.
#
# Implementation: 2D kernel density via MASS::kde2d when available, with a
# dependency-free binned-density fallback so the function never fails to render.

# Internal: 2D density grid (list with x, y, z) from points, no hard MASS dep.
.nova_density2d <- function(px, py, n = 120) {
  rng_x <- range(px, finite = TRUE); rng_y <- range(py, finite = TRUE)
  pad_x <- diff(rng_x) * 0.08 + 1e-6; pad_y <- diff(rng_y) * 0.08 + 1e-6
  lims <- c(rng_x[1] - pad_x, rng_x[2] + pad_x, rng_y[1] - pad_y, rng_y[2] + pad_y)
  if (requireNamespace("MASS", quietly = TRUE)) {
    d <- MASS::kde2d(px, py, n = n, lims = lims)
    return(list(x = d$x, y = d$y, z = d$z))
  }
  # Fallback: 2D histogram smoothed by a small Gaussian-ish kernel.
  xs <- seq(lims[1], lims[2], length.out = n)
  ys <- seq(lims[3], lims[4], length.out = n)
  bw_x <- diff(rng_x) / 12 + 1e-6; bw_y <- diff(rng_y) / 12 + 1e-6
  z <- matrix(0, n, n)
  for (k in seq_along(px)) {
    z <- z + outer(stats::dnorm(xs, px[k], bw_x), stats::dnorm(ys, py[k], bw_y))
  }
  list(x = xs, y = ys, z = z / length(px))
}

# Internal: tidy a density grid into a long data frame for ggplot rasters.
.nova_grid_long <- function(d) {
  data.frame(
    x = rep(d$x, times = length(d$y)),
    y = rep(d$y, each = length(d$x)),
    z = as.vector(d$z)
  )
}

#' State-occupancy landscape
#'
#' Estimates and plots the density of occupied states in the embedding plane -
#' the regions neuronal networks visit most - plus a pseudo-potential surface
#' \eqn{U = -\log p} (valleys = frequently occupied / attractor-like) and an
#' optional overlay of the mean trajectories.
#'
#' @param x A PCA result, embedding data frame, or \code{nova_trajectories}.
#'   Density uses ALL samples in \code{plot_data} (not just group means) when a
#'   raw embedding is supplied.
#' @param dims Embedding columns (default \code{c("PC1","PC2")}).
#' @param group_var,timepoint_var,timepoint_order Extraction args (for the
#'   trajectory overlay).
#' @param n_grid Grid resolution per axis (default 120).
#' @param overlay_trajectories Logical; draw mean group trajectories on top
#'   (default \code{TRUE}).
#' @param verbose Logical.
#' @return Object of class \code{nova_landscape} with \code{density} (grid),
#'   \code{grid} (long df), and \code{plots} (\code{density}, \code{potential},
#'   \code{occupancy}).
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   PC1 = c(rnorm(60), rnorm(60, 4)), PC2 = c(rnorm(60), rnorm(60, 3)),
#'   Treatment = rep(c("A","B"), each = 60),
#'   Timepoint = rep(c("baseline","30min","1h","2h"), 30)
#' )
#' L <- nova_landscape(df, overlay_trajectories = FALSE, verbose = FALSE)
#' @export
nova_landscape <- function(x,
                           dims = c("PC1", "PC2"),
                           group_var = NULL,
                           timepoint_var = "Timepoint",
                           timepoint_order = NULL,
                           n_grid = 120,
                           overlay_trajectories = TRUE,
                           verbose = TRUE) {

  # Pull the FULL point cloud for density (every sample), and group means for overlay.
  if (is.list(x) && "plot_data" %in% names(x)) { pd <- x$plot_data; var_exp <- x$variance_explained }
  else if (inherits(x, "nova_trajectories")) { pd <- x; var_exp <- attr(x, "variance_explained"); dims <- attr(x, "dims") }
  else { pd <- x; var_exp <- NULL }
  pd <- as.data.frame(pd, stringsAsFactors = FALSE)

  if (!all(dims %in% names(pd))) stop("Embedding columns not found: ",
                                      paste(setdiff(dims, names(pd)), collapse = ", "))
  px <- pd[[dims[1]]]; py <- pd[[dims[2]]]
  ok <- is.finite(px) & is.finite(py); px <- px[ok]; py <- py[ok]
  if (length(px) < 5L) stop("Too few finite points (", length(px), ") to estimate a landscape.")

  d <- .nova_density2d(px, py, n = n_grid)
  long <- .nova_grid_long(d)
  long$potential <- -log(long$z + max(long$z) * 1e-6)

  xl <- .nova_axis_label(dims[1], var_exp); yl <- .nova_axis_label(dims[2], var_exp)

  tr <- NULL
  if (overlay_trajectories) {
    tr <- tryCatch(
      nova_extract_trajectories(pd, dims = dims, group_var = group_var,
                                timepoint_var = timepoint_var, timepoint_order = timepoint_order),
      error = function(e) NULL)
  }

  add_overlay <- function(p, line_col = "white") {
    if (is.null(tr)) return(p)
    base_pts <- tr[tr$time_rank == 1, ]
    p +
      ggplot2::geom_path(data = tr, ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]],
                          group = .data$traj_id), colour = line_col, linewidth = 0.9,
                          alpha = 0.9, lineend = "round",
                          arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed")) +
      ggplot2::geom_point(data = base_pts, ggplot2::aes(.data[[dims[1]]], .data[[dims[2]]]),
                          shape = 22, size = 2.8, fill = line_col, colour = "grey15")
  }

  # (1) Density landscape ------------------------------------------------------
  density_plot <- add_overlay(
    ggplot2::ggplot(long, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$z), interpolate = TRUE) +
      ggplot2::geom_contour(ggplot2::aes(z = .data$z), colour = "white", alpha = 0.25, linewidth = 0.25) +
      ggplot2::scale_fill_viridis_c(option = "magma", name = "Density"),
    line_col = "white") +
    ggplot2::labs(title = "State-occupancy landscape",
                  subtitle = "Brighter = more frequently occupied regions of state space",
                  x = xl, y = yl, caption = "nova_landscape() - KDE over all samples") +
    ggplot2::coord_cartesian(expand = FALSE) + nova_theme()

  # (2) Pseudo-potential -------------------------------------------------------
  potential_plot <- add_overlay(
    ggplot2::ggplot(long, ggplot2::aes(.data$x, .data$y)) +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$potential), interpolate = TRUE) +
      ggplot2::geom_contour(ggplot2::aes(z = .data$potential), colour = "white", alpha = 0.22, linewidth = 0.25) +
      ggplot2::scale_fill_viridis_c(option = "viridis", direction = -1, name = expression(U == -log~p)),
    line_col = "grey15") +
    ggplot2::labs(title = "Pseudo-potential landscape",
                  subtitle = "Valleys (dark) = attractor-like basins - heuristic, not a fitted energy",
                  x = xl, y = yl, caption = "nova_landscape()") +
    ggplot2::coord_cartesian(expand = FALSE) + nova_theme()

  # (3) Occupancy hexbin -------------------------------------------------------
  occ_df <- data.frame(x = px, y = py)
  occupancy_plot <- ggplot2::ggplot(occ_df, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_bin2d(bins = 28) +
    ggplot2::scale_fill_viridis_c(option = "cividis", name = "Count") +
    ggplot2::labs(title = "State occupancy map",
                  subtitle = "Binned counts of samples per state-space cell",
                  x = xl, y = yl, caption = "nova_landscape()") +
    nova_theme()

  if (verbose) message("nova_landscape: density over ", length(px), " samples on a ",
                       n_grid, "x", n_grid, " grid",
                       if (!requireNamespace("MASS", quietly = TRUE)) " (binned fallback; install 'MASS' for kde2d)" else "")

  structure(list(
    density = d, grid = tibble::as_tibble(long), trajectories = tr,
    plots = list(density = density_plot, potential = potential_plot, occupancy = occupancy_plot),
    params = list(dims = dims, n_points = length(px))
  ), class = c("nova_landscape", "nova_dynamics_result", "list"))
}

#' @export
print.nova_landscape <- function(x, ...) {
  cat("<nova_landscape> density grid:", length(x$density$x), "x", length(x$density$y),
      "| samples:", x$params$n_points, "\n")
  invisible(x)
}

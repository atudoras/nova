# dynamics_geometry.R
# nova_state_geometry(): quantify the geometry of state-space trajectories.
#
# Scientific rationale ------------------------------------------------------
# A neuronal network's path through PCA space is a curve x(t) in R^d. Its
# geometry encodes biology that a static scatter cannot:
#   * path length        = total network reconfiguration ("how much it moved")
#   * net displacement    = end-state separation from baseline
#   * straightness        = how directed the change was (1 = straight line)
#   * tortuosity          = path / net (>=1; large = wandering / reversing)
#   * speed = |dx|/dt     = rate of reconfiguration (uses REAL elapsed time)
#   * acceleration        = change in velocity (onset vs adaptation kinetics)
#   * directional persistence = mean cosine between consecutive steps
#                           (+1 = ballistic, 0 = random walk, -1 = oscillating)
# Using real elapsed time (time_numeric) means a fast acute drug response and a
# slow maturation drift are not conflated just because they share step counts.

# Internal: euclidean norm of a numeric vector.
.nova_norm <- function(v) sqrt(sum(v^2))

# Internal: nice axis label with variance-explained, when available.
.nova_axis_label <- function(dim, var_exp) {
  if (!is.null(var_exp) && dim %in% names(var_exp)) {
    sprintf("%s (%.1f%%)", dim, as.numeric(var_exp[[dim]]))
  } else dim
}

# Internal: per-trajectory geometric metrics from an ordered point matrix P
# (rows = timepoints) and a numeric time vector `t`.
.nova_traj_metrics <- function(P, t) {
  n <- nrow(P)
  if (n < 2L) {
    return(list(
      n_points = n, total_time = 0, path_length = 0, net_displacement = 0,
      straightness = NA_real_, tortuosity = NA_real_, mean_speed = NA_real_,
      max_speed = NA_real_, final_speed = NA_real_, mean_accel = NA_real_,
      directional_persistence = NA_real_,
      seg = data.frame()
    ))
  }
  V  <- P[-1, , drop = FALSE] - P[-n, , drop = FALSE]      # step vectors
  L  <- sqrt(rowSums(V^2))                                 # step lengths
  dt <- diff(t)
  dt[!is.finite(dt) | dt <= 0] <- 1                        # guard against bad time
  speed <- L / dt

  path_length <- sum(L)
  net <- .nova_norm(P[n, ] - P[1, ])
  straightness <- if (path_length > 0) net / path_length else NA_real_
  tortuosity   <- if (net > .Machine$double.eps) path_length / net else NA_real_

  # acceleration: magnitude of change in velocity vectors per unit time
  U <- V / dt                                              # velocity vectors
  if (n >= 3L) {
    dU <- U[-1, , drop = FALSE] - U[-(n - 1), , drop = FALSE]
    dtm <- (dt[-1] + dt[-(n - 1)]) / 2
    accel <- sqrt(rowSums(dU^2)) / dtm
    mean_accel <- mean(accel, na.rm = TRUE)
  } else mean_accel <- NA_real_

  # directional persistence: mean cosine between consecutive step vectors
  if (n >= 3L) {
    cosines <- vapply(seq_len(n - 2L), function(i) {
      a <- V[i, ]; b <- V[i + 1, ]
      den <- .nova_norm(a) * .nova_norm(b)
      if (den < .Machine$double.eps) NA_real_ else sum(a * b) / den
    }, numeric(1))
    dir_persist <- mean(cosines, na.rm = TRUE)
  } else {
    cosines <- NA_real_
    dir_persist <- NA_real_
  }

  seg <- data.frame(
    step       = seq_len(n - 1L),
    seg_length = L,
    dt         = dt,
    speed      = speed,
    cum_length = cumsum(L)
  )

  list(
    n_points = n, total_time = sum(dt), path_length = path_length,
    net_displacement = net, straightness = straightness, tortuosity = tortuosity,
    mean_speed = mean(speed, na.rm = TRUE), max_speed = max(speed, na.rm = TRUE),
    final_speed = speed[length(speed)], mean_accel = mean_accel,
    directional_persistence = dir_persist, seg = seg
  )
}

#' State-space trajectory geometry
#'
#' Quantifies the geometry of each group's (or unit's) trajectory through an
#' embedding (PCA/UMAP/latent) state space: path length, net displacement,
#' straightness, tortuosity, velocity, acceleration, and directional
#' persistence. Velocities use real elapsed time, so non-uniform timepoint
#' spacing (e.g. \code{baseline, 0min, 15min, 1h, 2h}) is handled correctly.
#'
#' @param x A \code{pca_analysis_enhanced()} result, a data frame of embedding
#'   coordinates + timepoint + grouping columns, or a \code{nova_trajectories}
#'   object from \code{nova_extract_trajectories()}.
#' @param dims Embedding columns to analyse (default \code{c("PC1","PC2")}).
#'   Metrics use all supplied dims; plots use the first two.
#' @param group_var,unit_var,timepoint_var,timepoint_order Passed to
#'   \code{nova_extract_trajectories()} when \code{x} is not already extracted.
#' @param verbose Logical; print a short summary (default \code{TRUE}).
#' @return An object of class \code{nova_state_geometry} (a list) with:
#'   \code{summary} (per-trajectory metrics), \code{segments} (per-step table),
#'   \code{trajectories} (the ordered points), and \code{plots}
#'   (\code{overlay}, \code{velocity}, \code{displacement}).
#' @examples
#' df <- data.frame(
#'   PC1 = c(0, 1, 2, 3, 0, 0.5, 1, 1.2),
#'   PC2 = c(0, 0, 0, 0, 0, 1, 0, 1),
#'   Treatment = rep(c("Directed", "Wander"), each = 4),
#'   Timepoint = rep(c("baseline", "30min", "1h", "2h"), 2)
#' )
#' g <- nova_state_geometry(df, group_var = "Treatment", verbose = FALSE)
#' g$summary
#' @export
nova_state_geometry <- function(x,
                                dims = c("PC1", "PC2"),
                                group_var = NULL,
                                unit_var = NULL,
                                timepoint_var = "Timepoint",
                                timepoint_order = NULL,
                                verbose = TRUE) {

  tr <- if (inherits(x, "nova_trajectories")) x else
    nova_extract_trajectories(x, dims = dims, group_var = group_var,
                              unit_var = unit_var, timepoint_var = timepoint_var,
                              timepoint_order = timepoint_order)
  dims <- attr(tr, "dims")
  var_exp <- attr(tr, "variance_explained")

  ids <- unique(tr$traj_id)
  summ_list <- list(); seg_list <- list()
  for (id in ids) {
    sub <- tr[tr$traj_id == id, , drop = FALSE]
    sub <- sub[order(sub$time_rank), , drop = FALSE]
    P <- as.matrix(sub[, dims, drop = FALSE])
    m <- .nova_traj_metrics(P, sub$time_numeric)
    summ_list[[id]] <- data.frame(
      traj_id = id, group = sub$group[1],
      n_points = m$n_points, total_time = m$total_time,
      path_length = m$path_length, net_displacement = m$net_displacement,
      straightness = m$straightness, tortuosity = m$tortuosity,
      mean_speed = m$mean_speed, max_speed = m$max_speed,
      final_speed = m$final_speed, mean_accel = m$mean_accel,
      directional_persistence = m$directional_persistence,
      stringsAsFactors = FALSE
    )
    if (nrow(m$seg) > 0L) {
      s <- m$seg
      s$traj_id <- id; s$group <- sub$group[1]
      s$from_label <- sub$time_label[-nrow(sub)]
      s$to_label   <- sub$time_label[-1]
      s[[dims[1]]]      <- sub[[dims[1]]][-nrow(sub)]
      s[[dims[2]]]      <- sub[[dims[2]]][-nrow(sub)]
      s[[paste0(dims[1], "end")]] <- sub[[dims[1]]][-1]
      s[[paste0(dims[2], "end")]] <- sub[[dims[2]]][-1]
      seg_list[[id]] <- s
    }
  }
  summary_df <- tibble::as_tibble(do.call(rbind, summ_list))
  segments_df <- tibble::as_tibble(do.call(rbind, seg_list))

  xl <- .nova_axis_label(dims[1], var_exp); yl <- .nova_axis_label(dims[2], var_exp)
  groups <- unique(tr$group); pal <- stats::setNames(nova_palette(length(groups)), groups)

  base_pts <- tr[tr$time_rank == 1, , drop = FALSE]
  end_pts  <- do.call(rbind, lapply(ids, function(id) {
    s <- tr[tr$traj_id == id, ]; s[which.max(s$time_rank), ]
  }))

  # (1) Trajectory overlay -----------------------------------------------------
  overlay <- ggplot2::ggplot(tr, ggplot2::aes(x = .data[[dims[1]]], y = .data[[dims[2]]],
                                              group = .data$traj_id, colour = .data$group)) +
    ggplot2::geom_path(linewidth = 1.1, lineend = "round",
                       arrow = grid::arrow(length = grid::unit(0.10, "inches"), type = "closed")) +
    ggplot2::geom_point(size = 2.2, alpha = 0.9) +
    ggplot2::geom_point(data = base_pts, shape = 22, size = 3.6, stroke = 1.0,
                        fill = "white", colour = "grey20") +
    ggplot2::scale_colour_manual(values = pal, name = attr(tr, "group_var")) +
    ggplot2::labs(title = "State-space trajectories",
                  subtitle = "Square = baseline - arrowhead = final state",
                  x = xl, y = yl,
                  caption = "nova_state_geometry() - path through embedding space") +
    nova_theme()

  # (2) Velocity-coloured segments --------------------------------------------
  velocity <- ggplot2::ggplot(segments_df,
      ggplot2::aes(x = .data[[dims[1]]], y = .data[[dims[2]]],
                   xend = .data[[paste0(dims[1], "end")]],
                   yend = .data[[paste0(dims[2], "end")]], colour = .data$speed)) +
    ggplot2::geom_segment(linewidth = 1.4, lineend = "round",
                          arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed")) +
    ggplot2::facet_wrap(ggplot2::vars(.data$group)) +
    ggplot2::scale_colour_viridis_c(option = "plasma", name = "Speed\n(units/min)") +
    ggplot2::labs(title = "Velocity along trajectories",
                  subtitle = "Segment colour = instantaneous speed (displacement / elapsed time)",
                  x = xl, y = yl, caption = "nova_state_geometry()") +
    nova_theme()

  # (3) Displacement-from-baseline over time ----------------------------------
  disp <- do.call(rbind, lapply(ids, function(id) {
    s <- tr[tr$traj_id == id, , drop = FALSE]; s <- s[order(s$time_rank), ]
    P <- as.matrix(s[, dims, drop = FALSE])
    d <- sqrt(rowSums((P - matrix(P[1, ], nrow(P), ncol(P), byrow = TRUE))^2))
    data.frame(traj_id = id, group = s$group, time_numeric = s$time_numeric,
               time_label = s$time_label, disp_from_baseline = d, stringsAsFactors = FALSE)
  }))
  disp$time_label <- factor(disp$time_label, levels = attr(tr, "timepoint_order"))
  displacement <- ggplot2::ggplot(disp, ggplot2::aes(x = .data$time_label,
                                  y = .data$disp_from_baseline,
                                  colour = .data$group, group = .data$traj_id)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = pal, name = attr(tr, "group_var")) +
    ggplot2::labs(title = "Displacement from baseline",
                  subtitle = "Distance travelled in state space relative to the baseline state",
                  x = "Timepoint", y = "Euclidean distance from baseline",
                  caption = "nova_state_geometry() - rising = divergence, plateau = settling") +
    nova_theme()

  if (verbose) {
    message("nova_state_geometry: ", length(ids), " trajectories x ",
            length(dims), " dims")
    message("  mean path length = ", round(mean(summary_df$path_length, na.rm = TRUE), 2),
            " | mean straightness = ", round(mean(summary_df$straightness, na.rm = TRUE), 2))
  }

  structure(list(
    summary = summary_df, segments = segments_df, trajectories = tr,
    displacement = tibble::as_tibble(disp),
    plots = list(overlay = overlay, velocity = velocity, displacement = displacement),
    params = list(dims = dims, group_var = attr(tr, "group_var"))
  ), class = c("nova_state_geometry", "nova_dynamics_result", "list"))
}

#' @export
print.nova_state_geometry <- function(x, ...) {
  cat("<nova_state_geometry>\n")
  cat("  trajectories:", nrow(x$summary), "| dims:", paste(x$params$dims, collapse = ", "), "\n")
  print(x$summary)
  invisible(x)
}

# metric_plots.R
# Per-metric bar, box, violin, and line plots for MEA data

#' Plot a Single MEA Metric Across Conditions
#'
#' Creates a bar (mean + error), box, violin, or line plot for one measured
#' variable from processed MEA data.
#'
#' @param data Data frame - long-format MEA data (must contain 'Variable' column).
#' @param metric Character. Exact name of the variable to plot.
#' @param x_var Character. Column to use as the x-axis (default "Timepoint").
#' @param group_by Character. Column to use for fill/colour grouping (default "Treatment").
#' @param facet_by Character or NULL. Column name for faceting. NULL = no facets.
#' @param filter_timepoints Character vector or NULL. Subset to these timepoints.
#' @param filter_treatments Character vector or NULL. Subset to these treatments.
#' @param filter_genotypes  Character vector or NULL. Subset to these genotypes.
#' @param value_column Character. Which column holds the numeric values.
#'   Defaults to "Normalized_Value" if present, else "Value".
#' @param error_type Character. "sem" (default), "sd", or "ci95".
#' @param plot_type Character. "bar" (default), "box", "violin", or "line".
#' @param colors Named character vector of colours, or NULL for ggplot2 defaults.
#' @param show_points Logical. Overlay individual data points (default TRUE).
#' @param point_alpha Numeric. Transparency of data points (default 0.6).
#' @param title Character or NULL. Plot title. NULL = metric name.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_mea_metric(processed$all_data, "Mean Firing Rate (Hz)")
#' plot_mea_metric(processed$all_data, "Burst Rate (Hz)",
#'                 plot_type = "violin", facet_by = "Genotype")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_boxplot geom_violin geom_line
#'   geom_point geom_errorbar facet_wrap labs theme_bw theme
#'   scale_fill_manual scale_colour_manual element_text position_dodge
#' @importFrom dplyr filter group_by summarise mutate n across all_of
#' @export
plot_mea_metric <- function(
    data,
    metric,
    x_var             = "Timepoint",
    group_by          = "Treatment",
    facet_by          = NULL,
    filter_timepoints = NULL,
    filter_treatments = NULL,
    filter_genotypes  = NULL,
    value_column      = NULL,
    error_type        = c("sem", "sd", "ci95"),
    plot_type         = c("bar", "box", "violin", "line"),
    colors            = NULL,
    show_points       = TRUE,
    point_alpha       = 0.6,
    title             = NULL
) {
  error_type <- match.arg(error_type)
  plot_type  <- match.arg(plot_type)

  # resolve value column
  if (is.null(value_column)) {
    value_column <- if ("Normalized_Value" %in% names(data)) "Normalized_Value" else "Value"
  }
  y_label <- if (value_column == "Normalized_Value") "Normalized Value" else "Value"

  # validate metric
  if (!"Variable" %in% names(data)) stop("'data' must contain a 'Variable' column")
  if (!metric %in% data$Variable) {
    avail <- paste(head(unique(data$Variable), 6), collapse = ", ")
    stop("Metric '", metric, "' not found in data$Variable. Available (first 6): ", avail)
  }

  # filter
  d <- data[data$Variable == metric, , drop = FALSE]
  if (!is.null(filter_timepoints) && "Timepoint" %in% names(d))
    d <- d[d$Timepoint %in% filter_timepoints, , drop = FALSE]
  if (!is.null(filter_treatments) && "Treatment" %in% names(d))
    d <- d[d$Treatment %in% filter_treatments, , drop = FALSE]
  if (!is.null(filter_genotypes)  && "Genotype"  %in% names(d))
    d <- d[d$Genotype  %in% filter_genotypes,  , drop = FALSE]
  if (nrow(d) == 0) stop("No data remaining after applying filters")

  d$.value <- as.numeric(d[[value_column]])

  # error bar functions
  sem_fn  <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  ci95_fn <- function(x) qt(0.975, df = max(1, sum(!is.na(x)) - 1)) * sem_fn(x)
  err_fn  <- switch(error_type,
    sem  = sem_fn,
    sd   = function(x) sd(x, na.rm = TRUE),
    ci95 = ci95_fn
  )

  group_vars <- unique(c(x_var, group_by, facet_by))
  group_vars <- group_vars[group_vars %in% names(d)]

  summ <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      mean_val = mean(.value, na.rm = TRUE),
      err      = err_fn(.value),
      n        = sum(!is.na(.value)),
      .groups  = "drop"
    )

  dodge <- ggplot2::position_dodge(width = 0.8)

  p <- ggplot2::ggplot()

  if (plot_type == "bar") {
    p <- p +
      ggplot2::geom_bar(
        data     = summ,
        ggplot2::aes(x = .data[[x_var]], y = mean_val, fill = .data[[group_by]]),
        stat     = "identity", position = dodge, alpha = 0.85
      ) +
      ggplot2::geom_errorbar(
        data  = summ,
        ggplot2::aes(x = .data[[x_var]],
                     ymin = mean_val - err, ymax = mean_val + err,
                     group = .data[[group_by]]),
        width = 0.2, position = dodge
      )

  } else if (plot_type == "line") {
    p <- p +
      ggplot2::geom_line(
        data = summ,
        ggplot2::aes(x = .data[[x_var]], y = mean_val,
                     colour = .data[[group_by]], group = .data[[group_by]])
      ) +
      ggplot2::geom_errorbar(
        data  = summ,
        ggplot2::aes(x = .data[[x_var]],
                     ymin = mean_val - err, ymax = mean_val + err,
                     colour = .data[[group_by]], group = .data[[group_by]]),
        width = 0.15
      )

  } else if (plot_type == "box") {
    p <- p +
      ggplot2::geom_boxplot(
        data  = d,
        ggplot2::aes(x = .data[[x_var]], y = .value, fill = .data[[group_by]]),
        position = dodge, outlier.shape = NA, alpha = 0.75
      )

  } else if (plot_type == "violin") {
    p <- p +
      ggplot2::geom_violin(
        data  = d,
        ggplot2::aes(x = .data[[x_var]], y = .value, fill = .data[[group_by]]),
        position = dodge, alpha = 0.75, trim = FALSE
      )
  }

  if (show_points && plot_type %in% c("bar", "line", "violin")) {
    p <- p +
      ggplot2::geom_point(
        data     = d,
        ggplot2::aes(x = .data[[x_var]], y = .value,
                     colour = .data[[group_by]], group = .data[[group_by]]),
        position = ggplot2::position_jitterdodge(dodge.width = 0.8, jitter.width = 0.1),
        alpha    = point_alpha, size = 2
      )
  }

  if (!is.null(facet_by) && facet_by %in% names(d)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_by)))
  }

  if (!is.null(colors)) {
    p <- p +
      ggplot2::scale_fill_manual(values   = colors) +
      ggplot2::scale_colour_manual(values = colors)
  }

  p <- p +
    ggplot2::labs(
      title  = if (is.null(title)) metric else title,
      x      = x_var,
      y      = y_label,
      fill   = group_by,
      colour = group_by
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  p
}

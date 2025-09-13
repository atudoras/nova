# utilities.R
# Helper functions and utilities for MEA data analysis

#' Standardize Timepoint Names
#' 
#' Template function for standardizing timepoint naming across experiments
#'
#' @param timepoints Character vector of timepoint names
#' @param naming_scheme Character. Standardization scheme to apply
#' @param ... Additional parameters
#'
#' @return Character vector with standardized timepoint names
#'
#' @examples
#' \dontrun{
#' # Standardize timepoint names
#' std_timepoints <- standardize_timepoint_names(c("1h", "1hr", "60min"))
#' }
#'
#' @export

handle_missing_values <- function(data, value_column, method, verbose) {
  original_na <- sum(is.na(data[[value_column]]))
  
  switch(method,
         "remove" = {
           data <- data %>% filter(!is.na(.data[[value_column]]))
         },
         "impute_mean" = {
           mean_val <- mean(data[[value_column]], na.rm = TRUE)
           data[[value_column]][is.na(data[[value_column]])] <- mean_val
         },
         "impute_zero" = {
           data[[value_column]][is.na(data[[value_column]])] <- 0
         }
  )
  
  final_na <- sum(is.na(data[[value_column]]))
  if (verbose && original_na > 0) {
    cat("Missing values: ", original_na, "->", final_na, "(method:", method, ")\n")
  }
  
  return(data)
}

quality_filter <- function(data, variable_column, value_column, grouping_columns, 
                           quality_threshold, min_observations, verbose) {
  original_vars <- length(unique(data[[variable_column]]))
  
  # Remove variables with insufficient observations
  var_counts <- data %>%
    group_by(.data[[variable_column]]) %>%
    summarise(n_obs = n(), .groups = 'drop') %>%
    filter(n_obs >= min_observations)
  
  data <- data %>% filter(.data[[variable_column]] %in% var_counts[[variable_column]])
  
  # Remove groups with insufficient data completeness
  if (length(grouping_columns) > 0) {
    for (group_col in grouping_columns) {
      if (group_col %in% names(data)) {
        group_quality <- data %>%
          group_by(.data[[group_col]]) %>%
          summarise(
            completeness = sum(!is.na(.data[[value_column]])) / n(),
            .groups = 'drop'
          ) %>%
          filter(completeness >= quality_threshold)
        
        data <- data %>% filter(.data[[group_col]] %in% group_quality[[group_col]])
      }
    }
  }
  
  final_vars <- length(unique(data[[variable_column]]))
  if (verbose && (final_vars != original_vars)) {
    cat("Quality filtering: ", original_vars, "->", final_vars, "variables retained\n")
  }
  
  return(data)
}

aggregate_data <- function(data, group_col, variable_column, value_column, method) {
  data %>%
    group_by(.data[[group_col]], .data[[variable_column]]) %>%
    summarise(
      agg_value = switch(method,
                         "mean" = mean(.data[[value_column]], na.rm = TRUE),
                         "median" = median(.data[[value_column]], na.rm = TRUE),
                         "sum" = sum(.data[[value_column]], na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

apply_scaling_enhanced <- function(matrix_data, scale_method, verbose = FALSE) {
  if (verbose) cat("Applying scaling method:", scale_method, "\n")
  
  switch(scale_method,
         "variable_0_10" = {
           scaled_matrix <- apply(matrix_data, 2, function(x) {
             x_clean <- x[is.finite(x)]
             if (length(x_clean) < 2) return(rep(5, length(x)))
             
             x_min <- min(x_clean)
             x_max <- max(x_clean)
             
             if (x_max > x_min) {
               scaled_x <- 10 * (x - x_min) / (x_max - x_min)
             } else {
               scaled_x <- rep(5, length(x))
             }
             scaled_x[!is.finite(scaled_x)] <- 0
             return(scaled_x)
           })
           as.matrix(scaled_matrix)
         },
         "robust" = {
           # Robust scaling using median and MAD
           scaled_matrix <- apply(matrix_data, 2, function(x) {
             x_clean <- x[is.finite(x)]
             if (length(x_clean) < 2) return(x)
             
             med <- median(x_clean)
             mad_val <- mad(x_clean)
             
             if (mad_val > 0) {
               (x - med) / mad_val
             } else {
               x - med
             }
           })
           as.matrix(scaled_matrix)
         },
         "row" = t(scale(t(matrix_data))),
         "column" = scale(matrix_data),
         "none" = matrix_data,
         {
           if (verbose) cat("Unknown scaling method, using variable_0_10\n")
           apply_scaling_enhanced(matrix_data, "variable_0_10", verbose)
         }
  )
}

clean_heatmap_matrix <- function(matrix_data, min_finite = 2, verbose = FALSE) {
  if (nrow(matrix_data) == 0 || ncol(matrix_data) == 0) {
    if (verbose) cat("Warning: Empty matrix\n")
    return(NULL)
  }
  
  # Remove columns and rows with insufficient data
  valid_cols <- apply(matrix_data, 2, function(x) sum(is.finite(x)) >= min_finite)
  valid_rows <- apply(matrix_data, 1, function(x) sum(is.finite(x)) >= min_finite)
  
  matrix_clean <- matrix_data[valid_rows, valid_cols, drop = FALSE]
  
  if (verbose) {
    removed_cols <- sum(!valid_cols)
    removed_rows <- sum(!valid_rows)
    if (removed_cols > 0 || removed_rows > 0) {
      cat("Removed", removed_rows, "rows and", removed_cols, "columns due to insufficient data\n")
    }
    cat("Final matrix dimensions:", nrow(matrix_clean), "x", ncol(matrix_clean), "\n")
  }
  
  if (nrow(matrix_clean) < 2 || ncol(matrix_clean) < 2) {
    if (verbose) cat("Warning: Insufficient data for heatmap\n")
    return(NULL)
  }
  
  return(matrix_clean)
}

create_annotations_enhanced <- function(rownames_vector, factor_cols) {
  # Parse combined names back to individual factors
  split_names <- strsplit(rownames_vector, "_")
  n_factors <- length(factor_cols)
  
  annotations <- data.frame(row.names = rownames_vector)
  
  for (i in seq_along(factor_cols)) {
    annotations[[factor_cols[i]]] <- sapply(split_names, function(x) {
      if (length(x) >= i) x[i] else NA
    })
  }
  
  # Create color palettes for each factor
  annotation_colors <- list()
  
  for (factor_col in factor_cols) {
    unique_vals <- unique(annotations[[factor_col]])
    n_vals <- length(unique_vals)
    
    if (n_vals <= 12) {
      colors <- RColorBrewer::brewer.pal(max(3, n_vals), "Set3")[1:n_vals]
    } else {
      colors <- rainbow(n_vals)
    }
    
    names(colors) <- unique_vals
    annotation_colors[[factor_col]] <- colors
  }
  
  return(list(annotations = annotations, colors = annotation_colors))
}

create_color_palette_enhanced <- function(palette_name = "yellow_purple", custom_colors = NULL, data_matrix = NULL) {
  if (!is.null(custom_colors)) {
    colors <- colorRampPalette(custom_colors)(100)
    breaks <- seq(min(data_matrix, na.rm = TRUE), max(data_matrix, na.rm = TRUE), length.out = 101)
  } else {
    switch(palette_name,
           "yellow_purple" = {
             colors <- colorRampPalette(c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"))(100)
             data_range <- range(data_matrix, na.rm = TRUE)
             breaks <- seq(data_range[1], data_range[2], length.out = 101)
           },
           "viridis" = {
             colors <- viridis::viridis(100)
             data_range <- range(data_matrix, na.rm = TRUE)
             breaks <- seq(data_range[1], data_range[2], length.out = 101)
           },
           "RdBu" = {
             colors <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(100)
             # For correlation matrices, center around 0
             data_range <- range(data_matrix, na.rm = TRUE)
             max_abs <- max(abs(data_range))
             breaks <- seq(-max_abs, max_abs, length.out = 101)
           },
           "plasma" = {
             colors <- viridis::plasma(100)
             data_range <- range(data_matrix, na.rm = TRUE)
             breaks <- seq(data_range[1], data_range[2], length.out = 101)
           },
           "magma" = {
             colors <- viridis::magma(100)
             data_range <- range(data_matrix, na.rm = TRUE)
             breaks <- seq(data_range[1], data_range[2], length.out = 101)
           },
           {
             # Default to improved yellow-purple
             colors <- colorRampPalette(c("#FFFFCC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"))(100)
             data_range <- range(data_matrix, na.rm = TRUE)
             breaks <- seq(data_range[1], data_range[2], length.out = 101)
           }
    )
  }
  
  return(list(colors = colors, breaks = breaks))
}

# Helper function for null coalescing (R equivalent of %||%)
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

print_detailed_summary <- function(top_vars, pc_x_top, pc_y_top, high_both, 
                                   pc_x, pc_y, top_n, min_loading_threshold) {
  
  cat("\n=== TOP", top_n, "VARIABLES BY COMBINED IMPORTANCE ===\n")
  for (i in 1:nrow(top_vars)) {
    cat(sprintf("%2d. %-30s | %s: %7.3f | %s: %7.3f | Combined: %6.3f\n",
                i, top_vars$Variable[i], 
                pc_x, top_vars$PC_X_Loading[i],
                pc_y, top_vars$PC_Y_Loading[i],
                top_vars$Combined_Importance[i]))
  }
  
  cat(paste("\n=== MOST IMPORTANT FOR", pc_x, "===\n"))
  for (i in 1:min(nrow(pc_x_top), 5)) {  # Show top 5
    direction <- ifelse(pc_x_top$PC_X_Loading[i] > 0, "(+)", "(-)")
    cat(sprintf("%2d. %-30s | Loading: %7.3f %s\n",
                i, pc_x_top$Variable[i], 
                pc_x_top$PC_X_Loading[i], direction))
  }
  
  cat(paste("\n=== MOST IMPORTANT FOR", pc_y, "===\n"))
  for (i in 1:min(nrow(pc_y_top), 5)) {  # Show top 5
    direction <- ifelse(pc_y_top$PC_Y_Loading[i] > 0, "(+)", "(-)")
    cat(sprintf("%2d. %-30s | Loading: %7.3f %s\n",
                i, pc_y_top$Variable[i], 
                pc_y_top$PC_Y_Loading[i], direction))
  }
  
  if (nrow(high_both) > 0) {
    cat(paste("\n=== VARIABLES IMPORTANT IN BOTH", pc_x, "AND", pc_y, "===\n"))
    cat("(Absolute loading >", min_loading_threshold, "in both PCs)\n")
    for (i in 1:nrow(high_both)) {
      cat(sprintf("%-30s | %s: %7.3f | %s: %7.3f\n",
                  high_both$Variable[i],
                  pc_x, high_both$PC_X_Loading[i],
                  pc_y, high_both$PC_Y_Loading[i]))
    }
  } else {
    cat(paste("\nNo variables exceed threshold (", min_loading_threshold, ") in both PCs\n"))
  }
}

setup_color_scheme <- function(color_scheme, custom_colors) {
  if (color_scheme == "viridis") {
    return(list(
      primary = "#440154FF",
      secondary = "#21908CFF",
      accent = "#FDE725FF",
      gradient = viridis::viridis(100)
    ))
  } else if (color_scheme == "custom" && !is.null(custom_colors)) {
    return(custom_colors)
  } else {
    # Default color scheme
    return(list(
      primary = "#0066CC",
      secondary = "#CC0000",
      accent = "#FF6600",
      gradient = c("#0066CC", "#FFFFFF", "#CC0000")
    ))
  }
}

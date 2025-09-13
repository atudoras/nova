# plots.R
# Functions for visualizing MEA data and PCA results

#' Plot PCA Biplot
#' 
#' Template function for creating PCA biplots
#'
#' @param pca_result PCA result object or scores data frame
#' @param pc_x Integer. Principal component for x-axis (default: 1)
#' @param pc_y Integer. Principal component for y-axis (default: 2)
#' @param color_by Character. Variable to color points by
#' @param shape_by Character. Variable to shape points by
#' @param show_loadings Logical. Whether to show loading vectors (default: TRUE)
#' @param ... Additional plotting parameters
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' # Create PCA biplot colored by treatment
#' p <- plot_pca_biplot(pca_results, color_by = "treatment")
#' print(p)
#' }
#'
#' Enhanced PCA Plotting for Neural and Omics Data
#'
#' Creates publication-ready PCA plots with scientific color palettes, flexible
#' aesthetic mapping, and multiple visualization options. Designed specifically
#' for neural activity and omics datasets with support for complex experimental
#' designs including treatments, genotypes, and timepoints.
#'
#' @param pca_output List. Complete PCA output object from pca_analysis_enhanced() (optional)
#' @param plot_data Data.frame. Data containing PC coordinates and metadata variables
#' @param pca_result List. PCA result object (e.g., from prcomp() or princomp())
#' @param output_dir Character. Directory path for saving plots (default: current working directory)
#' @param processing_result List. Result object from process_mea_flexible() (optional)
#' @param experiment_name Character. Name for the experiment (used in titles and filenames)
#' @param grouping_variables Character vector. Available metadata variables for plotting (default: c("Treatment", "Genotype", "Timepoint"))
#' @param color_variable Character. Variable name for color aesthetic (default: "Treatment")
#' @param shape_variable Character. Variable name for shape aesthetic (default: "Genotype")
#' @param secondary_shape_variable Character. Alternative shape variable (default: "Timepoint")
#' @param components Numeric vector. PC components to plot (default: c(1, 2))
#' @param gray_color_value Character. Specific value of color_variable to display in gray (default: NULL)
#' @param save_plots Logical. Whether to save plots to files (default: TRUE)
#' @param plot_width Numeric. Plot width in inches (default: 12)
#' @param plot_height Numeric. Plot height in inches (default: 10)
#' @param dpi Numeric. Plot resolution (default: 300)
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{plots}{Named list of ggplot objects for each plot type}
#'   \item{plot_data}{Data.frame with plotting data and metadata}
#'   \item{variance_explained}{Numeric vector of variance explained by each component}
#'   \item{components_plotted}{Numeric vector of components used in plots}
#'   \item{color_palette}{Named character vector of colors used}
#'   \item{shape_palette}{Named numeric vector of shapes used}
#'   \item{plotting_config}{List of configuration parameters used}
#'   \item{saved_files}{Character vector of saved file paths (if save_plots = TRUE)}
#'
#' @details
#' The function creates up to 5 different plot variants:
#' \enumerate{
#'   \item Primary combination: color + shape mapping
#'   \item Secondary combination: color + alternative shape mapping
#'   \item Color only: simplified view with larger points
#'   \item Color with ellipses: 95% confidence ellipses around groups
#'   \item Faceted plots: color mapping with faceting by third variable
#' }
#'
#' The function uses a carefully curated scientific color palette optimized for
#' publications and colorblind accessibility. The gray_color_value parameter
#' allows highlighting specific experimental conditions by displaying them in gray.
#'
#' Input flexibility:
#' \itemize{
#'   \item Use complete pca_output object from integrated analysis pipeline
#'   \item Use processing_result from process_mea_flexible() with separate PCA results
#'   \item Manually specify plot_data and pca_result for custom workflows
#' }
#'
#' @examples
#' \dontrun{
#' # Using complete PCA output object
#' plots <- pca_plots_enhanced(
#'   pca_output = my_pca_results,
#'   experiment_name = "MEA_Experiment_2024",
#'   gray_color_value = "Control"
#' )
#'
#' # Using separate components
#' plots <- pca_plots_enhanced(
#'   plot_data = pca_data,
#'   pca_result = pca_obj,
#'   experiment_name = "Custom_Analysis",
#'   color_variable = "Treatment",
#'   shape_variable = "Genotype",
#'   components = c(2, 3)
#' )
#'
#' # Access individual plots
#' primary_plot <- plots$plots$primary_combination
#' ellipse_plot <- plots$plots$color_with_ellipses
#'
#' # View all plots
#' lapply(plots$plots, print)
#' }
#'
#' @seealso
#' \code{\link{process_mea_flexible}} for MEA data processing,
#' \code{\link{discover_mea_structure}} for automatic data structure detection
#'
#' @importFrom dplyr left_join mutate filter select
#' @importFrom ggplot2 ggplot aes aes_string geom_point scale_color_manual scale_shape_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text element_rect element_blank element_line
#' @importFrom ggplot2 coord_fixed guides guide_legend facet_wrap stat_ellipse ggsave unit margin
#' @importFrom stringr str_to_title
#' @importFrom scales alpha
#' @importFrom rlang syms
#' @export
pca_plots_enhanced <- function(pca_output = NULL,
                               plot_data = NULL,
                               pca_result = NULL,
                               output_dir = NULL,
                               processing_result = NULL,
                               experiment_name = NULL,
                               grouping_variables = NULL,
                               color_variable = "Treatment",
                               shape_variable = "Genotype",
                               secondary_shape_variable = "Timepoint",
                               components = c(1, 2),
                               gray_color_value = NULL,
                               save_plots = TRUE,
                               plot_width = 12,
                               plot_height = 10,
                               dpi = 300,
                               verbose = TRUE) {
  
  if (verbose) cat("=== ENHANCED PCA PLOTTING ===\n")
  
  # ============================================================================
  # INPUT VALIDATION AND DEPENDENCIES
  # ============================================================================
  
  # Check required packages
  required_packages <- c("dplyr", "ggplot2", "scales", "rlang", "stringr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "), 
         "\nPlease install with: install.packages(c(", 
         paste(paste0("'", missing_packages, "'"), collapse = ", "), "))")
  }
  
  # Load required libraries
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr package is required")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required")
  if (!requireNamespace("scales", quietly = TRUE)) stop("scales package is required")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang package is required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("stringr package is required")
  
  # Validate input parameters
  if (!is.numeric(components) || length(components) != 2) {
    stop("components must be a numeric vector of length 2")
  }
  
  if (!is.logical(save_plots) || length(save_plots) != 1) {
    stop("save_plots must be a single logical value")
  }
  
  if (!is.numeric(plot_width) || !is.numeric(plot_height) || !is.numeric(dpi)) {
    stop("plot_width, plot_height, and dpi must be numeric")
  }
  
  if (plot_width <= 0 || plot_height <= 0 || dpi <= 0) {
    stop("plot_width, plot_height, and dpi must be positive")
  }
  
  # ============================================================================
  # FLEXIBLE INPUT HANDLING
  # ============================================================================
  
  # Option 1: Use complete pca_output object (from pca_analysis_enhanced)
  if (!is.null(pca_output)) {
    if (verbose) cat("Using complete PCA output object...\n")
    
    if (!is.list(pca_output)) {
      stop("pca_output must be a list object")
    }
    
    if (!"plot_data" %in% names(pca_output) || !"pca_result" %in% names(pca_output)) {
      stop("pca_output must contain 'plot_data' and 'pca_result' elements")
    }
    
    plot_data <- pca_output$plot_data
    pca_result <- pca_output$pca_result
    
    # Extract grouping variables from PCA output if available
    if (is.null(grouping_variables) && !is.null(pca_output$config_used$grouping_variables)) {
      grouping_variables <- pca_output$config_used$grouping_variables
    }
    
    # Extract output directory from processing source info
    if (is.null(output_dir) && !is.null(pca_output$processing_source) && 
        pca_output$processing_source == "processing_result") {
      output_dir <- getwd()
    }
  }
  
  # Option 2: Use processing result (from process_mea_flexible_enhanced)
  else if (!is.null(processing_result)) {
    if (verbose) cat("Extracting PCA data from processing result...\n")
    
    if (!is.list(processing_result)) {
      stop("processing_result must be a list object")
    }
    
    if (is.null(plot_data) || is.null(pca_result)) {
      stop("When using processing_result, you must also run PCA first and provide plot_data and pca_result")
    }
    
    if (is.null(output_dir) && "output_path" %in% names(processing_result)) {
      output_dir <- dirname(processing_result$output_path)
    }
    if (is.null(experiment_name) && "experiment_name" %in% names(processing_result)) {
      experiment_name <- processing_result$experiment_name
    }
    if (is.null(grouping_variables) && "processing_params" %in% names(processing_result) &&
        "grouping_variables" %in% names(processing_result$processing_params)) {
      grouping_variables <- processing_result$processing_params$grouping_variables
    }
  }
  
  # Option 3: Manual specification (backwards compatibility)
  else {
    if (is.null(plot_data) || is.null(pca_result)) {
      stop("Must provide either pca_output, or both plot_data and pca_result")
    }
    if (verbose) cat("Using manually provided plot_data and pca_result...\n")
  }
  
  # Validate plot_data and pca_result
  if (!is.data.frame(plot_data)) {
    stop("plot_data must be a data.frame")
  }
  
  if (nrow(plot_data) == 0) {
    stop("plot_data cannot be empty")
  }
  
  if (!is.list(pca_result) || !"sdev" %in% names(pca_result)) {
    stop("pca_result must be a list with 'sdev' element (e.g., from prcomp())")
  }
  
  # Set defaults
  if (is.null(output_dir)) output_dir <- getwd()
  if (is.null(experiment_name)) experiment_name <- paste0("MEA_PCA_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  if (is.null(grouping_variables)) grouping_variables <- c("Treatment", "Genotype", "Timepoint")
  
  if (verbose) {
    cat("Output directory:", output_dir, "\n")
    cat("Experiment name:", experiment_name, "\n")
    cat("Available grouping variables:", paste(grouping_variables, collapse = ", "), "\n")
    cat("Plot dimensions:", nrow(plot_data), "samples\n")
    if (!is.null(gray_color_value)) {
      cat("Gray color value specified:", gray_color_value, "\n")
    }
  }
  
  # ============================================================================
  # VALIDATE AND PREPARE DATA
  # ============================================================================
  
  # Check which grouping variables are actually available in the data
  available_columns <- names(plot_data)
  valid_grouping_vars <- grouping_variables[grouping_variables %in% available_columns]
  
  if (verbose) {
    cat("Available columns in plot_data:", paste(available_columns, collapse = ", "), "\n")
    cat("Valid grouping variables:", paste(valid_grouping_vars, collapse = ", "), "\n")
  }
  
  # Respect user input for variables, only fall back if they don't exist in data
  if (!color_variable %in% available_columns) {
    if (length(valid_grouping_vars) > 0) {
      old_color_variable <- color_variable
      color_variable <- valid_grouping_vars[1]
      if (verbose) cat("Requested color variable '", old_color_variable, "' not available, using:", color_variable, "\n")
    } else {
      color_variable <- NULL
      if (verbose) cat("No suitable color variable found\n")
    }
  } else {
    if (verbose) cat("Using requested color variable:", color_variable, "\n")
  }
  
  if (!shape_variable %in% available_columns) {
    # Find an alternative that's not the color_variable
    available_alternatives <- valid_grouping_vars[valid_grouping_vars != color_variable]
    if (length(available_alternatives) > 0) {
      old_shape_variable <- shape_variable
      shape_variable <- available_alternatives[1]
      if (verbose) cat("Requested shape variable '", old_shape_variable, "' not available, using:", shape_variable, "\n")
    } else {
      shape_variable <- NULL
      if (verbose) cat("No suitable shape variable found\n")
    }
  } else {
    if (verbose) cat("Using requested shape variable:", shape_variable, "\n")
  }
  
  if (!secondary_shape_variable %in% available_columns) {
    # Find an alternative that's not the color_variable or shape_variable
    available_alternatives <- valid_grouping_vars[!valid_grouping_vars %in% c(color_variable, shape_variable)]
    if (length(available_alternatives) > 0) {
      old_secondary_shape_variable <- secondary_shape_variable
      secondary_shape_variable <- available_alternatives[1]
      if (verbose) cat("Requested secondary shape variable '", old_secondary_shape_variable, "' not available, using:", secondary_shape_variable, "\n")
    } else {
      secondary_shape_variable <- NULL
      if (verbose) cat("No suitable secondary shape variable found\n")
    }
  } else {
    if (verbose) cat("Using requested secondary shape variable:", secondary_shape_variable, "\n")
  }
  
  # Get PC column names and validate they exist
  pc_cols <- paste0("PC", components)
  if (!all(pc_cols %in% available_columns)) {
    missing_pcs <- pc_cols[!pc_cols %in% available_columns]
    stop("Missing PC columns in plot_data: ", paste(missing_pcs, collapse = ", "))
  }
  
  pc1_col <- pc_cols[1]
  pc2_col <- pc_cols[2]
  
  # Validate that PCA components exist in pca_result
  if (max(components) > length(pca_result$sdev)) {
    stop("Requested components exceed available components in pca_result. ",
         "Available: ", length(pca_result$sdev), ", Requested: ", max(components))
  }
  
  # ============================================================================
  # CALCULATE VARIANCE EXPLAINED
  # ============================================================================
  
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  pc1_var <- round(var_explained[components[1]] * 100, 1)
  pc2_var <- round(var_explained[components[2]] * 100, 1)
  
  # ============================================================================
  # CREATE COLOR AND SHAPE PALETTES WITH GRAY OPTION
  # ============================================================================
  
  # Enhanced scientific color palette - carefully curated for publications
  scientific_colors <- c(
    "#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A",  # Strong primary colors
    "#FB9A99", "#A6CEE3", "#B2DF8A", "#FDBF6F", "#CAB2D6",  # Lighter versions
    "#FFFF99", "#B15928", "#FF1493", "#00CED1", "#FFD700",   # Additional colors
    "#8B008B", "#00FFFF", "#32CD32", "#8B4513", "#DC143C"   # More colors
  )
  
  # Create color mapping for primary color variable with gray option
  if (!is.null(color_variable)) {
    unique_color_vals <- sort(unique(plot_data[[color_variable]]))
    unique_color_vals <- unique_color_vals[!is.na(unique_color_vals)]  # Remove NA values
    n_colors <- length(unique_color_vals)
    
    if (n_colors == 0) {
      warning("No valid values found for color variable: ", color_variable)
      color_variable <- NULL
    } else {
      # Validate gray_color_value if provided
      if (!is.null(gray_color_value)) {
        if (!gray_color_value %in% unique_color_vals) {
          warning("Specified gray_color_value '", gray_color_value, "' not found in ", color_variable, 
                  ". Available values: ", paste(unique_color_vals, collapse = ", "))
          gray_color_value <- NULL
        }
      }
      
      # Create color palette
      if (!is.null(gray_color_value)) {
        # Create palette with specified value as gray
        color_palette <- scientific_colors[1:min(n_colors, length(scientific_colors))]
        names(color_palette) <- unique_color_vals
        
        # Set the specified value to gray
        color_palette[gray_color_value] <- "gray50"  # Medium gray
        
        if (verbose) {
          cat("Color mapping (", color_variable, ") with gray option:\n")
          for (i in seq_along(color_palette)) {
            gray_indicator <- if(names(color_palette)[i] == gray_color_value) " (GRAY)" else ""
            cat("  ", names(color_palette)[i], ": ", color_palette[i], gray_indicator, "\n", sep = "")
          }
        }
      } else {
        # Standard color palette without gray
        color_palette <- scientific_colors[1:min(n_colors, length(scientific_colors))]
        names(color_palette) <- unique_color_vals
        
        if (verbose) cat("Color mapping (", color_variable, "):", paste(names(color_palette), collapse = ", "), "\n")
      }
    }
  }
  
  # Create shape mappings
  basic_shapes <- c(16, 17, 15, 18, 19, 8, 0, 1, 2, 5, 6, 3, 4, 7, 9:14, 20:25)
  
  if (!is.null(shape_variable)) {
    unique_shape_vals <- sort(unique(plot_data[[shape_variable]]))
    unique_shape_vals <- unique_shape_vals[!is.na(unique_shape_vals)]  # Remove NA values
    
    if (length(unique_shape_vals) == 0) {
      warning("No valid values found for shape variable: ", shape_variable)
      shape_variable <- NULL
    } else {
      n_shapes <- length(unique_shape_vals)
      shape_palette <- basic_shapes[1:min(n_shapes, length(basic_shapes))]
      names(shape_palette) <- unique_shape_vals
      
      if (verbose) cat("Shape mapping (", shape_variable, "):", paste(names(shape_palette), "=", shape_palette, collapse = ", "), "\n")
    }
  }
  
  # Special handling for timepoint ordering if it's used
  if (!is.null(secondary_shape_variable) && secondary_shape_variable == "Timepoint" &&
      "Timepoint" %in% names(plot_data)) {
    standard_timepoints <- c("baseline", "0min", "15min", "30min", "45min", "1h", "1h15", "1h30", "1h45", "2h", "2h30", "3h")
    present_timepoints <- standard_timepoints[standard_timepoints %in% unique(plot_data$Timepoint)]
    
    if (length(present_timepoints) == 0) {
      present_timepoints <- sort(unique(plot_data$Timepoint))
      present_timepoints <- present_timepoints[!is.na(present_timepoints)]
    }
    
    if (length(present_timepoints) > 0) {
      plot_data$Timepoint <- factor(plot_data$Timepoint, levels = present_timepoints, ordered = TRUE)
      
      # Progressive shapes for timepoints
      progressive_shapes <- c(16, 17, 15, 18, 19, 25, 8, 0, 1, 2, 5, 6)
      timepoint_shape_palette <- progressive_shapes[1:length(present_timepoints)]
      names(timepoint_shape_palette) <- present_timepoints
      
      # Make the last timepoint distinctive
      if (length(timepoint_shape_palette) > 1) {
        timepoint_shape_palette[length(timepoint_shape_palette)] <- 8
      }
    }
  }
  
  # ============================================================================
  # ENHANCED THEME
  # ============================================================================
  
  enhanced_theme <- function() {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        legend.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "grey90", size = 0.3),
        aspect.ratio = 1,
        panel.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.8),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.key = ggplot2::element_rect(fill = "white", color = NA),
        legend.box.background = ggplot2::element_rect(fill = "white", color = "gray80", size = 0.5),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_line(color = "black", size = 0.5),
        axis.ticks.length = ggplot2::unit(0.15, "cm"),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
  }
  
  # ============================================================================
  # PLOT GENERATION
  # ============================================================================
  
  plots_list <- list()
  
  # --- PLOT 1: Color + Shape (Primary combination) ---
  if (!is.null(color_variable) && !is.null(shape_variable)) {
    if (verbose) cat("Creating Plot 1: Color =", color_variable, ", Shape =", shape_variable, "\n")
    
    plot_subtitle <- paste0("Experiment: ", experiment_name, " | Components ", components[1], " & ", components[2])
    if (!is.null(gray_color_value)) {
      plot_subtitle <- paste0(plot_subtitle, " | ", gray_color_value, " in gray")
    }
    
    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = pc1_col, y = pc2_col, 
                                                         color = color_variable, shape = shape_variable)) +
      ggplot2::geom_point(size = 3.5, alpha = 0.8, stroke = 0.5) +
      ggplot2::scale_color_manual(values = color_palette, name = stringr::str_to_title(color_variable)) +
      ggplot2::scale_shape_manual(values = shape_palette, name = stringr::str_to_title(shape_variable)) +
      ggplot2::labs(
        title = paste0("PCA Analysis: ", stringr::str_to_title(color_variable), " × ", stringr::str_to_title(shape_variable)),
        subtitle = plot_subtitle,
        x = paste0("PC", components[1], " (", pc1_var, "% variance)"),
        y = paste0("PC", components[2], " (", pc2_var, "% variance)")
      ) +
      enhanced_theme() +
      ggplot2::coord_fixed() +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5),
        shape = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5)
      ) +
      ggplot2::theme(legend.box = "vertical", legend.position = "right")
    
    plots_list[["primary_combination"]] <- p1
    print(p1)
  }
  
  # --- PLOT 2: Color + Secondary Shape (e.g., Treatment + Timepoint) ---
  if (!is.null(color_variable) && !is.null(secondary_shape_variable)) {
    if (verbose) cat("Creating Plot 2: Color =", color_variable, ", Shape =", secondary_shape_variable, "\n")
    
    # Use appropriate shape palette
    if (secondary_shape_variable == "Timepoint" && exists("timepoint_shape_palette")) {
      sec_shape_palette <- timepoint_shape_palette
    } else {
      unique_sec_vals <- sort(unique(plot_data[[secondary_shape_variable]]))
      unique_sec_vals <- unique_sec_vals[!is.na(unique_sec_vals)]
      sec_shape_palette <- basic_shapes[1:length(unique_sec_vals)]
      names(sec_shape_palette) <- unique_sec_vals
    }
    
    plot_subtitle <- paste0("Experiment: ", experiment_name, " | Components ", components[1], " & ", components[2])
    if (!is.null(gray_color_value)) {
      plot_subtitle <- paste0(plot_subtitle, " | ", gray_color_value, " in gray")
    }
    
    p2 <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = pc1_col, y = pc2_col, 
                                                         color = color_variable, shape = secondary_shape_variable)) +
      ggplot2::geom_point(size = 3.5, alpha = 0.8, stroke = 0.5) +
      ggplot2::scale_color_manual(values = color_palette, name = stringr::str_to_title(color_variable)) +
      ggplot2::scale_shape_manual(values = sec_shape_palette, name = stringr::str_to_title(secondary_shape_variable)) +
      ggplot2::labs(
        title = paste0("PCA Analysis: ", stringr::str_to_title(color_variable), " × ", stringr::str_to_title(secondary_shape_variable)),
        subtitle = plot_subtitle,
        x = paste0("PC", components[1], " (", pc1_var, "% variance)"),
        y = paste0("PC", components[2], " (", pc2_var, "% variance)")
      ) +
      enhanced_theme() +
      ggplot2::coord_fixed() +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5),
        shape = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5)
      ) +
      ggplot2::theme(legend.box = "vertical", legend.position = "right")
    
    plots_list[["secondary_combination"]] <- p2
    print(p2)
  }
  
  # --- PLOT 3: Color Only (Simplified view) ---
  if (!is.null(color_variable)) {
    if (verbose) cat("Creating Plot 3: Color =", color_variable, " only\n")
    
    plot_subtitle <- paste0("Experiment: ", experiment_name, " | Components ", components[1], " & ", components[2])
    if (!is.null(gray_color_value)) {
      plot_subtitle <- paste0(plot_subtitle, " | ", gray_color_value, " in gray")
    }
    
    p3 <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
      ggplot2::geom_point(size = 4, alpha = 0.8) +
      ggplot2::scale_color_manual(values = color_palette, name = stringr::str_to_title(color_variable)) +
      ggplot2::labs(
        title = paste0("PCA Analysis: ", stringr::str_to_title(color_variable)),
        subtitle = plot_subtitle,
        x = paste0("PC", components[1], " (", pc1_var, "% variance)"),
        y = paste0("PC", components[2], " (", pc2_var, "% variance)")
      ) +
      enhanced_theme() +
      ggplot2::coord_fixed() +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5)) +
      ggplot2::theme(legend.position = "right")
    
    plots_list[["color_only"]] <- p3
    print(p3)
  }
  
  # --- PLOT 4: Color with Ellipses ---
  if (!is.null(color_variable)) {
    if (verbose) cat("Creating Plot 4: Color =", color_variable, " with ellipses\n")
    
    plot_subtitle <- paste0("Experiment: ", experiment_name, " | Components ", components[1], " & ", components[2], " | With 95% confidence ellipses")
    if (!is.null(gray_color_value)) {
      plot_subtitle <- paste0(plot_subtitle, " | ", gray_color_value, " in gray")
    }
    
    p4 <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
      ggplot2::stat_ellipse(type = "norm", level = 0.95, size = 1.2, alpha = 0.8) +  # Add confidence ellipses
      ggplot2::geom_point(size = 2, alpha = 0.8) +  # Half the size of original dots (4 -> 2)
      ggplot2::scale_color_manual(values = color_palette, name = stringr::str_to_title(color_variable)) +
      ggplot2::labs(
        title = paste0("PCA Analysis: ", stringr::str_to_title(color_variable), " with Ellipses"),
        subtitle = plot_subtitle,
        x = paste0("PC", components[1], " (", pc1_var, "% variance)"),
        y = paste0("PC", components[2], " (", pc2_var, "% variance)")
      ) +
      enhanced_theme() +
      ggplot2::coord_fixed() +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4), title.position = "top", title.hjust = 0.5)) +
      ggplot2::theme(legend.position = "right")
    
    plots_list[["color_with_ellipses"]] <- p4
    print(p4)
  }
  
  # --- PLOT 5: Faceted by third variable ---
  if (length(valid_grouping_vars) >= 3) {
    third_var <- valid_grouping_vars[3]
    if (verbose) cat("Creating Plot 5: Faceted by", third_var, "\n")
    
    plot_subtitle <- paste0("Experiment: ", experiment_name, " | Components ", components[1], " & ", components[2])
    if (!is.null(gray_color_value)) {
      plot_subtitle <- paste0(plot_subtitle, " | ", gray_color_value, " in gray")
    }
    
    p5 <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
      ggplot2::geom_point(size = 3, alpha = 0.8) +
      ggplot2::scale_color_manual(values = color_palette, name = stringr::str_to_title(color_variable)) +
      ggplot2::facet_wrap(as.formula(paste("~", third_var))) +
      ggplot2::labs(
        title = paste0("PCA Analysis: ", stringr::str_to_title(color_variable), " by ", stringr::str_to_title(third_var)),
        subtitle = plot_subtitle,
        x = paste0("PC", components[1], " (", pc1_var, "% variance)"),
        y = paste0("PC", components[2], " (", pc2_var, "% variance)")
      ) +
      enhanced_theme() +
      ggplot2::coord_fixed() +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4))) +
      ggplot2::theme(
        legend.position = "bottom",
        strip.background = ggplot2::element_rect(fill = "gray90", color = "black"),
        strip.text = ggplot2::element_text(face = "bold")
      )
    
    plots_list[["faceted"]] <- p5
    print(p5)
  }
  
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
  
  if (save_plots) {
    if (verbose) cat("\nSaving plots to:", output_dir, "\n")
    
    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    saved_files <- character()
    
    for (plot_name in names(plots_list)) {
      # Add gray indicator to filename if applicable
      gray_suffix <- if (!is.null(gray_color_value)) paste0("_", gray_color_value, "Gray") else ""
      
      filename <- paste0(experiment_name, "_PCA_", plot_name, "_PC", components[1], "-", components[2], gray_suffix, ".png")
      filepath <- file.path(output_dir, filename)
      
      ggplot2::ggsave(filepath, plot = plots_list[[plot_name]], 
                      width = plot_width, height = plot_height, dpi = dpi)
      
      saved_files <- c(saved_files, filename)
    }
    
    if (verbose) {
      cat("✅ Saved", length(saved_files), "PCA plots:\n")
      for (file in saved_files) {
        cat("  -", file, "\n")
      }
    }
  }
  
  # ============================================================================
  # COMPREHENSIVE SUMMARY
  # ============================================================================
  
  if (verbose) {
    cat("\n", strrep("=", 60), "\n")
    cat("PCA PLOTTING SUMMARY for", experiment_name, "\n")
    cat(strrep("=", 60), "\n")
    
    # PCA summary
    cat("Variance explained by selected components:\n")
    for (i in seq_along(components)) {
      comp_num <- components[i]
      if (comp_num <= length(var_explained)) {
        cat(sprintf("  PC%d: %.2f%%\n", comp_num, var_explained[comp_num] * 100))
      }
    }
    
    cat("\nCumulative variance (PC1-PC", max(components), "): ", 
        round(sum(var_explained[1:max(components)]) * 100, 2), "%\n", sep = "")
    
    # Data summary
    cat("\nData summary:\n")
    cat("  Total samples plotted:", nrow(plot_data), "\n")
    
    for (var in valid_grouping_vars) {
      unique_vals <- unique(plot_data[[var]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      cat("  ", stringr::str_to_title(var), ":", length(unique_vals), "levels -", 
          paste(sort(unique_vals), collapse = ", "), "\n")
    }
    
    cat("\nPlot configuration:\n")
    cat("  Components plotted: PC", components[1], " vs PC", components[2], "\n")
    if (!is.null(color_variable)) cat("  Primary color variable:", color_variable, "\n")
    if (!is.null(shape_variable)) cat("  Primary shape variable:", shape_variable, "\n")
    if (!is.null(secondary_shape_variable)) cat("  Secondary shape variable:", secondary_shape_variable, "\n")
    if (!is.null(gray_color_value)) cat("  Gray color value:", gray_color_value, "\n")
    
    cat(strrep("=", 60), "\n")
  }
  
  # ============================================================================
  # RETURN COMPREHENSIVE RESULTS
  # ============================================================================
  
  return(list(
    plots = plots_list,
    plot_data = plot_data,
    variance_explained = var_explained,
    components_plotted = components,
    color_palette = if(exists("color_palette")) color_palette else NULL,
    shape_palette = if(exists("shape_palette")) shape_palette else NULL,
    plotting_config = list(
      color_variable = color_variable,
      shape_variable = shape_variable,
      secondary_shape_variable = secondary_shape_variable,
      components = components,
      experiment_name = experiment_name,
      output_dir = output_dir,
      valid_grouping_vars = valid_grouping_vars,
      gray_color_value = gray_color_value
    ),
    saved_files = if(save_plots && exists("saved_files")) saved_files else NULL
  ))
}
#' Plot PCA Trajectories for Time Series Data
#'
#' This function creates comprehensive visualizations of PCA trajectories over time,
#' showing both individual and group-averaged trajectories with optional smoothing.
#' It automatically detects grouping variables and timepoint ordering, and generates
#' multiple plot types for detailed trajectory analysis.
#'
#' @param pca_results A data frame or list containing PCA results. If a list, must contain
#'   a 'plot_data' component. If a data frame, will be used directly as plotting data.
#' @param pc_x Character string specifying the principal component for x-axis (default: "PC1").
#' @param pc_y Character string specifying the principal component for y-axis (default: "PC2").
#' @param trajectory_grouping Character vector of column names to use for grouping trajectories.
#'   If NULL, the function will auto-detect suitable grouping variables.
#' @param timepoint_var Character string specifying the column name containing timepoint information
#'   (default: "Timepoint"). Function will attempt smart detection if not found.
#' @param timepoint_order Character vector specifying the order of timepoints. If NULL,
#'   the function will attempt to auto-detect logical ordering (e.g., "baseline", "15min", "1h").
#' @param individual_var Character string specifying the column for individual trajectory identification
#'   (default: "Experiment"). Used for creating separate trajectories per individual.
#' @param point_size Numeric value controlling the size of points in plots (default: 3).
#' @param alpha Numeric value between 0-1 controlling point transparency (default: 0.7).
#' @param line_size Numeric value controlling the thickness of trajectory lines (default: 2).
#' @param smooth_lines Logical indicating whether to apply gentle smoothing to trajectories
#'   (default: FALSE). When TRUE, applies 75% linear + 25% spline smoothing.
#' @param color_palette Character vector of colors for different groups. If NULL,
#'   uses an automatic color palette.
#' @param save_plots Logical indicating whether to automatically save plots to disk (default: TRUE).
#' @param output_dir Character string specifying the directory for saving plots (default: ".").
#' @param plot_prefix Character string prefix for saved plot filenames (default: "PCA_trajectories").
#' @param width Numeric value specifying plot width in inches (default: 12).
#' @param height Numeric value specifying plot height in inches (default: 8).
#' @param dpi Numeric value specifying plot resolution for saved files (default: 150).
#' @param return_list Logical indicating whether to return results as a list (default: TRUE).
#' @param verbose Logical indicating whether to print progress messages (default: TRUE).
#'
#' @return A list containing:
#'   \item{plots}{Named list of ggplot objects for each generated plot}
#'   \item{individual_trajectories}{Data frame with individual trajectory coordinates}
#'   \item{group_average_trajectories}{Data frame with group-averaged trajectory coordinates}
#'   \item{group_summary}{Data frame summarizing the number of trajectories per group}
#'   \item{plotting_params}{List of parameters used for plotting}
#'   \item{data_info}{List with basic information about the dataset}
#'
#' @details
#' The function generates multiple plot types:
#' \itemize{
#'   \item Individual trajectory plots for each group
#'   \item Group-averaged trajectory plots with error bars (SEM)
#'   \item Combined plots showing all trajectories together
#' }
#'
#' The function automatically detects and handles various timepoint naming conventions
#' (e.g., "baseline", "15min", "1h", "2h", etc.) and creates logical ordering.
#' Individual trajectories are identified by extracting well IDs from the experiment variable.
#'
#' @examples
#' \dontrun{
#' # Basic usage with auto-detection
#' results <- plot_pca_trajectories_general(pca_data)
#' 
#' # Specify custom grouping and timepoints
#' results <- plot_pca_trajectories_general(
#'   pca_data,
#'   trajectory_grouping = c("Treatment", "Genotype"),
#'   timepoint_order = c("baseline", "15min", "30min", "1h"),
#'   smooth_lines = TRUE
#' )
#' 
#' # Custom styling and save options
#' results <- plot_pca_trajectories_general(
#'   pca_data,
#'   point_size = 4,
#'   line_size = 1.5,
#'   color_palette = c("#E31A1C", "#1F78B4", "#33A02C"),
#'   output_dir = "trajectory_plots",
#'   plot_prefix = "experiment_PCA"
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise mutate arrange distinct
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_errorbar geom_errorbarh geom_text
#'   aes labs theme_minimal theme element_rect element_line coord_fixed ggsave
#' @importFrom tidyr unnest
#' @importFrom purrr walk map
#' @importFrom rlang syms .data
#' @importFrom stringr str_detect
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 scale_color_viridis_c
#'
#' @export
plot_pca_trajectories_general <- function(pca_results, 
                                          pc_x = "PC1", 
                                          pc_y = "PC2",
                                          trajectory_grouping = NULL,  # Auto-detect or specify grouping vars
                                          timepoint_var = "Timepoint", 
                                          timepoint_order = NULL,      # Specify custom order or auto-detect
                                          individual_var = "Experiment", # Variable for labeling individual trajectories
                                          point_size = 3,
                                          alpha = 0.7,
                                          line_size = 2,
                                          smooth_lines = FALSE,        # Changed from smoothness to match other function
                                          color_palette = NULL,        # Custom colors for groups
                                          save_plots = TRUE,           # NEW: Auto-save plots
                                          output_dir = ".",            # NEW: Output directory
                                          plot_prefix = "PCA_trajectories", # NEW: File prefix
                                          width = 12,                  # NEW: Plot width
                                          height = 8,                  # NEW: Plot height
                                          dpi = 150,                   # NEW: Plot DPI
                                          return_list = TRUE,
                                          verbose = TRUE) {
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(rlang)
  library(stringr)
  library(RColorBrewer)
  library(viridis)
  
  if (verbose) cat("=== GENERALIZED PCA TRAJECTORY PLOTTING ===\n")
  
  # ============================================================================
  # FLEXIBLE DATA EXTRACTION
  # ============================================================================
  
  # Extract plot_data from various possible PCA result structures
  plot_data <- NULL
  
  if (is.list(pca_results) && "plot_data" %in% names(pca_results)) {
    plot_data <- pca_results$plot_data
    if (verbose) cat("Found plot_data in PCA results\n")
  } else if (is.data.frame(pca_results)) {
    plot_data <- pca_results
    if (verbose) cat("Using PCA results directly as data frame\n")
  } else {
    stop("Cannot extract plottable data from pca_results. Expected either data.frame or list with 'plot_data' component.")
  }
  
  if (is.null(plot_data)) stop("No plottable data found in pca_results")
  
  # ============================================================================
  # COLUMN VALIDATION AND DETECTION
  # ============================================================================
  
  available_cols <- names(plot_data)
  
  # Validate PC columns
  if (!pc_x %in% available_cols || !pc_y %in% available_cols) {
    stop("Principal components '", pc_x, "' and/or '", pc_y, "' not found in data. Available PC columns: ", 
         paste(available_cols[grepl("^PC", available_cols)], collapse = ", "))
  }
  
  # Smart timepoint detection
  timepoint_candidates <- c(timepoint_var, "Timepoint", "Time", "timepoint", "time", "Time_point")
  timepoint_var <- timepoint_candidates[timepoint_candidates %in% available_cols][1]
  
  if (is.na(timepoint_var) || is.null(timepoint_var)) {
    stop("No timepoint variable found. Available columns: ", paste(available_cols, collapse = ", "))
  }
  
  # Smart experiment variable detection
  experiment_candidates <- c(individual_var, "Experiment", "experiment", "Exp", "exp", "ID", "Sample")
  individual_var <- experiment_candidates[experiment_candidates %in% available_cols][1]
  
  if (is.na(individual_var) || is.null(individual_var)) {
    warning("No experiment variable found for labeling. Will use row numbers.")
    plot_data$Experiment <- paste0("Traj", seq_len(nrow(plot_data)))
    individual_var <- "Experiment"
  }
  
  if (verbose) {
    cat("Using timepoint variable:", timepoint_var, "\n")
    cat("Variable for individual trajectories:", individual_var, "\n")
  }
  
  # ============================================================================
  # AUTO-DETECT OR VALIDATE TRAJECTORY GROUPING VARIABLES
  # ============================================================================
  
  if (is.null(trajectory_grouping)) {
    # Smart detection of grouping variables
    grouping_candidates <- c("Treatment", "Genotype", "Condition", "Group", 
                             "treatment", "genotype", "condition", "group")
    
    # Find available grouping variables
    available_grouping <- grouping_candidates[grouping_candidates %in% available_cols]
    
    # Remove timepoint and experiment from grouping if present
    available_grouping <- setdiff(available_grouping, c(timepoint_var, individual_var))
    
    # Remove PC columns from grouping
    available_grouping <- available_grouping[!grepl("^PC\\d+", available_grouping)]
    
    if (length(available_grouping) == 0) {
      stop("No suitable grouping variables detected. Available columns: ", 
           paste(available_cols, collapse = ", "), 
           "\nPlease specify 'trajectory_grouping' manually.")
    }
    
    trajectory_grouping <- available_grouping[1:min(2, length(available_grouping))]
    
    if (verbose) {
      cat("Auto-detected trajectory grouping variables:", paste(trajectory_grouping, collapse = ", "), "\n")
    }
  } else {
    # Validate user-specified trajectory grouping variables
    missing_vars <- setdiff(trajectory_grouping, available_cols)
    if (length(missing_vars) > 0) {
      stop("Trajectory grouping variables not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }
  
  # ============================================================================
  # TIMEPOINT ORDERING
  # ============================================================================
  
  unique_timepoints <- unique(plot_data[[timepoint_var]])
  
  if (is.null(timepoint_order)) {
    # Smart timepoint ordering - matching the other function's approach
    timepoint_order <- tryCatch({
      # Try common biological timepoint patterns
      baseline_patterns <- c("baseline", "Baseline", "BL", "bl", "0", "pre", "Pre")
      minute_patterns <- c("0min", "15min", "30min", "45min", "60min")
      hour_patterns <- c("1h", "1h30min", "2h", "3h", "4h", "6h", "8h", "12h", "24h")
      day_patterns <- c("1d", "2d", "3d", "7d", "14d", "21d", "28d")
      week_patterns <- c("1w", "2w", "3w", "4w")
      
      all_patterns <- c(baseline_patterns, minute_patterns, hour_patterns, day_patterns, week_patterns)
      
      # Find which timepoints match known patterns
      matched_timepoints <- intersect(all_patterns, unique_timepoints)
      unmatched_timepoints <- setdiff(unique_timepoints, matched_timepoints)
      
      if (length(matched_timepoints) > 0) {
        ordered_matched <- all_patterns[all_patterns %in% matched_timepoints]
        
        if (length(unmatched_timepoints) > 0) {
          # Try numeric sorting for unmatched
          numeric_attempt <- suppressWarnings(as.numeric(unmatched_timepoints))
          if (!any(is.na(numeric_attempt))) {
            sorted_unmatched <- unmatched_timepoints[order(numeric_attempt)]
          } else {
            sorted_unmatched <- sort(unmatched_timepoints)
          }
          c(ordered_matched, sorted_unmatched)
        } else {
          ordered_matched
        }
      } else {
        # Try pure numeric sorting
        numeric_attempt <- suppressWarnings(as.numeric(unique_timepoints))
        if (!any(is.na(numeric_attempt))) {
          unique_timepoints[order(numeric_attempt)]
        } else {
          sort(unique_timepoints)
        }
      }
    }, error = function(e) {
      sort(unique_timepoints)
    })
    
    if (verbose) cat("Auto-detected timepoint order:", paste(timepoint_order, collapse = " → "), "\n")
  }
  
  # Set timepoint as ordered factor
  plot_data[[timepoint_var]] <- factor(plot_data[[timepoint_var]], levels = timepoint_order)
  
  # ============================================================================
  # CREATE GROUP COMBINATIONS AND CALCULATE TRAJECTORIES
  # ============================================================================
  
  # Create group identifier
  plot_data$group_id <- do.call(paste, c(plot_data[trajectory_grouping], sep = "_"))
  
  # Add time rank for interpolation
  plot_data$time_rank <- as.integer(plot_data[[timepoint_var]])
  
  # Remove rows with missing data
  plot_data_clean <- plot_data %>%
    filter(!is.na(time_rank), 
           !is.na(.data[[pc_x]]), 
           !is.na(.data[[pc_y]]),
           !is.na(group_id))
  
  # Extract well/plate identifier from experiment variable to create true trajectories
  # Pattern: extract  ID (e.g., "A4" from "A4_DIV11_cgasKO")
  plot_data_clean$well_id <- sub("_.*", "", plot_data_clean[[individual_var]])
  
  # Calculate individual trajectory summaries - CREATE PROPER TRAJECTORIES
  # Group by ID AND timepoint to get one point per timepoint
  individual_trajectories <- plot_data_clean %>%
    group_by(group_id, !!!syms(trajectory_grouping), well_id, .data[[timepoint_var]], time_rank) %>%
    summarise(
      mean_x = mean(.data[[pc_x]], na.rm = TRUE),
      mean_y = mean(.data[[pc_y]], na.rm = TRUE),
      n_obs = n(),
      .groups = 'drop'
    )
  
  # Count actual trajectories per group
  well_trajectory_counts <- plot_data_clean %>%
    group_by(group_id, !!!syms(trajectory_grouping)) %>%
    summarise(
      n_wells = n_distinct(well_id),
      n_timepoints_per_well = round(n() / n_distinct(well_id), 1),
      wells = paste(sort(unique(well_id)), collapse = ", "),
      .groups = 'drop'
    )
  
  # Calculate group average trajectories (mean across all per group per timepoint)
  group_average_trajectories <- plot_data_clean %>%
    group_by(group_id, !!!syms(trajectory_grouping), .data[[timepoint_var]], time_rank) %>%
    summarise(
      avg_x = mean(.data[[pc_x]], na.rm = TRUE),
      avg_y = mean(.data[[pc_y]], na.rm = TRUE),
      n_wells = n_distinct(well_id),
      se_x = sd(.data[[pc_x]], na.rm = TRUE) / sqrt(n()),
      se_y = sd(.data[[pc_y]], na.rm = TRUE) / sqrt(n()),
      sd_x = sd(.data[[pc_x]], na.rm = TRUE),
      sd_y = sd(.data[[pc_y]], na.rm = TRUE),
      .groups = 'drop'
    )
  
  if (verbose) {
    cat("\n=== GROUP TRAJECTORY SUMMARY ===\n")
    for (i in seq_len(nrow(well_trajectory_counts))) {
      row <- well_trajectory_counts[i, ]
      cat("Group:", row$group_id, "\n")
      cat("  - Number of individual trajectories:", row$n_wells, "\n")
    }
  }
  
  # ============================================================================
  # COLOR SETUP - Matching the other function's approach
  # ============================================================================
  
  unique_groups <- unique(plot_data_clean$group_id)
  n_groups <- length(unique_groups)
  
  generate_colors <- function(n) {
    if (n <= 1) return("#E31A1C")
    # Create a gradient from red to blue with intermediate colors
    gradient_colors <- c("#E31A1C", "#FF7F00", "#FDBF6F", "#33A02C", "#1F78B4", "#6A3D9A", "#B15928", "#FB9A99", "#A6CEE3", "#B2DF8A")
    if (n <= length(gradient_colors)) {
      return(gradient_colors[1:n])
    } else {
      # If more colors needed, interpolate
      colorRampPalette(gradient_colors)(n)
    }
  }
  
  if (is.null(color_palette)) {
    color_palette <- generate_colors(n_groups)
  } else if (length(color_palette) < n_groups) {
    warning("Not enough colors provided, extending palette")
    color_palette <- rep(color_palette, ceiling(n_groups / length(color_palette)))[1:n_groups]
  }
  
  names(color_palette) <- unique_groups
  
  # ============================================================================
  # GENTLE SMOOTHING HELPER FUNCTIONS - MODIFIED FOR SUBTLE CURVES
  # ============================================================================
  
  create_gradient_segments <- function(data, group_var, smooth = FALSE, pts = 100) {
    grad_list <- list()
    
    # ALWAYS create individual well trajectories
    if ("well_id" %in% names(data)) {
      # Get unique combinations for individual trajectories
      combos <- distinct(data, group_id, well_id)
      for (i in seq_len(nrow(combos))) {
        tv <- data %>% 
          filter(group_id == combos$group_id[i], well_id == combos$well_id[i]) %>% 
          filter(!is.na(mean_x) & !is.na(mean_y)) %>%
          arrange(time_rank)
        
        if (nrow(tv) < 2) next
        
        tryCatch({
          if (smooth) {
            # GENTLE SMOOTHING: Use lower smoothing parameter and fewer interpolation points
            # Method 1: Use approx with more conservative interpolation
            n_interp_pts <- min(pts, nrow(tv) * 8)  # Reduce interpolation points
            
            # Create a gentle interpolation that stays closer to original path
            # Use a weighted approach: 70% linear interpolation + 30% spline smoothing
            
            # Linear interpolation (sharp)
            xi_linear <- approx(tv$time_rank, tv$mean_x, n = n_interp_pts)$y
            yi_linear <- approx(tv$time_rank, tv$mean_y, n = n_interp_pts)$y
            
            # Gentle spline (use low smoothing parameter)
            # Use smooth.spline with low spar value for minimal smoothing
            tryCatch({
              smooth_x <- smooth.spline(tv$time_rank, tv$mean_x, spar = 0.1)  # Low smoothing
              smooth_y <- smooth.spline(tv$time_rank, tv$mean_y, spar = 0.1)
              
              time_seq <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
              xi_smooth <- predict(smooth_x, time_seq)$y
              yi_smooth <- predict(smooth_y, time_seq)$y
              
              # Blend: 75% linear + 25% smooth for very gentle curves
              xi <- 0.75 * xi_linear + 0.25 * xi_smooth
              yi <- 0.75 * yi_linear + 0.25 * yi_smooth
              
            }, error = function(e) {
              # Fallback to pure linear if smooth.spline fails
              xi <- xi_linear
              yi <- yi_linear
            })
            
            ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
            
          } else {
            # Regular linear interpolation
            xi <- approx(tv$time_rank, tv$mean_x, n = pts)$y
            yi <- approx(tv$time_rank, tv$mean_y, n = pts)$y
            ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = pts)
          }
          
          n_final_pts <- length(xi)
          grad_list[[length(grad_list) + 1]] <- data.frame(
            x = xi[-n_final_pts], y = yi[-n_final_pts], 
            xend = xi[-1], yend = yi[-1],
            tfrac = (ti[-n_final_pts] - min(ti)) / (max(ti) - min(ti)),
            group_id = tv$group_id[1],
            well_id = tv$well_id[1]  # Keep track of well ID
          )
        }, error = function(e) {
          if (verbose) cat("Warning: Could not create gradient for", combos$group_id[i], combos$well_id[i], "\n")
        })
      }
    } else {
      # For averaged data (group averages only)
      unique_groups <- unique(data$group_id)
      for (group in unique_groups) {
        tv <- data %>% 
          filter(group_id == group) %>%
          filter(!is.na(avg_x) & !is.na(avg_y)) %>%
          arrange(time_rank)
        
        if (nrow(tv) < 2) next
        
        tryCatch({
          if (smooth) {
            # GENTLE SMOOTHING for group averages
            n_interp_pts <- min(pts, nrow(tv) * 8)
            
            # Linear interpolation
            xi_linear <- approx(tv$time_rank, tv$avg_x, n = n_interp_pts)$y
            yi_linear <- approx(tv$time_rank, tv$avg_y, n = n_interp_pts)$y
            
            # Gentle smoothing
            tryCatch({
              smooth_x <- smooth.spline(tv$time_rank, tv$avg_x, spar = 0.1)
              smooth_y <- smooth.spline(tv$time_rank, tv$avg_y, spar = 0.1)
              
              time_seq <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
              xi_smooth <- predict(smooth_x, time_seq)$y
              yi_smooth <- predict(smooth_y, time_seq)$y
              
              # Blend: 75% linear + 25% smooth
              xi <- 0.75 * xi_linear + 0.25 * xi_smooth
              yi <- 0.75 * yi_linear + 0.25 * yi_smooth
              
            }, error = function(e) {
              xi <- xi_linear
              yi <- yi_linear
            })
            
            ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
            
          } else {
            xi <- approx(tv$time_rank, tv$avg_x, n = pts)$y
            yi <- approx(tv$time_rank, tv$avg_y, n = pts)$y
            ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = pts)
          }
          
          n_final_pts <- length(xi)
          grad_list[[length(grad_list) + 1]] <- data.frame(
            x = xi[-n_final_pts], y = yi[-n_final_pts], 
            xend = xi[-1], yend = yi[-1],
            tfrac = (ti[-n_final_pts] - min(ti)) / (max(ti) - min(ti)),
            group_id = group
          )
        }, error = function(e) {
          if (verbose) cat("Warning: Could not create gradient for group", group, "\n")
          return(data.frame())
        })
      }
    }
    
    if (length(grad_list) == 0) {
      return(data.frame())
    }
    
    bind_rows(grad_list)
  }
  
  # ============================================================================
  # PLOT GENERATION FUNCTIONS - Matching other function's style
  # ============================================================================
  
  create_individual_trajectories_plot <- function(group_data, group_name) {
    # Create gradient segments for EACH INDIVIDUAL ID - FIXED
    grad_df <- create_gradient_segments(group_data, "group_id", smooth_lines)
    
    # Get unique wells in this group for color variation
    unique_wells <- unique(group_data$well_id)
    n_wells <- length(unique_wells)
    
    # Create a color palette for wells (subtle variations of the same hue)
    well_colors <- colorRampPalette(c("#1f78b4", "#a6cee3"))(n_wells)
    names(well_colors) <- unique_wells
    
    # Label only 3 key timepoints per trajectory: first, middle, last
    all_timepoints <- unique(as.character(group_data[[timepoint_var]]))
    n_tp <- length(all_timepoints)
    if (n_tp >= 3) {
      available_labels <- c(all_timepoints[1], all_timepoints[ceiling(n_tp/2)], all_timepoints[n_tp])
    } else {
      available_labels <- all_timepoints
    }
    
    # Only label every 2nd or 3rd well if there are many
    wells_to_label <- if (n_wells <= 3) {
      unique_wells
    } else if (n_wells <= 8) {
      unique_wells[seq(1, n_wells, by = 2)]
    } else {
      unique_wells[seq(1, n_wells, by = 3)]
    }
    
    label_df <- group_data %>% 
      filter(.data[[timepoint_var]] %in% available_labels,
             well_id %in% wells_to_label) %>%
      mutate(
        label_text = case_when(
          .data[[timepoint_var]] == "baseline" ~ "B",
          TRUE ~ as.character(.data[[timepoint_var]])
        )
      )
    
    p <- ggplot() +
      # Individual trajectory lines - FIXED 
      {if(nrow(grad_df) > 0 && "well_id" %in% names(grad_df)) {
        geom_segment(data = grad_df, aes(x, y, xend = xend, yend = yend, group = well_id, color = tfrac), 
                     size = line_size * 0.4, alpha = 0.6)
      } else if(nrow(grad_df) > 0) {
        geom_segment(data = grad_df, aes(x, y, xend = xend, yend = yend, color = tfrac), 
                     size = line_size * 0.4, alpha = 0.6)
      }} +
      scale_color_viridis_c(guide = 'none') +
      # Points for each well - colored by well
      geom_point(data = group_data, aes(x = mean_x, y = mean_y, fill = well_id), 
                 shape = 21, size = point_size * 0.6, alpha = alpha * 0.8, 
                 color = 'black', stroke = 0.2) +
      scale_fill_manual(values = well_colors, name = "Well", guide = "none") +  # Hide legend for cleaner look
      # Labels only for selected wells and timepoints
      geom_text(data = label_df, aes(x = mean_x, y = mean_y, label = label_text), 
                nudge_x = 0.02, nudge_y = 0.02, size = point_size * 0.7, fontface = 'bold') +
      labs(title = paste('Individual Trajectories - Group:', group_name, '(', n_wells,')'), x = pc_x, y = pc_y) +
      coord_fixed() +
      theme_minimal() +
      theme(
        aspect.ratio = 1,
        panel.border = element_rect(color = 'black', fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray85")
      )
    
    return(p)
  }
  
  create_group_average_plot <- function(group_data, group_name, group_color) {
    # Create gradient segments for average
    grad_avg <- create_gradient_segments(group_data, "group_id", smooth_lines)
    
    # Label only 3 key timepoints per trajectory: first, middle, last
    all_timepoints <- unique(as.character(group_data[[timepoint_var]]))
    n_tp <- length(all_timepoints)
    if (n_tp >= 3) {
      available_labels <- c(all_timepoints[1], all_timepoints[ceiling(n_tp/2)], all_timepoints[n_tp])
    } else {
      available_labels <- all_timepoints
    }
    
    label_df <- group_data %>% 
      filter(.data[[timepoint_var]] %in% available_labels) %>%
      mutate(
        label_text = case_when(
          .data[[timepoint_var]] == "baseline" ~ "B",
          TRUE ~ as.character(.data[[timepoint_var]])
        )
      )
    
    p <- ggplot() +
      # Main average trajectory (removed individual background trajectories)
      {if(nrow(grad_avg) > 0) geom_segment(data = grad_avg, aes(x, y, xend = xend, yend = yend, color = tfrac), 
                                           size = line_size)} +
      scale_color_viridis_c(guide = 'none') +
      # Points for labeled timepoints
      geom_point(data = label_df, aes(x = avg_x, y = avg_y), 
                 shape = 21, fill = 'white', size = point_size) +
      # Error bars - using SEM (standard error of mean)
      geom_errorbar(data = group_data, aes(x = avg_x, ymin = avg_y - se_y, ymax = avg_y + se_y), 
                    width = 0.08, color = "gray60", alpha = 0.6, size = 0.5) +
      geom_errorbarh(data = group_data, aes(y = avg_y, xmin = avg_x - se_x, xmax = avg_x + se_x), 
                     height = 0.08, color = "gray60", alpha = 0.6, size = 0.5) +
      # Labels
      geom_text(data = label_df, aes(x = avg_x, y = avg_y, label = label_text), 
                nudge_x = 0.02, nudge_y = 0.02, size = point_size * 0.9, fontface = 'bold') +
      labs(title = paste('Avg Trajectory ± SEM - Group:', group_name), x = pc_x, y = pc_y) +
      coord_fixed() +
      theme_minimal() +
      theme(
        aspect.ratio = 1,
        panel.border = element_rect(color = 'black', fill = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray85")
      )
    
    return(p)
  }
  
  # ============================================================================
  # GENERATE PLOTS FOR EACH GROUP
  # ============================================================================
  
  plot_list <- list()
  
  for (group in unique_groups) {
    group_data_ind <- individual_trajectories %>% filter(group_id == group)
    group_data_avg <- group_average_trajectories %>% filter(group_id == group)
    
    if (nrow(group_data_ind) > 0) {
      # Individual trajectories plot
      plot_list[[paste0(group, "_all")]] <- create_individual_trajectories_plot(group_data_ind, group)
      
      # Group average plot
      plot_list[[paste0(group, "_avg")]] <- create_group_average_plot(group_data_avg, group, color_palette[group])
    }
  }
  
  # ============================================================================
  # CREATE COMBINED PLOTS - WITH MODIFIED END POINTS AND GENTLE SMOOTHING
  # ============================================================================
  
  # Combined individual trajectories
  pts <- 100
  grad_list_combined <- list()
  
  for (group in unique_groups) {
    group_data <- individual_trajectories %>% filter(group_id == group)
    combos <- distinct(group_data, well_id)
    
    for (i in seq_len(nrow(combos))) {
      tv <- group_data %>% 
        filter(well_id == combos$well_id[i]) %>% 
        filter(!is.na(mean_x) & !is.na(mean_y)) %>%
        arrange(time_rank)
      
      if (nrow(tv) < 2) next
      
      tryCatch({
        if (smooth_lines) {
          # GENTLE SMOOTHING for combined plot
          n_interp_pts <- min(pts, nrow(tv) * 8)
          
          # Linear interpolation
          xi_linear <- approx(tv$time_rank, tv$mean_x, n = n_interp_pts)$y
          yi_linear <- approx(tv$time_rank, tv$mean_y, n = n_interp_pts)$y
          
          # Gentle smoothing
          tryCatch({
            smooth_x <- smooth.spline(tv$time_rank, tv$mean_x, spar = 0.1)
            smooth_y <- smooth.spline(tv$time_rank, tv$mean_y, spar = 0.1)
            
            time_seq <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
            xi_smooth <- predict(smooth_x, time_seq)$y
            yi_smooth <- predict(smooth_y, time_seq)$y
            
            # Blend: 75% linear + 25% smooth
            xi <- 0.75 * xi_linear + 0.25 * xi_smooth
            yi <- 0.75 * yi_linear + 0.25 * yi_smooth
            
          }, error = function(e) {
            xi <- xi_linear
            yi <- yi_linear
          })
          
          ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = n_interp_pts)
          
        } else {
          xi <- approx(tv$time_rank, tv$mean_x, n = pts)$y
          yi <- approx(tv$time_rank, tv$mean_y, n = pts)$y
          ti <- seq(min(tv$time_rank), max(tv$time_rank), length.out = pts)
        }
        
        n_final_pts <- length(xi)
        grad_list_combined[[length(grad_list_combined) + 1]] <- data.frame(
          x = xi[-n_final_pts], y = yi[-n_final_pts], 
          xend = xi[-1], yend = yi[-1],
          tfrac = (ti[-n_final_pts] - min(ti)) / (max(ti) - min(ti)),
          group_id = tv$group_id[1]
        )
      }, error = function(e) {
        if (verbose) cat("Warning: Could not create gradient for", group, combos$well_id[i], "\n")
      })
    }
  }
  grad_df_combined <- bind_rows(grad_list_combined)
  
  # Combined individual plot - THINNER LINES for more trajectories
  p_combined <- ggplot() +
    geom_segment(data = grad_df_combined, aes(x, y, xend = xend, yend = yend, color = group_id),
                 size = line_size * 0.3, alpha = alpha * 0.6) +  # MUCH THINNER LINES
    geom_point(data = individual_trajectories, aes(x = mean_x, y = mean_y, color = group_id),
               size = point_size * 0.2, alpha = 0.5) +  # MUCH SMALLER POINTS
    scale_color_manual(values = color_palette, name = "Group") +
    labs(title = "Combined Individual Trajectories", x = pc_x, y = pc_y) +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray85"),
      panel.border = element_rect(color = "black", fill = NA),
      aspect.ratio = 1,
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
  
  # Get first and last points for combined_all plot with colored borders
  first_last_combined <- individual_trajectories %>%
    group_by(group_id, well_id) %>%
    arrange(time_rank) %>%
    summarise(
      first_x = first(mean_x),
      first_y = first(mean_y),
      last_x = last(mean_x),
      last_y = last(mean_y),
      .groups = "drop"
    )
  
  # Add start and end points to combined plot - MODIFIED for individual wells
  p_combined <- p_combined +
    # Start points: white with colored border (smaller for many trajectories)
    geom_point(data = first_last_combined, aes(x = first_x, y = first_y, color = group_id),
               fill = "white", shape = 21, size = point_size * 0.5, stroke = 0.8) +
    # End points: black fill with colored border (smaller for many trajectories)
    geom_point(data = first_last_combined, aes(x = last_x, y = last_y, color = group_id),
               fill = "black", shape = 21, size = point_size * 0.5, stroke = 0.8)
  
  # Combined average trajectories
  grad_avg_combined_list <- list()
  for (group in unique_groups) {
    group_data <- group_average_trajectories %>% filter(group_id == group) %>% arrange(time_rank)
    
    if (nrow(group_data) < 2) next
    
    # Remove any rows with NA values
    group_data <- group_data %>% filter(!is.na(avg_x) & !is.na(avg_y))
    
    if (nrow(group_data) < 2) next
    
    if (smooth_lines) {
      # GENTLE SMOOTHING for combined average trajectories
      n_interp_pts <- min(pts, nrow(group_data) * 8)
      
      # Linear interpolation
      xi_linear <- approx(group_data$time_rank, group_data$avg_x, n = n_interp_pts)$y
      yi_linear <- approx(group_data$time_rank, group_data$avg_y, n = n_interp_pts)$y
      
      # Gentle smoothing
      tryCatch({
        smooth_x <- smooth.spline(group_data$time_rank, group_data$avg_x, spar = 0.1)
        smooth_y <- smooth.spline(group_data$time_rank, group_data$avg_y, spar = 0.1)
        
        time_seq <- seq(min(group_data$time_rank), max(group_data$time_rank), length.out = n_interp_pts)
        xi_smooth <- predict(smooth_x, time_seq)$y
        yi_smooth <- predict(smooth_y, time_seq)$y
        
        # Blend: 75% linear + 25% smooth
        xi <- 0.75 * xi_linear + 0.25 * xi_smooth
        yi <- 0.75 * yi_linear + 0.25 * yi_smooth
        
      }, error = function(e) {
        xi <- xi_linear
        yi <- yi_linear
      })
      
      ti <- seq(min(group_data$time_rank), max(group_data$time_rank), length.out = n_interp_pts)
      
    } else {
      xi <- approx(group_data$time_rank, group_data$avg_x, n = pts)$y
      yi <- approx(group_data$time_rank, group_data$avg_y, n = pts)$y
      ti <- seq(min(group_data$time_rank), max(group_data$time_rank), length.out = pts)
    }
    
    n_final_pts <- length(xi)
    grad_avg_combined_list[[group]] <- data.frame(
      x = xi[-n_final_pts], y = yi[-n_final_pts], 
      xend = xi[-1], yend = yi[-1],
      tfrac = (ti[-n_final_pts] - min(ti)) / (max(ti) - min(ti)),
      group_id = group
    )
  }
  
  grad_avg_combined <- bind_rows(grad_avg_combined_list)
  
  # Get first and last points for each group
  first_last_points <- group_average_trajectories %>%
    group_by(group_id) %>%
    arrange(time_rank) %>%
    summarise(
      first_x = first(avg_x),
      first_y = first(avg_y),
      first_label = "B",  # Always use "B" for baseline/start point
      last_x = last(avg_x),
      last_y = last(avg_y),
      last_label = last(as.character(.data[[timepoint_var]])),
      .groups = "drop"
    )
  
  # Professional color palette for combined average plot
  professional_colors <- c(
    "#E31A1C",  # Red
    "#FF7F00",  # Orange
    "#FDBF6F",  # Light Orange
    "#33A02C",  # Green
    "#1F78B4",  # Blue
    "#6A3D9A",  # Purple
    "#B15928",  # Brown
    "#FB9A99",  # Light Pink
    "#A6CEE3",  # Light Blue
    "#B2DF8A"   # Light Green
  )
  
  
  
  # Extend palette if needed
  if (n_groups > length(professional_colors)) {
    professional_colors <- colorRampPalette(professional_colors)(n_groups)
  }
  names(professional_colors) <- unique_groups
  
  # Combined average plot - MODIFIED WITH COLORED BORDERS FOR BLACK POINTS
  p_comb_avg <- ggplot() +
    # Trajectory lines
    geom_segment(data = grad_avg_combined, aes(x, y, xend = xend, yend = yend, color = group_id),
                 size = line_size, alpha = 1) +
    # All timepoint dots (small, colored)
    geom_point(data = group_average_trajectories, aes(x = avg_x, y = avg_y, color = group_id),
               size = point_size * 0.5, alpha = 0.8) +
    # Error bars - using SEM (standard error of mean) instead of SD
    geom_errorbar(data = group_average_trajectories, aes(x = avg_x, ymin = avg_y - se_y, ymax = avg_y + se_y, color = group_id),
                  width = 0.05, alpha = 0.5, size = 0.4) +
    geom_errorbarh(data = group_average_trajectories, aes(y = avg_y, xmin = avg_x - se_x, xmax = avg_x + se_x, color = group_id),
                   height = 0.05, alpha = 0.5, size = 0.4) +
    # First point: white-filled with colored border (larger)
    geom_point(data = first_last_points, aes(x = first_x, y = first_y, color = group_id),
               fill = "white", shape = 21, size = point_size * 1.2, stroke = 1.2) +
    # Last point: black-filled with colored border (larger) - MODIFIED
    geom_point(data = first_last_points, aes(x = last_x, y = last_y, color = group_id),
               fill = "black", shape = 21, size = point_size * 1.2, stroke = 1.2) +
    # Offset text labels (B for start, actual timepoint for end)
    geom_text(data = first_last_points, aes(x = last_x, y = last_y, label = last_label),
              nudge_x = 0.08, nudge_y = 0.08, fontface = "bold", size = point_size * 1.1, color = "black") +
    geom_text(data = first_last_points, aes(x = first_x, y = first_y, label = first_label),
              nudge_x = -0.08, nudge_y = -0.08, fontface = "bold", size = point_size * 1.1, color = "black") +
    scale_color_manual(values = professional_colors, name = "Group") +
    labs(title = "Averaged PCA Trajectories", x = pc_x, y = pc_y) +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray85"),
      panel.border = element_rect(color = "black", fill = NA),
      aspect.ratio = 1,
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
  
  plot_list$combined_all <- p_combined
  plot_list$combined_avg <- p_comb_avg
  
  # ============================================================================
  # SAVE PLOTS AUTOMATICALLY
  # ============================================================================
  
  if (save_plots) {
    if (verbose) cat("\n=== SAVING PLOTS ===\n")
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      if (verbose) cat("Created output directory:", output_dir, "\n")
    }
    
    # Save each plot with appropriate filename
    for (plot_name in names(plot_list)) {
      filename <- file.path(output_dir, paste0(plot_prefix, "_", plot_name, ".png"))
      
      tryCatch({
        ggsave(filename = filename, 
               plot = plot_list[[plot_name]], 
               width = width, 
               height = height, 
               dpi = dpi,
               bg = "white")
        if (verbose) cat("Saved:", filename, "\n")
      }, error = function(e) {
        warning("Failed to save ", filename, ": ", e$message)
      })
    }
  }
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  if (verbose) {
    cat("\n=== PLOTTING COMPLETED ===\n")
    cat("Generated plots:", length(plot_list), "\n")
    cat("Plot names:", paste(names(plot_list), collapse = ", "), "\n")
    if (smooth_lines) {
      cat("Smoothing: Enabled (gentle curves - 75% linear + 25% smooth)\n")
    } else {
      cat("Smoothing: Disabled (direct line connections)\n")
    }
    if (save_plots) {
      cat("Plots saved to:", output_dir, "\n")
    }
  }
  
  results <- list(
    plots = plot_list,
    individual_trajectories = individual_trajectories,
    group_average_trajectories = group_average_trajectories,
    group_summary = well_trajectory_counts,
    plotting_params = list(
      pc_x = pc_x, pc_y = pc_y,
      trajectory_grouping = trajectory_grouping,
      timepoint_var = timepoint_var,
      timepoint_order = timepoint_order,
      individual_var = individual_var,
      smooth_lines = smooth_lines,
      save_plots = save_plots,
      output_dir = output_dir
    ),
    data_info = list(
      n_groups = length(unique_groups),
      n_timepoints = length(timepoint_order),
      total_observations = nrow(plot_data_clean)
    )
  )
  
  if (return_list) {
    # Display plots automatically when function is called
    if (verbose) cat("\n=== DISPLAYING PLOTS ===\n")
    for (plot_name in names(plot_list)) {
      cat("Displaying:", plot_name, "\n")
      print(plot_list[[plot_name]])
    }
    return(results)
  } else {
    walk(plot_list, print)
    invisible(results)
  }
}

#' Create Enhanced Heatmaps for Multi-Electrode Array (MEA) Data Analysis
#'
#' This function generates comprehensive heatmap visualizations for MEA data analysis,
#' including individual grouping variable heatmaps, combined interaction heatmaps, and
#' variable correlation matrices. It provides flexible scaling, clustering, and
#' customization options with automatic quality filtering and missing data handling.
#'
#' @param data A data frame containing MEA measurement data. If NULL, must provide processing_result.
#' @param processing_result A list object from MEA data processing containing normalized_data or raw_data components.
#'   Takes precedence over the data parameter if provided.
#' @param config Configuration list from MEA processing. If NULL and processing_result is provided,
#'   will attempt to use config from processing_result$config_used.
#' @param value_column Character string specifying the column containing measurement values (default: "Normalized_Value").
#' @param variable_column Character string specifying the column containing variable names (default: "Variable").
#' @param grouping_columns Character vector of column names to use for grouping (default: c("Treatment", "Genotype")).
#'   Function will auto-detect which columns are available.
#' @param sample_id_columns Character vector of columns identifying individual samples (default: c("Well")).
#' @param timepoint_column Character string specifying the timepoint column (default: "Timepoint").
#' @param scale_method Character string specifying scaling method. Options: "z_score" (default), "min_max", "robust", "none".
#' @param aggregation_method Character string specifying how to aggregate multiple measurements. Options: "mean" (default), "median", "sum".
#' @param missing_value_handling Character string specifying how to handle missing values. Options: "remove" (default), "impute_mean", "impute_zero".
#' @param cluster_method Character string specifying clustering distance method. Options: "euclidean" (default), "correlation", "manhattan".
#' @param cluster_rows Logical indicating whether to cluster rows (default: TRUE).
#' @param cluster_cols Logical indicating whether to cluster columns (default: TRUE).
#' @param create_individual_heatmaps Logical indicating whether to create separate heatmaps for each grouping variable (default: TRUE).
#' @param create_combined_heatmap Logical indicating whether to create interaction heatmap when multiple grouping variables are present (default: TRUE).
#' @param create_variable_correlation Logical indicating whether to create variable correlation heatmap (default: TRUE).
#' @param output_dir Character string specifying output directory for saved plots. If NULL, creates "mea_heatmaps" in current directory.
#' @param save_plots Logical indicating whether to save plots to disk (default: TRUE).
#' @param plot_format Character string specifying file format for saved plots (default: "png").
#' @param plot_width Numeric value specifying plot width in inches (default: 10).
#' @param plot_height Numeric value specifying plot height in inches (default: 8).
#' @param dpi Numeric value specifying resolution for saved plots (default: 300).
#' @param fontsize Numeric value specifying font size for heatmap labels (default: 10).
#' @param angle_col Numeric value specifying angle for column labels in degrees (default: 45).
#' @param show_rownames Logical indicating whether to show row names (default: TRUE).
#' @param show_colnames Logical indicating whether to show column names (default: TRUE).
#' @param return_data Logical indicating whether to return processed data matrices (default: TRUE).
#' @param verbose Logical indicating whether to print progress messages (default: TRUE).
#' @param quality_threshold Numeric value between 0-1 specifying minimum data completeness per variable (default: 0.8).
#' @param min_observations Numeric value specifying minimum observations required per group (default: 3).
#'
#' @return A list containing:
#'   \item{individual_heatmaps}{Named list of heatmap objects for each grouping variable}
#'   \item{combined_heatmap}{Heatmap object for grouping variable interactions (if applicable)}
#'   \item{variable_correlation}{List with correlation heatmap and correlation matrix}
#'   \item{metadata}{List containing processing information and parameters used}
#'   Each heatmap object contains: heatmap (pheatmap object), scaled_data (processed matrix),
#'   raw_data (aggregated input data), annotation (row annotations), annotation_colors (color schemes),
#'   and scaling_info (scaling parameters).
#'
#' @details
#' The function performs several key operations:
#' \itemize{
#'   \item Quality filtering: Removes variables with insufficient data completeness
#'   \item Missing value handling: Multiple strategies for dealing with NA values
#'   \item Data aggregation: Combines multiple measurements per group using specified method
#'   \item Scaling: Applies normalization methods appropriate for heatmap visualization
#'   \item Clustering: Hierarchical clustering of rows and/or columns using specified distance metrics
#'   \item Visualization: Creates publication-ready heatmaps with proper color schemes and annotations
#' }
#'
#' For scaling methods:
#' \itemize{
#'   \item z_score: Centers data around mean with unit variance (best for comparing relative changes)
#'   \item min_max: Scales to 0-1 range (best for absolute comparisons)
#'   \item robust: Uses median and MAD for outlier-resistant scaling
#'   \item none: No scaling applied
#' }
#'
#' The function automatically adjusts plot dimensions based on data size and uses optimized
#' color palettes appropriate for the scaling method chosen (diverging palettes for z_score/robust,
#' sequential palettes for min_max).
#'
#' @examples
#' \dontrun{
#' # Basic usage with processing result
#' heatmaps <- create_mea_heatmaps_enhanced(
#'   processing_result = mea_results,
#'   scale_method = "z_score"
#' )
#' 
#' # Custom grouping and scaling
#' heatmaps <- create_mea_heatmaps_enhanced(
#'   data = mea_data,
#'   grouping_columns = c("Treatment", "Genotype", "Timepoint"),
#'   scale_method = "robust",
#'   aggregation_method = "median",
#'   cluster_method = "correlation"
#' )
#' 
#' # High-quality publication plots
#' heatmaps <- create_mea_heatmaps_enhanced(
#'   processing_result = results,
#'   save_plots = TRUE,
#'   plot_format = "pdf",
#'   dpi = 600,
#'   plot_width = 12,
#'   plot_height = 10,
#'   fontsize = 12
#' )
#' 
#' # Access specific results
#' treatment_heatmap <- heatmaps$treatment_heatmap$heatmap
#' correlation_matrix <- heatmaps$variable_correlation$correlation_matrix
#' }
#'
#' @importFrom pheatmap pheatmap
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom dplyr group_by summarise filter select mutate arrange distinct across all_of
#' @importFrom tidyr pivot_wider unite
#' @importFrom ggplot2 ggplot
#'
#' @seealso 
#' \code{\link{pheatmap}} for the underlying heatmap generation
#' 
#' @export
create_mea_heatmaps_enhanced <- function(
    data = NULL,
    processing_result = NULL,
    config = NULL,
    
    # Data column specifications
    value_column = "Normalized_Value",
    variable_column = "Variable",
    grouping_columns = c("Treatment", "Genotype"),
    sample_id_columns = c("Well"),
    timepoint_column = "Timepoint",
    
    # Scaling and transformation options
    scale_method = "z_score",           # "z_score", "min_max", "robust", "none"
    aggregation_method = "mean",        # "mean", "median", "sum"
    missing_value_handling = "remove",  # "remove", "impute_mean", "impute_zero"
    
    # Visual customization
    cluster_method = "euclidean",       # "euclidean", "correlation", "manhattan"
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    
    # Heatmap generation options
    create_individual_heatmaps = TRUE,   # Each grouping variable separately
    create_combined_heatmap = TRUE,      # Interaction heatmap
    create_variable_correlation = TRUE,   # Variable correlation
    
    # Output options
    output_dir = NULL,
    save_plots = TRUE,
    plot_format = "png",
    plot_width = 10,  # Changed to match R Markdown
    plot_height = 8,  # Changed to match R Markdown
    dpi = 300,
    
    # Display options (matching R Markdown exactly)
    fontsize = 10,
    angle_col = 45,
    show_rownames = TRUE,
    show_colnames = TRUE,
    
    # Advanced options
    return_data = TRUE,
    verbose = TRUE,
    quality_threshold = 0.8,
    min_observations = 3
) {
  
  # Load required libraries
  required_packages <- c("pheatmap", "RColorBrewer", "viridis", "dplyr", "tidyr", "ggplot2")
  for (pkg in required_packages) {
    if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  if (verbose) cat("\n=== ENHANCED MEA HEATMAP GENERATION ===\n")
  
  # ============================================================================
  # ENHANCED COLOR PALETTES (matching R Markdown)
  # ============================================================================
  
  enhanced_color_palettes <- list(
    scientific_diverging = colorRampPalette(c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7", 
                                              "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"))(100),
    scientific_sequential = colorRampPalette(c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", 
                                               "#EF6548", "#D7301F", "#B30000", "#7F0000"))(100),
    rdbu_diverging = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100),
    viridis_plasma = viridis(100, option = "plasma"),
    cool_warm = colorRampPalette(c("#3B4CC0", "#688AE8", "#A4C2F4", "#E6E6FA", 
                                   "#FFB6C1", "#FF6B6B", "#DC143C", "#8B0000"))(100)
  )
  
  get_optimal_colors <- function(scale_method, color_scheme = "RdBu") {
    if (scale_method %in% c("z_score", "robust")) {
      switch(color_scheme,
             "RdBu" = enhanced_color_palettes$rdbu_diverging,
             "scientific" = enhanced_color_palettes$scientific_diverging,
             enhanced_color_palettes$rdbu_diverging)
    } else {
      switch(color_scheme,
             "viridis" = enhanced_color_palettes$viridis_plasma,
             "scientific" = enhanced_color_palettes$scientific_sequential,
             enhanced_color_palettes$scientific_sequential)
    }
  }
  
  # ============================================================================
  # DATA INPUT HANDLING
  # ============================================================================
  
  if (!is.null(processing_result)) {
    if (verbose) cat("Using data from processing result...\n")
    
    if (!is.null(processing_result$normalized_data)) {
      data <- processing_result$normalized_data
      if (verbose) cat("Using normalized data\n")
    } else if (!is.null(processing_result$raw_data)) {
      data <- processing_result$raw_data
      value_column <- "Value"
      if (verbose) cat("Using raw data (no normalization found)\n")
    } else {
      stop("Processing result does not contain usable data")
    }
    
    if (is.null(config) && !is.null(processing_result$config_used)) {
      config <- processing_result$config_used
    }
  } else if (is.null(data)) {
    stop("Must provide either 'data' or 'processing_result'")
  }
  
  # Validate required columns
  required_cols <- c(value_column, variable_column)
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Auto-detect available grouping columns (exclude Experiment)
  available_grouping <- grouping_columns[grouping_columns %in% names(data)]
  available_grouping <- available_grouping[!available_grouping %in% c("Experiment", "experiment", "Exp")]
  
  if (verbose) {
    cat("Data dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
    cat("All column names:", paste(names(data), collapse = ", "), "\n")
    cat("Requested grouping columns:", paste(grouping_columns, collapse = ", "), "\n")
    cat("Found grouping columns:", paste(available_grouping, collapse = ", "), "\n")
    
    # Show which requested columns are missing
    missing_grouping <- grouping_columns[!grouping_columns %in% names(data)]
    if (length(missing_grouping) > 0) {
      cat("Missing grouping columns:", paste(missing_grouping, collapse = ", "), "\n")
    }
  }
  
  if (length(available_grouping) == 0) {
    warning("No specified grouping columns found.")
    potential_grouping <- setdiff(names(data), c(value_column, variable_column, sample_id_columns, 
                                                 "Experiment", "experiment", "Exp", timepoint_column))
    available_grouping <- potential_grouping
    if (verbose) {
      cat("Using potential grouping columns:", paste(available_grouping, collapse = ", "), "\n")
    }
  }
  
  if (verbose) {
    cat("Final grouping columns to use:", paste(available_grouping, collapse = ", "), "\n")
    cat("Scaling method:", scale_method, "\n")
  }
  
  # ============================================================================
  # HELPER FUNCTIONS
  # ============================================================================
  
  # Handle missing values
  handle_missing_values <- function(data, value_col, method, verbose) {
    if (method == "remove") {
      original_rows <- nrow(data)
      data <- data[!is.na(data[[value_col]]), ]
      if (verbose) cat("Removed", original_rows - nrow(data), "rows with missing values\n")
    } else if (method == "impute_mean") {
      mean_val <- mean(data[[value_col]], na.rm = TRUE)
      data[[value_col]][is.na(data[[value_col]])] <- mean_val
      if (verbose) cat("Imputed missing values with mean:", round(mean_val, 3), "\n")
    } else if (method == "impute_zero") {
      data[[value_col]][is.na(data[[value_col]])] <- 0
      if (verbose) cat("Imputed missing values with zero\n")
    }
    return(data)
  }
  
  # Quality filtering
  quality_filter <- function(data, var_col, val_col, group_cols, threshold, min_obs, verbose) {
    original_vars <- length(unique(data[[var_col]]))
    
    # Calculate data completeness per variable
    var_completeness <- data %>%
      group_by(!!sym(var_col)) %>%
      summarise(completeness = mean(!is.na(!!sym(val_col))), .groups = 'drop') %>%
      filter(completeness >= threshold)
    
    data <- data %>%
      filter(!!sym(var_col) %in% var_completeness[[var_col]])
    
    # Remove groups with insufficient observations
    for (group_col in group_cols) {
      if (group_col %in% names(data)) {
        sufficient_groups <- data %>%
          group_by(!!sym(group_col)) %>%
          summarise(n = n(), .groups = 'drop') %>%
          filter(n >= min_obs)
        
        data <- data %>%
          filter(!!sym(group_col) %in% sufficient_groups[[group_col]])
      }
    }
    
    if (verbose) {
      final_vars <- length(unique(data[[var_col]]))
      cat("Quality filtering: kept", final_vars, "of", original_vars, "variables\n")
    }
    
    return(data)
  }
  
  # Enhanced scaling function (matching R Markdown)
  apply_scaling <- function(matrix_data, method = scale_method, verbose = FALSE) {
    if (is.null(matrix_data)) return(NULL)
    
    if (verbose) cat("    Applying", method, "scaling...\n")
    
    tryCatch({
      scaled_matrix <- switch(method,
                              "z_score" = scale(matrix_data),
                              "min_max" = {
                                min_val <- min(matrix_data, na.rm = TRUE)
                                max_val <- max(matrix_data, na.rm = TRUE)
                                if (max_val > min_val) (matrix_data - min_val) / (max_val - min_val) else matrix_data
                              },
                              "robust" = scale(matrix_data, 
                                               center = apply(matrix_data, 2, median, na.rm = TRUE),
                                               scale = apply(matrix_data, 2, mad, na.rm = TRUE)),
                              "none" = matrix_data,
                              matrix_data
      )
      
      # Handle any scaling issues
      if (any(is.na(scaled_matrix)) && !any(is.na(matrix_data))) {
        warning("Scaling introduced NA values, using original matrix")
        scaled_matrix <- matrix_data
      }
      
      return(scaled_matrix)
    }, error = function(e) {
      if (verbose) cat("    ✗ Scaling error:", e$message, "\n")
      return(matrix_data)
    })
  }
  
  # Enhanced color scheme function (matching R Markdown)
  get_color_scheme <- function(matrix_data, method, color_scheme = "RdBu") {
    colors <- get_optimal_colors(method, color_scheme)
    
    if (method %in% c("z_score", "robust")) {
      max_abs <- max(abs(matrix_data), na.rm = TRUE)
      if (max_abs == 0 || is.infinite(max_abs)) max_abs <- 1
      breaks <- seq(-max_abs, max_abs, length.out = 101)
    } else {
      min_val <- min(matrix_data, na.rm = TRUE)
      max_val <- max(matrix_data, na.rm = TRUE)
      if (min_val == max_val || is.infinite(min_val) || is.infinite(max_val)) {
        breaks <- c(min_val - 0.1, min_val, max_val + 0.1)
        colors <- colors[c(1, 50, 100)]
      } else {
        breaks <- seq(min_val, max_val, length.out = 101)
      }
    }
    
    return(list(breaks = breaks, colors = colors))
  }
  
  # ============================================================================
  # CORE HEATMAP CREATION FUNCTION (Enhanced with R Markdown aesthetics)
  # ============================================================================
  
  create_heatmap <- function(data, group_vars, var_col, val_col, title, filename, 
                             annotation_setup = NULL) {
    # Handle both single and multiple grouping variables
    if (length(group_vars) == 1) {
      # Single grouping variable
      agg_data <- data %>%
        group_by(across(all_of(c(group_vars, var_col)))) %>%
        summarise(value = get(aggregation_method)(.data[[val_col]], na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = all_of(var_col), values_from = value, values_fill = NA)
      
      # Convert to matrix
      row_names <- agg_data[[group_vars]]
      matrix_data <- as.matrix(agg_data[, -1])
      rownames(matrix_data) <- row_names
      
      annotation_row <- NULL
      annotation_colors <- NULL
      
    } else if (length(group_vars) == 2) {
      # Two grouping variables - create proper interaction matrix
      primary_var <- group_vars[1]
      secondary_var <- group_vars[2]
      
      # Aggregate data for each combination, keeping grouping variables
      agg_data <- data %>%
        group_by(across(all_of(c(group_vars, var_col)))) %>%
        summarise(value = get(aggregation_method)(.data[[val_col]], na.rm = TRUE), .groups = 'drop')
      
      # Create annotation BEFORE transforming the data
      annotation_data <- agg_data %>%
        select(all_of(group_vars)) %>%
        distinct() %>%
        unite("group_combination", all_of(group_vars), sep = " × ", remove = FALSE) %>%
        column_to_rownames("group_combination")
      
      # Create matrix with proper row names
      matrix_data <- agg_data %>%
        unite("group_combination", all_of(group_vars), sep = " × ") %>%
        select(group_combination, all_of(var_col), value) %>%
        pivot_wider(names_from = all_of(var_col), values_from = value, values_fill = NA) %>%
        column_to_rownames("group_combination") %>%
        as.matrix()
      
      # Use the annotation_data we created earlier
      annotation_row <- annotation_data
      
      # Create distinct color palettes
      primary_groups <- unique(annotation_row[[primary_var]])
      secondary_groups <- unique(annotation_row[[secondary_var]])
      
      primary_colors <- RColorBrewer::brewer.pal(min(max(3, length(primary_groups)), 9), "Set1")[1:length(primary_groups)]
      names(primary_colors) <- primary_groups
      
      if (length(secondary_groups) <= 8) {
        secondary_colors <- RColorBrewer::brewer.pal(max(3, length(secondary_groups)), "Set2")
      } else {
        secondary_colors <- rainbow(length(secondary_groups))
      }
      names(secondary_colors) <- secondary_groups
      
      annotation_colors <- list()
      annotation_colors[[primary_var]] <- primary_colors
      annotation_colors[[secondary_var]] <- secondary_colors
      
    } else {
      stop("Can only handle 1 or 2 grouping variables")
    }
    
    # Apply scaling
    matrix_data <- apply_scaling(matrix_data, scale_method, verbose)
    
    # Get color scheme (using enhanced function)
    color_scheme <- get_color_scheme(matrix_data, scale_method, "RdBu")
    
    # Auto-adjust dimensions (matching R Markdown logic)
    auto_adjust_dimensions <- TRUE  # Set to TRUE to match R Markdown
    if (auto_adjust_dimensions) {
      plot_width_adj <- max(6, min(plot_width + ncol(matrix_data) * 0.1, 20))
      plot_height_adj <- max(4, min(plot_height + nrow(matrix_data) * 0.1, 16))
    } else {
      plot_width_adj <- plot_width
      plot_height_adj <- plot_height
    }
    
    # Additional adjustment for annotations
    if (!is.null(annotation_row)) plot_width_adj <- plot_width_adj * 1.2
    
    # Create heatmap with EXACT R Markdown aesthetics
    tryCatch({
      # Ensure output directory exists
      if (save_plots) {
        output_file <- file.path(output_dir, paste0(filename, ".", plot_format))
        output_file_dir <- dirname(output_file)
        if (!dir.exists(output_file_dir)) {
          dir.create(output_file_dir, recursive = TRUE, showWarnings = FALSE)
        }
      }
      
      p <- pheatmap(
        matrix_data,
        cluster_rows = cluster_rows && nrow(matrix_data) > 1,
        cluster_cols = cluster_cols && ncol(matrix_data) > 1,
        show_rownames = show_rownames,
        show_colnames = show_colnames,
        fontsize = fontsize,
        fontsize_row = fontsize,      # Control row label size (R Markdown style)
        fontsize_col = fontsize,      # Control column label size (R Markdown style)
        angle_col = angle_col,
        cellwidth = 22,               # Auto cell width (R Markdown style)
        cellheight = 30,              # Auto cell height (R Markdown style)
        color = color_scheme$colors,
        breaks = color_scheme$breaks,
        main = title,
        annotation_row = annotation_row,
        annotation_colors = annotation_colors,
        clustering_distance_rows = cluster_method,
        clustering_distance_cols = cluster_method,
        na_col = "grey90",
        filename = if(save_plots) file.path(output_dir, paste0(filename, ".", plot_format)) else NA,
        width = plot_width_adj,
        height = plot_height_adj,
        dpi = dpi,
        silent = !verbose
      )
      
      return(list(
        heatmap = p,
        scaled_data = matrix_data,
        raw_data = agg_data,
        annotation = annotation_row,
        annotation_colors = annotation_colors,
        scaling_info = list(method = scale_method, breaks = color_scheme$breaks)
      ))
      
    }, error = function(e) {
      if (verbose) cat("    ✗ Heatmap creation error:", e$message, "\n")
      try(dev.off(), silent = TRUE)
      return(NULL)
    })
  }
  
  # ============================================================================
  # PREPROCESSING
  # ============================================================================
  
  # Handle missing values and quality filtering
  data <- handle_missing_values(data, value_column, missing_value_handling, verbose)
  data <- quality_filter(data, variable_column, value_column, available_grouping, 
                         quality_threshold, min_observations, verbose)
  
  # Set up output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "mea_heatmaps")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize results
  results <- list()
  
  # ============================================================================
  # GENERATE HEATMAPS
  # ============================================================================
  
  # 1. INDIVIDUAL GROUPING VARIABLE HEATMAPS
  if (create_individual_heatmaps) {
    for (i in seq_along(available_grouping)) {
      group_var <- available_grouping[i]
      if (verbose) cat("\n--- Creating", group_var, "Heatmap ---\n")
      
      heatmap_result <- create_heatmap(
        data = data,
        group_vars = group_var,
        var_col = variable_column,
        val_col = value_column,
        title = paste0(group_var, " Analysis - ", scale_method, " scaling"),
        filename = paste0(tolower(group_var), "_heatmap")
      )
      
      if (!is.null(heatmap_result)) {
        results[[paste0(tolower(group_var), "_heatmap")]] <- heatmap_result
      }
    }
  }
  
  # 2. COMBINED HEATMAP (if 2 or more grouping variables)
  if (create_combined_heatmap && length(available_grouping) >= 2) {
    primary_group <- available_grouping[1]
    secondary_group <- available_grouping[2]
    
    if (verbose) cat("\n--- Creating Combined", primary_group, "×", secondary_group, "Heatmap ---\n")
    
    heatmap_result <- create_heatmap(
      data = data,
      group_vars = c(primary_group, secondary_group),
      var_col = variable_column,
      val_col = value_column,
      title = paste0(primary_group, " × ", secondary_group, " Analysis - ", scale_method, " scaling"),
      filename = paste0(tolower(primary_group), "_", tolower(secondary_group), "_combined")
    )
    
    if (!is.null(heatmap_result)) {
      results$combined_heatmap <- heatmap_result
    }
  }
  
  # 3. VARIABLE CORRELATION HEATMAP (with R Markdown aesthetics)
  if (create_variable_correlation) {
    if (verbose) cat("\n--- Creating Variable Correlation Heatmap ---\n")
    
    tryCatch({
      # Prepare data for correlation - more robust approach
      cor_prep <- data %>%
        mutate(!!sym(value_column) := as.numeric(!!sym(value_column))) %>%
        filter(!is.na(!!sym(value_column))) %>%
        # Create unique sample identifier to avoid aggregation issues
        group_by(across(all_of(c(variable_column, available_grouping)))) %>%
        summarise(avg_value = mean(!!sym(value_column), na.rm = TRUE), .groups = 'drop') %>%
        # Create sample ID based on grouping variables
        unite("sample_id", all_of(available_grouping), sep = "_", remove = FALSE) %>%
        mutate(sample_id = paste0(sample_id, "_", row_number()))
      
      if (verbose) {
        cat("Correlation prep dimensions:", nrow(cor_prep), "rows\n")
        cat("Number of variables:", length(unique(cor_prep[[variable_column]])), "\n")
        cat("Number of samples:", length(unique(cor_prep$sample_id)), "\n")
      }
      
      # Create wide format
      cor_wide <- cor_prep %>%
        select(sample_id, all_of(variable_column), avg_value) %>%
        pivot_wider(names_from = all_of(variable_column), 
                    values_from = avg_value, 
                    values_fill = NA) %>%
        column_to_rownames("sample_id")
      
      # Convert to numeric matrix
      cor_matrix_data <- as.matrix(cor_wide)
      
      # Remove columns with all NAs or no variation
      valid_cols <- apply(cor_matrix_data, 2, function(x) {
        !all(is.na(x)) && var(x, na.rm = TRUE) > 0
      })
      
      if (sum(valid_cols) < 2) {
        if (verbose) cat("Insufficient valid variables for correlation analysis\n")
        return(results)
      }
      
      cor_matrix_data <- cor_matrix_data[, valid_cols]
      
      # Calculate correlation with error handling
      cor_matrix <- cor(cor_matrix_data, use = "pairwise.complete.obs", method = "pearson")
      
      # Check for valid correlation matrix
      if (nrow(cor_matrix) > 1 && !any(is.na(diag(cor_matrix))) && !any(is.infinite(cor_matrix))) {
        
        # Calculate plot dimensions
        plot_width_cor <- max(6, min(10 + ncol(cor_matrix) * 0.2, 20))
        plot_height_cor <- max(4, min(8 + nrow(cor_matrix) * 0.2, 16))
        
        p <- pheatmap(
          cor_matrix,
          cluster_rows = cluster_rows,
          cluster_cols = cluster_cols,
          show_rownames = show_rownames,
          show_colnames = show_colnames,
          fontsize = fontsize,
          color = colorRampPalette(c("#2166AC", "white", "#B2182B"))(100),
          breaks = seq(-1, 1, length.out = 101),
          main = "Variable Correlation - Pearson",
          clustering_distance_rows = "correlation",
          clustering_distance_cols = "correlation",
          display_numbers = TRUE,
          number_format = "%.2f",
          filename = if(save_plots) file.path(output_dir, paste0("variable_correlation.", plot_format)) else NA,
          width = plot_width_cor,
          height = plot_height_cor,
          dpi = dpi,
          silent = !verbose
        )
        
        results$variable_correlation <- list(
          heatmap = p,
          correlation_matrix = cor_matrix
        )
        
        if (verbose) cat("Variable correlation heatmap created successfully\n")
      } else {
        if (verbose) cat("Invalid correlation matrix - contains NAs or infinite values\n")
      }
      
    }, error = function(e) {
      if (verbose) cat("Error creating correlation heatmap:", e$message, "\n")
    })
  }
  
  # ============================================================================
  # SUMMARY
  # ============================================================================
  
  if (verbose) {
    cat("\n=== HEATMAP GENERATION SUMMARY ===\n")
    success_count <- length(results)
    cat("Successfully created", success_count, "heatmap analyses\n")
    
    for (name in names(results)) {
      if (!is.null(results[[name]]$scaled_data)) {
        dims <- dim(results[[name]]$scaled_data)
        cat(paste0("✓ ", gsub("_", " ", name), ": ", dims[1], " groups × ", dims[2], " variables\n"))
      } else if (!is.null(results[[name]]$correlation_matrix)) {
        dims <- dim(results[[name]]$correlation_matrix)
        cat(paste0("✓ ", gsub("_", " ", name), ": ", dims[1], "×", dims[2], " correlation matrix\n"))
      }
    }
    
    if (save_plots) cat("Plots saved to:", output_dir, "\n")
  }
  
  # Add metadata
  results$metadata <- list(
    input_dimensions = dim(data),
    grouping_columns = available_grouping,
    scaling_method = scale_method,
    aggregation_method = aggregation_method,
    creation_time = Sys.time(),
    output_directory = output_dir
  )
  
  return(results)
}

#' Analyze and Visualize PCA Variable Importance
#'
#' This function performs comprehensive analysis of variable importance in Principal Component Analysis,
#' generating multiple visualization types including loading biplots, importance rankings, PC comparisons,
#' and heatmaps. It extracts variable contributions to specified principal components and creates
#' publication-ready plots with detailed statistical summaries.
#'
#' @param pca_result A PCA result object. Can be either a \code{prcomp} object directly, or a list
#'   containing a PCA object in fields named 'pca_result', 'pca', 'result', or 'prcomp'.
#' @param output_dir Character string specifying the directory for saving plots and results (default: "pca_plots").
#' @param experiment_name Character string used as a prefix for output files and plot titles (default: "PCA_Analysis").
#' @param pc_x Character string specifying the principal component for x-axis analysis (default: "PC1").
#' @param pc_y Character string specifying the principal component for y-axis analysis (default: "PC2").
#' @param color_scheme Character string specifying the color palette. Options: "default", "viridis", "colorbrewer" (default: "default").
#' @param top_n Numeric value specifying the number of top variables to focus on in detailed analyses (default: 15).
#' @param min_loading_threshold Numeric value specifying the minimum loading threshold for importance filtering (default: 0.1).
#' @param save_plots Logical indicating whether to save plots and results to disk (default: TRUE).
#' @param show_labels Logical indicating whether to show variable labels on the biplot (default: TRUE).
#' @param verbose Logical indicating whether to print detailed progress messages (default: TRUE).
#'
#' @return A list containing:
#'   \item{plots}{Named list of ggplot objects: 'biplot', 'importance_bar', 'pc_comparison', 'heatmap'}
#'   \item{variable_importance}{Data frame with comprehensive variable importance metrics for all variables}
#'   \item{selected_variables}{Data frame containing the top N most important variables with detailed statistics}
#'   \item{analysis_summary}{List with key analysis metrics and variance explained information}
#'   \item{config_used}{List documenting all parameters used in the analysis}
#'
#' @details
#' The function calculates multiple importance metrics for each variable:
#' \itemize{
#'   \item \strong{PC loadings}: Direct loading values for specified principal components
#'   \item \strong{Combined importance}: Euclidean distance combining both PC loadings
#'   \item \strong{Contribution percentages}: Percent contribution to each PC's total variance
#'   \item \strong{Ranking}: Variables ranked by combined importance score
#' }
#'
#' Four visualization types are generated:
#' \itemize{
#'   \item \strong{Loading Biplot}: Scatter plot showing variable loadings on both PCs with size indicating importance
#'   \item \strong{Importance Bar Chart}: Ranked bar chart of top variables by combined importance
#'   \item \strong{PC Comparison}: Side-by-side comparison of absolute loadings for both PCs
#'   \item \strong{Loading Heatmap}: Color-coded matrix showing loading values and directions
#' }
#'
#' The function automatically:
#' \itemize{
#'   \item Validates input PCA objects from various sources
#'   \item Calculates variance explained by each principal component
#'   \item Creates publication-ready plots with consistent theming
#'   \item Exports detailed CSV files with variable rankings and analysis summaries
#'   \item Provides comprehensive statistical summaries
#' }
#'
#' Color schemes provide different aesthetic options:
#' \itemize{
#'   \item \code{default}: Blue/red palette suitable for most publications
#'   \item \code{viridis}: Colorblind-friendly viridis color scale
#'   \item \code{colorbrewer}: ColorBrewer palettes optimized for scientific visualization
#' }
#'
#' @examples
#' \dontrun{
#' # Basic analysis with default settings
#' results <- analyze_pca_variable_importance_general(pca_result = my_pca)
#' 
#' # Custom analysis focusing on PC2 vs PC3
#' results <- analyze_pca_variable_importance_general(
#'   pca_result = my_pca,
#'   pc_x = "PC2",
#'   pc_y = "PC3",
#'   experiment_name = "Treatment_Analysis",
#'   color_scheme = "viridis",
#'   top_n = 20
#' )
#' 
#' # High-throughput analysis with minimal output
#' results <- analyze_pca_variable_importance_general(
#'   pca_result = my_pca,
#'   save_plots = FALSE,
#'   show_labels = FALSE,
#'   verbose = FALSE,
#'   top_n = 50
#' )
#' 
#' # Access specific results
#' importance_table <- results$variable_importance
#' top_variables <- results$selected_variables
#' biplot <- results$plots$biplot
#' }
#'
#' @section Output Files:
#' When \code{save_plots = TRUE}, the function creates:
#' \itemize{
#'   \item PNG files for each visualization type
#'   \item CSV file with complete variable importance rankings
#'   \item CSV file with selected top variables and detailed metrics
#'   \item CSV file with analysis summary and metadata
#' }
#'
#' @importFrom ggplot2 ggplot geom_point geom_hline geom_vline geom_text geom_col geom_tile
#'   aes labs theme_minimal theme element_text element_rect element_blank scale_size_continuous
#'   scale_fill_manual scale_fill_gradient2 coord_fixed coord_flip position_dodge ggsave
#' @importFrom dplyr select mutate arrange
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr gather
#' @importFrom knitr kable
#' @importFrom DT datatable
#'
#' @seealso
#' \code{\link{prcomp}} for PCA computation, \code{\link{biplot}} for basic PCA plotting
#'
#' @export
analyze_pca_variable_importance_general <- function(pca_result = NULL,
                                                    output_dir = "pca_plots",
                                                    experiment_name = "PCA_Analysis",
                                                    pc_x = "PC1",
                                                    pc_y = "PC2",
                                                    color_scheme = "default",
                                                    top_n = 15,
                                                    min_loading_threshold = 0.1,
                                                    save_plots = TRUE,
                                                    show_labels = TRUE,
                                                    verbose = TRUE) {
  
  # Load required libraries
  required_packages <- c("ggplot2", "dplyr", "viridis", "RColorBrewer", "gridExtra", 
                         "tidyr", "knitr", "DT")
  for (pkg in required_packages) {
    if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Helper function for null coalescing
  `%||%` <- function(lhs, rhs) {
    if (!is.null(lhs)) lhs else rhs
  }
  
  if (verbose) cat("=== PCA VARIABLE IMPORTANCE ANALYSIS ===\n")
  
  # ============================================================================
  # INPUT VALIDATION AND PCA OBJECT EXTRACTION
  # ============================================================================
  
  # Function to extract PCA object from various input formats
  extract_pca_object <- function(input_data) {
    if (inherits(input_data, "prcomp")) {
      return(input_data)
    } else if (is.list(input_data)) {
      possible_fields <- c("pca_result", "pca", "result", "prcomp")
      for (field in possible_fields) {
        if (!is.null(input_data[[field]]) && inherits(input_data[[field]], "prcomp")) {
          return(input_data[[field]])
        }
      }
    }
    stop("Could not find PCA object in provided data")
  }
  
  # Extract PCA object
  pca_obj <- extract_pca_object(pca_result)
  
  # Get basic information
  loadings <- pca_obj$rotation
  n_vars <- nrow(loadings)
  n_pcs <- ncol(loadings)
  
  # Calculate variance explained
  var_explained <- pca_obj$sdev^2 / sum(pca_obj$sdev^2)
  var_explained_pct <- round(var_explained * 100, 1)
  
  if (verbose) {
    cat("PCA Analysis Summary:\n")
    cat("- Total Variables:", n_vars, "\n")
    cat("- Total Principal Components:", n_pcs, "\n")
    cat("- Top 5 PCs explain:", sum(var_explained_pct[1:min(5, n_pcs)]), "% of variance\n")
  }
  
  # Create output directory
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created output directory:", output_dir, "\n")
  }
  
  # ============================================================================
  # VARIABLE IMPORTANCE CALCULATION
  # ============================================================================
  
  # Extract PC numbers
  pc_x_num <- as.numeric(gsub("PC", "", pc_x))
  pc_y_num <- as.numeric(gsub("PC", "", pc_y))
  
  # Validate PC selection
  if (pc_x_num > n_pcs || pc_y_num > n_pcs || pc_x_num < 1 || pc_y_num < 1) {
    stop("Invalid PC selection. Available PCs: 1 to ", n_pcs)
  }
  
  # Get loadings for selected PCs
  pc_x_loadings <- loadings[, pc_x_num]
  pc_y_loadings <- loadings[, pc_y_num]
  
  # Calculate comprehensive variable importance
  variable_importance <- data.frame(
    Variable = rownames(loadings),
    PC_X_Loading = pc_x_loadings,
    PC_Y_Loading = pc_y_loadings,
    PC_X_Abs_Loading = abs(pc_x_loadings),
    PC_Y_Abs_Loading = abs(pc_y_loadings),
    Combined_Importance = sqrt(pc_x_loadings^2 + pc_y_loadings^2),
    PC_X_Contribution = pc_x_loadings^2,
    PC_Y_Contribution = pc_y_loadings^2,
    stringsAsFactors = FALSE
  )
  
  # Add percentage contributions
  variable_importance$PC_X_Contribution_Pct <- 
    (variable_importance$PC_X_Contribution / sum(variable_importance$PC_X_Contribution)) * 100
  variable_importance$PC_Y_Contribution_Pct <- 
    (variable_importance$PC_Y_Contribution / sum(variable_importance$PC_Y_Contribution)) * 100
  
  # Sort by combined importance
  variable_importance <- variable_importance[order(variable_importance$Combined_Importance, decreasing = TRUE), ]
  variable_importance$Rank <- 1:nrow(variable_importance)
  
  # Get variance explained for selected PCs
  pc_x_var <- var_explained_pct[pc_x_num]
  pc_y_var <- var_explained_pct[pc_y_num]
  
  if (verbose) {
    cat("Analysis of", pc_x, "vs", pc_y, "\n")
    cat("Variance explained:", pc_x, "=", pc_x_var, "%,", pc_y, "=", pc_y_var, "%\n")
    cat("Combined variance explained:", round(pc_x_var + pc_y_var, 1), "%\n")
  }
  
  # ============================================================================
  # COLOR SCHEME SETUP
  # ============================================================================
  
  # Define color schemes
  color_schemes <- list(
    default = list(
      points = "#2E86AB",
      text = "#F24236",
      axes = "grey60",
      background = "white",
      bar = "#E74C3C",
      pc_colors = c("#3498DB", "#E67E22"),
      heatmap = c("#2C3E50", "white", "#E74C3C")
    ),
    viridis = list(
      points = viridis::viridis(3)[2],
      text = viridis::viridis(3)[3],
      axes = "grey60",
      background = "white",
      bar = viridis::viridis(3)[1],
      pc_colors = viridis::viridis(2),
      heatmap = viridis::viridis(3)
    ),
    colorbrewer = list(
      points = RColorBrewer::brewer.pal(3, "Set1")[1],
      text = RColorBrewer::brewer.pal(3, "Set1")[2],
      axes = "grey60",
      background = "white",
      bar = RColorBrewer::brewer.pal(3, "Set1")[3],
      pc_colors = RColorBrewer::brewer.pal(2, "Dark2"),
      heatmap = RColorBrewer::brewer.pal(3, "RdBu")
    )
  )
  
  colors <- color_schemes[[color_scheme]] %||% color_schemes[["default"]]
  
  # ============================================================================
  # PLOT 1: LOADING BIPLOT
  # ============================================================================
  
  selected_variables <- head(variable_importance, top_n)
  biplot_data <- selected_variables
  
  p_biplot <- ggplot(biplot_data, aes(x = PC_X_Loading, y = PC_Y_Loading)) +
    # Add axis lines
    geom_hline(yintercept = 0, linetype = "dashed", color = colors$axes, alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = colors$axes, alpha = 0.8) +
    
    # Add points
    geom_point(aes(size = Combined_Importance), 
               color = colors$points, alpha = 0.7) +
    
    # Customize scales
    scale_size_continuous(range = c(2, 6), name = "Combined\nImportance") +
    
    # Labels and theme
    labs(
      title = paste("PCA Variable Loadings:", pc_x, "vs", pc_y),
      subtitle = paste("Experiment:", experiment_name, "| Variables shown:", nrow(biplot_data)),
      x = paste0(pc_x, " Loadings (", pc_x_var, "% variance)"),
      y = paste0(pc_y, " Loadings (", pc_y_var, "% variance)"),
      caption = paste("Total variance explained:", round(pc_x_var + pc_y_var, 1), "%")
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.background = element_rect(fill = colors$background, color = "black", size = 0.3)
    ) +
    coord_fixed()
  
  # Add labels if requested
  if (show_labels) {
    label_data <- head(biplot_data, min(10, nrow(biplot_data)))
    p_biplot <- p_biplot + 
      geom_text(data = label_data, aes(label = Variable), 
                vjust = -0.5, hjust = 0.5, size = 3.5, 
                color = colors$text, check_overlap = TRUE)
  }
  
  # ============================================================================
  # PLOT 2: IMPORTANCE BAR CHART
  # ============================================================================
  
  n_bars <- min(20, nrow(selected_variables))
  bar_data <- head(selected_variables, n_bars)
  bar_data$Variable <- factor(bar_data$Variable, levels = rev(bar_data$Variable))
  
  p_bars <- ggplot(bar_data, aes(x = Variable, y = Combined_Importance)) +
    geom_col(fill = colors$bar, alpha = 0.8) +
    coord_flip() +
    geom_text(aes(label = round(Combined_Importance, 3)), 
              hjust = -0.1, size = 3, color = "black") +
    
    labs(
      title = paste("Top", n_bars, "Variables by Combined Importance"),
      subtitle = paste("Principal Components:", pc_x, "vs", pc_y),
      x = "Variables",
      y = "Combined Importance Score",
      caption = paste("Experiment:", experiment_name)
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # ============================================================================
  # PLOT 3: PC CONTRIBUTION COMPARISON
  # ============================================================================
  
  n_comparison <- min(15, nrow(selected_variables))
  comparison_data <- head(selected_variables, n_comparison) %>%
    select(Variable, PC_X_Abs_Loading, PC_Y_Abs_Loading) %>%
    gather(key = "PC", value = "Loading", -Variable) %>%
    mutate(PC = ifelse(PC == "PC_X_Abs_Loading", pc_x, pc_y),
           Variable = factor(Variable, levels = rev(head(selected_variables$Variable, n_comparison))))
  
  pc_colors_named <- colors$pc_colors
  names(pc_colors_named) <- c(pc_x, pc_y)
  
  p_comparison <- ggplot(comparison_data, aes(x = Variable, y = Loading, fill = PC)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    scale_fill_manual(values = pc_colors_named) +
    coord_flip() +
    
    labs(
      title = paste("Loading Comparison:", pc_x, "vs", pc_y),
      subtitle = paste("Top", n_comparison, "Variables | Absolute Loading Values"),
      x = "Variables",
      y = "Absolute Loading Value",
      fill = "Principal Component",
      caption = paste("Experiment:", experiment_name)
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
  
  # ============================================================================
  # PLOT 4: LOADING HEATMAP
  # ============================================================================
  
  n_heatmap <- min(20, nrow(selected_variables))
  heatmap_data <- head(selected_variables, n_heatmap) %>%
    select(Variable, PC_X_Loading, PC_Y_Loading) %>%
    gather(key = "PC", value = "Loading", -Variable) %>%
    mutate(
      PC = ifelse(PC == "PC_X_Loading", pc_x, pc_y),
      Variable = factor(Variable, levels = rev(head(selected_variables$Variable, n_heatmap)))
    )
  
  p_heatmap <- ggplot(heatmap_data, aes(x = PC, y = Variable, fill = Loading)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = colors$heatmap[1], 
      mid = colors$heatmap[2], 
      high = colors$heatmap[3], 
      midpoint = 0, 
      name = "Loading\nValue"
    ) +
    geom_text(aes(label = round(Loading, 2)), 
              size = 3, 
              color = "black") +
    
    labs(
      title = paste("Loading Heatmap:", pc_x, "vs", pc_y),
      subtitle = paste("Top", n_heatmap, "Variables | Color indicates loading strength and direction"),
      x = "Principal Components",
      y = "Variables",
      caption = paste("Experiment:", experiment_name)
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 11),
      panel.grid = element_blank()
    )
  
  # ============================================================================
  # SAVE PLOTS
  # ============================================================================
  
  plots <- list(
    biplot = p_biplot,
    importance_bar = p_bars,
    pc_comparison = p_comparison,
    heatmap = p_heatmap
  )
  
  if (save_plots) {
    plot_names <- c("Biplot", "ImportanceBar", "PCComparison", "Heatmap")
    
    for (i in seq_along(plots)) {
      filename <- file.path(output_dir, paste0(experiment_name, "_", pc_x, "_", pc_y, "_", plot_names[i], ".png"))
      ggsave(
        filename = filename,
        plot = plots[[i]],
        width = 12, height = 10, dpi = 300
      )
      if (verbose) cat("Saved:", plot_names[i], "to", filename, "\n")
    }
  }
  
  # ============================================================================
  # EXPORT RESULTS
  # ============================================================================
  
  if (save_plots) {
    # Save detailed results to CSV
    results_file <- file.path(output_dir, paste0(experiment_name, "_VariableImportance_", pc_x, "_", pc_y, ".csv"))
    write.csv(variable_importance, results_file, row.names = FALSE)
    
    # Save selected variables subset
    selected_file <- file.path(output_dir, paste0(experiment_name, "_SelectedVariables_", pc_x, "_", pc_y, ".csv"))
    write.csv(selected_variables, selected_file, row.names = FALSE)
    
    # Create a summary report
    summary_report <- data.frame(
      Analysis_Date = Sys.time(),
      Experiment = experiment_name,
      PC_X = pc_x,
      PC_Y = pc_y,
      PC_X_Variance = pc_x_var,
      PC_Y_Variance = pc_y_var,
      Combined_Variance = pc_x_var + pc_y_var,
      Total_Variables = n_vars,
      Variables_Analyzed = nrow(selected_variables),
      Max_Importance = max(variable_importance$Combined_Importance),
      Mean_Importance = mean(variable_importance$Combined_Importance),
      Top_Variable = variable_importance$Variable[1],
      Top_Variable_Importance = variable_importance$Combined_Importance[1]
    )
    
    summary_file <- file.path(output_dir, paste0(experiment_name, "_AnalysisSummary.csv"))
    write.csv(summary_report, summary_file, row.names = FALSE)
    
    if (verbose) {
      cat("Results exported to:", results_file, "\n")
      cat("Selected variables exported to:", selected_file, "\n")
      cat("Analysis summary exported to:", summary_file, "\n")
    }
  }
  
  # ============================================================================
  # ANALYSIS SUMMARY
  # ============================================================================
  
  if (verbose) {
    cat("\n=== FINAL ANALYSIS SUMMARY ===\n")
    cat("Experiment:", experiment_name, "\n")
    cat("Principal Components Analyzed:", pc_x, "vs", pc_y, "\n")
    cat("Variance Explained:", pc_x_var, "% +", pc_y_var, "% =", round(pc_x_var + pc_y_var, 1), "%\n")
    cat("Total Variables:", n_vars, "\n")
    cat("Variables in Analysis:", nrow(selected_variables), "\n")
    if (save_plots) cat("Output Directory:", output_dir, "\n")
    
    cat("\nVariable Importance Statistics:\n")
    cat("Maximum Combined Importance:", round(max(variable_importance$Combined_Importance), 3), "\n")
    cat("Mean Combined Importance:", round(mean(variable_importance$Combined_Importance), 3), "\n")
    cat("Variables above threshold (", min_loading_threshold, "):", 
        sum(variable_importance$Combined_Importance > min_loading_threshold), "\n")
    
    cat("\nTop 5 Most Important Variables:\n")
    for (i in 1:5) {
      cat(sprintf("%d. %s (Importance: %.3f)\n", 
                  i, 
                  variable_importance$Variable[i], 
                  variable_importance$Combined_Importance[i]))
    }
  }
  
  # ============================================================================
  # RETURN COMPREHENSIVE RESULTS
  # ============================================================================
  
  return(list(
    plots = plots,
    variable_importance = variable_importance,
    selected_variables = selected_variables,
    analysis_summary = list(
      experiment_name = experiment_name,
      pc_x = pc_x,
      pc_y = pc_y,
      pc_x_variance = pc_x_var,
      pc_y_variance = pc_y_var,
      combined_variance = pc_x_var + pc_y_var,
      total_variables = n_vars,
      variables_analyzed = nrow(selected_variables),
      max_importance = max(variable_importance$Combined_Importance),
      mean_importance = mean(variable_importance$Combined_Importance),
      top_variable = variable_importance$Variable[1],
      top_variable_importance = variable_importance$Combined_Importance[1],
      variables_above_threshold = sum(variable_importance$Combined_Importance > min_loading_threshold)
    ),
    config_used = list(
      color_scheme = color_scheme,
      top_n = top_n,
      min_loading_threshold = min_loading_threshold,
      save_plots = save_plots,
      show_labels = show_labels,
      output_dir = output_dir
    )
  ))
}
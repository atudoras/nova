# pca_analysis.R
# Functions for Principal Component Analysis of MEA data

#' Perform MEA PCA Analysis
#' 
#' Template function for performing PCA on MEA data
#'
#' @param data Data frame or tibble with processed MEA data
#' @param variables Character vector. Variables to include in PCA (if NULL, uses all numeric)
#' @param scale Logical. Whether to scale variables before PCA (default: TRUE)
#' @param center Logical. Whether to center variables before PCA (default: TRUE)
#' @param ... Additional PCA parameters
#'
#' @return List containing PCA results (scores, loadings, variance explained, etc.)
#'
#' @examples
#' \dontrun{
#' # Perform PCA analysis
#' pca_results <- perform_mea_pca(processed_data)
#' 
#' # Access PC scores
#' scores <- pca_results$scores
#' 
#' # Access loadings
#' loadings <- pca_results$loadings
#' }
#'
#' @export
perform_mea_pca <- function(data, variables = NULL, scale = TRUE, center = TRUE, ...) {
  # Template function - implement your PCA logic here
  stop("This is a template function. Please implement the PCA analysis logic.")
}

#' Enhanced PCA Analysis for MEA Data
#' 
#' This function performs Principal Component Analysis (PCA) on MEA data with extensive
#' flexibility for data input sources, parameter configuration, and output options.
#' It handles missing values, applies variance filtering, creates visualization plots,
#' and provides comprehensive results suitable for downstream analysis.
#'
#' @param normalized_data Data.frame. Pre-loaded MEA data in long format (default: NULL)
#' @param data_path Character. Path to Excel file containing MEA data (default: NULL)
#' @param config List. Configuration object with analysis parameters (default: NULL)
#' @param processing_result List. Output from process_mea_flexible function (default: NULL)
#' @param min_var Numeric. Minimum variance threshold for variable inclusion (default: 0.01)
#' @param impute Logical. Whether to impute missing values (default: TRUE)
#' @param scale_data Logical. Whether to scale variables before PCA (default: TRUE)
#' @param n_components Integer. Number of principal components to extract (default: 2)
#' @param variance_cutoff Numeric. Cumulative variance percentage threshold (default: 70)
#' @param grouping_variables Character vector. Variables for sample grouping (default: c("Treatment", "Genotype"))
#' @param sample_id_components Character vector. Variables to create unique sample IDs (default: c("Well", "Timepoint", "Treatment", "Genotype"))
#' @param value_column Character. Name of column containing values for PCA (default: "Normalized_Value")
#' @param variable_column Character. Name of column containing variable names (default: "Variable")
#' @param timepoint_column Character. Name of column containing timepoint information (default: "Timepoint")
#' @param verbose Logical. Whether to print detailed progress messages (default: TRUE)
#'
#' @return A list containing:
#'   - pca_result: Complete prcomp() object with PCA results
#'   - plot_data: Data frame ready for plotting with PC scores and metadata
#'   - variance_explained: Vector of variance explained by each component
#'   - cumulative_variance: Vector of cumulative variance explained
#'   - elbow_plot: ggplot2 object showing variance explained by components
#'   - elbow_data: Data frame underlying the elbow plot
#'   - components_needed: Number of components needed for various variance thresholds
#'   - count_summary: Summary of sample counts by groups (if applicable)
#'   - data_info: Information about data processing steps
#'   - config_used: Configuration parameters actually used
#'   - processing_source: Source of input data ("processing_result", "excel_file", or "direct_data")
#'
#' @details
#' The function provides three flexible data input methods:
#' 1. **processing_result**: Direct output from process_mea_flexible function
#' 2. **data_path**: Path to Excel file with normalized_data sheet
#' 3. **normalized_data**: Pre-loaded data frame in long format
#' 
#' Data processing includes:
#' - Automatic detection of available columns
#' - Flexible sample ID creation from specified components
#' - Missing value imputation (mean, median, or zero)
#' - Variance-based variable filtering
#' - Automatic scaling option
#' - Creation of elbow plot for component selection
#' 
#' The function handles common MEA data challenges:
#' - Missing timepoint or treatment information
#' - Inconsistent column naming
#' - Mixed data types and missing values
#' - Variable numbers of experiments and conditions
#'
#' @examples
#' \dontrun{
#' # Method 1: Use output from MEA processing function
#' mea_result <- process_mea_flexible("/path/to/data", baseline_timepoint = "baseline")
#' pca_result <- pca_analysis_enhanced(processing_result = mea_result)
#' 
#' # Method 2: Load from saved Excel file
#' pca_result <- pca_analysis_enhanced(data_path = "/path/to/processed_data.xlsx")
#' 
#' # Method 3: Use pre-loaded data with custom parameters
#' pca_result <- pca_analysis_enhanced(
#'   normalized_data = my_data,
#'   min_var = 0.05,
#'   n_components = 5,
#'   variance_cutoff = 80,
#'   grouping_variables = c("Treatment", "Genotype", "Batch")
#' )
#' 
#' # Access results
#' plot(pca_result$elbow_plot)
#' head(pca_result$plot_data)
#' print(pca_result$components_needed)
#' }
#'
#' @export
pca_analysis_enhanced <- function(normalized_data = NULL, 
                                  data_path = NULL, 
                                  config = NULL,
                                  processing_result = NULL,
                                  min_var = NULL,
                                  impute = NULL,
                                  scale_data = NULL,
                                  n_components = NULL,
                                  variance_cutoff = NULL,
                                  grouping_variables = NULL,
                                  sample_id_components = NULL,
                                  value_column = "Normalized_Value",
                                  variable_column = "Variable",
                                  timepoint_column = "Timepoint",
                                  verbose = TRUE) {
  
  if (verbose) cat("=== ENHANCED PCA ANALYSIS ===\n")
  
  # ============================================================================
  # FLEXIBLE DATA INPUT HANDLING
  # ============================================================================
  
  # Option 1: Use full processing result from process_mea_flexible
  if (!is.null(processing_result)) {
    if (verbose) cat("Using data from processing result...\n")
    
    # Check if normalized data exists, if not use raw data
    if (is.null(processing_result$normalized_data)) {
      if (verbose) cat("No baseline normalization found, proceeding with raw values\n")
      
      # Try to use raw_data instead
      if (!is.null(processing_result$raw_data)) {
        normalized_data <- processing_result$raw_data
        # Update value_column to match raw data structure
        if (!"Normalized_Value" %in% names(normalized_data) && "Value" %in% names(normalized_data)) {
          value_column <- "Value"
        }
      } else if (!is.null(processing_result$processed_data)) {
        normalized_data <- processing_result$processed_data
        # Update value_column to match processed data structure
        if (!"Normalized_Value" %in% names(normalized_data) && "Value" %in% names(normalized_data)) {
          value_column <- "Value"
        }
      } else {
        stop("Processing result does not contain usable data (neither normalized_data, raw_data, nor processed_data).")
      }
    } else {
      normalized_data <- processing_result$normalized_data
    }
    
    # Extract parameters from processing result if not explicitly provided
    if (is.null(grouping_variables) && !is.null(processing_result$processing_params$grouping_variables)) {
      grouping_variables <- processing_result$processing_params$grouping_variables
    }
    
    if (verbose) {
      cat("Data source: Processing result\n")
      if (!is.null(processing_result$processing_timestamp)) {
        cat("Processing timestamp:", as.character(processing_result$processing_timestamp), "\n")
      }
      if (!is.null(processing_result$processing_params$selected_experiments)) {
        cat("Experiments processed:", paste(processing_result$processing_params$selected_experiments, collapse = ", "), "\n")
      }
      cat("Value column used:", value_column, "\n")
    }
  }
  
  # Option 2: Load from Excel file path
  else if (!is.null(data_path)) {
    if (verbose) cat("Loading data from Excel file:", data_path, "\n")
    if (!file.exists(data_path)) stop("Data file not found: ", data_path)
    
    # Try to load normalized data first, then raw data if not available
    tryCatch({
      normalized_data <- readxl::read_excel(data_path, sheet = "normalized_data")
    }, error = function(e) {
      if (verbose) cat("No normalized_data sheet found, trying raw_data sheet...\n")
      tryCatch({
        normalized_data <- readxl::read_excel(data_path, sheet = "raw_data")
        # Update value column for raw data
        if (!"Normalized_Value" %in% names(normalized_data) && "Value" %in% names(normalized_data)) {
          value_column <- "Value"
        }
      }, error = function(e2) {
        stop("Could not load data from either 'normalized_data' or 'raw_data' sheets in: ", data_path)
      })
    })
  }
  
  # Option 3: Use provided normalized_data directly
  else if (!is.null(normalized_data)) {
    if (verbose) cat("Using provided normalized_data directly...\n")
  }
  
  else {
    stop("Must provide one of: processing_result, data_path, or normalized_data")
  }
  
  # Validate input data structure
  if (is.null(normalized_data) || nrow(normalized_data) == 0) {
    stop("Input data is empty or NULL")
  }
  
  # Validate required columns
  if (!value_column %in% names(normalized_data)) {
    stop("Missing required column: '", value_column, "'. Available columns: ", paste(names(normalized_data), collapse = ", "))
  }
  if (!variable_column %in% names(normalized_data)) {
    stop("Missing required column: '", variable_column, "'. Available columns: ", paste(names(normalized_data), collapse = ", "))
  }
  
  # ============================================================================
  # CONFIG SYSTEM INTEGRATION WITH SMART DEFAULTS
  # ============================================================================
  
  # Define %||% operator for NULL coalescing if not already defined
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Set config-based defaults if config provided
  if (!is.null(config)) {
    if (is.null(min_var)) min_var <- config$min_variance_threshold %||% 0.01
    if (is.null(impute)) impute <- config$impute_missing %||% TRUE
    if (is.null(scale_data)) scale_data <- config$scale_pca_data %||% TRUE
    if (is.null(n_components)) n_components <- config$pca_components %||% 2
    if (is.null(variance_cutoff)) variance_cutoff <- config$pca_variance_cutoff %||% 70
    if (is.null(grouping_variables)) grouping_variables <- config$grouping_variables %||% c("Treatment", "Genotype")
    if (is.null(sample_id_components)) sample_id_components <- config$sample_id_components %||% c("Well", "Timepoint", "Treatment", "Genotype")
  } else {
    # Fallback defaults when no config provided
    if (is.null(min_var)) min_var <- 0.01
    if (is.null(impute)) impute <- TRUE
    if (is.null(scale_data)) scale_data <- TRUE
    if (is.null(n_components)) n_components <- 2
    if (is.null(variance_cutoff)) variance_cutoff <- 70
    if (is.null(grouping_variables)) grouping_variables <- c("Treatment", "Genotype")
    if (is.null(sample_id_components)) sample_id_components <- c("Well", "Timepoint", "Treatment", "Genotype")
  }
  
  if (verbose) {
    cat("PCA Parameters:\n")
    cat("  Min variance threshold:", min_var, "\n")
    cat("  Impute missing values:", impute, "\n")
    cat("  Scale data:", scale_data, "\n")
    cat("  Components to extract:", n_components, "\n")
    cat("  Variance cutoff:", variance_cutoff, "%\n")
    cat("  Grouping variables:", paste(grouping_variables, collapse = ", "), "\n")
  }
  
  # Load required packages
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # ============================================================================
  # FLEXIBLE DATA PREPARATION
  # ============================================================================
  
  # Check which columns are actually present in the data
  available_columns <- names(normalized_data)
  
  # Filter sample_id_components to only include available columns
  valid_sample_id_components <- sample_id_components[sample_id_components %in% available_columns]
  
  if (length(valid_sample_id_components) == 0) {
    warning("None of the specified sample_id_components are available in data. Using all available columns except value and variable columns.")
    valid_sample_id_components <- setdiff(available_columns, c(value_column, variable_column))
  }
  
  # Filter grouping_variables to only include available columns
  valid_grouping_variables <- grouping_variables[grouping_variables %in% available_columns]
  
  if (length(valid_grouping_variables) == 0) {
    warning("None of the specified grouping_variables are available. PCA will proceed without grouping.")
    valid_grouping_variables <- character(0)
  }
  
  if (verbose) {
    cat("Available columns:", paste(available_columns, collapse = ", "), "\n")
    cat("Valid sample ID components:", paste(valid_sample_id_components, collapse = ", "), "\n")
    cat("Valid grouping variables:", paste(valid_grouping_variables, collapse = ", "), "\n")
  }
  
  # Build Sample ID using available components
  sample_id_sep <- "_"
  if (!is.null(config) && !is.null(config$sample_id_separator)) {
    sample_id_sep <- config$sample_id_separator
  }
  
  # Clean and prepare data
  normalized_data_clean <- normalized_data %>%
    # Clean timepoint data if column exists
    {if (timepoint_column %in% names(.)) mutate(., Timepoint_clean = as.character(.data[[timepoint_column]])) else .} %>%
    # Create Sample ID using available components
    mutate(
      Sample = do.call(paste, c(select(., all_of(valid_sample_id_components)), sep = sample_id_sep))
    )
  
  # Count raw observations by available grouping variables
  if (length(valid_grouping_variables) > 0 && timepoint_column %in% names(normalized_data_clean)) {
    count_vars <- c("Timepoint_clean", valid_grouping_variables)
    count_vars <- count_vars[count_vars %in% names(normalized_data_clean)]
    
    raw_counts <- normalized_data_clean %>%
      group_by(across(all_of(count_vars))) %>%
      summarise(raw_n = n(), .groups = 'drop') %>%
      rename_with(~"Timepoint", "Timepoint_clean")
  } else {
    raw_counts <- data.frame(raw_n = nrow(normalized_data_clean))
  }
  
  # Extract metadata using available grouping variables
  metadata_vars <- c("Sample", valid_grouping_variables)
  if (timepoint_column %in% names(normalized_data_clean)) {
    metadata_vars <- c(metadata_vars, "Timepoint_clean")
  }
  
  # Filter to only include available columns
  metadata_vars <- metadata_vars[metadata_vars %in% names(normalized_data_clean)]
  
  sample_metadata <- normalized_data_clean %>%
    select(all_of(metadata_vars)) %>%
    distinct()
  
  # Rename timepoint column for consistency
  if ("Timepoint_clean" %in% names(sample_metadata)) {
    sample_metadata <- sample_metadata %>% rename(Timepoint = Timepoint_clean)
  }
  
  # ============================================================================
  # PIVOT TO WIDE FORMAT FOR PCA
  # ============================================================================
  
  if (verbose) cat("Reshaping data for PCA...\n")
  
  # Pivot to wide format - aggregate duplicates by taking mean
  pca_input <- normalized_data_clean %>%
    group_by(Sample, .data[[variable_column]]) %>%
    summarise(Value = mean(.data[[value_column]], na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = all_of(variable_column), values_from = Value)
  
  # Build matrix for PCA
  row_names <- pca_input$Sample
  mat <- as.matrix(pca_input %>% select(-Sample))
  rownames(mat) <- row_names
  
  if (ncol(mat) < 2) {
    stop("PCA matrix has fewer than 2 variables. Cannot perform PCA. Variables available: ", ncol(mat))
  }
  
  if (verbose) cat("Initial PCA matrix:", nrow(mat), "samples x", ncol(mat), "variables\n")
  
  # ============================================================================
  # HANDLE MISSING VALUES
  # ============================================================================
  
  if (impute) {
    impute_method <- "mean"
    if (!is.null(config) && !is.null(config$imputation_method)) {
      impute_method <- config$imputation_method
    }
    
    if (verbose) cat("Imputing missing values using method:", impute_method, "\n")
    
    for (j in seq_len(ncol(mat))) {
      na_idx <- is.na(mat[, j])
      if (any(na_idx)) {
        if (impute_method == "mean") {
          mat[na_idx, j] <- mean(mat[, j], na.rm = TRUE)
        } else if (impute_method == "median") {
          mat[na_idx, j] <- median(mat[, j], na.rm = TRUE)
        } else if (impute_method == "zero") {
          mat[na_idx, j] <- 0
        }
      }
    }
  } else {
    complete_idx <- complete.cases(mat)
    mat <- mat[complete_idx, , drop = FALSE]
    sample_metadata <- sample_metadata %>% filter(Sample %in% rownames(mat))
    if (verbose) cat("Removed", sum(!complete_idx), "samples with missing values\n")
  }
  
  # ============================================================================
  # VARIANCE FILTERING
  # ============================================================================
  
  # Filter variables based on variance threshold
  vars <- apply(mat, 2, var, na.rm = TRUE)
  keep <- vars > min_var & !is.na(vars)
  
  if (sum(keep) < 2) {
    stop("Not enough variables with sufficient variance for PCA after filtering (threshold: ", min_var, "). Variables with sufficient variance: ", sum(keep))
  }
  
  mat <- mat[, keep, drop = FALSE]
  
  if (verbose) {
    cat("Variables removed due to low variance:", sum(!keep), "\n")
    cat("Final PCA matrix:", nrow(mat), "samples x", ncol(mat), "variables\n")
  }
  
  # ============================================================================
  # PERFORM PCA
  # ============================================================================
  
  if (verbose) cat("Performing PCA...\n")
  pca_result <- prcomp(mat, scale. = scale_data, center = TRUE)
  
  # Calculate variance explained
  variance_summary <- summary(pca_result)
  individual_var <- variance_summary$importance[2, ] * 100  # Proportion of Variance
  cumulative_var <- variance_summary$importance[3, ] * 100  # Cumulative Proportion
  
  # Find components needed for different cutoffs
  cutoffs <- c(70, 80, 90, variance_cutoff)
  cutoffs <- unique(sort(cutoffs))
  
  components_needed <- sapply(cutoffs, function(cutoff) {
    idx <- which(cumulative_var >= cutoff)[1]
    if (is.na(idx)) length(cumulative_var) else idx
  })
  names(components_needed) <- paste0(cutoffs, "%")
  
  # ============================================================================
  # CREATE ELBOW PLOT
  # ============================================================================
  
  n_plot_components <- min(20, length(individual_var))
  elbow_data <- data.frame(
    PC = 1:n_plot_components,
    Individual_Variance = individual_var[1:n_plot_components],
    Cumulative_Variance = cumulative_var[1:n_plot_components]
  )
  
  elbow_plot <- ggplot(elbow_data, aes(x = PC)) +
    geom_line(aes(y = Individual_Variance), color = "blue", size = 1) +
    geom_point(aes(y = Individual_Variance), color = "blue", size = 2) +
    geom_line(aes(y = Cumulative_Variance), color = "red", size = 1) +
    geom_point(aes(y = Cumulative_Variance), color = "red", size = 2) +
    geom_hline(yintercept = 70, linetype = "dotted", color = "gray50", alpha = 0.7) +
    geom_hline(yintercept = 80, linetype = "dotted", color = "gray50", alpha = 0.7) +
    geom_hline(yintercept = 90, linetype = "dotted", color = "gray50", alpha = 0.7) +
    geom_hline(yintercept = variance_cutoff, linetype = "dashed", color = "black", size = 1) +
    annotate("text", x = max(elbow_data$PC) * 0.8, y = 72, label = "70%", hjust = 0) +
    annotate("text", x = max(elbow_data$PC) * 0.8, y = 82, label = "80%", hjust = 0) +
    annotate("text", x = max(elbow_data$PC) * 0.8, y = 92, label = "90%", hjust = 0) +
    annotate("text", x = max(elbow_data$PC) * 0.8, y = variance_cutoff + 2, 
             label = paste0("Selected: ", variance_cutoff, "%"), hjust = 0, fontface = "bold") +
    scale_x_continuous(breaks = seq(1, n_plot_components, by = max(1, floor(n_plot_components/10)))) +
    labs(
      title = "PCA Elbow Plot: Variance Explained by Principal Components",
      subtitle = paste("Blue: Individual variance | Red: Cumulative variance | Selected cutoff:", variance_cutoff, "%"),
      x = "Principal Component",
      y = "Variance Explained (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    )
  
  # ============================================================================
  # PREPARE PLOT DATA
  # ============================================================================
  
  max_components <- min(n_components, ncol(pca_result$x))
  pc_names <- paste0("PC", 1:max_components)
  
  scores <- as.data.frame(pca_result$x[, 1:max_components, drop = FALSE])
  names(scores) <- pc_names
  scores$Sample <- rownames(scores)
  
  plot_data <- left_join(scores, sample_metadata, by = 'Sample')
  
  # Calculate variance explained for selected components
  variance_explained <- individual_var[1:max_components]
  names(variance_explained) <- pc_names
  
  # ============================================================================
  # COUNT SUMMARY (if grouping variables available)
  # ============================================================================
  
  filtered_counts <- NULL
  count_summary <- NULL
  
  if (length(valid_grouping_variables) > 0 && "Timepoint" %in% names(plot_data)) {
    count_vars <- c("Timepoint", valid_grouping_variables)
    count_vars <- count_vars[count_vars %in% names(plot_data)]
    
    filtered_counts <- plot_data %>%
      group_by(across(all_of(count_vars))) %>%
      summarise(filtered_n = n(), .groups = 'drop')
    
    if (nrow(raw_counts) > 1) {  # Only if we have meaningful raw counts
      count_summary <- full_join(raw_counts, filtered_counts, by = count_vars) %>%
        mutate(
          raw_n = ifelse(is.na(raw_n), 0, raw_n),
          filtered_n = ifelse(is.na(filtered_n), 0, filtered_n)
        )
    } else {
      count_summary <- filtered_counts
    }
  }
  
  # ============================================================================
  # VERBOSE OUTPUT
  # ============================================================================
  
  if (verbose) {
    cat("\n=== PCA RESULTS SUMMARY ===\n")
    cat("Final samples in PCA:", nrow(plot_data), "\n")
    
    if (length(valid_grouping_variables) > 0) {
      for (var in valid_grouping_variables) {
        if (var %in% names(plot_data)) {
          unique_vals <- unique(plot_data[[var]])
          unique_vals <- unique_vals[!is.na(unique_vals)]  # Remove NAs for display
          cat(paste0(var, ": "), length(unique_vals), "unique values -", paste(unique_vals, collapse = ", "), "\n")
        }
      }
    }
    
    cat("Components extracted:", max_components, "\n")
    cat("Variance explained by PC1:", round(variance_explained[1], 2), "%\n")
    if (max_components > 1) cat("Variance explained by PC2:", round(variance_explained[2], 2), "%\n")
    
    cat("\n--- Component Recommendations ---\n")
    for (i in seq_along(components_needed)) {
      cutoff_name <- names(components_needed)[i]
      n_comp <- components_needed[i]
      if (!is.na(n_comp) && n_comp <= length(cumulative_var)) {
        cat("For", cutoff_name, "cumulative variance: use", n_comp, "components\n")
      } else {
        cat("For", cutoff_name, "cumulative variance: more components needed than available\n")
      }
    }
  }
  
  # ============================================================================
  # RETURN COMPREHENSIVE RESULTS
  # ============================================================================
  
  return(list(
    pca_result = pca_result,
    plot_data = plot_data,
    variance_explained = variance_explained,
    cumulative_variance = cumulative_var,
    elbow_plot = elbow_plot,
    elbow_data = elbow_data,
    components_needed = components_needed,
    count_summary = count_summary,
    data_info = list(
      original_samples = nrow(normalized_data),
      pca_samples = nrow(plot_data),
      original_variables = if(exists("mat")) ncol(pca_input) - 1 else NA,
      pca_variables = ncol(mat),
      variables_removed_variance = sum(!keep),
      valid_grouping_variables = valid_grouping_variables,
      valid_sample_id_components = valid_sample_id_components
    ),
    config_used = list(
      min_var = min_var,
      impute = impute,
      scale_data = scale_data,
      n_components = max_components,
      variance_cutoff = variance_cutoff,
      imputation_method = if(impute) (if(!is.null(config) && !is.null(config$imputation_method)) config$imputation_method else "mean") else "none",
      grouping_variables = grouping_variables,
      sample_id_components = sample_id_components,
      value_column = value_column,
      variable_column = variable_column,
      timepoint_column = timepoint_column
    ),
    processing_source = if(!is.null(processing_result)) "processing_result" else if(!is.null(data_path)) "excel_file" else "direct_data"
  ))
}

# Add more PCA analysis functions here as you share them
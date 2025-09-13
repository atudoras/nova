# data_handling.R
# Functions for discovering, loading, and processing MEA data

#' Discover MEA Data Structure
#' 
#' This function scans a directory containing MEA (Multi-Electrode Array) experiment 
#' folders and analyzes the structure of CSV files to identify experiments, timepoints,
#' measured variables, treatments, and genotypes. It provides a comprehensive overview
#' of the data organization without loading all files into memory.
#'
#' @param main_dir Character. Path to the main directory containing experiment folders
#' @param experiment_pattern Character. Regex pattern to identify experiment directories (default: "MEA\\d+")
#' @param file_pattern Character. Regex pattern to identify data files (default: "\\.csv$")
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   - experiments: List of experiment info (directories, files, timepoints, metadata)
#'   - all_timepoints: Vector of all unique timepoints found across experiments
#'   - all_variables: Vector of all unique measured variables
#'   - potential_baselines: Timepoints that might serve as baseline conditions
#'   - experiment_count: Total number of experiments found
#'   - discovery_timestamp: When the analysis was performed
#'
#' @details 
#' The function expects MEA CSV files with standard format:
#' - Row 121: Well identifiers (A1, A2, B1, etc.)
#' - Row 122: Treatment conditions
#' - Row 123: Genotype information  
#' - Row 124: Exclusion flags
#' - Rows 125-168: Variable names and measurements
#'
#' @examples
#' \dontrun{
#' # Discover structure of MEA data
#' structure <- discover_mea_structure("/path/to/mea/data")
#' 
#' # View discovered experiments
#' names(structure$experiments)
#' 
#' # Check available timepoints
#' structure$all_timepoints
#' }
#'
#' @export
discover_mea_structure <- function(main_dir, 
                                   experiment_pattern = "MEA\\d+",
                                   file_pattern = "\\.csv$",
                                   verbose = TRUE) {
  
  # Input validation
  if (!dir.exists(main_dir)) {
    stop("Directory does not exist: ", main_dir)
  }
  
  if (verbose) cat("=== DISCOVERING MEA DATA STRUCTURE ===\n")
  
  # Find experiment directories
  all_dirs <- list.dirs(main_dir, recursive = FALSE)
  experiment_dirs <- all_dirs[grepl(experiment_pattern, basename(all_dirs))]
  
  if (length(experiment_dirs) == 0) {
    stop("No experiment directories found matching pattern: ", experiment_pattern)
  }
  
  # Initialize storage for results
  experiment_info <- list()
  all_variables <- character()
  all_timepoints <- character()
  file_structure_patterns <- list()
  
  # Process each experiment directory
  for (exp_dir in experiment_dirs) {
    exp_name <- basename(exp_dir)
    if (verbose) cat("\n--- Analyzing experiment:", exp_name, "---\n")
    
    # Find CSV files
    csv_files <- list.files(exp_dir, pattern = file_pattern, full.names = TRUE)
    
    if (length(csv_files) == 0) {
      if (verbose) cat("  No CSV files found in", exp_name, "\n")
      next
    }
    
    # Extract timepoints/conditions from filenames
    file_basenames <- tools::file_path_sans_ext(basename(csv_files))
    
    # Try different patterns to extract timepoints
    timepoint_patterns <- c(
      paste0(exp_name, "_(.+)$"),  # Standard: MEA012_1h
      paste0(exp_name, "([a-z]?)_(.+)$"),  # With letter: MEA016a_DIV2
      "^.+_(.+)$",  # General: anything_timepoint
      "^(.+)_[^_]+\\.csv$"  # Fallback pattern
    )
    
    extracted_timepoints <- character()
    for (pattern in timepoint_patterns) {
      matches <- stringr::str_match(file_basenames, pattern)
      if (any(!is.na(matches[,2]))) {
        if (ncol(matches) >= 3 && any(!is.na(matches[,3]))) {
          # Pattern with experiment + letter + timepoint
          extracted_timepoints <- matches[,3][!is.na(matches[,3])]
        } else {
          # Pattern with just timepoint
          extracted_timepoints <- matches[,2][!is.na(matches[,2])]
        }
        break
      }
    }
    
    if (length(extracted_timepoints) == 0) {
      extracted_timepoints <- file_basenames
      if (verbose) cat("  Warning: Could not extract timepoints, using full filenames\n")
    }
    
    # Sample one file to analyze structure
    sample_file <- csv_files[1]
    if (verbose) cat("  Sampling file:", basename(sample_file), "\n")
    
    tryCatch({
      raw_data <- readr::read_csv(sample_file, col_names = FALSE, show_col_types = FALSE)
      
      if (nrow(raw_data) < 124) {
        if (verbose) cat("  Warning: File has fewer than expected rows (", nrow(raw_data), ")\n")
        next
      }
      
      # Extract metadata from standard MEA format positions
      metadata_info <- list()
      
      if (nrow(raw_data) >= 124) {
        # Standard MEA file structure positions
        well_row <- 121      # Well identifiers (A1, A2, B1, etc.)
        treatment_row <- 122 # Treatment conditions
        genotype_row <- 123  # Genotype information
        exclude_row <- 124   # Exclusion flags
        
        # Extract and analyze metadata
        wells <- unlist(raw_data[well_row, -1])
        treatments <- unlist(raw_data[treatment_row, -1])
        genotypes <- unlist(raw_data[genotype_row, -1])
        
        # Find valid columns (non-empty wells)
        valid_cols <- which(!(is.na(wells) | wells == "" | wells == "NA"))
        
        if (length(valid_cols) > 0) {
          metadata_info$wells <- unique(wells[valid_cols])
          metadata_info$treatments <- unique(treatments[valid_cols][!is.na(treatments[valid_cols])])
          metadata_info$genotypes <- unique(genotypes[valid_cols][!is.na(genotypes[valid_cols])])
          metadata_info$n_wells <- length(valid_cols)
        }
      }
      
      # Extract variable names (features measured) from rows 125-168
      if (nrow(raw_data) >= 168) {
        variables <- unlist(raw_data[125:168, 1])
        variables <- variables[!is.na(variables) & variables != ""]
        metadata_info$variables <- variables
        metadata_info$n_variables <- length(variables)
      }
      
      # Store experiment information
      experiment_info[[exp_name]] <- list(
        directory = exp_dir,
        files = csv_files,
        timepoints = extracted_timepoints,
        file_count = length(csv_files),
        metadata = metadata_info
      )
      
      # Accumulate all timepoints and variables
      all_timepoints <- c(all_timepoints, extracted_timepoints)
      if (!is.null(metadata_info$variables)) {
        all_variables <- c(all_variables, metadata_info$variables)
      }
      
      # Print summary for this experiment
      if (verbose) {
        cat("  Files found:", length(csv_files), "\n")
        cat("  Timepoints:", paste(extracted_timepoints, collapse = ", "), "\n")
        if (!is.null(metadata_info$n_wells)) {
          cat("  Wells:", metadata_info$n_wells, "\n")
        }
        if (!is.null(metadata_info$treatments)) {
          cat("  Treatments:", paste(unique(metadata_info$treatments), collapse = ", "), "\n")
        }
        if (!is.null(metadata_info$genotypes)) {
          cat("  Genotypes:", paste(unique(metadata_info$genotypes), collapse = ", "), "\n")
        }
        if (!is.null(metadata_info$n_variables)) {
          cat("  Variables measured:", metadata_info$n_variables, "\n")
        }
      }
      
    }, error = function(e) {
      if (verbose) cat("  Error analyzing", basename(sample_file), ":", e$message, "\n")
    })
  }
  
  # Generate summary statistics
  unique_timepoints <- unique(all_timepoints)
  unique_variables <- unique(all_variables)
  
  # Detect potential baseline timepoints
  potential_baselines <- unique_timepoints[grepl("baseline|base|0min|0h|pre|control", 
                                                 unique_timepoints, ignore.case = TRUE)]
  
  # Create final structure summary
  structure_summary <- list(
    experiments = experiment_info,
    all_timepoints = unique_timepoints,
    all_variables = unique_variables,
    potential_baselines = potential_baselines,
    experiment_count = length(experiment_info),
    discovery_timestamp = Sys.time()
  )
  
  # Print overall summary
  if (verbose) {
    cat("\n=== DISCOVERY SUMMARY ===\n")
    cat("Experiments found:", length(experiment_info), "\n")
    cat("Experiment names:", paste(names(experiment_info), collapse = ", "), "\n")
    cat("Unique timepoints across all experiments:", paste(unique_timepoints, collapse = ", "), "\n")
    cat("Total unique timepoints:", length(unique_timepoints), "\n")
    cat("Potential baseline timepoints:", paste(potential_baselines, collapse = ", "), "\n")
    cat("Total variables measured:", length(unique_variables), "\n")
  }
  
  return(structure_summary)
}

#' Process MEA Data Flexibly
#' 
#' This function processes Multi-Electrode Array (MEA) data files by reading CSV files,
#' extracting measurements and metadata, applying filters, and optionally normalizing
#' to baseline conditions. It automatically excludes standard deviation variables and
#' handles exclusion flags to produce clean, analysis-ready datasets.
#'
#' @param main_dir Character. Path to the main directory containing experiment folders
#' @param selected_experiments Character vector. Experiment names to process (default: NULL = all)
#' @param selected_timepoints Character vector. Timepoints to include (default: NULL = all)  
#' @param grouping_variables Character vector. Metadata columns to include ("Treatment", "Genotype")
#' @param baseline_timepoint Character. Timepoint to use for normalization (default: NULL = no normalization)
#' @param unique_id_vars Character vector. Variables that uniquely identify observations for normalization
#' @param exclude_std_variables Logical. Whether to automatically exclude standard deviation variables (default: TRUE)
#' @param experiment_pattern Character. Regex pattern for experiment directories (default: "MEA\\d+")
#' @param verbose Logical. Whether to print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   - raw_data: Processed data in long format
#'   - normalized_data: Baseline-normalized data (if baseline_timepoint specified)
#'   - processing_params: List of parameters used for processing
#'   - output_path: Path to saved Excel file
#'   - experiment_name: Combined experiment identifier
#'
#' @details
#' The function automatically detects and excludes variables containing "Std", "std", or "STD"
#' in their names (e.g., "Number of Spikes - Std") while keeping average/mean variables
#' (e.g., "Number of Spikes - Avg"). Wells marked with "Ex" or "ex" in row 124 are excluded.
#' 
#' Data is exported to Excel format with separate sheets for raw and normalized data.
#' Normalization creates fold-change values relative to baseline timepoint.
#'
#' @examples
#' \dontrun{
#' # Process all experiments and timepoints
#' result <- process_mea_flexible("/path/to/mea/data")
#' 
#' # Process specific experiments with baseline normalization
#' result <- process_mea_flexible(
#'   main_dir = "/path/to/data",
#'   selected_experiments = c("MEA001", "MEA002"), 
#'   baseline_timepoint = "baseline"
#' )
#' 
#' # Access processed data
#' raw_data <- result$raw_data
#' normalized_data <- result$normalized_data
#' }
#' @export
process_mea_flexible <- function(main_dir,
                                 selected_experiments = NULL,
                                 selected_timepoints = NULL,
                                 grouping_variables = c("Treatment", "Genotype"),
                                 baseline_timepoint = NULL,
                                 unique_id_vars = c("Well", "Variable"),
                                 exclude_std_variables = TRUE,
                                 experiment_pattern = "MEA\\d+",
                                 timepoint_fusions = NULL,
                                 verbose = TRUE) {
  
  if (verbose) cat("=== PROCESSING MEA DATA WITH USER PARAMETERS ===\n")
  
  # ============================================================================
  # TIMEPOINT FUSION SETUP
  # ============================================================================
  
  # Validate and setup timepoint fusions
  fusion_map <- NULL
  if (!is.null(timepoint_fusions)) {
    if (verbose) cat("Setting up timepoint fusions...\n")
    
    # Convert to standardized format if needed
    if (is.list(timepoint_fusions[[1]])) {
      # Multiple fusion groups: list(list("1h30", c("1h30", "1h30min")), list("2h", c("2h", "120min")))
      fusion_groups <- timepoint_fusions
    } else {
      # Single fusion group: list("1h30", c("1h30", "1h30min"))
      fusion_groups <- list(timepoint_fusions)
    }
    
    # Create fusion mapping
    fusion_map <- list()
    for (fusion_group in fusion_groups) {
      if (length(fusion_group) != 2) {
        stop("Each timepoint fusion must be a list with 2 elements: target_name and vector of source names")
      }
      
      target_name <- fusion_group[[1]]
      source_names <- fusion_group[[2]]
      
      if (verbose) {
        cat("  Fusion rule:", paste(source_names, collapse = ", "), "->", target_name, "\n")
      }
      
      # Map each source name to target name
      for (source in source_names) {
        fusion_map[[source]] <- target_name
      }
    }
  }
  
  # Auto-discover structure if selections not provided
  if (is.null(selected_experiments) || is.null(selected_timepoints)) {
    if (verbose) cat("Auto-discovering data structure...\n")
    discovery <- discover_mea_structure(main_dir, experiment_pattern, verbose = FALSE)
    
    if (is.null(selected_experiments)) {
      selected_experiments <- names(discovery$experiments)
    }
    if (is.null(selected_timepoints)) {
      selected_timepoints <- discovery$all_timepoints
    }
  }
  
  # Apply fusion mapping to selected timepoints
  if (!is.null(fusion_map)) {
    original_timepoints <- selected_timepoints
    
    # Replace timepoints with their fused versions
    for (i in seq_along(selected_timepoints)) {
      if (selected_timepoints[i] %in% names(fusion_map)) {
        selected_timepoints[i] <- fusion_map[[selected_timepoints[i]]]
      }
    }
    
    # Remove duplicates after fusion
    selected_timepoints <- unique(selected_timepoints)
    
    if (verbose) {
      cat("Original timepoints:", paste(original_timepoints, collapse = ", "), "\n")
      cat("After fusion:", paste(selected_timepoints, collapse = ", "), "\n")
    }
  }
  
  # Print processing parameters
  if (verbose) {
    cat("Processing experiments:", paste(selected_experiments, collapse = ", "), "\n")
    cat("Including timepoints:", paste(selected_timepoints, collapse = ", "), "\n")
    cat("Grouping variables:", paste(grouping_variables, collapse = ", "), "\n")
    cat("Exclude std variables:", exclude_std_variables, "\n")
    if (!is.null(baseline_timepoint)) {
      cat("Baseline timepoint:", baseline_timepoint, "\n")
    }
  }
  
  all_data <- list()
  
  # ============================================================================
  # PROCESS EACH EXPERIMENT
  # ============================================================================
  
  # Process each experiment
  for (exp_name in selected_experiments) {
    exp_dir <- file.path(main_dir, exp_name)
    
    if (!dir.exists(exp_dir)) {
      if (verbose) cat("Warning: Experiment directory not found:", exp_dir, "\n")
      next
    }
    
    csv_files <- list.files(exp_dir, pattern = "\\.csv$", full.names = TRUE)
    
    # Process each CSV file in the experiment
    for (file_path in csv_files) {
      filename <- basename(file_path)
      
      # Extract timepoint from filename using multiple patterns
      file_basename <- tools::file_path_sans_ext(filename)
      timepoint <- NA
      patterns <- c(
        paste0(exp_name, "_(.+)$"),           # MEA001_timepoint
        paste0(exp_name, "[a-z]?_(.+)$"),    # MEA001a_timepoint  
        "^.+_(.+)$"                          # anything_timepoint
      )
      
      for (pattern in patterns) {
        match <- stringr::str_match(file_basename, pattern)
        if (!is.na(match[1,2])) {
          timepoint <- match[1,2]
          break
        }
      }
      
      if (is.na(timepoint)) {
        timepoint <- file_basename  # Fallback to full filename
      }
      
      # Apply fusion mapping to extracted timepoint
      original_timepoint <- timepoint
      if (!is.null(fusion_map) && timepoint %in% names(fusion_map)) {
        timepoint <- fusion_map[[timepoint]]
        if (verbose) cat("  Fusing timepoint:", original_timepoint, "->", timepoint, "\n")
      }
      
      # Skip files with timepoints not in selection (after fusion)
      if (!timepoint %in% selected_timepoints) {
        if (verbose) cat("Skipping", filename, "- timepoint", timepoint, "not selected\n")
        next
      }
      
      if (verbose) cat("Processing:", filename, "(timepoint:", timepoint, ")\n")
      
      # Read and process individual file
      tryCatch({
        raw <- readr::read_csv(file_path, col_names = FALSE, show_col_types = FALSE)
        
        if (nrow(raw) < 168) {
          warning("File ", filename, " has insufficient rows (", nrow(raw), " < 168)")
          next
        }
        
        # Extract metadata from standard MEA positions
        well_ids   <- unlist(raw[121, -1])  # Well identifiers
        treatments <- unlist(raw[122, -1])  # Treatment conditions  
        genotypes  <- unlist(raw[123, -1])  # Genotype information
        exclude    <- unlist(raw[124, -1])  # Exclusion flags
        
        # Identify valid wells (non-empty well IDs)
        valid_cols <- which(!(is.na(well_ids) | well_ids == "" | well_ids == "NA"))
        if (length(valid_cols) == 0) {
          warning("No valid wells in ", filename)
          next
        }
        
        # Extract variable names and measurement matrix
        variable_names <- unlist(raw[125:168, 1])
        values_matrix <- raw[125:168, -1]
        
        # Remove empty/NA variable names
        valid_vars <- which(!is.na(variable_names) & variable_names != "")
        variable_names <- variable_names[valid_vars]
        values_matrix <- values_matrix[valid_vars, , drop = FALSE]
        
        # Automatically exclude standard deviation variables if requested
        if (exclude_std_variables) {
          std_pattern <- "std|Std|STD"
          std_vars <- grepl(std_pattern, variable_names, ignore.case = TRUE)
          
          if (any(std_vars)) {
            excluded_var_names <- variable_names[std_vars]
            if (verbose) {
              cat("  Excluding", sum(std_vars), "std variables:", 
                  paste(head(excluded_var_names, 3), collapse = ", "), 
                  ifelse(length(excluded_var_names) > 3, "...", ""), "\n")
            }
            
            # Keep only non-std variables
            keep_vars <- !std_vars
            variable_names <- variable_names[keep_vars]
            values_matrix <- values_matrix[keep_vars, , drop = FALSE]
          }
        }
        
        # Subset to valid wells only
        values_matrix <- values_matrix[, valid_cols, drop = FALSE]
        
        # Convert to long format
        data_wide <- as.data.frame(values_matrix)
        clean_well_ids <- make.names(well_ids[valid_cols], unique = TRUE)
        colnames(data_wide) <- clean_well_ids
        data_wide$Variable <- variable_names
        
        data_long <- tidyr::pivot_longer(data_wide, 
                                         cols = -Variable, 
                                         names_to = "Well", 
                                         values_to = "Value")
        
        # Create metadata dataframe
        meta_df <- data.frame(Well = clean_well_ids, stringsAsFactors = FALSE)
        
        # Add requested grouping variables
        if ("Treatment" %in% grouping_variables) {
          meta_df$Treatment <- treatments[valid_cols]
        }
        if ("Genotype" %in% grouping_variables) {
          meta_df$Genotype <- genotypes[valid_cols]
        }
        
        # Add exclusion information temporarily
        meta_df$Exclude <- exclude[valid_cols]
        
        # Combine data with metadata and apply exclusions
        data_full <- dplyr::left_join(data_long, meta_df, by = "Well") %>%
          dplyr::filter(is.na(Exclude) | trimws(as.character(Exclude)) == "" | 
                          tolower(trimws(as.character(Exclude))) != "ex") %>%
          dplyr::mutate(
            Experiment = exp_name,
            Timepoint = timepoint,  # Use the fused timepoint name
            Original_Timepoint = original_timepoint,  # Keep original for reference
            Value = as.numeric(Value)
          ) %>%
          dplyr::select(-Exclude)  # Remove exclusion column from final data
        
        # Store processed data with fused timepoint key
        data_key <- paste(exp_name, timepoint, sep = "_")
        
        # If this fused timepoint already exists, combine the data
        if (data_key %in% names(all_data)) {
          if (verbose) cat("  Combining with existing data for timepoint:", timepoint, "\n")
          all_data[[data_key]] <- dplyr::bind_rows(all_data[[data_key]], data_full)
        } else {
          all_data[[data_key]] <- data_full
        }
        
        if (verbose) cat("  Processed", nrow(data_full), "observations\n")
        
      }, error = function(e) {
        warning("Error processing ", filename, ": ", e$message)
      })
    }
  }
  
  # Check if any data was processed
  if (length(all_data) == 0) {
    stop("No data was successfully processed")
  }
  
  # ============================================================================
  # COMBINE AND SUMMARIZE DATA
  # ============================================================================
  
  # Combine all processed data
  final_data <- dplyr::bind_rows(all_data) %>%
    dplyr::mutate_if(is.character, stringr::str_trim) %>%
    dplyr::filter(!is.na(Value))
  
  # Print summary statistics
  if (verbose) {
    cat("\n=== COMBINED DATA SUMMARY ===\n")
    cat("Total observations:", nrow(final_data), "\n")
    cat("Experiments:", paste(unique(final_data$Experiment), collapse = ", "), "\n")
    cat("Timepoints:", paste(unique(final_data$Timepoint), collapse = ", "), "\n")
    cat("Variables:", length(unique(final_data$Variable)), "\n")
    cat("Wells:", length(unique(final_data$Well)), "\n")
    
    for (var in grouping_variables) {
      if (var %in% colnames(final_data)) {
        unique_vals <- unique(final_data[[var]])
        cat(paste0(var, ": "), paste(unique_vals, collapse = ", "), "\n")
      }
    }
    
    # Show fusion summary if fusions were applied
    if (!is.null(fusion_map)) {
      cat("\n--- Timepoint Fusion Summary ---\n")
      fusion_summary <- final_data %>%
        dplyr::group_by(Timepoint, Original_Timepoint) %>%
        dplyr::summarise(n_obs = n(), .groups = "drop") %>%
        dplyr::arrange(Timepoint)
      
      for (i in 1:nrow(fusion_summary)) {
        row <- fusion_summary[i, ]
        if (row$Timepoint != row$Original_Timepoint) {
          cat("  ", row$Original_Timepoint, "->", row$Timepoint, "(", row$n_obs, "observations )\n")
        }
      }
    }
  }
  
  # ============================================================================
  # BASELINE NORMALIZATION
  # ============================================================================
  
  # Perform baseline normalization if requested
  normalized_data <- NULL
  if (!is.null(baseline_timepoint)) {
    if (verbose) cat("\n--- Normalizing to baseline ---\n")
    
    if (!baseline_timepoint %in% unique(final_data$Timepoint)) {
      warning("Baseline timepoint '", baseline_timepoint, "' not found in data. Available: ", 
              paste(unique(final_data$Timepoint), collapse = ", "))
    } else {
      # Determine variables for baseline matching
      baseline_vars <- unique_id_vars[unique_id_vars %in% colnames(final_data)]
      missing_vars <- unique_id_vars[!unique_id_vars %in% colnames(final_data)]
      
      if (length(missing_vars) > 0) {
        warning("Some unique_id_vars not found in data: ", paste(missing_vars, collapse = ", "))
      }
      
      # Add grouping variables to baseline identification
      for (var in grouping_variables) {
        if (var %in% colnames(final_data) && !var %in% baseline_vars) {
          baseline_vars <- c(baseline_vars, var)
        }
      }
      
      # Create baseline reference data
      baseline_df <- final_data %>%
        dplyr::filter(Timepoint == baseline_timepoint) %>%
        dplyr::select(!!!rlang::syms(baseline_vars), Baseline_Value = Value)
      
      # Join with baseline and calculate normalized values
      normalized_data <- final_data %>%
        dplyr::left_join(baseline_df, by = baseline_vars) %>%
        dplyr::mutate(
          Normalized_Value = ifelse(is.na(Baseline_Value) | Baseline_Value == 0, 
                                    NA, 
                                    Value / Baseline_Value)
        ) %>%
        dplyr::select(-Baseline_Value)
      
      if (verbose) {
        n_normalized <- sum(!is.na(normalized_data$Normalized_Value))
        cat("Successfully normalized", n_normalized, "observations\n")
      }
    }
  }
  
  # ============================================================================
  # SAVE RESULTS
  # ============================================================================
  
  # Generate output filename and save results
  experiment_name <- paste(selected_experiments, collapse = "_")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_filename <- paste0("MEA_processed_", experiment_name, "_", timestamp, ".xlsx")
  output_path <- file.path(main_dir, output_filename)
  
  # Prepare data for Excel export
  export_data <- list("raw_data" = final_data)
  if (!is.null(normalized_data)) {
    export_data$normalized_data <- normalized_data
  }
  
  # Add fusion mapping info if used
  if (!is.null(fusion_map)) {
    fusion_info <- data.frame(
      Source_Timepoint = names(fusion_map),
      Target_Timepoint = unlist(fusion_map),
      stringsAsFactors = FALSE
    )
    export_data$timepoint_fusions <- fusion_info
  }
  
  # Export to Excel file
  writexl::write_xlsx(export_data, path = output_path)
  
  if (verbose) cat("\n✅ Data saved to:", output_path, "\n")
  
  # ============================================================================
  # RETURN COMPREHENSIVE RESULTS
  # ============================================================================
  
  # Return comprehensive results
  return(list(
    raw_data = final_data,
    normalized_data = normalized_data,
    processing_params = list(
      selected_experiments = selected_experiments,
      selected_timepoints = selected_timepoints,
      grouping_variables = grouping_variables,
      baseline_timepoint = baseline_timepoint,
      unique_id_vars = unique_id_vars,
      exclude_std_variables = exclude_std_variables,
      timepoint_fusions = timepoint_fusions,
      fusion_map = fusion_map
    ),
    output_path = output_path,
    experiment_name = experiment_name
  ))
}



# Add more data handling functions here as you share them
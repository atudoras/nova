# NOVA
*Neural Output Visualization and Analysis*

A comprehensive R toolkit for analyzing and visualizing neural data outputs, including Principal Component Analysis (PCA) trajectory plotting, Multi-Electrode Array (MEA) heatmap generation, and variable importance analysis. Provides publication-ready visualizations with flexible customization options for neuroscience research applications.

## Installation

```r
# Install from GitHub (replace 'yourusername' with your GitHub username)
devtools::install_github("atudoras/NOVA")

# Or install from CRAN (when available)
install.packages("NOVA")
```

## Usage

```r
library(NOVA)

# 1. Discover your MEA data structure
discovery_results <- discover_mea_structure("path/to/your/MEA_data")

# 2. Process MEA data with flexible options
processed_data <- process_mea_flexible(
  main_dir = "path/to/your/MEA_data",
  selected_timepoints = c("baseline", "0min", "15min", "30min", "1h", "2h"),
  grouping_variables = c("Experiment", "Treatment", "Genotype", "Well"),
  baseline_timepoint = "baseline"
)

# 3. Perform enhanced PCA analysis
pca_results <- pca_analysis_enhanced(processing_result = processed_data)

# 4. Generate comprehensive PCA plots
pca_plots <- pca_plots_enhanced(
  pca_output = pca_results,
  color_variable = "Treatment",
  shape_variable = "Genotype"
)

# 5. Create trajectory analysis
trajectories <- plot_pca_trajectories_general(
  pca_results,
  timepoint_order = c("baseline", "0min", "15min", "30min", "1h", "2h"),
  trajectory_grouping = c("Genotype", "Treatment")
)

# 6. Generate MEA heatmaps
heatmaps <- create_mea_heatmaps_enhanced(
  processing_result = processed_data,
  grouping_columns = c("Genotype", "Treatment")
)
```

# MEA Package Directory Structure Guide

## Overview
The MEA package expects a specific directory structure to automatically discover and process your experimental data. Here's how to organize your files:

## Required Directory Structure

```
main_directory/
├── MEA001/
│   ├── MEA001_baseline.csv
│   ├── MEA001_1h.csv
│   ├── MEA001_3h.csv
│   └── MEA001_24h.csv
├── MEA002/
    ├── MEA002_baseline.csv
    ├── MEA002_1h.csv
    └── MEA002_6h.csv
```

## Key Requirements

### 1. Main Directory
- Create a parent folder that contains all your MEA experiments
- This is the `main_dir` parameter you'll pass to the function

### 2. Experiment Folders
- **Naming Convention**: Each experiment folder must follow the pattern `MEA` + numbers
  - Examples: `MEA001`, `MEA012`, `MEA123`
  - Optional letter suffix is supported: `MEA016a`, `MEA025b`
- **Pattern**: The function looks for folders matching `MEA\\d+` (MEA followed by digits)

### 3. CSV Files Within Each Experiment
- **File Format**: All data files must be CSV format (`.csv` extension)
- **Naming Pattern**: Files should follow one of these patterns:
  - `MEAExperimentNumber_timepoint.csv` (e.g., `MEA001_1h.csv`)
  - `MEAExperimentNumber[letter]_timepoint.csv` (e.g., `MEA016a_DIV2.csv`)

### 4. Timepoint Naming Examples
The function can extract various timepoint formats:
- **Time-based**: `baseline`, `1h`, `3h`, `24h`, `0min`
- **Days in vitro**: `DIV2`, `DIV7`, `DIV14`
- **Custom**: Any descriptive name that follows the underscore

## CSV File Structure Requirements

Each CSV file must contain:
- Minimum 124 rows for basic processing (more if you have additional metadata)
- Row 121: Well identifiers (A1, A2, B1, etc.) - This is fixed
- Row 122: First metadata variable (e.g., Treatment, Genotype, Dose, etc.)
- Row 123: Second metadata variable
- Row 124: Third metadata variable
- Additional rows: You can add more metadata variables in subsequent rows
Variable names start after metadata: If you have metadata in rows 122-125, then variables would start in row 126

## Tips for Success

1. **Consistent Naming**: Keep experiment folder names consistent with the MEA + number pattern
2. **Clear Timepoints**: Use descriptive timepoint names in your CSV filenames
3. **File Completeness**: Ensure CSV files have the required metadata rows (121-168)
4. **No Spaces**: Avoid spaces in folder and file names; use underscores instead
5. **Backup Data**: Always keep backups of your original data files

## Troubleshooting

- If no experiments are found, check that folder names match `MEA` + numbers
- If timepoints aren't detected, verify filename follows `experiment_timepoint.csv` pattern
- If files can't be read, ensure they're valid CSV files with proper structure
- Enable `verbose = TRUE` to see detailed discovery process and identify issues

## Detailed Example

See an example of a complete analysis workflow in the folder "Example".

- **Flexible data discovery**: Automatically detect MEA data structure
- **Multi-experiment processing**: Handle multiple experiments and timepoints
- **Enhanced PCA analysis**: Publication-ready PCA plots with trajectory analysis
- **Variable importance analysis**: Identify key neural variables
- **MEA heatmap generation**: Comprehensive electrode data visualization
- **Batch effect correction**: Built-in normalization options

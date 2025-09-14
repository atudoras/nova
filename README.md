# NOVA
*Neural Output Visualization and Analysis*

A comprehensive R toolkit for analyzing and visualizing neural data outputs, including Principal Component Analysis (PCA) trajectory plotting, Multi-Electrode Array (MEA) heatmap generation, and variable importance analysis. Provides publication-ready visualizations with flexible customization options for neuroscience research applications.

## Installation

```r
# Install from GitHub (replace 'yourusername' with your GitHub username)
devtools::install_github("yourusername/NOVA")

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

## Quick Example

See an example of a complete analysis workflow in the folder "Example".

- **Flexible data discovery**: Automatically detect MEA data structure
- **Multi-experiment processing**: Handle multiple experiments and timepoints
- **Enhanced PCA analysis**: Publication-ready PCA plots with trajectory analysis
- **Variable importance analysis**: Identify key neural variables
- **MEA heatmap generation**: Comprehensive electrode data visualization
- **Batch effect correction**: Built-in normalization options

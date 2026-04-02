# NOVA 0.1.6

## Example Dataset Update

* Replaced private example data (MEA022b/MEA022c, Mavs KO genotypes) with
  publicly available WT neuron data (MEA012, MEA013)
* Example dataset now uses WT neurons with neuronal agonist treatments:
  PBS, KCl, DHPG, Gabazine (MEA012) and PBS, AMPA, KA, DHPG, Gabazine (MEA013)
* Updated all example scripts (nova_quickstart.R, 01_compute.R, 02_plot.R,
  mea-analysis-example.Rmd) to reflect WT-only dataset structure
* GENOTYPE_COLUMN now defaults to NULL in quickstart for WT-only datasets
* Removed compiled HTML and figure outputs generated from private dataset

---

# NOVA 0.1.1

## CRAN Resubmission Fixes

* Removed all commented code from examples and function bodies
* Fixed DESCRIPTION to spell out Multi-Electrode Array (MEA) without quotes
* Added newlines at end of all R source files
* Fixed parse errors in documentation examples

## Initial CRAN Release

This is the first release of NOVA (Neuroactivity Omics Visualization and Analysis), a comprehensive toolkit for analyzing and visualizing Multi-Electrode Array (MEA) data.

### Main Features

* **Data Processing**: Flexible MEA data processing with automatic structure discovery
  - `discover_mea_structure()`: Automatically detects data structure
  - `process_mea_flexible()`: Processes MEA data with quality filtering options
  - `handle_missing_values()`: Multiple strategies for handling missing data

* **Principal Component Analysis**: 
  - `pca_analysis_enhanced()`: Comprehensive PCA with visualization
  - `analyze_pca_variable_importance_general()`: Variable importance analysis
  - `plot_pca_trajectories_general()`: Trajectory visualization across conditions

* **Heatmap Visualization**:
  - `create_mea_heatmaps_enhanced()`: Publication-ready heatmaps with multiple scaling options
  - Support for hierarchical clustering and custom annotations

* **Utility Functions**: Helper functions for data manipulation, scaling, and color schemes

### Documentation

* Comprehensive function documentation with examples
* Vignettes demonstrating typical workflows
* Support for various MEA data formats
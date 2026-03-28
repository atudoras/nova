# =============================================================================
# NOVA — Per-Metric Plot Module
# =============================================================================
# PURPOSE:
#   Plots a single MEA metric (e.g. Mean Firing Rate, Burst Frequency) across
#   groups and timepoints. Choose between bar, box, violin, or line plot.
#
# REQUIRES:
#   Run 01_compute.R first — this module loads the saved nova_results.rds file.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to wherever you saved nova_results.rds
#   2. Set METRIC to the exact column name of the metric you want to plot
#      (run  names(results$processed$processed_data)  to see all available metrics)
#   3. Edit the SETTINGS section below
#   4. Run the whole script  →  figure saves to OUTPUT_DIR automatically
# =============================================================================

library(NOVA)

# =============================================================================
# PATHS
# =============================================================================

RESULTS_PATH <- "nova_results.rds"      # path to your saved results file
OUTPUT_DIR   <- "nova_output/metrics"   # folder where the figure will be saved

# =============================================================================
# SETTINGS — edit these to customise the plot
# =============================================================================

# --- Which metric to plot ---
# Must match a column name in your data exactly.
# To see all available metrics, run:
#   results <- readRDS("nova_results.rds")
#   names(results$processed$processed_data)
METRIC <- "MeanFiringRate"    # e.g. "MeanFiringRate", "BurstFrequency", "NetworkBurstRate"

# --- Plot type ---
# Choose one: "bar", "box", "violin", "line"
#   bar    = mean + error bars  (good for a clean summary)
#   box    = median + IQR       (good for showing distribution)
#   violin = full distribution  (good when you have many wells)
#   line   = connected means over time  (good for time course)
PLOT_TYPE <- "violin"

# --- Grouping ---
# X-axis variable — what goes on the horizontal axis.
GROUP_BY <- "Treatment"     # e.g. "Treatment", "Genotype"

# --- Facet panels ---
# Split into separate panels by a variable. NULL = one combined plot.
FACET_BY <- "Timepoint"    # e.g. "Timepoint", "Genotype",  or NULL

# --- Error bars (for bar plots) ---
# "sem"  = standard error of the mean  (recommended for most cases)
# "sd"   = standard deviation
# "ci95" = 95% confidence interval
ERROR_TYPE <- "sem"

# --- Filter what to show ---
# Set to NULL to include everything, or list specific values.
SHOW_TREATMENTS <- NULL    # e.g. c("PBS", "KA")      or NULL for all
SHOW_GENOTYPES  <- NULL    # e.g. c("WT")              or NULL for all
SHOW_TIMEPOINTS <- NULL    # e.g. c("baseline", "1h", "2h")  or NULL for all

# --- Colors ---
# NULL = automatic palette.
# To use custom colors provide a named vector matching your GROUP_BY values:
#   CUSTOM_COLORS <- c("PBS" = "#6B7280", "KA" = "#EF4444")
CUSTOM_COLORS <- NULL

# --- Save settings ---
FIGURE_WIDTH  <- 10     # inches (wider helps when there are many facets)
FIGURE_HEIGHT <- 6      # inches
DPI           <- 300

# =============================================================================
# LOAD RESULTS  (no need to edit below this line)
# =============================================================================

if (!file.exists(RESULTS_PATH)) {
  stop("Results file not found: ", RESULTS_PATH,
       "\nRun 01_compute.R first to generate it.")
}

message("Loading results from: ", RESULTS_PATH)
results <- readRDS(RESULTS_PATH)

data <- results$processed$processed_data

# Check the requested metric exists
if (!METRIC %in% names(data)) {
  available <- names(data)[!names(data) %in% c("Treatment","Genotype","Timepoint","Well","Experiment")]
  stop("Metric '", METRIC, "' not found in data.\n",
       "Available metrics:\n  ", paste(available, collapse = "\n  "))
}

# =============================================================================
# GENERATE FIGURE
# =============================================================================

message("Generating ", PLOT_TYPE, " plot for: ", METRIC)

p <- plot_mea_metric(
  data              = data,
  metric            = METRIC,
  plot_type         = PLOT_TYPE,
  group_by          = GROUP_BY,
  facet_by          = FACET_BY,
  error_type        = ERROR_TYPE,
  filter_treatments = SHOW_TREATMENTS,
  filter_genotypes  = SHOW_GENOTYPES,
  filter_timepoints = SHOW_TIMEPOINTS,
  color_palette     = CUSTOM_COLORS,
  save_plots        = FALSE
)

# =============================================================================
# SAVE
# =============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Sanitise metric name for filename (remove spaces and special characters)
metric_clean <- gsub("[^A-Za-z0-9_]", "_", METRIC)
out_file <- file.path(OUTPUT_DIR,
  paste0(metric_clean, "_", PLOT_TYPE, ".pdf"))

pdf(out_file, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
print(p)
dev.off()

message("Saved: ", out_file)

# =============================================================================
# NOVA — PCA Scatter Plot Module
# =============================================================================
# PURPOSE:
#   Generates PCA scatter plots — each point is one well/sample, coloured and
#   shaped by the variables you choose. Optionally adds confidence ellipses.
#
# REQUIRES:
#   Run 01_compute.R first — this module loads the saved nova_results.rds file.
#
# HOW TO USE:
#   1. Set RESULTS_PATH to wherever you saved nova_results.rds
#   2. Edit the SETTINGS section below
#   3. Run the whole script  →  figure saves to OUTPUT_DIR automatically
# =============================================================================

library(NOVA)

# =============================================================================
# PATHS
# =============================================================================

RESULTS_PATH <- "nova_results.rds"    # path to your saved results file
OUTPUT_DIR   <- "nova_output/pca"     # folder where the figure will be saved

# =============================================================================
# SETTINGS — edit these to customise the plot
# =============================================================================

# --- Which PCs to show ---
PC_X <- 1    # horizontal axis  (1 = PC1)
PC_Y <- 2    # vertical axis    (2 = PC2)

# --- Color and shape ---
# These should match column names in your data (e.g. "Treatment", "Genotype")
COLOR_BY <- "Treatment"   # which variable controls point color
SHAPE_BY <- NULL          # which variable controls point shape  (NULL = no shape distinction)

# --- Facet panels ---
# Split into separate panels by a variable. NULL = one combined plot.
FACET_BY <- NULL          # e.g. "Genotype"  or  NULL

# --- Confidence ellipses ---
# Draw a shaded region showing the spread of each group.
SHOW_ELLIPSES <- TRUE     # TRUE or FALSE

# --- Filter what to show ---
# Set to NULL to include everything, or list specific values.
SHOW_TREATMENTS <- NULL   # e.g. c("PBS", "KA")      or NULL for all
SHOW_GENOTYPES  <- NULL   # e.g. c("WT")              or NULL for all
SHOW_TIMEPOINTS <- NULL   # e.g. c("baseline", "2h")  or NULL for all (one timepoint recommended)

# --- Colors ---
# NULL = automatic palette.
# To use custom colors, provide a named vector matching your COLOR_BY values:
#   CUSTOM_COLORS <- c("PBS" = "#6B7280", "KA" = "#EF4444")
CUSTOM_COLORS <- NULL

# --- Point appearance ---
POINT_SIZE  <- 3      # dot size
POINT_ALPHA <- 0.8    # transparency (0 = invisible, 1 = solid)

# --- Save settings ---
FIGURE_WIDTH  <- 8      # inches
FIGURE_HEIGHT <- 7      # inches
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

pca_data  <- results$pca
plot_data <- pca_data$plot_data

# Apply filters
if (!is.null(SHOW_TREATMENTS) && "Treatment" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Treatment %in% SHOW_TREATMENTS, ]

if (!is.null(SHOW_GENOTYPES) && "Genotype" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Genotype %in% SHOW_GENOTYPES, ]

if (!is.null(SHOW_TIMEPOINTS) && "Timepoint" %in% names(plot_data))
  plot_data <- plot_data[plot_data$Timepoint %in% SHOW_TIMEPOINTS, ]

pca_filtered           <- pca_data
pca_filtered$plot_data <- plot_data

# =============================================================================
# GENERATE FIGURE
# =============================================================================

message("Generating PCA scatter plot...")

p <- pca_plots_enhanced(
  pca_output      = pca_filtered,
  color_variable  = COLOR_BY,
  shape_variable  = SHAPE_BY,
  pannels_var     = FACET_BY,
  components      = c(PC_X, PC_Y),
  show_ellipses   = SHOW_ELLIPSES,
  point_size      = POINT_SIZE,
  point_alpha     = POINT_ALPHA,
  color_palette   = CUSTOM_COLORS,
  save_plots      = FALSE
)

# =============================================================================
# SAVE
# =============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(OUTPUT_DIR,
  paste0("pca_PC", PC_X, "_vs_PC", PC_Y, ".pdf"))

pdf(out_file, width = FIGURE_WIDTH, height = FIGURE_HEIGHT)
if (is.list(p) && !inherits(p, "gg")) lapply(p, print) else print(p)
dev.off()

message("Saved: ", out_file)

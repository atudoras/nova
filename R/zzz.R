# R/zzz.R
#' NOVA: package-level imports and global variables
#'
#' Internal imports used across the package and a list of non-standard
#' evaluation (NSE) column names suppressed for R CMD check.
#'
#' @name NOVA-package
#' @keywords internal
#'
#' @importFrom grDevices dev.off rainbow colorRampPalette
#' @importFrom stats as.formula complete.cases cor prcomp sd var approx
#' @importFrom stats median mad predict smooth.spline
#' @importFrom utils head write.csv
#' @importFrom rlang sym
#' @importFrom dplyr rename_with
#' @importFrom tibble rownames_to_column
NULL

## Global variables used in NSE pipelines (silence R CMD check NOTE)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", ":=", "PC", "PC_X_Loading", "PC_Y_Loading",
      "Combined_Importance", "Variable", "PC_X_Abs_Loading",
      "PC_Y_Abs_Loading", "Loading", "value", "sample_id", "avg_value",
      "group_combination", "group_id", "well_id", "mean_x", "mean_y",
      "time_rank", "avg_x", "avg_y", "se_x", "se_y", "x", "y", "xend",
      "yend", "first_x", "first_y", "last_x", "last_y", "label_text",
      "last_label", "first_label", "tfrac", "coid", "n_obs", "completeness",
      "raw_n", "filtered_n", "Baseline_Value", "Original_Timepoint",
      "Timepoint_clean", "Sample", "Value"
    )
  )
}

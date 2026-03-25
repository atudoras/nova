library(testthat)
library(NOVA)

# Helper: minimal long-format data frame mimicking process_mea_flexible output
make_raw_data <- function() {
  data.frame(
    Well       = rep(c("A1", "B1"), each = 4),
    Treatment  = rep(c("Control", "Drug"), each = 4),
    Genotype   = "WT",
    Timepoint  = "baseline",
    Variable   = rep(c("Mean Firing Rate (Hz)", "Burst Rate (Hz)"), 4),
    Value      = runif(8, 0, 5),
    stringsAsFactors = FALSE
  )
}

test_that("create_mea_heatmaps_enhanced accepts raw data frame with Value column", {
  df <- make_raw_data()
  expect_no_error(
    create_mea_heatmaps_enhanced(
      data         = df,
      value_column = "Value",
      verbose      = FALSE,
      save_plots   = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced use_raw=TRUE auto-switches value_column", {
  pr <- list(
    raw_data        = make_raw_data(),
    normalized_data = NULL,
    config_used     = NULL
  )
  expect_no_error(
    create_mea_heatmaps_enhanced(
      processing_result = pr,
      use_raw           = TRUE,
      verbose           = FALSE,
      save_plots        = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced title says 'Raw' not 'Normalized' when use_raw=TRUE", {
  pr <- list(
    raw_data        = make_raw_data(),
    normalized_data = NULL,
    config_used     = NULL
  )
  result <- create_mea_heatmaps_enhanced(
    processing_result = pr,
    use_raw           = TRUE,
    verbose           = FALSE,
    save_plots        = FALSE
  )
  expect_false(isTRUE(result$metadata$value_column == "Normalized_Value"))
})

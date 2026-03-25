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

test_that("create_mea_heatmaps_enhanced filter_treatments subsets data", {
  df <- data.frame(
    Well      = rep(c("A1","B1"), each = 2),
    Treatment = rep(c("PBS","KA"), each = 2),
    Genotype  = "WT",
    Timepoint = "baseline",
    Variable  = rep(c("Firing Rate","Burst Rate"), 2),
    Value     = runif(8),
    stringsAsFactors = FALSE
  )
  expect_no_error(
    create_mea_heatmaps_enhanced(
      data                = df,
      value_column        = "Value",
      filter_treatments   = "PBS",
      verbose             = FALSE,
      save_plots          = FALSE
    )
  )
})

test_that("create_mea_heatmaps_enhanced split_by returns one result per level", {
  df <- data.frame(
    Well      = rep(c("A1","A2","B1","B2"), each = 2),
    Treatment = rep(c("PBS","PBS","KA","KA"), each = 2),
    Genotype  = rep(c("WT","KO","WT","KO"), each = 2),
    Timepoint = "baseline",
    Variable  = rep(c("Firing Rate","Burst Rate"), 4),
    Value     = runif(8),
    stringsAsFactors = FALSE
  )
  result <- create_mea_heatmaps_enhanced(
    data         = df,
    value_column = "Value",
    split_by     = "Genotype",
    verbose      = FALSE,
    save_plots   = FALSE
  )
  expect_true("split_results" %in% names(result))
  expect_equal(length(result$split_results), 2)
  expect_true(all(c("WT","KO") %in% names(result$split_results)))
})

test_that("split_by = 'combination' creates a combination_result with pheatmap", {
  set.seed(42)
  df <- data.frame(
    Well      = rep(c("A1","A2","B1","B2"), each = 2),
    Treatment = rep(c("PBS","PBS","KA","KA"), each = 2),
    Genotype  = rep(c("WT","KO","WT","KO"), each = 2),
    Timepoint = "baseline",
    Variable  = rep(c("Firing Rate","Burst Rate"), 4),
    Value     = runif(8),
    stringsAsFactors = FALSE
  )
  result <- create_mea_heatmaps_enhanced(
    data         = df,
    value_column = "Value",
    split_by     = "combination",
    verbose      = FALSE,
    save_plots   = FALSE
  )
  expect_true("combination_result" %in% names(result))
  expect_s3_class(result$combination_result$heatmap, "pheatmap")
  expect_true(is.matrix(result$combination_result$data))
  expect_true(is.data.frame(result$combination_result$annotation))
})

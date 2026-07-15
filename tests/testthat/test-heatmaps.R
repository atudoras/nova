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

# Two plates that reuse well IDs. The combination heatmap keys its rows on the
# well, so this is where a Well-only key either merges two plates' wells or --
# when the plates disagree about the well's treatment -- fails outright.
make_two_plate_raw <- function(same_treatment = TRUE) {
  d <- expand.grid(Timepoint  = c("baseline", "1h"),
                   Well       = c("A1", "A2"),
                   Experiment = c("MEA001", "MEA002"),
                   Variable   = c("Mean Firing Rate (Hz)", "Burst Rate (Hz)",
                                  "Number of Spikes", "Synchrony Index"),
                   stringsAsFactors = FALSE)
  d$Treatment <- if (same_treatment) "Drug" else
    ifelse(d$Experiment == "MEA001", "Drug", "Control")
  d$Genotype <- "WT"
  d$Normalized_Value <- seq_len(nrow(d)) / 10
  d
}

test_that("combination heatmap keys rows on the well's plate-qualified identity", {
  # Regression: rows keyed on Well alone silently averaged MEA001_A1 with
  # MEA002_A1 into a single row.
  res <- create_mea_heatmaps_enhanced(
    data = make_two_plate_raw(), grouping_columns = c("Treatment", "Genotype"),
    split_by = "combination", save_plots = FALSE, verbose = FALSE)
  rn <- rownames(res$combination_result$data)
  expect_equal(length(rn), 4L)   # 2 plates x 2 wells, not 2 pooled wells
  expect_setequal(rn, c("MEA001_A1", "MEA001_A2", "MEA002_A1", "MEA002_A2"))
})

test_that("combination heatmap survives a well whose treatment differs by plate", {
  # Regression: this errored with "duplicate 'row.names' are not allowed",
  # because distinct(Well, Treatment, Genotype) produced two rows for A1.
  expect_no_error(
    res <- create_mea_heatmaps_enhanced(
      data = make_two_plate_raw(same_treatment = FALSE),
      grouping_columns = c("Treatment", "Genotype"),
      split_by = "combination", save_plots = FALSE, verbose = FALSE)
  )
  expect_equal(nrow(res$combination_result$annotation), 4L)
})

test_that("sample_id_columns actually keys the combination heatmap", {
  # It was documented as identifying samples while keying nothing. Dropping
  # Experiment must visibly pool the plates back together.
  res <- create_mea_heatmaps_enhanced(
    data = make_two_plate_raw(), grouping_columns = c("Treatment", "Genotype"),
    sample_id_columns = "Well",
    split_by = "combination", save_plots = FALSE, verbose = FALSE)
  expect_setequal(rownames(res$combination_result$data), c("A1", "A2"))
})

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

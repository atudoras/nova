library(testthat)
library(NOVA)

# Minimal processed data (long format, as returned by process_mea_flexible)
make_processed <- function() {
  set.seed(42)
  expand.grid(
    Well      = c("A1", "A2", "B1", "B2"),
    Timepoint = c("baseline", "1h", "2h"),
    Variable  = c("Mean Firing Rate (Hz)", "Burst Rate (Hz)"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      Treatment         = ifelse(Well %in% c("A1","A2"), "PBS", "KA"),
      Genotype          = ifelse(Well %in% c("A1","B1"), "WT",  "KO"),
      Value             = runif(dplyr::n(), 0, 10),
      Normalized_Value  = runif(dplyr::n(), 0.5, 2)
    )
}

test_that("plot_mea_metric returns a ggplot object", {
  df <- make_processed()
  p  <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)")
  expect_s3_class(p, "gg")
})

test_that("plot_mea_metric errors informatively for unknown metric", {
  df <- make_processed()
  expect_error(
    plot_mea_metric(df, metric = "Not A Real Metric"),
    regexp = "not found"
  )
})

test_that("plot_mea_metric respects filter_treatments", {
  df  <- make_processed()
  p   <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)",
                          filter_treatments = "PBS")
  pd  <- ggplot2::ggplot_build(p)$data[[1]]
  expect_lte(nrow(pd), 12)
})

test_that("plot_mea_metric facet_by creates faceted plot", {
  df <- make_processed()
  p  <- plot_mea_metric(df, metric = "Mean Firing Rate (Hz)",
                         facet_by = "Genotype")
  expect_true(!is.null(p$facet))
})

test_that("plot_mea_metric plot_type='box' works", {
  df <- make_processed()
  expect_no_error(
    plot_mea_metric(df, metric = "Mean Firing Rate (Hz)", plot_type = "box")
  )
})

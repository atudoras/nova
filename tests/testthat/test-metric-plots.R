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

# ── ggplot2 currency ─────────────────────────────────────────────────────────
# Regression: the package emitted five distinct deprecation warnings on every
# run (`size` in element_line/element_rect and on line geoms, geom_errorbarh,
# and a labs() entry for an aesthetic the plot never mapped). Warning noise that
# users learn to scroll past is how a real warning gets missed.

test_that("plot_mea_metric labels only the aesthetic it maps", {
  d <- data.frame(
    Variable = "Mean Firing Rate (Hz)",
    Well = rep(c("A1", "A2", "A3", "A4"), 4),
    Experiment = "MEA1",
    Treatment = rep(c("ctrl", "drug"), each = 8),
    Timepoint = rep(c("baseline", "1h"), each = 4),
    Value = c(1, 1.2, 0.9, 1.1, 2, 2.2, 1.9, 2.1,
              1, 1.1, 1.0, 0.9, 3, 3.2, 2.9, 3.1),
    stringsAsFactors = FALSE
  )
  for (ty in c("bar", "box", "violin", "line")) {
    p <- plot_mea_metric(d, metric = "Mean Firing Rate (Hz)", group_by = "Treatment",
                         value_column = "Value", plot_type = ty)
    expect_no_warning(ggplot2::ggplot_build(p))
  }
})

test_that("no deprecated ggplot2 spelling survives in the plotting source", {
  # `size` for lines and for rect/line theme elements became `linewidth` in 3.4.0;
  # geom_errorbarh() became geom_errorbar(orientation = "y") in 4.0.0. Checked at
  # source level so a reintroduction is caught even in a path no test renders.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package source not available (installed check)")
  code <- unlist(lapply(list.files(r_dir, pattern = "[.]R$", full.names = TRUE), readLines))
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)   # prose, not code
  expect_length(grep("geom_errorbarh\\s*\\(", code), 0)
  expect_length(grep("element_(line|rect)\\([^)]*\\bsize *=", code), 0)
  expect_length(grep("(geom_line|geom_path|geom_segment)\\([^)]*\\bsize *=", code), 0)
})

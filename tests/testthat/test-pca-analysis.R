# tests/testthat/test-pca-analysis.R
test_that("perform_mea_pca redirects to pca_analysis_enhanced with helpful message", {
  expect_error(
    perform_mea_pca(data.frame(), variables = NULL),
    regexp = "pca_analysis_enhanced"
  )
})

test_that("null_coalesce is not redefined inside pca_analysis_enhanced body", {
  fn_body <- deparse(body(pca_analysis_enhanced))
  local_def <- any(grepl("null_coalesce\\s*<-\\s*function", fn_body))
  expect_false(local_def,
    info = "null_coalesce should not be locally redefined inside pca_analysis_enhanced")
})

test_that("pca_analysis_enhanced loads from raw_data sheet when normalized_data missing", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Build minimal long-format data
  fake_data <- data.frame(
    Variable = rep(paste0("V", 1:5), 2),
    Sample   = rep(c("S1", "S2"), each = 5),
    Value    = rnorm(10),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(raw_data = fake_data), path = tmp)

  # Should NOT throw a scoping-related error
  expect_no_error({
    result <- pca_analysis_enhanced(
      data_path       = tmp,
      value_column    = "Value",
      variable_column = "Variable",
      verbose         = FALSE
    )
  })
  unlink(tmp)
})

# ── sample identity ───────────────────────────────────────────────────────────
# Regression: sample_id_components omitted Experiment, so the same well ID on
# two plates produced one Sample and the two observations were averaged into a
# single PCA point without warning.

make_two_plate_long <- function() {
  grid <- expand.grid(Timepoint = c("baseline", "1h"), Well = c("A1", "A2"),
                      Experiment = c("MEA001", "MEA002"),
                      Variable = paste0("V", 1:4),
                      stringsAsFactors = FALSE)
  grid$Treatment <- "drug"
  grid$Normalized_Value <- seq_len(nrow(grid)) / 10
  grid
}

test_that("wells sharing an ID across plates stay distinct samples", {
  d <- make_two_plate_long()
  pca <- pca_analysis_enhanced(normalized_data = d, grouping_variables = "Treatment",
                               verbose = FALSE)
  # 2 wells x 2 timepoints x 2 plates = 8 distinct samples, not 4.
  expect_equal(nrow(pca$plot_data), 8L)
  expect_equal(length(unique(pca$plot_data$Sample)), 8L)
})

test_that("sample_id_components that collide warn instead of silently averaging", {
  d <- make_two_plate_long()
  expect_warning(
    pca_analysis_enhanced(normalized_data = d, grouping_variables = "Treatment",
                          sample_id_components = c("Well", "Timepoint"),  # Experiment omitted
                          verbose = FALSE),
    regexp = "averaged into one PCA point"
  )
})

# -- trajectory color_by tests ──────────────────────────────────────────────

make_mini_pca <- function() {
  plot_data <- data.frame(
    PC1        = c(-1, 0, 1, -0.8, 0.2, 1.2),
    PC2        = c(-1, 0, 1,  0.8,-0.2,-1.2),
    Timepoint  = rep(c("0min","30min","60min"), 2),
    Treatment  = rep(c("PBS","KA"), each = 3),
    Genotype   = rep(c("WT","KO"), each = 3),
    Experiment = rep(c("Exp1","Exp2"), each = 3),
    stringsAsFactors = FALSE
  )
  list(plot_data = plot_data)
}

test_that("plot_pca_trajectories_general accepts color_by = 'Treatment'", {
  skip_if_not_installed("ggrepel")
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment","Genotype"),
    color_by            = "Treatment",
    save_plots          = FALSE,
    verbose             = FALSE
  )
  expect_type(result, "list")
})

test_that("color_by = 'group' still works (backward compat)", {
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment","Genotype"),
    color_by            = "group",
    save_plots          = FALSE,
    verbose             = FALSE
  )
  expect_type(result, "list")
})

test_that("combined averaged plot subtitle contains timepoints", {
  result <- plot_pca_trajectories_general(
    pca_results         = make_mini_pca(),
    trajectory_grouping = c("Treatment","Genotype"),
    save_plots          = FALSE,
    verbose             = FALSE
  )
  comb_avg <- result$plots$combined_average
  subtitle  <- comb_avg$labels$subtitle
  expect_true(!is.null(subtitle) && nchar(subtitle) > 0)
})

# ── NULL aesthetics ───────────────────────────────────────────────────────────
# Regression: `!NULL %in% x` is logical(0), and `if (logical(0))` is an error, so
# opting out of an aesthetic crashed -- including the documented single-genotype
# case where Genotype is deliberately absent.

test_that("pca_plots_enhanced accepts NULL shape variables", {
  pca <- make_mini_pca()
  pca$pca_result <- prcomp(matrix(rnorm(24), nrow = 6))
  expect_no_error(
    pca_plots_enhanced(
      pca_output               = pca,
      grouping_variables       = "Treatment",
      color_variable           = "Treatment",
      shape_variable           = NULL,
      secondary_shape_variable = NULL,
      save_plots               = FALSE,
      verbose                  = FALSE
    )
  )
})

# tests/testthat/test-pca-analysis.R
test_that("perform_mea_pca redirects to pca_analysis_enhanced with helpful message", {
  expect_error(
    perform_mea_pca(data.frame(), variables = NULL),
    regexp = "pca_analysis_enhanced"
  )
})

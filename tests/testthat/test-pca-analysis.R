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

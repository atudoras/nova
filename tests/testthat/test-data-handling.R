# tests/testthat/test-data-handling.R
test_that("MEA file structure constants are defined and correct", {
  expect_equal(NOVA:::MEA_ROW_WELLS,      121L)
  expect_equal(NOVA:::MEA_ROW_TREATMENT,  122L)
  expect_equal(NOVA:::MEA_ROW_GENOTYPE,   123L)
  expect_equal(NOVA:::MEA_ROW_EXCLUDE,    124L)
  expect_equal(NOVA:::MEA_ROW_VARS_START, 125L)
  expect_equal(NOVA:::MEA_ROW_VARS_END,   168L)
})

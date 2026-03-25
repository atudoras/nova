# tests/testthat/test-data-handling.R
test_that("MEA file structure constants are defined and correct", {
  expect_equal(NOVA:::MEA_ROW_WELLS,      121L)
  expect_equal(NOVA:::MEA_ROW_TREATMENT,  122L)
  expect_equal(NOVA:::MEA_ROW_GENOTYPE,   123L)
  expect_equal(NOVA:::MEA_ROW_EXCLUDE,    124L)
  expect_equal(NOVA:::MEA_ROW_VARS_START, 125L)
  expect_equal(NOVA:::MEA_ROW_VARS_END,   168L)
})

# ── find_mea_metadata_row ──────────────────────────────────────────────────────

test_that("find_mea_metadata_row finds Treatment at standard row 122", {
  # Build a minimal fake CSV raw table: 170 rows x 3 cols
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[122, 1] <- "Treatment"
  raw[123, 1] <- "Genotype"
  raw[124, 1] <- "Exclude"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 122L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Genotype"),  123L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Exclude"),   124L)
})

test_that("find_mea_metadata_row finds Treatment when shifted to row 123", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[123, 1] <- "Treatment"
  raw[124, 1] <- "Genotype"
  raw[125, 1] <- "Exclude"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 123L)
})

test_that("find_mea_metadata_row falls back to constant when label absent", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)

  result <- NOVA:::find_mea_metadata_row(raw, "Treatment", fallback = NOVA:::MEA_ROW_TREATMENT)
  expect_equal(result, NOVA:::MEA_ROW_TREATMENT)  # 122L
})

test_that("find_mea_metadata_row is case-insensitive", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[122, 1] <- "treatment"   # lowercase

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment"), 122L)
})

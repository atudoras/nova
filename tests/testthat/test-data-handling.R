# tests/testthat/test-data-handling.R

# Write a minimal file in the Axion layout process_mea_flexible() expects:
# row 121 wells, 122 Treatment, 123 Genotype, 124 Exclude, 125+ one row per
# variable. Column 1 holds the row label, columns 2+ hold one well each.
# `values` is a [variable x well] matrix.
#
# The labels are the ones Axion actually writes -- qualified ("Well Averages",
# "Treatment/ID", "Exclude/Include"), not bare. A fixture using bare labels
# exercises a branch real exports never take.
write_fake_mea_csv <- function(path, wells, treatments, variables, values,
                               exclude = NULL, label_row_offset = 0L) {
  n <- length(wells)
  if (is.null(exclude)) exclude <- rep("", n)
  first <- 121L + label_row_offset
  raw <- matrix("", nrow = first + 3L + length(variables), ncol = n + 1)
  raw[first,      ] <- c("Well Averages",   wells)
  raw[first + 1L, ] <- c("Treatment/ID",    treatments)
  raw[first + 2L, ] <- c("Genotype",        rep("WT", n))
  raw[first + 3L, ] <- c("Exclude/Include", exclude)
  for (i in seq_along(variables)) {
    raw[first + 3L + i, ] <- c(variables[i], as.character(values[i, ]))
  }
  utils::write.table(raw, path, sep = ",", row.names = FALSE, col.names = FALSE,
                     na = "", quote = FALSE)
}

# Two plates that both contain well A1 on the same treatment, with deliberately
# different baselines, so any cross-plate leakage shows up in the arithmetic.
make_two_plate_dir <- function() {
  dir <- tempfile("mea_plates_")
  vars <- c("Mean Firing Rate (Hz)", "Number of Spikes")
  # "30min" is present on purpose: it contains the substring "0min", so it is the
  # label a pattern-matched baseline search wrongly reports as a candidate.
  specs <- list(
    list(exp = "MEA001", baseline = c( 10,  10), thirty = c( 15,  15), one_h = c( 20,  20)),
    list(exp = "MEA002", baseline = c(100, 100), thirty = c(200, 200), one_h = c(400, 400))
  )
  for (s in specs) {
    dir.create(file.path(dir, s$exp), recursive = TRUE)
    for (tp in c("baseline", "30min", "1h")) {
      vals <- switch(tp, baseline = s$baseline, "30min" = s$thirty, "1h" = s$one_h)
      write_fake_mea_csv(file.path(dir, s$exp, paste0(s$exp, "_", tp, ".csv")),
                         wells = "A1", treatments = "drug", variables = vars,
                         values = matrix(vals, nrow = 2, ncol = 1))
    }
  }
  dir
}

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

test_that("find_mea_metadata_row finds the qualified labels Axion actually writes", {
  # Regression: matching was exact, so of the four real labels only "Genotype"
  # was ever found and the rest silently fell back to hardcoded row constants.
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[121, 1] <- "Well Averages"
  raw[122, 1] <- "Treatment/ID"
  raw[123, 1] <- "Genotype"
  raw[124, 1] <- "Exclude/Include"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Well",      fallback = NA), 121L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment", fallback = NA), 122L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Genotype",  fallback = NA), 123L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Exclude",   fallback = NA), 124L)
})

test_that("find_mea_metadata_row matches whole words, not any prefix", {
  raw <- as.data.frame(matrix("", nrow = 170, ncol = 3), stringsAsFactors = FALSE)
  raw[110, 1] <- "Wellington Data"   # must not be mistaken for the Well row
  raw[121, 1] <- "Well Averages"

  expect_equal(NOVA:::find_mea_metadata_row(raw, "Well", fallback = NA), 121L)
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

# ── baseline normalisation across plates ──────────────────────────────────────
# Regression: well IDs repeat on every plate, so a baseline key without
# Experiment matched every plate's baseline, fanning the join out and
# normalising each well to other plates' baselines as well as its own.

test_that("each well is normalised to its own plate's baseline", {
  dir <- make_two_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment",
                              baseline_timepoint = "baseline", verbose = FALSE)

  one_h <- res$normalized_data[res$normalized_data$Timepoint == "1h" &
                               res$normalized_data$Variable == "Mean Firing Rate (Hz)", ]
  # One row per plate -- no fan-out.
  expect_equal(nrow(one_h), 2L)
  # MEA001: 20/10 = 2. MEA002: 400/100 = 4. Never 20/100 or 400/10.
  expect_equal(one_h$Normalized_Value[one_h$Experiment == "MEA001"], 2)
  expect_equal(one_h$Normalized_Value[one_h$Experiment == "MEA002"], 4)
})

test_that("baseline join does not inflate row count", {
  dir <- make_two_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment",
                              baseline_timepoint = "baseline", verbose = FALSE)
  expect_equal(nrow(res$normalized_data), nrow(res$raw_data))
})

test_that("baseline rows normalise to exactly 1", {
  dir <- make_two_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment",
                              baseline_timepoint = "baseline", verbose = FALSE)
  base <- res$normalized_data[res$normalized_data$Timepoint == "baseline", ]
  expect_true(all(base$Normalized_Value == 1))
})

# ── baseline detection ────────────────────────────────────────────────────────
# Regression: the candidate baseline test was an unanchored regex, so "30min"
# matched the "0min" pattern and was reported as a candidate baseline.

test_that("discover_mea_structure does not mistake 30min for a baseline", {
  dir <- make_two_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  d <- discover_mea_structure(dir, verbose = FALSE)
  expect_true("baseline" %in% d$potential_baselines)
  expect_false("30min" %in% d$potential_baselines)
})

test_that("potential_baselines lists only real candidates, best first", {
  dir <- make_two_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  d <- discover_mea_structure(dir, verbose = FALSE)
  # Elapsed timepoints are not baseline candidates, whatever their labels spell.
  expect_equal(d$potential_baselines, "baseline")
})

# ── exclusion flags and zero baselines ────────────────────────────────────────
# Real exports mark wells "ex" (the bundled MEA012 baseline marks 16 of them) and
# can carry a zero baseline, which makes a fold-change undefined. Neither path
# had any coverage.

make_one_plate_dir <- function(exclude = NULL, baseline_vals = c(10, 10)) {
  dir <- tempfile("mea_one_")
  dir.create(file.path(dir, "MEA001"), recursive = TRUE)
  vars <- c("Mean Firing Rate (Hz)", "Number of Spikes")
  wells <- c("A1", "A2")
  write_fake_mea_csv(file.path(dir, "MEA001", "MEA001_baseline.csv"),
                     wells = wells, treatments = c("drug", "drug"), variables = vars,
                     values = matrix(rep(baseline_vals, each = 2), nrow = 2, byrow = TRUE),
                     exclude = exclude)
  write_fake_mea_csv(file.path(dir, "MEA001", "MEA001_1h.csv"),
                     wells = wells, treatments = c("drug", "drug"), variables = vars,
                     values = matrix(c(20, 30, 20, 30), nrow = 2, byrow = TRUE),
                     exclude = exclude)
  dir
}

test_that("wells marked ex are dropped", {
  dir <- make_one_plate_dir(exclude = c("ex", ""))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment", verbose = FALSE)
  expect_false("A1" %in% res$raw_data$Well)
  expect_true("A2" %in% res$raw_data$Well)
})

test_that("a zero baseline yields NA rather than an infinite fold-change", {
  dir <- make_one_plate_dir(baseline_vals = c(0, 0))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment",
                              baseline_timepoint = "baseline", verbose = FALSE)
  norm <- res$normalized_data$Normalized_Value
  expect_true(all(is.na(norm)))
  expect_false(any(is.infinite(norm)))
})

test_that("single-plate data normalises correctly", {
  dir <- make_one_plate_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- process_mea_flexible(dir, grouping_variables = "Treatment",
                              baseline_timepoint = "baseline", verbose = FALSE)
  expect_equal(nrow(res$normalized_data), nrow(res$raw_data))
  # values are [variable x well]: A1 reads 20 and A2 reads 30, both over a
  # baseline of 10, for each of the two variables.
  one_h <- res$normalized_data[res$normalized_data$Timepoint == "1h", ]
  expect_equal(unique(one_h$Normalized_Value[one_h$Well == "A1"]), 2)
  expect_equal(unique(one_h$Normalized_Value[one_h$Well == "A2"]), 3)
})

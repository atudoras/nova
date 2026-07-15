# case_studies/_shared/test_adapt_epa.R
#
# Unit tests for the EPA adapter. Run from the repo root:
#   devtools::load_all("."); testthat::test_file("case_studies/_shared/test_adapt_epa.R")
#
# These are not part of the package suite (case_studies/ is .Rbuildignore'd), but
# the adapter is where a silent mapping error would poison everything downstream,
# so it gets checked against hand-computed values rather than eyeballed.

library(testthat)

# testthat::test_file() sets the working directory to this file's folder, while
# source()ing from the repo root does not. Resolve the shared directory either way
# rather than assuming one caller.
.shared_dir <- if (file.exists("adapt_epa.R")) "." else file.path("case_studies", "_shared")
source(file.path(.shared_dir, "adapt_epa.R"))

EPA_CSV <- normalizePath(
  file.path(.shared_dir, "..", "01_epa_dnt_ontogeny", "data", "brown2016_mea_dnt.csv"),
  mustWork = FALSE)
skip_no_data <- function() skip_if_not(file.exists(EPA_CSV), "EPA CSV not downloaded")

test_that("adapter reproduces the verified shape of the source file", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  raw <- utils::read.csv(EPA_CSV, stringsAsFactors = FALSE)

  # 26 columns - 6 keys - units - file.name - 2 excluded = 16 metrics
  expect_equal(length(a$processing_params$excluded_metrics), 2L)
  expect_equal(nrow(a$raw_data), nrow(raw) * 16L)
  expect_equal(dplyr::n_distinct(a$raw_data$Timepoint), 4L)
  expect_setequal(unique(a$raw_data$Timepoint), c("DIV5", "DIV7", "DIV9", "DIV12"))
})

test_that("experiment identity is the culture, not the plastic", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  raw <- utils::read.csv(EPA_CSV, stringsAsFactors = FALSE)

  # 4 plate serials but 6 experiments: serials are reused across culture dates.
  expect_equal(dplyr::n_distinct(raw$Plate.SN), 4L)
  expect_equal(dplyr::n_distinct(a$raw_data$Experiment), 6L)

  # The reused serials must not collapse two cultures into one identity.
  reused <- names(which(table(unique(raw[, c("date", "Plate.SN")])$Plate.SN) > 1))
  expect_gt(length(reused), 0)
  for (sn in reused) {
    exps <- unique(a$raw_data$Experiment[a$raw_data$Plate_SN == sn])
    expect_gt(length(exps), 1)
  }

  # And a well is only identified once its experiment is known.
  expect_gt(nrow(unique(a$raw_data[, c("Experiment", "Well")])),
            dplyr::n_distinct(a$raw_data$Well))
})

test_that("excluded and structural-NA metrics are handled as documented", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  # EPA says CVtime/CVnetwork were not used: they must not appear at all.
  expect_false(any(grepl("cv\\.(time|network)", a$raw_data$Variable)))
  # Structural-NA metrics stay in the long table but never reach the PCA.
  expect_true("Burst Duration (s)" %in% a$raw_data$Variable)
  expect_false("Burst Duration (s)" %in% a$pca_metrics)
  expect_true("Mean Firing Rate (Hz)" %in% a$pca_metrics)
})

test_that("PCA metric set carries no structural NA", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  d <- a$raw_data[a$raw_data$Variable %in% a$pca_metrics, ]
  expect_equal(sum(is.na(d$Value)), 0L)
})

test_that("normalisation divides by the right controls, hand-checked", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, control_scope = "plate", verbose = FALSE)
  raw <- utils::read.csv(EPA_CSV, stringsAsFactors = FALSE)
  raw$Experiment <- paste(raw$date, raw$Plate.SN, sep = "_")

  ex <- "20140205_MW1007-26"
  tp <- 12
  # Recompute the control mean straight from the source file.
  ctrl_rows <- raw[raw$Experiment == ex & raw$DIV == tp & raw$dose == 0, ]
  expect_gt(nrow(ctrl_rows), 1)
  expected_ctrl <- mean(ctrl_rows$meanfiringrate, na.rm = TRUE)

  got <- a$normalized_data[a$normalized_data$Experiment == ex &
                           a$normalized_data$Timepoint == "DIV12" &
                           a$normalized_data$Variable == "Mean Firing Rate (Hz)", ]
  expect_equal(unique(got$Control_Value), expected_ctrl, tolerance = 1e-9)

  # And a single well's normalised value is its own raw value over that mean.
  w <- got[1, ]
  src <- raw[raw$Experiment == ex & raw$DIV == tp & raw$well == w$Well, ]
  expect_equal(w$Value, src$meanfiringrate, tolerance = 1e-9)
  expect_equal(w$Normalized_Value, src$meanfiringrate / expected_ctrl, tolerance = 1e-9)
})

test_that("control wells normalise to ~1 on average, by construction", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, control_scope = "plate", verbose = FALSE)
  ctl <- a$normalized_data[a$normalized_data$Is_Control &
                           a$normalized_data$Variable == "Mean Firing Rate (Hz)" &
                           a$normalized_data$Timepoint == "DIV12", ]
  expect_equal(mean(ctl$Normalized_Value, na.rm = TRUE), 1, tolerance = 0.05)
})

test_that("compound-scoped controls are matched, and narrower", {
  skip_no_data()
  ap <- adapt_epa(EPA_CSV, control_scope = "plate", verbose = FALSE)
  ac <- adapt_epa(EPA_CSV, control_scope = "compound", verbose = FALSE)
  # Per-compound controls are 2 wells; plate-level pools all of them.
  expect_lt(max(ac$normalized_data$n_control_wells, na.rm = TRUE),
            max(ap$normalized_data$n_control_wells, na.rm = TRUE))
  expect_equal(nrow(ap$normalized_data), nrow(ac$normalized_data))
})

test_that("a zero control mean yields NA, never Inf", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  expect_false(any(is.infinite(a$normalized_data$Normalized_Value)))
  # DIV 5 controls are largely silent, so undefined normalisations are expected
  # there and must be NA rather than a fabricated number.
  zero_ctrl <- a$normalized_data[!is.na(a$normalized_data$Control_Value) &
                                 a$normalized_data$Control_Value == 0, ]
  if (nrow(zero_ctrl)) expect_true(all(is.na(zero_ctrl$Normalized_Value)))
})

test_that("the join never fabricates rows", {
  skip_no_data()
  a <- adapt_epa(EPA_CSV, verbose = FALSE)
  expect_equal(nrow(a$normalized_data), nrow(a$raw_data))
  expect_equal(anyDuplicated(a$normalized_data[, c("Experiment", "Well",
                                                   "Timepoint", "Variable")]), 0L)
})

test_that("a file that is not the EPA export fails loudly", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(data.frame(a = 1, b = 2), tmp, row.names = FALSE)
  expect_error(adapt_epa(tmp, verbose = FALSE), "missing column")
})

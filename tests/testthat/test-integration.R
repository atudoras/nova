# tests/testthat/test-integration.R
#
# End-to-end checks against the real two-plate Axion export bundled in Example/.
#
# Every other test file builds its own synthetic input, which is what let a whole
# class of bugs through: the fixtures were single-plate and used bare metadata
# labels, so they could not exercise the cross-plate paths that were broken, nor
# the label-matching that real exports need. These tests run the actual pipeline
# on the actual files and assert the invariants that were violated.
#
# Example/ is excluded from the package build (.Rbuildignore), so these skip
# rather than fail wherever the data is absent -- R CMD check included.

mea_example_dir <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "Example", "MEA Neuronal Agonists"),
    "Example/MEA Neuronal Agonists"
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit) == 0L) NULL else normalizePath(hit[1])
}

skip_without_example <- function() {
  d <- mea_example_dir()
  testthat::skip_if(is.null(d), "bundled Example/ MEA data not available")
  d
}

# The bundled export: MEA012 has 10 timepoints, MEA013 has 6, and they share
# well IDs -- D1/pbs exists on both, which is what made the cross-plate bugs
# visible in the first place.
processed_example <- function(dir) {
  process_mea_flexible(dir, grouping_variables = "Treatment",
                       baseline_timepoint = "baseline", verbose = FALSE)
}

test_that("the real export parses: both plates, all wells, no data lost", {
  dir <- skip_without_example()
  p <- processed_example(dir)

  expect_setequal(unique(p$raw_data$Experiment), c("MEA012", "MEA013"))
  expect_setequal(unique(p$raw_data$Treatment), c("dhpg", "gabazine", "ka", "pbs"))
  expect_true("baseline" %in% p$raw_data$Timepoint)
  expect_false(any(is.na(p$raw_data$Value)))
})

test_that("metadata rows are located by label, not by falling back to constants", {
  dir <- skip_without_example()
  raw <- readr::read_csv(file.path(dir, "MEA012", "MEA012_baseline.csv"),
                         col_names = FALSE, show_col_types = FALSE)
  # Real Axion labels are qualified: "Well Averages", "Treatment/ID",
  # "Exclude/Include". Passing fallback = NA means a match must be real.
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Well",      fallback = NA), 121L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Treatment", fallback = NA), 122L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Genotype",  fallback = NA), 123L)
  expect_equal(NOVA:::find_mea_metadata_row(raw, "Exclude",   fallback = NA), 124L)
})

test_that("wells marked ex in the real export are excluded", {
  dir <- skip_without_example()
  raw <- readr::read_csv(file.path(dir, "MEA012", "MEA012_baseline.csv"),
                         col_names = FALSE, show_col_types = FALSE)
  wells   <- unlist(raw[121, -1])
  flags   <- tolower(trimws(as.character(unlist(raw[124, -1]))))
  excluded <- unique(wells[which(flags == "ex")])
  expect_gt(length(excluded), 0)   # the file really does mark wells ex

  p <- processed_example(dir)
  kept <- unique(p$raw_data$Well[p$raw_data$Experiment == "MEA012"])
  expect_false(any(excluded %in% kept))
})

test_that("baseline normalisation does not fan out across plates", {
  dir <- skip_without_example()
  p <- processed_example(dir)

  # The join must be a lookup, not a many-to-many match.
  expect_equal(nrow(p$normalized_data), nrow(p$raw_data))
  # Every baseline observation is its own reference.
  base <- p$normalized_data[p$normalized_data$Timepoint == "baseline", ]
  expect_true(all(base$Normalized_Value == 1, na.rm = TRUE))
})

test_that("each well is normalised against its own plate's baseline", {
  dir <- skip_without_example()
  p <- processed_example(dir)

  # Recompute independently, per plate, and demand agreement everywhere.
  d <- p$normalized_data
  base <- d[d$Timepoint == "baseline",
            c("Experiment", "Well", "Variable", "Treatment", "Value")]
  names(base)[names(base) == "Value"] <- "own_baseline"
  chk <- merge(d, base, by = c("Experiment", "Well", "Variable", "Treatment"),
               all.x = TRUE)
  expected <- ifelse(is.na(chk$own_baseline) | chk$own_baseline == 0,
                     NA_real_, chk$Value / chk$own_baseline)
  expect_equal(chk$Normalized_Value, expected, tolerance = 1e-9)
})

test_that("the same well ID on both plates stays two distinct samples", {
  dir <- skip_without_example()
  p <- processed_example(dir)
  pca <- pca_analysis_enhanced(normalized_data = p$normalized_data,
                               grouping_variables = "Treatment",
                               value_column = "Normalized_Value", verbose = FALSE)

  # One PCA point per real observation, not per pooled well ID.
  n_expected <- nrow(unique(p$normalized_data[, c("Experiment", "Well", "Timepoint", "Treatment")]))
  expect_equal(nrow(pca$plot_data), n_expected)
  expect_equal(anyDuplicated(pca$plot_data$Sample), 0L)
  # Identity has to survive into plot_data or downstream cannot recover it.
  expect_true(all(c("Experiment", "Well") %in% names(pca$plot_data)))
})

test_that("the trajectory figure describes real movement, not a flat zero line", {
  dir <- skip_without_example()
  p <- processed_example(dir)
  pca <- pca_analysis_enhanced(normalized_data = p$normalized_data,
                               grouping_variables = "Treatment",
                               value_column = "Normalized_Value", verbose = FALSE)
  s <- nova_trajectory_summary(pca, verbose = FALSE)

  expect_equal(s$params$unit_var, c("Experiment", "Well"))
  expect_gt(max(s$displacement$mean_disp), 0)
  expect_true(any(s$displacement$sem_disp > 0, na.rm = TRUE))
  # Vehicle should move least: a sanity check on the biology, not just the code.
  m <- s$metrics
  expect_equal(m$group[which.min(m$net_displacement)], "pbs")
})

test_that("replicate units are wells, and each is counted once", {
  dir <- skip_without_example()
  p <- processed_example(dir)
  pca <- pca_analysis_enhanced(normalized_data = p$normalized_data,
                               grouping_variables = "Treatment",
                               value_column = "Normalized_Value", verbose = FALSE)

  n_units <- nrow(unique(p$normalized_data[, c("Experiment", "Well")]))
  tr <- nova_extract_trajectories(pca, group_var = "Treatment",
                                  unit_var = c("Experiment", "Well"))
  expect_equal(max(tr$n_obs), 1L)          # no silent averaging of two wells
  expect_equal(length(unique(tr$unit)), n_units)

  # The trajectory plotter must agree about what a well is.
  tp <- plot_pca_trajectories_general(pca, trajectory_grouping = "Treatment",
                                      save_plots = FALSE, verbose = FALSE)
  expect_equal(length(unique(tp$individual_trajectories$well_id)), n_units)
  expect_equal(max(tp$individual_trajectories$n_obs), 1L)
})

# tests/testthat/test-table-input.R
# process_mea_table(): ingesting an already-tidy published table.

# Two plates reusing well IDs, two timepoints, a control and a treated condition.
# Values are deterministic so every expectation below is hand-computable.
make_wide <- function() {
  d <- expand.grid(well = c("A1", "A2"), plate = c("P1", "P2"),
                   div = c(5, 7), stringsAsFactors = FALSE)
  d$drug <- ifelse(d$well == "A1", "ctrl", "cpd")
  # P1 reads 10/20, P2 reads 100/200: any cross-plate leak is visible at a glance.
  base <- ifelse(d$plate == "P1", 10, 100)
  d$firing_rate <- base * ifelse(d$div == 7, 2, 1)
  d$n_bursts    <- base
  d
}

test_that("wide input reshapes to the NOVA schema", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = c("firing_rate", "n_bursts"),
                         timepoint_prefix = "DIV", verbose = FALSE)
  expect_true(all(c("Variable", "Well", "Value", "Treatment", "Experiment",
                    "Timepoint") %in% names(r$raw_data)))
  expect_equal(nrow(r$raw_data), nrow(make_wide()) * 2L)
  expect_setequal(unique(r$raw_data$Timepoint), c("DIV5", "DIV7"))
  expect_null(r$normalized_data)   # normalize = "none"
})

test_that("long input is accepted without reshaping", {
  w <- make_wide()
  lng <- tidyr::pivot_longer(w, c("firing_rate", "n_bursts"),
                             names_to = "metric", values_to = "val")
  r <- process_mea_table(as.data.frame(lng), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         variable_column = "metric", value_column = "val",
                         timepoint_prefix = "DIV", verbose = FALSE)
  expect_equal(nrow(r$raw_data), nrow(lng))
  expect_setequal(unique(r$raw_data$Variable), c("firing_rate", "n_bursts"))
})

# ── experiment identity ───────────────────────────────────────────────────────
# The argument most often got wrong, and the one this package has a history with.

test_that("several columns can identify one experiment", {
  w <- make_wide()
  w$date <- rep(c("d1", "d2"), length.out = nrow(w))   # plates reused across dates
  r <- process_mea_table(w, experiment = c("date", "plate"), well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = "firing_rate", verbose = FALSE)
  # 2 dates x 2 plates = 4 experiments, not 2.
  expect_equal(dplyr::n_distinct(r$raw_data$Experiment), 4L)
  expect_true(all(grepl("^d[12]_P[12]$", unique(r$raw_data$Experiment))))
})

test_that("the same well ID on two plates stays two wells", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = "firing_rate", verbose = FALSE)
  expect_equal(dplyr::n_distinct(r$raw_data$Well), 2L)
  expect_equal(nrow(unique(r$raw_data[, c("Experiment", "Well")])), 4L)
})

test_that("duplicate observations warn instead of being averaged in silence", {
  w <- rbind(make_wide(), make_wide()[1, ])   # one row repeated
  expect_warning(
    process_mea_table(w, experiment = "plate", well = "well", timepoint = "div",
                      treatment = "drug", metrics = "firing_rate", verbose = FALSE),
    "more than one value per"
  )
})

# ── baseline normalisation ────────────────────────────────────────────────────

test_that("baseline normalisation divides each well by its own earlier value", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = c("firing_rate", "n_bursts"),
                         timepoint_prefix = "DIV", normalize = "baseline",
                         verbose = FALSE)
  n <- r$normalized_data
  # firing_rate doubles from DIV5 to DIV7 on both plates, whatever the scale.
  fr7 <- n[n$Variable == "firing_rate" & n$Timepoint == "DIV7", ]
  expect_true(all(fr7$Normalized_Value == 2))
  # Baseline rows are their own reference.
  fr5 <- n[n$Variable == "firing_rate" & n$Timepoint == "DIV5", ]
  expect_true(all(fr5$Normalized_Value == 1))
  expect_equal(nrow(n), nrow(r$raw_data))   # a lookup, not a fan-out
})

test_that("baseline normalisation never leaks across plates", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = "firing_rate", timepoint_prefix = "DIV",
                         normalize = "baseline", verbose = FALSE)
  # P2 reads 10x P1 throughout. Normalised to its own baseline it is still 2x,
  # never 20x (P2 value over P1 baseline).
  expect_true(all(r$normalized_data$Normalized_Value %in% c(1, 2)))
})

# ── control normalisation ─────────────────────────────────────────────────────

test_that("control normalisation divides by same-plate controls at the same timepoint", {
  w <- make_wide()
  r <- process_mea_table(w, experiment = "plate", well = "well", timepoint = "div",
                         treatment = "drug", metrics = "firing_rate",
                         timepoint_prefix = "DIV", normalize = "control",
                         control = w$drug == "ctrl", verbose = FALSE)
  n <- r$normalized_data
  # Control and treated wells read the same here, so everything is 1x its control.
  expect_true(all(n$Normalized_Value == 1))
  # And the divisor is inspectable, not implicit.
  expect_true(all(c("Control_Value", "n_control_wells") %in% names(n)))
  expect_true(all(n$n_control_wells == 1))
})

test_that("control_within narrows which controls count", {
  w <- make_wide()
  w$compound <- rep(c("X", "Y"), length.out = nrow(w))
  wide_ctrl <- process_mea_table(w, experiment = "plate", well = "well",
                                 timepoint = "div", treatment = "drug",
                                 metrics = "firing_rate", normalize = "control",
                                 control = w$drug == "ctrl", verbose = FALSE)
  narrow <- process_mea_table(w, experiment = "plate", well = "well",
                              timepoint = "div", treatment = "drug",
                              metrics = "firing_rate", normalize = "control",
                              control = w$drug == "ctrl",
                              control_within = "compound", verbose = FALSE)
  expect_lte(max(narrow$normalized_data$n_control_wells, na.rm = TRUE),
             max(wide_ctrl$normalized_data$n_control_wells, na.rm = TRUE))
})

test_that("a zero divisor yields NA, never Inf", {
  w <- make_wide()
  w$firing_rate[w$drug == "ctrl"] <- 0     # silent controls
  r <- process_mea_table(w, experiment = "plate", well = "well", timepoint = "div",
                         treatment = "drug", metrics = "firing_rate",
                         normalize = "control", control = w$drug == "ctrl",
                         verbose = FALSE)
  expect_false(any(is.infinite(r$normalized_data$Normalized_Value)))
  expect_true(all(is.na(r$normalized_data$Normalized_Value)))
})

test_that("control normalisation without `control` fails loudly", {
  expect_error(
    process_mea_table(make_wide(), experiment = "plate", well = "well",
                      timepoint = "div", treatment = "drug",
                      metrics = "firing_rate", normalize = "control", verbose = FALSE),
    "needs `control`"
  )
})

# ── mapping errors surface as errors, not as wrong numbers ────────────────────

test_that("an unknown column names itself and lists the alternatives", {
  expect_error(
    process_mea_table(make_wide(), experiment = "nope", well = "well",
                      timepoint = "div", treatment = "drug",
                      metrics = "firing_rate", verbose = FALSE),
    "not found"
  )
})

test_that("neither metrics nor a long-format mapping is an error", {
  expect_error(
    process_mea_table(make_wide(), experiment = "plate", well = "well",
                      timepoint = "div", treatment = "drug", verbose = FALSE),
    "Supply either"
  )
})

test_that("a mis-shaped control vector is rejected", {
  expect_error(
    process_mea_table(make_wide(), experiment = "plate", well = "well",
                      timepoint = "div", treatment = "drug",
                      metrics = "firing_rate", normalize = "control",
                      control = c(TRUE, FALSE), verbose = FALSE),
    "one element per row"
  )
})

test_that("excluded metrics never reach the output", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = c("firing_rate", "n_bursts"),
                         exclude_metrics = "n_bursts", verbose = FALSE)
  expect_setequal(unique(r$raw_data$Variable), "firing_rate")
})

test_that("metric_labels rename for display", {
  r <- process_mea_table(make_wide(), experiment = "plate", well = "well",
                         timepoint = "div", treatment = "drug",
                         metrics = "firing_rate",
                         metric_labels = c(firing_rate = "Mean Firing Rate (Hz)"),
                         verbose = FALSE)
  expect_setequal(unique(r$raw_data$Variable), "Mean Firing Rate (Hz)")
})

# ── the point of all this: the rest of NOVA works on the result ───────────────

test_that("output feeds the PCA and trajectory layers unchanged", {
  d <- expand.grid(well = paste0("A", 1:3), plate = c("P1", "P2"),
                   div = c(5, 7, 9), stringsAsFactors = FALSE)
  d$drug <- rep(c("ctrl", "cpd"), length.out = nrow(d))
  for (m in paste0("m", 1:4)) {
    d[[m]] <- seq_len(nrow(d)) / 7 + match(m, paste0("m", 1:4)) +
      ifelse(d$drug == "cpd", d$div / 3, 0)
  }
  r <- process_mea_table(d, experiment = "plate", well = "well", timepoint = "div",
                         treatment = "drug", metrics = paste0("m", 1:4),
                         timepoint_prefix = "DIV", normalize = "baseline",
                         verbose = FALSE)
  pca <- pca_analysis_enhanced(normalized_data = r$normalized_data,
                               grouping_variables = "Treatment", verbose = FALSE)
  expect_true(all(c("Experiment", "Well") %in% names(pca$plot_data)))

  s <- nova_trajectory_summary(pca, group_var = "Treatment", verbose = FALSE)
  expect_equal(s$params$unit_var, c("Experiment", "Well"))
  expect_false(all(s$displacement$mean_disp == 0))
})

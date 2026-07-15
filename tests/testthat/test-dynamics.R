# tests/testthat/test-dynamics.R
# Tests for the trajectory-summary layer + timepoint helpers.

make_df <- function() {
  data.frame(
    PC1 = c(0, 1, 2, 3,    # Direct: straight out along PC1
            0, 1, 0, 1,    # Wander: back and forth
            0, 0.05, 0.0, 0.05),  # Still: barely moves
    PC2 = c(0, 0, 0, 0,
            0, 1, 0, 1,
            0, 0.0, 0.05, 0.0),
    Treatment = rep(c("Direct", "Wander", "Still"), each = 4),
    Well      = rep(c("W1", "W1", "W2", "W2"), 3),
    Timepoint = rep(c("baseline", "30min", "1h", "2h"), 3),
    stringsAsFactors = FALSE
  )
}

# ---- timepoint parsing / ordering (correctness fix kept) -------------------
test_that("nova_time_to_minutes parses heterogeneous labels", {
  expect_true(is.na(nova_time_to_minutes("baseline")))
  expect_equal(nova_time_to_minutes("90min"), 90)
  expect_equal(nova_time_to_minutes("1h"), 60)
  expect_equal(nova_time_to_minutes("1h30"), 90)
  expect_equal(nova_time_to_minutes("30s"), 0.5)
  expect_equal(nova_time_to_minutes("DIV7"), 7 * 24 * 60)
})

test_that("nova_order_timepoints puts baseline first, orders compound labels", {
  ord <- nova_order_timepoints(c("1h30", "baseline", "15min", "1h", "2h", "0min", "1h15", "1h45"))
  expect_equal(ord, c("baseline", "0min", "15min", "1h", "1h15", "1h30", "1h45", "2h"))
  expect_lt(match("1h", ord), match("1h15", ord))
})

# ---- extraction ------------------------------------------------------------
test_that("nova_extract_trajectories returns ordered trajectories, baseline first", {
  tr <- nova_extract_trajectories(make_df(), group_var = "Treatment")
  expect_s3_class(tr, "nova_trajectories")
  expect_setequal(unique(tr$group), c("Direct", "Wander", "Still"))
  base <- tr[tr$time_label == "baseline", ]
  expect_true(all(base$time_rank == 1))
})

# ---- trajectory summary ----------------------------------------------------
test_that("nova_trajectory_summary recovers known displacement geometry", {
  s <- nova_trajectory_summary(make_df(), group_var = "Treatment", verbose = FALSE)
  m <- s$metrics
  d <- m[m$group == "Direct", ]
  w <- m[m$group == "Wander", ]
  st <- m[m$group == "Still", ]
  # straight-out path: net == path, directness 1
  expect_equal(d$net_displacement, 3, tolerance = 1e-8)
  expect_equal(d$path_length, 3, tolerance = 1e-8)
  expect_equal(d$directness, 1, tolerance = 1e-8)
  # wanderer: net < path, directness < 1
  expect_lt(w$directness, 0.5)
  # still: tiny net displacement
  expect_lt(st$net_displacement, 0.2)
})

test_that("nova_trajectory_summary builds both plots and a displacement table", {
  s <- nova_trajectory_summary(make_df(), group_var = "Treatment", verbose = FALSE)
  expect_s3_class(s$plots$displacement, "ggplot")
  expect_s3_class(s$plots$map, "ggplot")
  expect_true(all(c("group", "time_label", "mean_disp") %in% names(s$displacement)))
  # SEM present because a replicate column (Well) was detected
  expect_true("sem_disp" %in% names(s$displacement))
})

test_that("nova_trajectory_summary works without a replicate column (no error bands)", {
  df <- make_df(); df$Well <- NULL
  s <- nova_trajectory_summary(df, group_var = "Treatment", unit_var = NULL, verbose = FALSE)
  expect_s3_class(s, "nova_trajectory_summary")
  expect_true(all(is.na(s$displacement$sem_disp)))
})

# ---- describe --------------------------------------------------------------
test_that("nova_describe returns a non-empty narrative", {
  s <- nova_trajectory_summary(make_df(), group_var = "Treatment", verbose = FALSE)
  txt <- nova_describe(s)
  expect_type(txt, "character")
  expect_gt(length(txt), 1)
  # the farthest mover should be named in the lead sentence
  expect_true(any(grepl("Direct", txt)))
})

# ---- the real pca_analysis_enhanced() -> nova_trajectory_summary() path ------
# Regression: this end-to-end path had no coverage. plot_data folded Well into
# the Sample string and dropped it, so unit_var auto-detection landed on Sample
# -- a per-observation ID containing the timepoint. Every "replicate" then
# spanned one timepoint, making each displacement 0 by construction, and the
# headline figure rendered as a flat line at zero with no error.

# Long-format data in the shape process_mea_flexible() emits: two treatments,
# three wells each, on TWO plates, moving apart over four timepoints.
# Two plates is the point -- well IDs repeat across plates, so a single-plate
# fixture cannot exercise any of the identity bugs this file guards.
make_long_mea <- function() {
  tps <- c("baseline", "30min", "1h", "2h")
  grid <- expand.grid(Timepoint = tps, Well = c("A1", "A2", "A3"),
                      Experiment = c("MEA001", "MEA002"),
                      Treatment = c("drug", "vehicle"),
                      Variable = paste0("V", 1:4),
                      stringsAsFactors = FALSE)
  step <- match(grid$Timepoint, tps) - 1L
  # drug drifts with timepoint; vehicle stays put. Well adds a small offset so
  # replicates are not identical (a zero-variance metric would be filtered out).
  drift  <- ifelse(grid$Treatment == "drug", step * 1.5, 0)
  offset <- match(grid$Well, c("A1", "A2", "A3")) * 0.1
  # The plates sit at different levels, so merging a well across plates shows up
  # as inflated spread rather than as plausible-looking numbers.
  plate  <- ifelse(grid$Experiment == "MEA002", 2, 0)
  grid$Value <- 10 + drift + offset + plate + match(grid$Variable, paste0("V", 1:4)) * 0.3
  grid$Normalized_Value <- grid$Value / 10
  grid
}

test_that("pca_analysis_enhanced carries Well and Experiment into plot_data", {
  pca <- pca_analysis_enhanced(normalized_data = make_long_mea(),
                               grouping_variables = "Treatment", verbose = FALSE)
  expect_true(all(c("Well", "Experiment") %in% names(pca$plot_data)))
})

test_that("trajectory summary off a PCA result yields real displacement, not zeros", {
  pca <- pca_analysis_enhanced(normalized_data = make_long_mea(),
                               grouping_variables = "Treatment", verbose = FALSE)
  s <- nova_trajectory_summary(pca, verbose = FALSE)

  # The replicate unit is the well AND its plate -- never Sample, which cannot
  # describe movement, and never Well alone, which merges plates.
  expect_equal(s$params$unit_var, c("Experiment", "Well"))
  expect_false(all(s$displacement$mean_disp == 0))
  expect_true(any(s$displacement$sem_disp > 0, na.rm = TRUE))
  # The condition that actually moved should out-displace the one that did not.
  m <- s$metrics
  expect_gt(m$net_displacement[m$group == "drug"],
            m$net_displacement[m$group == "vehicle"])
})

test_that("the same well on two plates stays two replicates", {
  pca <- pca_analysis_enhanced(normalized_data = make_long_mea(),
                               grouping_variables = "Treatment", verbose = FALSE)
  tr <- nova_extract_trajectories(pca, group_var = "Treatment",
                                  unit_var = c("Experiment", "Well"))
  # Every (unit, timepoint) holds exactly one observation: no plate merging.
  expect_equal(max(tr$n_obs), 1L)
  # 2 treatments x 2 plates x 3 wells = 12 trajectories.
  expect_equal(length(unique(tr$traj_id)), 12L)
})

test_that("unit_var accepts multiple columns rather than failing silently", {
  pca <- pca_analysis_enhanced(normalized_data = make_long_mea(),
                               grouping_variables = "Treatment", verbose = FALSE)
  s <- nova_trajectory_summary(pca, group_var = "Treatment",
                               unit_var = c("Experiment", "Well"), verbose = FALSE)
  expect_equal(s$params$unit_var, c("Experiment", "Well"))
  expect_true(any(s$displacement$sem_disp > 0, na.rm = TRUE))
})

test_that("an unusable unit_var warns instead of silently dropping the bands", {
  # Regression: the error was swallowed by tryCatch, so a bad unit_var was
  # indistinguishable from "no replicate column found".
  df <- make_df()
  expect_warning(
    s <- nova_trajectory_summary(df, group_var = "Treatment",
                                 unit_var = c("Well", "NoSuchColumn"), verbose = FALSE),
    regexp = "not found"
  )
  expect_equal(s$params$unit_var, "Well")
})

test_that("a unit_var with one timepoint per unit warns and drops the error bands", {
  df <- make_df()
  # Sample-style ID: unique per observation, so it cannot span timepoints.
  df$RowID <- paste0("obs", seq_len(nrow(df)))
  expect_warning(
    s <- nova_trajectory_summary(df, group_var = "Treatment", unit_var = "RowID",
                                 verbose = FALSE),
    regexp = "one timepoint per unit"
  )
  # Falls back to the group-mean path: correct geometry, no bands, and honest
  # about it rather than reporting a flat zero.
  expect_null(s$params$unit_var)
  expect_false(all(s$displacement$mean_disp == 0))
  expect_true(all(is.na(s$displacement$sem_disp)))
})

test_that("Sample is not used as a replicate unit even when present", {
  # Pins the candidate list specifically: Sample is the only column that could be
  # chosen here, and it must not be. A per-row ID would give one timepoint per
  # unit, which the degenerate guard would then also reject -- so this asserts on
  # the absence of a warning too, or the guard would mask a regression here.
  df <- make_df()
  df$Well <- NULL
  df$Sample <- paste0("s", seq_len(nrow(df)))
  expect_no_warning(
    s <- nova_trajectory_summary(df, group_var = "Treatment", verbose = FALSE)
  )
  expect_null(s$params$unit_var)
})

test_that("plot_pca_trajectories_general keys wells by Well, not by plate", {
  # Regression: individual_var defaults to "Experiment", and well_id was derived
  # by string-splitting it, so every well on a plate collapsed into one "well".
  pca <- pca_analysis_enhanced(normalized_data = make_long_mea(),
                               grouping_variables = "Treatment", verbose = FALSE)
  tr <- plot_pca_trajectories_general(pca, trajectory_grouping = "Treatment",
                                      save_plots = FALSE, verbose = FALSE)
  wells <- unique(tr$individual_trajectories$well_id)
  # 2 plates x 3 wells = 6 distinct replicate wells, not 2 plates.
  expect_equal(length(wells), 6L)
  expect_false(any(wells %in% c("MEA001", "MEA002")))
  # Each (well, timepoint) is one observation: nothing silently averaged.
  expect_equal(max(tr$individual_trajectories$n_obs), 1L)
})

# ---- well identity ---------------------------------------------------------
test_that("nova_unit_cols/nova_unit_id keep same-ID wells on different plates apart", {
  d <- data.frame(Experiment = c("MEA1", "MEA2", "MEA1"),
                  Well = c("A1", "A1", "A2"), stringsAsFactors = FALSE)
  expect_equal(nova_unit_cols(d), c("Experiment", "Well"))
  expect_equal(nova_unit_id(d), c("MEA1_A1", "MEA2_A1", "MEA1_A2"))
  # The whole point: counting on Well alone loses a replicate.
  expect_equal(length(unique(d$Well)), 2L)
  expect_equal(length(unique(nova_unit_id(d))), 3L)
})

test_that("nova_unit_cols warns rather than silently narrowing to Well alone", {
  # Falling back to Well without saying so is how this bug keeps returning: the
  # caller asks for a well's identity and gets something that is not one.
  expect_warning(cols <- nova_unit_cols(data.frame(Well = "A1")), "does not distinguish")
  expect_equal(cols, "Well")
  expect_no_warning(nova_unit_cols(data.frame(Well = "A1"), warn = FALSE))
  expect_no_warning(nova_unit_cols(data.frame(Experiment = "E1", Well = "A1")))
})

test_that("nova_unit_cols degrades honestly when identity columns are absent", {
  expect_equal(nova_unit_cols(data.frame(x = 1)), character(0))
  expect_null(nova_unit_id(data.frame(x = 1)))
})

# ---- backward compatibility ------------------------------------------------
test_that("existing exported functions are untouched and still present", {
  for (fn in c("pca_analysis_enhanced", "plot_pca_trajectories_general",
               "process_mea_flexible", "create_mea_heatmaps_enhanced", "plot_mea_metric")) {
    expect_true(is.function(get(fn)), info = fn)
  }
})

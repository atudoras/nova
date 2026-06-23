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

# ---- backward compatibility ------------------------------------------------
test_that("existing exported functions are untouched and still present", {
  for (fn in c("pca_analysis_enhanced", "plot_pca_trajectories_general",
               "process_mea_flexible", "create_mea_heatmaps_enhanced", "plot_mea_metric")) {
    expect_true(is.function(get(fn)), info = fn)
  }
})

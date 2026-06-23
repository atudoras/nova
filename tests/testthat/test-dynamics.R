# tests/testthat/test-dynamics.R
# Unit tests for the nova_dynamics submodule.

# ---- fixtures --------------------------------------------------------------
make_df <- function() {
  data.frame(
    PC1 = c(0, 1, 2, 3,          # A: straight line along PC1
            0, 1, 0, 1,          # B: zig-zag (oscillatory)
            0, 0.05, 0.02, 0.01),# C: barely moves (stable)
    PC2 = c(0, 0, 0, 0,
            0, 1, 0, 1,
            0, 0.02, -0.01, 0.0),
    Treatment = rep(c("A", "B", "C"), each = 4),
    Well      = rep(c("W1", "W2", "W1", "W2", "W1", "W2"), each = 2),
    Timepoint = rep(c("baseline", "30min", "1h", "2h"), 3),
    stringsAsFactors = FALSE
  )
}

# ---- timepoint parsing / ordering (the correctness fix) --------------------
test_that("nova_time_to_minutes parses heterogeneous labels", {
  expect_true(is.na(nova_time_to_minutes("baseline")))
  expect_true(is.na(nova_time_to_minutes("pre")))
  expect_equal(nova_time_to_minutes("0min"), 0)
  expect_equal(nova_time_to_minutes("90min"), 90)
  expect_equal(nova_time_to_minutes("1h"), 60)
  expect_equal(nova_time_to_minutes("1h30"), 90)
  expect_equal(nova_time_to_minutes("1h30min"), 90)
  expect_equal(nova_time_to_minutes("30s"), 0.5)
  expect_equal(nova_time_to_minutes("DIV7"), 7 * 24 * 60)
  expect_equal(nova_time_to_minutes("2h"), 120)
})

test_that("nova_order_timepoints puts baseline first and orders compound labels", {
  ord <- nova_order_timepoints(c("1h30", "baseline", "15min", "1h", "2h", "0min", "1h15", "1h45"))
  expect_equal(ord[1], "baseline")
  expect_equal(ord, c("baseline", "0min", "15min", "1h", "1h15", "1h30", "1h45", "2h"))
  # naive alphabetical would wrongly put "1h15" before "1h"
  expect_lt(match("1h", ord), match("1h15", ord))
})

# ---- extraction ------------------------------------------------------------
test_that("nova_extract_trajectories returns a well-formed nova_trajectories object", {
  tr <- nova_extract_trajectories(make_df(), group_var = "Treatment")
  expect_s3_class(tr, "nova_trajectories")
  expect_setequal(unique(tr$group), c("A", "B", "C"))
  expect_equal(attr(tr, "timepoint_order")[1], "baseline")
  # baseline always rank 1 and is the earliest numeric time
  base <- tr[tr$time_label == "baseline", ]
  expect_true(all(base$time_rank == 1))
  expect_true(all(base$time_numeric <= tr$time_numeric))
})

# ---- geometry --------------------------------------------------------------
test_that("nova_state_geometry recovers known geometry", {
  g <- nova_state_geometry(make_df(), group_var = "Treatment", verbose = FALSE)
  s <- g$summary
  a <- s[s$group == "A", ]
  b <- s[s$group == "B", ]
  c_ <- s[s$group == "C", ]
  # straight line -> straightness ~1, tortuosity ~1, persistence ~1
  expect_gt(a$straightness, 0.99)
  expect_lt(abs(a$tortuosity - 1), 0.02)
  expect_gt(a$directional_persistence, 0.99)
  # zig-zag -> much lower straightness than the straight line
  expect_lt(b$straightness, a$straightness)
  # barely-moving -> tiny path length
  expect_lt(c_$path_length, 0.2)
  expect_s3_class(g$plots$overlay, "ggplot")
  expect_s3_class(g$plots$velocity, "ggplot")
  expect_s3_class(g$plots$displacement, "ggplot")
})

# ---- similarity ------------------------------------------------------------
test_that("nova_trajectory_similarity: identical paths -> zero distance, symmetric", {
  df <- make_df()
  df2 <- df[df$Treatment %in% c("A"), ]
  df2b <- df2; df2b$Treatment <- "A_copy"
  dd <- rbind(df2, df2b)
  for (m in c("dtw", "frechet", "euclidean", "cosine")) {
    s <- nova_trajectory_similarity(dd, method = m, group_var = "Treatment", verbose = FALSE)
    expect_equal(unname(s$distance["A", "A_copy"]), 0, tolerance = 1e-8)
    expect_true(isSymmetric(unname(s$distance)))
    expect_equal(diag(s$distance), c(A = 0, A_copy = 0), ignore_attr = TRUE)
  }
})

test_that("nova_trajectory_similarity clusters and builds plots", {
  s <- nova_trajectory_similarity(make_df(), method = "frechet",
                                  group_var = "Treatment", n_clusters = 2, verbose = FALSE)
  expect_length(unique(s$clusters), 2)
  expect_s3_class(s$plots$dendrogram, "ggplot")
  expect_s3_class(s$plots$heatmap, "ggplot")
})

# ---- transitions -----------------------------------------------------------
test_that("nova_transition_matrix is row-stochastic with valid occupancy", {
  tm <- nova_transition_matrix(make_df(), k = 3, group_var = "Treatment",
                               unit_var = "Well", verbose = FALSE)
  rs <- rowSums(tm$transition)
  expect_true(all(abs(rs - 1) < 1e-8 | abs(rs) < 1e-8))  # each row sums to 1 or 0
  expect_equal(sum(tm$occupancy), 1, tolerance = 1e-8)
  expect_equal(nrow(tm$transition), 3)
  expect_s3_class(tm$plots$heatmap, "ggplot")
  expect_s3_class(tm$plots$flow, "ggplot")
})

# ---- regime ----------------------------------------------------------------
test_that("nova_dynamical_regime classifies stable vs directed sensibly", {
  r <- nova_dynamical_regime(make_df(), group_var = "Treatment", verbose = FALSE)
  cl <- r$classification
  expect_equal(cl$regime[cl$group == "C"], "stable")         # barely moves
  expect_true(cl$regime[cl$group == "A"] %in%
                c("convergent", "transitional", "divergent")) # directed motion
  expect_true(all(r$scores >= 0 & r$scores <= 1))
  expect_true(all(cl$confidence >= 0 & cl$confidence <= 1))
  expect_s3_class(r$plots$overlay, "ggplot")
})

test_that("regime thresholds are overridable", {
  default <- nova_dynamical_regime(make_df(), group_var = "Treatment", verbose = FALSE)
  raised  <- nova_dynamical_regime(make_df(), group_var = "Treatment",
                                   thresholds = list(stable_path = 5), verbose = FALSE)
  reg_def <- function(o, g) o$classification$regime[o$classification$group == g]
  # the directed group A is NOT stable by default, but a large stable_path
  # threshold (network "barely moves" up to 5x scale) flips it to stable
  expect_false(reg_def(default, "A") == "stable")
  expect_equal(reg_def(raised, "A"), "stable")
})

# ---- landscape -------------------------------------------------------------
test_that("nova_landscape builds a density grid and plots", {
  set.seed(1)
  df <- data.frame(
    PC1 = c(rnorm(40), rnorm(40, 4)), PC2 = c(rnorm(40), rnorm(40, 3)),
    Treatment = rep(c("A", "B"), each = 40),
    Timepoint = rep(c("baseline", "30min", "1h", "2h"), 20)
  )
  L <- nova_landscape(df, n_grid = 40, verbose = FALSE)
  expect_length(L$density$x, 40)
  expect_true(all(is.finite(L$grid$z)))
  expect_s3_class(L$plots$density, "ggplot")
  expect_s3_class(L$plots$potential, "ggplot")
})

# ---- describe + wrapper ----------------------------------------------------
test_that("nova_describe returns non-empty text for each result type", {
  df <- make_df()
  expect_type(nova_describe(nova_state_geometry(df, group_var = "Treatment", verbose = FALSE)), "character")
  expect_type(nova_describe(nova_dynamical_regime(df, group_var = "Treatment", verbose = FALSE)), "character")
  s <- nova_trajectory_similarity(df, group_var = "Treatment", verbose = FALSE)
  expect_gt(length(nova_describe(s)), 0)
})

test_that("nova_dynamics wrapper runs the pipeline", {
  dyn <- nova_dynamics(make_df(), analyses = c("geometry", "regime"),
                       group_var = "Treatment", verbose = FALSE)
  expect_s3_class(dyn, "nova_dynamics")
  expect_true(!is.null(dyn$geometry) && !is.null(dyn$regime))
})

# ---- backward compatibility ------------------------------------------------
test_that("existing exported functions are untouched and still present", {
  for (fn in c("pca_analysis_enhanced", "plot_pca_trajectories_general",
               "process_mea_flexible", "create_mea_heatmaps_enhanced", "plot_mea_metric")) {
    expect_true(is.function(get(fn)), info = fn)
  }
})

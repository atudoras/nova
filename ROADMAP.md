# NOVA Dynamics — Roadmap

NOVA 0.2.0 introduced `nova_dynamics`: a dynamical-systems layer that turns the
existing PCA/UMAP state space into quantified trajectory geometry, transitions,
similarity, regimes, landscapes, and a rule-based interpretation engine.

This document specifies the **next five modules**. It defines their scientific
rationale, public interfaces, return contracts, and dependencies so they can be
implemented consistently. **None of these are implemented yet** — this is an
architecture specification, not code.

## Design invariants (apply to every future module)

1. **Embedding-agnostic input.** Accept the same `x` as the current module: a
   `pca_analysis_enhanced()` result, a bare data frame with embedding +
   timepoint + grouping columns, or a `nova_trajectories` object from
   `nova_extract_trajectories()`.
2. **Additive, no breakage.** New `R/dynamics_<name>.R` files only; never modify
   existing signatures or exports.
3. **No new hard dependencies.** Heavy methods get a base-R reference
   implementation; specialist packages enter `Suggests` behind
   `requireNamespace()` guards with graceful fallbacks.
4. **Uniform return contract.** Return an S3 object
   `class = c("nova_<name>", "nova_dynamics_result", "list")` carrying metric
   tables, a `$plots` list of ggplot objects styled with `nova_theme()` /
   `nova_palette()`, and a `$params` record. Provide a `nova_describe()` method.
5. **Baseline-first, real-time ordering.** Reuse `nova_order_timepoints()` and
   `time_numeric` everywhere.
6. **Honest uncertainty.** Where postmortem/in-vitro sample sizes or timepoint
   counts are too small to support a method, cap confidence and say so in
   `nova_describe()` (as `nova_dynamical_regime()` already does).

---

## 1. Attractor analysis — `nova_attractors()`  *(priority: high)*

**Scientific rationale.** The current regime detector infers attractor-like
behaviour indirectly (deceleration, settling). Attractor analysis estimates the
attracting sets directly: fixed points, limit cycles, and their basins. This is
the natural completion of "what state is the network approaching?".

**Mathematical approach.** With replicate trajectories as samples of a flow,
estimate a local linear vector field $\dot{x}=f(x)$ by regressing finite-
difference velocities on position over a neighbourhood grid; locate fixed points
where $\lVert\hat f\rVert\approx 0$ and classify them from the Jacobian
eigenvalues (stable node/spiral, saddle, unstable). Detect limit cycles via
recurrence of the phase angle / Poincaré-section returns. Basins from forward
integration of the estimated field.

**Interface.**
```r
nova_attractors(x, dims = c("PC1","PC2"), group_var = NULL,
                grid_n = 25, neighborhood = 0.2, method = c("local_linear","rbf"),
                ...)
# returns: $fixed_points (tibble: coord, type, eigenvalues),
#          $vector_field (grid tibble), $basins (grid labels),
#          $plots = list(vector_field, fixed_points, basins)
```
**Dependencies.** Base R for local-linear fit; optional `deSolve` (Suggests) for
basin integration; falls back to discrete forward iteration.

---

## 2. Resilience analysis — `nova_resilience()`  *(priority: high)*

**Scientific rationale.** Does the network return to baseline after a
perturbation (drug washout, stimulus), and how fast? Resilience quantifies
recovery — central to plasticity, homeostasis, and toxicity screening.

**Mathematical approach.** Given a perturbation timepoint, fit recovery kinetics
of the displacement-from-baseline curve $d(t)$: recovery half-time $t_{1/2}$,
asymptotic offset (incomplete recovery), and an exponential/biexponential rate.
Estimate an empirical "return rate" as the slope of $\log d(t)$ post-peak; report
a resilience index in $[0,1]$ (1 = full fast recovery, 0 = no return).

**Interface.**
```r
nova_resilience(x, perturbation_time, baseline_window = NULL,
                dims = c("PC1","PC2"), group_var = NULL,
                model = c("exp","biexp","empirical"), ...)
# returns: $kinetics (tibble: group, t_half, asymptote, rate, resilience_index),
#          $plots = list(recovery_curves, resilience_bars)
```
**Dependencies.** `stats::nls` (base) with an empirical fallback when `nls`
fails to converge (common with short series).

---

## 3. Criticality analysis — `nova_criticality()`  *(priority: medium)*

**Scientific rationale.** Healthy cortical networks operate near a critical
point (neuronal avalanches, scale-free dynamics). Distance-to-criticality is a
candidate biomarker for excitation/inhibition balance and disease.

**Mathematical approach.** Two complementary readouts: (a) **avalanche
statistics** from event/burst data — fit avalanche size/duration distributions,
test power-law vs lognormal, estimate the exponent and the
crackling-noise relation $\langle S\rangle\sim D^{\,1/\sigma\nu z}$; (b)
**state-space criticality** — branching ratio from the embedding via
autoregressive estimation ($\hat m\to 1$ at criticality) and the largest
eigenvalue of the lag-1 autocovariance (critical slowing down).

**Interface.**
```r
nova_criticality(x, event_column = NULL, dims = c("PC1","PC2"),
                 group_var = NULL, method = c("branching","avalanche"), ...)
# returns: $metrics (tibble: group, branching_ratio, distance_to_critical,
#                     power_law_p, exponent),
#          $plots = list(avalanche_distribution, branching, slowing_down)
```
**Dependencies.** Base R; optional `poweRlaw` (Suggests) for rigorous
distribution fitting, with a maximum-likelihood fallback.

---

## 4. Learning analysis — `nova_learning()`  *(priority: medium)*

**Scientific rationale.** In stimulation/training paradigms, does the network's
*response* to a repeated stimulus change over trials? Learning analysis tracks
trial-to-trial drift, habituation, and potentiation in state space.

**Mathematical approach.** Treat each trial as a short trajectory; align trials
and measure (a) drift of the response vector across trials (systematic vs
random), (b) change in response magnitude (habituation = decay, potentiation =
growth), and (c) convergence of trial endpoints (consolidation). Fit a
trial-index regression on the response metric and test for monotone trend.

**Interface.**
```r
nova_learning(x, trial_var, stimulus_window = NULL,
              dims = c("PC1","PC2"), group_var = NULL, ...)
# returns: $trends (tibble: group, drift, habituation_rate, consolidation),
#          $plots = list(trial_trajectories, response_over_trials)
```
**Dependencies.** Base R (`lm`, trend tests).

---

## 5. Closed-loop benchmarking — `nova_closed_loop()`  *(priority: exploratory)*

**Scientific rationale.** Closed-loop / adaptive-stimulation experiments need a
standard way to score whether stimulation drove the network toward a *target*
state and held it there. This module is the evaluation harness for control
experiments.

**Mathematical approach.** Given a target state (or target region) and the
realised trajectory under control, compute time-to-target, steady-state error
(residual distance to target), overshoot, settling time, and a controllability
score; compare against an open-loop baseline trajectory. Borrows control-theory
metrics (rise/settling/overshoot) applied in state space.

**Interface.**
```r
nova_closed_loop(x, target, control_var, baseline_label = NULL,
                 dims = c("PC1","PC2"), ...)
# returns: $performance (tibble: condition, time_to_target, ss_error,
#                         overshoot, settling_time, controllability),
#          $plots = list(target_approach, performance_summary)
```
**Dependencies.** Base R.

---

## Sequencing rationale

| Order | Module | Why now |
|------:|--------|---------|
| 1 | Attractors | Directly completes the v0.2.0 regime story; reuses geometry primitives. |
| 2 | Resilience | High experimental demand (washout/recovery); small, well-posed addition. |
| 3 | Criticality | High scientific value; needs event-level data so it is a larger lift. |
| 4 | Learning | Requires trial-structured designs; narrower audience. |
| 5 | Closed-loop | Depends on adaptive-stimulation hardware; most specialised. |

Each module ships with: roxygen docs + examples, a `testthat` file with
deterministic correctness checks, a `nova_describe()` method, and a vignette
section. Backward compatibility is non-negotiable.

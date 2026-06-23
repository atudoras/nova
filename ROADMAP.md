# NOVA — Roadmap

NOVA's philosophy is to report **only what the data can support**. MEA
timecourses typically have few timepoints and a modest number of replicate
wells, so the trajectory layer stays deliberately descriptive
(`nova_trajectory_summary()`): distance from baseline over time, path directness,
and timing.

## Near-term

- Optional confidence intervals on `net_displacement` and `directness` via
  replicate bootstrapping (uses the per-well structure already extracted).
- Pairwise condition comparison: a simple, well-justified distance between
  conditions' distance-from-baseline profiles (no time-warping / clustering
  claims).
- Per-metric (not just PCA) trajectory summaries.

## Deferred until experiments support them

Richer dynamical-systems analyses — velocity/acceleration fields, attractor and
basin estimation, transition/Markov models, criticality, and regime
classification — require many more timepoints and replicates than a standard MEA
agonist run provides. They were prototyped in 0.2.0 and intentionally removed in
0.3.0 to avoid presenting noise as dynamics. They will only return behind clear
data-sufficiency checks, and as opt-in functions that state their assumptions.

Suggestions and use cases welcome via
[GitHub Issues](https://github.com/atudoras/nova/issues).

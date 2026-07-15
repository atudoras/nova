# Source — spike-level dataset (raw → NOVA metrics)

> **STATUS: NOT YET SELECTED.** Template. Fill from the actual download.

| Field | Value |
|-------|-------|
| Dataset | *TBD — meaRtools bundled example spike data, or a small Zenodo/DANDI spike-list set* |
| URL | *TBD* |
| Accessed | *TBD* |
| SHA-256 | *TBD* |
| Licence | *TBD — record explicitly; unlike the EPA data this is not automatically public domain* |
| Format | spike times per electrode |

## Purpose

Case study 01 uses NOVA's native input: a metrics table. This one asks the
upstream question — can NOVA consume data that starts as spike times? — and
registers the answer as a candidate ingestion feature (`ROADMAP.md`).

The extraction step (`extract_metrics.R`, wrapped as an experimental
`nova_metrics_from_spikes()`) computes NOVA's core metrics from spike times:
mean firing rate, active electrodes by rate threshold, ISI-threshold bursts,
network bursts, and STTC synchrony. It leans on `meaRtools` where that package
already implements these, and cites it rather than reimplementing.

**`meaRtools` is CRAN-archived and not currently installed.** It is to be pinned
from the CRAN archive via renv; whether it builds against R 4.4.2 is unverified
and must be checked before this study is planned in detail.

The wrapper stays **experimental and outside the package build** until it has been
validated against values the source dataset reports independently. An extraction
step that produces plausible numbers nobody has checked is worse than no
extraction step, because everything downstream inherits the error silently.

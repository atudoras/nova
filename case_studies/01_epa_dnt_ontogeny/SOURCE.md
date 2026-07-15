# Source — EPA/Shafer MEA developmental neurotoxicity screen (Brown et al. 2016)

| Field | Value |
|-------|-------|
| Dataset | Data for Brown et al MEA Developmental Neurotoxicity Screening Manuscript |
| Catalog | <https://catalog.data.gov/dataset/data-for-brown-et-al-mea-developmental-neurotoxicity-screening-manuscript> |
| File URL | <https://pasteur.epa.gov/uploads/10.23719/1412682/Final_Data_Set_SA1_DNT_Paper1%20%282%29%28updated%29_CF.csv> |
| Local file | `data/brown2016_mea_dnt.csv` (gitignored) |
| Size | 210,268 bytes |
| SHA-256 | `80522f6654012a18851ad17f180f51d98df6b84ab122078ce303c4d3c079f43a` |
| Downloaded | 2026-07-14 |
| Server Last-Modified | 2017-12-24 |
| Licence | US Government work — public domain, 17 U.S.C. §105. Verified on the EPA ScienceHub licence page: no attribution or redistribution conditions; EPA disclaims warranty as to accuracy, and its seal may not be used to imply endorsement. |
| Publisher | U.S. EPA Office of Research and Development (ORD) |
| Contact | Timothy Shafer (shafer.tim@epa.gov) |
| Publication | Brown J, Hall D, Frank C, Wallace K, Mundy W, Shafer T (2016). "Evaluation of a Microelectrode Array-based Assay for Neural Network Ontogeny using Training Set Chemicals." *Toxicological Sciences* 154(1):126–139. |
| Instrument | Axion Maestro, 48-well MEA plates |

**Publisher's own note, carried forward:** "These data are the individual parameter
and well-level data that were support the conclusions in Brown et al. Note: the
parameters CVtime and CVnetwork were not used." → `cv.time` and `cv.network` are
therefore **excluded**.

## Verified structure

Confirmed by inspecting the downloaded file, not inferred from the paper.

- **990 rows × 26 columns**, wide: one row per well × DIV, metrics as columns.
- **6 experiments**, being 6 unique `(date, Plate.SN)` pairs — see the identity note below.
- **256 wells** (unique `date × Plate.SN × well`); 48 well IDs per plate (A1…).
- **4 timepoints**: DIV 5, 7, 9, 12. 222 wells have all four, 34 have three (990 = 222×4 + 34×3).
- **7 compounds**, each well carrying exactly one compound at one dose for the whole run
  (12-day exposure). Doses: 0, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30 µM. `dose = 0`
  is that compound's vehicle control.
- **6 culture dates** (batches): 20140205, 20140212, 20140402, 20140423, 20140716, 20140730.
- No duplicate `(well, DIV)` rows.

### Identity: `Experiment` must be `(date, Plate.SN)`, not `Plate.SN`

`Plate.SN` is a **physical plate serial number, and plates are reused across culture
dates** — MW1007-26 and MW1007-27 each appear on two dates. There are 4 serial numbers
but 6 actual experiments.

Mapping `Experiment = Plate.SN` would merge well A1 of MW1007-26 on 2014-02-05 with
well A1 of MW1007-26 on 2014-02-12: two different cultures, one identity. That is
exactly the cross-plate collision NOVA 0.4.0 fixed, reintroduced through the adapter.
`Experiment = paste(date, Plate.SN)`.

Well IDs are ambiguous by a factor of ~3.7 in this dataset: **48 unique well IDs vs 178
unique `Plate.SN × well`, and 256 unique `date × Plate.SN × well`.**

### Metrics (16 usable)

`meanfiringrate`, `burst.per.min`, `mean.isis`, `per.spikes.in.burst`, `mean.dur`,
`mean.IBIs`, `nAE`, `nABE`, `ns.n`, `ns.peak.m`, `ns.durn.m`,
`ns.percent.of.spikes.in.ns`, `ns.mean.insis`, `ns.durn.sd`, `ns.mean.spikes.in.ns`, `r`.

**Missingness is structural, not random.** The network-spike metrics (`ns.*`) are 42% NA
and the burst-interval metrics (`mean.isis`, `mean.dur`, `mean.IBIs`) 33% NA, because a
well with no bursts has no burst duration to report. NA here means "did not burst", not
"measurement lost". Mean-imputing it — NOVA's `pca_analysis_enhanced(impute = TRUE)`
default — would invent burst statistics for wells that never burst.

### DIV 5 cannot serve as a normalisation baseline

At DIV 5, a large share of wells read exactly zero:

| Metric | Zero at DIV 5 | Median at DIV 5 |
|--------|---------------|-----------------|
| `ns.n` (network spikes) | 236/256 (92.2%) | 0 |
| `burst.per.min` | 168/256 (65.6%) | 0 |
| `r` (synchrony) | 112/256 (43.8%) | 0 |
| `meanfiringrate` | 107/256 (41.8%) | 0.21 |
| `nAE` (active electrodes) | 107/256 (41.8%) | 1 |

This is the assay working as intended — cortical networks are near-silent at DIV 5 and
develop activity through DIV 12; that development *is* the measured phenomenon. But it
means a fold-change against DIV 5 divides by zero for 42–92% of wells, which NOVA
correctly returns as `NA`. Baseline-normalising to the earliest DIV would discard most
of the dataset and keep a biased remnant: precisely the wells that were already active
at DIV 5.

**The Phase-0 assumption "baseline = earliest DIV" does not hold for this dataset.**

## Caveats to carry into any analysis

- **DIV 5 is already exposed.** Compounds are applied from the start of the 12-day
  culture, so no timepoint here is pre-treatment. "Change from DIV 5" is not "effect of
  compound".
- **Dose is nested within compound** and the discovery engine treats groups as flat.
- **DIV is a repeated measure** on the same wells; unmodelled.
- **Plate and batch structure**: wells nested in plates, plates nested in culture dates,
  and plate serial numbers reused across dates. Unmodelled.
- **Glyphosate appears only at dose 0** (14 rows, no treated wells in this file) — it
  cannot support a dose-response and should not be read as a tested negative here.
- "Separates neuroactive from inactive" would be a statement about prediction, not about
  neurotoxicity in vivo.

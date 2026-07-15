# Source — EPA/Shafer developmental neurotoxicity MEA screen

> **STATUS: NOT YET ACQUIRED.** This file is a template. Every field below must be
> filled from the actual download before any analysis is run or reported. Do not
> populate it from memory or from the citation alone — links drift, and a
> provenance record that was guessed is worse than none.

| Field | Value |
|-------|-------|
| Dataset | *TBD — locate via catalog.data.gov / EPA CompTox / EDAP* |
| Primary citation | Brown JP et al. (2016) *Toxicol Sci* 154(1):126–139, "Evaluation of a Microelectrode Array-Based Assay for Neural Network Ontogeny Using Training Set Chemicals" |
| Related | Frank CL et al. (2017) *Toxicol Appl Pharmacol* |
| URL | *TBD — record the exact URL used* |
| Accessed | *TBD* |
| File(s) | *TBD* |
| SHA-256 | *TBD — record per file* |
| Licence | US Government work — public domain (17 U.S.C. §105). **Verify on the landing page**; EPA pages occasionally carry additional terms. |
| Instrument | Axion Maestro MEA |
| Format | *TBD — expected published long/tidy table, NOT the Axion row-121 CSV export* |

## Structure

*TBD after inspection.* Expected, to be confirmed rather than assumed:

- chemicals × concentrations (dose–response)
- DIV ontogeny (repeated measures on the same wells)
- replicate wells nested in plates

## Mapping to the NOVA schema

The published table is expected to be a tidy export, **not** the Axion
"Neural Metrics" CSV that `process_mea_flexible()` parses. `_shared/adapt_epa.R`
therefore constructs the processed schema directly and does its own baseline
normalisation:

| NOVA column | EPA source | Notes |
|-------------|-----------|-------|
| `Experiment` | plate ID | Load-bearing. Well IDs repeat on every plate; without this, baseline normalisation matches wells to other plates' baselines. See NOVA 0.4.0 NEWS. |
| `Well` | well ID | |
| `Treatment` | compound (and/or dose) | |
| `Timepoint` | DIV | `nova_time_to_minutes()` parses `"DIV7"` natively and treats `"DIV0"` as baseline-like. |
| `Variable` | metric name | |
| `Value` | metric value | |
| `Normalized_Value` | computed | `Value / baseline`, keyed on `(Experiment, Well, Variable, Treatment)` |

## Caveats to carry into the analysis

- **Dose is nested in compound**, and the discovery engine treats groups as flat.
  Whatever `Treatment` is set to determines what the ranking compares — record the
  choice here once made.
- **DIV is a repeated measure** on the same wells; the engine does not model that.
- **Plate/batch structure** is present and unmodelled.
- **Baselines near zero** are plausible at early DIV for firing-rate metrics. The
  engine excludes those wells from the ranking and reports the count; check it.
- "Separates neuroactive from inactive" would be a statement about prediction, not
  about neurotoxicity in vivo.

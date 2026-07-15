# Case studies

What NOVA finds in other people's data.

Each study takes a published, openly available MEA dataset, runs NOVA's standard
pipeline over it with no dataset-specific tuning, and reports what came out.
Everything shown is picked by effect-size rank — not by eye.

---

## 01 · Can a generic pipeline pick out the neurotoxins?

**The data.** The US EPA's developmental-neurotoxicity screen. Rat cortical networks grown
on 48-well Axion MEAs and dosed from day one — six compounds, nine concentrations each
plus vehicle — recorded as the networks form, at DIV 5, 7, 9 and 12. Six cultures,
256 wells. Public domain.

> Brown JP, Hall DL, Frank CL, Wallace K, Mundy WR, Shafer TJ (2016).
> *Evaluation of a Microelectrode Array-Based Assay for Neural Network Ontogeny Using
> Training Set Chemicals.* **Toxicological Sciences** 154(1):126–139.
> [doi:10.1093/toxsci/kfw147](https://doi.org/10.1093/toxsci/kfw147) ·
> [dataset](https://catalog.data.gov/dataset/data-for-brown-et-al-mea-developmental-neurotoxicity-screening-manuscript)

**What NOVA found.** All 25 of the 25 largest changes belong to the five compounds the
paper calls active. Acetaminophen — its designated negative — never appears. Sodium
orthovanadate takes network synchrony down to **1% of vehicle**, in dose order. NOVA was
told none of this: it normalised to the vehicle wells, ran a PCA, and sorted by effect
size.

**[📓 Notebook](01_epa_dnt_ontogeny/summary.html)  ·  [📄 3-page brief](01_epa_dnt_ontogeny/summary.pdf)**

*Exploratory.* It recovers labels the paper had already assigned — a claim about
prediction, not about neurotoxicity. And 20 of those 25 are compounds the paper reports
as cytotoxic, with no viability data here to tell "changed the network" from "killed the
cells". The notebook says all of this properly.

---

## 02 · Can NOVA read raw spikes?

Scaffolded. Study 01 starts from a metrics table; this one starts from spike times and
asks whether the extraction step belongs inside NOVA.

---

## Reproduce

Data is gitignored — each study's `SOURCE.md` carries the URL, licence and SHA-256.

```r
devtools::load_all(".")                                     # from the repo root
source("case_studies/01_epa_dnt_ontogeny/run_discovery.R")  # -> outputs/discovery/findings.csv
```

```sh
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to html
quarto render case_studies/01_epa_dnt_ontogeny/summary.qmd --to typst   # PDF, no LaTeX needed
```

Two stages, kept apart on purpose. `run_discovery.R` scans every metric and every group
and ranks the lot ([`_shared/discovery_run.R`](_shared/discovery_run.R)); `summary.qmd`
curates from that ranking. So everything in a summary is traceable to a row in
`findings.csv` rather than to somebody's taste.

`renv.lock` records the 126 package versions these results were produced with —
`renv::restore(project = "case_studies")` if you want them exactly.

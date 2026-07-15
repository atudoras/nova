# Case studies

Cool things NOVA found in other people's data, with barely any code.

One folder, one notebook. Each takes a published, openly available MEA dataset, runs a
basic NOVA analysis, and shows what came out. Studies land here when there's something
worth showing.

| | Study | Data |
|---|---|---|
| 🧪 | **[Can a generic pipeline pick out the neurotoxins?](epa_dnt/notebook.md)** — all 25 of the top 25 changes are the five compounds the paper calls active; the designated negative never appears. Sodium orthovanadate takes synchrony to 1% of vehicle, in dose order. | [US EPA / Brown et al. 2016](https://doi.org/10.1093/toxsci/kfw147) |

Each notebook downloads its own data and checks the hash, so it runs from a clone:

```sh
quarto render case_studies/epa_dnt/notebook.qmd --to gfm
```

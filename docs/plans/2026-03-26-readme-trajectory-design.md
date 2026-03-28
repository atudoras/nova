# NOVA README Trajectory Plot Design

**Date:** 2026-03-26
**Status:** Approved

## Goal

Produce one publication-quality trajectory plot for the GitHub README that immediately communicates what NOVA does: track neural network activity through PCA space as treatments unfold over time.

## Plot Specification

### Data
- Source: `Example/MEA Neuronal Agonists/my_data.RData` → `pca_output$plot_data`
- Groups: 4 treatment-level group averages (PBS, KA, Gabazine, NMDA), averaged across both genotypes and all wells
- Timepoints: all 7 (baseline, 15min, 30min, 1h, 1h30min, 2h) in chronological order
- Axes: PC1 (39.6%) × PC2 (25.8%)

### Color Palette
| Treatment | Hex     | Rationale                        |
|-----------|---------|----------------------------------|
| PBS       | #6B7280 | Neutral gray — control baseline  |
| KA        | #EF4444 | Red — seizure/high-salience      |
| Gabazine  | #3B82F6 | Blue — inhibitory blocker        |
| NMDA      | #10B981 | Emerald — excitatory receptor    |

### Visual Elements
- **Lines:** `linewidth = 2.5`, `alpha = 0.9`
- **Timepoint dots:** filled circles, `size = 3`, same color as line
- **Start (baseline):** open diamond shape `(shape = 5)`, `size = 5`, distinguishes origin
- **End (2h):** filled circle with black outline `(shape = 21, fill = treatment_color)`, `size = 5`
- **Directional arrows:** small arrowhead on final segment of each trajectory via `geom_segment(arrow = arrow(length = unit(0.15, "cm"), type = "closed"))`
- **Labels:** treatment name at endpoint, `ggrepel::geom_text_repel`, `size = 4`, `fontface = "bold"`, no legend box needed

### Theme
- Base: `theme_minimal()`
- Background: white (`#FFFFFF`)
- Panel border: black, `linewidth = 0.8`
- Major gridlines: `#F0F0F0`, `linewidth = 0.3`
- No minor gridlines
- No plot title (README section heading provides context)
- Axis labels: `"PC1 (39.6%)"` / `"PC2 (25.8%)"`, bold, size 12
- Axis text: size 10
- No legend (treatment labels are on the plot via ggrepel)

### Output
- File: `docs/user-guide/figures/readme_trajectory.png`
- Dimensions: 7 × 6.5 inches
- DPI: 300
- Format: PNG, white background

## README Integration

### Placement
After the feature bullets, before "Installation" — first visual a visitor sees.

### HTML block
```html
<p align="center">
  <img src="docs/user-guide/figures/readme_trajectory.png" width="75%" alt="NOVA PCA trajectory analysis">
</p>
<p align="center"><em>Four treatment groups traced through PCA space over 7 timepoints. PBS (control) remains near the origin; KA, Gabazine and NMDA each drive distinct network-level responses.</em></p>
```

## Overall Task Scope (this implementation plan covers)

1. **CRAN readiness tasks 5–11** (remaining from 2026-03-25-cran-readiness.md):
   - Task 5: cat() → message() in data_handling.R
   - Task 6: cat() → message() in plots.R
   - Task 7: Remove print() side-effects
   - Task 8: aes_string() → .data[[]]
   - Task 9: linewidth fix
   - Task 10: globalVariables
   - Task 11: Final check

2. **Generate readme_trajectory.png** from real data per this spec

3. **Update README.md** in worktree with trajectory figure + polish

4. **Push to GitHub** — merge `claude/nova-features` → `main`, tag as v0.1.1

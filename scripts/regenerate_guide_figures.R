# Regenerates all user-guide figures from the cached nova_results.rds
# Run once after any visual changes to the package.
suppressMessages({ devtools::load_all("."); library(ggplot2) })

OUT <- "docs/user-guide/figures"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

nova <- readRDS("/tmp/nova_results.rds")   # adjust path if needed

save_png <- function(p, name, w=1400, h=900, res=120) {
  png(file.path(OUT, paste0(name, ".png")), width=w, height=h, res=res)
  print(p); dev.off()
  cat("Saved:", name, "\n")
}

# PCA scatter
save_png(nova$pca$plots$scatter_treatment_genotype, "pca_primary")
save_png(nova$pca$plots$elbow,                      "pca_elbow", w=900, h=600)

# Trajectories: combined avg
save_png(nova$trajectories$plots[["combined_avg"]], "traj_combined_avg", h=800)

# Heatmap: treatment
pheatmap::pheatmap(nova$heatmaps$treatment_heatmap$scaled_data,
  annotation_col = nova$heatmaps$treatment_heatmap$annotation,
  main = "MEA Variables by Treatment (Z-score)")
dev.copy(png, file.path(OUT, "heatmap_treatment.png"), width=1200, height=900, res=120)
dev.off()

cat("\nAll figures saved to", OUT, "\n")

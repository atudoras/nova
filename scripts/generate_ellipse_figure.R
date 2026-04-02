## Generate README ellipse showcase figure
## 95% CI ellipses per treatment group — all timepoints overlaid

library(NOVA)
library(ggplot2)
library(dplyr)

DATA_DIR <- "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features/Example/MEA Neuronal Agonists"
OUT_FILE <- "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features/docs/user-guide/figures/readme_ellipses.png"

treatment_colors <- c(
  PBS      = "#6B7280",
  KA       = "#EF4444",
  Gabazine = "#3B82F6",
  NMDA     = "#10B981"
)

# Load pre-computed data
load(file.path(DATA_DIR, "my_data.RData"))

# pca_output already computed and saved in RData
plot_data <- pca_output$plot_data
pc1_var   <- round(pca_output$variance_explained[1], 1)
pc2_var   <- round(pca_output$variance_explained[2], 1)

# Keep only post-baseline timepoints for a cleaner picture
timepoints_keep <- c("0min", "15min", "30min", "1h", "1h30min", "2h")
pd <- plot_data %>%
  filter(Timepoint %in% timepoints_keep) %>%
  filter(Treatment %in% names(treatment_colors))

p <- ggplot(pd, aes(x = PC1, y = PC2, colour = Treatment, fill = Treatment)) +

  # 95% CI ellipses — filled, semi-transparent
  stat_ellipse(
    type    = "norm",
    level   = 0.95,
    geom    = "polygon",
    alpha   = 0.12,
    linewidth = 0
  ) +
  # Ellipse borders
  stat_ellipse(
    type      = "norm",
    level     = 0.95,
    linewidth = 1.1,
    alpha     = 0.85
  ) +

  # Individual points — small, slightly transparent
  geom_point(
    size  = 1.6,
    alpha = 0.45,
    shape = 16
  ) +

  # Group centroids
  stat_summary(
    fun      = mean,
    geom     = "point",
    size     = 4.5,
    shape    = 18,
    alpha    = 1
  ) +

  scale_colour_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +

  labs(
    x       = paste0("PC1  (", pc1_var, "% variance)"),
    y       = paste0("PC2  (", pc2_var, "% variance)"),
    colour  = "Treatment",
    fill    = "Treatment",
    caption = "95% confidence ellipses  ·  diamond = group centroid  ·  post-baseline timepoints pooled"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "#FAFAFA", colour = NA),
    panel.grid.major   = element_line(colour = "#E5E7EB", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.title         = element_text(size = 11.5, colour = "#374151"),
    axis.text          = element_text(size = 10,   colour = "#6B7280"),
    legend.title       = element_text(size = 11,   face = "bold", colour = "#1F2937"),
    legend.text        = element_text(size = 10.5, colour = "#374151"),
    legend.key.size    = unit(1.1, "lines"),
    legend.position    = "right",
    plot.caption       = element_text(size = 8.5,  colour = "#9CA3AF", hjust = 0.5,
                                      margin = margin(t = 8)),
    plot.margin        = margin(16, 16, 10, 16)
  ) +

  coord_fixed(ratio = 1)

ggsave(OUT_FILE, plot = p, width = 7, height = 5.5, dpi = 300, bg = "white")
message("Saved: ", OUT_FILE)

# Generate publication-quality README trajectory figure
# Design spec: docs/plans/2026-03-26-readme-trajectory-design.md

library(dplyr)
library(ggplot2)
library(ggrepel)

# ── Load data ──────────────────────────────────────────────────────────────────
data_path <- file.path(
  dirname(getwd()),  # up from worktree to NOVA copy 2
  "../../Example/MEA Neuronal Agonists/my_data.RData"
)
# Direct absolute path as fallback:
data_path <- "/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/Example/MEA Neuronal Agonists/my_data.RData"
load(data_path)
plot_data <- pca_output$plot_data

# ── Timepoint order ────────────────────────────────────────────────────────────
tp_order <- c("baseline", "15min", "30min", "1h", "1h30min", "2h")
plot_data <- plot_data %>%
  filter(Timepoint %in% tp_order) %>%
  mutate(
    Timepoint = factor(Timepoint, levels = tp_order),
    tp_rank   = as.integer(Timepoint)
  )

# ── Treatment palette ──────────────────────────────────────────────────────────
treatment_colors <- c(
  "PBS"      = "#6B7280",
  "KA"       = "#EF4444",
  "Gabazine" = "#3B82F6",
  "NMDA"     = "#10B981"
)

# ── Group averages per Treatment x Timepoint ───────────────────────────────────
traj <- plot_data %>%
  group_by(Treatment, Timepoint, tp_rank) %>%
  summarise(
    mean_PC1 = mean(PC1, na.rm = TRUE),
    mean_PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Treatment, tp_rank)

# ── Start and end points ───────────────────────────────────────────────────────
starts <- traj %>% group_by(Treatment) %>% slice_min(tp_rank, n = 1) %>% ungroup()
ends   <- traj %>% group_by(Treatment) %>% slice_max(tp_rank, n = 1) %>% ungroup()

# ── Arrow segments (final segment of each trajectory) ─────────────────────────
arrow_segs <- traj %>%
  group_by(Treatment) %>%
  slice_tail(n = 2) %>%
  summarise(
    x    = first(mean_PC1),
    y    = first(mean_PC2),
    xend = last(mean_PC1),
    yend = last(mean_PC2),
    .groups = "drop"
  )

# ── Build plot ─────────────────────────────────────────────────────────────────
p <- ggplot(traj, aes(x = mean_PC1, y = mean_PC2,
                       color = Treatment, group = Treatment)) +

  # Trajectory lines
  geom_path(linewidth = 2.2, alpha = 0.85,
            lineend = "round", linejoin = "round") +

  # Timepoint dots
  geom_point(size = 2.8, alpha = 0.9) +

  # Arrow on final segment (drawn over lines)
  geom_segment(
    data = arrow_segs,
    aes(x = x, y = y, xend = xend, yend = yend, color = Treatment),
    linewidth = 2.2,
    arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
    show.legend = FALSE
  ) +

  # Start: open diamond
  geom_point(
    data = starts,
    aes(x = mean_PC1, y = mean_PC2, color = Treatment),
    shape = 5, size = 5, stroke = 1.8,
    show.legend = FALSE
  ) +

  # End: filled circle with black outline
  geom_point(
    data = ends,
    aes(x = mean_PC1, y = mean_PC2, fill = Treatment),
    shape = 21, size = 5, color = "black", stroke = 1.2,
    show.legend = FALSE
  ) +

  # Treatment labels at endpoints
  ggrepel::geom_text_repel(
    data = ends,
    aes(x = mean_PC1, y = mean_PC2, label = Treatment, color = Treatment),
    size          = 4.2,
    fontface      = "bold",
    nudge_x       = 0.4,
    direction     = "y",
    segment.size  = 0.35,
    segment.alpha = 0.5,
    box.padding   = 0.4,
    point.padding = 0.4,
    show.legend   = FALSE
  ) +

  # Start/end legend hint
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "  o = start   * = end",
    hjust = 0, vjust = 1.6,
    size = 3.2, color = "gray45", fontface = "italic"
  ) +

  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values  = treatment_colors) +

  labs(
    x = "PC1 (39.6%)",
    y = "PC2 (25.8%)",
    color = NULL
  ) +

  theme_minimal(base_size = 12) +
  theme(
    aspect.ratio      = 1,
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid.major  = element_line(color = "#F0F0F0", linewidth = 0.35),
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(size = 12, face = "bold"),
    axis.text         = element_text(size = 10, color = "gray20"),
    axis.ticks        = element_line(color = "gray50", linewidth = 0.4),
    axis.ticks.length = unit(0.15, "cm"),
    legend.position   = "none",
    plot.margin       = margin(16, 20, 16, 16)
  )

# ── Save ───────────────────────────────────────────────────────────────────────
out_path <- "docs/user-guide/figures/readme_trajectory.png"
ggsave(
  filename = out_path,
  plot     = p,
  width    = 7,
  height   = 6.5,
  dpi      = 300,
  bg       = "white"
)
message("Saved: ", out_path)

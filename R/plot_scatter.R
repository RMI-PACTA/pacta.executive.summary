plot_scatter <- function(data) {
  score_bar <- ggplot(
    alignment_scores_values, 
    aes(
      x = .data$category, 
      y = .data$score_delta, 
      fill = factor(
        .data$score_symbol, 
        levels = rev(alignment_scores_values$score_symbol)
        )
      )
    ) +
    geom_bar(stat = "identity") +
    geom_point(
      data = data %>%
        mutate(category = "score"), 
      aes(
        color = .data$entity_type, 
        y = .data$score, 
        fill = "black", 
        shape = .data$entity_type, 
        size = 1.5),
      position = position_dodge(width=0.2)
      ) +
    scale_y_continuous(
      breaks = alignment_scores_values$score_label, 
      labels = alignment_scores_values$score_symbol,
      expand = expansion(mult = c(0,0.1))
      ) +
    scale_colour_2dii(colour_groups = data$entity_type) +
    scale_fill_manual(values = fill_colours_scores) +
    scale_shape_manual(
      values = c(16, 16, 1, 16),
      labels = r2dii.plot::to_title(levels(data$entity_type))
      ) +
    theme_2dii(
      base_size = 20
    ) + 
    theme(
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
      ) +
    labs(
      y = axis_labels_scatter["y"]
    )
 
  p <- ggplot(
    data, 
    aes(
      x = .data$tech_mix_green, 
      y = .data$score, 
      color = .data$entity_type, 
      shape = .data$entity_type, 
      size = 1.5)
    ) +
    geom_point() + 
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "#C0C0C0") +
    geom_hline(yintercept = alignment_scores_values$score_upper[2], linetype = "dashed", color = "#C0C0C0") +
    scale_y_continuous(
      limits = c(1, 100),
      breaks = alignment_scores_values$score_label, 
      labels = alignment_scores_values$score_symbol,
      expand = expansion(mult = c(0,0.1))
      ) +
    scale_x_continuous(
      labels = scales::percent, 
      limits = c(0, 1), 
      expand = expansion(mult = c(0,0.1))
      ) +
    scale_shape_manual(
      values = c(16, 16, 1, 16),
      labels = r2dii.plot::to_title(levels(data$entity_type))
      ) +
    scale_colour_2dii(colour_groups = data$entity_type) +
    theme_2dii(
      base_size = 20
    ) +
    theme(
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    ) +
    guides(shape = "none", size = "none")

  tech_mix <- tibble::tibble(
    tech_mix_green = seq(from = 0.05, to = 1, by = 0.05),
    category = "techmix"
  )
  
  tech_mix_bar_fut <- ggplot(
    tech_mix, 
    aes(x = .data$category, y = .data$tech_mix_green, fill = .data$tech_mix_green)
    ) +
    geom_bar(stat = "identity", position = "fill") + 
    geom_point(
      data = data %>%
        mutate(category = "techmix"), 
      aes(color = .data$entity_type, shape = .data$entity_type, size = 1.5),
      position = position_dodge(width=0.2)
      ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent, 
      expand = expansion(mult = c(0,0.1))
      ) +
    scale_fill_gradient(low = fill_colours_techmix["brown"], high = fill_colours_techmix["green"]) +
    scale_colour_2dii(colour_groups = data$entity_type) +
    scale_shape_manual(
      values = c(16, 16, 1, 16), 
      labels = r2dii.plot::to_title(levels(data$entity_type))
      ) +
    theme_2dii(
      base_size = 20
    ) + 
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none"
      ) +
    labs(y = axis_labels_scatter["x"])
  
  score_bar + p + guide_area() + tech_mix_bar_fut + 
    plot_layout(ncol = 2, widths = c(0.7, 3.8), heights = c(3.8, 1), guides = "collect") +
    theme(
      legend.position = "left",
      legend.title = element_text()
      ) +
    guides(
      size = "none", 
      shape = "none",
      fill = "none",
      colour = guide_legend(
        title = axis_labels_scatter["legend_title"], 
        override.aes = list(size = 4, shape = c(16, 16, 1, 16))
        )
      )
}

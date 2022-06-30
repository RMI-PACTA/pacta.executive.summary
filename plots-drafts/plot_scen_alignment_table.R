plot_scen_alignment_table <- function(data) {
  pj <- position_jitter(width = 0, height = 0.1, seed = 0)
  
  p <- ggplot(
    data,
    aes(
        x = entity, 
        y = aligned_scen_temp, 
        group = technology
      )
    ) +
    annotate(
      "rect",
      fill = palette_2dii_scenario %>% filter(label == "red") %>% pull(hex), 
      xmin = -Inf, 
      xmax = Inf, 
      ymin = .5, 
      ymax = 1.5,
      alpha = 0.25
      ) +
    annotate(
      "rect",
      fill = palette_2dii_scenario %>% filter(label == "dark_yellow") %>% pull(hex), 
      xmin = -Inf, 
      xmax = Inf, 
      ymin = 1.5, 
      ymax = 2.5,
      alpha = 0.25
      ) +
    annotate(
      "rect",
      fill = palette_2dii_scenario %>% filter(label == "dark_green") %>% pull(hex), 
      xmin = -Inf, 
      xmax = Inf, 
      ymin = 2.5, 
      ymax = 3.5,
      alpha = 0.25
      )+ 
    #geom_line(position = pj) +
    geom_point(
      aes(shape = entity, colour = green_brown, size = perc_aum),
      position = pj
      ) +
    scale_shape_manual(values = c(15, 0), labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))) +
    scale_colour_manual(values = c("#5B432B", "#5E8E5E")) +
    scale_x_discrete(labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))) + 
    scale_y_discrete(
      expand = expansion(add = c(0.5, 0.5)) 
    ) +
    theme_2dii(base_size = 20) +
    theme(
      legend.title = element_text(size = 16),
      #legend.position = "bottom",
      axis.title.x = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()
      ) +
    guides(
      color = guide_legend(order = 3, override.aes = list(shape = 15, size = 5), values = c("#5B432B", "#5E8E5E")), 
      size = guide_legend(order = 2, override.aes = list(shape = 15)), 
      shape = guide_legend(order = 1, override.aes = list(size = 5))
      )
  
  p
}
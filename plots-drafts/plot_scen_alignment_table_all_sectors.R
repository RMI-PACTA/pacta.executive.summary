plot_scen_alignment_table_all_sectors <- function(data) {
  size_lim <- c(min(data$perc_aum, na.rm = TRUE), max(data$perc_aum, na.rm = TRUE))
  size_range <- c(4,12)
  p_auto <- plot_scen_alignment_table_perc(data %>% filter(sector == "automotive")) +
  scale_size_continuous(
    limits = size_lim,
    range = size_range,
    labels = scales::percent
    ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Automotive",
    colour = "Technology",
    size = "Exposure\n(% of AUM)",
    shape = "Entity"
    ) + 
  theme(axis.title.y = element_blank()) +
  facet_wrap(~ technology, labeller = as_labeller(as_function(~ r2dii.colours:::make_pretty_labels(.x))), ncol = 3)

  p_fossil <- plot_scen_alignment_table_perc(data %>% filter(sector == "fossil_fuels")) +
    scale_size_continuous(
      limits = size_lim,
      range = size_range,
      labels = scales::percent
      ) +
    labs(
      title = "Fossil fuels",
      colour = "Technology",
      size = "Exposure\n(% of AUM)",
      shape = "Entity"
      ) + 
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
      ) +
    facet_wrap(~ technology, labeller = as_labeller(as_function(~ r2dii.colours:::make_pretty_labels(.x))), ncol = 3)
    
  p_power <- plot_scen_alignment_table_perc(data %>% filter(sector == "power")) +
    scale_size_continuous(
      limits = size_lim,
      range = size_range,
      labels = scales::percent
      ) +
    labs(
      title = "Power",
      colour = "Technology",
      size = "Exposure\n(% of AUM)",
      shape = "Entity"
      ) + 
    theme(axis.title.y = element_blank()) +
    facet_wrap(~ technology, labeller = as_labeller(as_function(~ r2dii.colours:::make_pretty_labels(.x))), ncol = 4)
  
  p_ylabel = ggplot(data.frame(l = "Aligned scenario temperature", x = 1, y = 1)) +
          geom_text(aes(x, y, label = l), angle = 90, size = 7) + 
          theme_void() +
          coord_cartesian(clip = "off")
  
  p_tech <- (p_power / p_fossil / p_auto) + 
    plot_layout(guides = "collect") #& 
    #theme(legend.position = "bottom", legend.box="vertical")
  
  p <- p_ylabel + p_tech + plot_layout(widths = c(1, 15))
  
  p
}
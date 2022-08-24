plot_exposures_survey <- function(data) {
  data <- data %>%
    mutate(
      entity = factor(.data$entity, levels = c("portfolio", "peers"))
    ) %>%
    dplyr::arrange(.data$entity)
  
  p <- ggplot(
    data, 
    aes(
      x = .data$entity, 
      y = .data$exposure_perc_aum, 
      fill = .data$sector, 
      alpha = .data$entity
      )
    ) +
    geom_bar(stat = "identity") +
    scale_x_discrete(labels = r2dii.plot::to_title) +
    scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = scales::percent) +
    r2dii.colours::scale_fill_2dii(palette = "pacta", colour_groups = data$sector) +
    scale_alpha_discrete(range = c(1, 0.7)) +
    theme_2dii(base_size = 24) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.ticks.x = element_blank()
      )
  p
}
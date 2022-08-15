plot_scores <- function(data) {
  data <- data %>%
    dplyr::inner_join(alignment_scores_values, by = c("score" = "score_symbol")) %>%
    select(-.data$category, -.data$score_delta, -.data$score_upper) %>%
    mutate(
      score_symbol = .data$score
      )
  
  p_portfolio <- plot_score_portfolio(data %>% filter(is.na(.data$sector))) +
    labs(subtitle = "Portfolio")
  
  p_power <- plot_score_sector(data, "power")
  
  p_auto <- plot_score_sector(data, "automotive")
  
  p_coal <- plot_score_sector(data, "coal")
  
  p_gas <- plot_score_sector(data, "gas")
  
  p_steel <- plot_score_sector(data, "steel")
  
  p_cement <- plot_score_sector(data, "cement")
  
  p_oil <- plot_score_sector(data, "oil")
  
  p_sector_up <- p_power + p_auto + p_coal + p_gas + plot_layout(ncol = 4)
  p_sector_down <- p_steel + p_cement + p_oil + plot_spacer() + plot_layout(ncol = 4)
  
  p <- p_portfolio + (p_sector_up / p_sector_down) + plot_layout(widths = c(1, 5))
  p
}

plot_score_portfolio <- function(data) {
  c_position = alignment_scores_values %>%
    filter(.data$score_symbol == "C") %>%
    pull(.data$score_label)
  
  p <- plot_basic_scorebar() + 
    geom_text(
      data = data %>% filter(.data$entity == "this_portfolio"), 
      x = 1.48, 
      label = "\u25B2", 
      angle = 270 / pi,
      size = 10,
      color = "#1c324f",
      aes(y = .data$score_label)
      ) +
    geom_text(
      data = data %>% filter(.data$entity != "this_portfolio"), 
      x = 1.48, 
      label = "\u25B2", 
      angle = 270 / pi,
      size = 7,
      color = "#66696b",
      aes(y = .data$score_label)
      ) +
    annotate("segment", x = 0.5, xend = 1.5, y = c_position, yend = c_position, size = 1.3) +
    theme(axis.line = element_blank())
  
  p
}

plot_score_sector <- function(data, sector) {
  data <- data %>% filter(.data$sector == .env$sector)
  
  c_position = alignment_scores_values %>%
    filter(.data$score_symbol == "C") %>%
    pull(.data$score_label)
  
  p <- plot_basic_scorebar() + 
    geom_text(
      data = data %>% filter(.data$entity == "this_portfolio"), 
      x = 1.48, 
      label = "\u25B2", 
      angle = 270 / pi,
      size = 10,
      color = "#1c324f",
      aes(y = .data$score_label)
      ) +
    geom_text(
      data = data %>% filter(.data$entity != "this_portfolio"), 
      x = 1.48, 
      label = "\u25B2", 
      angle = 270 / pi,
      size = 7,
      color = "#66696b",
      aes(y = .data$score_label)
      ) +
    annotate(
      "segment", 
      x = 0.5, 
      xend = 1.5, 
      y = c_position, 
      yend = c_position, 
      size = 1.5
      ) +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank()
      ) +
    labs(
      subtitle = r2dii.plot::to_title(sector) 
    )
  
  p
}

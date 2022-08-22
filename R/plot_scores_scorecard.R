plot_scores_scorecard <- function(data) {
  score_upper_min <- min(alignment_scores_values$score_upper)
  score_upper_max <- max(alignment_scores_values$score_upper)
  
  scores_labels <- c("A+", "A", "B", "C", "D", "E")
  c_position <- 2
  
  data_scores <- alignment_scores_values %>%
    mutate(
      score_symbol = factor(score_symbol, levels = rev(scores_labels)),
      y = abs(100 - score_upper + score_upper_min)
    )
  
  data_triangles <- tibble::tibble(.rows = nrow(data_scores) * 3)
  idx <- 0
  for (score in rev(scores_labels)) {
    idx <- idx + 1
    data_score <- data_scores %>%
      filter(
        score_symbol == score
      )
    
    data_triangles <- tibble::tibble(
      group = replicate(3, score),
      x = c(idx - 0.45, idx + 0.45, idx),
      y = c(data_score$y, data_score$y, data_score$y + 0.05 * score_upper_max)
    ) %>%
    rbind(data_triangles)
  }
    
  score_y <- data %>%
    dplyr::inner_join(alignment_scores_values, by = c("score" = "score_symbol")) %>%
    pull(score_upper)
  idx <- which(scores_labels == data$score[1])
  
  portfolio_pointer <- tibble::tibble(
    group = replicate(5, "portfolio"),
    x = c(idx, idx - 0.3, idx - 0.3, idx + 0.3, idx + 0.3),
    y = c(
      score_y + 0.075 * score_upper_max, 
      score_y + 0.115 * score_upper_max, 
      score_y + 0.155 * score_upper_max, 
      score_y + 0.155 * score_upper_max,
      score_y + 0.115 * score_upper_max
      )
  )
    
  p <- ggplot(
    data_scores,
    aes(x = score_symbol, y = y, fill = score_symbol, colour = score_symbol)
    ) +
    geom_bar(stat = "identity", width = 0.9) +
    geom_polygon(
      data = data_triangles, 
      aes(x = x, y = y, group = group, fill = group, colour = group)
      ) +
    geom_polygon(
      data = portfolio_pointer, 
      aes(x = x, y = y, group = group, fill = group, colour = group)
      ) +
    geom_text(aes(y = 6, label = score_symbol), colour = "black", size = 8) +
    annotate(
      "segment", 
      x = c_position + 0.5, 
      y = -1, 
      xend = c_position + 0.5, 
      yend = score_upper_max * 1.1, 
      colour = unname(fill_colours_scores["C"]),
      size = 1.1
        ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(values = fill_colours_scores) +
    scale_colour_manual(values = fill_colours_scores) +
    theme_2dii(base_size = 22) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
      ) +
    coord_flip()
  p
}

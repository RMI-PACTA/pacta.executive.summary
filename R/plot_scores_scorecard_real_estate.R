plot_scores_scorecard_real_estate <- function(data) {
  env <- list(data = substitute(data))
  check_data_scores_scorecard_re(data, env)

  facets <- intersect(c("directly held", "mortgages"), unique(data$asset_class))

  p <- plot_scores_scorecard_single_re(data) +
    theme(
      strip.text = element_text(face = "bold")
    ) +
    facet_wrap(
      ~ factor(.data$asset_class, levels = facets),
      labeller = as_labeller(r2dii.plot::to_title)
    )
  p
}

plot_scores_scorecard_single_re <- function(data) {
  data_scores <- scores_real_estate %>%
    mutate(
      score_symbol = factor(.data$score_symbol, levels = rev(scores_labels_re())),
      y = calc_y_position(data_scores = .env$scores_real_estate, x = .data$score_upper)
    )

  portfolio_pointers <- unique(data$asset_class) %>%
    purrr::map_df(~ get_portfolio_pointer_df(data, .x, data_scores) %>% mutate(asset_class = .x))

  fill_colours_extended <- c(
    fill_colours_scores_re,
    c("portfolio" = unname(fill_colours_entities_scores["portfolio"]))
  )

  p <- plot_scores_pyramide(
    data_scores,
    score_labels_ordered = scores_labels_re(),
    fill_colours_scores = fill_colours_scores_re
    ) +
    geom_polygon(
      data = portfolio_pointers,
      aes(
        x = .data$x,
        y = .data$y,
        group = .data$group,
        fill = .data$group,
        colour = .data$group
      )
    ) +
    scale_fill_manual(values = fill_colours_extended) +
    scale_colour_manual(values = fill_colours_extended)
  p
}

scores_labels_re <- function() {
  c("A", "B", "C", "D", "E", "F", "G")
}

check_data_scores_scorecard_re <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(data, c("asset_class", "score"))
  abort_if_invalid_values(data, "asset_class", c("directly held", "mortgages"))
  abort_if_invalid_values(data, "score", c("A", "B", "C", "D", "E", "F", "G"))
}

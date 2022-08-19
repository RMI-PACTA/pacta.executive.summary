#' Create a plot showing aggregated scores for portfolio and peers
#'
#' @param data A data frame. In principle, an output of `prep_scores()`.
#'   Requirements:
#'   * Must have columns: `asset_class`,`scope`,`entity`, `sector`, `score`.
#'   * `asset_class` must have a single value.
#'   * `scope` must be one of: "portfolio", "sector".
#'   * `entity` must have following values: "this_portfolio", "peers".
#'   * `sector` must be one of: "power", "automotive", "coal", "oil", "gas",
#'   "steel", "cement" or `NA` in case of `scope` == "portfolio".
#'   * `score` must be one of: "A+", "A", "B", "C", "D", "E".
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' plot_scores(toy_data_scores %>% filter(asset_class == "equity"))
plot_scores <- function(data) {
  env <- list(data = substitute(data))
  check_data_scores(data, env = env)

  data <- data %>%
    dplyr::inner_join(alignment_scores_values, by = c("score" = "score_symbol")) %>%
    select(-.data$category, -.data$score_delta, -.data$score_upper) %>%
    mutate(
      score_symbol = .data$score
    )

  p_portfolio <- plot_score_portfolio(data %>% filter(.data$scope == "portfolio")) +
    labs(subtitle = "Portfolio")

  p_power <- plot_score_sector(data, "power")

  p_auto <- plot_score_sector(data, "automotive")

  p_coal <- plot_score_sector(data, "coal")

  p_gas <- plot_score_sector(data, "gas")

  p_steel <- plot_score_sector(data, "steel")

  p_cement <- plot_score_sector(data, "cement")

  p_oil <- plot_score_sector(data, "oil")

  p_sector_up <- p_power + p_auto + p_coal + p_gas + plot_layout(ncol = 4)
  p_sector_down <- p_steel + p_cement + p_oil + legend_scores() + plot_layout(ncol = 4)

  p <- p_portfolio + (p_sector_up / p_sector_down) + plot_layout(widths = c(1, 6))
  p
}

plot_score_portfolio <- function(data) {
  c_position <- alignment_scores_values %>%
    filter(.data$score_symbol == "C") %>%
    pull(.data$score_label)

  p <- plot_basic_scorebar() +
    annotate("segment", x = 0.5, xend = 1.5, y = c_position, yend = c_position, size = 1.3) +
    geom_text(
      data = data %>% filter(.data$entity == "this_portfolio"),
      x = 1.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 14,
      color = unname(fill_colours_entities_scores["portfolio"]),
      aes(y = .data$score_label)
    ) +
    geom_text(
      data = data %>% filter(.data$entity == "peers"),
      x = 1.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 11,
      color = unname(fill_colours_entities_scores["benchmark"]),
      aes(y = .data$score_label)
    ) +
    theme(axis.line = element_blank())

  p
}

plot_score_sector <- function(data, sector) {
  data <- data %>% filter(
    .data$sector == .env$sector,
    .data$scope == "sector"
  )

  c_position <- alignment_scores_values %>%
    filter(.data$score_symbol == "C") %>%
    pull(.data$score_label)

  p <- plot_basic_scorebar() +
    annotate(
      "segment",
      x = 0.5,
      xend = 1.5,
      y = c_position,
      yend = c_position,
      size = 1
    ) +
    geom_text(
      data = data %>% filter(.data$entity == "this_portfolio"),
      x = 1.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 10,
      color = unname(fill_colours_entities_scores["portfolio"]),
      aes(y = .data$score_label)
    ) +
    geom_text(
      data = data %>% filter(.data$entity != "this_portfolio"),
      x = 1.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 7,
      color = unname(fill_colours_entities_scores["benchmark"]),
      aes(y = .data$score_label)
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

legend_scores <- function() {
  fake_data <- tibble::tibble(
    x = c(1, 2, 3, 3),
    y = c(1, 2, 3, 4)
  )

  data_labels <- tibble::tibble(
    label = c("Portfolio", "Peers", "2\u00B0 aligned\nthreshold"),
    x = c(1, 1, 1),
    y = c(3.5, 2.5, 1)
  )

  l <- ggplot(fake_data, aes(x = .data$x, y = .data$y)) +
    annotate(
      "text",
      x = 0,
      y = 3.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 7,
      color = unname(fill_colours_entities_scores["portfolio"])
    ) +
    annotate(
      "text",
      x = 0,
      y = 2.5,
      label = "\u25B2",
      angle = 270 / pi,
      size = 5,
      color = unname(fill_colours_entities_scores["benchmark"])
    ) +
    annotate(
      "segment",
      x = -0.5,
      xend = 0.5,
      y = 1,
      yend = 1,
      size = 1
    ) +
    geom_text(
      data = data_labels,
      aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ),
      hjust = 0
    ) +
    scale_x_continuous(limits = c(-0.5, 3)) +
    scale_y_continuous(limits = c(0, 4)) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(
      panel.spacing = unit(0, "cm")
    )
  l
}

check_data_scores <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "scope", "entity", "sector", "score")
  )
  abort_if_multiple(data, "asset_class", env)
  abort_if_invalid_values(data, "scope", c("portfolio", "sector"))
  abort_if_missing_crucial_values(data, "entity", c("this_portfolio", "peers"), env)
  abort_if_invalid_values(
    data,
    "sector",
    c(NA, "power", "automotive", "coal", "oil", "gas", "cement", "steel")
  )
  abort_if_invalid_values(data, "score", c("A+", "A", "B", "C", "D", "E"))
}

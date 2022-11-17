#' Create a plot showing aggregated scores for portfolio and peers
#'
#' @param data A data frame. In principle, an output of `prep_scores()`.
#'   Requirements:
#'   * Must have columns: `asset_class`,`scope`,`entity`, `sector`, `score`.
#'   * `asset_class` must have a single value.
#'   * `scope` must be one of: "portfolio", "sector".
#'   * `entity` must have following values: "this_portfolio", "peers".
#'   * `sector` must be one of: "power", "automotive", "coal", "oil", "gas",
#'   "steel", "aviation" or `NA` in case of `scope` == "portfolio".
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
  tryCatch(
    {
      stopifnot(is.data.frame(data))
      if (nrow(data) > 0) {
        env <- list(data = substitute(data))
        check_data_scores(data, env = env)

        data <- data %>%
          dplyr::inner_join(alignment_scores_values, by = c("score" = "score_symbol")) %>%
          select(-c("category", "score_delta", "score_upper")) %>%
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

        p_aviation <- plot_score_sector(data, "aviation")

        p_oil <- plot_score_sector(data, "oil")

        p_sector_up <- p_power + p_auto + p_coal + p_gas + plot_layout(ncol = 4)
        p_sector_down <- p_steel + p_aviation + p_oil + legend_scores() + plot_layout(ncol = 4)

        p <- p_portfolio + (p_sector_up / p_sector_down) + plot_layout(widths = c(1, 6))
      } else {
        p <- empty_plot_no_data_message()
      }
    },
    error = function (e) {
      cat("There was an error in plot_scores().\nReturning empty plot object.\n")
      p <- empty_plot_error_message()
    }
  )
  p
}

plot_score_portfolio <- function(data) {
  c_position <- alignment_scores_values %>%
    filter(.data$score_symbol == "C") %>%
    pull("score_label")

  portfolio_label_y <- data %>% filter(.data$entity == "this_portfolio") %>% pull("score_label")
  peer_label_y <- data %>% filter(.data$entity == "peers") %>% pull("score_label")

  p <- plot_basic_scorebar() +
    annotate("segment", x = 0.5, xend = 1.5, y = c_position, yend = c_position, linewidth = 1.3) +
    annotate(
      "segment",
      x = 1.9,
      xend = 1.5,
      y = portfolio_label_y,
      yend = portfolio_label_y,
      arrow = arrow(type = "closed", length = unit(0.4, "npc")),
      color = unname(fill_colours_entities_scores["portfolio"])
    ) +
    annotate(
      "segment",
      x = 1.7,
      xend = 1.5,
      y = peer_label_y,
      yend = peer_label_y,
      arrow = arrow(type = "closed", length = unit(0.2, "npc")),
      color = unname(fill_colours_entities_scores["benchmark"])
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
    pull("score_label")

  portfolio_label_y <- data %>% filter(.data$entity == "this_portfolio") %>% pull("score_label")
  peer_label_y <- data %>% filter(.data$entity == "peers") %>% pull("score_label")

  p <- plot_basic_scorebar() +
    annotate(
      "segment",
      x = 0.5,
      xend = 1.5,
      y = c_position,
      yend = c_position,
      size = 1
    ) +
    annotate(
      "segment",
      x = 1.8,
      xend = 1.5,
      y = portfolio_label_y,
      yend = portfolio_label_y,
      arrow = arrow(type = "closed", length = unit(0.3, "npc")),
      color = unname(fill_colours_entities_scores["portfolio"])
    ) +
    annotate(
      "segment",
      x = 1.65,
      xend = 1.5,
      y = peer_label_y,
      yend = peer_label_y,
      arrow = arrow(type = "closed", length = unit(0.15, "npc")),
      color = unname(fill_colours_entities_scores["benchmark"])
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
      "segment",
      x = 0.1,
      xend = -0.5,
      y = 3.5,
      yend = 3.5,
      arrow = arrow(type = "closed", length = unit(0.2, "npc")),
      color = unname(fill_colours_entities_scores["portfolio"])
    ) +
    annotate(
      "segment",
      x = 0.1,
      xend = -0.2,
      y = 2.5,
      yend = 2.5,
      arrow = arrow(type = "closed", length = unit(0.15, "npc")),
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
    c(NA, "power", "automotive", "coal", "oil", "gas", "aviation", "steel")
  )
  abort_if_invalid_values(data, "score", c("A+", "A", "B", "C", "D", "E"))
}

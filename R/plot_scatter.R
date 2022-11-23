#' Create a scatter plot of exposure to low-carbon technology vs. alignment score
#'
#' @param data A data frame. In principle, an output of
#'   `prep_scatter()`. Requirements:
#'   * `asset_class` must have a single value.
#'   * Must have columns: `asset_class`,`tech_mix_green`,`score`, `entity_name`,
#'    `entity_type`.
#'   * `entity_type` column must be factor and only have following values:
#'   "average", "this_portfolio", "peer", "benchmark".
#'   * `tech_mix_green` must be a percentage in decimal format, with values
#'   between 0 and 1.
#'   * `score` must be a number between 0 and 100.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' plot_scatter(toy_data_scatter %>% filter(asset_class == "equity"))
plot_scatter <- function(data) {
  stopifnot(is.data.frame(data))

  if (nrow(data) > 0) {
    env <- list(data = substitute(data))
    check_data_scatter(data, env = env)

    entity_type_levels <- c("this_portfolio", "peers", "peers_mean", "benchmark")
    colour_assignment_entities <- c(
      r2dii.colours::colour_aliases_2dii[["green"]],
      r2dii.colours::colour_aliases_2dii[["orange"]],
      r2dii.colours::colour_aliases_2dii[["dark_blue"]],
      r2dii.colours::colour_aliases_2dii[["ruby_red"]]
      )
    names(colour_assignment_entities) <- entity_type_levels

    data <- data %>%
      dplyr::inner_join(alignment_scores_values, by = c("score" = "score_symbol")) %>%
      select(-.data$category, -.data$score_upper) %>%
      mutate(
        score_symbol = .data$score,
        score = .data$score_label,
        entity_type = factor(.data$entity_type, levels = entity_type_levels)
      )

    score_bar <- plot_basic_scorebar() +
      geom_point(
        data = data %>%
          mutate(category = "score"),
        aes(
          color = .data$entity_type,
          y = .data$score,
          fill = "black",
          shape = .data$entity_type,
          size = 1.5
        ),
        position = position_dodge(width = 0.2)
      ) +
      scale_colour_manual(
        values = colour_assignment_entities,
        labels = relabel_entities(levels(data$entity_type))
        ) +
      scale_shape_manual(
        values = c("this_portfolio" = 16, "peers" = 1, "peers_mean" = 16, "benchmark" = 16),
        labels = relabel_entities(levels(data$entity_type))
      ) +
      theme(
        axis.title.y = element_text()
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
        size = 1.5
      )
    ) +
      geom_point() +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "#C0C0C0") +
      geom_hline(yintercept = alignment_scores_values$score_upper[2], linetype = "dashed", color = "#C0C0C0") +
      scale_y_continuous(
        limits = c(1, 100),
        breaks = alignment_scores_values$score_label,
        labels = alignment_scores_values$score_symbol,
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_x_continuous(
        labels = scales::percent,
        limits = c(0, 1),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_shape_manual(
        values = c("this_portfolio" = 16, "peers" = 1, "peers_mean" = 16, "benchmark" = 16),
        labels = relabel_entities(levels(data$entity_type))
      ) +
      scale_colour_manual(
        values = colour_assignment_entities,
        labels = relabel_entities(levels(data$entity_type))
        ) +
      theme_2dii(
        base_size = 14
      ) +
      theme(
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
      ) +
      guides(shape = "none", size = "none")

    tech_mix <- tibble(
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
        position = position_dodge(width = 0.2)
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = scales::percent,
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_fill_gradient(low = fill_colours_techmix["brown"], high = fill_colours_techmix["green"]) +
      scale_colour_manual(
        values = colour_assignment_entities,
        labels = relabel_entities(levels(data$entity_type))
        ) +
      scale_shape_manual(
        values = c("this_portfolio" = 16, "peers" = 1, "peers_mean" = 16, "benchmark" = 16),
        labels = relabel_entities(levels(data$entity_type))
      ) +
      theme_2dii(
        base_size = 14
      ) +
      theme(
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none"
      ) +
      labs(y = axis_labels_scatter["x"])

    p_out <- score_bar + p + guide_area() + tech_mix_bar_fut +
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
         override.aes = list(size = 4, shape = c(16, 1, 16, 16))
        )
      )
  } else {
    p_out <- empty_plot_no_data_message()
  }
  p_out
}

check_data_scatter <- function(data, env) {
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "tech_mix_green", "score", "entity_name", "entity_type")
  )
  abort_if_multiple(data, "asset_class", env)
  abort_if_invalid_values(data, "entity_type", c("peers_mean", "this_portfolio", "peers", "benchmark"))
  abort_if_missing_crucial_values(data, "entity_type", c("this_portfolio", "peers", "peers_mean", "benchmark"))
  abort_if_invalid_values(data, "score", c("A+", "A", "B", "C", "D", "E"))
  stopifnot(is.numeric(data$tech_mix_green))
  stopifnot((data$tech_mix_green <= 1) & (data$tech_mix_green >= 0))
}

plot_basic_scorebar <- function() {
  score_bar <- ggplot(
    alignment_scores_values,
    aes(
      x = .data$category,
      y = .data$score_delta,
      fill = factor(
        .data$score_symbol,
        levels = rev(.data$score_symbol)
      )
    )
  ) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      breaks = alignment_scores_values$score_label,
      labels = alignment_scores_values$score_symbol,
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_fill_manual(values = fill_colours_scores) +
    theme_2dii(
      base_size = 14
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    )
  score_bar
}

relabel_entities <- function(label) {
  out <- case_when(
    label == "peers_mean" ~ "Peers Aggr.",
    label == "this_portfolio" ~ "This Portfolio",
    TRUE ~ r2dii.plot::to_title(label)
  )
  out
}

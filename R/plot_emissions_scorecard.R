#' Create a plot showing scorecard emissions
#'
#' @param data A data frame. In principle, an output of
#'   `prep_emissions_scorecard()`. Requirements:
#'   * Must have columns: `asset_class`, `entity`, `emissions`.
#'   * `asset_class` must be one of: "equity", "bonds".
#'   * `entity` must be one of: "portfolio", "benchmark".
#'   * `emissions` must be numeric.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' plot_emissions_scorecard(toy_data_emissions_scorecard)
plot_emissions_scorecard <- function(data) {
  env <- list(data = substitute(data))
  check_data_emissions_scorecard(data, env = env)

  data <- data %>%
    mutate(
      entity = r2dii.plot::to_title(.data$entity),
      entity = factor(
        .data$entity,
        levels = r2dii.plot::to_title(c("portfolio", "benchmark"))
      ),
      asset_class = factor(.data$asset_class, levels = c("equity", "bonds"))
    )

  p <- ggplot(data, aes(x = .data$entity, y = .data$emissions, fill = .data$entity)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = .data$emissions + 0.1 * max(data$emissions, na.rm = TRUE),
        label = paste(
          format(round(.data$emissions), big.mark = ","),
          "t CO2/M CHF"
        )
      ),
      size = 4
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),
      name = expression(atop("Carbon footprint", "(tonnes " * CO[2] * "/M CHF invested)"))
    ) +
    scale_fill_manual(
      values = c(
        unname(fill_colours_entities_scores["portfolio"]),
        unname(fill_colours_entities_scores["benchmark"])
      )
    ) +
    theme_2dii(base_size = 14) +
    theme(
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    ) +
    facet_wrap(~asset_class, labeller = as_labeller(r2dii.plot::to_title))
  p
}

check_data_emissions_scorecard <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(data, c("asset_class", "entity", "emissions"))
  abort_if_invalid_values(data, "asset_class", c("bonds", "equity"))
  abort_if_invalid_values(data, "entity", c("portfolio", "benchmark"))
  stopifnot(is.numeric(data$emissions))
}

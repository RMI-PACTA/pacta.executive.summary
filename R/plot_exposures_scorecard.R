#' Create a plot showing exposure to sectors relevant to scorecard
#'
#' @param data A data frame. In principle, an output of
#'   `prep_exposures_scorecard()`. Requirements:
#'   * Must have columns: `asset_class`, `sector_or_tech`, `exposure_perc_aum`.
#'   * `sector_or_tech` must be one of: "coal", "other_fossil_fuels",
#'   "fossil_power", "renewables_power".
#'   * `exposure_perc_aum` must be a percentage in decimal format, with values
#'   between 0 and 1.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' plot_exposures_scorecard(toy_data_exposures_scorecard)
plot_exposures_scorecard <- function(data) {
  env <- list(data = substitute(data))
  check_data_exposures_scorecard(data, env = env)

  p <- ggplot(
    data, 
    aes(
      x = .data$sector_or_tech, 
      y = .data$exposure_perc_aum, 
      fill = .data$sector_or_tech)
    ) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = .data$exposure_perc_aum,
        label = scales::percent(round(.data$exposure_perc_aum, digits = 2))
      ),
      hjust = -0.2,
      size = 7
    ) +
    scale_x_discrete(labels = r2dii.plot::to_title) +
    scale_y_continuous(expand = expansion(mult = c(0, .8)), labels = scales::percent) +
    scale_fill_manual(values = fill_colours_exposures_scorecard) +
    coord_flip() +
    theme_2dii(base_size = 24) +
    theme(
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    ) +
    facet_wrap(~ asset_class, labeller = as_labeller(r2dii.plot::to_title))
  p
}

check_data_exposures_scorecard <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "sector_or_tech", "exposure_perc_aum")
  )
  abort_if_invalid_values(
    data,
    "sector_or_tech",
    c("coal", "other_fossil_fuels", "fossil_power", "renewables_power")
  )
  stopifnot(is.numeric(data$exposure_perc_aum))
  stopifnot((data$exposure_perc_aum <= 1) & (data$exposure_perc_aum >= 0))
}

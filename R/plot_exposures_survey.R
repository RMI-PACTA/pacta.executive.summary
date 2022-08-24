#' Create a bar plot showing exposure to a sector
#'
#' @param data A data frame. In principle, an output of
#'   `prep_exposures_survey()`. Requirements:
#'   * Must have columns: `asset_class`, `entity`, `sector`, `exposure_perc_aum`.
#'   * `asset_class` must have a single value.
#'   * `entity` must be one of: "portfolio", "peers".
#'   * `sector` must be one of PACTA sectors. Run
#'   `names(r2dii.colours::colour_aliases_pacta)` for the list of values.
#'   * `exposure_perc_aum` must be a percentage in decimal format, with values
#'   between 0 and 1.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' data <- toy_data_exposures_survey %>%
#'   dplyr::filter(asset_class == "equity", sector == "coal")
#'
#' plot_exposures_survey(data)
plot_exposures_survey <- function(data) {
  env <- list(data = substitute(data))
  check_data_exposures_survey(data, env = env)

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

check_data_exposures_survey <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "entity", "sector", "exposure_perc_aum")
  )
  abort_if_multiple(data, "asset_class", env)
  abort_if_invalid_values(data, "entity", c("portfolio", "peers"))
  abort_if_invalid_values(
    data,
    "sector",
    names(r2dii.colours::colour_aliases_pacta)
  )
  stopifnot(is.numeric(data$exposure_perc_aum))
  stopifnot((data$exposure_perc_aum <= 1) & (data$exposure_perc_aum >= 0))
}

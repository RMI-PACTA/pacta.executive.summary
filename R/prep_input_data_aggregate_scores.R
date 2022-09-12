prep_input_data_aggregate_scores <- function(data,
                                             scenario_source,
                                             scenarios,
                                             scenario_geography,
                                             equity_market,
                                             start_year,
                                             time_horizon,
                                             green_techs) {
  # filter data
  # TODO: probably more filtering needed?
  # TODO: add checks that data is given for the filter values
  data <- data %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      .data$scenario %in% .env$scenarios,
      .data$scenario_geography == .env$scenario_geography,
      .data$equity_market == .env$equity_market,
      dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)
    )

  # Oil & Gas are treated as separate sectors in the aggregate score.

  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$technology == "Oil" ~ "Oil",
        .data$technology == "Gas" ~ "Gas",
        TRUE ~ .data$ald_sector
      )
    )

  return(data)
}

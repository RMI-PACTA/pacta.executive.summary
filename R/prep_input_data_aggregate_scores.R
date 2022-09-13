prep_input_data_aggregate_scores <- function(data,
                                             scenario_source,
                                             scenarios,
                                             scenario_geography,
                                             portfolio_allocation_method_bonds,
                                             portfolio_allocation_method_equity,
                                             equity_market,
                                             start_year,
                                             time_horizon,
                                             green_techs) {
  # filter data
  data <- data %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      .data$scenario %in% .env$scenarios,
      .data$scenario_geography == .env$scenario_geography,
      .data$equity_market == .env$equity_market,
      dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon),
      .data$asset_class == "bonds" & .data$allocation == .env$portfolio_allocation_method_bonds |
        .data$asset_class == "equity" & .data$allocation == .env$portfolio_allocation_method_equity
    )

  # map sectors to p4b style
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c(.data$sector_p4b, .data$technology_p4b))

  # Oil & Gas are treated as separate sectors in the aggregate score.
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$technology == "oil" ~ "oil",
        .data$technology == "gas" ~ "gas",
        TRUE ~ .data$ald_sector
      )
    )

  return(data)
}

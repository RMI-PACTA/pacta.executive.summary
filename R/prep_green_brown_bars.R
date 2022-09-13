prep_green_brown_bars <- function(equity_results_portfolio,
                                  bonds_results_portfolio,
                                  start_year,
                                  time_horizon,
                                  scenario_source,
                                  scenario_selected,
                                  scenario_geography,
                                  equity_market,
                                  portfolio_allocation_method_bonds,
                                  portfolio_allocation_method_equity) {
  # add asset class and entity information
  data_equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      entity_name = "portfolio",
      entity_type = "this_portfolio",
      asset_class = "equity"
    )

  data_bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      entity_name = "portfolio",
      entity_type = "this_portfolio",
      asset_class = "bonds"
    )

  # combine input data
  data_inputs <- data_equity_results_portfolio %>%
    dplyr::bind_rows(data_bonds_results_portfolio)

  # filter data
  data <- data_inputs %>%
    dplyr::filter(
      year %in% c(.env$start_year, .env$start_year + .env$time_horizon),
      .data$scenario_source == .env$scenario_source,
      .data$scenario == .env$scenario_selected,
      .data$scenario_geography == .env$scenario_geography,
      .data$equity_market == .env$equity_market,
      dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon),
      .data$asset_class == "bonds" & .data$allocation == .env$portfolio_allocation_method_bonds |
        .data$asset_class == "equity" & .data$allocation == .env$portfolio_allocation_method_equity
    )

  # map technologies and sectors
  # TODO: use input args to define groupings?
  # TODO: snake_case?
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "fossil_fuels",
        .data$ald_sector == "Oil&Gas" ~ "fossil_fuels",
        TRUE ~ .data$ald_sector
      )
    ) %>%
    dplyr::mutate(
      tech_type = dplyr::case_when(
        .data$ald_sector == "fossil_fuels" ~ "brown",
        .data$ald_sector == "Automotive" &
          .data$technology %in% c(
            "Electric", "Hybrid", "FuelCell",
            "Electric_HDV", "Hybrid_HDV", "FuelCell_HDV"
          ) ~ "green",
        .data$ald_sector == "Automotive" &
          .data$technology %in% c("ICE", "ICE_HDV") ~ "brown",
        .data$ald_sector == "Power" & .data$technology %in% c("CoalCap", "GasCap", "OilCap") ~ "brown",
        .data$ald_sector == "Power" & .data$technology %in% c("HydroCap", "NuclearCap") ~ "hydro_and_nuclear",
        .data$ald_sector == "Power" & .data$technology == "RenewablesCap" ~ "green",
        TRUE ~ "other"
      )
    ) %>%
    dplyr::mutate(
      technology = dplyr::case_when(
        .data$ald_sector == "Automotive" & .data$tech_type == "green" ~ "green",
        .data$ald_sector == "Automotive" & .data$tech_type == "brown" ~ "brown",
        .data$ald_sector %in% c("Aviation", "Cement", "Steel") ~ "other",
        TRUE ~ .data$technology
      )
    )

  # calculate technology exposures
  # calculate sector exposures
  data_out <- data %>%
    dplyr::select(
      .data$entity_name, .data$entity_type, .data$asset_class, .data$year,
      .data$tech_type, .data$technology, .data$ald_sector, .data$plan_carsten
    ) %>%
    dplyr::group_by(
      .data$entity_name, .data$entity_type, .data$asset_class, .data$year,
      .data$technology, .data$ald_sector
    ) %>%
    dplyr::mutate(
      technology_exposure = sum(.data$plan_carsten, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$entity_name, .data$entity_type, .data$asset_class, .data$year,
      .data$ald_sector
    ) %>%
    dplyr::mutate(
      sector_exposure = sum(.data$plan_carsten, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  return(data_out)
}

prep_data_executive_summary <- function(investor_name,
                                        portfolio_name,
                                        peer_group,
                                        start_year,
                                        scenario_source,
                                        scenario_selected,
                                        scenario_geography,
                                        equity_market,
                                        portfolio_allocation_method_equity,
                                        portfolio_allocation_method_bonds,
                                        pacta_sectors,
                                        tech_roadmap_sectors,
                                        green_techs,
                                        brown_techs,
                                        equity_results_portfolio,
                                        bonds_results_portfolio,
                                        peers_equity_results_aggregated,
                                        peers_bonds_results_aggregated,
                                        peers_equity_results_individual,
                                        peers_bonds_results_individual,
                                        indices_equity_results_portfolio,
                                        indices_bonds_results_portfolio,
                                        audit_file = audit_file,
                                        emissions = emissions) {

  equity_results_portfolio <- equity_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_equity
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$investor_name,
      .data$portfolio_name == .env$portfolio_name
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$investor_name,
      .data$portfolio_name == .env$portfolio_name
    )

  peers_equity_results_aggregated <- peers_equity_results_aggregated %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_equity
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$peer_group,
      .data$portfolio_name == .env$peer_group
    )

  peers_bonds_results_aggregated <- peers_bonds_results_aggregated %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$peer_group,
      .data$portfolio_name == .env$peer_group
    )

  peers_equity_results_individual <- peers_equity_results_individual %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_equity
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$peer_group
    )

  peers_bonds_results_individual <- peers_bonds_results_individual %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    ) %>%
    dplyr::filter(
      .data$investor_name == .env$peer_group
    )

  indices_equity_results_portfolio <- indices_equity_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_equity
    )

  indices_bonds_results_portfolio <- indices_bonds_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    )

  # add asset class, entity type and grenn/brown, combine data sets
  # ... portfolios
  equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "this_portfolio"
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "this_portfolio"
    )

  results_portfolio <- equity_results_portfolio %>%
    dplyr::bind_rows(bonds_results_portfolio) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      )
    )

  # ... aggregated peer group results
  peers_equity_results_aggregated <- peers_equity_results_aggregated %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "peers"
    )

  peers_bonds_results_aggregated <- peers_bonds_results_aggregated %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "peers"
    )

  peers_results_aggregated <- peers_equity_results_aggregated %>%
    dplyr::bind_rows(peers_bonds_results_aggregated) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      )
    )

  # ... individual peer group results
  peers_equity_results_individual <- peers_equity_results_individual %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "peers"
    )

  peers_bonds_results_individual <- peers_bonds_results_individual %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "peers"
    )

  peers_results_individual <- peers_equity_results_individual %>%
    dplyr::bind_rows(peers_bonds_results_individual) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      )
    )

  # ... indices
  indices_equity_results_portfolio <- indices_equity_results_portfolio %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "index"
    )

  indices_bonds_results_portfolio <- indices_bonds_results_portfolio %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "index"
    )

  indices_results_portfolio <- indices_equity_results_portfolio %>%
    dplyr::bind_rows(indices_bonds_results_portfolio) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      )
    )

  # TODO: translate data

  data_out <- list(
    results_portfolio = results_portfolio,
    peers_results_aggregated = peers_results_aggregated,
    peers_results_individual = peers_results_individual,
    indices_results_portfolio = indices_results_portfolio
  )

  return(data_out)
}

apply_general_filters <- function(data,
                                  scenario_source,
                                  scenario_geography,
                                  equity_market,
                                  start_year,
                                  allocation_type) {
  data <- data %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      .data$scenario_geography == .env$scenario_geography,
      .data$equity_market == .env$equity_market,
      dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon_lookup),
      .data$allocation == .env$allocation_type
    )

  return(data)
}

prep_scores <- function(equity_results_portfolio,
                        bonds_results_portfolio,
                        peers_equity_results_aggregated,
                        peers_bonds_results_aggregated,
                        portfolio_allocation_method_equity,
                        portfolio_allocation_method_bonds,
                        scenario_source,
                        scenarios,
                        scenario_geography,
                        equity_market,
                        start_year,
                        time_horizon,
                        green_techs,
                        remaining_carbon_budgets) {
  # add asset class and entity type
  data_equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "this_portfolio"
    )

  data_bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "this_portfolio"
    )

  data_equity_results_peers <- peers_equity_results_aggregated %>%
    dplyr::mutate(
      asset_class = "equity",
      entity = "peers"
    )

  data_bonds_results_peers <- peers_bonds_results_aggregated %>%
    dplyr::mutate(
      asset_class = "bonds",
      entity = "peers"
    )

  # combine all data frames
  data_inputs <- data_equity_results_portfolio %>%
    dplyr::bind_rows(data_bonds_results_portfolio) %>%
    dplyr::bind_rows(data_equity_results_peers) %>%
    dplyr::bind_rows(data_bonds_results_peers)

  # prepare data for sector aggregation
  data_aggregate_scores <- data_inputs %>%
    prep_input_data_aggregate_scores(
      scenario_source = scenario_source,
      scenarios = scenarios,
      scenario_geography = scenario_geography,
      portfolio_allocation_method_bonds = portfolio_allocation_method_bonds,
      portfolio_allocation_method_equity = portfolio_allocation_method_equity,
      equity_market = equity_market,
      start_year = start_year,
      time_horizon = time_horizon,
      green_techs = green_techs
    )

  # calculate technology level alignment
  data_aggregate_scores <- data_aggregate_scores %>%
    prep_calculate_technology_alignment(
      green_techs = green_techs,
      start_year = start_year,
      time_horizon = time_horizon
    )

  # sectors aggregate score
  sector_aggregate_scores <- data_aggregate_scores %>%
    prep_calculate_sector_aggregate_scores()

  # portfolio aggregate score
  portfolio_aggregate_scores <- sector_aggregate_scores %>%
    prep_calculate_portfolio_aggregate_scores(remaining_carbon_budgets = remaining_carbon_budgets)

  output_scores <- sector_aggregate_scores %>%
    dplyr::bind_rows(portfolio_aggregate_scores) %>%
    dplyr::select(-.data$sector_exposure)

  # get scenario thresholds and apply scenario based grades
  scenario_thresholds <- get("scenario_thresholds")

  data_out <- output_scores %>%
    calculate_aggregate_scores_with_scenarios(
      scenario_thresholds = scenario_thresholds
    )

  return(data_out)
}

prep_scores <- function(results_portfolio,
                        peers_results_aggregated,
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

  data <- results_portfolio %>%
    dplyr::bind_rows(peers_results_aggregated)

  # prepare data for sector aggregation
  data_aggregate_scores <- data %>%
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

  # calculate sectors aggregate score
  sector_aggregate_scores <- data_aggregate_scores %>%
    prep_calculate_sector_aggregate_scores()

  # calculate portfolio aggregate score
  portfolio_aggregate_scores <- sector_aggregate_scores %>%
    prep_calculate_portfolio_aggregate_scores(remaining_carbon_budgets = remaining_carbon_budgets)

  # combine scores in single data frame
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

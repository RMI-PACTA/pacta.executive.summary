prep_scores <- function(results_portfolio,
                        peers_results_aggregated,
                        asset_class,
                        scenario_source = "GECO2021") {

  start_year <- min(results_portfolio$year, na.rm = TRUE)

  data <- results_portfolio %>%
    dplyr::bind_rows(peers_results_aggregated) %>%
    dplyr::filter(.data$asset_class == .env$asset_class)

  # get scenarios
  scenario_thresholds <- get("scenario_thresholds")

  scenarios <- scenario_thresholds %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source) %>%
    dplyr::pull(.data$scenario)

  # prepare data for sector aggregation
  data_aggregate_scores <- data %>%
    prep_input_data_aggregate_scores(scenarios = scenarios)

  # calculate technology level alignment
  data_aggregate_scores <- data_aggregate_scores %>%
    prep_calculate_technology_alignment(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # calculate sectors aggregate score
  sector_aggregate_scores <- data_aggregate_scores %>%
    prep_calculate_sector_aggregate_scores()

  # get remaining carbon budgets and calculate portfolio aggregate score
  remaining_carbon_budgets <- get("remaining_carbon_budgets")

  portfolio_aggregate_scores <- sector_aggregate_scores %>%
    prep_calculate_portfolio_aggregate_scores(
      remaining_carbon_budgets = remaining_carbon_budgets
    )

  # combine scores in single data frame
  output_scores <- sector_aggregate_scores %>%
    dplyr::bind_rows(portfolio_aggregate_scores) %>%
    dplyr::select(-.data$sector_exposure)

  # apply scenario based grades
  data_out <- output_scores %>%
    calculate_aggregate_scores_with_scenarios(
      scenario_thresholds = scenario_thresholds
    )

  return(data_out)
}

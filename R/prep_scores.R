#' Prepare data input for plotting aggregate climate scores
#'
#' @description Prepare data input for plotting aggregate climate scores based
#' on PACTA for investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param peers_results_aggregated Data frame that contains pre-wrangled
#'   aggregate peer group level PACTA results from a PACTA for investors
#'   analysis.
#' @param asset_class Character vector of length 1. Must be either `equity` or
#'   `bonds`.
#' @param scenario_source Character vector of length 1. Must be a
#'   `scenario_source` featured in the `scenario_thresholds` data set.
#'
#' @return data.frame
prep_scores <- function(results_portfolio,
                        peers_results_aggregated,
                        asset_class = c("equity", "bonds"),
                        scenario_source = "GECO2021") {
  # validate inputs
  asset_class <- match.arg(asset_class)
  if (!scenario_source %in% unique(get("scenario_thresholds")$scenario_source)) {
    stop("Argument scenario_source does not hold an accepted value.")
  }
  if (length(asset_class) != 1) {
    stop("Argument asset_class must be of length 1. Please check your input.")
  }
  if (length(scenario_source) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }

  # infer start_year
  start_year <- min(results_portfolio$year, na.rm = TRUE)

  # filter selected asset_class
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

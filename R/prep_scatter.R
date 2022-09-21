prep_scatter <- function(results_portfolio,
                         peers_results_aggregated,
                         peers_results_individual,
                         indices_results_portfolio,
                         scenario_source = "GECO2021",
                         scenario_selected = "1.5C-Unif",
                         asset_class = c("equity", "bonds")) {
  # validate inputs
  asset_class <- match.arg(asset_class)

  check_data_prep_scatter(
    asset_class = asset_class,
    scenario_source = scenario_source,
    scenario_selected = scenario_selected
  )

  # infer start year
  start_year <- min(results_portfolio$year, na.rm = TRUE)

  # get scenarios
  scenario_thresholds <- get("scenario_thresholds")

  scenarios <- scenario_thresholds %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source) %>%
    dplyr::pull(.data$scenario)

  # combine input data
  data <- results_portfolio %>%
    dplyr::bind_rows(peers_results_aggregated) %>%
    dplyr::bind_rows(peers_results_individual) %>%
    dplyr::bind_rows(indices_results_portfolio)

  # filter input data
  data <- data %>%
    dplyr::filter(
      .data$asset_class == .env$asset_class,
      .data$ald_sector == scatter_sector_lookup
    )

  # calculate current exposures
  data_exposure <- data %>%
    dplyr::filter(
      year == .env$start_year,
      .data$scenario == .env$scenario_selected
    )

  # map sectors to p4b style
  # map tech_type and fossil_fuels
  data_exposure <- data_exposure %>%
    # TODO: use input args to define groupings?
    map_sectors_and_tech_type()

  # calculate tech_type and sector exposures
  data_exposure <- data_exposure %>%
    calculate_exposures() %>%
    dplyr::filter(tech_type == "green")

  # calculate future alignment scores

  # prepare data for sector aggregation
  data_aggregate_scores <- data %>%
    wrangle_input_data_aggregate_scores(scenarios = scenarios)

  # calculate technology level alignment
  data_aggregate_scores <- data_aggregate_scores %>%
    calculate_technology_alignment(
      start_year = start_year,
      time_horizon = time_horizon_lookup
    )

  # calculate sectors aggregate score
  sector_aggregate_scores <- data_aggregate_scores %>%
    calculate_sector_aggregate_scores()

  sector_aggregate_scores <- sector_aggregate_scores %>%
    calculate_aggregate_scores_with_scenarios(
      scenario_thresholds = scenario_thresholds
    )

  # combine outputs and wrangle
  data_out <- data_exposure %>%
    dplyr::inner_join(
      sector_aggregate_scores,
      by = c(
        "investor_name", "portfolio_name", "asset_class", "sector",
        "entity_name", "entity_type", "entity"
      )
    ) %>%
    dplyr::select(
      .data$asset_class, .data$year, .data$perc_tech_exposure, .data$score,
      .data$entity_name, .data$entity_type
    ) %>%
    dplyr::rename(
      tech_mix_green = .data$perc_tech_exposure,

    )

  return(data_out)
}

check_data_prep_scatter <- function(asset_class,
                                    scenario_source,
                                    scenario_selected) {
  if (length(asset_class) != 1) {
    stop("Argument asset_class must be of length 1. Please check your input.")
  }
  if (!scenario_source %in% unique(get("scenario_thresholds")$scenario_source)) {
    stop("Argument scenario_source does not hold an accepted value.")
  }
  if (length(scenario_source) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!scenario_selected %in% unique(get("scenario_thresholds")$scenario)) {
    stop("Argument scenario_selected does not hold an accepted value.")
  }
  if (length(scenario_selected) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}


#' Prepare data input for plotting aggregate climate scores
#'
#' Prepare data input for plotting aggregate climate scores based
#' on PACTA for investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param peers_results_aggregated Data frame that contains pre-wrangled
#'   aggregate peer group level PACTA results from a PACTA for investors
#'   analysis.
#' @param asset_class Character. Must be either `equity` or
#'   `bonds`.
#' @param scenario_source Character. Must be a
#'   `scenario_source` featured in the `scenario_thresholds` data set.
#'
#' @return data.frame
#' @export
prep_scores <- function(results_portfolio,
                        peers_results_aggregated,
                        asset_class = c("equity", "bonds"),
                        scenario_source = "GECO2021") {
  # validate inputs
  asset_class <- match.arg(asset_class)

  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("scores") %>% filter(asset_class == .env$asset_class)
  } else {
    check_data_prep_scores(
      asset_class = asset_class,
      scenario_source = scenario_source
    )

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
      dplyr::pull("scenario")

    # prepare data for sector aggregation
    data_aggregate_scores <- data %>%
      wrangle_input_data_aggregate_scores(scenarios = scenarios)

    # roadmap sectors
    data_aggregate_scores_tech <- data_aggregate_scores %>%
      dplyr::filter(.data$ald_sector %in% c("automotive", "coal", "oil", "gas", "power"))

    # calculate technology level alignment
    data_aggregate_scores_tech <- data_aggregate_scores_tech %>%
      calculate_technology_alignment(
        start_year = start_year,
        time_horizon = time_horizon_lookup
      )

    # calculate roadmap sectors aggregate score
    sector_aggregate_scores_tech <- data_aggregate_scores_tech %>%
      calculate_sector_aggregate_scores()

    # emission intensity sectors
    data_aggregate_scores_emissions <- data_aggregate_scores %>%
      dplyr::filter(.data$ald_sector %in% c("aviation", "cement", "steel", "shipping"))

    # calculate emission intensity sectors aggregate score
    sector_aggregate_scores_emissions <- data_aggregate_scores_emissions %>%
      calculate_emissions_sectors_aggregate_scores(
        start_year = start_year,
        time_horizon = time_horizon_lookup
      )

    # combine scores of roadmap and emission intensity sectors
    sector_aggregate_scores <- sector_aggregate_scores_tech %>%
      dplyr::bind_rows(sector_aggregate_scores_emissions)

    # get remaining carbon budgets and calculate portfolio aggregate score
    remaining_carbon_budgets <- get("remaining_carbon_budgets")

    portfolio_aggregate_scores <- sector_aggregate_scores %>%
      calculate_portfolio_aggregate_scores(
        remaining_carbon_budgets = remaining_carbon_budgets
      )

    # combine scores in single data frame
    output_scores <- sector_aggregate_scores %>%
      dplyr::bind_rows(portfolio_aggregate_scores) %>%
      dplyr::select(-"sector_exposure")

    # apply scenario based grades
    data_out <- output_scores %>%
      calculate_aggregate_scores_with_scenarios(
        scenario_thresholds = scenario_thresholds
      ) %>%
      dplyr::select(
        c("asset_class", "scope", "entity", "sector", "score")
      )
  }
  data_out
}

check_data_prep_scores <- function(scenario_source,
                                   asset_class) {
  if (!scenario_source %in% unique(get("scenario_thresholds")$scenario_source)) {
    stop("Argument scenario_source does not hold an accepted value.")
  }
  if (length(asset_class) != 1) {
    stop("Argument asset_class must be of length 1. Please check your input.")
  }
  if (length(scenario_source) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}

wrangle_input_data_aggregate_scores <- function(data,
                                                scenarios) {
  # filter data
  data <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios)

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
    dplyr::select(-c("sector_p4b", "technology_p4b"))

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

calculate_technology_alignment <- function(data,
                                           start_year,
                                           time_horizon) {
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      scen_alloc_wt_tech_prod_t0 = dplyr::first(.data$scen_alloc_wt_tech_prod),
      scen_alloc_wt_tech_prod_t5 = dplyr::last(.data$scen_alloc_wt_tech_prod),
      plan_alloc_wt_tech_prod_t5 = dplyr::last(.data$plan_alloc_wt_tech_prod),
      exposure_t0 = dplyr::first(.data$plan_carsten)
    ) %>%
    dplyr::select(-c("scen_alloc_wt_tech_prod")) %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::mutate(
      tech_trajectory_alignment = dplyr::if_else(
        .data$green_or_brown == "green",
        (.data$plan_alloc_wt_tech_prod - .data$scen_alloc_wt_tech_prod_t5) /
          .data$scen_alloc_wt_tech_prod_t5,
        (.data$scen_alloc_wt_tech_prod_t5 - .data$plan_alloc_wt_tech_prod) /
          .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}

calculate_sector_aggregate_scores <- function(data) {
  # create helper variables for the weighting factors
  # account for special handling of CoalCap, RenewablesCap, ICE and Electric
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      tech_allocation_weight = dplyr::if_else(
        # TODO: add an input that defines these technologies
        .data$technology %in% techs_edge_case_aggregate_score_lookup &
          .data$plan_alloc_wt_tech_prod_t5 > .data$scen_alloc_wt_tech_prod_t5,
        .data$plan_alloc_wt_tech_prod_t5,
        .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::mutate(
      scenario_change = abs(.data$scen_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0),
      production_change = abs(.data$plan_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0)
    ) %>%
    dplyr::mutate(
      scenario_change = dplyr::if_else(
        .data$technology %in% techs_edge_case_aggregate_score_lookup &
          .data$production_change > .data$scenario_change,
        .data$production_change,
        .data$scenario_change
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("production_change"))

  # calculate sector score & sector exposure
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$tech_trajectory_alignment,
        w = .data$tech_allocation_weight * .data$scenario_change
      ),
      sector_exposure = sum(.data$exposure_t0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scope = "sector")

  return(data)
}

calculate_emissions_sectors_aggregate_scores <- function(data,
                                                         start_year,
                                                         time_horizon) {
  data_out <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(sector_exposure = dplyr::first(.data$plan_sec_carsten)) %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::mutate(
      score = (.data$scen_sec_emissions_factor - .data$plan_sec_emissions_factor) /
        .data$scen_sec_emissions_factor
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$score, .data$sector_exposure
    ) %>%
    dplyr::mutate(scope = "sector")

  return(data_out)
}

calculate_portfolio_aggregate_scores <- function(data,
                                                 remaining_carbon_budgets) {
  data <- data %>%
    dplyr::inner_join(
      remaining_carbon_budgets,
      by = c("scenario_source", "ald_sector")
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$score,
        w = .data$remaining_carbon_budget * .data$sector_exposure
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scope = "portfolio",
      ald_sector = NA_character_
    )

  return(data)
}

calculate_aggregate_scores_with_scenarios <- function(data,
                                                      scenario_thresholds) {
  # add threshold scenarios
  data <- data %>%
    dplyr::select(
      c("investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "ald_sector", "scenario_source", "scenario", "score")
    ) %>%
    dplyr::inner_join(
      scenario_thresholds,
      by = c("scenario_source", "scenario")
    ) %>%
    dplyr::select(-c("scenario", "scenario_source"))

  data <- data %>%
    tidyr::pivot_wider(
      id_cols = c(
        .data$investor_name, .data$portfolio_name, .data$asset_class,
        .data$scope, .data$entity_name, .data$entity_type, .data$entity,
        .data$ald_sector
      ),
      names_from = .data$threshold,
      values_from = .data$score
    ) %>%
    dplyr::rename(sector = "ald_sector")

  # calculate grades based on scores and scenario thresholds
  data <- data %>%
    dplyr::mutate(
      score = dplyr::case_when(
        .data$high >= 0.15 ~ "A+",
        .data$high >= 0 ~ "A",
        .data$high < 0 & .data$mid >= 0.15 ~ "B",
        .data$mid >= 0 ~ "C",
        .data$mid < 0 & .data$low >= 0 ~ "D",
        .data$low < 0 ~ "E",
        TRUE ~ NA_character_,
      )
    )

  data <- data %>%
    dplyr::select(
      c("investor_name", "portfolio_name", "asset_class", "scope", "entity_name",
        "entity_type", "entity", "sector", "score")
    ) %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$scope,
      .data$sector, .data$entity_name, .data$entity_type, .data$entity
    )

  return(data)
}

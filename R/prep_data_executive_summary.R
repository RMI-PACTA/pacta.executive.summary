#' Prepares data inputs needed for rendering the executive summary
#'
#' @param investor_name Some description
#' @param portfolio_name Some description
#' @param peer_group Some description
#' @param start_year Some description
#' @param scenario_source Some description
#' @param scenario_selected Some description
#' @param scenario_geography Some description
#' @param equity_market Some description
#' @param portfolio_allocation_method_equity Some description
#' @param portfolio_allocation_method_bonds Some description
#' @param green_techs Some description
#' @param equity_results_portfolio Some description
#' @param bonds_results_portfolio Some description
#' @param peers_equity_results_aggregated Some description
#' @param peers_bonds_results_aggregated Some description
#' @param peers_equity_results_individual Some description
#' @param peers_bonds_results_individual Some description
#' @param indices_equity_results_portfolio Some description
#' @param indices_bonds_results_portfolio Some description
#' @param audit_file Some description
#' @param emissions_portfolio Some description
#' @param index_dir Directory containing `.rds` files with index benchmarks
#'
#' @return data.frame
#' @export
#'
prep_data_executive_summary <- function(investor_name,
                                        portfolio_name,
                                        peer_group,
                                        start_year,
                                        scenario_source,
                                        scenario_selected, # might not need this
                                        scenario_geography,
                                        equity_market,
                                        portfolio_allocation_method_equity,
                                        portfolio_allocation_method_bonds,
                                        green_techs,
                                        equity_results_portfolio,
                                        bonds_results_portfolio,
                                        peers_equity_results_aggregated,
                                        peers_bonds_results_aggregated,
                                        peers_equity_results_individual,
                                        peers_bonds_results_individual,
                                        indices_equity_results_portfolio,
                                        indices_bonds_results_portfolio,
                                        audit_file,
                                        emissions_portfolio,
                                        index_dir) {

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
    ) %>%
    dplyr::filter(grepl("MSCI - WORLD", .data$portfolio_name))

  indices_bonds_results_portfolio <- indices_bonds_results_portfolio %>%
    apply_general_filters(
      scenario_source = scenario_source,
      scenario_geography = scenario_geography,
      equity_market = equity_market,
      start_year = start_year,
      allocation_type = portfolio_allocation_method_bonds
    ) %>%
    dplyr::filter(grepl("Global Corp Bond", .data$portfolio_name))

  # add asset class, entity type and grenn/brown, combine data sets
  # ... portfolios
  equity_results_portfolio <- equity_results_portfolio %>%
    dplyr::mutate(
      asset_class = "equity"
    )

  bonds_results_portfolio <- bonds_results_portfolio %>%
    dplyr::mutate(
      asset_class = "bonds"
    )

  results_portfolio <- equity_results_portfolio %>%
    dplyr::bind_rows(bonds_results_portfolio) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      ),
      entity_name = "portfolio",
      entity_type = "this_portfolio",
      entity = "this_portfolio"
    )

  # ... aggregated peer group results
  peers_equity_results_aggregated <- peers_equity_results_aggregated %>%
    dplyr::mutate(
      asset_class = "equity"
    )

  peers_bonds_results_aggregated <- peers_bonds_results_aggregated %>%
    dplyr::mutate(
      asset_class = "bonds"
    )

  peers_results_aggregated <- peers_equity_results_aggregated %>%
    dplyr::bind_rows(peers_bonds_results_aggregated) %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      ),
        entity_name = "peers_average",
        entity_type = "peers",
        entity = "peers"
    )

  # ... individual peer group results
  peers_equity_results_individual <- peers_equity_results_individual %>%
    dplyr::mutate(
      asset_class = "equity"
    )

  peers_bonds_results_individual <- peers_bonds_results_individual %>%
    dplyr::mutate(
      asset_class = "bonds"
    )

  peers_results_individual <- peers_equity_results_individual %>%
    dplyr::bind_rows(peers_bonds_results_individual) %>%
    dplyr::group_by(investor_name, portfolio_name) %>%
    mutate(
      green_or_brown = dplyr::if_else(
        .data$technology %in% .env$green_techs, "green", "brown"
      ),
      entity_name = glue::glue("peer_{dplyr::cur_group_id()}"),
      entity_type = "peers",
      entity = "peers"
    ) %>%
    dplyr::ungroup()

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
      ),
      entity_name = dplyr::if_else(
        .data$asset_class == "equity",
        index_eq_short_lookup,
        index_cb_short_lookup
      ),
      entity_type = "benchmark"
    )

  emissions_portfolio <- emissions_portfolio %>%
    dplyr::mutate(entity = "portfolio")

  emissions_indices_eq <- readRDS(file.path(index_dir, "Indices_equity_emissions.rds")) %>%
    dplyr::mutate(entity = "benchmark") %>%
    dplyr::filter(grepl("MSCI - WORLD", .data$portfolio_name))

  emissions_indices_cb <- readRDS(file.path(index_dir, "Indices_bonds_emissions.rds")) %>%
    dplyr::mutate(entity = "benchmark") %>%
    dplyr::filter(grepl("Global Corp Bond", .data$portfolio_name))

  emissions <- emissions_portfolio %>%
    dplyr::bind_rows(emissions_indices_eq) %>%
    dplyr::bind_rows(emissions_indices_cb)


  audit_portfolio <- audit_file %>%
    dplyr::mutate(entity = "portfolio")

  audit_indices_eq <- readRDS(file.path(index_dir, "Indices_equity_audit.rds")) %>%
    dplyr::mutate(entity = "benchmark") %>%
    dplyr::filter(grepl("MSCI - WORLD", .data$portfolio_name))

  audit_indices_cb <- readRDS(file.path(index_dir, "Indices_bonds_audit.rds")) %>%
    dplyr::mutate(entity = "benchmark") %>%
    dplyr::filter(grepl("Global Corp Bond", .data$portfolio_name))

  audit_data <- audit_portfolio %>%
    dplyr::bind_rows(audit_indices_eq) %>%
    dplyr::bind_rows(audit_indices_cb)

  data_out <- list(
    results_portfolio = results_portfolio,
    peers_results_aggregated = peers_results_aggregated,
    peers_results_individual = peers_results_individual,
    indices_results_portfolio = indices_results_portfolio,
    audit_data = audit_data,
    emissions_data = emissions
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

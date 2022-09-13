prep_data_executive_summary <- function(investor_name,
                                        portfolio_name,
                                        peer_group,
                                        start_year,
                                        time_horizon,
                                        scenario_source,
                                        scenarios,
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
  # TODO: apply filters to data (investor name, portfolio name, allocation
  # method, scenario, equity market, scenario geography, peer group, index)
  # TODO: merge bonds and equity data in one table
  # TODO: translate data

  # ----- prepare data for aggregate score
  remaining_carbon_budgets <- get("remaining_carbon_budgets")

  data_aggregate_scores <- prep_scores(
    equity_results_portfolio = equity_results_portfolio,
    bonds_results_portfolio = bonds_results_portfolio,
    peers_equity_results_aggregated = peers_equity_results_aggregated,
    peers_bonds_results_aggregated = peers_bonds_results_aggregated,
    portfolio_allocation_method_equity = portfolio_allocation_method_equity,
    portfolio_allocation_method_bonds = portfolio_allocation_method_bonds,
    scenario_source = scenario_source,
    scenarios = scenarios,
    scenario_geography = scenario_geography,
    equity_market = equity_market,
    start_year = start_year,
    time_horizon = time_horizon,
    green_techs = green_techs,
    remaining_carbon_budgets = remaining_carbon_budgets
  )

  data_out <- list(
    data_aggregate_scores = data_aggregate_scores
  )

  return(data_out)
}

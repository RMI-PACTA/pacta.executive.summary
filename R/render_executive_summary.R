#' Renders executive summary
#'
#' @return a pdf document written to output_dir
#' @export
#'
render_executive_summary <- function(data,
                                     language,
                                     output_dir,
                                     exec_summary_dir,
                                     survey_dir,
                                     real_estate_dir,
                                     file_name = "template.Rmd",
                                     investor_name,
                                     portfolio_name,
                                     peer_group,
                                     total_portfolio,
                                     scenario_selected,
                                     currency_exchange_value) {
  render(
    input = file.path(exec_summary_dir, file_name),
    output_dir = output_dir,
    clean = TRUE,
    quiet = TRUE,
    params = list(
      survey_dir = survey_dir,
      real_estate_dir = real_estate_dir,
      language = language,
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      peer_group = peer_group,
      scenario_selected = scenario_selected,
      audit_data = data$audit_data,
      emissions_data = data$emissions_data,
      total_portfolio = total_portfolio,
      results_portfolio = data$results_portfolio,
      peers_results_aggregated = data$peers_results_aggregated,
      peers_results_individual = data$peers_results_individual,
      indices_results_portfolio = data$indices_results_portfolio,
      currency_exchange_value = currency_exchange_value
    )
  )
}

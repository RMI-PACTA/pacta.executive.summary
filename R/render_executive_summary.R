#' Renders executive summary
#'
#' @param data List of data frames in the format returned by `prep_data_executive_summary()`
#' @param language Character single, valid two letter language identifier in uppercase e.g. "EN"
#' @param output_dir Character single, valid filepath to a directory where the output will be saved
#' @param exec_summary_dir Character single, valid filepath to a directory that contains the template, e.g. `system.file("extdata", "PA2022CH_en_exec_summary", package = "pacta.executive.summary")`
#' @param survey_dir Character single, valid filepath to a directory that contains the survey files for the user
#' @param real_estate_dir Character single, valid filepath to a directory that contains real estate files for the user
#' @param file_name Character single, valid filename of the Rmd template file, e.g. "template.Rmd"
#' @param investor_name Character single string specifying the investor name
#' @param portfolio_name Character single string specifying the portfolio name
#' @param peer_group Character single string specifying the peer group
#' @param total_portfolio Data frame that contains the total portfolio as found in the standard PACTA processed inputs file "total_portfolio.rds"
#' @param scenario_selected Character single string specifying the selected scenario, e.g. "1.5C-Unif"
#' @param currency_exchange_value Numeric single numeric value specifying the exchange rate from USD into the desired display currency, e.g. `1.03`
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
                                     score_card_dir,
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
      score_card_dir = score_card_dir,
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

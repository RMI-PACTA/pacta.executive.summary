#' Renders executive summary
#'
#' ?This one lives in create_interactive_report repo
#'
#' @return
#' @export
#'
#' @examples
#'
# make sure that all the data needed for this to run are in the parameters list
render_executive_summary <- function(data,
                                     language,
                                     output_dir,
                                     exec_summary_dir,
                                     survey_dir,
                                     real_estate_dir,
                                     file_name = "template.Rmd",
                                     investor_name,
                                     portfolio_name) {
  render(path(exec_summary_dir, file_name),
    output_dir = output_dir,
    survey_dir = survey_dir,
    real_estate_dir = real_estate_dir,
    clean = TRUE,
    quiet = TRUE,
    params = list(
      language = language,
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      audit_data = data$audit_data_filtered,
      emissions_data = data$emissions_data_filtered,
      results_portfolio = data$results_portfolio_filtered,
      peers_results_aggregated = data$peers_results_aggregated_filtered,
      peers_results_individual = data$peers_results_individual_filtered,
      indices_results_portfolio = data$indices_results_portfolio_filtered
    )
  )
}

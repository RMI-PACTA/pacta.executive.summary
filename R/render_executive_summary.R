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
  render(file.path(exec_summary_dir, file_name),
    output_dir = output_dir,
    clean = TRUE,
    quiet = TRUE,
    params = list(
      survey_dir = survey_dir,
      real_estate_dir = real_estate_dir,
      language = language,
      investor_name = investor_name,
      portfolio_name = portfolio_name,
      audit_data = data$audit_data,
      emissions_data = data$emissions_data,
      results_portfolio = data$results_portfolio,
      peers_results_aggregated = data$peers_results_aggregated,
      peers_results_individual = data$peers_results_individual,
      indices_results_portfolio = data$indices_results_portfolio
    )
  )
}

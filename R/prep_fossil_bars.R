prep_fossil_bars <- function(results_portfolio = NULL,
                             peers_results_aggregated = NULL,
                             indices_results_portfolio = NULL) {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("fossil_bars")
  }
  data_out
}

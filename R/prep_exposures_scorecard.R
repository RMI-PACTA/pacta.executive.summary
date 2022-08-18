prep_exposures_scorecard <- function(results_portfolio = NULL) {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("exposures_scorecard")
  }
  data_out
}

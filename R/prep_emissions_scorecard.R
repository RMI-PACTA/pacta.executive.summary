prep_emissions_scorecard <- function(emissions_data = NULL) {
  if (is.null(emissions_data)) {
    data_out <- use_toy_data("emissions_scorecard")
  }
  data_out
}

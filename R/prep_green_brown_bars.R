prep_green_brown_bars <- function(results_portfolio = NULL) {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("green_brown_bars")
  }
  data_out
}
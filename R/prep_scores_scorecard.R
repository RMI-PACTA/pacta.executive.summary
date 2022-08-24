prep_scores_scorecard <- function(results_portfolio = NULL) {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("scores") %>% filter(
      .data$scope == "portfolio",
      .data$entity == "this_portfolio"
    )
  }
  data_out
}

prep_scores <- function(results_portfolio = NULL, peers_results_aggregated = NULL,
                        asset_class = "equity") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("scores") %>% filter(asset_class == .env$asset_class)
  }
  data_out
}

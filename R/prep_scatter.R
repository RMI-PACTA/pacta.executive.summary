prep_scatter <- function(results_portfolio = NULL, peers_results_individual = NULL,
                         indices_results_portfolio = NULL, sector = "power", asset_class = "equity") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("scatter") %>% filter(asset_class == .env$asset_class)
  }
  data_out
}

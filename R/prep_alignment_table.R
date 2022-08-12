prep_alignment_table <- function(
  results_portfolio = NULL, 
  peers_results_aggregated = NULL, 
  asset_class = "equity") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("alignment_table") %>% filter(asset_class == .env$asset_class)
  }
  data_out
}

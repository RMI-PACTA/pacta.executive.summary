prep_exposures_survey <- function(results_portfolio = NULL, 
                                             sector = "coal",
                                             asset_class = "equity") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("exposures_survey") %>%
      filter(
        .data$asset_class == .env$asset_class,
        .data$sector == .env$sector
      )
  }
  data_out
}
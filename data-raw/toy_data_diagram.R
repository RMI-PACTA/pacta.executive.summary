toy_data_diagram <- tibble::tibble(
  exposure_portfolio = c(3000000, 3000000),
  asset_class = c("bonds", "equity"),
  exposure_asset_class = c(300000, 450000),
  exposure_asset_class_perc = c(0.1, 0.15),
  exposure_pacta = c(60000, 135000),
  exposure_pacta_perc_asset_class_exposure = c(0.2, 0.3),
  emissions_pacta_perc = c(0.8, 0.7),
  emissions_pacta = c(70000, 100000)
)

usethis::use_data(toy_data_diagram)

toy_data_emissions_scorecard <- tibble::tribble(
  ~asset_class,     ~entity, ~emissions,
       "bonds", "portfolio",      70000,
       "bonds", "benchmark",      80000,
      "equity", "portfolio",      60000,
      "equity", "benchmark",      70000
  )

usethis::use_data(toy_data_emissions_scorecard, overwrite = TRUE)

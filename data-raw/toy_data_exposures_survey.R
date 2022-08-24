# styler: off
toy_data_exposures_survey <- tibble::tribble(
~asset_class,     ~entity,       ~sector, ~exposure_perc_aum,
      "equity", "portfolio",        "coal",               0.01,
      "equity",     "peers",        "coal",               0.02,
      "equity", "portfolio", "oil_and_gas",               0.03,
      "equity",     "peers", "oil_and_gas",               0.02,
       "bonds", "portfolio",        "coal",               0.03,
       "bonds",     "peers",        "coal",               0.01,
       "bonds", "portfolio", "oil_and_gas",                0.1,
       "bonds",     "peers", "oil_and_gas",                0.2
  )
# styler: on

toy_data_exposures_survey <- toy_data_exposures_survey %>%
  mutate(
    entity = factor(
      entity,
      levels = rev(c("portfolio", "peers"))
    )
  )

usethis::use_data(toy_data_exposures_survey, overwrite = TRUE)

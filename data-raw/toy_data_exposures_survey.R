# styler: off
toy_data_exposures_survey <- tibble::tribble(
  ~asset_class,     ~entity,     ~technology, ~exposure_perc_aum,
      "equity", "portfolio",        "coal",               0.01,
      "equity",     "peers",        "coal",               0.02,
      "equity", "portfolio",         "oil",               0.03,
      "equity",     "peers",         "oil",               0.02,
      "equity", "portfolio",         "gas",               0.05,
      "equity",     "peers",         "gas",               0.04,
       "bonds", "portfolio",        "coal",               0.03,
       "bonds",     "peers",        "coal",               0.01,
       "bonds", "portfolio",         "oil",              0.015,
       "bonds",     "peers",         "oil",              0.025,
       "bonds", "portfolio",         "gas",               0.03,
       "bonds",     "peers",         "gas",               0.25
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

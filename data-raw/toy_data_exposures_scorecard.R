# styler: off
toy_data_exposures_scorecard <- tibble::tribble(
  ~asset_class,      ~sector_or_tech, ~exposure_perc_aum,
      "equity",               "coal",               0.01,
      "equity", "other_fossil_fuels",               0.02,
      "equity",       "fossil_power",               0.15,
      "equity",   "renewables_power",               0.12,
       "bonds",               "coal",               0.03,
       "bonds", "other_fossil_fuels",               0.01,
       "bonds",       "fossil_power",                0.1,
       "bonds",   "renewables_power",                0.2
  )
# styler: on

toy_data_exposures_scorecard <- toy_data_exposures_scorecard %>%
  mutate(
    sector_or_tech = factor(
      sector_or_tech,
      levels = rev(c("coal", "other_fossil_fuels", "fossil_power", "renewable_power"))
      )
  )

usethis::use_data(toy_data_exposures_scorecard, overwrite = TRUE)

# styler: off
remaining_carbon_budgets <- tibble::tribble(
  ~scenario_source,  ~ald_sector, ~remaining_carbon_budget,           ~unit, ~weighting_factor,
        "GECO2021",       "Coal",                     8432, "Mt CO2 (2019)",              0.24,
        "GECO2021",        "Oil",                     8085, "Mt CO2 (2019)",              0.23,
        "GECO2021",        "Gas",                     5702, "Mt CO2 (2019)",              0.16,
        "GECO2021",      "Power",                     6967, "Mt CO2 (2019)",              0.19,
        "GECO2021",      "Steel",                     1250, "Mt CO2 (2019)",              0.03,
        "GECO2021",     "Cement",                     2455, "Mt CO2 (2019)",              0.07,
        # TODO: split automotive in HDV and LDV
        "GECO2021", "Automotive",                     2432, "Mt CO2 (2019)",              0.07,
        "GECO2021",   "Aviation",                      514, "Mt CO2 (2019)",              0.01
  )
# styler: on

usethis::use_data(remaining_carbon_budgets, overwrite = TRUE)

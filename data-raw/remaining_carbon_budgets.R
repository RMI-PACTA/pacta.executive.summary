# styler: off
remaining_carbon_budgets <- tibble::tribble(
  ~scenario_source,  ~ald_sector, ~remaining_carbon_budget,           ~unit, ~weighting_factor,
        "GECO2021",       "coal",                     8432, "Mt CO2 (2019)",              0.24,
        "GECO2021",        "oil",                     8085, "Mt CO2 (2019)",              0.23,
        "GECO2021",        "gas",                     5702, "Mt CO2 (2019)",              0.16,
        "GECO2021",      "power",                     6967, "Mt CO2 (2019)",              0.19,
        "GECO2021",      "steel",                     1250, "Mt CO2 (2019)",              0.03,
        "GECO2021",     "cement",                     2455, "Mt CO2 (2019)",              0.07,
        "GECO2021", "automotive",                     2432, "Mt CO2 (2019)",              0.07,
        "GECO2021",   "aviation",                      514, "Mt CO2 (2019)",              0.01
  )
# styler: on

usethis::use_data(remaining_carbon_budgets, overwrite = TRUE)

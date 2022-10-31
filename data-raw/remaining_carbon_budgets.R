# styler: off
remaining_carbon_budgets <- tibble::tribble(
  ~scenario_source,  ~ald_sector, ~remaining_carbon_budget,           ~unit, ~weighting_factor,
        "GECO2021",       "coal",                 84934.79, "Mt CO2 (2019)",              0.25,
        "GECO2021",        "oil",                 82987.79, "Mt CO2 (2019)",              0.24,
        "GECO2021",        "gas",                 49094.21, "Mt CO2 (2019)",              0.14,
        "GECO2021",      "power",                 91121.17, "Mt CO2 (2019)",              0.27,
        "GECO2021",      "steel",                 10746.03, "Mt CO2 (2019)",              0.03,
        "GECO2021", "automotive",                  11338.9, "Mt CO2 (2019)",              0.03,
        "GECO2021",        "hdv",                  7388.01, "Mt CO2 (2019)",              0.02,
        "GECO2021",   "aviation",                  5963.32, "Mt CO2 (2019)",              0.02
  )
# styler: on

usethis::use_data(remaining_carbon_budgets, overwrite = TRUE)

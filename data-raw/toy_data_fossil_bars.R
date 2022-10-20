# styler: off
toy_data_fossil_bars <- tibble::tribble(
      ~entity,               ~entity_name,     ~entity_type, ~year,  ~tech, ~perc_aum, ~asset_class,
  "portfolio",                "portfolio", "this_portfolio",  2022, "coal",     0.001,     "equity",
  "portfolio",                "portfolio", "this_portfolio",  2022,  "oil",     0.002,     "equity",
  "portfolio",                "portfolio", "this_portfolio",  2022,  "gas",     0.001,     "equity",
      "peers",                    "peers",      "benchmark",  2022, "coal",     0.002,     "equity",
      "peers",                    "peers",      "benchmark",  2022,  "oil",     0.003,     "equity",
      "peers",                    "peers",      "benchmark",  2022,  "gas",     0.001,     "equity",
      "index",               "MSCI_world",      "benchmark",  2022, "coal",     0.005,     "equity",
      "index",               "MSCI_world",      "benchmark",  2022,  "oil",     0.002,     "equity",
      "index",               "MSCI_world",      "benchmark",  2022,  "gas",     0.003,     "equity",
  "portfolio",                "portfolio", "this_portfolio",  2022, "coal",     0.003,      "bonds",
  "portfolio",                "portfolio", "this_portfolio",  2022,  "oil",     0.005,      "bonds",
  "portfolio",                "portfolio", "this_portfolio",  2022,  "gas",     0.001,      "bonds",
      "peers",                    "peers",      "benchmark",  2022, "coal",     0.004,      "bonds",
      "peers",                    "peers",      "benchmark",  2022,  "oil",     0.001,      "bonds",
      "peers",                    "peers",      "benchmark",  2022,  "gas",     0.001,      "bonds",
      "index", "iShares_Global_Corp_Bond",      "benchmark",  2022, "coal",     0.003,      "bonds",
      "index", "iShares_Global_Corp_Bond",      "benchmark",  2022,  "oil",     0.002,      "bonds",
      "index", "iShares_Global_Corp_Bond",      "benchmark",  2022,  "gas",     0.002,      "bonds"
  )
# styler: on

usethis::use_data(toy_data_fossil_bars, overwrite = TRUE)

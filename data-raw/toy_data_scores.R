# styler: off
toy_data_scores <- tibble::tribble(
  ~asset_class,      ~scope,          ~entity,      ~sector, ~score,
       "bonds", "portfolio", "this_portfolio",           NA,   "A+",
       "bonds", "portfolio",          "peers",           NA,    "A",
       "bonds",    "sector", "this_portfolio",      "power",    "B",
       "bonds",    "sector",          "peers",      "power",   "A+",
       "bonds",    "sector", "this_portfolio", "automotive",    "C",
       "bonds",    "sector",          "peers", "automotive",    "B",
       "bonds",    "sector", "this_portfolio",       "coal",   "A+",
       "bonds",    "sector",          "peers",       "coal",    "B",
       "bonds",    "sector", "this_portfolio",        "oil",    "D",
       "bonds",    "sector",          "peers",        "oil",    "B",
       "bonds",    "sector", "this_portfolio",        "gas",    "A",
       "bonds",    "sector",          "peers",        "gas",    "B",
       "bonds",    "sector", "this_portfolio",      "steel",    "B",
       "bonds",    "sector",          "peers",      "steel",    "C",
      "equity", "portfolio", "this_portfolio",           NA,    "B",
      "equity", "portfolio",          "peers",           NA,    "A",
      "equity",    "sector", "this_portfolio",      "power",    "D",
      "equity",    "sector",          "peers",      "power",    "B",
      "equity",    "sector", "this_portfolio", "automotive",    "A",
      "equity",    "sector",          "peers", "automotive",    "A",
      "equity",    "sector", "this_portfolio",       "coal",    "C",
      "equity",    "sector",          "peers",       "coal",    "B",
      "equity",    "sector", "this_portfolio",        "oil",    "A",
      "equity",    "sector",          "peers",        "oil",    "C",
      "equity",    "sector", "this_portfolio",        "gas",    "B",
      "equity",    "sector",          "peers",        "gas",    "B",
      "equity",    "sector", "this_portfolio",      "steel",    "A",
      "equity",    "sector",          "peers",      "steel",    "C"
  )
# styler: on

usethis::use_data(toy_data_scores, overwrite = TRUE)

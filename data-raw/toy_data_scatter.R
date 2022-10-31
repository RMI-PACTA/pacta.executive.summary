# styler: off
toy_data_scatter <- tibble::tribble(
  ~asset_class, ~year,   ~tech_mix_green, ~score,   ~entity_name,     ~entity_type,
      "equity",  2027, 0.490194514934063,    "C", "peers_average",      "peers_mean",
      "equity",  2027,               0.3,    "E",    "portfolio", "this_portfolio",
      "equity",  2027,               0.5,   "A+",        "peer1",           "peers",
      "equity",  2027,               0.3,    "A",        "peer2",           "peers",
      "equity",  2027,               0.1,    "D",        "peer3",           "peers",
      "equity",  2027,               0.7,   "A+",        "peer4",           "peers",
      "equity",  2027,               0.6,    "B",   "msci_world",      "benchmark",
       "bonds",  2027, 0.490194514934063,    "C", "peers_average",      "peers_mean",
       "bonds",  2027,               0.3,    "E",    "portfolio", "this_portfolio",
       "bonds",  2027,               0.5,    "A",        "peer1",           "peers",
       "bonds",  2027,               0.3,    "B",        "peer2",           "peers",
       "bonds",  2027,               0.1,    "C",        "peer3",           "peers",
       "bonds",  2027,               0.7,   "A+",        "peer4",           "peers",
       "bonds",  2027,               0.6,    "B",   "msci_world",      "benchmark"
  )
# styler: on

usethis::use_data(toy_data_scatter, overwrite = TRUE)

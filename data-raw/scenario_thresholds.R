# styler: off
scenario_thresholds <- tibble::tribble(
  ~scenario_source,   ~scenario, ~threshold,
  "GECO2021",       "1.5C-Unif",     "high",
  "GECO2021",         "NDC-LTS",      "mid",
  "GECO2021",          "CurPol",      "low"
)
# styler: on

usethis::use_data(scenario_thresholds, overwrite = TRUE)

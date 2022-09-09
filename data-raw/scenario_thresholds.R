# styler: off
scenario_thresholds <- tibble::tribble(
  ~scenario_source,   ~scenario, ~threshold,
  "GECO2021",       "1.5C-Unif",     "1.5C",
  "GECO2021",         "NDC-LTS",     "2.0C",
  "GECO2021",          "CurPol",      "ref"
)
# styler: on

usethis::use_data(scenario_thresholds, overwrite = TRUE)

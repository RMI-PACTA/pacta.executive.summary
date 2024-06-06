# styler: off
scenario_thresholds <- tibble::tribble(
  ~scenario_source,   ~scenario, ~threshold,
  "GECO2021",       "1.5C-Unif",     "high",
  "GECO2021",         "NDC-LTS",      "mid",
  "GECO2021",          "CurPol",      "low",
  "GECO2023",            "1.5C",     "high",
  "GECO2023",         "NDC-LTS",      "mid",
  "GECO2023",       "Reference",      "low",
   "WEO2023",        "NZE_2050",     "high",
   "WEO2023",             "APS",      "mid",
   "WEO2023",           "STEPS",      "low"
)
# styler: on

usethis::use_data(scenario_thresholds, overwrite = TRUE)

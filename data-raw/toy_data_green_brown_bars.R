# styler: off
toy_data_green_brown_bars <- tibble::tribble(
  ~asset_class, ~year,          ~tech_type,        ~sector, ~perc_tech_exposure,  ~perc_sec_exposure,
       "bonds",  2022,             "brown",   "automotive",  0.0919540229885057,   0.229885057471264,
       "bonds",  2022,             "green",   "automotive",   0.137931034482759,   0.229885057471264,
      "equity",  2022,             "brown",   "automotive",    0.14002333722287,   0.233372228704784,
      "equity",  2022,             "green",   "automotive",  0.0933488914819137,   0.233372228704784,
       "bonds",  2022,             "brown",        "power",   0.275862068965517,   0.689655172413793,
       "bonds",  2022,             "green",        "power",   0.137931034482759,   0.689655172413793,
       "bonds",  2022,           "nuclear",        "power",   0.275862068965517,   0.689655172413793,
      "equity",  2022,             "brown",        "power",    0.29171528588098,    0.58343057176196,
      "equity",  2022,             "green",        "power",   0.175029171528588,    0.58343057176196,
      "equity",  2022,           "nuclear",        "power",   0.116686114352392,    0.58343057176196,
       "bonds",  2022,             "brown", "fossil_fuels",  0.0114942528735632,  0.0114942528735632,
      "equity",  2022,             "brown", "fossil_fuels", 0.00233372228704784, 0.00233372228704784,
       "bonds",  2022,             "other",        "steel",  0.0229885057471264,  0.0229885057471264,
      "equity",  2022,             "other",        "steel",   0.058343057176196,   0.058343057176196,
       "bonds",  2022,             "other",       "cement",  0.0229885057471264,  0.0229885057471264,
      "equity",  2022,             "other",       "cement",  0.0058343057176196,  0.0058343057176196,
       "bonds",  2022,             "other",     "aviation",  0.0229885057471264,  0.0229885057471264,
      "equity",  2022,             "other",     "aviation",   0.116686114352392,   0.116686114352392
  )

# styler: on

usethis::use_data(toy_data_green_brown_bars, overwrite = TRUE)

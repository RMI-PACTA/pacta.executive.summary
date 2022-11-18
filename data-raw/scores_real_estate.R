scores_real_estate <- tibble::tribble(
  ~score_upper, ~score_symbol, ~category, ~score_delta, ~score_label,
          14.3,           "G",   "score",         14.3,         7.15,
          28.6,           "F",   "score",         14.3,        21.45,
          42.9,           "E",   "score",         14.3,        35.75,
          57.2,           "D",   "score",         14.3,        50.05,
          71.5,           "C",   "score",         14.3,        64.35,
          85.8,           "B",   "score",         14.3,        78.65,
           100,           "A",   "score",         14.2,        92.95
  )

usethis::use_data(scores_real_estate, replace = TRUE)

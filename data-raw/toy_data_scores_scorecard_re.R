toy_data_scores_scorecard_re <- tibble::tribble(
     ~asset_class, ~score,
  "directly held",    "C",
      "mortgages",    "E"
  )

usethis::use_data(toy_data_scores_scorecard_re, replace = TRUE)

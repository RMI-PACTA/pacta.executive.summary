fill_labels_green_brown_bars <- c(
  "Green" = "Low-carbon",
  "Hydro And Nuclear" = "Hydro & Nuclear",
  "Brown" = "High-carbon",
  "Other" = "Sectors without\ntech. roadmap"
)

fill_labels_fossil_bars <- c(
  "this_portfolio" = "Portfolio",
  "benchmark" = "Benchmarks"
)

usethis::use_data(
  fill_labels_green_brown_bars,
  fill_labels_fossil_bars,
  internal = TRUE,
  overwrite = TRUE
)

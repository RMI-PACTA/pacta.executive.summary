fill_colours_green_brown_bars <- c(
  "Green" = "#7BC17E",
  "Hydro And Nuclear" = "#b9b5b0",
  "Brown" = "#977447",
  "Other" = "#6e819c"
)

fill_colours_fossil_bars <- c(
  "this_portfolio" = "#181716",
  "benchmark" = "#c5c4cf"
)

usethis::use_data(
  fill_colours_green_brown_bars,
  fill_colours_fossil_bars,
  internal = TRUE,
  overwrite = TRUE
)

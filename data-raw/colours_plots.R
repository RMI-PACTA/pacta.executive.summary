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

fill_colours_scores <- c(
  "E" = "#FF0D0D", "D" = "#FF4E11", "C" = "#FF8E15", "B" = "#FAB733", 
  "A" = "#ACB334", "A+" = "#69B34C"
)

fill_colours_techmix <- c("green" = "#7bc17e", "brown" = "#986B41")

usethis::use_data(
  fill_colours_green_brown_bars,
  fill_colours_fossil_bars,
  fill_colours_scores,
  fill_colours_techmix,
  internal = TRUE,
  overwrite = TRUE
)

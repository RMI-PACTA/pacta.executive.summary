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

axis_labels_scatter <- c(
  x = "Share of low-carbon\ntechnologies in Power sector (current)",
  y = "Alignment score\n(forward-looking)",
  legend_title = "Entity type"
)

usethis::use_data(
  fill_colours_green_brown_bars,
  fill_colours_fossil_bars,
  fill_colours_scores,
  fill_colours_techmix,
  fill_labels_green_brown_bars,
  fill_labels_fossil_bars,
  axis_labels_scatter,
  internal = TRUE,
  overwrite = TRUE
)

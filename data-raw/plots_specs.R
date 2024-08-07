fill_colours_green_brown_bars <- c(
  "Green" = "#7BC17E",
  "Nuclear" = "#b9b5b0",
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

fill_colours_entities_scores <- c(
  "portfolio" = "#1c324f",
  "benchmark" = "#66696b"
)

fill_colours_techmix <- c("green" = "#7bc17e", "brown" = "#986B41")

fill_colours_exposures_scorecard <- c(
  "renewables_power" = "#7BC17E",
  "renewables_and_hydro_power" = "#7BC17E",
  "other_fossil_fuels" = "#181716",
  "coal" = "#4e3b37",
  "fossil_power" = "#977447"
)

fill_labels_green_brown_bars <- c(
  "Green" = "Low-carbon**",
  "Nuclear" = "Nuclear",
  "Brown" = "High-carbon**",
  "Other" = "Sectors without tech. roadmap"
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

fill_colours_scores_re <- c(
  "G" = "#eb1c25", "F" = "#f3701c", "E" = "#fdb912", "D" = "#fef200",
  "C" = "#bdd532", "B" = "#51b747", "A" = "#05a551"
)

usethis::use_data(
  fill_colours_green_brown_bars,
  fill_colours_fossil_bars,
  fill_colours_scores,
  fill_colours_entities_scores,
  fill_colours_techmix,
  fill_colours_exposures_scorecard,
  fill_labels_green_brown_bars,
  fill_labels_fossil_bars,
  axis_labels_scatter,
  fill_colours_scores_re,
  internal = TRUE,
  overwrite = TRUE
)

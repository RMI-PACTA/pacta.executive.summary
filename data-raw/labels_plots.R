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
  fill_labels_green_brown_bars,
  fill_labels_fossil_bars,
  axis_labels_scatter,
  internal = TRUE,
  overwrite = TRUE
)

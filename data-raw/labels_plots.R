fill_labels_green_brown_bars <- c(
  "Green" = "Low-carbon",
  "Hydro And Nuclear" = "Hydro & Nuclear",
  "Brown" = "High-carbon",
  "Other" = "Sectors without\ntech. roadmap"
)

usethis::use_data(
  fill_labels_green_brown_bars,
  internal = TRUE,
  overwrite = TRUE
)

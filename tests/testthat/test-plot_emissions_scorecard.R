options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_emissions_scorecard(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_emissions_scorecard[0L, ]
  expect_snapshot_error(plot_emissions_scorecard(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_emissions_scorecard
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3")
  expect_snapshot_error(plot_emissions_scorecard(data_wrong_names))
})

test_that("with wrong values of `asset_class` errors gracefully", {
  data_wrong <- toy_data_emissions_scorecard
  data_wrong$asset_class <- as.character(data_wrong$asset_class)
  data_wrong$asset_class[1] <- "bad"
  expect_snapshot_error(plot_emissions_scorecard(data_wrong))
})

test_that("with wrong values of `entity` errors gracefully", {
  data_wrong <- toy_data_emissions_scorecard
  data_wrong$entity <- as.character(data_wrong$entity)
  data_wrong$entity[1] <- "bad"
  expect_snapshot_error(plot_emissions_scorecard(data_wrong))
})

test_that("with non-numeric values of `emissions` errors gracefully", {
  data_wrong <- toy_data_emissions_scorecard
  data_wrong$emissions <- as.character(data_wrong$emissions)
  expect_snapshot_error(plot_emissions_scorecard(data_wrong))
})

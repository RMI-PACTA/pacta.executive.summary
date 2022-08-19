options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_exposures_scorecard(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_exposures_scorecard[0L, ]
  expect_snapshot_error(plot_exposures_scorecard(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_exposures_scorecard
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3")
  expect_snapshot_error(plot_exposures_scorecard(data_wrong_names))
})

test_that("with wrong values of `sector_or_tech` errors gracefully", {
  data_wrong <- toy_data_exposures_scorecard
  data_wrong$sector_or_tech <- as.character(data_wrong$sector_or_tech)
  data_wrong$sector_or_tech[1] <- "bad"
  expect_snapshot_error(plot_exposures_scorecard(data_wrong))
})

test_that("with non-numeric values of `exposure_perc_aum` errors gracefully", {
  data_wrong <- toy_data_exposures_scorecard
  data_wrong$exposure_perc_aum <- as.character(data_wrong$exposure_perc_aum)
  expect_snapshot_error(plot_exposures_scorecard(data_wrong))
})

test_that("with wrong values of `exposure_perc_aum` errors gracefully", {
  data_wrong <- toy_data_exposures_scorecard
  data_wrong$exposure_perc_aum <- data_wrong$exposure_perc_aum * 10000
  expect_snapshot_error(plot_exposures_scorecard(data_wrong))
})

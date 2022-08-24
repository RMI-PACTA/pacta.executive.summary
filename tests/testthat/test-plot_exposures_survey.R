options(warn = -1)

toy_data_exposures_survey_ex <- function() {
  toy_data_exposures_survey %>%
    dplyr::filter(
      asset_class == "equity",
      sector == "coal"
    )
}

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_exposures_survey(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_exposures_survey_ex()[0L, ]
  expect_snapshot_error(plot_exposures_survey(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_exposures_survey_ex()
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4")
  expect_snapshot_error(plot_exposures_survey(data_wrong_names))
})

test_that("with multiple values for `asset_class` errors gracefully", {
  expect_snapshot_error(plot_exposures_survey(toy_data_exposures_survey))
})

test_that("with wrong values of `entity` errors gracefully", {
  data_wrong <- toy_data_exposures_survey_ex()
  data_wrong$entity <- as.character(data_wrong$entity)
  data_wrong$entity[1] <- "bad"
  expect_snapshot_error(plot_exposures_survey(data_wrong))
})

test_that("with wrong values of `sector` errors gracefully", {
  data_wrong <- toy_data_exposures_survey_ex()
  data_wrong$sector <- as.character(data_wrong$sector)
  data_wrong$sector[1] <- "bad"
  expect_snapshot_error(plot_exposures_survey(data_wrong))
})

test_that("with non-numeric values of `exposure_perc_aum` errors gracefully", {
  data_wrong <- toy_data_exposures_survey_ex()
  data_wrong$exposure_perc_aum <- as.character(data_wrong$exposure_perc_aum)
  expect_snapshot_error(plot_exposures_survey(data_wrong))
})

test_that("with wrong values of `exposure_perc_aum` errors gracefully", {
  data_wrong <- toy_data_exposures_survey_ex()
  data_wrong$exposure_perc_aum <- data_wrong$exposure_perc_aum * 10000
  expect_snapshot_error(plot_exposures_survey(data_wrong))
})


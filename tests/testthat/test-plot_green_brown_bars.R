options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_green_brown_bars(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_green_brown_bars[0L, ]
  expect_snapshot_error(plot_green_brown_bars(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_green_brown_bars
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5", "bad6")
  expect_snapshot_error(plot_green_brown_bars(data_wrong_names))
})

test_that("with wrong values of `tech_type` errors gracefully", {
  data_wrong_tech_type <- toy_data_green_brown_bars
  data_wrong_tech_type$tech_type <- as.character(data_wrong_tech_type$tech_type)
  data_wrong_tech_type$tech_type[1] <- "bad"
  expect_snapshot_error(plot_green_brown_bars(data_wrong_tech_type))
})

test_that("with non-numeric values of `perc_tech_exposure` and `perc_sec_exposure` errors gracefully", {
  data_wrong_perc_tech <- toy_data_green_brown_bars
  data_wrong_perc_tech$perc_tech_exposure <- as.character(data_wrong_perc_tech$perc_tech_exposure)
  expect_snapshot_error(plot_green_brown_bars(data_wrong_perc_tech))
  
  data_wrong_perc_sec <- toy_data_green_brown_bars
  data_wrong_perc_sec$perc_sec_exposure <- as.character(data_wrong_perc_sec$perc_sec_exposure)
  expect_snapshot_error(plot_green_brown_bars(data_wrong_perc_sec))
})

test_that("with wrong values of `perc_tech_exposure` and `perc_sec_exposure` errors gracefully", {
  data_wrong_perc_tech <- toy_data_green_brown_bars
  data_wrong_perc_tech$perc_tech_exposure <- data_wrong_perc_tech$perc_tech_exposure * 100
  expect_snapshot_error(plot_green_brown_bars(data_wrong_perc_tech))
  
  data_wrong_perc_sec <- toy_data_green_brown_bars
  data_wrong_perc_sec$perc_sec_exposure <- data_wrong_perc_sec$perc_sec_exposure * 100
  expect_snapshot_error(plot_green_brown_bars(data_wrong_perc_sec))
})